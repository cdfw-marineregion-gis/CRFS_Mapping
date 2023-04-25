# Script to clean up and compile the i8 (location data)
# Sourced in the observed catch, reported catch, and observed effort scripts to add in cleaned up block data for each ID
# Michael Patton (michael.patton@wildlife.ca.gov)

#USER INPUT:
# copy path to where you downloaded the shared CRFS_Mapping folder between the ()
working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping)"
setwd(working_directory)

#load in required packages, if R says the package is not installed then run the following code in the console below install.packages("packagename") so for example install.packages("data.table")
library(data.table)
library(stringi)
library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(leaflet)
options(scipen = 999)


# Format block data ----------------------------------------------

# function takes block column and microblock column and standardizes if populated
format_box = function(block_column, microblock_column) {
  combined = paste(block_column, microblock_column, sep="-")
  combined_noNA = gsub("NA", "", combined)
  combined_noblanks = ifelse(nchar(combined_noNA) < 5, "", combined_noNA)
  return(combined_noblanks)
}

i8_new = fread(file = here('RCode', 'PR', 'Dat16toPresent', 'Data', 'i8_data_16to21.csv'), fill = TRUE) %>%
  rename(ID_CODE = id_code) %>%
  mutate_all(as.character)

# Read in location data, or the i8 data table for northern california
NLocList <- fread(file = here("RCode", "PR", "Dat04to15", "Data", "i8_1999-2016_NorCal_235374r.csv"), fill = TRUE, na.string = c("",".", "NA")) 
# read in i8 location data for southern california
SLocList <- fread(file = here("RCode", "PR", "Dat04to15", "Data", "i8_1999-2016_SoCal_204029r.csv"), fill = TRUE, na.string = c("",".", "NA")) 

#combine north and south locaiton data together
loc = rbind(NLocList, SLocList) %>%
  mutate_all(as.character) %>%
  mutate( 
    date = ymd(stri_sub(ID_CODE, 6, 13)), 
    year = year(date)) %>%
  filter(year != 2016) %>% # duplicate to what CDFW produced. 
  select(-date, -month)

setdiff(names(i8_new), names(loc))
# [1] "assnid"  "Ref #"  # can integrate these ASSNID REF_NUM

setdiff(names(loc), names(i8_new))

loc = bind_rows(loc, i8_new)
loc_org = loc

# create new id based on id code and location number, format boxes into XX-XXX format, apply to each block column

loc <- loc %>% 
  mutate(locn = ifelse(is.na(locn)| locn == 0, 1, locn),
         block1 = ifelse(block1 == 9999999, NA, block1),
         id_loc = as.character(paste(ID_CODE, locn, sep="")),
         Bk1Bx1a = format_box(block1, box1a),  
         Bk1Bx1b = format_box(block1, box1b), 
         Bk1Bx1c = format_box(block1, box1c), 
         Bk2Bx2a = format_box(block2, box2a), 
         Bk2Bx2b = format_box(block2, box2b), 
         Bk2Bx2c = format_box(block2, box2c)) %>%
  mutate( 
         date = ymd(stri_sub(ID_CODE, 6, 13)), 
         month = month(date), 
         year = year(date)) %>%
  select(id_loc, year, ID_CODE, MODE_FX, survey, HLDEPTH, HLDEPTH2, ddlat, ddlong, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, HGSIZE, hgsize2)




# remove any data that is not PR (MODE_FX = 7)
notused = loc %>% 
  filter(MODE_FX != 7 | is.na(MODE_FX)) %>%
  mutate(Reason = "Location not MODE_FX == 7 (PR data)")
unique(notused$MODE_FX)
write.csv(notused, 'Outputs/NotUsed/i8/notused.csv', row.names = F, na = "")
byyear = notused %>% group_by(year) %>% count()

loc <- loc %>% 
  filter(MODE_FX ==7)

# clean up the dataframe by changing any blank cells to NA
loc[loc==""]<-NA

before = nrow(loc)
# remove data that does not have block or coordinates reported
notused2 = loc %>%
  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a) & is.na(Bk2Bx2b)  & is.na(Bk2Bx2c) & is.na(ddlat)) %>%
  mutate(Reason = "Location data does not include any block or coordinate data.")
# 2021 and 2020 missing a lot of data due to COVID
# other data is mostly block1 reported with no box
byyear = notused2 %>% group_by(year) %>% count()

write.csv(notused2, 'Outputs/NotUsed/i8/notused2.csv', row.names = F, na = "")


#lots of new data does not have block or coordinate data
newdata_test = loc_org %>%
  filter(ID_CODE %in% notused2$ID_CODE) %>%
  mutate(ID_CODE = paste0('ID', ID_CODE)) %>%
  arrange(desc(YEAR))

#write.csv(newdata_test, 'Outputs/i8_new_weirdblockformat.csv', na = "")

removed = nrow(notused2)

loc = loc %>%
  filter(!is.na(Bk1Bx1a) | !is.na(Bk1Bx1b) | !is.na(Bk1Bx1c) | !is.na(Bk2Bx2a) | !is.na(Bk2Bx2b)  | !is.na(Bk2Bx2c) | !is.na(ddlat))
after = nrow(loc)

before - removed == after

# remove any blocks that have an hgsize inputted, this field is where the survey gives the "range" of blocks visited?
hgsize_summary = loc %>% group_by(HGSIZE) %>% count()

notused3 = filter(loc, !is.na(HGSIZE) | !is.na(hgsize2)) %>%
  mutate(Reason = "HGSize or HGSize2 reported ") #2004 and 2005 is the majority of this data
write.csv(notused3, 'Outputs/NotUsed/i8/notused3.csv', row.names = F, na = "")
byyear = notused3 %>% group_by(year, HGSIZE, hgsize2) %>% count()

loc = loc %>%
  filter(is.na(HGSIZE) & is.na(hgsize2))

nrow(loc) + nrow(notused3) == after


#pull out the data that has blocks report
BlkFilter <- loc  %>% 
  filter(!is.na(Bk1Bx1a) | !is.na(Bk1Bx1b) | !is.na(Bk1Bx1c) | !is.na(Bk2Bx2a) | !is.na(Bk2Bx2b)  | !is.na(Bk2Bx2c)) 

#pull out the data has has lat longs reported INSTEAD of blocks
LatFilter  <- loc %>%  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a)  & is.na(Bk2Bx2b) & is.na(Bk2Bx2c) & !is.na(ddlat)) %>%
  mutate(ddlong = as.numeric(ddlong)*-1) %>%
  mutate(ddlat = as.numeric(ddlat))



# qa the lat long filter, there is alot of bad coordinates
notused4 = LatFilter %>%
  filter(ddlat > 50 | ddlong < -150  | is.na(ddlong) ) %>%
  mutate(Reason = "Bad COORDS REPORTED")
write.csv(notused4, 'Outputs/NotUsed/i8/notused4.csv', row.names = F, na = "")


LatFilter = LatFilter %>%
  filter(ddlat < 50 & ddlong > -150)

# convert data with coordinates reported to spatial dataframe, plotted by the coordinates
coords.SP <- st_as_sf(LatFilter, coords = c("ddlong", "ddlat"), crs = 4326, remove = FALSE)

# bring in the block shapefile from lookup folder
blocks.SP <- st_read(here("Lookups", "MAN_CA_CRFS_microblocks2013.shp"))
# change projection of blocks to WGS84 (what the coordinates are)
blocks.SP = st_transform(blocks.SP, crs = st_crs(coords.SP)) 
blocks.SP = select(blocks.SP, NM_INDEX)

# block feature has some overlap so need to remove sphereical geometry requirements (arc does this on the fly when loaded into PRO)
sf_use_s2(FALSE)

# create on the fly map to review coordinates, WHY ARE SO MANY SO BAD?
leaflet() %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  addCircleMarkers(data=coords.SP)

# spatial join to get corresponding block data
coords_wblocks.SP = st_join(coords.SP, blocks.SP, left = TRUE)

# convert back to dataframe
coords_wblocks = coords_wblocks.SP %>%
  st_drop_geometry() %>%
  mutate(Bk1Bx1a = NM_INDEX) %>%
  select(colnames(BlkFilter))

# remove any data that did not get blocks after join
notused5 = filter(coords_wblocks, is.na(Bk1Bx1a)) %>%
  mutate(Reason = "No fishing block overlapped with coordinate.")
write.csv(notused5, 'Outputs/NotUsed/i8/notused5.csv', row.names = F, na = "")

#only data that had a block joined
coords_wblocks = filter(coords_wblocks, !is.na(Bk1Bx1a))

# recombine the data that had a block reported to the data that had coordinates (and now a joined block)
all_locations = rbind(BlkFilter, coords_wblocks)


# count the number of times a unique id code appears in the location data
freq_summary <- all_locations %>% group_by(id_loc) %>% count()

#Calculate percentage of occurrence where frequency of singular id code > 1 and calculate the maximum number of duplicate ID codes
per = (nrow(filter(freq_summary, n > 1))/nrow(freq_summary))*100
message(paste(round(per,2), "% of ids have duplicates with a maximum of ", max(freq_summary$n), "duplicates."))

# new script to aggregate all blocks from these duplicate ID codes that are missing locn to a single row for that ID
qa_duplicates = all_locations %>%
  left_join(freq_summary, by = "id_loc") %>%
  filter(n> 1) %>%
  pivot_longer(Bk1Bx1a:Bk2Bx2c, names_to = "col", values_to = 'Block') %>%
  filter(!is.na(Block)) %>%
  select(-col, -HLDEPTH, -HLDEPTH2) %>%
  unique() %>% # remove when a block is repeated in the duplicate IDs
  group_by(id_loc) %>%
  mutate(block_col = row_number()) %>%
  pivot_wider(names_from = block_col, values_from = Block) %>%
  rename("Bk1Bx1a"="1","Bk1Bx1b"="2","Bk1Bx1c"="3","Bk2Bx2a"="4","Bk2Bx2b"="5","Bk2Bx2c"="6",'extrablock7' = '7','extrablock8' = '8', 'extrablock9' = '9', 'extrablock10' = '10', 'extrablock11' = '11',  'freq'='n')  # small numbner of ids have more than 6 total blocks reported across their duplicate IDs, lots of downstream fixes to integrate these may not be worth it because its a 'sloppy' fix. If new data was brought in with more than 11 blocks, the script would break

byyear = qa_duplicates %>% group_by(year) %>% count()


# join back in this count data
all_locations <- all_locations %>%
  left_join(freq_summary, by = "id_loc") %>%
  arrange(id_loc) %>%
  rename(freq = n)


#filter to just ids used once
all_locations = filter(all_locations, freq == 1) %>%
  bind_rows(qa_duplicates) %>% # add back in duplicate IDs that now have all blocks grouped together under a single ID row
  relocate('extrablock7', .after = 'Bk2Bx2c') %>%
  relocate('extrablock8', .after = 'extrablock7') %>%
  relocate('extrablock9', .after = 'extrablock8') %>%
  relocate('extrablock10', .after = 'extrablock9') %>%
  relocate('extrablock11', .after = 'extrablock10')


# this counts the number of UNIQUE blocks reported in each row of data, simplifies the long repeated lines in WINNS code
all_locations <- cbind(all_locations, total_blocks = apply(all_locations[, Bk1Bx1a:extrablock11], 1, function(x)length(unique(x[!is.na(x)]))))

# final check for duplicates that will lead to double counting, these are caused by IDs reported twice using different Pr1 or Pr2. These are an error and should be removed
pr1vpr2_duplicates = all_locations %>%
  group_by(id_loc) %>%
  count() %>%
  filter(n > 1) %>%
  left_join(all_locations)

notused6 = all_locations %>%
  filter((id_loc %in% pr1vpr2_duplicates$id_loc)) %>%
  mutate(Reason = 'Duplicate IDs used for both Pr1 and Pr2')

all_locations = all_locations %>%
  filter(!(id_loc %in% pr1vpr2_duplicates$id_loc))




notused_summary = data.frame(c(unique(notused$Reason), unique(notused2$Reason), unique(notused3$Reason), unique(notused4$Reason), unique(notused5$Reason), unique(notused6$Reason)), 
                             c(nrow(notused), nrow(notused2), nrow(notused3), nrow(notused4), nrow(notused5), nrow(notused6)))
names(notused_summary) = c("Reason", "Count")



write.csv(all_locations, "Outputs/location_cleaned.csv", na = "", row.names = F)
write.csv(notused_summary, "Outputs/location_notusedsummary.csv", na = "", row.names = F)

