# PR Observed Catch 2004_2015 ---------------------------------------------

# Michael Patton's simplification of original script (PR_ObservedCatch_2004_2015km Part 1 and Part 2.R)
#11/22/2022

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

# to put the cart before the horse, this line is required when sourcing multiple R scripts. The plan is to run all required catch and effort scripts in a "master" script to avoid having to run everything one by one. This line makes sure the required objects are not removed when sourcing multiple scripts. Youll see it in the other scripts as well.
rm(list = ls()[!ls() %in% c("oc_by_id_agg_04_15", "oe_by_id_agg_04_15", "by_id_agg_04_15", "by_id_agg_16_19", "by_id_agg_99_03")])

#  read in i3 table: sampler observed catch data. the here function provides the relative path to where the data is saved. 
oc <- fread(file = here("RCode", "PR", "Dat04to15", "Data", "PR_i3_2004-2015_759607r.csv"), fill = TRUE, na.string = c("",".")) 

# remove any data that does not have a valid SP_CODE, these "notused" objects will eventually be combined into a separate output for review
notused = oc %>%
  filter(is.na(as.numeric(SP_CODE)))  %>%
  mutate(Reason = "SP_CODE is not valid.")
unique(notused$SP_CODE)

locn_summary = oc %>% group_by(locn) %>% count()
# create a new id that combines the existing ID code with the location number. Extract the year, month and date. Select only the required columns. The NAs introduced by coercion warning message is for the SP Codes that are included in the not used object below. 
oc <- oc %>% 
  mutate(SP_CODE = as.numeric(SP_CODE)) %>%
  filter(!is.na(SP_CODE)) %>%
  mutate(locn = ifelse(is.na(locn) | locn == 0, 1, locn),
         id = paste(ID_CODE, locn, sep= ""), 
         date = ymd(stri_sub(ID_CODE, 6, 13)), 
         month = month(date)) %>%  
  select(id, date, month, year = YEAR, ALPHA5, SP_CODE, MODE_F, CNTRBTRS, DISP3, WGT, FSHINSP, HRSF)

#Create species table by extracting data from SpeciesList.csv
Sp<- fread(here("Lookups", "SpeciesList210510.csv" )) 
Sp<- Sp %>% 
  select(PSMFC_Code, Common_Name, TripType_Description) %>% 
  mutate(PSMFC_Code = as.numeric(PSMFC_Code)) %>%
  filter(Common_Name != "bivalve class") #SP_CODE assocated with bivalve class is duplicated in lookup table. Should fix in source table eventually. 

# extract any catch data that uses a SP code that is not in the lookup table
notused2 <- oc %>%
  anti_join(Sp, by = c("SP_CODE" = "PSMFC_Code")) %>%
  mutate(Reason = "Species not found in Lookup.")
unique(notused2$SP_CODE)

#join together catch and species
oc_species <- oc %>%
  inner_join(Sp, by = c("SP_CODE" = "PSMFC_Code")) 

#check to make sure no duplicates were created or rows were lost, should return TRUE
nrow(oc) == (nrow(oc_species) + nrow(notused2))


# Format and link block data ----------------------------------------------

# function takes block column and microblock column and standardizes if populated
format_box = function(block_column, microblock_column) {
  combined = paste(block_column, microblock_column, sep="-")
  combined_noNA = gsub("NA", "", combined)
  combined_noblanks = ifelse(nchar(combined_noNA) < 5, "", combined_noNA)
  return(combined_noblanks)
}
# Read in location data, or the i8 data table for northern california
NLocList <- fread(file = here("RCode", "PR", "Dat04to15", "Data", "i8_1999-2016_NorCal_235374r.csv"), fill = TRUE, na.string = c("",".", "NA")) 

# create new id based on id code and location number, format boxes into XX-XXX format, apply to each block column
NLocList <- NLocList %>% 
  mutate(locn = ifelse(is.na(locn) & !is.na(locnum), locnum, locn)) %>%
  mutate(locn = ifelse(is.na(locn)| locn == 0, 1, locn),
         block1 = ifelse(block1 == 9999999, NA, block1),
         id_loc = as.character(paste(ID_CODE, locn, sep="")),
         Bk1Bx1a = format_box(block1, box1a),  
         Bk1Bx1b = format_box(block1, box1b), 
         Bk1Bx1c  = format_box(block1, box1c), 
         Bk2Bx2a  = format_box(block2, box2a), 
         Bk2Bx2b  = format_box(block2, box2b), 
         Bk2Bx2c  = format_box(block2, box2c)) %>%
  select(id_loc, survey, MODE_FX, HLDEPTH, HLDEPTH2, ddlat, ddlong, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, HGSIZE, hgsize2)

# read in i8 location data for southern california
SLocList <- fread(file = here("RCode", "PR", "Dat04to15", "Data", "i8_1999-2016_SoCal_204029r.csv"), fill = TRUE, na.string = c("",".", "NA")) 

locn = SLocList %>% group_by(locn, locnum) %>% count()
SLocList <- SLocList %>% 
  mutate(locn = ifelse(is.na(locn) & !is.na(locnum), locnum, locn)) %>%
  mutate(locn = ifelse(is.na(locn)| locn == 0, 1, locn),
         block1 = ifelse(block1 == 9999999, NA, block1),
         id_loc = as.character(paste(ID_CODE, locn, sep="")),
         Bk1Bx1a = format_box(block1, box1a),  
         Bk1Bx1b = format_box(block1, box1b), 
         Bk1Bx1c = format_box(block1, box1c), 
         Bk2Bx2a = format_box(block2, box2a), 
         Bk2Bx2b = format_box(block2, box2b), 
         Bk2Bx2c = format_box(block2, box2c)) %>%
  select(id_loc, survey, MODE_FX, HLDEPTH, HLDEPTH2, ddlat, ddlong, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, HGSIZE, hgsize2)

#combine north and south locaiton data together
loc = rbind(NLocList, SLocList)


loc  <- loc %>% filter(!is.na(id_loc))

# remove any data that is not PR (MODE_FX = 7)
notused3 = loc %>% 
  filter(MODE_FX != 7) %>%
  mutate(Reason = "Location not MODE_FX == 7 (PR data)")
unique(notused3$MODE_FX)

loc <- loc %>% 
  filter(MODE_FX ==7)

loc[loc==""]<-NA

before = nrow(loc)
# remove data that does not have block or coordiantes reported
notused4 = loc %>%
  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a) & is.na(Bk2Bx2b)  & is.na(Bk2Bx2c) & is.na(ddlat)) %>%
  mutate(Reason = "Location data does not include any block or coordinate data.")

removed = nrow(notused4)

loc = loc %>%
  filter(!is.na(Bk1Bx1a) | !is.na(Bk1Bx1b) | !is.na(Bk1Bx1c) | !is.na(Bk2Bx2a) | !is.na(Bk2Bx2b)  | !is.na(Bk2Bx2c) | !is.na(ddlat))
after = nrow(loc)

before - removed == after
# THIS IS OLD LOGIC FROM WINNS SCRIPTS, LOOKING AT THE DUPLICATE ID VALUES (notused5 below) NOT SURE WHY SOME OF THESE CANNOT BE USED

# count the number of times a unique id code appears in the location data
freq_summary <- loc %>% group_by(id_loc) %>% count()

#Calculate percentage of occurrence where frequency of singular id code > 1 and calculate the maximum number of duplicate ID codes
per = (nrow(filter(freq_summary, n > 1))/nrow(freq_summary))*100
message(paste(round(per,2), "% of ids have duplicates with a maximum of ", max(freq_summary$n), "duplicates."))


qa_duplicates = loc %>%
  left_join(freq_summary, by = "id_loc") %>%
  filter(n> 1) %>%
  arrange(id_loc)


# join back in this count data
dfr_loc <- loc %>%
  left_join(freq_summary, by = "id_loc") %>%
  arrange(id_loc) %>%
  rename(freq = n)

# remove data that has duplicate ID codes
notused5 = oc_species %>%
  inner_join(filter(dfr_loc, freq > 1), by = c("id"= "id_loc")) %>%
  mutate(Reason = "Duplicate ids are used in the location (i8) data table")

#filter to just ids used once
dfr_loc = filter(dfr_loc, freq == 1)

# Merge together catch and location data ----------------------------------
# joining variable is the id, select required columns
oc_species_loc <- oc_species %>%
  inner_join(dfr_loc, by = c("id"= "id_loc"))  %>% 
  select(id, date, month, year, SP_CODE, ALPHA5, Common_Name, TripType_Description, DISP3, WGT, FSHINSP, HLDEPTH, HLDEPTH2, ddlat, ddlong, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, HGSIZE, hgsize2)

#pull out data that is lost in the join (id does not have any location data)
notused6 <- oc_species %>%
  anti_join(dfr_loc, by = c("id"= "id_loc")) %>%
  mutate(Reason = "Does not have corresponding location data by ID")

#check that no ids were lost
(n_distinct(oc_species_loc$id_n) + n_distinct(notused6$id_n)) == n_distinct(oc_species$id_n)

# remove any blocks that have an hgsize inputted, this field is where the survey gives the "range" of blocks visited?
hgsize_summary = oc_species_loc %>% group_by(HGSIZE) %>% count()

BlkFilterHGSIZERemoved = oc_species_loc
BlkFilterHGSIZERemoved <- oc_species_loc  %>%
  mutate(Bk1Bx1a = ifelse(is.na(HGSIZE),Bk1Bx1a, NA),
         Bk1Bx1b = ifelse(is.na(HGSIZE),Bk1Bx1b, NA),
         Bk1Bx1c = ifelse(is.na(HGSIZE),Bk1Bx1c, NA),
         Bk2Bx2a = ifelse(is.na(hgsize2),Bk2Bx2a, NA),
         Bk2Bx2b = ifelse(is.na(hgsize2),Bk2Bx2b, NA),
         Bk2Bx2c = ifelse(is.na(hgsize2),Bk2Bx2c, NA))

# clean up the dataframe by changing any blank cells to NA
BlkFilterHGSIZERemoved[BlkFilterHGSIZERemoved==""]<-NA

#pull out the data that has blocks report
BlkFilter <- BlkFilterHGSIZERemoved  %>% 
  filter(!is.na(Bk1Bx1a) | !is.na(Bk1Bx1b) | !is.na(Bk1Bx1c) | !is.na(Bk2Bx2a) | !is.na(Bk2Bx2b)  | !is.na(Bk2Bx2c)) 

notused9 = BlkFilterHGSIZERemoved  %>% 
  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a) & is.na(Bk2Bx2b)  & is.na(Bk2Bx2c)) %>%
  mutate(Reason = "HGSize or HGSize2 reported ")

#pull out the data has has lat longs reported INSTEAD of blocks
LatFilter  <- BlkFilterHGSIZERemoved %>%  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a)  & is.na(Bk2Bx2b) & is.na(Bk2Bx2c) & !is.na(ddlat) & is.na(HGSIZE) & is.na(hgsize2)) %>%
  mutate(ddlong = ddlong*-1)



# qa the lat long filter, there is alot of bad coordinates
notused7 = LatFilter %>%
  filter(ddlat > 50 | ddlong < -150) %>%
  mutate(Reason = "Bad COORDS REPORTED")

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
notused8 = filter(coords_wblocks, is.na(Bk1Bx1a)) %>%
  mutate(Reason = "No fishing block overlapped with coordinate.")

#only data that had a block joined
coords_wblocks = filter(coords_wblocks, !is.na(Bk1Bx1a))

# recombine the data that had a block reported to the data that had coordinates (and now a joined block)
all_locations = rbind(BlkFilter, coords_wblocks)

# this counts the number of UNIQUE blocks reported in each row of data, simplifies the long repeated lines in WINNS code
all_locations <- cbind(all_locations, total_blocks = apply(all_locations[, Bk1Bx1a:Bk2Bx2c], 1, function(x)length(unique(x[!is.na(x)]))))

# Part 2 Aggregate Block Data ---------------------------------------------

# clean up fish and wight fields so NAs are 0s
dat <- all_locations %>% 
  mutate(fish = ifelse(!is.na(FSHINSP), FSHINSP, 0), 
         weight = as.numeric(ifelse(is.na(WGT) | WGT == "WGT", 0, WGT))) %>%
  select(-WGT)

# normalize the fish count to the number of blocks visited, REMOVED NORMALIZING WEIGHT TO BLOCKS BECAUSE YOU WANT THE ACTUAL WEIGHT OF THE FISH AND THE COUNT IS ALREADY NORMALIZED. 
dat <- dat %>% 
  mutate(FishPerBlock = fish/total_blocks)

# Create tally for groups of identical records for id_n, SP_CODE and FSHINSP. Data has a unique row for each weight data entered. Need to account for this in total fish count. THIS IS OLD LOGIC NEED TO CONFIRM or MAYBE SIMPLIFY WHY FSHINSP IS INCLUDED
df  <- dat %>% mutate(unique_id = paste(id, SP_CODE, FSHINSP, sep="_"))
dups = df %>% group_by(unique_id) %>% summarise(freq_id = n())

# join the freq_id counter to table so that total fish can be divided
df  <- df %>%
  inner_join(dups, by= "unique_id")

# sort fish and weight into released alive, released dead and kept based on DISP3 code (dont think this is necessary since this is observed data so everything is kept)c
disp_summary = df%>%
  group_by(DISP3) %>%
  count()

oc_sorted <- df %>% 
  mutate(FishPerBlock = ifelse(!is.na(FishPerBlock), FishPerBlock/freq_id, 0),
         weight = ifelse(!is.na(weight), weight, 0),
         Ob.RelAlive = ifelse(DISP3 %in% 1:2 | DISP3 %in% 7:9, FishPerBlock, 0 ),
         Ob.AvRelAliveWgt = ifelse(DISP3 %in% 1:2 | DISP3 %in% 7:9, weight, 0), 
         Ob.RelDead = ifelse(DISP3 %in% 6:6, FishPerBlock, 0 ), 
         Ob.AvRelDeadWgt  = ifelse(DISP3 %in% 6:6, weight, 0),
         Ob.Kept = ifelse(DISP3 %in% 3:5, FishPerBlock, 0),
         Ob.KWgt = ifelse(DISP3 %in% 3:5, weight, 0))

# pivot the data so each block reported for a single id has its own row. Counts are already normalized by blocks visited so this will not double count anything but greatly simplifies the logic that WINN uses to generate summary statistics
by_block = oc_sorted %>%
  pivot_longer(Bk1Bx1a:Bk2Bx2c, names_to = "col", values_to = 'Block') %>%
  filter(!is.na(Block))

# aggregates to the id-block-species level the total number of fish caught and the average weight, this output is later used in another script to calculate different metrics but I thought there would be some utility in keeping things at the ID level (easily aggregate to a variety of temporal or sample level metrics)
oc_by_id_agg_04_15 = by_block %>%
  group_by(id, date, month, year, Block,  SP_CODE, FSHINSP, Common_Name, freq_id) %>%
  summarise(Ob_ReleasedAlive = sum(Ob.RelAlive), 
            Ob_ReleasedDead = sum(Ob.RelDead), 
            Ob_Kept  = sum(Ob.Kept), 
            Ob_AvKWgt = mean(Ob.KWgt, na.rm=TRUE),
            n = n()) %>%
  mutate(Total_Fish_Caught = Ob_ReleasedAlive + Ob_ReleasedDead + Ob_Kept) %>%
  rename(Ob_Weighed_Fish = freq_id)


notused_summary = data.frame(c(unique(notused$Reason), unique(notused2$Reason), unique(notused3$Reason), unique(notused4$Reason), unique(notused5$Reason), unique(notused6$Reason), unique(notused7$Reason), unique(notused8$Reason), unique(notused9$Reason)), 
                             c(nrow(notused), nrow(notused2), nrow(notused3), nrow(notused4), nrow(notused5), nrow(notused6), nrow(notused7), nrow(notused8), nrow(notused9)))
names(notused_summary) = c("Reason", "Count")



write.csv(oc_by_id_agg_04_15, "Outputs/oc_04_15.csv", na = "", row.names = F)
write.csv(notused_summary, "Outputs/oc_04_15_notusedsummary.csv", na = "", row.names = F)

