# Script to clean up and compile the i8 (location data)
# Sourced in the observed catch, reported catch, and observed effort scripts to add in cleaned up block data for each ID
# Michael Patton

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
  select(id_loc,ID_CODE, survey, MODE_FX, HLDEPTH, HLDEPTH2, ddlat, ddlong, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, HGSIZE, hgsize2)

# read in i8 location data for southern california
SLocList <- fread(file = here("RCode", "PR", "Dat04to15", "Data", "i8_1999-2016_SoCal_204029r.csv"), fill = TRUE, na.string = c("",".", "NA")) 

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
  select(id_loc,ID_CODE, survey, MODE_FX, HLDEPTH, HLDEPTH2, ddlat, ddlong, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, HGSIZE, hgsize2)

#combine north and south locaiton data together
loc = rbind(NLocList, SLocList)


# remove any data that is not PR (MODE_FX = 7)
notused = loc %>% 
  filter(MODE_FX != 7) %>%
  mutate(Reason = "Location not MODE_FX == 7 (PR data)")
unique(notused$MODE_FX)

loc <- loc %>% 
  filter(MODE_FX ==7)


loc[loc==""]<-NA

before = nrow(loc)
# remove data that does not have block or coordiantes reported
notused2 = loc %>%
  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a) & is.na(Bk2Bx2b)  & is.na(Bk2Bx2c) & is.na(ddlat)) %>%
  mutate(Reason = "Location data does not include any block or coordinate data.")

removed = nrow(notused2)

loc = loc %>%
  filter(!is.na(Bk1Bx1a) | !is.na(Bk1Bx1b) | !is.na(Bk1Bx1c) | !is.na(Bk2Bx2a) | !is.na(Bk2Bx2b)  | !is.na(Bk2Bx2c) | !is.na(ddlat))
after = nrow(loc)

before - removed == after

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
loc <- loc %>%
  left_join(freq_summary, by = "id_loc") %>%
  arrange(id_loc) %>%
  rename(freq = n)

# remove data that has duplicate ID codes
notused3 = filter(loc, freq > 1) %>%
  mutate(Reason = "Duplicate ids are used in the location (i8) data table")

#filter to just ids used once
loc = filter(loc, freq == 1)

# remove any blocks that have an hgsize inputted, this field is where the survey gives the "range" of blocks visited?
hgsize_summary = loc %>% group_by(HGSIZE) %>% count()

BlkFilterHGSIZERemoved <- loc  %>%
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

notused4 = BlkFilterHGSIZERemoved  %>% 
  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a) & is.na(Bk2Bx2b)  & is.na(Bk2Bx2c)) %>%
  mutate(Reason = "HGSize or HGSize2 reported ")

#pull out the data has has lat longs reported INSTEAD of blocks
LatFilter  <- BlkFilterHGSIZERemoved %>%  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a)  & is.na(Bk2Bx2b) & is.na(Bk2Bx2c) & !is.na(ddlat) & is.na(HGSIZE) & is.na(hgsize2)) %>%
  mutate(ddlong = ddlong*-1)

# qa the lat long filter, there is alot of bad coordinates
notused5 = LatFilter %>%
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
notused6 = filter(coords_wblocks, is.na(Bk1Bx1a)) %>%
  mutate(Reason = "No fishing block overlapped with coordinate.")

#only data that had a block joined
coords_wblocks = filter(coords_wblocks, !is.na(Bk1Bx1a))

# recombine the data that had a block reported to the data that had coordinates (and now a joined block)
all_locations = rbind(BlkFilter, coords_wblocks)


# this counts the number of UNIQUE blocks reported in each row of data, simplifies the long repeated lines in WINNS code
all_locations <- cbind(all_locations, total_blocks = apply(all_locations[, Bk1Bx1a:Bk2Bx2c], 1, function(x)length(unique(x[!is.na(x)]))))


notused_summary = data.frame(c(unique(notused$Reason), unique(notused2$Reason), unique(notused3$Reason), unique(notused4$Reason), unique(notused5$Reason), unique(notused6$Reason)), 
                             c(nrow(notused), nrow(notused2), nrow(notused3), nrow(notused4), nrow(notused5), nrow(notused6)))
names(notused_summary) = c("Reason", "Count")



write.csv(all_locations, "Outputs/location_cleaned.csv", na = "", row.names = F)
write.csv(notused_summary, "Outputs/location_notusedsummary.csv", na = "", row.names = F)

