# PR Reported Catch 2004_2015 ---------------------------------------------

# Michael Patton's simplification of original script (PR_ObservedEffort_2004_2015km Part 1 and art2.R)
#7/28/2022

# copy path to where you downloaded the shared CRFS_Mapping folder between the ()
working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping)"
setwd(working_directory)

#load in required packages, if R says the package is not installed then run the following code in the console below: install.packages("packagename") so for example install.packages("data.table")
library(data.table)
library(stringi)
library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(leaflet)
options(scipen = 999)

# to put the cart before the horse, this line is required when sourcing multiple R scripts. The plan is to run all required catch and effort scripts in a "master" script to avoid having to run everything one by one. This line makes sure the required objects are not removed when sourcing multiple scripts. Youll see it in the other scripts as well.
rm(list = ls()[!ls() %in% c("oc_by_id_agg_04_15", "oe_by_id_agg_04_15", "rc_by_id_agg_04_15", "oc_by_id_agg_16_19", "oc_by_id_agg_99_03")])

# read in the i2 table for reported catch
rc = fread(here("RCode", "PR", "Dat04to15", "Data", "PR_i2_2004-2015_429673r.csv"), fill = T, na.string = c("",".") )

# Not used data will be combined as a separate output for review. Reasons are provided in new column. 
notused = rc %>%
  filter(is.na(as.numeric(SP_CODE)))  %>%
  mutate(Reason = "SP_CODE is not valid.")
unique(notused$SP_CODE)

# remove invalid SP CODEs, create new id from ID_CODE and location number (locn), extract date, month and year. Select only needed columns. 
rc <- rc %>% 
  mutate(SP_CODE = as.numeric(SP_CODE)) %>%
  filter(!is.na(SP_CODE)) %>%
  mutate(locn = ifelse(is.na(locn) | locn == 0, 1, locn),
         id = paste(ID_CODE, locn, sep= ""), 
         date = ymd(stri_sub(ID_CODE, 6, 13)), 
         month = month(date)) %>%  
  select(id, date, month, year = YEAR, prim1, prim2, SP_CODE, MODE_F, MODE_FX, CNTRBTRS, DISPO, NUM_FISH) 

#Create species table by extracting data from SpeciesList.csv and merge species data with dfr_angrep data frame 
Sp<- fread(here("Lookups", "SpeciesList210510.csv" )) 
Sp<- Sp %>% 
  select(PSMFC_Code, Common_Name, TripType_Description, ALPHA5) %>% 
  mutate(PSMFC_Code = as.numeric(PSMFC_Code)) %>%
  filter(Common_Name != "bivalve class") 

notused2 <- rc %>%
  anti_join(Sp, by = c("SP_CODE" = "PSMFC_Code")) %>%
  mutate(Reason = "Species not found in Lookup.")
unique(notused2$SP_CODE)

# join catch data with species
rc_species <- rc %>%
  inner_join(Sp, by = c("SP_CODE" = "PSMFC_Code")) 

# make sure no duplicates or rows were lost in join
nrow(rc) == (nrow(rc_species) + nrow(notused2))

# Format and link block data ----------------------------------------------

# function that takes block column and microblock column and standardizes if populated
format_box = function(block_column, microblock_column) {
  combined = paste(block_column, microblock_column, sep="-")
  combined_noNA = gsub("NA", "", combined)
  combined_noblanks = ifelse(nchar(combined_noNA) < 5, "", combined_noNA)
  return(combined_noblanks)
}

# Read in location data, or the i8 data table for northern california
NLocList <- fread(file = here("RCode", "PR", "Dat04to15", "Data", "i8_1999-2016_NorCal_235374r.csv"), fill = TRUE, na.string = c("",".", "NA")) 

# creates new id_loc column from ID_CODE and location number (locn), apply above function to format box codes, select neccessary columns
NLocList <- NLocList %>% 
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

# southern california table
SLocList <- fread(file = here("RCode", "PR", "Dat04to15", "Data", "i8_1999-2016_SoCal_204029r.csv"), fill = TRUE, na.string = c("",".", "NA")) 

SLocList <- SLocList %>% 
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

loc = rbind(NLocList, SLocList)


loc  <- loc %>% filter(!is.na(id_loc))

notused3 = loc %>% 
  filter(MODE_FX != 7) %>%
  mutate(Reason = "Location not MODE_FX == 7 (PR data)")
unique(notused3$MODE_FX)

loc <- loc %>% 
  filter(MODE_FX ==7) %>%
  select(-MODE_FX)

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
# count the number of times a unique id code appears in the loction data
freq_summary <- loc %>% group_by(id_loc) %>% count()

#Calculate percentage of occurence where frequency of singular id code > 1 and calculate the maximum number of duplicate ID codes
per = (nrow(filter(freq_summary, n > 1))/nrow(freq_summary))*100
message(paste(round(per,2), "% of ids have duplicates with a maximum of ", max(freq_summary$n), "duplicates."))

dfr_loc <- loc %>%
  left_join(freq_summary, by = "id_loc") %>%
  arrange(id_loc) %>%
  rename(freq = n)

#should start an export that includes all data that is not used
notused5 = rc_species %>%
  inner_join(filter(dfr_loc, freq > 1), by = c("id"= "id_loc")) %>%
  mutate(Reason = "Duplicate ids are used in the location (i8) data table")

dfr_loc = filter(dfr_loc, freq == 1)

# Merge together catch and location data ----------------------------------

rc_species_loc <- rc_species %>%
  inner_join(dfr_loc, by = c("id"= "id_loc"))  %>% 
  select(id, date, month, year, SP_CODE, ALPHA5, Common_Name, TripType_Description, prim1, prim2, SP_CODE, MODE_F, MODE_FX, CNTRBTRS, DISPO, NUM_FISH, HLDEPTH, HLDEPTH2, ddlat, ddlong, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, HGSIZE, hgsize2)

notused6 <- rc_species %>%
  anti_join(dfr_loc, by = c("id"= "id_loc")) %>%
  mutate(Reason = "Does not have corresponding location data by ID")

(n_distinct(rc_species_loc$id_n) + n_distinct(notused6$id_n)) == n_distinct(rc_species$id_n)


#DO YOU USE AN HG SIZE OF 1 NEED TO LOOK AT THIS FIELD FURTHER
BlkFilterHGSIZERemoved <- rc_species_loc  %>% 
  mutate(Bk1Bx1a = ifelse(is.na(HGSIZE),Bk1Bx1a, NA), 
         Bk1Bx1b = ifelse(is.na(HGSIZE),Bk1Bx1b, NA), 
         Bk1Bx1c = ifelse(is.na(HGSIZE),Bk1Bx1c, NA), 
         Bk2Bx2a = ifelse(is.na(hgsize2),Bk2Bx2a, NA), 
         Bk2Bx2b = ifelse(is.na(hgsize2),Bk2Bx2b, NA), 
         Bk2Bx2c = ifelse(is.na(hgsize2),Bk2Bx2c, NA))

# clen up dataframe by replacing empty cells with NA
BlkFilterHGSIZERemoved[BlkFilterHGSIZERemoved==""]<-NA

# pull out data that has a block
BlkFilter <- BlkFilterHGSIZERemoved  %>% 
  filter(!is.na(Bk1Bx1a) | !is.na(Bk1Bx1b) | !is.na(Bk1Bx1c) | !is.na(Bk2Bx2a) | !is.na(Bk2Bx2b)  | !is.na(Bk2Bx2c)) 

notused10 = BlkFilterHGSIZERemoved  %>% 
  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a) & is.na(Bk2Bx2b)  & is.na(Bk2Bx2c)) %>%
  mutate(Reason = "HGSize or HGSize2 reported ")

# pull out data that has coordinates AND NO block
LatFilter  <- BlkFilterHGSIZERemoved %>%  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a)  & is.na(Bk2Bx2b) & is.na(Bk2Bx2c) & !is.na(ddlat) & is.na(HGSIZE) & is.na(hgsize2)) %>%
  mutate(ddlong = ddlong*-1)

notused7 = BlkFilterHGSIZERemoved %>%
  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a) & is.na(Bk2Bx2b)  & is.na(Bk2Bx2c) & is.na(ddlat)) %>%
  mutate(Reason = "Location data does not include any block or coordinate data.")

notused8 = LatFilter %>%
  filter(ddlat > 50 | ddlong < -150) %>%
  mutate(Reason = "Data has bad coordinates")

#filter out bad coordinates
LatFilter = LatFilter %>%
  filter(ddlat < 50 & ddlong > -150)

#convert to spatial dataframe, plotted by coordinates
coords.SP <- st_as_sf(LatFilter, coords = c("ddlong", "ddlat"), crs = 4326, remove = FALSE)

# read in shp of fishing blocks
blocks.SP <- st_read(here("Lookups", "MAN_CA_CRFS_microblocks2013.shp"))

# convert to WGS84
blocks.SP = st_transform(blocks.SP, crs = st_crs(coords.SP)) 
blocks.SP = select(blocks.SP, NM_INDEX)

# block data has some messy overlaps, need to turn off spherical geometry. ArcPRO does this on its own
sf_use_s2(FALSE)

# create widget to look at maps, lots of bad points
leaflet() %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  addCircleMarkers(data=coords.SP)

# spatial join to add blocks
coords_wblocks.SP = st_join(coords.SP, blocks.SP, left = TRUE)

# convert back to dataframe
coords_wblocks = coords_wblocks.SP %>%
  st_drop_geometry() %>%
  mutate(Bk1Bx1a = NM_INDEX) %>%
  select(colnames(BlkFilter))

#filter out data that did not join a block
notused9 = filter(coords_wblocks, is.na(Bk1Bx1a)) %>%
  mutate(Reason = "ID did not join with any location data")

coords_wblocks = filter(coords_wblocks, !is.na(Bk1Bx1a))

# recombine block and coordinate data
all_locations = rbind(BlkFilter, coords_wblocks)


# count the number of UNIQUE blocks in each row
all_locations <- cbind(all_locations, total_blocks = apply(all_locations[, Bk1Bx1a:Bk2Bx2c], 1, function(x)length(unique(x[!is.na(x)]))))

#normalize number of fish to the number of blocks visited
dat <- all_locations %>% 
  mutate(FishPerBlock = NUM_FISH/total_blocks)

# sort fish in Released Alive, Released Dead, and kept by DISPO code
dat   <- dat %>% 
  mutate(FishPerBlock = ifelse(!is.na(FishPerBlock), FishPerBlock, 0), 
         Rep.RelAlive  = ifelse(DISPO %in% 1:2 | DISPO %in% 7:9, FishPerBlock, 0 ), 
         Rep.RelDead = ifelse(DISPO %in% 6:6, FishPerBlock, 0 ), 
         Rep.Catch = ifelse(DISPO %in% 3:5, FishPerBlock, 0 ), 
         numfish = ifelse(is.na(NUM_FISH), 0, NUM_FISH))


# CONTRBTRS field creates duplicate entries for the same datapoint
duplicates = dat %>%
  group_by(id, SP_CODE, DISPO) %>%
  count()

dat = dat %>%
  left_join(duplicates)

# filter out data that had duplicate fishing counts, we may want to look into salvaging this data since contrbtrs is not important for catch
notused11 = dat %>%
  filter(n > 1) %>%
  mutate(Reason = "Duplicate IDs again")

rc_final = dat %>%
  filter(n == 1)

# pivot so each block-id has its own row. Makes summaries easier. Already normalized to fish per block so no double counting will occur
by_block = rc_final %>%
  pivot_longer(Bk1Bx1a:Bk2Bx2c, names_to = "col", values_to = 'Block') %>%
  filter(!is.na(Block))

rc_by_id_agg_04_15 = by_block %>%
  group_by(id, date, month, year, Block,  SP_CODE, Common_Name) %>%
  summarise(Rep_ReleasedAlive = sum(Rep.RelAlive), 
            Rep_ReleasedDead = sum(Rep.RelDead), 
            Rep_Kept  = sum(Rep.Catch)) %>%
  mutate(Total_Rep_Fish_Caught = Rep_ReleasedAlive +Rep_ReleasedDead  + Rep_Kept)

# create summary of data that is not used and the provided reason
notused_summary = data.frame(c(unique(notused$Reason), unique(notused2$Reason), unique(notused3$Reason), unique(notused4$Reason), unique(notused5$Reason), unique(notused6$Reason), unique(notused7$Reason), unique(notused8$Reason), unique(notused9$Reason), unique(notused10$Reason)), 
                             c(nrow(notused), nrow(notused2), nrow(notused3), nrow(notused4), nrow(notused5), nrow(notused6), nrow(notused7), nrow(notused8), nrow(notused9), nrow(notused10)))
names(notused_summary) = c("Reason", "Count")



write.csv(rc_by_id_agg_04_15, "Outputs/rc_04_15.csv", na = "", row.names = F)
write.csv(notused_summary, "Outputs/rc_04_15_notused_summary.csv", na = "", row.names = F)

