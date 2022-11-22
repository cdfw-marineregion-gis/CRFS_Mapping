# PR Observed Effort 2004_2015 ---------------------------------------------

# Michael Patton's simplification of original script (PR_ObservedEffort_2004_2015km Part 1 and Part2.R)
#7/28/2022


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

# this line is required when sourcing multiple R scripts. The plan is to run all required catch and effort scripts in a "master" script to avoid having to run everything one by one. This line makes sure the required objects are not removed when sourcing multiple scripts. Youll see it in the other scripts as well.
rm(list = ls()[!ls() %in% c("oc_by_id_agg_04_15", "oe_by_id_agg_04_15", "by_id_agg_04_15", "by_id_agg_16_19", "by_id_agg_99_03")])

#read in the i1 table 
oe = fread(file=here("RCode", "PR", "Dat04to15", "Data", "PR_i1_2004-2015_487087r.csv"), fill = T, na.string = c("",".") )


### temporary code to investigate prim1 prim2 issue
Sp<- fread(here("Lookups", "SpeciesList210510.csv" )) 
Sp<- Sp %>% 
  select(PSMFC_Code, Common_Name, TripType_Description) %>% 
  mutate(PSMFC_Code = as.character(PSMFC_Code)) %>%
  filter(Common_Name != "bivalve class") %>%
  filter(!is.na(PSMFC_Code)) %>%
  mutate(PSMFC_Code = as.character(PSMFC_Code))

#create summary of the different prim1/prim2 combinations
prims_stat = oe %>%
  group_by(prim1, prim2) %>% 
  count() %>%
  inner_join(Sp, by = c("prim1" = "PSMFC_Code")) %>%
  mutate(prim1 = Common_Name) %>%
  select(-Common_Name) %>%
  rename(Trip_1 = TripType_Description) %>%
  inner_join(Sp, by = c("prim2" = "PSMFC_Code")) %>%
  mutate(prim2 = Common_Name) %>%
  select(-Common_Name) %>%
  rename(Trip_2 = TripType_Description) %>%
  arrange(desc(n)) %>%
  select(prim1, Trip_1, prim2, Trip_2, n)

#write.csv(prims_stat, "Observed_Effort_Prim1vPrim2.csv", row.names = FALSE, na = "")



# cleans up id, date, month and year. Pivots data so both prim 1 and prim 2 are considered. This differs from WINNS and ACCESS LOGIC SO NEED TO CONFIRM.Selects only relevant columns (NO LOCN IN EFFORT THIS WILL MAKE JOINS WITH CATCH COMPLICATED)
oe <- oe %>% 
  mutate(id = as.character(ID_CODE), 
         date = ymd(stri_sub(ID_CODE, 6, 13)), 
                    month = month(date), 
                    year = year(date))%>%
  select(id, year, month, date, prim1, prim2, CNTRBTRS, DAYSF, HRSF) %>%
  pivot_longer(cols=prim1:prim2, names_to= "primary", values_to = "sp_code") %>%
  filter(!is.na(sp_code))

# summarizeddata when PRIM is split
prims = oe %>%
  group_by(id) %>%
  summarize(split_prim = n())

#clean up DAYSF and CNTRBTRS fields so NAs are 0, if days are reported as 0 in the data but CNTRBTRS were reported, change the days fished to 1 (NEW LOGIC)
oe <- oe %>% 
  mutate(DAYSF = as.numeric(ifelse(is.na(DAYSF), 0, DAYSF)), 
         CNTRBTRS = as.numeric(ifelse(is.na(CNTRBTRS), 0 , CNTRBTRS))) %>%
  mutate(DAYSF = ifelse(DAYSF == 0 & CNTRBTRS > 0, 1, DAYSF))

# NEED TO EVENTUALLY DEVELOP LOGIC ABOUT HOW TO SPLIT EFFORT BETWEEN THE PRIMS OR IF THIS IS NECCESSARY
oe = oe %>%
  inner_join(prims, by = c("id")) #%>%
  #mutate(DAYSF = DAYSF/split_prim) 

#Create species table by extracting data from SpeciesList.csv and merge species data with dfr_angrep data frame 
Sp<- fread(here("Lookups", "SpeciesList210510.csv" )) 
Sp<- Sp %>% 
  select(ALPHA5, PSMFC_Code, Common_Name, TripType_Description) %>% 
  mutate(PSMFC_Code = as.character(PSMFC_Code)) %>%
  filter(Common_Name != "bivalve class") %>%
  filter(!is.na(PSMFC_Code)) %>%
  rename(alpha = ALPHA5)

# pull out data that is not used because it uses a bad sp_code
notused <- oe %>%
  anti_join(Sp, by = c("sp_code" = "PSMFC_Code")) %>%
  mutate(Reason = "SP CODE not found in Lookup.")
unique(notused$sp_code) # looks like a lot of sp codes re reported as the alpha code need to clean this up

# join species lookup to data
oe_species <- oe %>%
  inner_join(Sp, by = c("sp_code" = "PSMFC_Code"))

#summary of the different trip type descriptions found in the data
type_summary = oe_species %>%
  group_by(TripType_Description) %>%
  count() %>%
  arrange(desc(n))
# summary of the different common names of sought species in the data. NOTE MOST IS UNIDENTIFIED FISH
common_name_summary = oe_species %>% 
  group_by(Common_Name) %>%
  count() %>%
  arrange(desc(n))

# check to make sure no duplicates were created or rows were lost
nrow(oe) == (nrow(oe_species) + nrow(notused))

# Format and link block data ----------------------------------------------

#takes block column and microblock column and standardizes if populated
format_box = function(block_column, microblock_column) {
  combined = paste(block_column, microblock_column, sep="-")
  combined_noNA = gsub("NA", "", combined)
  combined_noblanks = ifelse(nchar(combined_noNA) < 5, "", combined_noNA)
  return(combined_noblanks)
}

# Read in location data, or the i8 data table 
NLocList <- fread(file = here("RCode", "PR", "Dat04to15", "Data", "i8_1999-2016_NorCal_235374r.csv"), fill = TRUE, na.string = c("",".", "NA")) 

#EFFORT DOES NOT USE THE ID PLUS locn COMBINATION
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
  select(ID_CODE, id_loc, survey, MODE_FX, HLDEPTH, HLDEPTH2, ddlat, ddlong, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, HGSIZE, hgsize2)

SLocList <- fread(file = here("RCode", "PR", "Dat04to15", "Data", "i8_1999-2016_SoCal_204029r.csv"), fill = TRUE, na.string = c("",".", "NA")) 

#EFFORT DOES NOT USE THE ID PLUS locn COMBINATION
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
  select(ID_CODE, id_loc, survey, MODE_FX, HLDEPTH, HLDEPTH2, ddlat, ddlong, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, HGSIZE, hgsize2)

# combine nor cal and s cal together
loc = rbind(NLocList, SLocList)
loc  <- loc %>% filter(!is.na(id_loc))

# extract data that uses a different MODE_FX
notused2 = loc %>% 
  filter(MODE_FX != 7) %>%
  mutate(Reason = "Location not MODE_FX == 7 (PR data)")
unique(notused2$MODE_FX)

loc <- loc %>% 
  filter(MODE_FX ==7)

loc[loc==""]<-NA

before = nrow(loc)
# remove data that does not have block or coordiantes reported
notused3 = loc %>%
  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a) & is.na(Bk2Bx2b)  & is.na(Bk2Bx2c) & is.na(ddlat)) %>%
  mutate(Reason = "Location data does not include any block or coordinate data.")

loc = loc %>%
  filter(!is.na(Bk1Bx1a) | !is.na(Bk1Bx1b) | !is.na(Bk1Bx1c) | !is.na(Bk2Bx2a) | !is.na(Bk2Bx2b)  | !is.na(Bk2Bx2c) | !is.na(ddlat))
after = nrow(loc)

#test to make sure filtering was applied correctly
before - removed == after

# count the number of times a unique id code appears in the loction data
freq_summary <- loc %>% group_by(ID_CODE) %>% count()

#Calculate percentage of occurence where frequency of singular id code > 1 and calculate the maximum number of duplicate ID codes. NOTE THAT THIS IS HIGHER IN THE EFFORT SCRIPTS BECAUSE LOCN IS NOT USED SO LOOKS FOR DUPLICATE IDS. THIS WILL RESULT IN A LOT OF GOOD DATA BEING LOST WHEN THERE IS ACTUALLY TWO LOCN TO AN ID
per = (nrow(filter(freq_summary, n > 1))/nrow(freq_summary))*100
message(paste(round(per,2), "% of ids have duplicates with a maximum of ", max(freq_summary$n), "duplicates."))

## EFFORT DOES NOT HAVE THE LOCATION SO THIS IS JOINED WITH THE IDCODE AND NOT ID + locn
dfr_loc <- loc %>%
  left_join(freq_summary, by = "ID_CODE") %>%
  arrange(ID_CODE) %>%
  rename(freq = n)

#remove data that has duplicate id data. AGAIN, I THINK GOOD DATA WHERE CATCH HAS MULTIPLE LOCN IS BEING FILTERED OUT INCORRECTLY HERE. MAY WANT TO REMOVE THIS CHECK ON THE EFFORT SIDE
notused4 = oe_species %>%
  inner_join(filter(dfr_loc, freq > 1), by = c("id"= "ID_CODE")) %>%
  mutate(Reason = "Duplicate ids are used in the location (i8) data table")

dfr_loc = filter(dfr_loc, freq == 1) %>%
  select(-freq)

# Merge together catch and location data ----------------------------------

oe_species_loc <- oe_species %>%
  inner_join(dfr_loc, by = c("id" = "ID_CODE"))

notused5 <- oe_species %>%
  anti_join(dfr_loc, by = c("id"= "ID_CODE")) %>%
  mutate(Reason = "Does not have corresponding location data by ID")

#test to make sure no id is lost
(n_distinct(oe_species_loc$id) + n_distinct(notused5$id)) == n_distinct(oe_species$id)

# remove any blocks that have an hgsize inputted, this field is where the survey gives the "range" of blocks visited?
HGSize_summary = oe_species_loc %>% group_by(HGSIZE) %>% count()
BlkFilterHGSIZERemoved <- oe_species_loc  %>% 
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

# Latitude referencing\ ---------------------------------------------------
#pull out the data has has lat longs reported INSTEAD of blocks
LatFilter  <- BlkFilterHGSIZERemoved %>%  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a)  & is.na(Bk2Bx2b) & is.na(Bk2Bx2c) & !is.na(ddlat) & is.na(HGSIZE) & is.na(hgsize2)) %>%
  mutate(ddlong = ddlong*-1)

# remove data that does not have block or coordiantes reported
notused6 = BlkFilterHGSIZERemoved %>%
  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a) & is.na(Bk2Bx2b)  & is.na(Bk2Bx2c) & is.na(ddlat)) %>%
  mutate(Reason = "Location data does not include any block or coordinate data.")

notused7 = LatFilter %>%
  filter(ddlat > 50 | ddlong < -150) %>%
  mutate(Reason = "Location has bad coordinates provided")

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

all_locations = as.data.table(all_locations)

# this counts the number of UNIQUE blocks reported in each row of data, simplifies the long repeated lines in WINNS code
all_locations <- cbind(all_locations, total_blocks = apply(all_locations[, Bk1Bx1a:Bk2Bx2c], 1, function(x)length(unique(x[!is.na(x)]))))

# Part 2 Aggregate Block Data ---------------------------------------------

# do NOT normalize effort to the number of blocks visited
dat <- all_locations %>% 
  mutate(DaysPerBlock = DAYSF, 
         CntrbPerBlock = CNTRBTRS, 
         VesselPerBlock = 1)

# pivot the data so each block reported for a single id has its own row. Counts are already normalized by blocks visited so this will not double count anything but greatly simplifies the logic that WINN uses to generate summary statistics
by_block = dat %>%
  pivot_longer(Bk1Bx1a:Bk2Bx2c, names_to = "col", values_to = 'Block') %>%
  filter(!is.na(Block))

# aggregate effort to the id-block-species level. This will later be aggreageted to the Triptype level but wanted to leave species in for now. INCLUDING PRIMARY AND SPLIT_PRIM IN THE GROUP CHANGES THE TOTAL NUMBER OF ROWS. NEED TO LOOK INTO THIS FURTHER. 
oe_by_id_agg_04_15 = by_block %>%
  group_by(id, date, month, year, Block, sp_code, Common_Name, TripType_Description, primary, split_prim) %>%
  summarise(DaysPerBlock = sum(DaysPerBlock, na.rm = T), 
            CntrbPerBlock = mean(CntrbPerBlock, na.rm = T), 
            VesselPerBlock = sum(VesselPerBlock, na.rm = T)) %>%
  mutate(AnglerDays = CntrbPerBlock*DaysPerBlock) %>%
  arrange(id)
nrow(oe_by_id_agg_04_15)

# create summary of data that is not used and the provided reason
notused_summary = data.frame(c(unique(notused$Reason), unique(notused2$Reason), unique(notused3$Reason), unique(notused4$Reason), unique(notused5$Reason), unique(notused6$Reason), unique(notused7$Reason), unique(notused8$Reason)), 
                             c(nrow(notused), nrow(notused2), nrow(notused3), nrow(notused4), nrow(notused5), nrow(notused6), nrow(notused7), nrow(notused8)))
names(notused_summary) = c("Reason", "Count")



write.csv(oe_by_id_agg_04_15, "Outputs/oe_04_15.csv", na = "", row.names = F)
write.csv(notused_summary, "Outputs/oe_04_15_notused_summary.csv", na = "", row.names = F)
