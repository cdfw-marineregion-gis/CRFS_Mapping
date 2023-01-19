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
rm(list = ls()[!ls() %in% c("oc_by_id_agg_04_15", "oe_by_id_agg_04_15", "rc_by_id_agg_04_15", 'all_locations')])

# sources the script that is used to clean up the i8 table, returns a single variable 'all_locations' that provides the cleanup blocks at the ID level. Went through a series of filters as well. See other script for more information. 
source(here('RCode', "PR", "Locations", 'PR_Location.R'))


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

# Merge together catch and location data ----------------------------------

rc_species_loc <- rc_species %>%
  inner_join(all_locations, by = c("id"= "id_loc"))  %>% 
  select(id, ID_CODE, date, month, year, SP_CODE, ALPHA5, Common_Name, TripType_Description, prim1, prim2, SP_CODE, MODE_F, CNTRBTRS, DISPO, NUM_FISH, HLDEPTH, HLDEPTH2, ddlat, ddlong, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, extrablock7,  extrablock8,  extrablock9,  extrablock10, extrablock11, HGSIZE, hgsize2, total_blocks)

notused3 <- rc_species %>%
  anti_join(all_locations, by = c("id"= "id_loc")) %>%
  mutate(Reason = "Does not have corresponding location data by ID")



dat <- rc_species_loc %>% 
  mutate(FishPerBlock = NUM_FISH/total_blocks)


unique(dat$DISPO)
# sort fish in Released or kept by DISPO code
dat   <- dat %>% 
  mutate(FishPerBlock = ifelse(!is.na(FishPerBlock), FishPerBlock, 0)) %>%
  mutate(Rep.Released  = ifelse(DISPO %in% c(1,2,6), FishPerBlock, 0), 
         Rep.Kept = ifelse(DISPO %in% c(3,4,5,7), FishPerBlock, 0))

# CONTRBTRS field creates duplicate entries for the same datapoint
duplicates = dat %>%
  group_by(id, SP_CODE, DISPO) %>%
  count()

dat = dat %>%
  left_join(duplicates)

# filter out data that had duplicate fishing counts, we may want to look into salvaging this data since contrbtrs is not important for catch
notused4 = dat %>%
  filter(n > 1) %>%
  mutate(Reason = "Duplicate IDs again")

rc_final = dat %>%
  filter(n == 1)

# pivot so each block-id has its own row. Makes summaries easier. Already normalized to fish per block so no double counting will occur
by_block = rc_final %>%
  pivot_longer(Bk1Bx1a:extrablock11, names_to = "col", values_to = 'Block') %>%
  filter(!is.na(Block))

rc_by_id_agg_04_15 = by_block %>%
  group_by(id, ID_CODE, date, month, year, Block,  SP_CODE, Common_Name) %>%
  summarise(Rep_Released = sum(Rep.Released, na.rm = T), 
            Rep_Kept  = sum(Rep.Kept, na.rm = T)) %>%
  mutate(Total_Rep_Fish_Caught = Rep_Released + Rep_Kept)

# create summary of data that is not used and the provided reason
notused_summary = data.frame(c(unique(notused$Reason), unique(notused2$Reason), unique(notused3$Reason), unique(notused4$Reason)), 
                             c(nrow(notused), nrow(notused2), nrow(notused3), nrow(notused4)))
names(notused_summary) = c("Reason", "Count")



write.csv(rc_by_id_agg_04_15, "Outputs/rc_04_15.csv", na = "", row.names = F)
write.csv(notused_summary, "Outputs/rc_04_15_notused_summary.csv", na = "", row.names = F)

