# Standalone script to compile all existing PR script outputs (all years observed catch, reported catch and effort) together to a cleaned up version containing all information by ID

#USER INPUT:
# copy path to where you downloaded the shared CRFS_Mapping folder between the ()

rm(list = ls())

working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping)"
setwd(working_directory)

library(data.table)
library(stringi)
library(tidyverse)
library(lubridate)
library(here)
library(sf)
options(scipen = 999)

# section to rerun each of the initial scripts to make sure the most up-to-date data is being used to generate the final output. This section can be skipped if confident that no changes have been made in the underlying location, catch and effort scripts
source('RCode/PR/Locations/PR_Location.R')
source('RCode/PR/Ob.Catch/PR_ObservedCatch.R')
source('RCode/PR/Rp.Catch/PR_ReportedCatch.R')
source('RCode/PR/Effort/PR_Effort.R')

# read in the individual outputs from the observed catch, reported catch and effort scripts.
oc_by_id_agg = read.csv("Outputs/oc.csv", na.strings = "")
rc_by_id_agg = read.csv("Outputs/rc.csv", na.strings = "")
oe_by_id_agg = read.csv("Outputs/oe.csv", na.strings = "")

#read in species file
Sp <- fread(here("Lookups", "SpeciesList05102023.csv" )) 
Sp <- Sp %>% 
  select(PSMFC_Code, Alpha = ALPHA5, Common_Name, TripType_Description) %>% 
  mutate(PSMFC_Code = as.numeric(PSMFC_Code)) %>%
  filter(Common_Name != "bivalve class")

rc_by_id_agg = rc_by_id_agg %>%
  mutate(year = as.character(year))
oe_by_id_agg = oe_by_id_agg %>%
  mutate(year = as.character(year))
oc_by_id_agg = oc_by_id_agg %>%
  mutate(year = as.character(year))


# CPUA by ID --------------------------------------------------------------

# the big join: full join of rc to oc by ID_loc, species, block. Bring in species info, then join in effort by ID (only) and block. 
all_by_id = oc_by_id_agg %>%
  full_join(rc_by_id_agg, by = c("id","ID_CODE", "SP_CODE", "Block", "Common_Name", 'year')) %>%
  left_join(Sp, by = c("Common_Name", "SP_CODE" = "PSMFC_Code")) %>%
  full_join(oe_by_id_agg, by = c("ID_CODE" = "id_noloc", "Block", 'year'), suffix = c("_catch", "_effort")) %>%
  mutate(id = ifelse(is.na(id), ID_CODE, id)) #for effort with no catch snariors

# summary tables to see al combos of catch vs effort
cpue_combos = all_by_id %>%
  group_by(Common_Name_catch, Common_Name_effort, TripType_Description_catch, TripType_Description_effort) %>%
  count() %>%
  arrange(desc(n))


# check for bad matches between catch and effort
catch_noeffort = filter(all_by_id, !is.na(Common_Name_catch) & is.na(Common_Name_effort)) 
effort_nocatch = filter(all_by_id, is.na(Common_Name_catch) & !is.na(Common_Name_effort))
notused= catch_noeffort %>%
  mutate(Reason = "Catch was reported with no corresponding effort")
byyear = notused %>% group_by(year) %>% count()
write.csv(notused, 'Outputs/NotUsed/notused.csv', row.names = F, na = "")


# remove catch where there is no effort and clean up table to neccessary fields
all_by_id= all_by_id %>%
  ungroup() %>%
  filter(!(!is.na(Common_Name_catch) & is.na(Common_Name_effort))) %>%
  select(id, year, Block, SP_CODE, Common_Name_catch, Common_Name_effort, TripType_Description_catch, TripType_Description_effort, Ob_Weighed_Fish, Ob_AvKWgt, Total_Obs_Fish_Caught, Rep_Released_Dead, Rep_Kept, Rep_Released_Alive, Total_Rep_Fish_Caught, Days, Cntrbs, Vessels, AnglerDays)


# replaces NAs with 0s for the columns that will be aggregated
# calculates the total weight of weighed fish by multiplying the average weight by the number of weighed fish. This is then later summed so average of average is not used for average weight. 
all_by_id = all_by_id %>%
  mutate(across(Ob_Weighed_Fish:AnglerDays, ~ifelse(is.na(.x),0,.x))) %>%
  mutate(Total_Fish_Caught = Total_Obs_Fish_Caught + Total_Rep_Fish_Caught)

# final check for duplicates
dups = all_by_id %>%
  group_by(id, Block, Common_Name_catch, TripType_Description_effort) %>%
  count() %>%
  filter(n > 1)

# check for blocks not found in shapefile
# bring in the block shapefile from lookup folder
blocks.SP <- st_read(here("Lookups", "MAN_CA_CRFS_microblocks2013.shp"))
# change projection of blocks to WGS84 (what the coordinates are)
blocks.SP = st_transform(blocks.SP, crs = 4326) 
blocks.SP = select(blocks.SP, NM_INDEX)

# identify data that is associated with blocks that is not in the lookup table
notused2 = filter(all_by_id, !(Block %in% blocks.SP$NM_INDEX)) %>%
  mutate(Reason = 'Block is not in reference shapefile')
not_in_shape = notused2 %>%
  group_by(Block) %>%
  count()
write.csv(notused2, 'Outputs/NotUsed/notused2.csv' )

# data validation
outliers = all_by_id %>%
  ungroup() %>%
  mutate(Total_CPUA = Total_Fish_Caught/AnglerDays,
         Obs_CPUA = Total_Obs_Fish_Caught/AnglerDays,
         Rep_CPUA = Total_Rep_Fish_Caught/AnglerDays) %>%
  group_by(Common_Name_catch) %>%
  mutate(z_score_obs = scale(Obs_CPUA),
         z_score_rep = scale(Rep_CPUA))

obs_outliers = filter(outliers, z_score_obs >10) %>%
  arrange(desc(z_score_obs)) %>%
  select(-SP_CODE, -Common_Name_effort, -TripType_Description_catch, -Vessels, -z_score_rep)
write.csv(obs_outliers, 'Outputs/PotentialOutliers/ObsFish_Outliers.csv', na = "")

rc_outliers = filter(outliers, z_score_rep > 10) %>%
  arrange(desc(z_score_rep)) %>%
  select(-SP_CODE, -Common_Name_effort, -TripType_Description_catch, -Vessels, -z_score_obs)
write.csv(rc_outliers, 'Outputs/PotentialOutliers/RepFish_Outliers.csv', na = "")




write.csv(all_by_id, 'Outputs/all_by_id.csv', row.names = F, na = "")

gc() # clear session memory


