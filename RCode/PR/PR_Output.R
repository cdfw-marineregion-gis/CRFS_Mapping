# Standalone script to source all existing PR scripts (all years observed catch, reported catch and effort) and generate an output of CPUA (and other metrics) per block-year-species. 


#USER INPUT:
# copy path to where you downloaded the shared CRFS_Mapping folder between the ()
working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping)"
setwd(working_directory)


library(stringi)
library(tidyverse)
library(lubridate)
library(here)
library(sf)
options(scipen = 999)

rm(list = ls())

# individually source the observed catch, reported catch and observed effort scripts. This results in the final cleaned and aggregated data at the id level. Will take awhile to run. Will eventually move to just reading in .csv outputs of these scripts once finalized.
source(here("RCode", "PR", "Ob.Catch", "PR_ObservedCatch.R"))
source(here("RCode", "PR", "Effort", "PR_Effort.R"))
source(here("RCode", "PR", "Rp.Catch", "PR_ReportedCatch.R"))


#removes all of the unneeded parameters from the source
rm(list = ls()[!ls() %in% c("oc_by_id_agg", "oe_by_id_agg", "rc_by_id_agg")])

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


# CPUA by ID --------------------------------------------------------------

# the big join, full join of rc to oc by ID_loc, species, block. Bring in species info, then join in effort by ID (only) and block with no species. 
all_by_id = oc_by_id_agg %>%
  full_join(rc_by_id_agg, by = c("id","ID_CODE", "SP_CODE", "Block", "Common_Name", "date", 'month', 'year')) %>%
  left_join(Sp, by = c("Common_Name", "SP_CODE" = "PSMFC_Code")) %>%
  full_join(oe_by_id_agg, by = c("ID_CODE" = "id_noloc", "Block", "date", 'month', 'year'), suffix = c("_catch", "_effort")) %>%
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


# remove catch where there is no effort and clean up table to neccessary fields
all_by_id= all_by_id %>%
  ungroup() %>%
  filter(!(!is.na(Common_Name_catch) & is.na(Common_Name_effort))) %>%
  select(id, date, month, year, Block, SP_CODE, Common_Name_catch, Common_Name_effort, TripType_Description_catch, TripType_Description_effort, Ob_Weighed_Fish, Ob_AvKWgt, Ob_Kept, Total_Obs_Fish_Caught, Rep_Released, Rep_Kept, Total_Rep_Fish_Caught, Days, Cntrbs, Vessels, AnglerDays)


# replaces NAs with 0s for the columns that will be aggregated
# calculates the total weight of weighed fish by multiplying the average weight by the number of weighed fish. This is then later summed so average of average is not used for average weight. 
all_by_id = all_by_id %>%
  mutate(across(Ob_Weighed_Fish:AnglerDays, ~ifelse(is.na(.x),0,.x))) %>%
  mutate(fish_total_weight = Ob_AvKWgt*Ob_Weighed_Fish) %>%
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

not_in_shape = filter(all_by_id, !(Block %in% blocks.SP$NM_INDEX)) %>%
  group_by(Block) %>%
  count()

write.csv(all_by_id, 'Outputs/all_by_id.csv', row.names = F, na = "")




