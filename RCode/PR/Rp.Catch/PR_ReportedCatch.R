# PR Reported Catch ---------------------------------------------

# Michael Patton (michael.patton@wildlife.ca.gov)

# copy path to where you downloaded the shared CRFS_Mapping folder between the ()
working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping)"
setwd(working_directory)

#load in required packages, if R says the package is not installed then run the following code in the console below: install.packages("packagename") so for example install.packages("data.table")
library(data.table)
library(stringi)
library(tidyverse)
library(lubridate)
library(here)
options(scipen = 999)

# reads in the .csv exported from the locations script. This replaced the previous sourcing logic for speed. IF CHANGES HAVE BEEN MADE TO THE PR_LOCATION.R SCRIPT THEN THAT SCRIPT NEEDS TO BE RERAN TO HAVE THE MOST UP TO DATE DATA
all_locations = read.csv("Outputs/location_cleaned.csv", na.strings = "") %>%
  mutate_all(as.character) %>%
  mutate(total_blocks = as.numeric(total_blocks))

# memory variable clean up in case user is running multiple scripts
rm(list = ls()[!ls() %in% c("oc_by_id_agg", "oe_by_id_agg", "rc_by_id_agg", 'all_locations')])

# read in the i2 table for reported catch for 2004-2015
rc = fread(here("RCode", "PR", "Dat04to15", "Data", "PR_i2_2004-2015_429673r.csv"), fill = T, na.string = c("",".") )  %>%
  mutate_all(as.character) 

# read in 2016-2022 data. New years of data can be added in the PR_readdata_16_present.R
rc_16on <- fread(file = here('RCode', 'PR', 'Dat16toPresent', 'Data', 'i2_data_16to22.csv'), fill = TRUE) %>%
  mutate_all(as.character)

# see difference in column names between new and old data. No changes needed
setdiff(names(rc_16on), names(rc))
# [1] "assnid"    "scan_rslt" "Ref #" 
setdiff(names(rc), names(rc_16on))

# combine all years of data
rc = rc %>%
  bind_rows(rc_16on) %>%
  unique() # for whatever reason there are some duplicated rows

# Not used data will be combined as a separate output for review. Reasons are provided in new column. 
notused = rc %>%
  filter(is.na(as.numeric(SP_CODE)))  %>% # forces invalid SP CODES (not numbers) to be NAs and are therefore removed, responsible for the warning you recieve
  mutate(Reason = "SP_CODE is not valid.")
unique(notused$SP_CODE)
write.csv(notused, 'Outputs/NotUsed/i2/notused.csv', row.names = F, na = "")

# remove invalid SP CODEs, create new id from ID_CODE and location number (locn). Select only needed columns. 
rc <- rc %>% 
  mutate(SP_CODE = as.numeric(SP_CODE)) %>%
  filter(!is.na(SP_CODE)) %>%
  mutate(locn = ifelse(is.na(locn) | locn == 0, 1, locn),
         id = paste(ID_CODE, locn, sep= "")) %>%  
  select(id, locn, year = YEAR, SP_CODE, DISPO, NUM_FISH)

#Create species table by extracting data from SpeciesList.csv
Sp<- fread(here("Lookups", "SpeciesList05102023.csv" )) 
Sp<- Sp %>% 
  select(PSMFC_Code, Common_Name, TripType_Description, ALPHA5) %>% 
  mutate(PSMFC_Code = as.numeric(PSMFC_Code)) %>%
  filter(Common_Name != "bivalve class") 

# remove data from rc that has an SP_CODE not found in the species lookup
notused2 <- rc %>%
  anti_join(Sp, by = c("SP_CODE" = "PSMFC_Code")) %>%
  mutate(Reason = "Species not found in Lookup.")
unique(notused2$SP_CODE)

byyear = notused2 %>% group_by(year, SP_CODE) %>% count()
write.csv(notused2, 'Outputs/NotUsed/i2/notused2.csv', row.names = F, na = "")

# join catch data with species
rc_species <- rc %>%
  inner_join(Sp, by = c("SP_CODE" = "PSMFC_Code")) 

# make sure no duplicates or rows were lost in join
nrow(rc) == (nrow(rc_species) + nrow(notused2))

# Merge together catch and location data ----------------------------------
rc_species_loc <- rc_species  %>%
  mutate(id = paste0('ID', id)) %>%
  inner_join(all_locations, by = c("id"= "id_loc"))  %>% 
  select(id, ID_CODE, locn, year = year.x, SP_CODE, Common_Name, TripType_Description, DISPO, NUM_FISH, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, extrablock7,  extrablock8,  extrablock9,  extrablock10, extrablock11, total_blocks)

# export data that does not have corresponding location data. 
notused3 <- rc_species %>%
  mutate(id = paste0('ID', id)) %>%
  anti_join(all_locations, by = c("id"= "id_loc")) %>%
  mutate(Reason = "Does not have corresponding location data by ID")
byyear = notused3 %>% group_by(year) %>% count()
write.csv(notused3, 'Outputs/NotUsed/i2/notused3.csv', row.names = F, na = "")

#normalize the reported fish caught by the number of blocks visited
rc_joined <- rc_species_loc %>% 
  mutate(FishPerBlock = ifelse(!is.na(NUM_FISH), as.numeric(NUM_FISH)/total_blocks, 0))

unique(rc_joined$DISPO)
# sort fish in Released or kept by DISPO code
rc_joined = rc_joined %>% 
  mutate(RepReleasedAlive  = ifelse(DISPO %in% c(1,2), FishPerBlock, 0), 
         RepReleasedDead = ifelse(DISPO %in% c(6), FishPerBlock, 0),
         RepKept = ifelse(DISPO %in% c(3,4,5,7), FishPerBlock, 0))

# small number of remaining duplicates from bad data entry
duplicates = rc_joined %>%
  group_by(id, SP_CODE, DISPO) %>%
  count()

# join back to identify duplicates
rc_joined = rc_joined %>%
  left_join(duplicates)

# filter out data that had duplicate fishing counts, we may want to look into salvaging this data since contrbtrs is not important for catch
notused4 = rc_joined %>%
  filter(n > 1) %>%
  mutate(Reason = "Duplicate IDs caused by data entry")
byyear = notused4 %>% group_by(year) %>% count()
write.csv(notused4, 'Outputs/NotUsed/i2/notused4.csv', row.names = F, na = "")

rc_joined = rc_joined %>%
  filter(n == 1) %>%
  mutate(row = row_number())

# pivot so each block-id has its own row. Makes summaries easier. Already normalized to fish per block so no double counting will occur
by_block = rc_joined %>%
  pivot_longer(Bk1Bx1a:extrablock11, names_to = "col", values_to = 'Block') %>%
  filter(!is.na(Block)) %>%
  select(-col) %>%
  unique() # there are situations where the same block is entered mutlple times for the same id in the data. This is accounted for in the total blocks calculation but only a single instance of this block needs to be aggregated to avoid double counting. 

# read in i1 table to identify the leaders for pre2014 PR2 data
i1 = fread(file=here("RCode", "PR", "Dat04to15", "Data", "PR_i1_2004-2015_487087r.csv"), fill = T, na.string = c("",".") )%>%
  filter(YEAR <= 2013 & is.na(survey)) %>%
  mutate_all(as.character) %>%
  select(ID_CODE, LEADER, PRT_CODE) %>%
  mutate(LEADER_ID = ifelse(!is.na(LEADER), LEADER, PRT_CODE)) %>%
  mutate(LEADER_ID = ifelse(is.na(LEADER_ID), ID_CODE, LEADER_ID)) %>%
  select(ID_CODE, LEADER_ID) %>%
  mutate(ID_CODE = paste0('ID', ID_CODE),
         LEADER_ID = paste0('ID', LEADER_ID))

by_block_wleader = by_block %>%
  left_join(i1, by = 'ID_CODE') %>%
  mutate(ID_CODE = ifelse(!is.na(LEADER_ID), LEADER_ID, ID_CODE)) %>% # EFFORT DATA IS JOINED BY ID with no LOCN
  mutate(id = ifelse(!is.na(LEADER_ID), paste(LEADER_ID, locn, sep= ""), id))

# final aggregation by ID - block - species
rc_by_id_agg = by_block_wleader %>%
  group_by(id, ID_CODE, year, Block,  SP_CODE, Common_Name) %>%
  summarise(Rep_Released_Alive = sum(RepReleasedAlive, na.rm = T), 
            Rep_Released_Dead = sum(RepReleasedDead, na.rm = T), 
            Rep_Kept  = sum(RepKept, na.rm = T)) %>%
  mutate(Total_Rep_Fish_Caught = Rep_Released_Alive + Rep_Kept + Rep_Released_Dead) %>%
  mutate(Rep_Released = Rep_Released_Alive + Rep_Released_Dead)




# final check for duplicates
finalcheck = rc_by_id_agg %>%
  group_by(id, Block, Common_Name) %>%
  count() %>%
  filter(n > 1)

# create summary of data that is not used and the provided reason, manually write in notused2 reason as a temp fix because it went down to 0 rows
notused_summary = data.frame(c(unique(notused$Reason), 'SP Code not found in lookup', unique(notused3$Reason), unique(notused4$Reason)), 
                             c(nrow(notused), nrow(notused2), nrow(notused3), nrow(notused4)))
names(notused_summary) = c("Reason", "Count")

write.csv(rc_by_id_agg, "Outputs/rc.csv", na = "", row.names = F)
write.csv(notused_summary, "Outputs/rc_notused_summary.csv", na = "", row.names = F)


gc()