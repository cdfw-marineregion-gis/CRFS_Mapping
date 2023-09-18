# PR Observed Catch ---------------------------------------------

# Michael Patton's (michael.patton@wildlife.ca.gov) R script to clean and aggregate the CRFS observed catch data (i3 table)

# copy path to where you downloaded the shared CRFS_Mapping folder between the ()
working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping)"
setwd(working_directory)

#load in required packages, if R says the package is not installed then run the following code in the console below install.packages("packagename") so for example install.packages("data.table")
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
rm(list = ls()[!ls() %in% c("oc_by_id_agg", "oe_by_id_agg", "rc_by_id_agg" ,'all_locations')])

#  read in i3 table: sampler observed catch data. the here function provides the relative path to where the data is saved. 
oc <- fread(file = here("RCode", "PR", "Dat04to15", "Data", "PR_i3_2004-2015_759607r.csv"), fill = TRUE, na.string = c("",".")) %>%
  mutate_all(as.character) 

# read in newer i3 data for 2016 to 2022. Newer data can be added here in the PR_readdata_16_present.R script
oc_16on <- fread(file = here('RCode', 'PR', 'Dat16toPresent', 'Data', 'i3_data_16to22.csv'), fill = TRUE) %>%
  mutate_all(as.character)

# see differences between old and new data in data schema. No changes needed
setdiff(names(oc_16on), names(oc))
# [1] "assnid"    "scan_rslt" "Ref #" 
setdiff(names(oc), names(oc_16on))

# bind together old and new data
oc = oc %>%
  bind_rows(oc_16on)

# remove any data that does not have a valid SP_CODE, these "notused" variables are later summarized in a separate output
notused = oc %>%
  filter(is.na(as.numeric(SP_CODE)))  %>%
  mutate(Reason = "SP_CODE is not valid.")
unique(notused$SP_CODE)
write.csv(notused, 'Outputs/NotUsed/i3/notused.csv', row.names = F, na = "")

# create a new id that combines the existing ID code with the location number. Extract the year, month and date. Select only the required columns. The NAs introduced by coercion warning message is for the SP Codes that are included in the not used object below.
oc <- oc %>% 
  mutate(SP_CODE = as.numeric(SP_CODE)) %>%
  filter(!is.na(SP_CODE)) %>%
  mutate(locn = ifelse(is.na(locn) | locn == 0, 1, locn),
         id = paste(ID_CODE, locn, sep= ""), 
         date = ymd(stri_sub(ID_CODE, 6, 13)), 
         month = month(date)) %>%  
  select(id, locn, date, month, year = YEAR, SP_CODE, MODE_F, DISP3, WGT, FSHINSP)

#Create species table by extracting data from SpeciesList.csv
Sp<- fread(here("Lookups", "SpeciesList05102023.csv" )) 
Sp<- Sp %>% 
  select(PSMFC_Code, Common_Name, TripType_Description) %>% 
  mutate(PSMFC_Code = as.numeric(PSMFC_Code)) %>%
  filter(Common_Name != "bivalve class") #SP_CODE assocated with bivalve class is duplicated in lookup table. Should fix in source table eventually. 

# extract any catch data that uses a SP code that is not in the lookup table
notused2 <- oc %>%
  anti_join(Sp, by = c("SP_CODE" = "PSMFC_Code")) %>%
  mutate(Reason = "Species not found in Lookup.")
unique(notused2$SP_CODE)
write.csv(notused2, 'Outputs/NotUsed/i3/notused2.csv', row.names = F, na = "")
byyear = notused2 %>% group_by(year, SP_CODE) %>% count()

#join together catch and species
oc_species <- oc %>%
  inner_join(Sp, by = c("SP_CODE" = "PSMFC_Code")) 

#check to make sure no duplicates were created or rows were lost, should return TRUE
nrow(oc) == (nrow(oc_species) + nrow(notused2))

# this will later be used to test for duplication from a bad join
test_id = oc_species %>%
  group_by(id) %>%
  count()

# Merge together catch and location data ----------------------------------
# joining variable is the id, select required columns
oc_species_loc <- oc_species %>%
  mutate(id = paste0('ID', id)) %>%
  inner_join(all_locations, by = c("id"= "id_loc"))  %>% 
  select(id, ID_CODE, locn, date, month, year = year.x, SP_CODE, Common_Name, TripType_Description, DISP3, WGT, FSHINSP, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, extrablock7,  extrablock8,  extrablock9,  extrablock10, extrablock11, total_blocks)


#pull out data that is lost in the join (id does not have any location data)
notused3 <- oc_species %>%
  mutate(id = paste0('ID', id)) %>%
  anti_join(all_locations, by = c("id"= "id_loc")) %>%
  mutate(Reason = "Does not have corresponding location data by ID")
byyear = notused3 %>% group_by(year) %>% count()
write.csv(notused3, 'Outputs/NotUsed/i3/notused3.csv', row.names = F, na = "")


# checks for a bad join where id is duplicated, should be 0. If not, need to review data
test_id2 = oc_species_loc %>%
  group_by(id, year) %>%
  count() %>%
  inner_join(test_id, by = 'id') %>%
  filter(n.x != n.y)

# clean up fish so NAs are 0s and WGT values in weight are NAs (inspected but not weighed)
oc_joined <- oc_species_loc %>% 
  mutate(FSHINSP = ifelse(!is.na(FSHINSP), as.numeric(FSHINSP), 0), 
         WGT = as.numeric(ifelse(WGT == "WGT", NA, WGT)))

# normalize the fish count to the number of blocks visited
oc_joined <- oc_joined %>% 
  mutate(FishPerBlock = FSHINSP/total_blocks)

# Create tally for groups of identical records for id_n, SP_CODE. Data has a unique row for each weight data entered. Need to account for this in total fish count. 
oc_joined  <- oc_joined %>% mutate(unique_id = paste(id, SP_CODE, sep="_"))
fishinspected_count = oc_joined %>% group_by(unique_id) %>% summarise(Ob_Weighed_Fish = n())

# join the freq_id counter to table so that total fish can be divided
oc_joined  <- oc_joined %>%
  inner_join(fishinspected_count, by= "unique_id")

# normalize the fish per block to remove the repeated data due to reporting each fish weight
oc_normalized <- oc_joined %>% 
  mutate(FishPerBlock = ifelse(!is.na(FishPerBlock), FishPerBlock/Ob_Weighed_Fish, 0))

# Leader follower PR2 angler form fix -------------------------------------
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

oc_normalized = oc_normalized %>%
  left_join(i1, by = 'ID_CODE') %>%
  mutate(ID_CODE = ifelse(!is.na(LEADER_ID), LEADER_ID, ID_CODE)) %>% # EFFORT DATA IS JOINED BY ID with no LOCN
  mutate(id = ifelse(!is.na(LEADER_ID), paste(LEADER_ID, locn, sep= ""), id))

oc_normalized = oc_normalized %>%
  mutate(row = row_number()) # create index of rows to not get rid of identical weighed fish in the unique logic used below

# pivot the data so each block reported for a single id has its own row. Counts are already normalized by blocks visited so this will not double count anything but greatly simplifies the logic that WINN uses to generate summary statistics
by_block = oc_normalized %>%
  pivot_longer(Bk1Bx1a:extrablock11, names_to = "col", values_to = 'Block') %>%
  filter(!is.na(Block)) %>%
  select(-col) %>%
  unique() # there are situations where the same block is entered mutlple times for the same id in the data. This is accounted for in the total blocks calculation but only a single instance of this block needs to be aggregated to avoid double counting. 

# aggregates to the id-block-species level the total number of fish caught and the average weight, this output is later used in another script to calculate different metrics but I thought there would be some utility in keeping things at the ID level (easily aggregate to a variety of temporal or sample level metrics)
oc_by_id_agg = by_block %>%
  group_by(id, ID_CODE, locn, year, Block,  SP_CODE, Common_Name) %>%
  summarise(Total_Obs_Fish_Caught  = sum(FishPerBlock, na.rm = T), 
            Ob_AvKWgt = mean(WGT, na.rm=TRUE),
            Ob_Weighed_Fish = sum(!is.na(WGT)))


# data validation to identify outlier data
outliers = oc_by_id_agg %>%
  mutate(Total_Obs_Fish_Caught = as.numeric(Total_Obs_Fish_Caught),
         Ob_AvKWgt = as.numeric(Ob_AvKWgt)) %>%
  group_by(Common_Name) %>%
  mutate(z_score_caught = scale(Total_Obs_Fish_Caught),
         z_score_weight = scale(Ob_AvKWgt))


weight_outliers = filter(outliers, z_score_weight > 5) %>%
  select(-z_score_caught) %>%
  arrange(desc(z_score_weight))
write.csv(weight_outliers, 'Outputs/PotentialOutliers/OC_weight.csv', na = "")

# final check for duplicates
final_check = oc_by_id_agg %>%
  group_by(id, Block, Common_Name) %>% 
  count() %>%
  filter(n > 1)

# export not used data
notused_summary = data.frame(c(unique(notused$Reason), unique(notused2$Reason), unique(notused3$Reason)), 
                             c(nrow(notused), nrow(notused2), nrow(notused3)))
names(notused_summary) = c("Reason", "Count")

write.csv(oc_by_id_agg, "Outputs/oc.csv", na = "", row.names = F)
write.csv(notused_summary, "Outputs/oc_notusedsummary.csv", na = "", row.names = F)

gc()
