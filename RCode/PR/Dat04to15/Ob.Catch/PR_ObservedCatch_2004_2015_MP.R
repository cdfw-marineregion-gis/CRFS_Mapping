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
rm(list = ls()[!ls() %in% c("oc_by_id_agg_04_15", "oe_by_id_agg_04_15", "rc_by_id_agg_04_15" ,'all_locations')])

# sources the script that is used to clean up the i8 table, returns a single variable 'all_locations' that provides the cleanup blocks at the ID level. Went through a series of filters as well. See other script for more information. 
source(here('RCode', "PR", "Locations", 'PR_Location.R'))

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



# Merge together catch and location data ----------------------------------
# joining variable is the id, select required columns
oc_species_loc <- oc_species %>%
  inner_join(all_locations, by = c("id"= "id_loc"))  %>% 
  select(id, ID_CODE, date, month, year, SP_CODE, ALPHA5, Common_Name, TripType_Description, DISP3, WGT, FSHINSP, HLDEPTH, HLDEPTH2, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, total_blocks)

#pull out data that is lost in the join (id does not have any location data)
notused3 <- oc_species %>%
  anti_join(all_locations, by = c("id"= "id_loc")) %>%
  mutate(Reason = "Does not have corresponding location data by ID")


# Part 2 Aggregate Block Data ---------------------------------------------

# clean up fish and wight fields so NAs are 0s
dat <- oc_species_loc %>% 
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
  count() %>%
  mutate(sorted = case_when(DISP3 %in% 1:2 | DISP3 %in% 7:9 ~ 'Released Alive',
                            DISP3 %in% 6 ~ 'Released Dead',
                            DISP3 %in% 3:5 ~ ' Kept')) %>%
  mutate(table = 'Observed Catch 04-15') %>%
  select(table, sorted, DISP3, n) %>%
  arrange(DISP3)


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
  group_by(id, ID_CODE, date, month, year, Block,  SP_CODE, FSHINSP, Common_Name, freq_id) %>%
  summarise(Ob_ReleasedAlive = sum(Ob.RelAlive, na.rm = T), 
            Ob_ReleasedDead = sum(Ob.RelDead, na.rm = T), 
            Ob_Kept  = sum(Ob.Kept, na.rm = T), 
            Ob_AvKWgt = mean(Ob.KWgt, na.rm=TRUE)) %>%
  mutate(Total_Obs_Fish_Caught = Ob_ReleasedAlive + Ob_ReleasedDead + Ob_Kept) %>%
  rename(Ob_Weighed_Fish = freq_id)


notused_summary = data.frame(c(unique(notused$Reason), unique(notused2$Reason), unique(notused3$Reason)), 
                             c(nrow(notused), nrow(notused2), nrow(notused3)))
names(notused_summary) = c("Reason", "Count")



write.csv(oc_by_id_agg_04_15, "Outputs/oc_04_15.csv", na = "", row.names = F)
write.csv(notused_summary, "Outputs/oc_04_15_notusedsummary.csv", na = "", row.names = F)

