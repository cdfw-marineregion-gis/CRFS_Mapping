# Standalone script to source all existing PR scripts (all years observed, reported catch and effort) and generate an output of CPUA (and other metrics) per block-year-species. Currently only set up for 04-15 data. 


#USER INPUT:
# copy path to where you downloaded the shared CRFS_Mapping folder between the ()
working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping)"
setwd(working_directory)


library(data.table)
library(stringi)
library(tidyverse)
library(lubridate)
library(here)
library(sf)
options(scipen = 999)

rm(list = ls())

# individually source the observed catch, reported catch and observed effort scripts. This results in the final cleaned and aggregated data at the id level. Will take awhile to run
source(here("RCode", "PR", "Dat04to15", "Ob.Catch", "PR_ObservedCatch_2004_2015_MP.R"))
source(here("RCode", "PR", "Dat04to15", "Ob.Effort", "PR_ObservedEffort_2004_2015_MP.R"))
source(here("RCode", "PR", "Dat04to15", "Rp.Catch", "PR_ReportedCatch_2004_2015_MP.R"))
# does reported effort need to be here?

#removes all of the unneeded parameters from the source
rm(list = ls()[!ls() %in% c("oc_by_id_agg_04_15", "oe_by_id_agg_04_15", "rc_by_id_agg_04_15")])

#read in species file
Sp <- fread(here("Lookups", "SpeciesList210510.csv" )) 
Sp <- Sp %>% 
  select(PSMFC_Code, Alpha = ALPHA5, Common_Name, TripType_Description) %>% 
  mutate(PSMFC_Code = as.numeric(PSMFC_Code)) %>%
  filter(Common_Name != "bivalve class")



# CPUA by ID --------------------------------------------------------------

all_by_id = oc_by_id_agg_04_15 %>%
  full_join(rc_by_id_agg_04_15, by = c("id","ID_CODE", "SP_CODE", "Block", "Common_Name", "date", 'month', 'year')) %>%
  left_join(Sp, by = c("Common_Name", "SP_CODE" = "PSMFC_Code")) %>%
  full_join(oe_by_id_agg_04_15, by = c("ID_CODE" = "id_noloc", "Block", "date", 'month', 'year'), suffix = c("_catch", "_effort")) %>%
  mutate(id = ifelse(is.na(id), ID_CODE, id))


cpue_combos = all_by_id %>%
  group_by(Common_Name_catch, Common_Name_effort, TripType_Description_catch, TripType_Description_effort) %>%
  count() %>%
  arrange(desc(n))


catch_noeffort = filter(all_by_id, !is.na(Common_Name_catch) & is.na(Common_Name_effort)) # may need to be removed
effort_nocatch = filter(all_by_id, is.na(Common_Name_catch) & !is.na(Common_Name_effort))
notused= catch_noeffort %>%
  mutate(Reason = "Catch was reported with no corresponding effort")

cpua= all_by_id %>%
  ungroup() %>%
  filter(!(!is.na(Common_Name_catch) & is.na(Common_Name_effort))) %>%
  select(id, date, month, year, Block, SP_CODE, Common_Name_catch, Common_Name_effort, TripType_Description_catch, TripType_Description_effort, Ob_Weighed_Fish, Ob_AvKWgt, Ob_Kept, Total_Obs_Fish_Caught, Rep_Released, Rep_Kept, Total_Rep_Fish_Caught, Days, Cntrbs, Vessels, AnglerDays)


# replaces NAs with 0s for the columns that will be aggregated
# calculates the total weight of weighed fish by multiplying the average wiehgt by the number of weighed fish. This is then later summed so average of average is not used for average weight. 
cpua = cpua %>%
  mutate(across(Ob_Weighed_Fish:AnglerDays, ~ifelse(is.na(.x),0,.x))) %>%
  mutate(fish_total_weight = Ob_AvKWgt*Ob_Weighed_Fish) %>%
  mutate(Total_Fish_Caught = Total_Obs_Fish_Caught + Total_Rep_Fish_Caught)

# more infitiy to sort out 
cpua_year_aggregated = cpua %>%
  group_by(year, Block, Common_Name_catch, TripType_Description_effort) %>%
  summarise(n_samples = n(),
            n_samples2 = n_distinct(id),
            Ob_Kept = sum(Ob_Kept, na.rm = T),
            Rep_Released = sum(Rep_Released, na.rm = T),
            Rep_Kept = sum(Rep_Kept, na.rm = T),
            Total_Fish_Caught = sum(Total_Fish_Caught, na.rm = T),
            AnglerDays = sum(AnglerDays, na.rm = T),
            Days = sum(Days, na.rm = T),
            Cntrbs = sum(Cntrbs, na.rm = T),
            Vessels = sum(Vessels, na.rm = T),
            fish_total_weight = sum(fish_total_weight, na.rm = T),
            fish_total_weighed = sum(Ob_Weighed_Fish, na.rm=T)) %>%
  mutate(Kept = Ob_Kept + Rep_Kept) %>%
  mutate(Ob_AvKWgt = fish_total_weight/fish_total_weighed) %>%
  mutate(CPUA = Total_Fish_Caught/AnglerDays) %>%
  select(-fish_total_weight, -fish_total_weighed, -Ob_Kept, -Rep_Kept) %>%
  select(year, Block, Common_Name_catch, TripType_Description_effort,  Rep_Released, Kept, Total_Fish_Caught, AnglerDays, Days, Cntrbs, Vessels, Ob_AvKWgt, CPUA)


# need to look into infinite values
cpua_year_aggregated = cpua_year_aggregated %>%
  mutate(CPUA = ifelse(is.infinite(CPUA), 0, CPUA))

write.csv(cpua_year_aggregated , "Outputs/PR_CPUA.csv", row.names = F, na = "")

test = cpua_year_aggregated %>% filter(Common_Name_catch == 'lingcod')

samplesize= cpua_year_aggregated %>%
  group_by(n_samples) %>% 
  summarise(count = n(),
            mean_cpua = mean(CPUA, na.rm = T),
            max_cpua = max(CPUA, na.rm = T))

nas = filter(cpua_year_aggregated, is.na(CPUA))

library(ggplot2)

ggplot(test, aes(n_samples, CPUA)) +
  geom_point() +
  theme_classic()
