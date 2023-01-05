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
  full_join(oe_by_id_agg_04_15, by = c("ID_CODE" = "id_noloc", "Block", "date", 'month', 'year'), suffix = c("_catch", "_effort"))


cpue_combos = all_by_id %>%
  group_by(Common_Name_catch, Common_Name_effort, TripType_Description_catch, TripType_Description_effort, primary) %>%
  count() %>%
  arrange(desc(n))


catch_noeffort = filter(all_by_id, !is.na(Common_Name_catch) & is.na(Common_Name_effort)) # may need to be removed
effort_nocatch = filter(all_by_id, is.na(Common_Name_catch) & !is.na(Common_Name_effort))
notused= catch_noeffort %>%
  mutate(Reason = "Catch was reported with no corresponding effort")

cpua= all_by_id %>%
  ungroup() %>%
  filter(!(!is.na(Common_Name_catch) & is.na(Common_Name_effort))) %>%
  select(id, date, month, year, Block, SP_CODE, Common_Name_catch, Common_Name_effort, TripType_Description_catch, TripType_Description_effort, primary, Ob_Weighed_Fish, Ob_AvKWgt, Ob_ReleasedDead, Ob_ReleasedAlive, Ob_Kept, Total_Obs_Fish_Caught, Rep_ReleasedAlive, Rep_ReleasedDead, Rep_Kept, Total_Rep_Fish_Caught, Days, Cntrbs, Vessels, AnglerDays)


# DEPENDING ON DISP3 OUTCOME CERTAIN AGGREGATES SHOULD BE INCLUDED VS REMOVED (REP ALIVE etc)
cpua = cpua %>%
  mutate(across(Ob_Weighed_Fish:AnglerDays, ~ifelse(is.na(.x),0,.x))) %>%
  mutate(fish_total_weight = Ob_AvKWgt*Ob_Weighed_Fish) %>%
  mutate(Total_Fish_Caught = Total_Obs_Fish_Caught + Total_Rep_Fish_Caught)

# more infitiy to sort out 
cpua = cpua %>%
  group_by(year, Block, Common_Name_catch, TripType_Description_effort) %>%
  summarise(n_id = n(),
            Ob_ReleasedAlive = sum(Ob_ReleasedAlive, na.rm = T),
            Ob_ReleasedDead = sum(Ob_ReleasedDead, na.rm = T),
            Ob_Kept = sum(Ob_Kept, na.rm = T),
            Rep_ReleasedAlive = sum(Rep_ReleasedAlive, na.rm = T),
            Rep_ReleasedDead = sum(Rep_ReleasedDead, na.rm = T),
            Rep_Kept = sum(Rep_Kept, na.rm = T),
            Total_Fish_Caught = sum(Total_Fish_Caught, na.rm = T),
            AnglerDays = sum(AnglerDays, na.rm = T),
            Days = sum(Days, na.rm = T),
            Cntrbs = sum(Cntrbs, na.rm = T),
            Vessels = sum(Vessels, na.rm = T),
            fish_total_weight = sum(fish_total_weight, na.rm = T),
            fish_total_weighed = sum(Ob_Weighed_Fish, na.rm=T)) %>%
  mutate(Ob_AvKWgt = fish_total_weight/fish_total_weighed) %>%
  mutate(CPUA = Total_Fish_Caught/AnglerDays) %>%
  select(-fish_total_weight, -fish_total_weighed)


# need to look into infinite values
cpua_year_aggregated = cpua %>%
  mutate(CPUA = ifelse(is.infinite(CPUA), 0, CPUA))
dat_long = cpua_year_aggregated 

write.csv(dat_long, "Outputs/PR_CPUA.csv", row.names = F, na = "")






#potential tool to query the data based on a block-species etc



















# 
# # testing code to the access database: can ignore ------------------------------------------------
# 
# 
# raw_oe  = fread(file=here("RCode", "PR", "Dat04to15", "Data", "PR_i1_2004-2015_487087r.csv"), fill = T, na.string = c("",".") )
# 
# test = oc_block_final %>%
#   inner_join(oe_block_final, by = c("Block", "year", "TripType_Description")) %>%
#   mutate(CPUA = Total_Fish_Caught/Ob_AnglerDays)
# 
# 
# 
# 
# test = dat_years %>%
#   filter(year == 2007) %>%
#   filter(Alpha %in% c("SCCAB", "GRNKP", "GRNRK", "SBKLP", "LNGCD", "SCRCA", "SHEEP")| startsWith(Alpha, "RF"))
# 
# oe_test = oe_by_id_agg_04_15 %>%
#   filter(year == 2007) %>%
#   mutate(sp_code =as.numeric(sp_code)) %>%
#   left_join(select(Sp, PSMFC_Code, Alpha), by = c("sp_code" = "PSMFC_Code")) %>%
#   filter(Alpha %in% c("SCCAB", "GRNKP", "GRNRK", "SBKLP", "LNGCD", "SCRCA", "SHEEP")| startsWith(Alpha, "RF"))
# 
# oc_test = oc_by_id_agg_04_15 %>%
#   filter(year == 2007) %>%
#   mutate(SP_CODE =as.numeric(SP_CODE)) %>%
#   left_join(select(Sp, PSMFC_Code, Alpha), by = c("SP_CODE" = "PSMFC_Code")) %>%
#   filter(Alpha %in% c("SCCAB", "GRNKP", "GRNRK", "SBKLP", "LNGCD", "SCRCA", "SHEEP")| startsWith(Alpha, "RF"))
# 
# test2 = test %>%
#   group_by(Block, year) %>%
#   summarise(Total_Fish_Caught = sum(Total_Fish_Caught),
#             Ob_AnglerDays = mean(Ob_AnglerDays)) %>%
#   mutate(CPUA = Total_Fish_Caught/Ob_AnglerDays )
# 
# ag = dat_years %>%
#   filter(year==2007) %>%
#   group_by(Alpha, Common_Name) %>%
#   count()
# 
# 
# # id that is very different 1012020071107014, how to split up effort
# 
# 
# #Cabezon, Greenling, Kelpbass, Lingcod, Rockfish, Scorpionfish, and Sheephead 

