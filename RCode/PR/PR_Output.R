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
nrow(oc_by_id_agg_04_15)
nrow(rc_by_id_agg_04_15)
all_by_id = oc_by_id_agg_04_15 %>%
  full_join(rc_by_id_agg_04_15, by = c("id", "SP_CODE", "Block", "Common_Name", "date", 'month', 'year')) %>%
  left_join(Sp, by = c("Common_Name", "SP_CODE" = "PSMFC_Code")) %>%
  mutate(id_noloc = substr(id, 1, nchar(id) - 1)) %>% #need to make sure there are no double digit locations, should realistically go back to original catch scripts
  full_join(oe_by_id_agg_04_15, by = c("id_noloc" = "id", "Block", "date", 'month', 'year'), suffix = c("_catch", "_effort"))
nrow(all_by_id)
names(all_by_id)

comp = all_by_id %>%
  group_by(Common_Name_catch, Common_Name_effort, TripType_Description_catch, TripType_Description_effort, primary) %>%
  count() %>%
  arrange(desc(n))

write.csv(comp, "CPUE_combos.csv", na = "", row.names = F)

# Observed Catch ----------------------------------------------------------

#calculate the final results at the block, species, yearly level. Can modify to be at any other time frame that is included in the id level output (day or month)
oc_block_final = oc_by_id_agg_04_15 %>%
  mutate(fish_total_weight = Ob_AvKWgt*Ob_Weighed_Fish) %>%
  group_by(Block, SP_CODE, year) %>%
  summarise(Ob_ReleasedAlive = sum(Ob_ReleasedAlive, na.rm = T), 
            Ob_ReleasedDead = sum(Ob_ReleasedDead), 
            Ob_Kept  = sum(Ob_Kept, na.rm = T), 
            fish_total_weight = sum(fish_total_weight),
            fish_total_weighed = sum(Ob_Weighed_Fish),
            Ob_sample_freq = n_distinct(id)) %>%
  mutate(Ob_Total_Fish_Caught = Ob_ReleasedAlive + Ob_ReleasedDead + Ob_Kept) %>%
  mutate(Ob_caught_per_sample = Ob_Total_Fish_Caught/Ob_sample_freq) %>%
  mutate(Ob_AvKWgt = fish_total_weight/fish_total_weighed)


before = nrow(oc_block_final)
# join with species look up for trip type description
oc_block_final = oc_block_final %>%
  inner_join(Sp, by = c("SP_CODE" = "PSMFC_Code")) %>%
  rename(sp_code = SP_CODE)

#test to make sure duplicates were not created or lost
before == nrow(oc_block_final)

# Reported Catch ----------------------------------------------------------

#aggreaget the final reported catch statistics to the Block, species and temporal level
rc_block_final = rc_by_id_agg_04_15 %>%
  group_by(Block, SP_CODE, year) %>%
  summarise(Rep_ReleasedAlive = sum(Rep_ReleasedAlive, na.rm = T), 
            Rep_ReleasedDead = sum(Rep_ReleasedDead, na.rm = T), 
            Rep_Kept  = sum(Rep_Kept, na.rm = T), 
            Rep_sample_freq = n_distinct(id)) %>%
  mutate(Rep_Total_Fish_Caught = Rep_ReleasedAlive + Rep_ReleasedDead + Rep_Kept) %>%
  mutate(Rep_caught_per_sample = Rep_Total_Fish_Caught/Rep_sample_freq)


before = nrow(rc_block_final)
rc_block_final = rc_block_final %>%
  inner_join(Sp, by = c("SP_CODE" = "PSMFC_Code")) %>%
  rename(sp_code = SP_CODE)

before == nrow(rc_block_final)



# Observed Effort ---------------------------------------------------------

# effort is aggregated to the block, TRIPTYPE, and temporal level. This is because there is little match between targeting an "vermillion rockfish" and actually catching one. 
oe_block_final = oe_by_id_agg_04_15 %>%
  group_by(Block, TripType_Description, year) %>%
  summarise(Ob_AnglerDays = sum(AnglerDays),
            Ob_Vessels = sum(VesselPerBlock))

#Joins together the three aggregated outputs. Catch by species, catch to effort by trip type

final = oc_block_final %>%
  full_join(rc_block_final, by = c("Block", "year", "sp_code", "Alpha", "TripType_Description", "Common_Name")) %>%
  full_join(oe_block_final, by = c("Block", "year", "TripType_Description"))


# CPUA = reported fish caught + observed fish caught at the species level / observed (maybe reported at some point) at the trip type effort
cpua_year_aggregated = final %>%
  select(Block, year, sp_code, Alpha, Common_Name, TripType_Description, Ob_Total_Fish_Caught, Rep_Total_Fish_Caught, Ob_AnglerDays, Ob_Vessels) %>%
  mutate(Ob_Total_Fish_Caught = ifelse(is.na(Ob_Total_Fish_Caught), 0, Ob_Total_Fish_Caught)) %>%
  mutate(Rep_Total_Fish_Caught = ifelse(is.na(Rep_Total_Fish_Caught), 0, Rep_Total_Fish_Caught)) %>%
  mutate(Ob_AnglerDays = ifelse(is.na(Ob_AnglerDays), 0, Ob_AnglerDays)) %>%
  mutate(Total_Fish_Caught = Ob_Total_Fish_Caught + Rep_Total_Fish_Caught) %>%
  mutate(Total_Fish_Caught = ifelse(is.na(Total_Fish_Caught), 0, Total_Fish_Caught)) %>%
  mutate(CPUA = Total_Fish_Caught/Ob_AnglerDays) %>%
  arrange(Block, Common_Name, year)

cpua_year_aggregated = cpua_year_aggregated %>%
  mutate(CPUA = ifelse(is.infinite(CPUA), 0, CPUA))

dat_long = cpua 

write.csv(dat_long, "Outputs/PR_CPUA.csv", row.names = F, na = "")

dat_wide = cpua %>%
  pivot_wider(names_from = year, values_from = c("Total_Fish_Caught", "Ob_AnglerDays", "Ob_Vessels", "CPUA" ), values_fill = 0)

write.csv(dat_wide, "Outputs/PR_wide_CPUA.csv", row.names = F, na = "")










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

