# PR Effort  ---------------------------------------------

# Michael Patton (michael.patton@wildlife.ca.gov)
# script that processes the effort i1 table to provide a cleaned up and aggregated effort for each ID

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
options(scipen = 999)

# sources the script that is used to clean up the i8 table, returns a single variable 'all_locations' that provides the cleanup blocks at the ID level. Went through a series of filters as well. See other script for more information. 
source(here('RCode', "PR", "Locations", 'PR_Location.R'))

# this line is required when sourcing multiple R scripts. The plan is to run all required catch and effort scripts in a "master" script to avoid having to run everything one by one. This line makes sure the required objects are not removed when sourcing multiple scripts. Youll see it in the other scripts as well.
rm(list = ls()[!ls() %in% c("oc_by_id_agg", "oe_by_id_agg", "rc_by_id_agg", 'all_locations')])

# remove id with location since location is not used with effort. Additionally, there are some situations where they same blocks are used in different locations. This leads to double counting in effort
all_locations_effort = all_locations %>%
  select(-id_loc, -HLDEPTH2, -HLDEPTH, -freq, -ddlat, -ddlong) %>%
  unique()



#read in the i1 table for 2004-2015
oe = fread(file=here("RCode", "PR", "Dat04to15", "Data", "PR_i1_2004-2015_487087r.csv"), fill = T, na.string = c("",".") )  %>%
  mutate_all(as.character) 

# read in i1 for 2016 to 2021
new_oe <- fread(file = here('RCode', 'PR', 'Dat16toPresent', 'Data', 'i1_data_16to22.csv'), fill = TRUE) %>%
  mutate_all(as.character) %>%
  rename(DAYSF = daysf) 

#remove all columns that are only NAs
not_all_na <- function(x) any(!is.na(x))
oe = oe %>% select(where(not_all_na))

setdiff(names(new_oe), names(oe))
#[1] "geara"  "boatn"  "assnid" "port"   "daysf"  "gearB"  "island" "nolic"  "missed" "Ref #" 
setdiff(names(oe), names(new_oe))

# combine all years of data
oe = bind_rows(oe, new_oe)

### temporary code to investigate prim1 prim2 issue
Sp<- fread(here("Lookups", "SpeciesList05102023.csv" )) 
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
  left_join(Sp, by = c("prim1" = "PSMFC_Code")) %>%
  mutate(prim1 = Common_Name) %>%
  select(-Common_Name) %>%
  rename(Trip_1 = TripType_Description) %>%
  left_join(Sp, by = c("prim2" = "PSMFC_Code")) %>%
  mutate(prim2 = Common_Name) %>%
  select(-Common_Name) %>%
  rename(Trip_2 = TripType_Description) %>%
  arrange(desc(n)) %>%
  group_by(prim1, Trip_1, prim2, Trip_2) %>%
  summarise(n = sum(n)) %>%
  arrange(desc(n)) %>%
  mutate(different_type = Trip_1 != Trip_2) %>%
  mutate(different_type =  ifelse(is.na(different_type), FALSE, different_type))
#write.csv(prims_stat, "Outputs/Observed_Effort_Prim1vPrim2.csv", row.names = FALSE, na = "")

# cleans up id, date, month and year. 
# add in prim1 vs prim2 logic where prim1 is only used unless prim1 == 'Invertebrates"
#Selects only relevant columns 
oe <- oe %>% 
  mutate(id = as.character(ID_CODE), 
         date = ymd(stri_sub(ID_CODE, 6, 13)), 
                    month = month(date), 
                    year = year(date))%>%
  select(id, year, month, date, prim1, prim2, CNTRBTRS, DAYSF, HRSF) %>%
  left_join(Sp %>% select(PSMFC_Code, TripType_Description), by = c("prim1" = "PSMFC_Code")) %>%
  mutate(primary = ifelse(TripType_Description == "Invertebrates" & !is.na(TripType_Description) & !is.na(prim2), prim2, prim1)) %>%
  select(-TripType_Description)


# may want to convert these to anythings
na_triptypes = oe %>%
  filter(is.na(primary))

#clean up DAYSF and CNTRBTRS fields so NAs are 0, if days are reported as 0 in the data but CNTRBTRS were reported, change the days fished to 1 (NEW LOGIC)
oe <- oe %>% 
  mutate(DAYSF = as.numeric(ifelse(is.na(DAYSF), 0, DAYSF)), 
         CNTRBTRS = as.numeric(ifelse(is.na(CNTRBTRS), 0 , CNTRBTRS))) %>%
  mutate(DAYSF = ifelse(DAYSF == 0 & CNTRBTRS > 0, 1, DAYSF))


    #Create species table by extracting data from SpeciesList.csv and merge species data with dfr_angrep data frame 
Sp<- fread(here("Lookups", "SpeciesList05102023.csv" )) 
Sp<- Sp %>% 
  select(ALPHA5, PSMFC_Code, Common_Name, TripType_Description) %>% 
  mutate(PSMFC_Code = as.character(PSMFC_Code)) %>%
  filter(Common_Name != "bivalve class") %>%
  filter(!is.na(PSMFC_Code)) %>%
  rename(alpha = ALPHA5)


# pull out data that is not used because it uses a bad sp_code
notused <- oe %>%
  anti_join(Sp, by = c("primary" = "PSMFC_Code")) %>%
  mutate(Reason = "Primary Species not found in species lookup.")
unique(notused$primary) # looks like a lot of sp codes re reported as the alpha code need to clean this up
# NFOTH and others are used to indicate a non fishing vessel so are rightfully excluded

byyear = notused %>% group_by(year, primary) %>% count()
write.csv(notused, 'Outputs/NotUsed/i1/notused.csv', row.names = F, na = "")


# join species lookup to data
oe_species <- oe %>%
  inner_join(Sp, by = c("primary" = "PSMFC_Code"))

#summary of the different trip type descriptions found in the data
type_summary = oe_species %>%
  group_by(TripType_Description) %>%
  count() %>%
  arrange(desc(n)) # some invertebrates still make it through


# check to make sure no duplicates were created or rows were lost
nrow(oe) == (nrow(oe_species) + nrow(notused))

oe_species_loc <- oe_species %>%
  inner_join(all_locations_effort, by = c("id" = "ID_CODE")) %>%
  rename(year = year.x)

notused2 <- oe_species %>%
  anti_join(all_locations_effort, by = c("id"= "ID_CODE")) %>%
  mutate(Reason = "Does not have corresponding location data by ID")
byyear = notused2 %>% group_by(year) %>% count()
write.csv(notused2, 'Outputs/NotUsed/i1/notused2.csv', row.names = F, na = "")


# do NOT normalize effort to the number of blocks visited so no block information is needed to be brought in
dat <- oe_species_loc %>% 
  mutate(DaysPerBlock = DAYSF, 
         CntrbPerBlock = CNTRBTRS, 
         VesselPerBlock = 1)


notused3 = dat %>%
  filter(CntrbPerBlock == 0) %>%
  mutate(Reason = 'Zero contributors reported in data')

byyear = notused3 %>% group_by(year) %>%
  count()
write.csv(notused3, 'Outputs/NotUsed/i1/notused3.csv', row.names = F, na = "")

dat = filter(dat, CntrbPerBlock > 0)

# pivot the data so each block reported for a single id has its own row. Counts are already normalized by blocks visited so this will not double count anything but greatly simplifies the logic that WINN uses to generate summary statistics
by_block = dat %>%
  pivot_longer(Bk1Bx1a:extrablock11, names_to = "col", values_to = 'Block') %>%
  filter(!is.na(Block)) %>%
  select(-col, -total_blocks) %>%
  unique() # remove situations where the same block is entered into different block columns

# check for duplicates created by bad data entry that will effect the summing below
# notes from FAP: these are caused by when a sampler completes two different PR1 assignments in the same day and an associated issue with data entry. Will remove for now
duplicates = by_block %>%
  group_by(id, Block, TripType_Description, year) %>%
  count() %>%
  filter(n > 1) %>%
  left_join(by_block)

by_block = by_block %>%
  anti_join(duplicates)


# aggregate effort to the id-block-species level. This will later be aggregated to the Triptype level but wanted to leave species in for now. 
oe_by_id_agg = by_block %>%
  group_by(id, date, month, year, Block, Common_Name, TripType_Description) %>%
  summarise(Days = sum(DaysPerBlock, na.rm = T), 
            Cntrbs = sum(CntrbPerBlock, na.rm = T), 
            Vessels = sum(VesselPerBlock, na.rm = T)) %>%
  mutate(AnglerDays = Cntrbs*Days) %>%
  arrange(id) %>%
  rename(id_noloc = id) %>%
  filter(!is.na(Block))

#ideally nothing should change here
nrow(by_block) == nrow(oe_by_id_agg)


# QA check for duplicate combinations that will lead to double counting
finalcheck = oe_by_id_agg %>%
  group_by(id_noloc, Block, TripType_Description, year) %>%
  count() %>%
  filter(n > 1) %>%
  left_join(new_oe, by = c('id_noloc' = 'ID_CODE'))

# create summary of data that is not used and the provided reason
notused_summary = data.frame(c(unique(notused$Reason), unique(notused2$Reason), unique(notused3$Reason)), 
                             c(nrow(notused), nrow(notused2), nrow(notused3)))
names(notused_summary) = c("Reason", "Count")



write.csv(oe_by_id_agg, "Outputs/oe.csv", na = "", row.names = F)
write.csv(notused_summary, "Outputs/oe_notused_summary.csv", na = "", row.names = F)

