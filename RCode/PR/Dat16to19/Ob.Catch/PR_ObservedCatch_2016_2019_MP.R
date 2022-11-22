
# PR Observed Catch 2016_2019 ---------------------------------------------

# Michael Patton's simplification of original script (PR_ObservedCatch_2016_2019km.R)
# 7/20/2022


# USER INPUT
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

rm(list = ls()[!ls() %in% c("oc_by_id_agg_04_15", "oe_by_id_agg_04_15", "by_id_agg_04_15", "oc_by_id_agg_16_19", "by_id_agg_99_03")])

#  read in i3 table: sampler observed catch data
oc <- fread(file = here("RCode", "PR", "Dat16to19", "Data", "PR_i3_2016-2019.csv"), fill = TRUE, na.string = c("",".")) 

notused = oc %>%
  filter(is.na(as.numeric(SP_CODE))) %>%
  mutate(Reason = "SP_CODE is not valid.")

oc <- oc %>% 
  mutate(SP_CODE = as.numeric(SP_CODE)) %>%
  filter(!is.na(SP_CODE)) %>%
  mutate(locn = ifelse(is.na(locn)| locn == 0, 1, locn), 
         id = paste(ID_CODE, locn, sep= ""), 
         date = ymd(stri_sub(ID_CODE, 6, 13)), 
         month = month(date)) %>%
  select(id, date, month, year = YEAR, SP_CODE, ALPHA5, MODE_FX, CNTRBTRS, DISP3, WGT, FSHINSP) 



Sp <- fread(here("Lookups", "SpeciesList210510.csv" )) 
Sp <- Sp %>% 
  select(PSMFC_Code, Common_Name, TripType_Description) %>% 
  mutate(PSMFC_Code = as.numeric(PSMFC_Code)) %>%
  filter(Common_Name != "bivalve class") 

notused2 <- oc %>%
  anti_join(Sp, by = c("SP_CODE" = "PSMFC_Code")) %>%
  mutate(Reason = "Species not found in Lookup.")


oc_species <- oc %>%
  inner_join(Sp, by = c("SP_CODE" = "PSMFC_Code"))

nrow(oc) == (nrow(oc_species) + nrow(notused2))


# Format and link block data ----------------------------------------------
#takes block column and microblock column and standardizes if populated
format_box = function(block_column, microblock_column) {
  combined = paste(block_column, microblock_column, sep="-")
  combined_noNA = gsub("NA", "", combined)
  combined_noblanks = ifelse(nchar(combined_noNA) < 5, "", combined_noNA)
  return(combined_noblanks)
}

# Read in location data, or the i8 data table 
LocList <- fread(file = here("RCode", "PR", "Dat16to19", "Data", "PR_i8_2016-2019.csv"), fill = TRUE) 

loc <- LocList %>% 
  mutate(locn = ifelse(is.na(locn)| locn == 0, 1, locn), 
         block1 = ifelse(block1 == 9999999, NA, block1),
         id_loc = as.character(paste(id_code, locn, sep="")),
         Bk1Bx1a = format_box(block1, box1a),  
         Bk1Bx1b = format_box(block1, box1b), 
         Bk1Bx1c = format_box(block1, box1c), 
         Bk2Bx2a = format_box(block2, box2a), 
         Bk2Bx2b = format_box(block2, box2b), 
         Bk2Bx2c = format_box(block2, box2c)) %>% 
  select(id_loc, survey, MODE_FX, HLDEPTH, HLDEPTH2, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, HGSIZE, hgsize2)


notused3 = loc %>% 
  filter(is.na(id_loc)) %>%
  mutate(Reason = "Location missing ID code.")

loc  <- loc %>% filter(!is.na(id_loc))

notused4 = loc %>% 
  filter(MODE_FX != 7) %>%
  mutate(Reason = "Location not MODE_FX == 7 (PR data)")
unique(notused4$MODE_FX)

loc <- loc %>% 
  filter(MODE_FX ==7)

# THIS IS OLD LOGIC FROM WINNS SCRIPTS, LOOKING AT THE DUPLICATE ID VALUES (notused5 below) NOT SURE WHY SOME OF THESE CANNOT BE USED
# count the number of times a unique id code appears in the loction data
freq_summary <- loc %>% group_by(id_loc) %>% count()

#Calculate percentage of occurence where frequency of singular id code > 1 and calculate the maximum number of duplicate ID codes
per = (nrow(filter(freq_summary, n > 1))/nrow(freq_summary))*100
message(paste(round(per,2), "% of ids have duplicates with a maximum of ", max(freq_summary$n), "duplicates."))

#is it just when its reported in the data as a row for each block versus
#freq is used later in the calculations
#look up what assign# is

dfr_loc <- loc %>%
  left_join(freq_summary, by = "id_loc") %>%
  arrange(id_loc) %>%
  rename(freq = n)

#should start an export that includes all data that is not used
notused5 = oc %>%
  inner_join(filter(dfr_loc, freq > 1), by = c("id"= "id_loc")) %>%
  mutate(Reason = "Duplicate ids are used in the location (i8) data table")

dfr_loc = filter(dfr_loc, freq == 1)



# Merge together catch and location data ----------------------------------
oc_species_loc <- oc_species %>%
  inner_join(dfr_loc, by = c("id"= "id_loc")) %>% 
  select(id, date, month, year, SP_CODE, ALPHA5, Common_Name, TripType_Description, DISP3, WGT, FSHINSP, CNTRBTRS, HLDEPTH, HLDEPTH2, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, HGSIZE, hgsize2)

notused6 <- oc_species %>%
  anti_join(dfr_loc, by = c("id"= "id_loc")) %>%
  mutate(Reason = "Does not have corresponding location data by ID")

(n_distinct(oc_species_loc$id_n) + n_distinct(notused6$id_n)) == n_distinct(oc_species$id_n)


BlkFilterHGSIZERemoved <- oc_species_loc  %>% 
  mutate(Bk1Bx1a = ifelse(is.na(HGSIZE),Bk1Bx1a, NA), 
         Bk1Bx1b = ifelse(is.na(HGSIZE),Bk1Bx1b, NA), 
         Bk1Bx1c = ifelse(is.na(HGSIZE),Bk1Bx1c, NA), 
         Bk2Bx2a = ifelse(is.na(hgsize2),Bk2Bx2a, NA), 
         Bk2Bx2b = ifelse(is.na(hgsize2),Bk2Bx2b, NA), 
         Bk2Bx2c = ifelse(is.na(hgsize2),Bk2Bx2c, NA))

BlkFilterHGSIZERemoved[BlkFilterHGSIZERemoved==""]<-NA

BlkFilter <- BlkFilterHGSIZERemoved  %>% 
  filter(!is.na(Bk1Bx1a) | !is.na(Bk1Bx1b) | !is.na(Bk1Bx1c) | !is.na(Bk2Bx2a) | !is.na(Bk2Bx2b)  | !is.na(Bk2Bx2c)) 


notused7 = BlkFilterHGSIZERemoved %>%
  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a) & is.na(Bk2Bx2b)  & is.na(Bk2Bx2c)) %>%
  mutate(Reason = "Location data does not include any blocks.")

all_locations = BlkFilter


#calculate number of block visited
all_locations <- cbind(all_locations, total_blocks = apply(all_locations[, Bk1Bx1a:Bk2Bx2c], 1, function(x)length(unique(x[!is.na(x)]))))


# Add in row that has a fish counter value 
dat <- all_locations %>% 
  mutate(fish = ifelse(!is.na(FSHINSP), FSHINSP, 0),
         weight = as.numeric(ifelse(is.na(WGT) | WGT == "WGT", 0, WGT))) %>%
  select(-WGT)


dat <- dat %>% 
  mutate(FishPerBlock = fish/total_blocks, 
         WeightPerBlock = weight/total_blocks)

# Create tally for groups of identical records for id_n, SP_CODE and FSHINSP. data does this for different weight data entered
df  <- dat %>% mutate(unique_id = paste(id, SP_CODE, FSHINSP, sep="_"))
dups = df %>% group_by(unique_id) %>% summarise(freq_id = n())

df  <- df %>%
  inner_join(dups, by= "unique_id")

oc_final <- df %>% 
  mutate(FishPerBlock = ifelse(!is.na(FishPerBlock), FishPerBlock/freq_id, 0),
         WeightPerBlock = ifelse(!is.na(WeightPerBlock), WeightPerBlock, 0),
         Ob.RelAlive = ifelse(DISP3 %in% 1:2 | DISP3 %in% 7:9, FishPerBlock, 0 ),
         Ob.AvRelAliveWgt = ifelse(DISP3 %in% 1:2 | DISP3 %in% 7:9, WeightPerBlock, 0), 
         Ob.RelDead = ifelse(DISP3 %in% 6:6, FishPerBlock, 0 ), 
         Ob.AvRelDeadWgt  = ifelse(DISP3 %in% 6:6, WeightPerBlock, 0),
         Ob.Kept = ifelse(DISP3 %in% 3:5, FishPerBlock, 0),
         Ob.KWgt = ifelse(DISP3 %in% 3:5, WeightPerBlock, 0))



by_block = oc_final %>%
  pivot_longer(Bk1Bx1a:Bk2Bx2c, names_to = "col", values_to = 'Block') %>%
  filter(Block != "")

oc_by_id_agg_16_19 = by_block %>%
  group_by(id, date, month, year, Block,  SP_CODE, FSHINSP, Common_Name) %>%
  summarise(Ob_ReleasedAlive = sum(Ob.RelAlive), 
            Ob_AvRAWgt = mean(Ob.AvRelAliveWgt, na.rm = TRUE), 
            Ob_ReleasedDead = sum(Ob.RelDead, na.rm = T), 
            Ob_AvRDWgt = mean(Ob.AvRelDeadWgt, na.rm = TRUE), 
            Ob_Kept  = sum(Ob.Kept, na.rm = T), 
            Ob_AvKWgt = mean(Ob.KWgt, na.rm=TRUE)) %>%
  mutate(Total_Fish_Caught = Ob_ReleasedAlive + Ob_ReleasedDead + Ob_Kept)


