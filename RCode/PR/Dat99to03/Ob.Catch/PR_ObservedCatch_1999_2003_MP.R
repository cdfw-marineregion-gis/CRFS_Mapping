
# PR Observed Catch 1999_2003 ---------------------------------------------

# Michael Patton's simplification of original script (PR_ObservedCatch_1999_2003km.R)
# 7/20/2022

library(data.table)
library(stringi)
library(tidyverse)
library(lubridate)
library(here)
library(sf)
options(scipen = 999)

rm(list = ls()[!ls() %in% c("by_id_agg_04_15", "by_id_agg_16_19", "by_id_agg_99_03")])


#  read in i3 table: sampler observed catch data
oc <- fread(file = here("R code", "PR", "Dat99to03", "Data", "PR_i3_1980-2003_221886r.csv"), fill = TRUE, na.string = c("",".")) 

notused = oc %>%
  filter(is.na(as.numeric(SP_CODE))) %>%
  mutate(Reason = "SP_CODE is not valid.")

oc <- oc %>% 
  mutate(SP_CODE = as.numeric(SP_CODE)) %>%
  filter(!is.na(SP_CODE)) %>%
  mutate(id = id_code, 
         date = ymd(stri_sub(id_code, 6, 13)), 
         month = month(date)) %>%
  select(id, date, month, year = YEAR, SP_CODE, MODE_FX, CNTRBTRS, DISP3, WGT, FSHINSP) 



Sp <- fread(here("Lookups", "SpeciesList210510.csv" )) 
Sp <- Sp %>% 
  select(PSMFC_Code, Common_Name, TripType_Description) %>% 
  mutate(PSMFC_Code = as.numeric(PSMFC_Code)) %>%
  filter(Common_Name != "bivalve class") 

no_species <- oc %>%
  anti_join(Sp, by = c("SP_CODE" = "PSMFC_Code")) %>%
  mutate(Reason = "Species not found in Lookup.")


oc_species <- oc %>%
  inner_join(Sp, by = c("SP_CODE" = "PSMFC_Code"))

nrow(oc) == (nrow(oc_species) + nrow(no_species))


# Format and link block data ----------------------------------------------


#takes block column and microblock column and standardizes if populated
format_box = function(block_column, microblock_column) {
  combined = paste(block_column, microblock_column, sep="-")
  combined_noNA = gsub("NA", "", combined)
  combined_noblanks = ifelse(nchar(combined_noNA) < 5, "", combined_noNA)
  return(combined_noblanks)
}

# Read in location data, or the i8 data table 
NLocList <- fread(file = here("R code", "PR", "Dat99to03", "Data", "i8_1999-2016_NorCal_235374r.csv"), fill = TRUE, na.string = c("",".", "NA")) 

NLocList <- NLocList %>% 
  mutate(locn = ifelse(is.na(locn)| locn == 0, 1, locn),
         block1 = ifelse(block1 == 9999999, NA, block1),
         id_loc = as.character(paste(ID_CODE, locn, sep="")),
         Bk1Bx1a = format_box(block1, box1a),  
         Bk1Bx1b = format_box(block1, box1b), 
         Bk1Bx1c  = format_box(block1, box1c), 
         Bk2Bx2a  = format_box(block2, box2a), 
         Bk2Bx2b  = format_box(block2, box2b), 
         Bk2Bx2c  = format_box(block2, box2c)) %>%
  select(ID_CODE,  survey, MODE_FX, HLDEPTH, HLDEPTH2, ddlat, ddlong, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, HGSIZE, hgsize2)

SLocList <- fread(file = here("R code", "PR", "Dat99to03", "Data", "i8_1999-2016_SoCal_204029r.csv"), fill = TRUE, na.string = c("",".", "NA")) 

SLocList <- SLocList %>% 
  mutate(locn = ifelse(is.na(locn)| locn == 0, 1, locn),
         block1 = ifelse(block1 == 9999999, NA, block1),
         id_loc = as.character(paste(ID_CODE, locn, sep="")),
         Bk1Bx1a = format_box(block1, box1a),  
         Bk1Bx1b = format_box(block1, box1b), 
         Bk1Bx1c = format_box(block1, box1c), 
         Bk2Bx2a = format_box(block2, box2a), 
         Bk2Bx2b = format_box(block2, box2b), 
         Bk2Bx2c = format_box(block2, box2c)) %>%
  select(ID_CODE, survey, MODE_FX, HLDEPTH, HLDEPTH2, ddlat, ddlong, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, HGSIZE, hgsize2)

loc = rbind(NLocList, SLocList)

notused2 = loc %>% 
  filter(is.na(ID_CODE)) %>%
  mutate(Reason = "Location missing ID code.")

loc  <- loc %>% filter(!is.na(ID_CODE))

notused3 = loc %>% 
  filter(MODE_FX != 7) %>%
  mutate(Reason = "Location not MODE_FX == 7 (PR data)")

loc <- loc %>% 
  filter(MODE_FX ==7)


# Passing the id vector through the table function -  identifies the frequency in which a ID code was used in the table 
freq_summary <- loc %>% group_by(ID_CODE) %>% count()



#Calculate percentage of occurence where frequency of singular id code > 1 and calculate the maximum number of duplicate ID codes
per = (nrow(filter(freq_summary, n > 1))/nrow(freq_summary))*100
message(paste(round(per,2), "% of ids have duplicates with a maximum of ", max(freq_summary$n), "duplicates."))

#is it just when its reported in the data as a row for each block versus
#freq is used later in the calculations
#look up what assign# is

dfr_loc <- loc %>%
  left_join(freq_summary, by = "ID_CODE") %>%
  arrange(ID_CODE) %>%
  rename(freq = n)

#should start an export that includes all data that is not used
notused = oc %>%
  inner_join(filter(dfr_loc, freq > 1), by = c("id" = "ID_CODE")) %>%
  mutate(Reason = "Duplicate ids are used in the location (i8) data table")

dfr_loc = filter(dfr_loc, freq == 1)



# Merge together catch and location data ----------------------------------

oc_species_loc <- oc_species %>%
  inner_join(dfr_loc, by = c("id"= "ID_CODE")) %>% 
  select(id, date, month, year, SP_CODE, Common_Name, TripType_Description, DISP3, WGT, FSHINSP, CNTRBTRS, HLDEPTH, HLDEPTH2, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, ddlat, ddlong, HGSIZE, hgsize2)

noid_match <- oc_species %>%
  anti_join(dfr_loc, by = c("id"= "ID_CODE")) %>%
  mutate(Reason = "Does not have corresponding location data by ID")

(n_distinct(oc_species_loc$id_n) + n_distinct(noid_match$id_n)) == n_distinct(oc_species$id_n)


#duplicate ids in the location file from pr1 and pr2 datasets combined. These should have separate ids


#DO YOU USE AN HG SIZE OF 1 NEED TO LOOK AT THIS FIELD FURTHER

BlkFilterHGSIZERemoved <- oc_species_loc  %>% 
  mutate(Bk1Bx1a = ifelse(is.na(HGSIZE),Bk1Bx1a, NA), 
         Bk1Bx1b = ifelse(is.na(HGSIZE),Bk1Bx1b, NA), 
         Bk1Bx1c = ifelse(is.na(HGSIZE),Bk1Bx1c, NA), 
         Bk2Bx2a = ifelse(is.na(hgsize2),Bk2Bx2a, NA), 
         Bk2Bx2b = ifelse(is.na(hgsize2),Bk2Bx2b, NA), 
         Bk2Bx2c = ifelse(is.na(hgsize2),Bk2Bx2c, NA))

BlkFilterHGSIZERemoved[BlkFilterHGSIZERemoved==""]<-NA

BlkFilterHGSIZERemoved = BlkFilterHGSIZERemoved %>%
  filter(year %in% 1999:2003)

BlkFilter <- BlkFilterHGSIZERemoved  %>% 
  filter(!is.na(Bk1Bx1a) | !is.na(Bk1Bx1b) | !is.na(Bk1Bx1c) | !is.na(Bk2Bx2a) | !is.na(Bk2Bx2b)  | !is.na(Bk2Bx2c)) 

LatFilter  <- BlkFilterHGSIZERemoved %>%  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a)  & is.na(Bk2Bx2b) & is.na(Bk2Bx2c) & !is.na(ddlat) & is.na(HGSIZE) & is.na(hgsize2)) %>%
  mutate(ddlong = ddlong*-1)

noblock_or_coord = BlkFilterHGSIZERemoved %>%
  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a) & is.na(Bk2Bx2b)  & is.na(Bk2Bx2c) & is.na(ddlat)) %>%
  mutate(Reason = "Location data does not include any block or coordinate data.")

baddata = LatFilter %>%
  filter(ddlat > 50 | ddlong < -150)

LatFilter = LatFilter %>%
  filter(ddlat < 50 & ddlong > -150)

coords.SP <- st_as_sf(LatFilter, coords = c("ddlong", "ddlat"), crs = 4326, remove = FALSE)

blocks.SP <- st_read(here("C:/CRFS/GIS/commondata/crfs_mappingdata1", "MAN_CA_CRFS_microblocks2013.shp"))

blocks.SP = st_transform(blocks.SP, crs = st_crs(coords.SP)) 
blocks.SP = select(blocks.SP, NM_INDEX)

sf_use_s2(FALSE)

library(leaflet)
leaflet() %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  addCircleMarkers(data=coords.SP)

coords_wblocks.SP = st_join(coords.SP, blocks.SP, left = TRUE)


coords_wblocks = coords_wblocks.SP %>%
  st_drop_geometry() %>%
  mutate(Bk1Bx1a = NM_INDEX) %>%
  select(colnames(BlkFilter))

badlatlong = filter(coords_wblocks, is.na(Bk1Bx1a))

coords_wblocks = filter(coords_wblocks, !is.na(Bk1Bx1a))


all_locations = rbind(BlkFilter, coords_wblocks)


# Add in row that has a fish counter value 
dat <- all_locations %>% 
  mutate(fish = ifelse(!is.na(FSHINSP), FSHINSP, 0),
         weight = as.numeric(ifelse(is.na(WGT) | WGT == "WGT", 0, WGT))) %>%
  select(-WGT)


# weight per block calculations -------------------------------------------

#calculate number of block visited
dat = dat %>%
  mutate(total_blocks = rowSums(!is.na(dat[,Bk1Bx1a:Bk2Bx2c]), na.rm = TRUE))

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

 
by_id_agg_99_03 = by_block %>%
  group_by(id, date, month, year, Block,  SP_CODE, FSHINSP, Common_Name) %>%
  summarise(Ob.ReleasedAlive = sum(Ob.RelAlive), 
            Ob.AvRAWgt = mean(Ob.AvRelAliveWgt, na.rm = TRUE), 
            Ob.ReleasedDead = sum(Ob.RelDead), 
            Ob.AvRDWgt = mean(Ob.AvRelDeadWgt, na.rm = TRUE), 
            Ob.Kept  = sum(Ob.Kept), 
            Ob.AvKWgt = mean(Ob.KWgt, na.rm=TRUE)) %>%
  mutate(Total_Fish_Caught = Ob.ReleasedAlive + Ob.ReleasedDead + Ob.Kept)




block_final = by_id_agg_99_03 %>%
  group_by(Block, Common_Name, year) %>%
  summarise(Ob.ReleasedAlive = sum(Ob.ReleasedAlive), 
            Ob.AvRAWgt = mean(Ob.AvRAWgt, na.rm = TRUE), 
            Ob.ReleasedDead = sum(Ob.ReleasedDead), 
            Ob.AvRDWgt = mean(Ob.AvRDWgt, na.rm = TRUE), 
            Ob.Kept  = sum(Ob.Kept), 
            Ob.AvKWgt = mean(Ob.AvKWgt, na.rm=TRUE)) %>%
  mutate(Total_Fish_Caught = Ob.ReleasedAlive + Ob.ReleasedDead + Ob.Kept)






#notused = rbind(notused, notused2)



