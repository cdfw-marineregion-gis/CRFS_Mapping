# this script is used to generate shapefiles that contain the CRFS CPUA data at the block-box level. A user of this script can adjust the inputs (CATCH, TRIPTYPE, MINIMUM NUMBER OF SAMPLES, METRIC) and output a shapefile containing the aggregated CPUA for that combination. 

# there are three different shapefiles formatted:
# # # A long: where each block - year has its own ROW. This is could for time series visualization or if you want multiple metrics included in a single file 
# # # A wide: where each year is inlcuded in its own COLUMN for each block. This limits the size of the file and is good for sharing since a block polygon is not duplicated. A drawback to this layer is only one metric (CPUA ALL, CPUA KEPT) can be included
# # # A wide grouped by years: where years are aggregated into ranges for each COLUMN. This is to limit the number of columns if you want to include multiple METRICS

# Michael Patton
# michael.patton@wildlife.ca.gov


# copy path to where you downloaded the shared CRFS_Mapping folder between the ()
working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping\RCode\PR\Dat16toPresent\Data)"
setwd(working_directory)


library(tidyverse)
library(sf)
library(here)
library(readxl)
library(leaflet)
options(scipen = 999)


# bring in the block shapefile from lookup folder
blocks.SP <- st_read(here("Lookups", "MAN_CA_CRFS_microblocks2013_clip.shp"))
# change projection of blocks to WGS84 (what the coordinates are)
blocks.SP = select(blocks.SP, NM_INDEX)
blocks.SP = st_transform(blocks.SP, crs = 4326) 

# bring in final ouputs
output = read.csv(here('Outputs', 'all_by_id.csv'))

# bring in species grouping
groups = read_excel(here('Lookups', 'SpeciesGroups_DRAFT.xlsx'))
group_options = names(groups)[2:ncol(groups)]


# select individual species or a species group
unique(group_options) # Another option is ALL
SPECIES = 'All'

# test to confirm that chosen species is either an option within the grouped species or is a unique species found within the CRFS species lookup table. The script will automatically stop if species is not found. 
if (SPECIES %in% group_options| SPECIES %in% unique(output$Common_Name_catch)){
  print('Chosen Species exists')
} else {
  stop('Chosen species does not exist in output or group options. Check spelling or capitilization')
}

# pick a trip type description
unique(output$TripType_Description_effort)
# another option is 'ALL'
TRIPTYPE = 'All'

# pick the minimum number of samples required per block
MINIMUM_SAMPLES = 3

# What metric either CPUA_Kept or CPUA_All
METRIC = 'CPUA_Kept'

# where you want it saved
WHERE = 'C:\\GIS\\CRFSMapping\\Outputs\\'


###############################################################################################################################################################################
# run the rest and the shapefiles will be included in the folder specified:


# if else statement to create a list of species that can be used to filter the catch data. If the user choose a species group then all individual species in that group is used. Otherwise its a list of 1 (the chosen species)
if (SPECIES %in% group_options) {
  species = filter(groups, !!as.symbol(SPECIES) == 1)
  species = species$Common_Name_catch
  is_group = TRUE
} else {
  species = c(SPECIES)
  is_group = FALSE
}


# filter catch data to both CATCH and TRIP
filtered_catch = output %>%
  filter(Common_Name_catch %in% species)

# if 'All' effort is chosen then no effort side filtering is applied, instead the trip type description is mutated to 'All' so all effort can be aggregated together
if (TRIPTYPE == 'All') {
  filtered_catch = mutate(filtered_catch, TripType_Description_effort = 'All')
} else {
  filtered_catch = filter(filtered_catch, TripType_Description_effort == TRIPTYPE)
}

# if a species group is chosen then the Common_Name_catch value is replaced with the species group so all catch of members within that species will be aggregated together. 
if (is_group) {
  filtered_catch$Common_Name_catch = SPECIES
}


# filter effort to only trip type
# if 'All' effort is chosen then no effort side filtering is applied, instead the trip type description is mutated to 'All' so all effort can be aggregated together
if (TRIPTYPE == 'All') {
  filtered_effort = mutate(output, TripType_Description_effort = 'All')
} else {
  filtered_effort = filter(output, TripType_Description_effort == TRIPTYPE)
}

# Because a given id has multiple rows, each describing a different species caught on the trip, the script must isolate unique combinations of the fields listed after select prior to summing angler days
id_aggregated_effort = filtered_effort %>%
  select(id, year, Block, Vessels, AnglerDays, TripType_Description_effort) %>%
  unique()

# sum angler days for each  block by TRIP TYPE
aggregate_effort = id_aggregated_effort %>%
  group_by(Block, TripType_Description_effort, year) %>%
  summarise(AnglerDays = sum(AnglerDays, na.rm = T),
            n_samples_effort = n_distinct(id))


# sum catch for each block by SPECIES AND TRIP TYPE
aggregate_catch = filtered_catch %>%
  group_by(Block, Common_Name_catch, TripType_Description_effort, year) %>%
  summarise(TotalCatch = sum(Total_Fish_Caught, na.rm = T),
            Kept = round(sum(Total_Obs_Fish_Caught, na.rm = T), 2),
            Released = round(sum(Total_Rep_Fish_Caught, na.rm=T), 2)) 


# join together aggregate catch and effort by BLOCK, YEAR, and TRIP Use effort on left side because you can have effort and no catch
# calculate CPUA for all fish and just kept
joined = aggregate_effort %>%
  left_join(aggregate_catch, by = c("Block", "year", "TripType_Description_effort")) %>%
  mutate(CPUA_All = round(TotalCatch/AnglerDays, 3)) %>%
  mutate(CPUA_Kept = round(Kept/AnglerDays, 3)) %>%
  mutate(CPUA_Kept = ifelse(is.na(CPUA_Kept), 0, CPUA_Kept)) %>%
  mutate(CPUA_All = ifelse(is.na(CPUA_All), 0, CPUA_All)) %>%
  mutate(Common_Name_catch = ifelse(is.na(Common_Name_catch) | Common_Name_catch == "", SPECIES, Common_Name_catch)) %>% 
  ungroup()

# long format -------------------------------------------------------------
# long format of data (where year is in own row) is good for time enabled features. Also can have multiple variables included such as CPUA Kept and CPUA All

joined_sample_limited = joined %>%
  filter(n_samples_effort >= MINIMUM_SAMPLES) # should make over all years

layer_long = blocks.SP %>%
  inner_join(joined_sample_limited, by = c( 'NM_INDEX' = 'Block')) %>%
  select(Block = NM_INDEX, Catch = Common_Name_catch, Trip = TripType_Description_effort, Samples = n_samples_effort, everything() )

long_name = paste0(SPECIES, '_',TRIPTYPE, '_', MINIMUM_SAMPLES ,'_SAMPLES_long.shp')

st_write(layer_long, paste(WHERE, long_name), delete_layer=T)



# Wide format -------------------------------------------------------------
# this is a good for a simple layer to be uploaded into BIOS
wide = joined %>%
  ungroup() %>%
  mutate(year = paste0('Y', year)) %>%
  select(Block, Catch = Common_Name_catch, Trip = TripType_Description_effort, year, !!as.symbol(METRIC)) %>%
  pivot_wider(names_from = year, values_from = !!as.symbol(METRIC)) %>%
  select(Block, Catch, Trip, any_of(c("Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011","Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020", "Y2021", "Y2022")))


wide_allyears = joined %>%
  ungroup() %>%
  dplyr::group_by(Block, Common_Name_catch, TripType_Description_effort) %>%
  summarise(AnglerDays = sum(AnglerDays, na.rm = T),
            TotalCatch = sum(TotalCatch, na.rm = T),
            Kept = round(sum(Kept, na.rm = T), 2),
            Released = round(sum(Released, na.rm=T), 2),
            total_samples = sum(n_samples_effort)) %>%
  mutate(CPUA_All = round(TotalCatch/AnglerDays, 3)) %>%
  mutate(CPUA_Kept = round(Kept/AnglerDays, 3)) %>%
  mutate(CPUA_Kept = ifelse(is.na(CPUA_Kept), 0, CPUA_Kept)) %>%
  mutate(CPUA_All = ifelse(is.na(CPUA_All), 0, CPUA_All)) %>%
  filter(total_samples >= MINIMUM_SAMPLES) %>%
  select(Block, Catch = Common_Name_catch, !!as.symbol(METRIC), Samples = total_samples)

layer_wide = blocks.SP %>%
  inner_join(wide, by = c( 'NM_INDEX' = 'Block')) %>%
  inner_join(wide_allyears, by = c('NM_INDEX' = 'Block', 'Catch')) %>%
  rename(Block = NM_INDEX)

layer_wide[is.na(layer_wide)] <- -9999

wide_name = paste0(SPECIES, '_',TRIPTYPE, '_', MINIMUM_SAMPLES ,'_SAMPLES_', METRIC, '.shp')

st_write(layer_wide, paste0(WHERE, wide_name), delete_layer=T)

# Wide Grouped ------------------------------------------------------------
# this format groups years together and includes both kept and all CPUA
wide_grouped = joined %>%
  ungroup() %>%
  mutate(year_group = case_when(year %in% (2004:2009) ~ '04_09',
                                year %in% (2010:2015) ~ '10_15',
                                year %in% (2016:2020) ~ '16_20',
                                year %in% (2021:2023) ~ '21_23')) %>%
  dplyr::group_by(Block, Common_Name_catch, TripType_Description_effort, year_group) %>%
  summarise(AnglerDays = sum(AnglerDays, na.rm = T),
            TotalCatch = sum(TotalCatch, na.rm = T),
            Kept = round(sum(Kept, na.rm = T), 2),
            Released = round(sum(Released, na.rm=T), 2)) %>%
  mutate(CPUA_All = round(TotalCatch/AnglerDays, 3)) %>%
  mutate(CPUA_Kept = round(Kept/AnglerDays, 3)) %>%
  mutate(CPUA_Kept = ifelse(is.na(CPUA_Kept), 0, CPUA_Kept)) %>%
  mutate(CPUA_All = ifelse(is.na(CPUA_All), 0, CPUA_All)) %>%
  select(Block, year_group, Catch = Common_Name_catch, Trip = TripType_Description_effort, All = CPUA_All, Kept = CPUA_Kept) %>%
  pivot_wider(names_from = year_group, values_from = c(All, Kept))

wide_allyears = joined %>%
  ungroup() %>%
  dplyr::group_by(Block, Common_Name_catch, TripType_Description_effort) %>%
  summarise(AnglerDays = sum(AnglerDays, na.rm = T),
            TotalCatch = sum(TotalCatch, na.rm = T),
            Kept = round(sum(Kept, na.rm = T), 2),
            Released = round(sum(Released, na.rm=T), 2),
            total_samples = sum(n_samples_effort)) %>%
  mutate(CPUA_All = round(TotalCatch/AnglerDays, 3)) %>%
  mutate(CPUA_Kept = round(Kept/AnglerDays, 3)) %>%
  mutate(CPUA_Kept = ifelse(is.na(CPUA_Kept), 0, CPUA_Kept)) %>%
  mutate(CPUA_All = ifelse(is.na(CPUA_All), 0, CPUA_All)) %>%
  filter(total_samples >= MINIMUM_SAMPLES) %>%
  select(Block, Catch = Common_Name_catch, CPUA_Kept, CPUA_All, Samples = total_samples)


layer_wide_grouped = blocks.SP %>%
  inner_join(wide_grouped, by = c( 'NM_INDEX' = 'Block')) %>%
  inner_join(wide_allyears, by = c('NM_INDEX' = 'Block', 'Catch')) %>%
  rename(Block = NM_INDEX)
  
layer_wide_grouped[is.na(layer_wide_grouped)] <- -9999

wide_group_name = paste0(SPECIES, '_',TRIPTYPE, '_', MINIMUM_SAMPLES ,'_SAMPLES_', '_GROUPS', '.shp')

st_write(layer_wide_grouped, paste0(WHERE, wide_group_name), delete_layer=T)




# Create leaflet map for testing output -----------------------------------


pal = colorNumeric('Reds', layer_wide_grouped$CPUA_Kept, na.color = 'transparent')

leaflet() %>%
  addTiles() %>%
  setView(lng = -122.85, lat = 37.45, zoom = '7') %>%
  addPolygons(data=layer_wide_grouped,
              
              color = "#444444", 
              stroke=FALSE,
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(CPUA_Kept),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              popup=paste0('Block:', layer_wide_grouped$Block, '<br>', 
                           'CPUA: ', round(layer_wide_grouped$CPUA_Kept,2), '<br>',
                           'Samples: ', layer_wide_grouped$Samples, '<br>')) %>%
  addLegend("bottomright", pal = pal, values = layer_wide_grouped$CPUA_Kept,
            title = "CPUA Kept",
            opacity = 1
  )

