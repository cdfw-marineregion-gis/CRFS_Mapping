library(tidyverse)
library(sf)
library(here)
library(readxl)


# bring in the block shapefile from lookup folder
blocks.SP <- st_read(here("Lookups", "MAN_CA_CRFS_microblocks2013_clip.shp"))
# change projection of blocks to WGS84 (what the coordinates are)
blocks.SP = select(blocks.SP, NM_INDEX)
# bring in final ouputs
output = read.csv(here('Outputs', 'all_by_id.csv'))
# bring in species grouping
groups = read_excel(here('Lookups', 'SpeciesGroups_DRAFT.xlsx'))
group_options = names(groups)[2:ncol(groups)]

# select individual species or a species group
unique(group_options)
SPECIES = 'Rockfish'

# pick a trip type description
unique(output$TripType_Description_effort)
TRIPTYPE = 'Bottomfish'

# if else statement to create a list of species that can be used to filter the catch data. If the user choose a species group then all individual species in that group is used. Otherwise its a list of 1 (the chosen species)
if (catch %in% group_options) {
  species = filter(groups, !!as.symbol(SPECIES) == 1)
  species = species$Common_Name_catch
  is_group = TRUE
} else {
  species = c(species)
  is_group = FALSE
}


# filter catch data to both CATCH and TRIP
filtered_catch = output %>%
  filter(Common_Name_catch %in% species) %>%
  filter(TripType_Description_effort == TRIPTYPE)

if (is_group) {
  filtered_catch$Common_Name_catch = SPECIES
}


filtered_effort = filter(output, TripType_Description_effort == TRIPTYPE)



aggregate_effort = filtered_effort %>%
  group_by(Block, TripType_Description_effort, year) %>%
  summarise(AnglerDays = sum(AnglerDays, na.rm = T),
            n_samples_effort = n_distinct(id))

aggregate_catch = filtered_catch %>%
  group_by(Block, Species_Catch, TripType_Description_effort, year) %>%
  summarise(TotalCatch = sum(Total_Fish_Caught, na.rm = T),
            Kept = round(sum(Ob_Kept, na.rm = T), 2),
            Released = round(sum(Rep_Released, na.rm=T), 2)) 

joined = aggregate_effort %>%
  left_join(aggregate_catch, by = c("Block", "year", "TripType_Description_effort")) %>%
  mutate(CPUA_All = round(TotalCatch/AnglerDays, 3)) %>%
  mutate(CPUA_Kept = round(Kept/AnglerDays, 3)) %>%
  mutate(CPUA_Kept = ifelse(is.na(CPUA_Kept), 0, CPUA_Kept)) %>%
  mutate(CPUA_All = ifelse(is.na(CPUA_All), 0, CPUA_All)) %>%
  mutate(Species_Catch = ifelse(is.na(Species_Catch), 'Rockfish', Species_Catch))

layer_long = blocks.SP %>%
  inner_join(joined, by = c( 'NM_INDEX' = 'Block')) %>%
  select(Block = NM_INDEX, Catch = Species_Catch, Trip = TripType_Description_effort, Samples = n_samples_effort, everything() )

st_write(layer_long, 'C:\\CRFS\\GIS\\CRFS_Map_Tool\\Output_Shapefiles\\Rockfish_long.shp', delete_layer=T)



wide = joined %>%
  mutate(year = paste0('Y', year)) %>%
  select(Block, Species_Catch, TripType_Description_effort, year, CPUA_All) %>%
  pivot_wider(names_from = year, values_from = CPUA_All) %>%
  select(Block, Species = Species_Catch, Trip = TripType_Description_effort, Y2004, Y2005, Y2006, Y2007, Y2008, Y2009, Y2010, Y2011,Y2012, Y2013, Y2014, Y2015, Y2016, Y2017, Y2018, Y2019, Y2020, Y2021, Y2022)  %>%
  mutate(CPUA_All = round(mean(c_across(Y2004:Y2022), na.rm = TRUE), 3))

layer_wide = blocks.SP %>%
  inner_join(wide, by = c( 'NM_INDEX' = 'Block'))

st_write(layer_wide, 'C:\\CRFS\\GIS\\CRFS_Map_Tool\\Output_Shapefiles\\Rockfish_wide.shp', delete_layer=T)


# blocks not in .shp
notinshape = filter(joined, !(Block %in% blocks.SP$NM_INDEX)) %>%
  group_by(Block) %>%
  count()

sum(notinshape$n)


layer = blocks.SP %>%
  inner_join(joined, by = c( 'NM_INDEX' = 'Block')) %>%
  select(Block = NM_INDEX, )

st_write(layer, 'C:\\CRFS\\GIS\\CRFS_Map_Tool\\Output_Shapefiles\\Rockfish_long.shp')

