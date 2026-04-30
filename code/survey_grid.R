# The purpose of this script is to load the 10-m survey grid and match it to pellet data in order to map the locations of dissected pellets and begin to explore spatial patterns in diets


# 0. load packages ----
library(tidyverse)
library(mapview)
library(sf)
library(leafem)
library(readxl)
library(janitor)


# 1.  read in pellet data to get list of quadrats with pellets ----
quadrats <- read_xlsx(path = "data/owl_pellet_data_downloaded_2026-04-21.xlsx", sheet = "Pellets") %>% 
  clean_names() %>% 
  drop_na(pellet_catalog_number_qr_code) %>% 
  filter(location == "Dangermond Preserve") %>% 
  # filter out a non-Dangermond pellet
  filter(pellet_catalog_number_qr_code != "UCSB-IZC00077015") %>% 
  # select just the catalog number column
  select(quadrat) %>% 
  # list of unique values
  unique() %>% 
  # use pull to change to vector (in R sense, not GIS-sense)
  pull()

# note that this has 31 values; not all quadrats had *intact* pellets
# need to decide what to do about "33512/33355". Map both?

# 2. Read in spatial data for survey grid ----
#view layers 
st_layers("data/Cojo_Survey_10x10_Grid.gpkg")

#read in geopackage layer
survey_grid<- st_read("data/Cojo_Survey_10x10_Grid.gpkg", layer = "Survey_10x10_Grid") %>% 
  st_transform(crs = 4326) %>% 
  st_zm() 

survey_simple <- st_cast(survey_grid, "POLYGON")

mapview(survey_simple)



# 3. map quadrats with pellets ----
quadrats_with_pellets <- survey_simple %>% 
  filter(GridID %in% quadrats)

mapview(quadrats_with_pellets, map.types = "Esri.WorldImagery")

# hard to see, so convert to points

centroids <- st_centroid(quadrats_with_pellets)

mapview(centroids, col.regions = "#87f7cbff", map.types = "Esri.WorldImagery")

# 4. Map grid cells where >7 pellets were collected ----

# list of quadrats with at least 8 pellets
quad_high_density <- c(41589, 1285, 3622, 677)

grid <- survey_simple %>% 
  st_drop_geometry() %>% 
  mutate(high_density = case_when(
    GridID %in% quad_high_density ~ "8 or more pellets",
    .default = "< 8 pellets"
  )) 

grid_sf <- st_as_sf(x = grid, coords = c("X", "Y"), crs = 4326) 

high_density_quads <- grid %>% 
  filter(high_density == "8 or more pellets")

high_density_quads_sf <-  st_as_sf(x = high_density_quads, coords = c("X", "Y"), crs = 4326) 

mapview(high_density_quads_sf, map.types = "Esri.WorldImagery") %>%  
 addStaticLabels(label = high_density_quads_sf$GridID,
                  noHide = TRUE,
                  direction = 'top',
                  textOnly = FALSE,
                  textsize = "20px")
 
  
