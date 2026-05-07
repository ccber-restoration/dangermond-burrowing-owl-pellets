# The purpose of this script is to load the 10-m survey grid and match it to pellet data in order to map the locations of dissected pellets and begin to explore spatial patterns in diets


# 0. load packages ----
library(tidyverse)
library(mapview)
library(sf)
library(leafem)
library(readxl)
library(janitor)
library(spatialEco)


# 1.  read in pellet data to get list of quadrats with pellets ----
quadrats <- read_xlsx(path = "data/owl_pellet_data_downloaded_2026-04-21.xlsx", sheet = "Pellets") %>% 
  clean_names() %>%
  drop_na(pellet_catalog_number_qr_code) %>% 
  filter(location == "Dangermond Preserve" & use_in_study == "Yes") %>% 
  # filter out a non-Dangermond pellet
  filter(pellet_catalog_number_qr_code != "UCSB-IZC00077015") %>% 
  mutate(quadrat = case_when(
    # pellet UCSB-IZC00076987 was recorded with two adjacent quadrat numbers. Assign to the first one
    quadrat == "33512/33355" ~ "33512",
    .default = quadrat
  )) %>% 
  # select just the catalog number column
  select(quadrat) %>%
  # list of unique values
  unique() %>% 
  # use pull to change to vector (in R sense, not GIS-sense)
  pull()

quadrats


# 2. Read in spatial data for survey grid ----
#view layers 
st_layers("data/Cojo_Survey_10x10_Grid.gpkg")

#read in geopackage layer
survey_grid<- st_read("data/Cojo_Survey_10x10_Grid.gpkg", layer = "Survey_10x10_Grid") %>% 
  st_transform(crs = 4326) %>% 
  st_zm() 

survey_simple <- st_cast(survey_grid, "POLYGON")

mapview(survey_simple)

# show just the boundary of the survey grid
boundary <- sf_dissolve(survey_simple)

mapview(boundary, alpha.regions = 0.4, map.types = "Esri.WorldImagery")


## view two quadrats listed for pellet UCSB-IZC00076987 ----
ambiguous_location <- survey_simple %>% 
  filter(GridID %in% c("33512", "33355"))

mapview(ambiguous_location)


# 3. map quadrats with pellets ----
quadrats_with_pellets <- survey_simple %>% 
  filter(GridID %in% quadrats)

mapview(quadrats_with_pellets, map.types = "Esri.WorldImagery")

# this is only 23 locations (expecting 25)
# impossible to see 10x10 m quadrats when zoomed out to full AOI extent

## Check which two pellets not matching to survey grid ----

non_matched_quadrats <- quadrats %>% 
  as.data.frame() %>% 
  filter(!(. %in% survey_simple$GridID))

# quadrats 41589 (UCSB-IZC00077047) and 4127 (UCSB-IZC00077066) are not in the survey grid

# In email from Wayne on 2026-02-20, he clarified that 41589 was from fall 2024, before the survey grid existed
# " I would guess they were in or about quadrat 41509"

# But 41509 is not in the survey grid data we're using here!

# hard to see, so convert to points
centroids <- st_centroid(quadrats_with_pellets)

mapview(centroids, col.regions = "#87f7cbff", map.types = "Esri.WorldImagery")

centroids %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  write_csv("data/pellet_locations_for_URCA_poster_map.csv")
  

# 4. Map grid cells where >7 pellets were collected ----

# list of quadrats with at least 8 pellets
quad_high_density <- c(41509, 1285, 3622, 677)

grid <- survey_simple %>% 
  st_drop_geometry() %>% 
  mutate(high_density = case_when(
    GridID %in% quad_high_density ~ "8 or more pellets",
    .default = "< 8 pellets")) 

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
 
  
