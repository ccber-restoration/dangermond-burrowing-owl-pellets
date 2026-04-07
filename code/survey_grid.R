library(tidyverse)

library(mapview)

library(sf)
library(leafem)

st_layers("data/Cojo_Survey_10x10_Grid.gpkg")

#read in geopackage layer
survey_grid<- st_read("data/Cojo_Survey_10x10_Grid.gpkg", layer = "Survey_10x10_Grid") 


survey_simple <- st_cast(survey_grid, "POLYGON")

#list of quadrats with at least 8 pellets
quad_high_density <- c(41589, 1285, 3622, 677)

grid <- survey_simple %>% 
  st_drop_geometry() %>% 
  mutate(high_density = case_when(
    GridID %in% quad_high_density ~ "8 or more pellets",
    .default = "< 8 pellets"
  )) 

high_density_quads <- grid %>% 
  filter(high_density == "8 or more pellets")


grid_sf <- st_as_sf(x = grid, coords = c("X", "Y"), crs = 4326) 

high_density_quads_sf <-  st_as_sf(x = high_density_quads, coords = c("X", "Y"), crs = 4326) 


# map.types = "Esri.WorldImagery"
mapview(grid_sf, map.types = "Esri.WorldImagery")

mapview(high_density_quads_sf, map.types = "Esri.WorldImagery") %>%  
 addStaticLabels(label = high_density_quads_sf$GridID,
                  noHide = TRUE,
                  direction = 'top',
                  textOnly = FALSE,
                  textsize = "20px")
 
  
