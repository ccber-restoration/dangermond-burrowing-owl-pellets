# the purpose of this script is to create a map depicting

# (a) the context of the Cojo Terrace/Dangermond within California
# (b) the boundary of the Cojo Terrace
# (c) the locations where intact pellets were collected
# (d) some metric of prey composition for each sampled pellet

#  Borrow heavily from this code: https://github.com/ccber-restoration/NCOS-Hibernacula-Study/blob/main/NCOS_Hibernacula_Study.qmd

library(tidyverse)

# Mapping and spatial
library(here)
library(leaflet)       # interactive maps
library(mapview)
library(RColorBrewer)  # color palettes for plots and maps
library(maptiles)
library(terra)
library(sf)
library(ggplot2)
library(cowplot)
library(maps)
library(ggspatial)
library(ggnewscale)    # for multiple fill scales in ggplot


# load layers ----

# inputs: layers as sf objects, all in same CRS EPSG 4326 (WGS84)

# boundary ----
st_layers("data/Cojo_West_Study_Area.gpkg")

#read in geopackage layer for boundary
boundary <- st_read("data/Cojo_West_Study_Area.gpkg", layer = "Study_Area") %>% 
  st_transform(crs = 4326) %>% 
  st_zm() 

mapview(boundary, map.types = "Esri.WorldImagery")


# create bounding box for the inset map
bbox <- st_bbox(boundary)

mapview(bbox, map.types = "Esri.WorldImagery")

bbox_sf <- st_as_sf(st_as_sfc(bbox), crs=4326)


# Download satellite tiles (Esri)

# this approach might now work because the study area is so large... but this is what Garrett used for NCOS
map_basemap <- get_tiles(boundary, provider = "Esri.WorldImagery", zoom = 18)


# Convert raster to data.frame for ggplot
map_rgb_df <- as.data.frame(map_basemap, xy = TRUE)
colnames(map_rgb_df) <- c("x", "y", "red", "green", "blue")
map_rgb_df$hex <- rgb(map_rgb_df$red, map_rgb_df$green, map_rgb_df$blue, maxColorValue = 255)



# Main satellite deployment map with habitat layer
map_main <- ggplot() +
  geom_raster(data = map_rgb_df, aes(x = x, y = y, fill = hex)) +
  scale_fill_identity() +
  # Add NCOS boundary (yellow outline, no fill)
  geom_sf(data = boundary |> mutate(boundary = "Dangermond Boundary"),
          aes(linetype = boundary), color = "yellow", linewidth = 1) +
  scale_linetype_manual(values = "solid", name = NULL) +
  # Add habitat polygons (semi-transparent overlay on satellite)
  # ggnewscale::new_scale_fill() +
  #geom_sf(data = ncos_habitats_filtered, aes(fill = habitat_simple), color = NA, alpha = 0.5) +
  # scale_fill_manual(values = habitat_colors, name = "Habitat Type") +
  # Add camera deployment points
  ggnewscale::new_scale_fill() +
  geom_sf(data = map_site_sf, aes(fill = feature_type_methodology, size = detections_per_camera_day),
          shape = 21, color = "black", stroke = 0.8, alpha = 0.9) +
  scale_fill_manual(values = feature_colors, name = "Feature Type") +
  scale_size(name = "Avg. Daily Obs.", range = c(2, 6)) +
  coord_sf(xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
           ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]),
           clip = "on") +
  annotation_scale(
    location = "bl",      # bottom left
    pad_x = unit(0.45, "in"),
    pad_y = unit(0.3, "in"),
    width_hint = 0.25,
    text_col = "white"   # White text
  ) +
  # add north arrow
  annotation_north_arrow(
    location = "br",      # bottom right
    which_north = "true",
    pad_x = unit(0.35, "in"),
    pad_y = unit(0.23, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  # theme settings
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.box = "vertical",
    legend.box.spacing = unit(0, "cm"),
    legend.spacing.y = unit(0, "cm"),
    legend.margin = margin(t = 0, b = 0),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.key.height = unit(0.4, "cm")
  ) +
  # legend settings
  guides(
    linetype = guide_legend(order = 1, override.aes = list(color = "yellow", linewidth = 1)),
    fill = guide_legend(order = 2, nrow = 1, override.aes = list(alpha = 0.9, size = 4, shape = 21, stroke = 0.8)),
    size = guide_legend(order = 3, nrow = 1, override.aes = list(fill = "black", shape = 21, stroke = 0.8))
  )

# Calculate study area centroid
map_centroid_coords <- boundary %>%
  st_coordinates() %>%
  as.data.frame() %>%
  summarise(longitude = mean(X), latitude = mean(Y))

# Overview map of CA with study area location

# this take a little while because reasonably high-resolution
map_CA <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  st_as_sf() %>%
  filter(ID == "california")

mapview(map_CA)

map_overview <- ggplot() +
  geom_sf(data = map_CA, fill = "gray90", color = "gray60", size = 0.3) +
  geom_sf(data = bbox_sf) +
  geom_point(data = map_centroid_coords, aes(x = longitude, y = latitude), 
             color = "red", size = 3, shape = 15) +
  theme_void() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.background = element_rect(fill = "white", color = NA)
  )

map_overview


# Combine maps with proper positioning
map_final <- ggdraw() +
  draw_plot(map_main) +
  draw_plot(map_overview, x = 0.68, y = 0.647, width = 0.3, height = 0.3)

map_final

ggsave("figures/map_draft.png", 
       plot = map_final, 
       width = 7, 
       height = 9, 
       dpi = 300, 
       bg = "white")


