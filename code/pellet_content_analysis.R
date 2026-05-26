# Setup ----

# load packages
library(tidyverse)
library(readxl)
library(cowplot)
library(rphylopic)
library(ggtext)

# Read in data ----
# read in ID_Counting sheet from Excel and assign to variable & exclude NCOS pellet data.
pellet_contents <- read_xlsx(path = "data/owl_pellet_data_downloaded_2026-04-21.xlsx", sheet = "ID_Counting") %>% 
  drop_na(catalog_number) %>% 
  filter(catalog_number != "UCSB-IZC00077015")

# Read in NCOS pellet data
NCOS_contents <- read.csv("data/nature_conservation_-056-151-s001.csv") %>%
  drop_na(Pellet.ID) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) 

# Dangermond Data Exploration ----
# check catalog numbers
# how many unique catalog numbers
n_distinct(pellet_contents$catalog_number)

catalog_numbers <- unique(pellet_contents$catalog_number)

catalog_numbers

# unique prey items

prey_taxa <- unique(pellet_contents$prey_taxon)

# Calculate proportions

count_sums <- pellet_contents %>% 
  group_by(catalog_number, `vert-invert`) %>% 
  summarize(count_sum = sum(MNI),
            n_records = n())

# Turn count_sums into a pivot table of the vertebrate/invertebrate prey counts
proportions <- count_sums %>%
  pivot_wider(id_cols = catalog_number, names_from = `vert-invert`, values_from = count_sum) %>% 
# Use mutate to create a total count column, then use that to calculate the proportion of vertebrate prey by number
  mutate(across(everything(), ~replace_na(.x, 0))) %>% # replacing any numerical NA values with 0
  mutate(total_prey = Vertebrate + Invertebrate) %>% # add column: sum of total # of vert and invert prey
  mutate(vert_proportion = Vertebrate/total_prey) %>% # this can later be multiplied by 100 to give a percentage
  mutate(invert_proportion = Invertebrate/total_prey) %>%
  mutate(Site = "Dangermond")

# Calculate total proportions of prey across all pellets
mean_invert_prop <- mean(proportions$invert_proportion)
mean_vert_prop <- mean(proportions$vert_proportion)

# NCOS Data Exploration ----
# check catalog numbers
# how many unique catalog numbers
n_distinct(NCOS_contents$Pellet.ID) # There are 33 unique pellet IDs/catalogue #s

unique(NCOS_contents$Pellet.ID)

NCOS_count_sums <- NCOS_contents %>% 
  group_by(Pellet.ID, Vert.or.Invert) %>% 
  summarize(count_sum = sum(Number.of.Individuals),
            n_records = n())

NCOS_proportions <- NCOS_count_sums %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  pivot_wider(id_cols = Pellet.ID, names_from = Vert.or.Invert, 
              values_from = count_sum) %>%
  mutate(total_prey = Vertebrate + Invertebrate) %>% 
  mutate(vert_proportion = Vertebrate/total_prey) %>% 
  mutate(invert_proportion = Invertebrate/total_prey) %>%
  mutate(Site = "NCOS") %>%
  rename(catalog_number = Pellet.ID)

# Calculate total proportion of invertebrate prey across all pellets
NCOS_mean_invert_prop <- mean(NCOS_proportions$invert_proportion)
NCOS_mean_vert_prop <- mean(NCOS_proportions$vert_proportion)

## Create taxon-based data frame for count of individuals ----
prey_taxa_lumped <- pellet_contents %>%
  select(prey_taxon, common_name, `vert-invert`, MNI) %>%
  group_by(common_name, `vert-invert`) %>%
  summarise(MNI_tot = sum(MNI)) %>%
  arrange(desc(MNI_tot)) %>%
  filter(MNI_tot != 0) %>%
# Group together/collapse repetitive/higher resolution taxa categories
  mutate(common_name = case_when(common_name %in% c("Rodent") ~ "Unidentified Rodent", TRUE ~ common_name), 
         common_name = case_when(common_name %in% c("Beetles") ~ "Unidentified Beetles",TRUE ~ common_name),
         common_name = case_when(common_name %in% c("Earwigs", "European earwig") ~ "Earwigs",TRUE ~ common_name),
         common_name = case_when(common_name %in% c("Grasshoppers, locusts, crickets", "Jerusalem crickets") ~ "Grasshoppers, locusts, crickets",TRUE ~ common_name)) %>%
  group_by(common_name, `vert-invert`) %>%
  summarise(across(where(is.numeric), sum))

## Create common-name-based data frame for occurrence frequency in pellets ----
# Lump together instances of taxa by how many times they appear in pellets
prey_occ <- pellet_contents %>% 
  select(catalog_number, common_name, `vert-invert`) %>%
  group_by(common_name, `vert-invert`) %>%
  count(common_name) %>%
  mutate(freq_occ = (n / n_distinct(pellet_contents$catalog_number))*100) %>%
  filter(common_name != "Insects") %>%
# Group together/collapse repetitive/higher resolution taxa categories
  mutate(common_name = case_when(common_name %in% c("Rodent") ~ "Unidentified Rodent", TRUE ~ common_name), 
         common_name = case_when(common_name %in% c("Beetles") ~ "Unidentified Beetles",TRUE ~ common_name),
         common_name = case_when(common_name %in% c("Earwigs", "European earwig") ~ "Earwigs",TRUE ~ common_name),
         common_name = case_when(common_name %in% c("Grasshoppers, locusts, crickets", "Jerusalem crickets") ~ "Grasshoppers, locusts, crickets",TRUE ~ common_name)) %>%
  group_by(common_name, `vert-invert`) %>%
  summarise(across(where(is.numeric), sum))

## Create a scientific-name-based taxon data frame for occurrence frequency ----
# Separate inverts and lump them by taxonomic order per pellet occurrence
inv_prey_occ <- pellet_contents %>%
  select(catalog_number, taxon_order, `vert-invert`) %>%
  group_by(catalog_number, taxon_order, `vert-invert`) %>%
  count(taxon_order) %>% # Lump together each occurring order
  mutate(n = ifelse(n > 1,1,n)) %>% # Renumber instances of taxa to 1 to count presence in a pellet once; account for duplicate instances. 
  filter(taxon_order != "Insecta", `vert-invert` == "Invertebrate") %>% # Isolate invertebrates for purely order-based lumping
  mutate(freq_occ = (n/n_distinct(pellet_contents$catalog_number)*100)) %>%
  group_by(taxon_order, `vert-invert`) %>%
  summarise(across(where(is.numeric), sum)) %>%
  rename(prey_taxon = taxon_order)

# Separate verts and lump by finest taxonomic resolution 
vert_prey_occ <- pellet_contents %>%
  select(catalog_number, prey_taxon, `vert-invert`) %>%
  group_by(catalog_number, prey_taxon, `vert-invert`) %>%
  filter(`vert-invert` == "Vertebrate") %>%
  count(prey_taxon) %>% #
  mutate(n = ifelse(n > 1,1,n)) %>% # Renumber instances of taxa to 1 to count presence in a pellet once; account for duplicate instances
  mutate(freq_occ = (n/n_distinct(pellet_contents$catalog_number)*100)) %>%
  group_by(prey_taxon, `vert-invert`) %>%
  summarise(across(where(is.numeric), sum))

sci_prey_occ <- rbind(inv_prey_occ, vert_prey_occ) %>% # Combine the lumped occurrence data subsets
  mutate(prey_taxon = case_when(prey_taxon %in% "Coleoptera" ~ "Coleoptera spp.", TRUE ~ prey_taxon),
         prey_taxon = case_when(prey_taxon %in% "Orthoptera" ~ "Orthoptera spp.", TRUE ~ prey_taxon),
         prey_taxon = case_when(prey_taxon %in% "Dermaptera" ~ "Dermaptera spp.", TRUE ~ prey_taxon),
         prey_taxon = case_when(prey_taxon %in% "Hemiptera" ~ "Hemiptera spp.", TRUE ~ prey_taxon))
  
# Figures ----
## Add images from phylopic ----
## Pick out the appropriate images
img_dermaptera <- pick_phylopic(name = "Dermaptera", n = 4, view = 4) # Select image 4
img_carabidae <- pick_phylopic(name = "Carabidae", n = 4, view = 4) # Select image 1
img_microtus <- pick_phylopic(name = "Microtus", n = 4, view = 4) # Select image 2
img_orthoptera <- pick_phylopic(name = "Orthoptera", n = 4, view = 4) # Select image 3
img_reithrodontomys <- pick_phylopic(name = "Neotominae", n = 4, view = 4) # Select image 2
img_rodentia <- pick_phylopic(name = "Rodentia", n = 4, view = 4) # Select image 3
img_coleoptera <- pick_phylopic(name = "Coleoptera", n = 4, view = 4) # Select image 2
img_scarabaeidae <- pick_phylopic(name = "Scarabaeidae", n = 4, view = 4) # Select image 4
img_tenebrionidae <- pick_phylopic(name = "Tenebrionidae", n = 4, view = 4) # Select image 1
img_staphylinidae <- pick_phylopic(name = "Staphylinidae", n = 4, view = 4) # Select image 3
img_sciuridae <- pick_phylopic(name = "Sciuridae", n = 7, view = 7) # Select image 6
img_pentatomidae <- pick_phylopic(name = "Pentatomidae", n = 4, view = 4) # Select image 4
img_buprestidae <- pick_phylopic(name = "Buprestidae", n = 4, view = 4) # Select image 2
img_peromyscus <- pick_phylopic(name = "Peromyscus", n = 4, view = 4) # Select image 2
img_curculionidae <- pick_phylopic(name = "Curculionidae", n = 4, view = 4) # Select image 2

## Total # individuals bar chart by common name ----
fig_prey_taxa <- ggplot(prey_taxa_lumped, 
                        aes(x = MNI_tot, 
                            y = reorder(common_name, MNI_tot), 
                                        fill = `vert-invert`)) +
  geom_bar(stat = "identity", color = "#003660ff") + 
  scale_fill_manual(values = c("#003660ff", "#87f7cbff")) +
  theme_bw() +
  labs(x = "Total Number of Individuals", y = "Prey Taxa", fill = "Classification") +
  theme(axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(size = 15),
        legend.position = "inside",
        legend.position.inside = c(0.75,0.75),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12),
        legend.box.background = element_rect(linewidth = 2, color = "#003660ff")) +
  # Add phylopic icons to the figure
  ## Dermaptera
  add_phylopic(img_dermaptera, x = 67, y = 15, 
               height = 0.70, fill = "white") +
  ## Carabidae
  add_phylopic(img_carabidae, x = 30, y = 14, 
               height = 0.75, fill = "white") +
  ## Orthoptera
  add_phylopic(img_orthoptera, x = 29, y = 13, 
               height = 0.7, fill = "white") +
  ## Curculionidae
  add_phylopic(rotate_phylopic(img_curculionidae, angle = 90), 
               x = 30, y = 12, 
               height = 0.80, fill = "#003660ff") +
  ## Microtus
  add_phylopic(img_microtus, x = 28, y = 11, 
               height = 0.60, fill = "#87f7cbff", color = "#003660ff") +
  ## Rodentia
  add_phylopic(flip_phylopic(img_rodentia, 
                             horizontal = TRUE, 
                             vertical = FALSE), 
               x = 12, y = 10, 
               height = 0.70, fill = "#87f7cbff", color = "#003660ff") +
  ## Reithrodontomys
  add_phylopic(img_reithrodontomys, 
               x = 11, y = 9, 
               height = 0.8, fill = "#87f7cbff", color = "#003660ff") +
  ## Scarabaeidae
  add_phylopic(rotate_phylopic(img_scarabaeidae, angle = 90), 
               x = 9, y = 8, 
               height = 0.70, fill = "#003660ff") +
  ## Coleoptera
  add_phylopic(img = rotate_phylopic(img_coleoptera, angle = 90), 
               x = 8, y = 7, 
               height = 0.70, fill = "#003660ff") +
  ## Tenebrionidae
  add_phylopic(flip_phylopic(img_tenebrionidae,
                             horizontal = TRUE,
                             vertical = FALSE), 
               x = 7, y = 6, 
               height = 1, fill = "#003660ff") +
  ## Staphylinidae
  add_phylopic(flip_phylopic(img_staphylinidae,
                             horizontal = TRUE,
                             vertical = FALSE), 
               x = 7, y = 5, 
               height = 0.40, fill = "#003660ff") +
  ## Sciuridae
  add_phylopic(flip_phylopic(img_sciuridae,
                             horizontal = TRUE,
                             vertical = FALSE), 
               x = 6, y = 4, 
               height = 0.80, fill = "#87f7cbff", color = "#003660ff") +
  ## Pentatomidae
  add_phylopic(flip_phylopic(img_pentatomidae,
                             horizontal = TRUE,
                             vertical = FALSE), 
               x = 7, y = 3, 
               height = 0.90, fill = "#003660ff") +
  ## Buprestidae
  add_phylopic(rotate_phylopic(img_buprestidae, angle = 90), 
               x = 6, y = 2, 
               height = 0.55, fill = "#003660ff") + 
  ## Peromyscus
  add_phylopic(img_peromyscus, 
               x = 6, y = 1, 
               height = 0.80, fill = "#87f7cbff", color = "#003660ff")

### Figure Data & Saving
# View the figure
fig_prey_taxa

# Save to a file as PDF
ggsave(filename = "figures/fig_prey_taxa.pdf",
       width = 8,
       height = 7)

# Save to a file as PNG
ggsave(filename = "figures/fig_prey_taxa.png",
       width = 8,
       height = 7)

## Prey occurrence in pellets bar chart by common name ---- 
fig_prey_occ <- ggplot(prey_occ, 
                       aes(x = freq_occ, 
                           y = reorder(common_name, freq_occ), 
                           fill = `vert-invert`)) +
  geom_bar(stat = "identity", color = "#003660ff") + 
  xlim(0, 100) +
  scale_fill_manual(values = c("#003660ff", "#87f7cbff")) +
  theme_bw() +
  labs(x = "% Occurrence in Pellets", y = "Prey Taxa", fill = "Classification") +
  theme(axis.title = element_text(face = "bold", size = 18, 
                                  family = "serif", color = "#003660ff"),
        axis.text = element_text(size = 15),
        legend.position = "inside",
        legend.position.inside = c(0.75,0.60),
        legend.title = element_text(size = 15, face = "bold",
                                    family = "serif", color = "#003660ff"),
        legend.text = element_text(size = 12),
        legend.box.background = element_rect(size = 2, color = "#003660ff")) +
  
  # Add phylopic icons to the figure
  ## Dermaptera
  add_phylopic(img_dermaptera, x = 84, y = 15, 
               height = 0.70, fill = "white") +
  ## Carabidae
  add_phylopic(img_carabidae, x = 82, y = 14, 
               height = 0.75, fill = "#003660ff") +
  ## Microtus
  add_phylopic(img_microtus, x = 80, y = 13, 
               height = 0.60, fill = "#87f7cbff", color = "#003660ff") +
  ## Orthoptera
  add_phylopic(img_orthoptera, x = 65, y = 12, 
               height = 0.70, fill = "#003660ff") +
  ## Rodentia
  add_phylopic(flip_phylopic(img_rodentia,
                             horizontal = TRUE, 
                             vertical = FALSE), 
               x = 32, y = 11, 
               height = 0.70, fill = "#87f7cbff", color = "#003660ff") +
  ## Curculionidae
  add_phylopic(rotate_phylopic(img_curculionidae, angle = 90), 
               x = 27, y = 10, 
               height = 0.80, fill = "#003660ff") +
  ## Reithrodontomys
  add_phylopic(img_reithrodontomys, 
               x = 22, y = 9, 
               height = 0.8, fill = "#87f7cbff", color = "#003660ff") +
  ## Coleoptera
  add_phylopic(img = rotate_phylopic(img_coleoptera, angle = 90), 
               x = 15, y = 8, 
               height = 0.70, fill = "#003660ff") +
  ## Scarabaeidae
  add_phylopic(rotate_phylopic(img_scarabaeidae, angle = 90), 
               x = 15, y = 7, 
               height = 0.70, fill = "#003660ff") +
  ## Tenebrionidae
  add_phylopic(flip_phylopic(img_tenebrionidae,
                             horizontal = TRUE,
                             vertical = FALSE), 
               x = 15, y = 6, 
               height = 1, fill = "#003660ff") +
  ## Staphylinidae
  add_phylopic(flip_phylopic(img_staphylinidae,
                             horizontal = TRUE,
                             vertical = FALSE), 
               x = 15, y = 5, 
               height = 0.40, fill = "#003660ff") +
  ## Sciuridae
  add_phylopic(flip_phylopic(img_sciuridae,
                             horizontal = TRUE,
                             vertical = FALSE), 
               x = 10, y = 4, 
               height = 0.80, fill = "#87f7cbff", color = "#003660ff") +
  ## Pentatomidae
  add_phylopic(flip_phylopic(img_pentatomidae,
                             horizontal = TRUE,
                             vertical = FALSE), 
               x = 12, y = 3, 
               height = 0.90, fill = "#003660ff") +
  ## Buprestidae
  add_phylopic(rotate_phylopic(img_buprestidae, angle = 90), 
               x = 11, y = 2, 
               height = 0.55, fill = "#003660ff") + 
  ## Peromyscus
  add_phylopic(img_peromyscus, 
               x = 11, y = 1, 
               height = 0.80, fill = "#87f7cbff", color = "#003660ff")

### Figure Data & Saving
# View the figure
fig_prey_occ

# Save to a file as PDF
ggsave(filename = "figures/fig_prey_occ.pdf",
       width = 8,
       height = 7)

# Save to a file as PNG
ggsave(filename = "figures/fig_prey_occ.png",
       width = 8,
       height = 7)

## Prey occurrence in pellets bar chart by scientific name ----
fig_sci_prey_occ <- ggplot(sci_prey_occ, 
                       aes(x = freq_occ, 
                           y = reorder(prey_taxon, freq_occ), 
                           fill = `vert-invert`)) +
  geom_bar(stat = "identity", color = "#003660ff") + 
  xlim(0, 100) +
  scale_fill_manual(values = c("#003660ff", "#87f7cbff")) +
  theme_bw() +
  labs(title = "Taxa occurrence in pellet samples", 
       x = "% Occurrence in Pellets", y = "Prey Taxon", fill = "Classification") +
  
  # TODO Italicize the species names where applicable
  theme(title = element_text(face = "bold", size = 18, 
                                  family = "serif", color = "#003660ff"),
        axis.title = element_text(face = "bold", size = 15, 
                                  family = "serif", color = "#003660ff"),
        axis.text = element_text(size = 15, family = "serif"),
        legend.position = "inside",
        legend.position.inside = c(0.75,0.15),
        legend.title = element_text(size = 18, face = "bold",
                                    family = "serif", color = "#003660ff"),
        legend.text = element_text(size = 15, family = "serif"),
        legend.box.background = element_rect(linewidth = 2, color = "#003660ff")) +
  
  # Add phylopic icons to the figure
  ## Coleoptera
  add_phylopic(img = rotate_phylopic(img_coleoptera, angle = 90), 
               x = 90, y = 9, 
               height = 0.70, fill = "#003660ff") +
  ## Microtus
  add_phylopic(img_microtus, x = 77, y = 8, 
               height = 0.60, fill = "#87f7cbff", color = "#003660ff") +
  ## Dermaptera
  add_phylopic(img_dermaptera, x = 69, y = 7, 
               height = 0.70, fill = "#003660ff") +
  ## Orthoptera
  add_phylopic(img_orthoptera, x = 54, y = 6, 
               height = 0.70, fill = "#003660ff") +
  ## Rodentia
  add_phylopic(flip_phylopic(img_rodentia,
                             horizontal = TRUE, 
                             vertical = FALSE), 
               x = 38, y = 5, 
               height = 0.70, fill = "#87f7cbff", color = "#003660ff") +
  ## Reithrodontomys
  add_phylopic(img_reithrodontomys, 
               x = 26, y = 4, 
               height = 0.8, fill = "#87f7cbff", color = "#003660ff") +
  ## Sciuridae
  add_phylopic(flip_phylopic(img_sciuridae,
                             horizontal = TRUE,
                             vertical = FALSE), 
               x = 12, y = 3, 
               height = 0.80, fill = "#87f7cbff", color = "#003660ff") +
  ## Peromyscus
  add_phylopic(img_peromyscus, 
               x = 13, y = 2, 
               height = 0.80, fill = "#87f7cbff", color = "#003660ff") +
  ## Pentatomidae
  add_phylopic(flip_phylopic(img_pentatomidae,
                             horizontal = TRUE,
                             vertical = FALSE), 
               x = 14, y = 1, 
               height = 0.90, fill = "#003660ff")


### Figure Data & Saving
# View the figure
fig_sci_prey_occ

# Save to a file as PDF
ggsave(filename = "figures/fig_sci_prey_occ.pdf",
       width = 8,
       height = 7)

# Save to a file as PNG
ggsave(filename = "figures/fig_sci_prey_occ.png",
       width = 8,
       height = 7)

## Invertebrate Props Scatter ----
sites_inv_prop <- rbind(proportions, NCOS_proportions) # Combine the proportions for both Dangermond and NCOS

fig_scatter_inv_prop <- ggplot(sites_inv_prop, aes(x = Site, y = invert_proportion)) +
  geom_point(alpha = 0) + 
  geom_jitter(aes(color = Site, fill = Site), 
              width = 0.15, alpha = 0.5, size = 3, pch = 21) +

  # Add the overall proportion of invertebrate prey for Dangermond
  geom_hline(yintercept = mean_invert_prop, linetype = "dashed", color = "darkolivegreen4",linewidth = 1) +
  geom_text(aes(0, mean_invert_prop, label = round(mean_invert_prop,2), 
                vjust = -1, hjust = -0.2), color = "darkolivegreen4") +
  
  # Add the overall proportion of invertebrate prey for NCOS
  geom_hline(yintercept = NCOS_mean_invert_prop, linetype = "dashed", color = "#fe7c11", linewidth = 1) +
  geom_text(aes(0, NCOS_mean_invert_prop, label = round(NCOS_mean_invert_prop,2), 
                vjust = -1, hjust = -0.2), color = "#fe7c11") +
  
  scale_fill_manual(values = c("darkolivegreen3", "#feac11")) +
  scale_color_manual(values = c("darkolivegreen", "#fe6c11")) +
  theme_bw() +
  labs(x = "Study Site", y = "Invertebrate Prey Proportion", color = "Site") +
  theme(axis.title = element_text(size = 18, face = "bold", 
                                  family = "serif", color = "#003660ff"),
        axis.text = element_text(size = 15),
        legend.title = element_text(face = "bold"),
        legend.position = "none")

# View the figure
fig_scatter_inv_prop

# Save to a file as PDF
ggsave(filename = "figures/fig_scatter_inv_prop.pdf",
       width = 5,
       height = 6)

# Save to a file as PNG
ggsave(filename = "figures/fig_scatter_inv_prop.png",
       width = 5,
       height = 6)

## Vertebrate Props Scatter ----
fig_scatter_vert_prop <- ggplot(sites_inv_prop, aes(x = Site, y = vert_proportion)) +
  geom_point(alpha = 0) + 
  geom_jitter(aes(color = Site, fill = Site), 
              width = 0.15, alpha = 0.5, size = 3, pch = 21) +
  
  # Add the overall proportion of invertebrate prey for Dangermond
  geom_hline(yintercept = mean_vert_prop, linetype = "dashed", color = "darkolivegreen4",linewidth = 1) +
  geom_text(aes(0, mean_vert_prop, label = round(mean_vert_prop,2), 
                vjust = -1, hjust = -0.2), color = "darkolivegreen4") +
  
  # Add the overall proportion of invertebrate prey for NCOS
  geom_hline(yintercept = NCOS_mean_vert_prop, linetype = "dashed", color = "#fe7c11", linewidth = 1) +
  geom_text(aes(0, NCOS_mean_vert_prop, label = round(NCOS_mean_vert_prop,2), 
                vjust = -1, hjust = -0.2), color = "#fe7c11") +
  
  scale_fill_manual(values = c("darkolivegreen3", "#feac11")) +
  scale_color_manual(values = c("darkolivegreen", "#fe6c11")) +
  theme_bw() +
  labs(x = "Study Site", y = "Vertebrate Prey Proportion", color = "Site") +
  theme(axis.title = element_text(size = 18, face = "bold", 
                                  family = "serif", color = "#003660ff"),
        axis.text = element_text(size = 15),
        legend.title = element_text(face = "bold"),
        legend.position = "none")

# View the figure
fig_scatter_vert_prop

# Save to a file as PDF
ggsave(filename = "figures/fig_scatter_vert_prop.pdf",
       width = 5,
       height = 6)

# Save to a file as PNG
ggsave(filename = "figures/fig_scatter_vert_prop.png",
       width = 5,
       height = 6)

## Stacked bar chart for vert contents only ----
fig_vert_stacked <- ggplot(data = pellet_contents_vert, aes(x = catalog_number, y = MNI, fill = prey_taxon)) +
  geom_col() + 
  ylab("Minimum number of individuals") +
  xlab("Catalog number") +
  guides(fill = guide_legend(title = "Prey taxon")) +
  theme_cowplot() +
  scale_y_continuous(expand = c(0,0))

# view figure
fig_vert_stacked

# save to file
ggsave(filename = "figures/fig_vert_stacked.pdf",
       width = 8,
       height = 6)

# Statistical Analyses ----
## Determine Normality
fig_box_inv_prop <- ggplot(sites_inv_prop, aes(x = Site, y = invert_proportion)) + 
  geom_jitter(fill = "black") +
  geom_boxplot(alpha = 0.2, fill = "transparent") +
  theme_bw() +
  labs(x = "Study Site", y = "Invertebrate Prey Proportion") + 
  theme(axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12))

# View the figure
fig_box_inv_prop 

# Normality tests
shapiro.test(proportions$invert_proportion) # for Dangermond; NOT normal
shapiro.test(NCOS_proportions$invert_proportion) # for NCOS; NOT normal

# Test of statistical difference/significance between the means for both sites
t.test(NCOS_proportions$invert_proportion, alternative = "greater", 
       mu = mean(proportions$invert_proportion))
t.test(proportions$invert_proportion, alternative = "greater", 
       mu = mean(NCOS_proportions$invert_proportion))
