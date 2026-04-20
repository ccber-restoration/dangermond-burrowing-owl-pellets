# Setup ----

# load packages
library(tidyverse)
library(readxl)
library(cowplot)

# Read in data ----
# read in ID_Counting sheet from Excel and assign to variable
pellet_contents <- read_xlsx(path = "data/owl_pellet_data_downloaded_2026-04-20.xlsx", sheet = "ID_Counting") %>% 
  drop_na(catalog_number) %>% 
  filter(catalog_number != "UCSB-IZC00077015")

# Data exploration/checking ----
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

# TODO- use mutate to create a total count column, then use that to calculate the proportion of vertebrate prey by number
proportions <- count_sums %>% 
  pivot_wider(id_cols = catalog_number, names_from = `vert-invert`, values_from = count_sum) %>% 
  mutate(vert_ratio = Vertebrate/Invertebrate)


# Subset data ----
# create vertebrate & invert subsets
pellet_contents_vert <- pellet_contents %>% 
  filter(`vert-invert` == "Vertebrate")

pellet_contents_invert <- pellet_contents %>% 
  filter(`vert-invert` == "Invertebrate")


view(prey_taxa)

# Figures ----

## Verts ----
# stacked bar chart for vert contents only
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

# TODO
# FIXME
