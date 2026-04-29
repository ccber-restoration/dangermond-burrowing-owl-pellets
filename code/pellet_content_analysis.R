# Setup ----

# load packages
library(tidyverse)
library(readxl)
library(cowplot)

# Read in data ----
# read in ID_Counting sheet from Excel and assign to variable & exclude NCOS pellet data.
pellet_contents <- read_xlsx(path = "data/owl_pellet_data_downloaded_2026-04-21.xlsx", sheet = "ID_Counting") %>% 
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

# Turn count_sums into a pivot table of the vertebrate/invertebrate prey counts
proportions <- count_sums %>%
  pivot_wider(id_cols = catalog_number, names_from = `vert-invert`, values_from = count_sum) %>% 
# Use mutate to create a total count column, then use that to calculate the proportion of vertebrate prey by number
  mutate(across(everything(), ~replace_na(.x, 0))) %>% # replacing any numerical NA values with 0
  mutate(total_prey = Vertebrate + Invertebrate) %>% # add column: sum of total # of vert and invert prey
  mutate(vert_proportion = Vertebrate/total_prey) %>% # this can later be multiplied by 100 to give a percentage
  mutate(invert_proportion = Invertebrate/total_prey) %>%
  mutate(Site = "Dangermond")

# Subset data ----
## create vertebrate & invert subsets ----
pellet_contents_vert <- pellet_contents %>% 
  filter(`vert-invert` == "Vertebrate")

pellet_contents_invert <- pellet_contents %>% 
  filter(`vert-invert` == "Invertebrate")


view(prey_taxa)

## Create taxon-based dataframe for count of individuals ----
prey_taxa_lumped <- pellet_contents %>%
  select(prey_taxon, common_name, MNI) %>%
  group_by(common_name) %>%
  summarise(MNI_tot = sum(MNI)) %>%
  arrange(desc(MNI_tot)) %>%
  filter(MNI_tot != 0)

# Figures ----
## Taxa Individuals Chart ----
# Create horizontally-aligned bar chart for the total count of individuals across taxa as referred to by their common names.
fig_prey_taxa <- ggplot(prey_taxa_lumped, aes(x = MNI_tot, y = reorder(common_name, MNI_tot))) +
  geom_bar(stat = "identity", fill = "#87f7cbff", color = "#003660ff") + 
  theme_bw() +
  labs(x = "Frequency", y = "Prey Taxa") +
  theme(axis.title = element_text(face = "bold", size = 12))

# View the figure
fig_prey_taxa

# Save to a file
ggsave(filename = "figures/fig_prey_taxa.pdf",
       width = 8,
       height = 8)

## Univariate Dangermond Insects Props Figure ----
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

# TODO
# FIXME
