# Header ------------------------------------------------------------------

# Load Libraries
library(readr)
library(janitor)
library(flextable)
library(corrr)
library(corrplot)
library(scales)
library(lme4)
library(gtsummary)
library(sf)
library(gt)
library(ggVennDiagram)
library(tidyverse)

# Set the location
#location <- "personal"
location <- "HPC"

if (location == "personal") {
  # Set Working Directory (Personal)
  setwd("C:/Users/Steven/OneDrive - Johns Hopkins/Dissertation/Dissertation/Data")
}

if (location == "HPC") { 
  # Set Working Directory (HPC)
  setwd("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH")
}

# Read in the rural datasets
#data_aim2 <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/data_aim2.rds")
# rural         <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/rural.rds")
descriptive   <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/descriptive.rds")
rural_hh    <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/rural_hh.rds")
under5_animal <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/under5_animal.rds")
#under5_dia    <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/under5_dia.rds")

# Mapping ------------------------------------------------------

# Get the Lat, Long, U/R designation, and Year from the dataset
# Drop all the missing and remove any improbable values
mapping <- descriptive %>% 
  select(LATNUM, LONGNUM, region, kgc_course) %>% 
  drop_na() %>% 
  filter(!between(LATNUM, -0.0001, 0.0001))

# Read in the Africa Shapefile
shapefile <- read_sf("Mapping/Africa/afr_g2014_2013_0.shp") %>% clean_names()

# Check the maximum extent of the households
max(mapping$LATNUM)
min(mapping$LATNUM)
max(mapping$LONGNUM)
min(mapping$LONGNUM)

# Crop the shapefile using the numbers from above
shapefile <- shapefile %>% 
  st_crop(c(xmin = -18, ymin = -35, 
            xmax = 51, ymax = 26))

# Set the CRS for the Africa Shapefile
shapefile <- st_transform(shapefile, crs = 4326)

# Set the CRS for the datapoints
points <- mapping %>% 
  st_as_sf(coords = c("LONGNUM", "LATNUM"), crs = 4326)

# Plot the Region households
ggplot() + 
  geom_sf(data = shapefile, fill = "white") +
  geom_sf(data = points, aes(color = region), size = 0.4, alpha = 0.15) +
  scale_color_manual(
    values = c("Eastern Africa" = "#e41a1c", "Middle Africa" = "#377eb8",
               "Southern Africa" = "#4daf4a", "Western Africa" = "#984ea3"),
    name = "Region") +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1))) + # Larger legend points
  theme_void() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.25, 0.45),
        legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 0.9, size = 22))


# Plot the KGZ households
ggplot() + 
  geom_sf(data = shapefile, fill = "white") +
  geom_sf(data = points, aes(color = kgc_course), size = 0.4, alpha = 0.15) +
  scale_color_manual(
    values = c("Dry" = "#e41a1c", "Temperate" = "#377eb8",
               "Tropical" = "#4daf4a"),
    name = "Koppen-Geiger Zone") +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1))) + # Larger legend points
  theme_void() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.25, 0.45),
        legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 0.9, size = 22))

# Animal Presence Category ------------------------------------------------

descriptive %>% 
  group_by(animal_singleonly) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4)*100) %>%
  mutate(animal_singleonly = recode(animal_singleonly, `none` = "No Animals", `chicken/poultry/duck only` = "Chicken/Poultry/Duck Only", 
                               `goat/sheep only` = "Goat/Sheep Only", `bull/cow/cattle only` = "Bull/Cow/Cattle Only",
                               `horse/donkey only` = "Horse/Donkey Only", `multispecies` = ">1 Species", 
                               `other` = "Other")) %>%
  mutate(animal_singleonly = factor(animal_singleonly, levels = c("No Animals", "Chicken/Poultry/Duck Only", "Goat/Sheep Only",
                                          "Bull/Cow/Cattle Only", "Horse/Donkey Only",  ">1 Species",
                                          "Other"))) %>%
  arrange(animal_singleonly) %>% 
  adorn_totals("row",,,,c(n, n_under5, num_diarrhea)) %>%
  qflextable() %>% 
  set_header_labels(animal_singleonly = "Single Animal Category", n = "Number of People", 
                    n_under5 = "Children Under 5", num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Diarrhea Prevalence by Single Animal Group")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)


# Animals by Household ----------------------------------------------------

# Number of animals by household, disaggregated categories
  rural_hh %>%
  group_by(country) %>% 
  summarise(n = n(),
            n_chicken = sum(chicken_poultry_present == 1, na.rm = T),
            n_goat = sum(goat_present == 1, na.rm = T),
            n_bull = sum(bull_cow_present == 1, na.rm = T),
            n_cattle = sum(cattle_present == 1, na.rm = T),
            n_horse = sum(horse_donkey_present == 1, na.rm = T),
            n_pig = sum(pig_present == 1, na.rm = T),
            n_other = sum(other_present == 1, na.rm = T),
            n_duck = sum(duck_present == 1, na.rm = T),
            n_camel = sum(camel_present == 1, na.rm = T),
            n_rabbit = sum(rabbit_present == 1, na.rm = T),
            n_bee = sum(bee_present == 1, na.rm = T),
            n_rodent = sum(rodent_present == 1, na.rm = T),
            n_sheep = sum(sheep_present == 1, na.rm = T)) %>%
    adorn_totals(where = c("row")) %>% 
    qflextable() %>% 
    set_header_labels(n = "# HH",
                      n_chicken = "Chickens", n_goat = "Goats",
                      n_sheep = "Sheep",
                      n_bull = "Bulls", n_cattle = "Cattle",
                      n_horse = "Horse", n_pig = "Pigs",
                      n_other = "Other", n_duck = "Ducks",
                      n_camel = "Camels", n_rabbit = "Rabbits",
                      n_bee = "Bees", n_rodent = "Rodents")%>% 
    theme_zebra() %>% theme_box() %>% 
    align_nottext_col(align = "center", header = TRUE) %>%
    align_text_col(align = "center", header = TRUE)

# Number of animals by household, aggregated categories
  rural_hh %>%
  group_by(country) %>% 
  summarise(n = n(),
            n_chicken = sum(hv246_chicken_poultry_duck_total_cat >= 1, na.rm = T),
            n_goat = sum(hv246_goat_sheep_total_cat >= 1, na.rm = T),
            n_bull = sum(hv246_bull_cow_cattle_total_cat >= 1, na.rm = T),
            n_rum = sum(hv246_ruminant_total_cat >= 1, na.rm = T),
            n_both = n_goat + n_bull - n_rum,
            n_horse = sum(hv246_horse_donkey_total_cat >= 1, na.rm = T),
            n_pig = sum(hv246_pig_total_cat >= 1, na.rm = T),
            n_other = sum(hv246_other_total_cat >= 1, na.rm = T)) %>%
  adorn_totals(where = c("row")) %>% 
  qflextable() %>% 
  set_header_labels(country = "Country", n = "# HH",
                    n_chicken = "Chicken/Poultry/Duck", n_goat = "Goat/Sheep", 
                    n_bull = "Cattle/Cow/Bull", n_rum = "Ruminants (cattle/cow/bull/goat/sheep)",
                    n_both = "Own both ruminant types", n_horse = "Horse/Donkey", n_pig = "Pigs",
                    n_other = "Other")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

  
  
  gabon <- rural_hh %>% filter(country == "Gabon")
  
   rural_2 <- gabon %>% 
    ungroup() %>% 
    select(hv246_bull_cow_cattle_total_cat, hv246_goat_sheep_total_cat)
  
  my_list <- list("Bull/Cow/Cattle" = which(rural_2$hv246_bull_cow_cattle_total_cat >= 1), 
                  "Goat/Sheep" = which(rural_2$hv246_goat_sheep_total_cat >= 1))
  
  ggVennDiagram(my_list, label_alpha = 0, set_color = c("darkred","darkgreen"), set_size = 6,
                label_percent_digit = 1) + 
    scale_fill_distiller(palette = "Reds", direction = 1) + 
    scale_x_continuous(expand = expansion(mult = .2))+ 
    guides(fill = guide_legend(title = "Household Count")) +
    theme(legend.position = "bottom")
  
# Animals by Region --------------------------------------------------------
  
  region_table <- descriptive %>%
    group_by(region) %>%
    summarise(
      n = n(),
      n_chicken = sum(hv246_chicken_poultry_duck_total_cat >= 1, na.rm = TRUE),
      n_goat = sum(hv246_goat_sheep_total_cat >= 1, na.rm = T),
      n_bull = sum(hv246_bull_cow_cattle_total_cat >= 1, na.rm = T),
      n_horse = sum(hv246_horse_donkey_total_cat >= 1, na.rm = T),
      n_pig = sum(hv246_pig_total_cat >= 1, na.rm = T)) %>%
    mutate(chicken_info = paste0(comma(n_chicken), " (", percent((n_chicken / n), accuracy = 0.1), ")"),
           goat_info = paste0(comma(n_goat), " (", percent((n_goat / n), accuracy = 0.1), ")"),
           bull_info = paste0(comma(n_bull), " (", percent((n_bull / n), accuracy = 0.1), ")"),
           horse_info = paste0(comma(n_horse), " (", percent((n_horse / n), accuracy = 0.1), ")"),
           pig_info = paste0(comma(n_pig), " (", percent((n_pig / n), accuracy = 0.1), ")")) %>%
    bind_rows(summarise(., region = "Total", n = sum(n),
                        chicken_info = paste0(comma(sum(n_chicken)), " (", percent(sum(n_chicken) / sum(n)), ")"),
                        goat_info = paste0(comma(sum(n_goat)), " (", percent(sum(n_goat) / sum(n)), ")"),
                        bull_info = paste0(comma(sum(n_bull)), " (", percent(sum(n_bull) / sum(n)), ")"),
                        horse_info = paste0(comma(sum(n_horse)), " (", percent(sum(n_horse) / sum(n)), ")"),
                        pig_info = paste0(comma(sum(n_pig)), " (", percent(sum(n_pig) / sum(n)), ")"))) %>%
    select(-c(n_chicken, n_goat, n_bull, n_horse, n_pig)) %>% 
    qflextable() %>%
    set_header_labels(region = "Region", n = "# HH", chicken_info = "Chicken/Poultry/Duck\n(n, %)",
                      goat_info = "Goat\n(n, %)", bull_info = "Bull\n(n, %)",
                      horse_info = "Horse\n(n, %)", pig_info = "Pig\n(n, %)") %>%
    theme_zebra() %>% theme_box() %>%
    align_nottext_col(align = "center", header = TRUE) %>%
    align_text_col(align = "center", header = TRUE)
  
  save_as_docx(region_table, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/tables/region_table.docx")
  
  

# Venn Diagrams -----------------------------------------------------------

## Venn Diagram 4 way -----------------------------------------------------
rural_2 <- rural_hh %>% 
  ungroup() %>% 
  select(bull_cow_cattle_present, horse_donkey_present,       
         chicken_poultry_duck_present, goat_sheep_present)

my_list <- list("Bull/Cow/Cattle" = which(rural_2$bull_cow_cattle_present == 1), 
                "Chicken/Poultry/Duck" = which(rural_2$chicken_poultry_duck_present == 1),
                "Goat/Sheep" = which(rural_2$goat_sheep_present == 1),
                "Horse/Donkey/Camel" = which(rural_2$horse_donkey_present == 1)) 

ggVennDiagram(my_list, label_alpha = 0, set_color = c("blue","darkred","darkgreen","black"), set_size = 6,
              label_percent_digit = 1) + 
  scale_fill_distiller(palette = "Reds", direction = 1) + 
  scale_x_continuous(expand = expansion(mult = .2))+ 
  guides(fill = guide_legend(title = "Household Count")) +
  theme(legend.position = "bottom")

# Table for the Venn diagram
rural_hh %>% filter(!animal_singleonly %in% c("none", "other")) %>% 
             group_by(chicken_poultry_duck_present, bull_cow_cattle_present,
                   goat_sheep_present, horse_donkey_present) %>%
             filter(chicken_poultry_duck_present == 1 | bull_cow_cattle_present == 1 |
                    goat_sheep_present == 1 | horse_donkey_present == 1) %>%
             mutate(chicken_poultry_duck_present = recode(chicken_poultry_duck_present, `1` = "Yes", `0` = "No"),
                    bull_cow_cattle_present = recode(bull_cow_cattle_present, `1` = "Yes", `0` = "No"),
                    goat_sheep_present = recode(goat_sheep_present, `1` = "Yes", `0` = "No"),
                    horse_donkey_present = recode(horse_donkey_present, `1` = "Yes", `0` = "No")) %>% 
             summarise(num = n()) %>% 
             arrange(desc(num)) %>%
             qflextable() %>% 
             set_header_labels(`chicken_poultry_duck_present` = "Chicken/Poultry/Duck", 
                               `goat_sheep_present` = "Goat/Sheep", `bull_cow_cattle_present` = "Bull/Cow/Cattle",
                               `horse_donkey_present` = "Horse/Donkey/Camel", num = "Number of Households") %>%
             add_header_lines(values = c("Household Ownership of Animals (4 Groups)")) %>% 
             theme_zebra() %>% theme_box() %>% 
             align_nottext_col(align = "center", header = TRUE) %>%
             align_text_col(align = "center", header = TRUE) %>% 
             bg(i = ~ chicken_poultry_duck_present == "Yes", j = ~ chicken_poultry_duck_present, bg = "#8ef5ad") %>%
             bg(i = ~ chicken_poultry_duck_present == "No", j = ~ chicken_poultry_duck_present, bg = "#f58e8e") %>%
             bg(i = ~ bull_cow_cattle_present == "Yes", j = ~ bull_cow_cattle_present, bg = "#8ef5ad") %>%
             bg(i = ~ bull_cow_cattle_present == "No", j = ~ bull_cow_cattle_present, bg = "#f58e8e") %>%
             bg(i = ~ goat_sheep_present == "Yes", j = ~ goat_sheep_present, bg = "#8ef5ad") %>%
             bg(i = ~ goat_sheep_present == "No", j = ~ goat_sheep_present, bg = "#f58e8e") %>%
             bg(i = ~ horse_donkey_present == "Yes", j = ~ horse_donkey_present, bg = "#8ef5ad") %>% 
             bg(i = ~ horse_donkey_present == "No", j = ~ horse_donkey_present, bg = "#f58e8e")

## Venn Diagram 5 way -----------------------------------------------------
rural_2 <- descriptive %>%
  # group_by(name_year) %>%
  # distinct(hhid, .keep_all = TRUE) %>% 
  # ungroup() %>% 
  filter(other_present == 0 ) %>% 
  select(bull_cow_cattle_present, horse_donkey_present,       
         chicken_poultry_duck_present, goat_sheep_present, pig_present)

my_list <- list("Bull/Cow/Cattle" = which(rural_2$bull_cow_cattle_present == 1), 
                "Poultry (Chicken/Duck)" = which(rural_2$chicken_poultry_duck_present == 1),
                "Goat/Sheep" = which(rural_2$goat_sheep_present == 1),
                "Horse/Donkey" = which(rural_2$horse_donkey_present == 1),
                "Pig" = which(rural_2$pig_present == 1))
                
ggVennDiagram(my_list, label_alpha = 0, set_color = c("blue","darkred","darkgreen","black", "darkorange"), set_size = 6,
              label_percent_digit = 1) + 
  scale_fill_distiller(palette = "Reds", direction = 1) + 
  scale_x_continuous(expand = expansion(mult = .2))+ 
  guides(fill = guide_legend(title = "Household Count")) +
  theme(legend.position = "bottom")

# Table for the Venn diagram
rural_hh %>% filter(!animal_singleonly %in% c("none", "other")) %>% 
             group_by(chicken_poultry_duck_present, bull_cow_cattle_present,
                      goat_sheep_present, horse_donkey_present, pig_present) %>%
             filter(chicken_poultry_duck_present == 1 | bull_cow_cattle_present == 1 |
                      goat_sheep_present == 1 | horse_donkey_present == 1 | pig_present == 1) %>%
             mutate(chicken_poultry_duck_present = recode(chicken_poultry_duck_present, `1` = "Yes", `0` = "No"),
                    bull_cow_cattle_present = recode(bull_cow_cattle_present, `1` = "Yes", `0` = "No"),
                    goat_sheep_present = recode(goat_sheep_present, `1` = "Yes", `0` = "No"),
                    horse_donkey_present = recode(horse_donkey_present, `1` = "Yes", `0` = "No"),
                    pig_present = recode(pig_present, `1` = "Yes", `0` = "No")) %>% 
             summarise(num = n()) %>% 
             arrange(desc(num)) %>%
             qflextable() %>% 
             set_header_labels(`chicken_poultry_duck_present` = "Chicken/Poultry/Duck", 
                               `goat_sheep_present` = "Goat/Sheep", `bull_cow_cattle_present` = "Bull/Cow/Cattle",
                               `horse_donkey_present` = "Horse/Donkey/Camel", pig_present = "Pig",
                               num = "Number of Households") %>%
             add_header_lines(values = c("Household Ownership of Animals (5 Groups)")) %>% 
             theme_zebra() %>% theme_box() %>% 
             align_nottext_col(align = "center", header = TRUE) %>%
             align_text_col(align = "center", header = TRUE) %>% 
             bg(i = ~ chicken_poultry_duck_present == "Yes", j = ~ chicken_poultry_duck_present, bg = "#8ef5ad") %>%
             bg(i = ~ chicken_poultry_duck_present == "No", j = ~ chicken_poultry_duck_present, bg = "#f58e8e") %>%
             bg(i = ~ bull_cow_cattle_present == "Yes", j = ~ bull_cow_cattle_present, bg = "#8ef5ad") %>%
             bg(i = ~ bull_cow_cattle_present == "No", j = ~ bull_cow_cattle_present, bg = "#f58e8e") %>%
             bg(i = ~ goat_sheep_present == "Yes", j = ~ goat_sheep_present, bg = "#8ef5ad") %>%
             bg(i = ~ goat_sheep_present == "No", j = ~ goat_sheep_present, bg = "#f58e8e") %>%
             bg(i = ~ horse_donkey_present == "Yes", j = ~ horse_donkey_present, bg = "#8ef5ad") %>% 
             bg(i = ~ horse_donkey_present == "No", j = ~ horse_donkey_present, bg = "#f58e8e") %>% 
             bg(i = ~ pig_present == "Yes", j = ~ pig_present, bg = "#8ef5ad") %>% 
             bg(i = ~ pig_present == "No", j = ~ pig_present, bg = "#f58e8e")

## Venn Diagram 6 way ------------------------------------------------------
rural_2 <- descriptive %>%
  group_by(name_year) %>%
  distinct(hhid, .keep_all = TRUE) %>% 
  ungroup() %>% 
  select(bull_cow_cattle_present, horse_donkey_present,       
         chicken_poultry_duck_present, goat_sheep_present, pig_present, animal_combo)

my_list <- list("Bull/Cow/Cattle" = which(rural_2$bull_cow_cattle_present == 1), 
                "Chicken/Poultry/Duck" = which(rural_2$chicken_poultry_duck_present == 1),
                "Goat/Sheep" = which(rural_2$goat_sheep_present == 1),
                "Horse/Donkey/Camel" = which(rural_2$horse_donkey_present == 1),
                "Pig" = which(rural_2$pig_present == 1),
                "Other" = which(rural_2$other == 1))


ggVennDiagram(my_list, label_alpha = 0, set_color = c("blue","darkred","darkgreen","black", "orange", "purple"), set_size = 6,
              label_percent_digit = 1) + 
  scale_fill_distiller(palette = "Reds", direction = 1) + 
  scale_x_continuous(expand = expansion(mult = .2))+ 
  guides(fill = guide_legend(title = "Household Count")) +
  theme(legend.position = "bottom")

# Table for the Venn diagram
rural_hh %>% filter(!animal_singleonly %in% c("none", "other")) %>% 
             group_by(chicken_poultry_duck_present, bull_cow_cattle_present,
                      goat_sheep_present, horse_donkey_present, pig_present, other_present) %>%
             filter(chicken_poultry_duck_present == 1 | bull_cow_cattle_present == 1 |
                      goat_sheep_present == 1 | horse_donkey_present == 1 | 
                      pig_present == 1 | other_present == 1) %>%
             mutate(chicken_poultry_duck_present = recode(chicken_poultry_duck_present, `1` = "Yes", `0` = "No"),
                    bull_cow_cattle_present = recode(bull_cow_cattle_present, `1` = "Yes", `0` = "No"),
                    goat_sheep_present = recode(goat_sheep_present, `1` = "Yes", `0` = "No"),
                    horse_donkey_present = recode(horse_donkey_present, `1` = "Yes", `0` = "No"),
                    pig_present = recode(pig_present, `1` = "Yes", `0` = "No"),
                    other_present = recode(other_present, `1` = "Yes", `0` = "No")) %>% 
             summarise(num = n()) %>% 
             arrange(desc(num)) %>%
             qflextable() %>% 
             set_header_labels(`chicken_poultry_duck_present` = "Chicken/Poultry/Duck", 
                               `goat_sheep_present` = "Goat/Sheep", `bull_cow_cattle_present` = "Bull/Cow/Cattle",
                               `horse_donkey_present` = "Horse/Donkey/Camel", `pig_present` = "Pig",
                               `other_present` = "Other", num = "Number of Households") %>%
             add_header_lines(values = c("Household Ownership of Animals (6 Groups)")) %>% 
             theme_zebra() %>% theme_box() %>% 
             align_nottext_col(align = "center", header = TRUE) %>%
             align_text_col(align = "center", header = TRUE) %>% 
             bg(i = ~ chicken_poultry_duck_present == "Yes", j = ~ chicken_poultry_duck_present, bg = "#8ef5ad") %>%
             bg(i = ~ chicken_poultry_duck_present == "No", j = ~ chicken_poultry_duck_present, bg = "#f58e8e") %>%
             bg(i = ~ bull_cow_cattle_present == "Yes", j = ~ bull_cow_cattle_present, bg = "#8ef5ad") %>%
             bg(i = ~ bull_cow_cattle_present == "No", j = ~ bull_cow_cattle_present, bg = "#f58e8e") %>%
             bg(i = ~ goat_sheep_present == "Yes", j = ~ goat_sheep_present, bg = "#8ef5ad") %>%
             bg(i = ~ goat_sheep_present == "No", j = ~ goat_sheep_present, bg = "#f58e8e") %>%
             bg(i = ~ horse_donkey_present == "Yes", j = ~ horse_donkey_present, bg = "#8ef5ad") %>% 
             bg(i = ~ horse_donkey_present == "No", j = ~ horse_donkey_present, bg = "#f58e8e") %>% 
             bg(i = ~ pig_present == "Yes", j = ~ pig_present, bg = "#8ef5ad") %>% 
             bg(i = ~ pig_present == "No", j = ~ pig_present, bg = "#f58e8e") %>% 
             bg(i = ~ other_present == "Yes", j = ~ other_present, bg = "#8ef5ad") %>% 
             bg(i = ~ other_present == "No", j = ~ other_present, bg = "#f58e8e")

# Summarise the diarrhea by any animals -----------------------------------

table1 <- descriptive %>% 
  group_by(name_year, hhid) %>% 
  mutate(first_animal_total = if_else(row_number()==1, animal_total, 0 )) %>%
  group_by(hv246) %>% 
  summarise(n = n(),
            animal_total = sum(first_animal_total),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4)*100) %>%
  mutate(hv246 = recode(hv246, `1` = "Yes", `0` = "No", .missing = "Unknown")) %>%
  mutate(hv246 = factor(hv246, levels = c("Yes", "No", "Unknown"))) %>%
  arrange(hv246) %>% 
  adorn_totals("row",,,,c(n, n_under5, animal_total, num_diarrhea)) %>%
  qflextable() %>% 
  set_header_labels(hv246 = "Any Animals?", n = "Number of People", animal_total = "Total Animals",
                    n_under5 = "Children Under 5", num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Diarrhea Prevalence by Animal Ownership")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

save_as_docx(table1, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/aim2_table1.docx")

# Summarise the diarrhea by any diarrhea

rural %>%  
  group_by(name_year, hhid) %>% 
  mutate(first_animal_total_hhunder5 = if_else(hh_under5 == 1 & row_number() == 1, animal_total, 0)) %>%
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            animal_total_under5 = sum(first_animal_total_hhunder5),
            avg_num_animal = round(mean(animal_total_under5/n_under5, na.rm = TRUE), 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No", "1" = "Yes")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, n_under5, animal_total_under5)) %>% 
  qflextable() %>% 
  set_header_labels(diarrhea_dichot = "Any Diarrhea", n = "Number of People", n_under5 = "Children Under 5",
                    animal_total_under5 = "Total Number of Animals*", avg_num_animal = "Avg Number of Animals Per Child under 5")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE) %>% 
  add_footer_lines("*Among households that have a child under the age of 5")

# Box plot for any animals
ggplot(data = rural, mapping = aes(
  x = diarrhea_dichot,
  y = log(animal_total),
  group = diarrhea_dichot)) + 
  geom_boxplot() +
  ylim(0, 5) +
  theme_classic() + 
  xlab("Diarrhea Status") +
  ylab("LOG(Total Number of Animals)") +
  scale_fill_manual(values = c("brown", "lightblue"),
                    name="Diarrhea Status") +  # Corrected the legend title
  ggtitle("Boxplot of Total Number of Animals by Diarrhea Status") +  # Corrected the plot title
  theme(plot.title = element_text(hjust = 0.5))


# Summarise the diarrhea by total animals
table2 <- descriptive %>% 
  mutate(animal_total_cut = if_else(is.na(animal_total_cut), "Unknown Animal", as.character(animal_total_cut))) %>%
  group_by(animal_total_cut) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            avg_num_animal = round(mean(animal_total, na.rm = TRUE), 2),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>%
  mutate(animal_total_cut = recode_factor(animal_total_cut, "(0,3]" = "1-3", "(3,7]" = "4-7",
                                          "(7,13]" = "8-13", "(13,25]" = "14-25",
                                          "(25,Inf]" = "26-475", .default = "Unknown Animal")) %>%
  arrange(animal_total_cut) %>% 
  adorn_totals("row",,,,c(n, n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(animal_total_cut = "Number of Animals in Household", n = "Number of People", n_under5 = "Children Under 5",
                    avg_num_animal = "Avg Number of Animals", num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

save_as_docx(table2, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/aim2_table2.docx")

# Summarise the diarrhea by total animals and SES
descriptive %>% 
  mutate(animal_total_cut = if_else(is.na(animal_total_cut), "Unknown Animal", as.character(animal_total_cut))) %>%
  group_by(animal_total_cut) %>% 
  summarise(n = n(),
            avg_num_animal = round(mean(animal_total, na.rm = TRUE), 2),
            ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
  mutate(animal_total_cut = recode_factor(animal_total_cut, "(0,3]" = "1-3", "(3,7]" = "4-7",
                                          "(7,13]" = "8-13", "(13,25]" = "14-25",
                                          "(25,Inf]" = "26-475", .default = "No Animals")) %>%
  arrange(animal_total_cut) %>% 
  adorn_totals("row",,,,c(n)) %>% 
  qflextable() %>% 
  set_header_labels(animal_total_cut = "Number of Animals in Household", n = "Number of People",
                    avg_num_animal = "Average Number of Animals", ses = "Average SES Level")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

descriptive %>% tabyl(animal_total_cut, hv270)


# Summarise the diarrhea by number of different animals exposed to
rural %>% 
  group_by(animal_combo_number) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            avg_num_animal = round(mean(animal_total, na.rm = TRUE), 2),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4)*100) %>%
  adorn_totals("row",,,,c(n, n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(animal_combo_number = "Number of Different Animals in Households", n = "Number of People", 
                    n_under5 = "Children under 5", avg_num_animal = "Average Number of Animals", 
                    num_diarrhea = "Children under 5 with Diarrhea", percent_diarrhea = "Diarrhea Prevalence")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

# Summarize by different animal categories (exclusive)
table3 <- descriptive %>% 
  group_by(animal_singleonly) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4)*100) %>%
  mutate(animal_singleonly = recode(animal_singleonly, `none` = "No Animals", `chicken/poultry/duck only` = "Chicken/Poultry/Duck Only", 
                                    `goat/sheep only` = "Goat/Sheep Only", `bull/cow/cattle only` = "Bull/Cow/Cattle Only",
                                    `horse/donkey only` = "Horse/Donkey Only", `multispecies` = ">1 Species", 
                                    `pig only` = "Pig Only", `other` = "Other")) %>%
  mutate(animal_singleonly = factor(animal_singleonly, levels = c("No Animals", "Chicken/Poultry/Duck Only", "Goat/Sheep Only",
                                                                  "Bull/Cow/Cattle Only", "Horse/Donkey Only",  ">1 Species",
                                                                  "Pig Only", "Other"))) %>%
  arrange(animal_singleonly) %>% 
  adorn_totals("row",,,,c(n, n_under5, num_diarrhea)) %>%
  qflextable() %>% 
  set_header_labels(animal_singleonly = "Single Animal Category", n = "Number of People", 
                    n_under5 = "Children Under 5", num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Diarrhea Prevalence by Single Animal Group")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

save_as_docx(table3, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/aim2_table3.docx")


rural %>% 
  mutate(ruminant_only = case_when(
    ruminant_present == 1 &
      chicken_poultry_duck_present == 0 &
        horse_donkey_present == 0 &
        pig_present == 0 &
        other_present == 0 &
        animal_combo == 0 ~ "ruminant", TRUE ~ NA_character_)) %>% 
  group_by(ruminant_only) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4)*100) %>%
  mutate(ruminant_only = recode(ruminant_only, `ruminant` = "Ruminant",  .missing = "Other")) %>%
  qflextable() %>% 
  set_header_labels(ruminant_only = "Single Animal Category", n = "Number of People", 
                    n_under5 = "Children Under 5", num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Diarrhea Prevalence by Single Animal Group")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

# Number of animals by diarrhea status -------------------------------------------------------

rural %>%  
  group_by(name_year, hhid) %>% 
  mutate(first_animal_total_hhunder5 = if_else(hh_under5 == 1 & row_number() == 1, animal_total, 0)) %>%
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            animal_total_under5 = sum(first_animal_total_hhunder5),
            avg_num_animal = round(mean(animal_total_under5/n_under5, na.rm = TRUE), 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No", "1" = "Yes")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, n_under5, animal_total_under5)) %>% 
  qflextable() %>% 
  set_header_labels(diarrhea_dichot = "Any Diarrhea", n = "Number of People", n_under5 = "Children Under 5",
                    animal_total_under5 = "Total Number of Animals*", avg_num_animal = "Avg Number of Animals Per Child under 5")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE) %>% 
  add_footer_lines("*Among households that have a child under the age of 5")

# SES ---------------------------------------------------------------------

rural %>% 
  group_by(animal_singleonly) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4)*100,
            ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
  mutate(animal_singleonly = recode(animal_singleonly, `none` = "No Animals", `chicken/poultry/duck only` = "Chicken/Poultry/Duck Only", 
                                    `goat/sheep only` = "Goat/Sheep Only", `bull/cow/cattle only` = "Bull/Cow/Cattle Only",
                                    `horse/donkey only` = "Horse/Donkey Only", `multispecies` = ">1 Species", 
                                    `other` = "Other")) %>%
  mutate(animal_singleonly = factor(animal_singleonly, levels = c("No Animals", "Chicken/Poultry/Duck Only", "Goat/Sheep Only",
                                                                  "Bull/Cow/Cattle Only", "Horse/Donkey Only",  ">1 Species",
                                                                  "Other"))) %>%
  arrange(animal_singleonly) %>% 
  adorn_totals("row",,,,c(n, n_under5, num_diarrhea)) %>%
  qflextable() %>% 
  set_header_labels(animal_singleonly = "Single Animal Category", n = "Number of People", 
                    n_under5 = "Children Under 5", num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence", ses = "SES Group")%>%
  add_header_lines(values = c("Diarrhea Prevalence by Single Animal Group")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)


table4 <- descriptive %>% 
  group_by(hv270) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4)*100) %>% 
  mutate(hv270 = recode_factor(hv270, "1" = "Poorest", "2" = "Poor", 
                                    "3" = "Middle", 
                               "4" = "Rich", "5" = "Richest", .missing = "Unknown")) %>% 
  adorn_totals("row",,,,c(n, n_under5, num_diarrhea)) %>%
  qflextable() %>% 
  set_header_labels(hv270 = "SES", n = "Number of People", 
                    n_under5 = "Children Under 5", num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Diarrhea Prevalence by SES Group")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

save_as_docx(table4, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/aim2_table4.docx")

## Animal and SES Trend ----------------------------------------------------

# Overall Ownership
overall <- descriptive %>% 
  group_by(animal_total) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
  filter(!num_diarrhea == 0) %>% 
  filter(animal_total <= 500)

# Perform Pearson correlation test
correlation_test <- cor.test(overall$animal_total, overall$ses, method = "pearson")

# Extract correlation coefficient and p-value
correlation_coefficient <- correlation_test$estimate
p_value <- correlation_test$p.value

# Create the plot with correlation annotation
ggplot(data = overall, aes(y = ses, x = animal_total)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "red", fill = "gray") +
  labs(x = "Number of Total Animals Owned",
       y = "SES Level") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  annotate("label", x = min(overall$animal_total) + 5, 
           y = max(overall$ses) - 0.3, 
           label = paste0("Pearson's r: ", round(correlation_coefficient, 2), 
                          "\nP-value: ", format(p_value, digits = 2)), 
           hjust = 0, vjust = 1, size = 6, fill = "gray", color = "black")

# Chickens
chicken <- descriptive %>% 
  group_by(hv246_chicken_poultry_duck_total_cat) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
  filter(!num_diarrhea == 0) %>% 
  filter(hv246_chicken_poultry_duck_total_cat <= 100)

# Perform Pearson correlation test
correlation_test <- cor.test(chicken$hv246_chicken_poultry_duck_total_cat, chicken$ses, method = "pearson")

# Extract correlation coefficient and p-value
correlation_coefficient <- correlation_test$estimate
p_value <- correlation_test$p.value

# Create the plot with correlation annotation
ggplot(data = chicken, aes(y = ses, x = hv246_chicken_poultry_duck_total_cat)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "red", fill = "gray") +
  labs(x = "Number of Chickens/Poultry/Duck",
       y = "SES Level") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  annotate("label", x = min(chicken$hv246_chicken_poultry_duck_total_cat) + 5, 
           y = max(chicken$ses) - 0.3, 
           label = paste0("Pearson's r: ", round(correlation_coefficient, 2), 
                          "\nP-value: ", format(p_value, digits = 2)), 
           hjust = 0, vjust = 1, size = 6, fill = "gray", color = "black")

# goat / sheep
goat <- descriptive %>% 
  group_by(hv246_goat_sheep_total_cat) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
  filter(!num_diarrhea == 0) %>% 
  filter(hv246_goat_sheep_total_cat <= 100)

# Perform Pearson correlation test
correlation_test <- cor.test(goat$hv246_goat_sheep_total_cat, goat$ses, method = "pearson")

# Extract correlation coefficient and p-value
correlation_coefficient <- correlation_test$estimate
p_value <- correlation_test$p.value

# Create the plot with correlation annotation
ggplot(data = goat, aes(y = ses, x = hv246_goat_sheep_total_cat)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "red", fill = "gray") +
  labs(x = "Number of Goat and Sheep",
       y = "SES Level") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  annotate("label", x = min(goat$hv246_goat_sheep_total_cat) + 5, 
           y = max(goat$ses) - 0.3, 
           label = paste0("Pearson's r: ", round(correlation_coefficient, 2), 
                          "\nP-value: ", format(p_value, digits = 2)), 
           hjust = 0, vjust = 1, size = 6, fill = "gray", color = "black")


# cattle/cow/bull
bull <- descriptive %>% 
  group_by(hv246_bull_cow_cattle_total_cat) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
  filter(!num_diarrhea == 0) %>% 
  filter(hv246_bull_cow_cattle_total_cat <= 100)

# Perform Pearson correlation test
correlation_test <- cor.test(bull$hv246_bull_cow_cattle_total_cat, bull$ses, method = "pearson")

# Extract correlation coefficient and p-value
correlation_coefficient <- correlation_test$estimate
p_value <- correlation_test$p.value

# Create the plot with correlation annotation
ggplot(data = bull, aes(y = ses, x = hv246_bull_cow_cattle_total_cat)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "red", fill = "gray") +
  labs(x = "Number of Bulls and Cows",
       y = "SES Level") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  annotate("label", x = min(bull$hv246_bull_cow_cattle_total_cat) + 5, 
           y = max(bull$ses) - 0.3, 
           label = paste0("Pearson's r: ", round(correlation_coefficient, 2), 
                          "\nP-value: ", format(p_value, digits = 2)), 
           hjust = 0, vjust = 1, size = 6, fill = "gray", color = "black")


# horse/donkey
horse <- descriptive %>% 
  group_by(hv246_horse_donkey_total_cat) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
  filter(!num_diarrhea == 0) %>% 
  filter(hv246_horse_donkey_total_cat <= 25)

# Perform Pearson correlation test
correlation_test <- cor.test(horse$hv246_horse_donkey_total_cat, horse$ses, method = "pearson")

# Extract correlation coefficient and p-value
correlation_coefficient <- correlation_test$estimate
p_value <- correlation_test$p.value

# Create the plot with correlation annotation
ggplot(data = horse, aes(y = ses, x = hv246_horse_donkey_total_cat)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "red", fill = "gray") +
  labs(x = "Number of Horses and Donkeys",
       y = "SES Level") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  annotate("label", x = min(horse$hv246_horse_donkey_total_cat) + 5, 
           y = max(horse$ses) - 0.3, 
           label = paste0("Pearson's r: ", round(correlation_coefficient, 2), 
                          "\nP-value: ", format(p_value, digits = 2)), 
           hjust = 0, vjust = 1, size = 6, fill = "gray", color = "black")

# pig
pig <- descriptive %>% 
  group_by(hv246_pig_total_cat) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
  filter(!num_diarrhea == 0) %>% 
  filter(hv246_pig_total_cat <= 30)

# Perform Pearson correlation test
correlation_test <- cor.test(pig$hv246_pig_total_cat, pig$ses, method = "pearson")

# Extract correlation coefficient and p-value
correlation_coefficient <- correlation_test$estimate
p_value <- correlation_test$p.value

# Create the plot with correlation annotation
ggplot(data = pig, aes(y = ses, x = hv246_pig_total_cat)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "red", fill = "gray") +
  labs(x = "Number of Pigs",
       y = "SES Level") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  annotate("label", x = min(pig$hv246_pig_total_cat) + 3, 
           y = max(pig$ses) - 0.25, 
           label = paste0("Pearson's r: ", round(correlation_coefficient, 2), 
                          "\nP-value: ", format(p_value, digits = 2)), 
           hjust = 0, vjust = 1, size = 6, fill = "gray", color = "black")


# other
other <- descriptive %>% 
  group_by(hv246_other_total_cat) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
  filter(!num_diarrhea == 0) %>% 
  filter(hv246_other_total_cat <= 50)

# Perform Pearson correlation test
correlation_test <- cor.test(other$hv246_other_total_cat, other$ses, method = "pearson")

# Extract correlation coefficient and p-value
correlation_coefficient <- correlation_test$estimate
p_value <- correlation_test$p.value

# Create the plot with correlation annotation
ggplot(data = other, aes(y = ses, x = hv246_other_total_cat)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "red", fill = "gray") +
  labs(x = "Number of Pigs",
       y = "SES Level") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  annotate("label", x = min(other$hv246_other_total_cat) + 3, 
           y = max(other$ses) - 0.25, 
           label = paste0("Pearson's r: ", round(correlation_coefficient, 2), 
                          "\nP-value: ", format(p_value, digits = 2)), 
           hjust = 0, vjust = 1, size = 6, fill = "gray", color = "black")


# Animal Diarrhea Trend ---------------------------------------------------

# Overall Ownership
overall <- descriptive %>% 
  group_by(animal_total) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
  filter(!num_diarrhea == 0) %>% 
  # Censor all those where num_diarrhea was less than 5
  filter(animal_total <= 100)

# Perform Pearson correlation test
correlation_test <- cor.test(overall$animal_total, overall$percent_diarrhea, method = "pearson")

# Extract correlation coefficient and p-value
correlation_coefficient <- correlation_test$estimate
p_value <- correlation_test$p.value

# Create the plot with correlation annotation
ggplot(data = overall, aes(y = percent_diarrhea, x = animal_total)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "red", fill = "gray") +
  labs(x = "Number of Total Animals Owned",
       y = "Percent Diarrhea") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  annotate("label", x = min(overall$animal_total) + 5, 
           y = max(overall$percent_diarrhea) - 0.3, 
           label = paste0("Pearson's r: ", round(correlation_coefficient, 2), 
                          "\nP-value: ", format(p_value, digits = 2)), 
           hjust = 0, vjust = 1, size = 6, fill = "gray", color = "black")









table(Hmisc::cut2(rural$hv246_chicken_poultry_duck_total_cat, g=10))

rural <- rural %>% 
  mutate(chicken_cut = cut(hv246_chicken_poultry_duck_total_cat, breaks = c(0,1,2,5,9,16,Inf)))

rural %>% tabyl(chicken_cut)

# Chicken groups
rural %>% 
  mutate(chicken_cut = if_else(is.na(chicken_cut), "No Chickens Owned", as.character(chicken_cut))) %>%
  group_by(chicken_cut) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            avg_num_animal = round(mean(hv246_chicken_poultry_duck_total_cat, na.rm = TRUE), 2),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
  mutate(chicken_cut = recode_factor(chicken_cut, "(0,1]" = "1", "(1,2]" = "2", "(2,5]" = "3-5", "(5,9]" = "6-9",
                                          "(9,16]" = "10-16", "(16,Inf]" = "17-285", .default = "No Chickens Owned")) %>%
  arrange(chicken_cut) %>% 
  adorn_totals("row",,,,c(n, n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(chicken_cut = "Number of Chickens in Household", n = "Number of People", n_under5 = "Children Under 5",
                    avg_num_animal = "Avg Number of Chickens", num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence", ses = "SES Group")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)


rural <- rural %>% 
  mutate(chicken_cut_2 = cut(hv246_chicken_poultry_duck_total_cat, breaks = c(0,25,75,Inf)))

rural %>% tabyl(chicken_cut_2)

# Chicken groups
rural %>% 
  mutate(chicken_cut_2 = if_else(is.na(chicken_cut_2), "No Chickens Owned", as.character(chicken_cut_2))) %>%
  group_by(chicken_cut_2) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            avg_num_animal = round(mean(hv246_chicken_poultry_duck_total_cat, na.rm = TRUE), 2),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
  mutate(chicken_cut_2 = recode_factor(chicken_cut_2, "(0,25]" = "1-25", "(25,75]" = "26-75", "(75,Inf]" = "75-285", .default = "No Chickens Owned")) %>%
  arrange(chicken_cut_2) %>% 
  adorn_totals("row",,,,c(n, n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(chicken_cut_2 = "Number of Chickens in Household", n = "Number of People", n_under5 = "Children Under 5",
                    avg_num_animal = "Avg Number of Chickens", num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence", ses = "SES Group")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)



# Chickens / Poultry
chicken <- rural %>% 
  group_by(hv246_chicken_poultry_duck_total_cat) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
  filter(!num_diarrhea == 0) %>% 
  filter(hv246_chicken_poultry_duck_total_cat <= 100)
  
  ggplot(data = chicken, aes(y=percent_diarrhea, x=hv246_chicken_poultry_duck_total_cat)) + 
  geom_point() +
  geom_smooth(color = "darkred") +
  labs(x = "Number of Chickens/Poultry/Duck",
       y = "Percent Diarrhea") +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    annotate("text", x = Inf, y = Inf, label = paste("Number of observations:", nrow(chicken)), 
             hjust = 1.5, vjust = 1.5, size = 8, color = "black")
  
  
  
  # goat / sheep
  goat <- rural %>% 
    group_by(hv246_goat_sheep_total_cat) %>% 
    summarise(n = n(),
              n_under5 = sum(b8 <= 4, na.rm = TRUE),
              num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
              percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
              ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
    filter(!num_diarrhea == 0) %>% 
    filter(hv246_goat_sheep_total_cat <= 100)
  
  ggplot(data = goat, aes(y=percent_diarrhea, x=hv246_goat_sheep_total_cat)) + 
    geom_point() +
    geom_smooth(color = "darkred") +
    labs(x = "Number of Goat/Sheep",
         y = "Percent Diarrhea") +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  # cattle/cow/bull
  goat <- rural %>% 
    group_by(hv246_bull_cow_cattle_total_cat) %>% 
    summarise(n = n(),
              n_under5 = sum(b8 <= 4, na.rm = TRUE),
              num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
              percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
              ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
    filter(!num_diarrhea == 0) %>% 
    filter(hv246_bull_cow_cattle_total_cat <= 100)
  
  ggplot(data = goat, aes(y=percent_diarrhea, x=hv246_bull_cow_cattle_total_cat)) + 
    geom_point() +
    geom_smooth(color = "darkred") +
    labs(x = "Number of Bull/Cow/Cattle",
         y = "Percent Diarrhea") +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  # horse/donkey
  horse <- rural %>% 
    group_by(hv246_horse_donkey_total_cat) %>% 
    summarise(n = n(),
              n_under5 = sum(b8 <= 4, na.rm = TRUE),
              num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
              percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
              ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
    filter(!num_diarrhea == 0) %>% 
    filter(hv246_horse_donkey_total_cat <= 50)
  
  ggplot(data = horse, aes(y=percent_diarrhea, x=hv246_horse_donkey_total_cat)) + 
    geom_point() +
    geom_smooth(color = "darkred") +
    labs(x = "Number of Horse/Donkey",
         y = "Percent Diarrhea") +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()

  # pig
  pig <- rural %>% 
    group_by(hv246_pig_total_cat) %>% 
    summarise(n = n(),
              n_under5 = sum(b8 <= 4, na.rm = TRUE),
              num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
              percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
              ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
    filter(!num_diarrhea == 0) %>% 
    filter(hv246_pig_total_cat <= 30)
  
  ggplot(data = pig, aes(y=percent_diarrhea, x=hv246_pig_total_cat)) + 
    geom_point() +
    geom_smooth(color = "darkred") +
    labs(x = "Number of Pig",
         y = "Percent Diarrhea") +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  # other
  other <- rural %>% 
    group_by(hv246_other_total_cat) %>% 
    summarise(n = n(),
              n_under5 = sum(b8 <= 4, na.rm = TRUE),
              num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
              percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
              ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
    filter(!num_diarrhea == 0) %>% 
    filter(hv246_other_total_cat <= 50)
  
  ggplot(data = other, aes(y=percent_diarrhea, x=hv246_other_total_cat)) + 
    geom_point() +
    geom_smooth(color = "darkred") +
    labs(x = "Number of Other Animals",
         y = "Percent Diarrhea") +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()

# Total Precipitation Average ---------------------------------------------

  descriptive <- descriptive %>% 
    mutate(tp_totalminus7_cm = tp_totalminus7 * 100)
  
  
  
descriptive %>% 
    group_by(kgc_course) %>% 
    summarize(
      n_hh = n(),
      avg_rainfall = round(mean(tp_totalminus7_cm, na.rm = TRUE), 2)) %>%
  mutate(kgc_course = recode(kgc_course, .missing = "Unknown")) %>% 
  adorn_totals("row",,,,c(n_hh)) %>% 
  qflextable() %>% 
  set_header_labels(n_hh = "Number of Households",  avg_rainfall = "Average 7-Day Rainfall (cm)",
                    kgc_course = "Koppen-Geiger Zone")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)
  
for_graph <- descriptive %>% 
  group_by(kgc_course, hv007) %>% 
  summarize(
    n_hh = n(),
    avg_rainfall = round(mean(tp_totalminus7_cm, na.rm = TRUE), 2)) %>%
  drop_na(kgc_course)

# Convert hv007 to factor for correct x-axis ordering
for_graph$hv007 <- as.factor(for_graph$hv007)

# Create the line plot
p <- ggplot(for_graph, aes(x = hv007, y = n_hh, color = kgc_course, group=kgc_course)) + #group is important here
  geom_line(size = 1.2) +
  geom_point(size = 3)+
  labs(title = "Number of Households (n_hh) by Course and hv007",
       x = "hv007",
       y = "Number of Households (n_hh)",
       color = "Course") +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

print(p)

# Plot for avg_rainfall
p2 <- ggplot(for_graph, aes(x = hv007, y = avg_rainfall, color = kgc_course, group=kgc_course)) +
  geom_line(size = 1.2) +
  geom_point(size = 3)+
  labs(title = "Average Rainfall by Koppen Geiger Zone and Year",
       x = "Year",
       y = "Average Rainfall",
       color = "Koppen-Geiger Zone") +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

print(p2)


library(patchwork)

print(p + p2)


# Unadjusted Models ------------------------------------------------------------------

model_chicken <- glmer(diarrhea_dichot ~ chicken_poultry_duck_present + tp_totalminus7 + kgc_course + epe_7_95 +
                     (1|name_year/hv001), data = under5_animal, family = binomial)

table_chicken <- model_chicken %>% tbl_regression(label = list(chicken_poultry_duck_present ~ "Chicken/Duck/Poultry"),exponentiate = TRUE) %>% bold_labels()

model_bull <- glmer(diarrhea_dichot ~ bull_cow_cattle_present +
                         (1|name_year/hv001), data = under5_animal, family = binomial)

table_bull <- model_bull %>% tbl_regression(label = list(bull_cow_cattle_present ~ "Bull/Cow/Cattle"),exponentiate = TRUE) %>% bold_labels()

model_goat <- glmer(diarrhea_dichot ~ goat_sheep_present +
                         (1|name_year/hv001), data = under5_animal, family = binomial)

table_goat <- model_goat %>% tbl_regression(label = list(goat_sheep_present ~ "Goat/Sheep"),exponentiate = TRUE) %>% bold_labels()

model_horse <- glmer(diarrhea_dichot ~ horse_donkey_present +
                         (1|name_year/hv001), data = under5_animal, family = binomial)

table_horse <- model_horse %>% tbl_regression(label = list(horse_donkey_present ~ "Horse/Donkey"),exponentiate = TRUE) %>% bold_labels()

model_pig <- glmer(diarrhea_dichot ~ pig_present +
                         (1|name_year/hv001), data = under5_animal, family = binomial)

table_pig <- model_pig %>% tbl_regression(label = list(pig_present ~ "Pig"),exponentiate = TRUE) %>% bold_labels()

model_other <- glmer(diarrhea_dichot ~ other_present +
                         (1|name_year/hv001), data = under5_animal, family = binomial)

table_other <- model_other %>% tbl_regression(label = list(other_present ~ "Other Animal"),exponentiate = TRUE) %>% bold_labels()

combined_models <- tbl_stack(tbls = list(table_chicken, table_bull, table_goat, table_horse, table_pig, table_other), group_header = "Test")

combined_models <- combined_models %>%
  modify_header(label = "**Animal Type**")

combined_models


# Unadjusted Models (Animals Only) ------------------------------------------------------------------

chicken_only <- under5_animal %>% select(bull_cow_cattle_present, horse_donkey_present,       
                       chicken_poultry_duck_present, goat_sheep_present, diarrhea_dichot, name_year, hv001) %>% 
                       filter(bull_cow_cattle_present == 0 & horse_donkey_present == 0 & goat_sheep_present == 0)

model_chicken <- glmer(diarrhea_dichot ~ chicken_poultry_duck_present +
                         (1|name_year/hv001), data = chicken_only, family = binomial)

table_chicken <- model_chicken %>% tbl_regression(label = list(chicken_poultry_duck_present ~ "Chicken/Duck/Poultry"),exponentiate = TRUE) %>% bold_labels() %>% add_n()



bull_only <- under5_animal %>% select(bull_cow_cattle_present, horse_donkey_present,       
                                         chicken_poultry_duck_present, goat_sheep_present, diarrhea_dichot, name_year, hv001) %>% 
                               filter(chicken_poultry_duck_present == 0 & horse_donkey_present == 0 & goat_sheep_present == 0)

model_bull <- glmer(diarrhea_dichot ~ bull_cow_cattle_present +
                      (1|name_year/hv001), data = bull_only, family = binomial)

table_bull <- model_bull %>% tbl_regression(label = list(bull_cow_cattle_present ~ "Bull/Cow/Cattle"),exponentiate = TRUE) %>% bold_labels() %>% add_n()




goat_only <- under5_animal %>% select(bull_cow_cattle_present, horse_donkey_present,       
                                      chicken_poultry_duck_present, goat_sheep_present, diarrhea_dichot, name_year, hv001) %>% 
  filter(chicken_poultry_duck_present == 0 & horse_donkey_present == 0 & bull_cow_cattle_present == 0)

model_goat <- glmer(diarrhea_dichot ~ goat_sheep_present +
                      (1|name_year/hv001), data = goat_only, family = binomial)

table_goat <- model_goat %>% tbl_regression(label = list(goat_sheep_present ~ "Goat/Sheep"),exponentiate = TRUE) %>% bold_labels() %>% add_n()



horse_only <- under5_animal %>% select(bull_cow_cattle_present, horse_donkey_present,       
                                      chicken_poultry_duck_present, goat_sheep_present, diarrhea_dichot, name_year, hv001) %>% 
                                filter(chicken_poultry_duck_present == 0 & goat_sheep_present == 0 & bull_cow_cattle_present == 0)


model_horse <- glmer(diarrhea_dichot ~ horse_donkey_present +
                       (1|name_year/hv001), data = horse_only, family = binomial)

table_horse <- model_horse %>% tbl_regression(label = list(horse_donkey_present ~ "Horse/Donkey"),exponentiate = TRUE) %>% bold_labels() %>% add_n()

model_pig <- glmer(diarrhea_dichot ~ pig_present +
                     (1|name_year/hv001), data = under5_animal, family = binomial)

table_pig <- model_pig %>% tbl_regression(label = list(pig_present ~ "Pig"),exponentiate = TRUE) %>% bold_labels()

model_other <- glmer(diarrhea_dichot ~ other_present +
                       (1|name_year/hv001), data = under5_animal, family = binomial)

table_other <- model_other %>% tbl_regression(label = list(other_present ~ "Other Animal"),exponentiate = TRUE) %>% bold_labels()

combined_models <- tbl_stack(tbls = list(table_chicken, table_bull, table_goat, table_horse))

combined_models <- combined_models %>%
  modify_header(label = "**Animal Type**")

combined_models



# Adjusted Models ---------------------------------------------------------

under5_animal_model <- under5_animal %>% select(diarrhea_dichot, chicken_poultry_duck_present, pig_present,  goat_sheep_present, 
                                                bull_cow_cattle_present, kgc_course, horse_donkey_present,
                                                hv270, epe_2228_95, b8, name_year, hv001)

model_chicken <- glmer(diarrhea_dichot ~ chicken_poultry_duck_present + kgc_course + hv270 + epe_2228_95 + b8 +
                       (1|name_year/hv001), data = under5_animal_model, family = binomial,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

table_chicken <- model_chicken %>% tbl_regression(label = list(chicken_poultry_duck_present ~ "Chicken/Duck/Poultry", kgc_course ~ "Koppen-Geiger Zone",
                                                               epe_2228_95 ~ "EPE, 22-28 Days", hv270 ~ "SES Group", b8 ~ "Age of Child (Years)"),
                                                               exponentiate = TRUE) %>% bold_labels()

saveRDS(model_chicken, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/model_chicken.rds")

model_bull <- glmer(diarrhea_dichot ~ bull_cow_cattle_present + kgc_course + hv270 + epe_2228_95 + b8 +
                       (1|name_year/hv001), data = under5_animal_model, family = binomial,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

table_bull <- model_bull %>% tbl_regression(label = list(bull_cow_cattle_present ~ "Bull/Cow/Cattle", kgc_course ~ "Koppen-Geiger Zone",
                                                         epe_2228_95 ~ "EPE, 22-28 Days", hv270 ~ "SES Group", b8 ~ "Age of Child (Years)"),
                                            exponentiate = TRUE) %>% bold_labels()

saveRDS(model_bull, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/model_bull.rds")

model_goat <- glmer(diarrhea_dichot ~ goat_sheep_present + kgc_course + hv270 + epe_2228_95 + b8 +
                       (1|name_year/hv001), data = under5_animal_model, family = binomial,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

table_goat <- model_goat %>% tbl_regression(label = list(goat_sheep_present ~ "Goat/Sheep", kgc_course ~ "Koppen-Geiger Zone",
                                                         epe_2228_95 ~ "EPE, 22-28 Days", hv270 ~ "SES Group", b8 ~ "Age of Child (Years)"),
                                            exponentiate = TRUE) %>% bold_labels()

saveRDS(model_goat, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/model_goat.rds")


model_horse <- glmer(diarrhea_dichot ~ horse_donkey_present + kgc_course + hv270 + epe_2228_95 + b8 +
                        (1|name_year/hv001), data = under5_animal_model, family = binomial,
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

#model_horse <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/model_horse.rds")

table_horse <- model_horse %>% tbl_regression(label = list(horse_donkey_present ~ "Horse/Donkey", kgc_course ~ "Koppen-Geiger Zone",
                                                           epe_2228_95 ~ "EPE, 22-28 Days", hv270 ~ "SES Group", b8 ~ "Age of Child (Years)"),
                                              exponentiate = TRUE) %>% bold_labels()

saveRDS(model_horse, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/model_horse.rds")


model_pig <- glmer(diarrhea_dichot ~ pig_present + kgc_course + hv270 + epe_2228_95 +  b8 +
                     (1|name_year/hv001), data = under5_animal_model, family = binomial,
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

table_pig <- model_pig %>% tbl_regression(label = list(pig_present ~ "Pig", kgc_course ~ "Koppen-Geiger Zone",
                                                       epe_2228_95 ~ "EPE, 22-28 Days", hv270 ~ "SES Group", b8 ~ "Age of Child (Years)"),
                                          exponentiate = TRUE) %>% bold_labels()

saveRDS(model_pig, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/model_pig.rds")



combined_models <- tbl_merge(tbls = list(table_chicken, table_bull, table_goat, table_horse, table_pig),
                             tab_spanner = c("**Chicken/Duck/Poultry**", "**Bull/Cow/Cattle**",
                                             "**Goat/Sheep**", "**Horse/Donkey**",
                                             "**Pig**")) %>%
  
  # combined_models <- combined_models %>%
  modify_table_body(~.x %>%
                      dplyr::arrange(factor(var_label, levels =
                                              c("Chicken/Duck/Poultry", "Bull/Cow/Cattle",
                                                "Goat/Sheep", "Horse/Donkey",
                                                "Pig", "Koppen-Geiger Zone", "SES Group",
                                                "EPE, 22-28 Days", "Age of Child (Years)"))))


combined_models <- combined_models %>% as_gt()


combined_models <- combined_models %>% 
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "gray80",
      weight = px(2),
      style = "solid"),
    locations = list(cells_body(),
                     cells_column_labels(),
                     cells_column_spanners()))

combined_models

save_as_docx(combined_models, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/combined_models.docx")



# Adjusted, No EPE --------------------------------------------------

noepe <- descriptive %>%  
  filter(hv246 == 1)                     # Only those with Animals

noepe <- noepe %>% drop_na(hv270)

under5_animal_model <- noepe %>% select(diarrhea_dichot, chicken_poultry_duck_present, pig_present,  goat_sheep_present, 
                                                bull_cow_cattle_present, region_combined, horse_donkey_present,
                                                hv270, b8, name_year, hv001)

model_chicken <- glmer(diarrhea_dichot ~ chicken_poultry_duck_present + region_combined + hv270 + b8 +
                         (1|name_year/hv001), data = under5_animal_model, family = binomial,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))


table_chicken <- model_chicken %>% tbl_regression(label = list(chicken_poultry_duck_present ~ "Chicken/Duck/Poultry", region_combined ~ "Region",
                                                               hv270 ~ "SES Group", b8 ~ "Age of Child (Years)"),
                                                  exponentiate = TRUE) %>% bold_labels()

saveRDS(model_chicken, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/models/model_chicken_noepe.rds")

model_bull <- glmer(diarrhea_dichot ~ bull_cow_cattle_present + region_combined + hv270 + b8 +
                      (1|name_year/hv001), data = under5_animal_model, family = binomial,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))


table_bull <- model_bull %>% tbl_regression(label = list(bull_cow_cattle_present ~ "Bull/Cow/Cattle", region_combined ~ "Region",
                                                         hv270 ~ "SES Group", b8 ~ "Age of Child (Years)"),
                                            exponentiate = TRUE) %>% bold_labels()

saveRDS(model_bull, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/models/model_bull_noepe.rds")

model_goat <- glmer(diarrhea_dichot ~ goat_sheep_present + region_combined + hv270 + b8 +
                      (1|name_year/hv001), data = under5_animal_model, family = binomial,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

table_goat <- model_goat %>% tbl_regression(label = list(goat_sheep_present ~ "Goat/Sheep", region_combined ~ "Region",
                                                         hv270 ~ "SES Group", b8 ~ "Age of Child (Years)"),
                                            exponentiate = TRUE) %>% bold_labels()

saveRDS(model_goat, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/models/model_goat_noepe.rds")


model_horse <- glmer(diarrhea_dichot ~ horse_donkey_present + region_combined + hv270 + b8 +
                       (1|name_year/hv001), data = under5_animal_model, family = binomial,
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))


table_horse <- model_horse %>% tbl_regression(label = list(horse_donkey_present ~ "Horse/Donkey", region_combined ~ "Region",
                                                           hv270 ~ "SES Group", b8 ~ "Age of Child (Years)"),
                                              exponentiate = TRUE) %>% bold_labels()

saveRDS(model_horse, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/models/model_horse_noepe.rds")


model_pig <- glmer(diarrhea_dichot ~ pig_present + region_combined + hv270 + b8 +
                     (1|name_year/hv001), data = under5_animal_model, family = binomial,
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

table_pig <- model_pig %>% tbl_regression(label = list(pig_present ~ "Pig", region_combined ~ "Region",
                                                       hv270 ~ "SES Group", b8 ~ "Age of Child (Years)"),
                                          exponentiate = TRUE) %>% bold_labels()

saveRDS(model_pig, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/models/model_pig_noepe.rds")


combined_models <- tbl_merge(tbls = list(table_chicken, table_bull, table_goat, table_horse, table_pig),
                             tab_spanner = c("**Chicken/Duck/Poultry**", "**Bull/Cow/Cattle**",
                                             "**Goat/Sheep**", "**Horse/Donkey**",
                                             "**Pig**")) %>%
  
  # combined_models <- combined_models %>%
  modify_table_body(~.x %>%
                      dplyr::arrange(factor(var_label, levels =
                                              c("Chicken/Duck/Poultry", "Bull/Cow/Cattle",
                                                "Goat/Sheep", "Horse/Donkey",
                                                "Pig", "Region", "SES Group",
                                                "Age of Child (Years)"))))


combined_models <- combined_models %>% as_gt()


combined_models <- combined_models %>% 
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "gray80",
      weight = px(2),
      style = "solid"),
    locations = list(cells_body(),
                     cells_column_labels(),
                     cells_column_spanners()))

combined_models

save_as_docx(combined_models, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/models/epe_models.docx")



# Model Tables ------------------------------------------------------------

model_bull <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/models/model_bull_noepe.rds")
model_chicken <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/models/model_chicken_noepe.rds")
model_goat <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/models/model_goat_noepe.rds")
model_horse <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/models/model_horse_noepe.rds")
model_pig <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/models/model_pig_noepe.rds")

table_chicken <- model_chicken %>% tbl_regression(label = list(chicken_poultry_duck_present ~ "Chicken/Duck/Poultry", 
                                                               region_combined ~ "Region",
                                                               hv270 ~ "SES Group", 
                                                               b8 ~ "Age of Child (Years)"),
                                                  exponentiate = TRUE) %>% bold_labels()

table_bull <- model_bull %>% tbl_regression(label = list(bull_cow_cattle_present ~ "Bull/Cow/Cattle", 
                                                         region_combined ~ "Region",
                                                         hv270 ~ "SES Group", 
                                                         b8 ~ "Age of Child (Years)"),
                                            exponentiate = TRUE) %>% bold_labels()

table_goat <- model_goat %>% tbl_regression(label = list(goat_sheep_present ~ "Goat/Sheep", 
                                                         region_combined ~ "Region",
                                                         hv270 ~ "SES Group", 
                                                         b8 ~ "Age of Child (Years)"),
                                            exponentiate = TRUE) %>% bold_labels()

table_horse <- model_horse %>% tbl_regression(label = list(horse_donkey_present ~ "Horse/Donkey", 
                                                           region_combined ~ "Region",
                                                           hv270 ~ "SES Group", 
                                                           b8 ~ "Age of Child (Years)"),
                                              exponentiate = TRUE) %>% bold_labels()

table_pig <- model_pig %>% tbl_regression(label = list(pig_present ~ "Pig", 
                                                       region_combined ~ "Region",
                                                       hv270 ~ "SES Group", 
                                                       b8 ~ "Age of Child (Years)"),
                                          exponentiate = TRUE) %>% bold_labels()

combined_models <- tbl_merge(tbls = list(table_chicken, table_bull, table_goat, table_horse, table_pig),
                             tab_spanner = c("**Chicken/Duck/Poultry**", "**Bull/Cow/Cattle**",
                                             "**Goat/Sheep**", "**Horse/Donkey**",
                                             "**Pig**")) %>%
  
  modify_table_body(~.x %>%
                      dplyr::arrange(factor(var_label, levels =
                                              c("Chicken/Duck/Poultry", "Bull/Cow/Cattle",
                                                "Goat/Sheep", "Horse/Donkey",
                                                "Pig", "Region", "SES Group",
                                                "Age of Child (Years)"))))
combined_models <- combined_models %>% as_gt()

combined_models <- combined_models %>% 
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "gray80",
      weight = px(2),
      style = "solid"),
    locations = list(cells_body(),
                     cells_column_labels(),
                     cells_column_spanners()))

combined_models