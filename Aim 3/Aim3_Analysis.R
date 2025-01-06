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


# Animals by household ----------------------------------------------------

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
rural_2 <- rural %>%
  group_by(name_year) %>%
  distinct(hhid, .keep_all = TRUE) %>% 
  ungroup() %>% 
  select(bull_cow_cattle_present, horse_donkey_present,       
         chicken_poultry_duck_present, goat_sheep_present, pig_present)

my_list <- list("Bull/Cow/Cattle" = which(rural_2$bull_cow_cattle_present == 1), 
                "Chicken/Poultry/Duck" = which(rural_2$chicken_poultry_duck_present == 1),
                "Goat/Sheep" = which(rural_2$goat_sheep_present == 1),
                "Horse/Donkey/Camel" = which(rural_2$horse_donkey_present == 1),
                "pig" = which(rural_2$pig_present == 1))
                
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
rural_2 <- rural %>%
  group_by(name_year) %>%
  distinct(hhid, .keep_all = TRUE) %>% 
  ungroup() %>% 
  select(bull_cow_cattle_present, horse_donkey_present,       
         chicken_poultry_duck_present, goat_sheep_present, pig_present, other_present)

my_list <- list("Bull/Cow/Cattle" = which(rural_2$bull_cow_cattle_present == 1), 
                "Chicken/Poultry/Duck" = which(rural_2$chicken_poultry_duck_present == 1),
                "Goat/Sheep" = which(rural_2$goat_sheep_present == 1),
                "Horse/Donkey/Camel" = which(rural_2$horse_donkey_present == 1),
                "pig" = which(rural_2$pig_present == 1),
                "Other" = which(rural_2$other_present == 1))


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


## Animal by Region --------------------------------------------------------


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


round(mean(descriptive$animal_total, na.rm = TRUE), 2)

descriptive$animal_total

save_as_docx(table2, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/aim2_table2.docx")



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

# KGC ---------------------------------------------------------------

# Summarise the diarrhea by kgz
rural %>% 
  group_by(kgc_course) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 4, na.rm = TRUE),
            avg_num_animal = round(mean(animal_total, na.rm = TRUE), 2),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4)*100) %>%
  adorn_totals("row",,,,c(n, n_under5, num_diarrhea)) %>% 
  mutate(kgc_course = recode(kgc_course, .missing = "Unknown KGZ")) %>% 
  qflextable() %>% 
  set_header_labels(kgc_course = "Köppen-Geiger Zone (KGZ)", n = "Number of People", n_under5 = "Children Under 5",
                    avg_num_animal = "Average Number of Animals", num_diarrhea = "Children under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

descriptive <- descriptive %>% 
  mutate(tp_totalminus7_cm = tp_totalminus7 * 100)



kgc_table <- descriptive %>%
  mutate(hv270 = case_when(
    hv270 == "Poorest" ~ 1,
    hv270 == "Poor" ~ 2,
    hv270 == "Middle" ~ 3,
    hv270 == "Rich" ~ 4,
    hv270 == "Richest" ~ 5,
    TRUE ~ NA_real_)) %>% 
  group_by(kgc_course) %>%
  summarise(
    n = n(),
    avg_rainfall = round(mean(tp_totalminus7_cm, na.rm = TRUE), 2),
    ses = round(mean(hv270, na.rm = TRUE), 2),
    n_chicken = sum(hv246_chicken_poultry_duck_total_cat >= 1, na.rm = TRUE),
    n_goat = sum(hv246_goat_sheep_total_cat >= 1, na.rm = T),
    n_bull = sum(hv246_bull_cow_cattle_total_cat >= 1, na.rm = T),
    n_horse = sum(hv246_horse_donkey_total_cat >= 1, na.rm = T),
    n_pig = sum(hv246_pig_total_cat >= 1, na.rm = T)) %>%
  mutate(chicken_info = paste0(comma(n_chicken), " (", percent((n_chicken / n), accuracy = 0.1), ")"),
         goat_info = paste0(comma(n_goat), " (", percent((n_goat / n), accuracy = 0.1), ")"),
         bull_info = paste0(comma(n_bull), " (", percent((n_bull / n), accuracy = 0.1), ")"),
         horse_info = paste0(comma(n_horse), " (", percent((n_horse / n), accuracy = 0.1), ")"),
         pig_info = paste0(comma(n_pig), " (", percent((n_pig / n), accuracy = 0.1), ")"),
         kgc_course = recode(kgc_course, .missing = "Unknown")) %>%
  bind_rows(summarise(., kgc_course = "Total", n = sum(n),
                      chicken_info = paste0(comma(sum(n_chicken)), " (", percent(sum(n_chicken) / sum(n)), ")"),
                      goat_info = paste0(comma(sum(n_goat)), " (", percent(sum(n_goat) / sum(n)), ")"),
                      bull_info = paste0(comma(sum(n_bull)), " (", percent(sum(n_bull) / sum(n)), ")"),
                      horse_info = paste0(comma(sum(n_horse)), " (", percent(sum(n_horse) / sum(n)), ")"),
                      pig_info = paste0(comma(sum(n_pig)), " (", percent(sum(n_pig) / sum(n)), ")"))) %>%
  select(-c(n_chicken, n_goat, n_bull, n_horse, n_pig)) %>% 
  qflextable() %>%
  set_header_labels(kgc_course = "Koppen-Geiger Zone", n = "Number of Households", avg_rainfall = "Average 7-Day Rainfall (cm)",
                    ses = "SES", chicken_info = "Chicken/Poultry/Duck\n(n, %)",
                    goat_info = "Goat/Sheep\n(n, %)", bull_info = "Bull/Cow/Cattle\n(n, %)",
                    horse_info = "Horse/Donkey\n(n, %)", pig_info = "Pig\n(n, %)") %>%
  theme_zebra() %>% theme_box() %>%
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)



descriptive %>%
  mutate(hv270 = case_when(
    hv270 == "Poorest" ~ 1,
    hv270 == "Poor" ~ 2,
    hv270 == "Middle" ~ 3,
    hv270 == "Rich" ~ 4,
    hv270 == "Richest" ~ 5,
    TRUE ~ NA_real_
  )) %>%
  group_by(kgc_course) %>%
  summarise(
    n = n(),
    ses = round(mean(hv270, na.rm = TRUE), 2)
  )


descriptive %>% 




test <- descriptive %>% filter(is.na(ClimateZ)) %>% select(LATNUM, LONGNUM) %>% 
        distinct(.keep_all = T) %>% 
  mutate(LATNUM = round(LATNUM, 8)) %>% 
  mutate(LONGNUM = round(LONGNUM, 8)) %>% 
  
  
  qflextable() %>% 
  theme_zebra() %>% theme_box() %>%
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

descriptive %>% filter(is.na(ClimateZ)) %>% select(LATNUM, LONGNUM) %>% 
  tabyl(LATNUM) %>% adorn_pct_formatting() %>% 
  qflextable() %>% 
  theme_zebra() %>% theme_box() %>%
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)
  

 
save_as_docx(kgc_table, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/tables/kgc_table.docx")

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



# Animal Diarrhea Trend ---------------------------------------------------
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
  
  

# Animal and SES Trend ----------------------------------------------------

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
  
  ggplot(data = chicken, aes(y=ses, x=hv246_chicken_poultry_duck_total_cat)) + 
    geom_point() +
    geom_smooth(color = "blue", fill = "lightblue") +
    labs(x = "Number of Chickens/Poultry/Duck",
         y = "SES Level") +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    annotate("text", x = Inf, y = Inf, label = paste("Number of observations:", nrow(chicken)), 
             hjust = 1.5, vjust = 1.5, size = 8, color = "black")
  
  
  
  chicken <- rural %>% 
    group_by(hv246_chicken_poultry_duck_total_cat) %>% 
    summarise(n = n(),
              n_under5 = sum(b8 <= 4, na.rm = TRUE),
              num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
              percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
              ses = round(mean(hv270a, na.rm = TRUE), 2)) %>%
    filter(!num_diarrhea == 0) %>% 
    filter(hv246_chicken_poultry_duck_total_cat <= 100)
  
  ggplot(data = chicken, aes(y=ses, x=hv246_chicken_poultry_duck_total_cat)) + 
    geom_point() +
    geom_smooth(color = "darkgreen", fill = "lightgreen") +
    labs(x = "Number of Chickens/Poultry/Duck",
         y = "SES Level") +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  
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
  
  ggplot(data = goat, aes(y=ses, x=hv246_goat_sheep_total_cat)) + 
    geom_point() +
    geom_smooth(color = "blue", fill = "lightblue") +
    labs(x = "Number of Goat/Sheep",
         y = "SES Level") +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  
  goat <- rural %>% 
    group_by(hv246_goat_sheep_total_cat) %>% 
    summarise(n = n(),
              n_under5 = sum(b8 <= 4, na.rm = TRUE),
              num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
              percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
              ses = round(mean(hv270a, na.rm = TRUE), 2)) %>%
    filter(!num_diarrhea == 0) %>% 
    filter(hv246_goat_sheep_total_cat <= 100)
  
  ggplot(data = goat, aes(y=ses, x=hv246_goat_sheep_total_cat)) + 
    geom_point() +
    geom_smooth(color = "darkgreen", fill = "lightgreen") +
    labs(x = "Number of Goat/Sheep",
         y = "SES Level") +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  # cattle/cow/bull
  bull <- rural %>% 
    group_by(hv246_bull_cow_cattle_total_cat) %>% 
    summarise(n = n(),
              n_under5 = sum(b8 <= 4, na.rm = TRUE),
              num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
              percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
              ses = round(mean(hv270, na.rm = TRUE), 2)) %>%
    filter(!num_diarrhea == 0) %>% 
    filter(hv246_bull_cow_cattle_total_cat <= 100)
  
  ggplot(data = bull, aes(y=ses, x=hv246_bull_cow_cattle_total_cat)) + 
    geom_point() +
    geom_smooth(color = "blue", fill = "lightblue") +
    labs(x = "Number of Bull/Cow/Cattle",
         y = "SES Level") +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  
  bull <- rural %>% 
    group_by(hv246_bull_cow_cattle_total_cat) %>% 
    summarise(n = n(),
              n_under5 = sum(b8 <= 4, na.rm = TRUE),
              num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
              percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
              ses = round(mean(hv270a, na.rm = TRUE), 2)) %>%
    filter(!num_diarrhea == 0) %>% 
    filter(hv246_bull_cow_cattle_total_cat <= 100)
  
  ggplot(data = bull, aes(y=ses, x=hv246_bull_cow_cattle_total_cat)) + 
    geom_point() +
    geom_smooth(color = "darkgreen", fill = "lightgreen") +
    labs(x = "Number of Bull/Cow/Cattle",
         y = "SES Level") +
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
    filter(hv246_horse_donkey_total_cat <= 25)
  
  ggplot(data = horse, aes(y=ses, x=hv246_horse_donkey_total_cat)) + 
    geom_point() +
    geom_smooth(color = "blue", fill = "lightblue") +
    labs(x = "Number of Horse/Donkey",
         y = "SES Level") +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  horse <- rural %>% 
    group_by(hv246_horse_donkey_total_cat) %>% 
    summarise(n = n(),
              n_under5 = sum(b8 <= 4, na.rm = TRUE),
              num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
              percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
              ses = round(mean(hv270a, na.rm = TRUE), 2)) %>%
    filter(!num_diarrhea == 0) %>% 
    filter(hv246_horse_donkey_total_cat <= 25)
  
  ggplot(data = horse, aes(y=ses, x=hv246_horse_donkey_total_cat)) + 
    geom_point() +
    geom_smooth(color = "darkgreen", fill = "lightgreen") +
    labs(x = "Number of Horse/Donkey",
         y = "SES Level") +
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
  
  ggplot(data = pig, aes(y=ses, x=hv246_pig_total_cat)) + 
    geom_point() +
    geom_smooth(color = "blue", fill = "lightblue") +
    labs(x = "Number of Pig",
         y = "SES Level") +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  
  pig <- rural %>% 
    group_by(hv246_pig_total_cat) %>% 
    summarise(n = n(),
              n_under5 = sum(b8 <= 4, na.rm = TRUE),
              num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
              percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
              ses = round(mean(hv270a, na.rm = TRUE), 2)) %>%
    filter(!num_diarrhea == 0) %>% 
    filter(hv246_pig_total_cat <= 30)
  
  ggplot(data = pig, aes(y=ses, x=hv246_pig_total_cat)) + 
    geom_point() +
    geom_smooth(color = "darkgreen", fill = "lightgreen") +
    labs(x = "Number of Pig",
         y = "SES Level") +
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
  
  ggplot(data = other, aes(y=ses, x=hv246_other_total_cat)) + 
    geom_point() +
    geom_smooth(color = "blue", fill = "lightblue") +
    labs(x = "Number of Other Animals",
         y = "SES Level") +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  other <- rural %>% 
    group_by(hv246_other_total_cat) %>% 
    summarise(n = n(),
              n_under5 = sum(b8 <= 4, na.rm = TRUE),
              num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
              percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
              ses = round(mean(hv270a, na.rm = TRUE), 2)) %>%
    filter(!num_diarrhea == 0) %>% 
    filter(hv246_other_total_cat <= 50)
  
  ggplot(data = other, aes(y=ses, x=hv246_other_total_cat)) + 
    geom_point() +
    geom_smooth(color = "darkgreen", fill = "lightgreen") +
    labs(x = "Number of Other Animals",
         y = "SES Level") +
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

datapasta::dpasta(for_graph)
  
  
tibble::tribble(
                                                                             ~kgc_course, ~hv007,  ~n_hh, ~avg_rainfall,
                                                                                  
                                                                                   "Dry",   2000,  4454L,          0.47,
                                                                                   "Dry",   2003,  5025L,          1.79,
                                                                                   "Dry",   2004,   586L,           1.5,
                                                                                   "Dry",   2005,  3313L,          2.01,
                                                                              
                                                                                   "Dry",   2019,  5861L,          0.53,
                                                                                   "Dry",   2020,  6086L,          0.05,
                                                                                   "Dry",   2021, 15905L,          0.99,
                                                                                   "Dry",   2022,  1545L,          0.18,
                                                                            
                                                                             "Temperate",   2015,  9989L,          1.78,
                                                                             "Temperate",   2016, 10625L,          3.74,
                                                                             "Temperate",   2017,  2119L,          3.39,
                                                                             "Temperate",   2018,  5090L,          1.18,
                                                                             "Temperate",   2019,  2722L,          7.07,
                                                                             "Temperate",   2020,  1833L,          5.47,
                                                                             "Temperate",   2021,  4309L,          1.74,
                                                                             "Temperate",   2022,  3509L,          2.91,
                                                                              "Tropical",   1990,  3320L,          5.76,
                                                                              "Tropical",   1992,   271L,             0,
                                                                              "Tropical",   1993,  4214L,          1.76,
                                                                              "Tropical",   1994,  4973L,          3.18,
                                                                              "Tropical",   1995,  1160L,          0.24,
                                                                              "Tropical",   1996,  3313L,          4.19,
                                                                     
                                                                              "Tropical",   2017, 18271L,          2.48,
                                                                              "Tropical",   2018, 38147L,           2.6,
                                                                              "Tropical",   2019, 15688L,          5.93,
                                                                              "Tropical",   2020, 11109L,           1.7,
                                                                              "Tropical",   2021, 29812L,          2.64,
                                                                              "Tropical",   2022,  7245L,          2.01
                                                                             )




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



# EPE Analysis ------------------------------------------------------------

# EPE_7
under5_animal %>% 
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            epe_7_95_0 = sum(epe_7_95 == 0, na.rm = TRUE),
            percent_0 = round((epe_7_95_0/n)*100, digits = 2),
            epe_7_95_1 = sum(epe_7_95 == 1, na.rm = TRUE),
            percent_1 = round((epe_7_95_1/n)*100, digits = 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No",
                             "1" = "Yes",
                             .missing = "Unknown")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, epe_7_95_0, epe_7_95_1)) %>% 
  qflextable() %>% 
  set_header_labels(n = "Number Under 5",  diarrhea_dichot = "Diarrhea?",
                    epe_7_95_0 = "0 EPEs", percent_0 = "%",
                    percent_1 = "%", 
                    epe_7_95_1 = "1 EPEs")%>% 
  add_header_lines(values = c("Total Precipitation, Past 7 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)


  epe_7 <- under5_animal %>% 
    group_by(epe_7_95) %>% 
    summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
              num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
              percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
    adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
    qflextable() %>% 
    set_header_labels(epe_7_95 = "EPE Past 7 Days", n_under5 = "Children Under 5 with Animals",
                      num_diarrhea = "Children Under 5 with Diarrhea",
                      percent_diarrhea = "Diarrhea Prevalence")%>%
    add_header_lines(values = c("Total Precipitation, Past 7 Days")) %>% 
    theme_zebra() %>% theme_box() %>% 
    align_nottext_col(align = "center", header = TRUE) %>%
    align_text_col(align = "center", header = TRUE)
  
  save_as_docx(epe_7, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/epe_7.docx")
  
  
# EPE_14
under5_animal %>% tabyl(epe_14_95)
under5_animal %>% 
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            epe_14_95_0 = sum(epe_14_95 == 0, na.rm = TRUE),
            percent_0 = round((epe_14_95_0/n)*100, digits = 2),
            epe_14_95_1 = sum(epe_14_95 == 1, na.rm = TRUE),
            percent_1 = round((epe_14_95_1/n)*100, digits = 2),
            epe_14_95_2 = sum(epe_14_95 == 2, na.rm = TRUE),
            percent_2 = round((epe_14_95_2/n)*100, digits = 2),
            epe_14_95_3 = sum(epe_14_95 == 3, na.rm = TRUE),
            percent_3 = round((epe_14_95_3/n)*100, digits = 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No",
                                  "1" = "Yes",
                                  .missing = "Unknown")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, epe_14_95_0, epe_14_95_1, epe_14_95_2, epe_14_95_3)) %>% 
  qflextable() %>% 
  set_header_labels(n = "Number Under 5",  diarrhea_dichot = "Diarrhea?",
                    epe_14_95_0 = "0 EPEs", percent_0 = "%",
                    percent_1 = "%", percent_2 = "%",
                    percent_3 = "%",
                    epe_14_95_1 = "1 EPEs",epe_14_95_2 = "2 EPEs",epe_14_95_3 = "3 EPEs")%>% 
  add_header_lines(values = c("Total Precipitation, Past 14 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)


epe_14 <- under5_animal %>% 
  group_by(epe_14_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(epe_14_95 = "EPE Past 14 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 14 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

save_as_docx(epe_14, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/epe_14.docx")

# EPE_30
under5_animal %>% tabyl(epe_30_95)
under5_animal %>% 
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            epe_30_95_0 = sum(epe_30_95 == 0, na.rm = TRUE),
            percent_0 = round((epe_30_95_0/n)*100, digits = 2),
            epe_30_95_1 = sum(epe_30_95 == 1, na.rm = TRUE),
            percent_1 = round((epe_30_95_1/n)*100, digits = 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No",
                                  "1" = "Yes",
                                  .missing = "Unknown")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, epe_30_95_0, epe_30_95_1)) %>% 
  qflextable() %>% 
  set_header_labels(n = "Number Under 5",  diarrhea_dichot = "Diarrhea?",
                    epe_30_95_0 = "0 EPEs", percent_0 = "%",
                    percent_1 = "%", 
                    epe_30_95_1 = "1 EPEs")%>% 
  add_header_lines(values = c("Total Precipitation, Past 30 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

epe_30 <- under5_animal %>% 
  group_by(epe_30_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(epe_30_95 = "EPE Past 30 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 30 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

save_as_docx(epe_30, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/epe_30.docx")

# EPE_60
under5_animal %>% tabyl(epe_60_95)
under5_animal %>% 
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            epe_60_95_0 = sum(epe_60_95 == 0, na.rm = TRUE),
            percent_0 = round((epe_60_95_0/n)*100, digits = 2),
            epe_60_95_1 = sum(epe_60_95 == 1, na.rm = TRUE),
            percent_1 = round((epe_60_95_1/n)*100, digits = 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No",
                                  "1" = "Yes",
                                  .missing = "Unknown")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, epe_60_95_0, epe_60_95_1)) %>% 
  qflextable() %>% 
  set_header_labels(n = "Number Under 5",  diarrhea_dichot = "Diarrhea?",
                    epe_60_95_0 = "0 EPEs", percent_0 = "%",
                    percent_1 = "%", 
                    epe_60_95_1 = "1 EPEs")%>% 
  add_header_lines(values = c("Total Precipitation, Past 60 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

epe_60 <- under5_animal %>% 
  group_by(epe_60_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(epe_60_95 = "EPE Past 60 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 60 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

save_as_docx(epe_60, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/epe_60.docx")

# EPE_8_14
under5_animal %>% tabyl(epe_814_95)
under5_animal %>% 
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            epe_814_95_0 = sum(epe_814_95 == 0, na.rm = TRUE),
            percent_0 = round((epe_814_95_0/n)*100, digits = 2),
            epe_814_95_1 = sum(epe_814_95 == 1, na.rm = TRUE),
            percent_1 = round((epe_814_95_1/n)*100, digits = 2),
            epe_814_95_2 = sum(epe_814_95 == 2, na.rm = TRUE),
            percent_2 = round((epe_814_95_2/n)*100, digits = 2),
            epe_814_95_3 = sum(epe_814_95 == 3, na.rm = TRUE),
            percent_3 = round((epe_814_95_3/n)*100, digits = 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No",
                                  "1" = "Yes",
                                  .missing = "Unknown")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, epe_814_95_0, epe_814_95_1, epe_814_95_2, epe_814_95_3)) %>% 
  qflextable() %>% 
  set_header_labels(n = "Number Under 5",  diarrhea_dichot = "Diarrhea?",
                    epe_814_95_0 = "0 EPEs", percent_0 = "%",
                    percent_1 = "%", percent_2 = "%",
                    percent_3 = "%",
                    epe_814_95_1 = "1 EPEs",epe_814_95_2 = "2 EPEs",epe_814_95_3 = "3 EPEs")%>% 
  add_header_lines(values = c("Total Precipitation, 8-14 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

epe_814 <- under5_animal %>% 
  group_by(epe_814_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(epe_814_95 = "EPE Past 8-14 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 8-14 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

save_as_docx(epe_814, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/epe_814.docx")


# EPE_15_21
under5_animal %>% tabyl(epe_1521_95)
under5_animal %>% 
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            epe_1521_95_0 = sum(epe_1521_95 == 0, na.rm = TRUE),
            percent_0 = round((epe_1521_95_0/n)*100, digits = 2),
            epe_1521_95_1 = sum(epe_1521_95 == 1, na.rm = TRUE),
            percent_1 = round((epe_1521_95_1/n)*100, digits = 2),
            epe_1521_95_2 = sum(epe_1521_95 == 2, na.rm = TRUE),
            percent_2 = round((epe_1521_95_2/n)*100, digits = 2),
            epe_1521_95_3 = sum(epe_1521_95 == 3, na.rm = TRUE),
            percent_3 = round((epe_1521_95_3/n)*100, digits = 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No",
                                  "1" = "Yes",
                                  .missing = "Unknown")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, epe_1521_95_0, epe_1521_95_1, epe_1521_95_2, epe_1521_95_3)) %>% 
  qflextable() %>% 
  set_header_labels(n = "Number Under 5",  diarrhea_dichot = "Diarrhea?",
                    epe_1521_95_0 = "0 EPEs", percent_0 = "%",
                    percent_1 = "%", percent_2 = "%",
                    percent_3 = "%",
                    epe_1521_95_1 = "1 EPEs",epe_1521_95_2 = "2 EPEs",epe_1521_95_3 = "3 EPEs")%>% 
  add_header_lines(values = c("Total Precipitation, 15-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

epe_1521 <- under5_animal %>% 
  group_by(epe_1521_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2),
            age = round(mean(b8, na.rm = TRUE), 2)) %>% 
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(epe_1521_95 = "EPE Past 15-21 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)


test_result <- wilcox.test(hv270 ~ epe_3060_95, data = under5_animal, 
                           exact = FALSE)

test_result


test_result <- wilcox.test(b8 ~ epe_3060_95, data = under5_animal, 
                           exact = FALSE)

test_result





save_as_docx(epe_1521, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/epe_1521.docx")

# EPE_821
under5_animal %>% tabyl(epe_821_95)
under5_animal %>% 
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            epe_821_95_0 = sum(epe_821_95 == 0, na.rm = TRUE),
            percent_0 = round((epe_821_95_0/n)*100, digits = 2),
            epe_821_95_1 = sum(epe_821_95 == 1, na.rm = TRUE),
            percent_1 = round((epe_821_95_1/n)*100, digits = 2),
            epe_821_95_2 = sum(epe_821_95 == 2, na.rm = TRUE),
            percent_2 = round((epe_821_95_2/n)*100, digits = 2),
            epe_821_95_3 = sum(epe_821_95 == 3, na.rm = TRUE),
            percent_3 = round((epe_821_95_3/n)*100, digits = 2),
            epe_821_95_4 = sum(epe_821_95 == 4, na.rm = TRUE),
            percent_4 = round((epe_821_95_4/n)*100, digits = 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No",
                                  "1" = "Yes",
                                  .missing = "Unknown")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, epe_821_95_0, epe_821_95_1, epe_821_95_2, epe_821_95_3, epe_821_95_4)) %>% 
  qflextable() %>% 
  set_header_labels(n = "Number Under 5",  diarrhea_dichot = "Diarrhea?",
                    epe_821_95_0 = "0 EPEs", percent_0 = "%",
                    percent_1 = "%", percent_2 = "%",
                    percent_3 = "%", percent_4 = "%",
                    epe_821_95_1 = "1 EPEs",epe_821_95_2 = "2 EPEs",epe_821_95_3 = "3 EPEs",epe_821_95_4 = "4 EPEs")%>% 
  add_header_lines(values = c("Total Precipitation, 8-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

epe_821 <- under5_animal %>% 
  group_by(epe_821_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(epe_821_95 = "EPE Past 8-21 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 8-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

save_as_docx(epe_821, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/epe_821.docx")

# EPE_1528

under5_animal %>% 
  group_by(epe_1528_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(epe_1528_95 = "EPE Past 15-28 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-28 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

epe_1528 <- under5_animal %>% 
  group_by(epe_1528_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2),
            age = round(mean(b8, na.rm = TRUE), 2)) %>% 
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(epe_1528_95 = "EPE Past 15-28 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-28 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

save_as_docx(epe_1528, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/epe_1528.docx")

# EPE_22-28

epe_2228 <- under5_animal %>% 
  group_by(epe_2228_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2),
            age = round(mean(b8, na.rm = TRUE), 2)) %>% 
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(epe_2228_95 = "EPE Past 22-28 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 22-28 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

save_as_docx(epe_2228, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/epe_2228.docx")

# EPE_30-60

epe_3060 <- under5_animal %>% 
  group_by(epe_3060_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2),
            age = round(mean(b8, na.rm = TRUE), 2)) %>% 
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(epe_3060_95 = "EPE Past 30-60 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 30-60 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

save_as_docx(epe_3060, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/epe_3060.docx")

# EPE_15-60

epe_1560 <- under5_animal %>% 
  group_by(epe_1560_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100,
            ses = round(mean(hv270, na.rm = TRUE), 2),
            age = round(mean(b8, na.rm = TRUE), 2)) %>% 
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(epe_1560_95 = "EPE Past 15-60 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-60 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

save_as_docx(epe_1560, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/epe_1560.docx")



# Animals and EPE Analysis -----------------------------------------------------

# Chicken

under5_animal %>% 
  group_by(chicken_poultry_duck_present, epe_1521_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
         mutate(chicken_poultry_duck_present = recode_factor(chicken_poultry_duck_present,
                                               "0" = "No",
                                               "1" = "Yes")) %>% 
  arrange(desc(chicken_poultry_duck_present)) %>% 
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(chicken_poultry_duck_present = "Chicken/Duck/Poultry?",
                    epe_1521_95 = "EPE Past 15-21 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)%>% 
    bg(i = ~ chicken_poultry_duck_present == "Yes", bg = "#c5ebc9") %>%
  bg(i = ~ chicken_poultry_duck_present == "No", bg = "#f7d0d0")


under5_animal %>% 
  group_by(chicken_poultry_duck_present, epe_1521_95_binary) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  mutate(chicken_poultry_duck_present = recode_factor(chicken_poultry_duck_present,
                                                      "0" = "No",
                                                      "1" = "Yes")) %>%   
  mutate(epe_1521_95_binary = recode_factor(epe_1521_95_binary,
                                                      "0" = "No",
                                                      "1" = "Yes")) %>% 
  arrange(desc(chicken_poultry_duck_present)) %>% 
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(chicken_poultry_duck_present = "Chicken/Duck/Poultry?",
                    epe_1521_95_binary = "EPE Past 15-21 Days?", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)%>% 
  bg(i = ~ chicken_poultry_duck_present == "Yes", bg = "#c5ebc9") %>%
  bg(i = ~ chicken_poultry_duck_present == "No", bg = "#f7d0d0")





test <- under5_animal %>% 
  group_by(chicken_poultry_duck_present, epe_1521_95_binary) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  select(-n_under5, -num_diarrhea) %>% 
  mutate(chicken_poultry_duck_present = recode_factor(chicken_poultry_duck_present,
                                                      "0" = "No",
                                                      "1" = "Yes")) %>%   
  mutate(epe_1521_95_binary = recode_factor(epe_1521_95_binary,
                                            "0" = "No",
                                            "1" = "Yes")) %>% 
  arrange(desc(chicken_poultry_duck_present))


# Create a 2x2 table using percent_diarrhea values
table_2x2 <- test %>%
  pivot_wider(names_from = epe_1521_95_binary, values_from = percent_diarrhea, names_prefix = "epe_") %>%
  column_to_rownames("chicken_poultry_duck_present") %>%
  as.data.frame()

# Rearrange columns to switch EPE Yes and EPE No
table_2x2 <- table_2x2[, c("epe_Yes", "epe_No")]

# Add row names as a column for qflextable compatibility
table_2x2 <- tibble::rownames_to_column(table_2x2, var = "chicken_poultry_duck_present")

# Convert the table to a qflextable
qf_table <- qflextable(table_2x2)

# Set column labels
qf_table <- qf_table %>%
  set_table_properties(width = 0.3, layout = "autofit") %>%
  set_header_labels(
    chicken_poultry_duck_present = "Chicken/Poultry/Duck Present",
    epe_Yes = "EPE Yes",
    epe_No = "EPE No"
  ) %>%
  # Adjust width of the first column (row names)
  width(j = 1, width = 0.1) %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)  # Adjust width of the first column as needed

# Print the qflextable
print(qf_table)

# Goat/Sheep

under5_animal %>% 
  group_by(goat_sheep_present, epe_1521_95_binary) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  mutate(goat_sheep_present = recode(goat_sheep_present,
                                               "0" = "No",
                                               "1" = "Yes")) %>%   
  mutate(epe_1521_95_binary = recode_factor(epe_1521_95_binary,
                                            "0" = "No",
                                            "1" = "Yes")) %>%
  arrange(desc(goat_sheep_present)) %>%  
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(goat_sheep_present = "Goat/Sheep?",
                    epe_1521_95_binary = "EPE Past 15-21 Days?", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)%>% 
  bg(i = ~ goat_sheep_present == "Yes", bg = "#c5ebc9") %>%
  bg(i = ~ goat_sheep_present == "No", bg = "#f7d0d0")

under5_animal %>% 
  group_by(goat_sheep_present, epe_1521_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  mutate(goat_sheep_present = recode(goat_sheep_present,
                                     "0" = "No",
                                     "1" = "Yes")) %>% 
  arrange(desc(goat_sheep_present)) %>%  
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(goat_sheep_present = "Goat/Sheep?",
                    epe_1521_95 = "EPE Past 15-21 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)%>% 
  bg(i = ~ goat_sheep_present == "Yes", bg = "#c5ebc9") %>%
  bg(i = ~ goat_sheep_present == "No", bg = "#f7d0d0")


# Bull/Cow

under5_animal %>% 
  group_by(bull_cow_cattle_present, epe_1521_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  mutate(bull_cow_cattle_present = recode(bull_cow_cattle_present,
                                     "0" = "No",
                                     "1" = "Yes")) %>% 
  arrange(desc(bull_cow_cattle_present)) %>%  
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(bull_cow_cattle_present = "Bull/Cow/Cattle?",
                    epe_1521_95 = "EPE Past 15-21 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)%>% 
  bg(i = ~ bull_cow_cattle_present == "Yes", bg = "#c5ebc9") %>%
  bg(i = ~ bull_cow_cattle_present == "No", bg = "#f7d0d0")






under5_animal %>% 
  group_by(bull_cow_cattle_present, epe_1521_95_binary) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  mutate(bull_cow_cattle_present = recode(bull_cow_cattle_present,
                                          "0" = "No",
                                          "1" = "Yes")) %>%   
  mutate(epe_1521_95_binary = recode_factor(epe_1521_95_binary,
                                            "0" = "No",
                                            "1" = "Yes")) %>%
  arrange(desc(bull_cow_cattle_present)) %>%  
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(bull_cow_cattle_present = "Bull/Cow/Cattle?",
                    epe_1521_95_binary = "EPE Past 15-21 Days?", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)%>% 
  bg(i = ~ bull_cow_cattle_present == "Yes", bg = "#c5ebc9") %>%
  bg(i = ~ bull_cow_cattle_present == "No", bg = "#f7d0d0")


# Horse/Donkey

under5_animal %>% 
  group_by(horse_donkey_present, epe_1521_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  mutate(horse_donkey_present = recode(horse_donkey_present,
                                     "0" = "No",
                                     "1" = "Yes")) %>% 
  arrange(desc(horse_donkey_present)) %>%  
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(horse_donkey_present = "Horse/Donkey?",
                    epe_1521_95 = "EPE Past 15-21 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)%>% 
  bg(i = ~ horse_donkey_present == "Yes", bg = "#c5ebc9") %>%
  bg(i = ~ horse_donkey_present == "No", bg = "#f7d0d0")



under5_animal %>% 
  group_by(horse_donkey_present, epe_1521_95_binary) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  mutate(horse_donkey_present = recode(horse_donkey_present,
                                       "0" = "No",
                                       "1" = "Yes")) %>%   
  mutate(epe_1521_95_binary = recode_factor(epe_1521_95_binary,
                                            "0" = "No",
                                            "1" = "Yes")) %>% 
  arrange(desc(horse_donkey_present)) %>%  
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(horse_donkey_present = "Horse/Donkey?",
                    epe_1521_95_binary = "EPE Past 15-21 Days?", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)%>% 
  bg(i = ~ horse_donkey_present == "Yes", bg = "#c5ebc9") %>%
  bg(i = ~ horse_donkey_present == "No", bg = "#f7d0d0")

# Pig

under5_animal %>% 
  group_by(pig_present, epe_1521_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  mutate(pig_present = recode(pig_present,
                                     "0" = "No",
                                     "1" = "Yes")) %>% 
  arrange(desc(pig_present)) %>% 
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(pig_present = "Pigs?",
                    epe_1521_95 = "EPE Past 15-21 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)%>% 
  bg(i = ~ pig_present == "Yes", bg = "#c5ebc9") %>%
  bg(i = ~ pig_present == "No", bg = "#f7d0d0")







under5_animal %>% 
  group_by(pig_present, epe_1521_95_binary) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  mutate(pig_present = recode(pig_present,
                              "0" = "No",
                              "1" = "Yes")) %>%   
  mutate(epe_1521_95_binary = recode_factor(epe_1521_95_binary,
                                            "0" = "No",
                                            "1" = "Yes")) %>% 
  arrange(desc(pig_present)) %>% 
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(pig_present = "Pigs?",
                    epe_1521_95_binary = "EPE Past 15-21 Days?", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)%>% 
  bg(i = ~ pig_present == "Yes", bg = "#c5ebc9") %>%
  bg(i = ~ pig_present == "No", bg = "#f7d0d0")


# Other

under5_animal %>% 
  group_by(other_present, epe_1521_95) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  mutate(other_present = recode(other_present,
                                     "0" = "No",
                                     "1" = "Yes")) %>% 
  arrange(desc(other_present)) %>%  
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(other_present = "Other Animals?",
                    epe_1521_95 = "EPE Past 15-21 Days", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)%>% 
  bg(i = ~ other_present == "Yes", bg = "#c5ebc9") %>%
  bg(i = ~ other_present == "No", bg = "#f7d0d0")



under5_animal %>% 
  group_by(other_present, epe_1521_95_binary) %>% 
  summarise(n_under5 = sum(b8 <= 4, na.rm = TRUE),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4) * 100) %>% 
  mutate(other_present = recode(other_present,
                                "0" = "No",
                                "1" = "Yes")) %>%   
  mutate(epe_1521_95_binary = recode_factor(epe_1521_95_binary,
                                            "0" = "No",
                                            "1" = "Yes")) %>% 
  arrange(desc(other_present)) %>%  
  adorn_totals("row",,,,c(n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(other_present = "Other Animals?",
                    epe_1521_95_binary = "EPE Past 15-21 Days?", n_under5 = "Children Under 5 with Animals",
                    num_diarrhea = "Children Under 5 with Diarrhea",
                    percent_diarrhea = "Diarrhea Prevalence")%>%
  add_header_lines(values = c("Total Precipitation, Past 15-21 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)%>% 
  bg(i = ~ other_present == "Yes", bg = "#c5ebc9") %>%
  bg(i = ~ other_present == "No", bg = "#f7d0d0")


# Vaccinations ------------------------------------------------------------

# Rotavirus
under5_animal %>% 
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            zero = sum(h57 == 0, na.rm = TRUE),
            percent_0 = round((zero/n)*100, digits = 2),
            one = sum(h57 == 1, na.rm = TRUE),
            percent_1 = round((one/n)*100, digits = 2),
            two = sum(h57 == 2, na.rm = TRUE),
            percent_2 = round((two/n)*100, digits = 2),
            three = sum(h57 == 3, na.rm = TRUE),
            percent_3 = round((three/n)*100, digits = 2),
            eight = sum(h57 == 8, na.rm = TRUE),
            percent_8 = round((eight/n)*100, digits = 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No",
                                  "1" = "Yes",
                                  .missing = "Unknown")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, zero, one, two, three, eight)) %>% 
  qflextable() %>% 
  set_header_labels(n = "Number Under 5",  diarrhea_dichot = "Diarrhea?",
                    zero = "None", percent_0 = "%",
                    one = "Vax Date", percent_1 = "%",
                    two = "Mom Report", percent_2 = "%",
                    three = "Vax Card", percent_3 = "%",
                    eight = "Don't Know", percent_8 = "%")%>% 
  add_header_lines(values = c("Rotavirus Vaccination")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

under5_animal %>% 
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            zero = sum(h61 == 0, na.rm = TRUE),
            percent_0 = round((zero/n)*100, digits = 2),
            one = sum(h61 == 1, na.rm = TRUE),
            percent_1 = round((one/n)*100, digits = 2),
            two = sum(h61 == 2, na.rm = TRUE),
            percent_2 = round((two/n)*100, digits = 2),
            three = sum(h61 == 3, na.rm = TRUE),
            percent_3 = round((three/n)*100, digits = 2),
            eight = sum(h61 == 8, na.rm = TRUE),
            percent_8 = round((eight/n)*100, digits = 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No",
                                  "1" = "Yes",
                                  .missing = "Unknown")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, zero, one, two, three, eight)) %>% 
  qflextable() %>% 
  set_header_labels(n = "Number Under 5",  diarrhea_dichot = "Diarrhea?",
                    zero = "None", percent_0 = "%",
                    one = "Vax Date", percent_1 = "%",
                    two = "Mom Report", percent_2 = "%",
                    three = "Vax Card", percent_3 = "%",
                    eight = "Don't Know", percent_8 = "%")%>% 
  add_header_lines(values = c("Hep B Vaccination")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)


under5_animal %>% 
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            zero = sum(h51 == 0, na.rm = TRUE),
            percent_0 = round((zero/n)*100, digits = 2),
            one = sum(h51 == 1, na.rm = TRUE),
            percent_1 = round((one/n)*100, digits = 2),
            two = sum(h51 == 2, na.rm = TRUE),
            percent_2 = round((two/n)*100, digits = 2),
            three = sum(h51 == 3, na.rm = TRUE),
            percent_3 = round((three/n)*100, digits = 2),
            eight = sum(h51 == 8, na.rm = TRUE),
            percent_8 = round((eight/n)*100, digits = 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No",
                                  "1" = "Yes",
                                  .missing = "Unknown")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, zero, one, two, three, eight)) %>% 
  qflextable() %>% 
  set_header_labels(n = "Number Under 5",  diarrhea_dichot = "Diarrhea?",
                    zero = "None", percent_0 = "%",
                    one = "Vax Date", percent_1 = "%",
                    two = "Mom Report", percent_2 = "%",
                    three = "Vax Card", percent_3 = "%",
                    eight = "Don't Know", percent_8 = "%")%>% 
  add_header_lines(values = c("DPT-HepB-Hib Pentavalent Vaccination")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)


# Quartiles ---------------------------------------------------------------


person_level_filter <- rural %>% select(hv246_chicken_poultry_duck_total_cat,
                                        hv246_bull_cow_cattle_total_cat,
                                        hv246_goat_sheep_total_cat,
                                        hv246_horse_donkey_camel_total_cat, b8) %>% 
                                 filter(b8 <= 4)

# Calculate quantiles for hv246_chicken_poultry_duck_total_cat
chicken_quantiles <- person_level_filter %>% 
  filter(hv246_chicken_poultry_duck_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.25),
    q50 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.5),
    q75 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.75),
    q90 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.90),
    q95 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Chicken/Poultry/Duck")

# Calculate quantiles for hv246_bull_cow_cattle_total_cat
cattle_quantiles <- person_level_filter %>% 
  filter(hv246_bull_cow_cattle_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.25),
    q50 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.5),
    q75 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.75),
    q90 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.90),
    q95 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Bull/Cow/Cattle")

# Calculate quantiles for hv246_bull_cow_cattle_total_cat
goat_sheep_quantiles <- person_level_filter %>% 
  filter(hv246_goat_sheep_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_goat_sheep_total_cat, probs = 0.25),
    q50 = quantile(hv246_goat_sheep_total_cat, probs = 0.5),
    q75 = quantile(hv246_goat_sheep_total_cat, probs = 0.75),
    q90 = quantile(hv246_goat_sheep_total_cat, probs = 0.90),
    q95 = quantile(hv246_goat_sheep_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Goat/Sheep")


# Calculate quantiles for hv246_bull_cow_cattle_total_cat
horse_donkey_quantiles <- person_level_filter %>% 
  filter(hv246_horse_donkey_camel_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.25),
    q50 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.5),
    q75 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.75),
    q90 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.90),
    q95 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Horse/Donkey/Camel")

# Combine the tables
bind_rows(chicken_quantiles, cattle_quantiles, goat_sheep_quantiles, horse_donkey_quantiles) %>% 
  select(category, everything()) %>% 
  qflextable() %>% 
  set_header_labels(category = "Animal Category")%>% 
  add_header_lines(values = c("Percentiles of Animal Ownership (Person Level)
                              n = 904,958*")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE) %>% 
  add_footer_lines("*Excludes people older than 5 and children that aren't exposed to each category")


hh_level_filter <- rural_hh %>% ungroup() %>% select(hv246_chicken_poultry_duck_total_cat,
                                       hv246_bull_cow_cattle_total_cat,
                                       hv246_goat_sheep_total_cat,
                                       hv246_horse_donkey_camel_total_cat, hh_under5) %>% 
                                filter(hh_under5 == 1)


# Calculate quantiles for hv246_chicken_poultry_duck_total_cat
chicken_quantiles <- hh_level_filter %>% 
  filter(hv246_chicken_poultry_duck_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.25),
    q50 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.5),
    q75 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.75),
    q90 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.90),
    q95 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Chicken/Poultry/Duck")

# Calculate quantiles for hv246_bull_cow_cattle_total_cat
cattle_quantiles <- hh_level_filter %>% 
  filter(hv246_bull_cow_cattle_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.25),
    q50 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.5),
    q75 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.75),
    q90 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.90),
    q95 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Bull/Cow/Cattle")

# Calculate quantiles for hv246_bull_cow_cattle_total_cat
goat_sheep_quantiles <- hh_level_filter %>% 
  filter(hv246_goat_sheep_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_goat_sheep_total_cat, probs = 0.25),
    q50 = quantile(hv246_goat_sheep_total_cat, probs = 0.5),
    q75 = quantile(hv246_goat_sheep_total_cat, probs = 0.75),
    q90 = quantile(hv246_goat_sheep_total_cat, probs = 0.90),
    q95 = quantile(hv246_goat_sheep_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Goat/Sheep")


# Calculate quantiles for hv246_bull_cow_cattle_total_cat
horse_donkey_quantiles <- hh_level_filter %>% 
  filter(hv246_horse_donkey_camel_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.25),
    q50 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.5),
    q75 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.75),
    q90 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.90),
    q95 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Horse/Donkey/Camel")
  
# Combine the tables
bind_rows(chicken_quantiles, cattle_quantiles, goat_sheep_quantiles, horse_donkey_quantiles) %>% 
  select(category, everything()) %>% 
  qflextable() %>% 
  set_header_labels(category = "Animal Category")%>% 
  add_header_lines(values = c("Percentiles of Animal Ownership (Household Level)
                              n = 504,036*")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE) %>% 
  add_footer_lines("*Excludes households that don't have a child under 5 and each category excludes households that don't own an animal in the category")


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

# Unadjusted Models (High Exposures Only) ------------------------------------------------------------------

chicken_only <- under5_animal %>% select(bull_cow_cattle_present, horse_donkey_present,       
                                         chicken_poultry_duck_present, goat_sheep_present, hv246_chicken_poultry_duck_total_cat,
                                         diarrhea_dichot, name_year, hv001) %>% 
                                  filter(bull_cow_cattle_present == 0 & horse_donkey_present == 0 & goat_sheep_present == 0)

chicken_high <- chicken_only %>% filter(hv246_chicken_poultry_duck_total_cat >= 15 | hv246_chicken_poultry_duck_total_cat == 0)

model_chicken <- glmer(diarrhea_dichot ~ chicken_poultry_duck_present +
                         (1|name_year/hv001), data = chicken_high, family = binomial)

table_chicken <- model_chicken %>% tbl_regression(label = list(chicken_poultry_duck_present ~ "Chicken/Duck/Poultry"),exponentiate = TRUE) %>% 
                 bold_labels() %>% add_n()



bull_only <- under5_animal %>% select(bull_cow_cattle_present, horse_donkey_present,       
                                      chicken_poultry_duck_present, goat_sheep_present, hv246_bull_cow_cattle_total_cat,
                                      diarrhea_dichot, name_year, hv001) %>% 
                              filter(chicken_poultry_duck_present == 0 & horse_donkey_present == 0 & goat_sheep_present == 0)

bull_high <- bull_only %>% filter(hv246_bull_cow_cattle_total_cat >= 10 | hv246_bull_cow_cattle_total_cat == 0)

model_bull <- glmer(diarrhea_dichot ~ bull_cow_cattle_present +
                      (1|name_year/hv001), data = bull_high, family = binomial)

table_bull <- model_bull %>% tbl_regression(label = list(bull_cow_cattle_present ~ "Bull/Cow/Cattle"),exponentiate = TRUE) %>% 
              bold_labels() %>% add_n()




goat_only <- under5_animal %>% select(bull_cow_cattle_present, horse_donkey_present,       
                                      chicken_poultry_duck_present, goat_sheep_present, hv246_goat_sheep_total_cat,
                                      diarrhea_dichot, name_year, hv001) %>% 
                               filter(chicken_poultry_duck_present == 0 & horse_donkey_present == 0 & bull_cow_cattle_present == 0)

goat_high <- goat_only %>% filter(hv246_goat_sheep_total_cat >= 8 | hv246_goat_sheep_total_cat == 0)

model_goat <- glmer(diarrhea_dichot ~ goat_sheep_present +
                      (1|name_year/hv001), data = goat_high, family = binomial)

table_goat <- model_goat %>% tbl_regression(label = list(goat_sheep_present ~ "Goat/Sheep"),exponentiate = TRUE) %>% bold_labels() %>% add_n()



horse_only <- under5_animal %>% select(bull_cow_cattle_present, horse_donkey_present,       
                                       chicken_poultry_duck_present, goat_sheep_present, hv246_horse_donkey_total_cat,
                                       diarrhea_dichot, name_year, hv001) %>% 
                                filter(chicken_poultry_duck_present == 0 & goat_sheep_present == 0 & bull_cow_cattle_present == 0)

horse_high <- horse_only %>% filter(hv246_horse_donkey_total_cat >= 1 | hv246_horse_donkey_total_cat == 0)

model_horse <- glmer(diarrhea_dichot ~ horse_donkey_present +
                       (1|name_year/hv001), data = horse_high, family = binomial)

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