# Header ------------------------------------------------------------------

# Load Libraries
library(readr)
library(janitor)
library(flextable)
library(corrr)
library(corrplot)
library(lme4)
library(gtsummary)
library(gt)
library()
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
rural         <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/rural.rds")
rural_hh      <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/rural_hh.rds")
under5_animal <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/under5_animal.rds")
under5_dia    <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/under5_dia.rds")

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
            n_horse = sum(hv246_horse_donkey_camel_total_cat >= 1, na.rm = T),
            n_pig = sum(hv246_pig_total_cat >= 1, na.rm = T),
            n_other = sum(hv246_other_total_cat >= 1, na.rm = T)) %>%
  adorn_totals(where = c("row")) %>% 
  qflextable() %>% 
  set_header_labels(country = "Country", n = "# HH",
                    n_chicken = "Chicken/Poultry/Duck", n_goat = "Goat/Sheep", 
                    n_bull = "Cattle/Cow/Bull",
                    n_horse = "Horse/Donkey/Camel", n_pig = "Pigs",
                    n_other = "Other")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)


# Venn Diagrams -----------------------------------------------------------

## Venn Diagram 4 way -----------------------------------------------------
rural_2 <- rural_hh %>% 
  ungroup() %>% 
  select(bull_cow_cattle_present, horse_donkey_camel_present,       
         chicken_poultry_duck_present, goat_sheep_present)

my_list <- list("Bull/Cow/Cattle" = which(rural_2$bull_cow_cattle_present == 1), 
                "Chicken/Poultry/Duck" = which(rural_2$chicken_poultry_duck_present == 1),
                "Goat/Sheep" = which(rural_2$goat_sheep_present == 1),
                "Horse/Donkey/Camel" = which(rural_2$horse_donkey_camel_present == 1)) 

ggVennDiagram(my_list, label_alpha = 0, set_color = c("blue","darkred","darkgreen","black"), set_size = 6,
              label_percent_digit = 1) + 
  scale_fill_distiller(palette = "Reds", direction = 1) + 
  scale_x_continuous(expand = expansion(mult = .2))+ 
  guides(fill = guide_legend(title = "Household Count")) +
  theme(legend.position = "bottom")

## Venn Diagram 5 way -----------------------------------------------------
rural_2 <- rural %>%
  group_by(name_year) %>%
  distinct(hhid, .keep_all = TRUE) %>% 
  ungroup() %>% 
  select(bull_cow_cattle_present, horse_donkey_camel_present,       
         chicken_poultry_duck_present, goat_sheep_present, pig_present)

my_list <- list("Bull/Cow/Cattle" = which(rural_2$bull_cow_cattle_present == 1), 
                "Chicken/Poultry/Duck" = which(rural_2$chicken_poultry_duck_present == 1),
                "Goat/Sheep" = which(rural_2$goat_sheep_present == 1),
                "Horse/Donkey/Camel" = which(rural_2$horse_donkey_camel_present == 1),
                "pig" = which(rural_2$pig_present == 1))
                
                
ggVennDiagram(my_list, label_alpha = 0, set_color = c("blue","darkred","darkgreen","black", "orange"), set_size = 6,
              label_percent_digit = 1) + 
  scale_fill_distiller(palette = "Reds", direction = 1) + 
  scale_x_continuous(expand = expansion(mult = .2))+ 
  guides(fill = guide_legend(title = "Household Count")) +
  theme(legend.position = "bottom")

## Venn Diagram 6 way ------------------------------------------------------
rural_2 <- rural %>%
  group_by(name_year) %>%
  distinct(hhid, .keep_all = TRUE) %>% 
  ungroup() %>% 
  select(bull_cow_cattle_present, horse_donkey_camel_present,       
         chicken_poultry_duck_present, goat_sheep_present, pig_present, other_present)

my_list <- list("Bull/Cow/Cattle" = which(rural_2$bull_cow_cattle_present == 1), 
                "Chicken/Poultry/Duck" = which(rural_2$chicken_poultry_duck_present == 1),
                "Goat/Sheep" = which(rural_2$goat_sheep_present == 1),
                "Horse/Donkey/Camel" = which(rural_2$horse_donkey_camel_present == 1),
                "pig" = which(rural_2$pig_present == 1),
                "Other" = which(rural_2$other_present == 1))


ggVennDiagram(my_list, label_alpha = 0, set_color = c("blue","darkred","darkgreen","black", "orange", "purple"), set_size = 6,
              label_percent_digit = 1) + 
  scale_fill_distiller(palette = "Reds", direction = 1) + 
  scale_x_continuous(expand = expansion(mult = .2))+ 
  guides(fill = guide_legend(title = "Household Count")) +
  theme(legend.position = "bottom")


# Summarise the diarrhea by any animals -----------------------------------

rural %>% 
  group_by(name_year, hhid) %>% 
  mutate(first_animal_total = if_else(row_number()==1, animal_total, 0 )) %>%
  group_by(hv246) %>% 
  summarise(n = n(),
            animal_total = sum(first_animal_total),
            n_under5 = sum(b8 <= 5, na.rm = TRUE),
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

# Summarise the diarrhea by any diarrhea

rural %>%  
  group_by(name_year, hhid) %>% 
  mutate(first_animal_total_hhunder5 = if_else(hh_under5 == 1 & row_number() == 1, animal_total, 0)) %>%
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 5, na.rm = TRUE),
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

rural %>% 
  mutate(animal_total_cut = if_else(is.na(animal_total_cut), "Unknown Animal", as.character(animal_total_cut))) %>%
  group_by(animal_total_cut) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 5, na.rm = TRUE),
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


# Summarise the diarrhea by number of different animals exposed to
rural %>% 
  group_by(animal_combo) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 5, na.rm = TRUE),
            avg_num_animal = round(mean(animal_total, na.rm = TRUE), 2),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4)*100) %>%
  adorn_totals("row",,,,c(n, n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(animal_combo = "Number of Different Animals in Households", n = "Number of People", 
                    n_under5 = "Children under 5", avg_num_animal = "Average Number of Animals", 
                    num_diarrhea = "Children under 5 with Diarrhea", percent_diarrhea = "Diarrhea Prevalence")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)


# KGC ---------------------------------------------------------------

# Summarise the diarrhea by kgz
rural %>% 
  group_by(kgc_course) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 5, na.rm = TRUE),
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


# EPE Analysis ------------------------------------------------------------

# EPE_7
under5_animal %>% 
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            epe_7_95_0 = sum(epe_7_95 == 0, na.rm = TRUE),
            percent_0 = round((epe_7_95_0/n)*100, digits = 2),
            epe_7_95_1 = sum(epe_7_95 == 1, na.rm = TRUE),
            percent_1 = round((epe_7_95_1/n)*100, digits = 2),
            epe_7_95_2 = sum(epe_7_95 == 2, na.rm = TRUE),
            percent_2 = round((epe_7_95_2/n)*100, digits = 2),
            epe_7_95_3 = sum(epe_7_95 == 3, na.rm = TRUE),
            percent_3 = round((epe_7_95_3/n)*100, digits = 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No",
                             "1" = "Yes",
                             .missing = "Unknown")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, epe_7_95_0, epe_7_95_1, epe_7_95_2, epe_7_95_3)) %>% 
  qflextable() %>% 
  set_header_labels(n = "Number Under 5",  diarrhea_dichot = "Diarrhea?",
                    epe_7_95_0 = "0 EPEs", percent_0 = "%",
                    percent_1 = "%", percent_2 = "%",
                    percent_3 = "%",
                    epe_7_95_1 = "1 EPEs",epe_7_95_2 = "2 EPEs",epe_7_95_3 = "3 EPEs")%>% 
  add_header_lines(values = c("Total Precipitation, Past 7 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

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


# Check the animals-------------------------------------------------------

under5_animal <- under5_animal %>%
  mutate(diarrhea_cattle = case_when(bull_cow_cattle_present == 0 ~ NA_integer_,
                                     bull_cow_cattle_present == 1 & diarrhea_dichot == 1 ~ 1, 
                                     TRUE ~ 0)) %>% 
  mutate(diarrhea_chicken = case_when(chicken_poultry_duck_present == 0 ~ NA_integer_,
                                  chicken_poultry_duck_present == 1 & diarrhea_dichot == 1 ~ 1, 
                                     TRUE ~ 0)) %>% 
  mutate(diarrhea_goat = case_when(goat_sheep_present == 0 ~ NA_integer_,
                                    goat_sheep_present == 1 & diarrhea_dichot == 1 ~ 1, 
                                     TRUE ~ 0)) %>% 
  mutate(diarrhea_horse = case_when(horse_donkey_camel_present == 0 ~ NA_integer_,
                                   horse_donkey_camel_present == 1 & diarrhea_dichot == 1 ~ 1, 
                                     TRUE ~ 0)) %>% 
  mutate(diarrhea_pig = case_when(pig_present == 0 ~ NA_integer_,
                                    pig_present == 1 & diarrhea_dichot == 1 ~ 1, 
                                     TRUE ~ 0)) %>% 
  mutate(diarrhea_other = case_when(other_present == 0 ~ NA_integer_,
                                      other_present == 1 & diarrhea_dichot == 1 ~ 1, 
                                     TRUE ~ 0))



logit_cattle <- glm(diarrhea_dichot ~ bull_cow_cattle_present, data = under5_animal, family = "binomial")
exp(cbind(OR = coef(logit_cattle), confint(logit_cattle)))




rural %>%  
  group_by(name_year, hhid) %>% 
  mutate(first_animal_total_hhunder5 = if_else(hh_under5 == 1 & row_number() == 1, animal_total, 0)) %>%
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 5, na.rm = TRUE),
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






rural %>%
  summarise(num_children = n(),
            num_cattle = sum(bull_cow_cattle_present),
            dia_yes = sum(diarrhea_cattle == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_cattle == 0, na.rm = TRUE),
            percent_dia = round((dia_yes/dia_no)*100, digits = 1),
            or = paste0("0.92 (0.91, 0.94)"))%>% 
  qflextable() %>% 
  set_header_labels(num_children = "Number of Children",
                    num_cattle = "Exposed to Cattle",
                    dia_yes = "Diarrhea",
                    dia_no = "No Diarrhea",
                    percent_dia = "% Diarrhea",
                    or = "OR (95% CI)") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)
  
logit_cow <- glm(diarrhea_dichot ~ animal_cow_present, data = rural_final, family = "binomial")
exp(cbind(OR = coef(logit_cow), confint(logit_cow)))

rural_final %>%
  summarise(num_children = n(),
            num_cattle = sum(animal_cow_present),
            dia_yes = sum(diarrhea_cow == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_cow == 0, na.rm = TRUE),
            percent_dia = round((dia_yes/dia_no)*100, digits = 1),
            or = paste0("0.93 (0.91, 0.95)"))%>% 
  qflextable() %>% 
  set_header_labels(num_children = "Number of Children",
                    num_cattle = "Exposed to Cows",
                    dia_yes = "Diarrhea",
                    dia_no = "No Diarrhea",
                    percent_dia = "% Diarrhea",
                    or = "OR (95% CI)") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

logit_horse <- glm(diarrhea_dichot ~ animal_horse_present, data = rural_final, family = "binomial")
exp(cbind(OR = coef(logit_horse), confint(logit_horse)))
           
rural_final %>%
  summarise(num_children = n(),
            num_cattle = sum(animal_horse_present),
            dia_yes = sum(diarrhea_horse == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_horse == 0, na.rm = TRUE),
            percent_dia = round((dia_yes/dia_no)*100, digits = 1),
            or = paste0("0.97 (0.95, 0.99)"))%>% 
  qflextable() %>% 
  set_header_labels(num_children = "Number of Children",
                    num_cattle = "Exposed to Horse",
                    dia_yes = "Diarrhea",
                    dia_no = "No Diarrhea",
                    percent_dia = "% Diarrhea",
                    or = "OR (95% CI)") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

logit_goat <- glm(diarrhea_dichot ~ animal_goat_present, data = rural_final, family = "binomial")
exp(cbind(OR = coef(logit_goat), confint(logit_goat)))

rural_final %>%
  summarise(num_children = n(),
            num_cattle = sum(animal_goat_present),
            dia_yes = sum(diarrhea_goat == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_goat == 0, na.rm = TRUE),
            percent_dia = round((dia_yes/dia_no)*100, digits = 1),
            or = paste0("1.02 (1.00, 1.04)"))%>% 
  qflextable() %>% 
  set_header_labels(num_children = "Number of Children",
                    num_cattle = "Exposed to Goat",
                    dia_yes = "Diarrhea",
                    dia_no = "No Diarrhea",
                    percent_dia = "% Diarrhea",
                    or = "OR (95% CI)") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

logit_sheep <- glm(diarrhea_dichot ~ animal_sheep_present, data = rural_final, family = "binomial")
exp(cbind(OR = coef(logit_sheep), confint(logit_sheep)))

rural_final %>%
  summarise(num_children = n(),
            num_cattle = sum(animal_sheep_present),
            dia_yes = sum(diarrhea_sheep == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_sheep == 0, na.rm = TRUE),
            percent_dia = round((dia_yes/dia_no)*100, digits = 1),
            or = paste0("0.96 (0.94, 0.98)"))%>% 
  qflextable() %>% 
  set_header_labels(num_children = "Number of Children",
                    num_cattle = "Exposed to Sheep",
                    dia_yes = "Diarrhea",
                    dia_no = "No Diarrhea",
                    percent_dia = "% Diarrhea",
                    or = "OR (95% CI)") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

logit_chicken <- glm(diarrhea_dichot ~ animal_chicken_present, data = rural_final, family = "binomial")
exp(cbind(OR = coef(logit_chicken), confint(logit_chicken)))

rural_final %>%
  summarise(num_children = n(),
            num_cattle = sum(animal_chicken_present),
            dia_yes = sum(diarrhea_chicken == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_chicken == 0, na.rm = TRUE),
            percent_dia = round((dia_yes/dia_no)*100, digits = 1),
            or = paste0("0.95 (0.93, 0.97)"))%>% 
  qflextable() %>% 
  set_header_labels(num_children = "Number of Children",
                    num_cattle = "Exposed to Chicken",
                    dia_yes = "Diarrhea",
                    dia_no = "No Diarrhea",
                    percent_dia = "% Diarrhea",
                    or = "OR (95% CI)") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)




tbl_stack_ex1 <- tbl_stack(list(cattle, cow, horse, goat, sheep, chicken))




# Correlation Graphs ----------------------------------------------------------------

# Variable to subset
weather_var <- c("e_totalminus7",    "e_totalminus14",    "e_totalminus30",    "e_totalminus60",
                 "tp_totalminus7",   "tp_totalminus14",   "tp_totalminus30",   "tp_totalminus60",
                 "sro_totalminus7",  "sro_totalminus14",  "sro_totalminus30",  "sro_totalminus60",
                 "ssro_totalminus7", "ssro_totalminus14", "ssro_totalminus30", "ssro_totalminus60",
                 "t2m_avgminus7",    "t2m_avgminus14",    "t2m_avgminus30",    "t2m_avgminus60",
                 "skt_avgminus7",    "skt_avgminus14",    "skt_avgminus30",    "skt_avgminus60",
                 "lai_lv_avgminus7", "lai_lv_avgminus14", "lai_lv_avgminus30", "lai_lv_avgminus60",
                 "lai_hv_avgminus7", "lai_hv_avgminus14", "lai_hv_avgminus30", "lai_hv_avgminus60",
                 "d2m_avgminus7",    "d2m_avgminus14",    "d2m_avgminus30",    "d2m_avgminus60")


# Correlation for rural data
subset_rural <- rural %>% select(h11, all_of(weather_var))

cor_rural_focus <- subset_rural %>% 
  correlate() %>% 
  focus(hv204) %>% 
  arrange(hv204)

cor_rural_focus <- cor_rural_focus %>%
  mutate(color = case_when(
    grepl("e", term) ~ "#e41a1c",
    grepl("^sro$", term) ~ "#377eb8",
    grepl("ssro", term) ~ "#4daf4a",
    grepl("t2m", term) ~ "#984ea3",
    grepl("tp", term) ~ "#ff7f00",
    grepl("skt", term) ~ "#2b888f",
    grepl("lai_lv", term) ~ "#a65628",
    grepl("lai_hv", term) ~ "#f781bf",
    grepl("d2m", term) ~ "#999999",
    TRUE ~ "#377eb8"
  ))

color_chart <- cor_rural_focus$color

cor_rural_focus %>% mutate(term = factor(term, levels = term[order(hv204)])) %>% 
  mutate(color = ifelse(hv204 < 0, "negative", "positive")) %>% 
  ggplot(aes(x = term, y = hv204)) + 
  geom_bar(stat = "identity", position = "identity", aes(fill = color), color = color_chart, linewidth = 1, alpha = 0.7) +
  ylab("Correlation with Water Walk Time (HV204)") + 
  xlab("Variable") +
  theme_bw() +
  scale_fill_manual(values = c(positive = "royalblue3", negative = "firebrick1"), guide = "none")+
  theme(axis.text.x = element_text(angle = 55, hjust = 1, colour = color_chart, face = "bold", size = 11))

# Corr Plot
subset_rural <- rural_final %>% select(all_of(weather_var))

cor_rural_weather <-  cor(subset_rural, use = "complete.obs")

testRes <- cor.mtest(cor_rural_weather, conf.level = 0.95)

labelCol <-  c("purple", "purple", "purple", "purple", "black", "black", "black", "black")

corrplot(cor_rural_weather, p.mat = testRes$p, method = "square", order = "alphabet",
         diag = TRUE, type = "lower", sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = "label_sig", pch.col = "grey1", tl.col = labelCol, tl.srt = 45)


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

model_horse <- glmer(diarrhea_dichot ~ horse_donkey_camel_present +
                         (1|name_year/hv001), data = under5_animal, family = binomial)

table_horse <- model_horse %>% tbl_regression(label = list(horse_donkey_camel_present ~ "Horse/Donkey/Camel"),exponentiate = TRUE) %>% bold_labels()

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


# Adjusted Models ---------------------------------------------------------

model_chicken <- glmer(diarrhea_dichot ~ chicken_poultry_duck_present + kgc_course + epe_30_95 +
                         (1|name_year/hv001), data = under5_animal, family = binomial)

table_chicken <- model_chicken %>% tbl_regression(label = list(chicken_poultry_duck_present ~ "Chicken/Duck/Poultry", kgc_course ~ "Koppen-Geiger Zone",
                                                               epe_30_95 ~ "EPE, 30 Days"),exponentiate = TRUE) %>% bold_labels()

model_bull <- glmer(diarrhea_dichot ~ bull_cow_cattle_present + kgc_course + epe_30_95 +
                      (1|name_year/hv001), data = under5_animal, family = binomial)

table_bull <- model_bull %>% tbl_regression(label = list(bull_cow_cattle_present ~ "Bull/Cow/Cattle", kgc_course ~ "Koppen-Geiger Zone",
                                                         epe_30_95 ~ "EPE, 30 Days"),exponentiate = TRUE) %>% bold_labels()


model_goat <- glmer(diarrhea_dichot ~ goat_sheep_present + kgc_course + epe_30_95 +
                      (1|name_year/hv001), data = under5_animal, family = binomial)

table_goat <- model_goat %>% tbl_regression(label = list(goat_sheep_present ~ "Goat/Sheep", kgc_course ~ "Koppen-Geiger Zone",
                                                         epe_30_95 ~ "EPE, 30 Days"),exponentiate = TRUE) %>% bold_labels()

model_horse <- glmer(diarrhea_dichot ~ horse_donkey_camel_present + kgc_course + epe_30_95 +
                       (1|name_year/hv001), data = under5_animal, family = binomial)

table_horse <- model_horse %>% tbl_regression(label = list(horse_donkey_camel_present ~ "Horse/Donkey/Camel", kgc_course ~ "Koppen-Geiger Zone",
                                                           epe_30_95 ~ "EPE, 30 Days"),exponentiate = TRUE) %>% bold_labels()

model_pig <- glmer(diarrhea_dichot ~ pig_present + kgc_course + epe_30_95 +
                     (1|name_year/hv001), data = under5_animal, family = binomial)

table_pig <- model_pig %>% tbl_regression(label = list(pig_present ~ "Pig", kgc_course ~ "Koppen-Geiger Zone",
                                                       epe_30_95 ~ "EPE, 30 Days"),exponentiate = TRUE) %>% bold_labels()

model_other <- glmer(diarrhea_dichot ~ other_present + kgc_course + epe_30_95 +
                       (1|name_year/hv001), data = under5_animal, family = binomial)

table_other <- model_other %>% tbl_regression(label = list(other_present ~ "Other Animal", kgc_course ~ "Koppen-Geiger Zone",
                                                           epe_30_95 ~ "EPE, 30 Days"),exponentiate = TRUE) %>% bold_labels()



combined_models <- tbl_merge(tbls = list(table_chicken, table_bull, table_goat, table_horse, table_pig, table_other),
                             tab_spanner = c("**Chicken/Duck/Poultry**", "**Bull/Cow/Cattle**",
                                             "**Goat/Sheep**", "**Horse/Donkey/Camel**",
                                             "**Pig**", "**Other Animal**")) %>%
  
  # combined_models <- combined_models %>%
  modify_table_body(~.x %>%
                      dplyr::arrange(factor(var_label, levels =
                                              c("Chicken/Duck/Poultry", "Bull/Cow/Cattle",
                                                "Goat/Sheep", "Horse/Donkey/Camel",
                                                "Pig", "Other Animal", "Koppen-Geiger Zone", 
                                                "EPE, 30 Days"))))


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


gtsave(combined_models, file = "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/combined_model.html")