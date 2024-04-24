# Header ------------------------------------------------------------------

# Load Libraries
library(readr)
library(janitor)
library(flextable)
library(tidyverse)

# Set the location
#location <- "personal"
location <- "HPC"

# Set the working directories
if (location == "personal") {
  # Set Working Directory (Personal)
  setwd("C:/Users/Steven/OneDrive - Johns Hopkins/Dissertation/Dissertation/Data")
}

if (location == "HPC") { 
  # Set Working Directory (HPC)
  setwd("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH")
}

# Read in the full dataset that combined all households
data_aim2 <- readRDS("~/data-mdavis65/steven_sola/3_Survey_Weather/person_final.Rdata")

# Clean HV007 (Year) ------------------------------------------------------

# Change the "Year 1" to 2001 (GA_00 dataset)
# Change the "Year 0" to 2000 (GA_00 dataset)
# Change the two digit years into four digits by adding 1900
data_aim2 <- data_aim2 %>% mutate(hv007 = case_when(
  hv007 == 1   ~ 2001,
  hv007 == 0   ~ 2000,
  hv007 < 100  ~ hv007+1900,
  TRUE ~ hv007))


# Clean KGC ---------------------------------------------------------------

# Categorize the Koppen-Geiger Climate Classification System into fine details
data_aim2 <- data_aim2 %>% mutate(kgc_fine = case_when(
  kgc == "Af"  ~ "Tropical Rainforest",
  kgc == "Am"  ~ "Tropical Monsoon",
  kgc == "As"  ~ "Tropical Savanna, Dry Summer",
  kgc == "Aw"  ~ "Tropical Savanna Dry Winter",
  kgc == "BSh" ~ "Dry Semi-Arid Hot",
  kgc == "BSk" ~ "Dry Semi-Arid Cold",
  kgc == "BWh" ~ "Dry Arid Hot",
  kgc == "BWk" ~ "Dry Arid Desert Cold",
  kgc == "Cfa" ~ "Temperate No Dry Season Hot Summer",
  kgc == "Cfb" ~ "Temperate No Dry Season Warm Summer",
  kgc == "Csa" ~ "Temperate Dry Summer Hot Summer",
  kgc == "Csb" ~ "Temperate Dry Summer Warm Summer",
  kgc == "Cwa" ~ "Temperate Dry Winter Hot Summer",
  kgc == "Cwb" ~ "Temperate Dry Winter Warm Summer",
  kgc == "Climate Zone info missing" ~ NA_character_,
  TRUE ~ NA_character_))

# Categorize the Koppen-Geiger Climate Classification System into course details
data_aim2 <- data_aim2 %>% mutate(kgc_course = case_when(
  kgc %in% c("Af", "Am", "As", "Aw") ~ "Tropical",
  kgc %in% c("BSh", "BSk", "BWh", "BWk") ~ "Dry",
  kgc %in% c("Cfa", "Cfb", "Csa", "Cwa", "Cwb") ~ "Temperate",
  kgc == "Climate Zone info missing" ~ NA_character_,
  TRUE ~ NA_character_))

# Coalesce the wlthind5 and hv270 variables -------------------------------

data_aim2 <- data_aim2 %>%
  mutate(hv270 = coalesce(wlthind5, hv270))


# Output the datasets -----------------------------------------------------


# Read in the full dataset that combined all households
data_aim2 <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/data_aim2.Rdata")


# Save the full dataset
saveRDS(data_aim2, file = "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/data_aim2.Rdata")

# Save the rural dataset
rural <- data_aim2 %>% filter(URBAN_RURA == "R")
saveRDS(rural, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/rural.Rdata")

# Save the urban dataset
urban <- data_aim2 %>% filter(URBAN_RURA == "U")
saveRDS(urban, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/urban.Rdata")
