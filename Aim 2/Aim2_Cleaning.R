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
data_aim2 <- readRDS("~/data-mdavis65/steven_sola/3_Survey_Weather/person_final.rds")

# Clean HV007 (Year) ------------------------------------------------------

# Change the "Year 1" to 2001 (GA_00 dataset)
# Change the "Year 0" to 2000 (GA_00 dataset)
# Change the two digit years into four digits by adding 1900
data_aim2 <- data_aim2 %>% mutate(hv007 = case_when(
                             hv007 == 1   ~ 2001,
                             hv007 == 0   ~ 2000,
                             hv007 < 100  ~ hv007+1900,
                             TRUE ~ hv007))

# Specify if Household has Child Under 5 ----------------------------------

data_aim2 <- data_aim2 %>%
  group_by(name_year, hhid) %>% 
  mutate(hh_under5 = if_else(any(b8 <= 5), 1, 0)) %>%
  mutate(hh_under5 = if_else(is.na(hh_under5), 0, hh_under5)) %>% 
  ungroup()

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

# Diarrhea ----------------------------------------------------------------

# diarrhea yes/no
data_aim2 <- data_aim2 %>% 
               mutate(diarrhea_dichot = 
                        case_when((h11 == 1 | h11 == 2) ~ 1, 
                                   TRUE ~ 0))

# Add in Country Name -----------------------------------------------------

# Read in the file that lists all countries
source("countries.R")

data_aim2 <- left_join(data_aim2, countries, by = "name_year")

# Animals -----------------------------------------------------------------

# If hv246 starts with "9", set it to missing
data_aim2 <- data_aim2 %>% mutate(hv246 = na_if(hv246, 9))

# If a variable that starts with "hv246" is 0, set to na
data_aim2 <- data_aim2 %>% 
                mutate(across(starts_with("hv246_"), ~ ifelse(. %in% c(0, 98, 99), NA, .)))

# Create a new "hv246_sheep" variable, since apparently it didn't work in the other script? WTF
data_aim2$hv246_sheep <- data_aim2$hv246e

# sum across all the variables that start with "hv246_bull_cow" and put the total in a new variable animal$hv_246_bull_cow_total
data_aim2 <- data_aim2 %>%
                  mutate(hv246_bee_total_raw = rowSums(select(., starts_with("hv246_bee")), na.rm = TRUE)) %>% 
                  mutate(hv246_bull_cow_total_raw = rowSums(select(., starts_with("hv246_bull_cow")), na.rm = TRUE)) %>% 
                  mutate(hv246_camel_total_raw = rowSums(select(., starts_with("hv246_camel")), na.rm = TRUE)) %>% 
                  mutate(hv246_cattle_total_raw = rowSums(select(., starts_with("hv246_cattle")), na.rm = TRUE)) %>% 
                  mutate(hv246_chicken_poultry_total_raw = rowSums(select(., starts_with("hv246_chicken_poultry")), na.rm = TRUE)) %>% 
                  mutate(hv246_duck_total_raw = rowSums(select(., starts_with("hv246_duck")), na.rm = TRUE)) %>%
                  mutate(hv246_goat_total_raw = rowSums(select(., starts_with("hv246_goat")), na.rm = TRUE)) %>% 
                  mutate(hv246_horse_donkey_total_raw = rowSums(select(., starts_with("hv246_horse_donkey")), na.rm = TRUE)) %>%
                  mutate(hv246_other_total_raw = rowSums(select(., starts_with("hv246_other")), na.rm = TRUE)) %>% 
                  mutate(hv246_pig_total_raw = rowSums(select(., starts_with("hv246_pig")), na.rm = TRUE)) %>%
                  mutate(hv246_rabbit_total_raw = rowSums(select(., starts_with("hv246_rabbit")), na.rm = TRUE)) %>%
                  mutate(hv246_rodent_total_raw = rowSums(select(., starts_with("hv246_rodent")), na.rm = TRUE)) %>%
                  mutate(hv246_sheep_total_raw = rowSums(select(., starts_with("hv246_sheep")), na.rm = TRUE))

# Recatagorize
data_aim2 <- data_aim2 %>% 
                  mutate(hv246_bull_cow_cattle_total_cat = hv246_bull_cow_total_raw + hv246_cattle_total_raw) %>%
                  mutate(hv246_chicken_poultry_duck_total_cat = hv246_chicken_poultry_total_raw + hv246_duck_total_raw) %>% 
                  mutate(hv246_goat_sheep_total_cat = hv246_goat_total_raw + hv246_sheep_total_raw) %>% 
                  mutate(hv246_horse_donkey_camel_total_cat = hv246_horse_donkey_total_raw + hv246_camel_total_raw) %>% 
                  mutate(hv246_pig_total_cat = hv246_pig_total_raw) %>% 
                  mutate(hv246_other_total_cat = hv246_other_total_raw + hv246_rabbit_total_raw +
                                                 hv246_rodent_total_raw + hv246_bee_total_raw)

# Presence / Absence of animal (disaggregated)
data_aim2 <- data_aim2 %>%
                mutate(bee_present = case_when(!is.na(hv246_bee_total_raw) & hv246_bee_total_raw >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(bull_cow_present = case_when(!is.na(hv246_bull_cow_total_raw) & hv246_bull_cow_total_raw >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(camel_present = case_when(!is.na(hv246_camel_total_raw) & hv246_camel_total_raw >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(cattle_present = case_when(!is.na(hv246_cattle_total_raw) & hv246_cattle_total_raw >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(chicken_poultry_present = case_when(!is.na(hv246_chicken_poultry_total_raw) & hv246_chicken_poultry_total_raw >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(duck_present = case_when(!is.na(hv246_duck_total_raw) & hv246_duck_total_raw >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(goat_present = case_when(!is.na(hv246_goat_total_raw) & hv246_goat_total_raw >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(horse_donkey_present = case_when(!is.na(hv246_horse_donkey_total_raw) & hv246_horse_donkey_total_raw >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(other_present = case_when(!is.na(hv246_other_total_raw) & hv246_other_total_raw >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(pig_present = case_when(!is.na(hv246_pig_total_raw) & hv246_pig_total_raw >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(rabbit_present = case_when(!is.na(hv246_rabbit_total_raw) & hv246_rabbit_total_raw >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(rodent_present = case_when(!is.na(hv246_rodent_total_raw) & hv246_rodent_total_raw >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(sheep_present = case_when(!is.na(hv246_sheep_total_raw) & hv246_sheep_total_raw >= 1 ~ 1, TRUE ~ 0))
 
# Presence / Absence of animal (aggregated)
data_aim2 <- data_aim2 %>%
                mutate(bull_cow_cattle_present = case_when(!is.na(hv246_bull_cow_cattle_total_cat) & hv246_bull_cow_cattle_total_cat >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(chicken_poultry_duck_present = case_when(!is.na(hv246_chicken_poultry_duck_total_cat) & hv246_chicken_poultry_duck_total_cat >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(goat_sheep_present = case_when(!is.na(hv246_goat_sheep_total_cat) & hv246_goat_sheep_total_cat >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(horse_donkey_camel_present = case_when(!is.na(hv246_horse_donkey_camel_total_cat) & hv246_horse_donkey_camel_total_cat >= 1 ~ 1, TRUE ~ 0)) %>%
                mutate(pig_present = case_when(!is.na(hv246_pig_total_cat) & hv246_pig_total_cat >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(other_present = case_when(!is.na(hv246_other_total_cat) & hv246_other_total_cat >= 1 ~ 1, TRUE ~ 0)) 

# Specify whether a person has been exposed to more than one animal
data_aim2 <- data_aim2 %>% mutate(animal_combo = 
                                    case_when(
                                       bull_cow_cattle_present + 
                                       chicken_poultry_present +
                                       goat_sheep_present + 
                                       horse_donkey_camel_present +
                                       other_present + 
                                       pig_present  >= 2 ~ 1, TRUE ~ 0)) 

# Number of categories exposed to
data_aim2 <- data_aim2 %>%
                mutate(animal_combo = bull_cow_cattle_present + 
                                      chicken_poultry_present +
                                      goat_sheep_present + 
                                      horse_donkey_camel_present +
                                      other_present + 
                                      pig_present) 

# Total animals
data_aim2 <- data_aim2 %>% mutate(animal_total = 
                                    rowSums(select(., c(
                                      hv246_bull_cow_cattle_total_cat,
                                      hv246_chicken_poultry_duck_total_cat,
                                      hv246_goat_sheep_total_cat, 
                                      hv246_horse_donkey_camel_total_cat,
                                      hv246_pig_total_cat, 
                                      hv246_other_total_cat)), na.rm = TRUE))

# Create even cut points for the total animals variable
table(Hmisc::cut2(data_aim2$animal_total, g=8))
data_aim2 <- data_aim2 %>% 
               mutate(animal_total_cut = cut(animal_total, breaks = c(0,3,7,13,25,Inf)))

# UG_06 had people who had animals, but their hv246 was set to 0
data_aim2 <- data_aim2 %>% 
               mutate(hv246 = ifelse(name_year == "UG_06" & is.na(hv246) & animal_total != 0, 1, hv246))

# EPE ---------------------------------------------------------------------
# Calculate the EPE
# EPE = single day that was above the 95% of the given timeframe
data_aim2 <- data_aim2 %>%
               mutate(across(starts_with("tp_"), ~ case_when(. < 0 ~ 0, TRUE ~ .))) %>% 
               mutate(across(starts_with("tp_totalminus"), ~ case_when(. == 0 ~ 999, TRUE ~ .)))%>%

               mutate(epe_7_95 = rowSums(select(., num_range("tp_", 1:7)) >= tp_totalminus7_95)) %>% 
               mutate(epe_14_95 = rowSums(select(., num_range("tp_", 1:14)) >= tp_totalminus14_95)) %>% 
               mutate(epe_30_95 = rowSums(select(., num_range("tp_", 1:30)) >= tp_totalminus30_95)) %>% 
               mutate(epe_60_95 = rowSums(select(., num_range("tp_", 1:60)) >= tp_totalminus60_95)) %>% 
               mutate(epe_814_95 = rowSums(select(., num_range("tp_", 8:14)) >= tp_totalminus8_14_95)) %>% 
               mutate(epe_1521_95 = rowSums(select(., num_range("tp_", 15:21)) >= tp_totalminus15_21_95)) %>% 
               mutate(epe_821_95 = rowSums(select(., num_range("tp_", 8:21)) >= tp_totalminus8_21_95)) %>% 
               mutate(epe_1528_95 = rowSums(select(., num_range("tp_", 15:28)) >= tp_totalminus15_28_95)) %>% 
               mutate(epe_2228_95 = rowSums(select(., num_range("tp_", 22:28)) >= tp_totalminus22_28_95)) %>% 
               mutate(epe_3060_95 = rowSums(select(., num_range("tp_", 30:60)) >= tp_totalminus30_60_95)) %>% 
               mutate(epe_1560_95 = rowSums(select(., num_range("tp_", 15:60)) >= tp_totalminus15_60_95))

# Codebook Animals --------------------------------------------------------

# Check which countries have animals
# Group by each survey
# Check number of people in each survey
# Check how many answered the question "animals owned?"
# If missing of question == number of people, no info
# We can exclude those people that we don't have information on
animals_not_present <- data_aim2 %>% 
                         group_by(name_year) %>% 
                         summarise(n_rows = n(),
                         missing_count = sum(is.na(hv246)),
                         all_missing = n_rows == missing_count) %>% 
                         filter(all_missing == TRUE) %>% 
                         select(name_year) 

# Filter out those countries that don't have animals
countries <- countries %>% anti_join(animals_not_present, by = "name_year")

# Filter out specific countries
countries <- countries %>% filter(!name_year %in% c("CM_91", "CM_98", "TD_9697", "CI_05",
                                                    "ML_9596", "ML_01", "NI_92", "NI_98",
                                                    "SN_05", "TG_98", "TZ_0304"))

# Create a list to bind all the DFs together at the end.
codebook <- list()

# Start of for_loop
for (i in 1:nrow(countries)){
  
  country <- countries$country[i]
  name_year <- countries$name_year[i]
  
  # Set the working directory to the specific country  
  setwd(paste0("~/data-mdavis65/steven_sola/", country,"/",name_year,"/",name_year,"_hhmember"))
  
  # Read in survey data (Household member)
  files <- list.files(getwd(), pattern="(\\.MAP|\\.map)", recursive = TRUE)
  
  # Set the locale for the files (helps with reading in weird formatting)
  locale <- locale(encoding = "latin1")
  
  # Read in the codebook files
  animals <- read_csv(files, show_col_types = F, col_names = "HV246", locale = locale)
  
  # Slice the codebook that was written in
  # Each one has "Piped" in common at the start, but they end at the different points.
  # Then separate out the result between the code and the hv201_source
  # Finally, get rid of all the missing labels
  animals <- animals %>% slice((grep("HV246", HV246)):(grep("HV247", HV246)-1)) %>% 
    separate(HV246, c("code", "HV246_animal"), sep = "\\s", extra = "merge") %>% 
    filter(!is.na(HV246_animal))
  
# Only include entries where the "code" has "HV246"
# Filter out where the codebook entry has a "NA
animals <- animals %>% filter(str_detect(code, "HV246")) %>% 
                         filter(!str_detect(HV246_animal, "NA"))

# Trim the white space in the HV246_animal variable
animals$HV246_animal <- trimws(animals$HV246_animal)

# Remove everything after the spaces
animals$HV246_animal <- str_remove(animals$HV246_animal, "  .*")

# Remove all the (CS) in the dataframe
animals$HV246_animal <- str_remove(animals$HV246_animal, "\\s*\\([^\\)]+\\)")

# Trim the white space in the hv201_source variable
animals$HV246_animal <- trimws(animals$HV246_animal)
  
# Add in the name_year variable
animals$name_year <- name_year
  
# Save all the df together to bind together
codebook[[i]] <- animals
  
# Print the name that is finished
print(paste0(name_year, " has finished processing"))

}

# bind all the DFs generated together
codebook <- do.call(rbind, codebook)

# Filter out the meaningless entries that didn't specify an animal
codebook <- codebook %>% filter(!str_detect(HV246_animal, "(Own|Owns|own) CS")) %>% 
                         filter(!str_detect(HV246_animal, "^CS (own|Own|Owns)$"))

# When the codebook mentions any other animal, just change it to "other"
codebook <- codebook %>% mutate(HV246_animal = if_else(str_detect(HV246_animal, "(Other|other)"), "other", HV246_animal))

# Filter out HV246, since that is standard across all surveys
codebook <- codebook %>% filter(!str_detect(code, "^HV246$"))

# Clean up 4 "codes" that have parentheses and extra bits not needed
codebook <- codebook %>% mutate(code = if_else(str_detect(code, "\\("), str_extract(code, "HV246(A|B|C)"), code))

# Cleaning up the distinct lables -----------------------------------------
codebook <- codebook %>% mutate(HV246_animal_recode = case_when(str_detect(HV246_animal, "(Chickens|Chicken|chicken|turkey|Poultry|Fowl|poultry|bird|fowl)") ~ "chicken_poultry",
                                                                str_detect(HV246_animal, "(Bull|bull|cow|Cow)") ~ "bull_cow",
                                                                str_detect(HV246_animal, "(Goat|goat)") ~ "goat",
                                                                str_detect(HV246_animal, "(Sheep|sheep)") ~ "sheep",
                                                                str_detect(HV246_animal, "(cattle|Cattle|Zebus)") ~ "cattle",
                                                                str_detect(HV246_animal, "(Horses|horse|donkey|Donkey|mule)") ~ "horse_donkey",
                                                                str_detect(HV246_animal, "(camel|Camel)") ~ "camel",
                                                                str_detect(HV246_animal, "(grasscutter|rodent|Rodent|guinea pig|guinea pigs)") ~ "rodent",
                                                                str_detect(HV246_animal, "(pork|porc|Pigs|pig)") ~ "pig",
                                                                str_detect(HV246_animal, "(duck|geese|Duck)") ~ "duck",
                                                                str_detect(HV246_animal, "(rabbit|Rabbits)") ~ "rabbit",
                                                                str_detect(HV246_animal, "(bee|Bee)") ~ "bee",
                                                                TRUE ~ HV246_animal))

# Prepend "hv246_" to the above
codebook$HV246_animal_recode <- paste0("hv246_", codebook$HV246_animal_recode)

# save the codebook as a .rds file
saveRDS(codebook, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/codebook.rds")

# Clean Unneeded Columns --------------------------------------------------

data_aim2 <- data_aim2 %>%  
  select(-matches("^(awf|ml|d4|fg|fy|g1|m[1-8]|s4|sdv|si|v[1-8])"))

# Output the datasets -----------------------------------------------------

# Save the full dataset
saveRDS(data_aim2, file = "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/data_aim2.rds")

## Save the urban dataset -------------------------------------------------
urban <- data_aim2 %>% filter(URBAN_RURA == "U")
saveRDS(urban, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/urban.rds")

# Household Level 
urban_hh <- urban %>%
            group_by(name_year) %>%
            distinct(hhid, .keep_all = TRUE)

saveRDS(urban_hh, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/urban_hh.rds")

# Remove urban dataset
rm(urban)
gc()

## Save the rural dataset -------------------------------------------------
rural <- data_aim2 %>% filter(URBAN_RURA == "R")
saveRDS(rural, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/rural.rds")

# Household Level 
rural_hh <- rural %>%
            group_by(name_year) %>%
            distinct(hhid, .keep_all = TRUE)

saveRDS(rural_hh, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/rural_hh.rds")

# Under 5 with Animals ----------------------------------------------------
under5_animal <- rural %>% filter(b8 <= 5) %>%                        # Only those under 5
                           filter(hv246 == 1) %>%                     # Only those with Animals
                           filter(b5 == 1) %>%                        # Only include those that are still alive
                           filter(!between(LATNUM, -0.0001, 0.0001))  # Remove unrealistic GPS

saveRDS(under5_animal, file = "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/under5_animal.rds")

# Rural Under 5 Diarrhea with Animals -------------------------------------
under5_dia <- rural %>% filter(b8 <= 5) %>%                        # Only those under 5
                        filter(diarrhea_dichot == 1) %>%           # Only those that have diarrhea
                        filter(hv246 == 1) %>%                     # Only those with Animals
                        filter(b5 == 1) %>%                        # Only include those that are still alive
                        filter(!between(LATNUM, -0.0001, 0.0001))  # Remove unrealistic GPS

saveRDS(under5_dia, file = "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/under5_dia.rds")