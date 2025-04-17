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
  mutate(hh_under5 = if_else(any(b8 <= 4), 1, 0)) %>%
  mutate(hh_under5 = if_else(is.na(hh_under5), 0, hh_under5)) %>% 
  ungroup()

# Clean KGC ---------------------------------------------------------------

# Open the fine resolution KGZ file
kgz_fine <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/kgz/kgc_small.rds") %>% select(LATNUM, LONGNUM, ClimateZ)

data_aim2 <- left_join(data_aim2, kgz_fine, by = c("LATNUM", "LONGNUM"))

# Categorize the Koppen-Geiger Climate Classification System into fine details
data_aim2 <- data_aim2 %>% mutate(kgc_fine = case_when(
  ClimateZ == "Af"  ~ "Tropical Rainforest",
  ClimateZ == "Am"  ~ "Tropical Monsoon",
  ClimateZ == "As"  ~ "Tropical Savanna, Dry Summer",
  ClimateZ == "Aw"  ~ "Tropical Savanna Dry Winter",
  ClimateZ == "BSh" ~ "Dry Semi-Arid Hot",
  ClimateZ == "BSk" ~ "Dry Semi-Arid Cold",
  ClimateZ == "BWh" ~ "Dry Arid Hot",
  ClimateZ == "BWk" ~ "Dry Arid Desert Cold",
  ClimateZ == "Cfa" ~ "Temperate No Dry Season Hot Summer",
  ClimateZ == "Cfb" ~ "Temperate No Dry Season Warm Summer",
  ClimateZ == "Csa" ~ "Temperate Dry Summer Hot Summer",
  ClimateZ == "Csb" ~ "Temperate Dry Summer Warm Summer",
  ClimateZ == "Cwa" ~ "Temperate Dry Winter Hot Summer",
  ClimateZ == "Cwb" ~ "Temperate Dry Winter Warm Summer",
  ClimateZ == "Cwc" ~ "Temperate Dry Winter Cold Summer",
  ClimateZ == "ET" ~ "Polar Tundra",
  ClimateZ == "Ocean" ~ NA_character_,
  TRUE ~ NA_character_))

# Categorize the Koppen-Geiger Climate Classification System into course details
data_aim2 <- data_aim2 %>% mutate(kgc_course = case_when(
  ClimateZ %in% c("Af", "Am", "As", "Aw") ~ "Tropical",
  ClimateZ %in% c("BSh", "BSk", "BWh", "BWk") ~ "Dry",
  ClimateZ %in% c("Cfa", "Cfb", "Csa", "Csb", "Cwa", "Cwb", "Cwc") ~ "Temperate",
  ClimateZ %in% c("ET", "Ocean") ~ "Other",
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

# Add in Region and LDC ---------------------------------------------------
africa_regions <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/africa_regions.rds") %>% 
                     mutate(Country = recode(Country,
                                             "Central African Republic"         = "CAR",
                                             "Democratic Republic of the Congo" = "Congo_Democratic_Republic",
                                             "Côte d'Ivoire"                    = "Cote_d'Ivoire",
                                             "Sao Tome and Principe"            = "Sao_Tome_and_Principe",
                                             "Sierra Leone"                     = "Sierra_Leone",
                                             "South Africa"                     = "South_Africa",
                                             "United Republic of Tanzania"      = "Tanzania",
                                             "Burkina Faso"                     = "Burkina_Faso")) %>%
                     mutate(LDC = if_else(LDC == TRUE, "less developed", "more developed")) %>%
                     rename("region" = "Region",
                            "developed" = "LDC",
                            "country" = "Country")

data_aim2 <- left_join(data_aim2, africa_regions, by = "country")

# Combine Middle and Southern Africa
data_aim2 <- data_aim2 %>% 
  mutate(region_combined = recode(region,
                                  "Middle Africa" = "Middle/Southern Africa",
                                  "Southern Africa" = "Middle/Southern Africa")) %>% 
  mutate(region_combined = factor(region_combined, levels = c("Western Africa", "Middle/Southern Africa", "Eastern Africa")))

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

# If the above categories are 0, but hv246 == 1, then set hv246 == 0
# Note that bees are purposely left out because they're excluded from analysis
data_aim2 <- data_aim2 %>%
  mutate(hv246 = case_when(is.na(hv246) ~ NA_real_,
                           hv246_bull_cow_total_raw == 0 & hv246_camel_total_raw == 0 & hv246_cattle_total_raw == 0 & hv246_chicken_poultry_total_raw == 0 & 
                           hv246_duck_total_raw == 0 & hv246_goat_total_raw == 0 & hv246_horse_donkey_total_raw == 0 & hv246_other_total_raw == 0 & 
                           hv246_pig_total_raw == 0 & hv246_rabbit_total_raw == 0 & hv246_rodent_total_raw == 0 & hv246_sheep_total_raw == 0 ~ 0, 
                           TRUE ~ hv246))

# Recatagorize
data_aim2 <- data_aim2 %>% 
                  mutate(hv246_bull_cow_cattle_total_cat = hv246_bull_cow_total_raw + hv246_cattle_total_raw) %>%
                  mutate(hv246_chicken_poultry_duck_total_cat = hv246_chicken_poultry_total_raw + hv246_duck_total_raw) %>% 
                  mutate(hv246_goat_sheep_total_cat = hv246_goat_total_raw + hv246_sheep_total_raw) %>% 
                  mutate(hv246_horse_donkey_total_cat = hv246_horse_donkey_total_raw) %>% 
                  mutate(hv246_pig_total_cat = hv246_pig_total_raw) %>% 
                  mutate(hv246_other_total_cat = hv246_other_total_raw + hv246_rabbit_total_raw +
                                                 hv246_rodent_total_raw + hv246_camel_total_raw) %>% 
                  mutate(hv246_ruminant_total_cat = hv246_bull_cow_cattle_total_cat + hv246_goat_sheep_total_cat) 

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
                mutate(sheep_present = case_when(!is.na(hv246_sheep_total_raw) & hv246_sheep_total_raw >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(ruminant_present = case_when(!is.na(hv246_ruminant_total_cat) & hv246_ruminant_total_cat >= 1 ~ 1, TRUE ~ 0))
 
# Presence / Absence of animal (aggregated)
data_aim2 <- data_aim2 %>%
                mutate(bull_cow_cattle_present = case_when(!is.na(hv246_bull_cow_cattle_total_cat) & hv246_bull_cow_cattle_total_cat >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(chicken_poultry_duck_present = case_when(!is.na(hv246_chicken_poultry_duck_total_cat) & hv246_chicken_poultry_duck_total_cat >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(goat_sheep_present = case_when(!is.na(hv246_goat_sheep_total_cat) & hv246_goat_sheep_total_cat >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(horse_donkey_present = case_when(!is.na(hv246_horse_donkey_total_cat) & hv246_horse_donkey_total_cat >= 1 ~ 1, TRUE ~ 0)) %>%
                mutate(pig_present = case_when(!is.na(hv246_pig_total_cat) & hv246_pig_total_cat >= 1 ~ 1, TRUE ~ 0)) %>% 
                mutate(other_present = case_when(!is.na(hv246_other_total_cat) & hv246_other_total_cat >= 1 ~ 1, TRUE ~ 0))

# Specify whether a person has been exposed to more than one animal
data_aim2 <- data_aim2 %>% mutate(animal_combo = 
                                    case_when(
                                       bull_cow_cattle_present + 
                                       chicken_poultry_duck_present +
                                       goat_sheep_present + 
                                       horse_donkey_present +
                                       other_present + 
                                       pig_present  >= 2 ~ 1, TRUE ~ 0)) 

# Number of categories exposed to
data_aim2 <- data_aim2 %>%
                mutate(animal_combo_number = bull_cow_cattle_present + 
                                             chicken_poultry_duck_present +
                                             goat_sheep_present + 
                                             horse_donkey_present +
                                             other_present + 
                                             pig_present) 

# Animal Presence Category
data_aim2 <- data_aim2 %>% 
  mutate(animal_singleonly = case_when(
    chicken_poultry_duck_present == 0 &
      bull_cow_cattle_present == 0 &
      goat_sheep_present == 0 &
      horse_donkey_present == 0 &
      pig_present == 0 &
      other_present == 0 &
      animal_combo == 0 ~ "none",
    chicken_poultry_duck_present == 1 &
      bull_cow_cattle_present == 0 &
      goat_sheep_present == 0 &
      horse_donkey_present == 0 &
      pig_present == 0 &
      other_present == 0 &
      animal_combo == 0 ~ "chicken/poultry/duck only",
    goat_sheep_present == 1 &
      bull_cow_cattle_present == 0 &
      chicken_poultry_duck_present == 0 &
      horse_donkey_present == 0 &
      pig_present == 0 &
      other_present == 0 &
      animal_combo == 0 ~ "goat/sheep only",
    bull_cow_cattle_present == 1 &
      goat_sheep_present == 0 &
      chicken_poultry_duck_present == 0 &
      horse_donkey_present == 0 &
      pig_present == 0 &
      other_present == 0 &
      animal_combo == 0 ~ "bull/cow/cattle only",
    horse_donkey_present == 1 &
      goat_sheep_present == 0 &
      chicken_poultry_duck_present == 0 &
      bull_cow_cattle_present == 0 &
      pig_present == 0 &
      other_present == 0 &
      animal_combo == 0 ~ "horse/donkey only",
    pig_present == 1 &
    horse_donkey_present == 0 &
      goat_sheep_present == 0 &
      chicken_poultry_duck_present == 0 &
      bull_cow_cattle_present == 0 &
      other_present == 0 &
      animal_combo == 0 ~ "pig only",
    animal_combo_number >= 2 ~ "multispecies",
    TRUE ~ "other"))

# Total animals
data_aim2 <- data_aim2 %>% mutate(animal_total = 
                                    rowSums(select(., c(
                                      hv246_bull_cow_cattle_total_cat,
                                      hv246_chicken_poultry_duck_total_cat,
                                      hv246_goat_sheep_total_cat, 
                                      hv246_horse_donkey_total_cat,
                                      hv246_pig_total_cat, 
                                      hv246_other_total_cat)), na.rm = TRUE))

# Create even cut points for the total animals variable
table(Hmisc::cut2(data_aim2$animal_total, g=8))
data_aim2 <- data_aim2 %>% 
                 mutate(animal_total_cut = cut(animal_total, breaks = c(0,3,7,13,25,Inf)))

# UG_06 had people who had animals, but their hv246 was set to 0
data_aim2 <- data_aim2 %>% 
               mutate(hv246 = ifelse(name_year == "UG_06" & is.na(hv246) & animal_total != 0, 1, hv246))


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


# Head of Household Education ---------------------------------------------

# data_aim2 <- data_aim2 %>%
#   rowwise() %>%
#   mutate(head_educ = {
#     # Find indices where hv101 equals 1
#     idx <- which(c_across(starts_with("hv101_")) == 1)
#     # If any found, get the corresponding hv106 value from the first match; otherwise, NA
#     if (length(idx) > 0) c_across(starts_with("hv106_"))[idx[1]] else NA_integer_
#   }) %>%
#   ungroup()

# Clean Unneeded Columns --------------------------------------------------

data_aim2 <- data_aim2 %>%  
  select(-matches("^(awf|ml|d4|fg|fy|g1|m[1-8]|s4|sdv|si|v[2-8])"))

# Output the datasets -----------------------------------------------------

# Save the full dataset
saveRDS(data_aim2, file = "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/data_aim2.rds")

# ## Save the urban dataset -------------------------------------------------
# urban <- data_aim2 %>% filter(URBAN_RURA == "U")
# saveRDS(urban, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/urban.rds")
# 
# # Household Level 
# urban_hh <- urban %>%
#             group_by(name_year) %>%
#             distinct(hhid, .keep_all = TRUE)
# 
# saveRDS(urban_hh, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/urban_hh.rds")
# 
# # Remove urban dataset
# rm(urban)
# gc()

## Save the rural dataset -------------------------------------------------
rural <- data_aim2 %>% filter(URBAN_RURA == "R")
saveRDS(rural, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/rural.rds")

# Household Level 
rural_hh <- rural %>%
            group_by(name_year) %>%
            distinct(hhid, .keep_all = TRUE)

saveRDS(rural_hh, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/rural_hh.rds")


# Descriptive Dataset -----------------------------------------------------
descriptive <- rural %>% filter(b8 <= 4) %>%    # Only those under 5
                         filter(b5 == 1)        # Only include those that are still alive

saveRDS(descriptive, file = "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/descriptive.rds")

# Under 5 with Animals ----------------------------------------------------
under5_animal <- rural %>% filter(b8 <= 4) %>%                        # Only those under 5
                           filter(hv246 == 1) %>%                     # Only those with Animals
                           filter(b5 == 1) %>%                        # Only include those that are still alive
                           filter(!between(LATNUM, -0.0001, 0.0001))  # Remove unrealistic GPS

saveRDS(under5_animal, file = "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/under5_animal.rds")

# Rural Under 5 Diarrhea with Animals -------------------------------------
under5_dia <- rural %>% filter(b8 <= 4) %>%                        # Only those under 5
                        filter(diarrhea_dichot == 1) %>%           # Only those that have diarrhea
                        filter(hv246 == 1) %>%                     # Only those with Animals
                        filter(b5 == 1) %>%                        # Only include those that are still alive
                        filter(!between(LATNUM, -0.0001, 0.0001))  # Remove unrealistic GPS

saveRDS(under5_dia, file = "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/under5_dia.rds")