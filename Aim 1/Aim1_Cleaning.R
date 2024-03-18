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
data_aim1 <- readRDS("~/data-mdavis65/steven_sola/4_HHlevel/hh_combined.Rdata")

# Clean HV007 (Year) ------------------------------------------------------

# Change the "Year 1" to 2001 (GA_00 dataset)
# Change the "Year 0" to 2000 (GA_00 dataset)
# Change the two digit years into four digits by adding 1900
data_aim1 <- data_aim1 %>% mutate(hv007 = case_when(
                             hv007 == 1   ~ 2001,
                             hv007 == 0   ~ 2000,
                             hv007 < 100  ~ hv007+1900,
                             TRUE ~ hv007))

# HV201 Source ------------------------------------------------------------

# Read in the file that lists all countries
source("countries.R") 

# Create a list to bind all the DFs together at the end.
codebook <- list()

# Start of for_loop
for (i in 1:nrow(countries)){
  
country <- countries$country[i]
name_year <- countries$name_year[i]

# Skip Nigeria 1990 because no codebook
if(name_year == "NG_90") next

# Set the working directory to the specific country  
setwd(paste0("~/data-mdavis65/steven_sola/", country,"/",name_year,"/",name_year,"_hh"))

# Read in survey data (Household member)
files <- list.files(getwd(), pattern="(\\.MAP|\\.map)", recursive = TRUE)

# Set the locale for the files (helps with reading in weird formatting)
locale <- locale(encoding = "latin1")

# Read in the codebook files
water_source <- read_csv(files, show_col_types = F, col_names = "HV201", locale = locale)

# Slice the codebook that was written in
# Each one has "Piped" in common at the start, but they end at the different points.
# Then separate out the result between the code and the hv201_source
# Finally, get rid of all the missing labels
water_source <- water_source %>% slice((grep("(PIPED|Piped|piped)", HV201)):(grep("(HV202|HV201a|HV204|HV205)", HV201)-1)) %>% 
                                 separate(HV201, c("code", "hv201_source"), sep = "\\s", extra = "merge") %>% 
                                 filter(!is.na(hv201_source))

# Get rid of some artifacts from the codebook
water_source$hv201_source <- str_remove(water_source$hv201_source, "\\?")
water_source$hv201_source <- str_remove(water_source$hv201_source, "\\{CG\\}")

# Further slice the codebook to focus on the main codes and not missings/other
# NG_15 is funny and doesn't have codes from Missing/Other
if(name_year != "NG_15") {
water_source <- water_source %>% slice(1:(grep("(Missing|applicable|NotAppl)", hv201_source))-1)
}

# Trim the white space in the hv201_source variable
water_source$hv201_source <- trimws(water_source$hv201_source)

# Add in the name_year variable
water_source$name_year <- name_year

# remove the strange characters from MZ_97 and CI_9899
if(name_year == "MZ_97") {
  water_source$hv201_source <- gsub("ÿû ","", water_source$hv201_source)
}

if(name_year == "CI_9899") {
  water_source$hv201_source <- gsub("ÿ","", water_source$hv201_source)
}

# Save all the df together to bind together
codebook[[i]] <- water_source

# Print the name that is finished
print(paste0(name_year, " has finished processing"))

# Remove everything except two objects to save on memory costs
rm(list = setdiff(ls(), c("countries", "codebook", "data_aim1")))
}

# bind all the DFs generated together
codebook <- do.call(rbind, codebook)

# Set the 96's in the codebook to "Other"
codebook <- codebook %>% mutate(
                hv201_source = if_else(hv201_source == 96, "Other", hv201_source))

# Clean HV201 Source ------------------------------------------------------

# Set the code in the codebook to numeric to match the full dataset
codebook$code <- as.numeric(codebook$code)

# Left join the hv201_sources in the codebook to the full dataset
data_aim1 <- data_aim1 %>% left_join(codebook, by = join_by(name_year, hv201 == code))

# Categorize the source of the water. 
data_aim1 <- data_aim1 %>% mutate(hv201_sourcecat = case_when(
                              str_detect(hv201_source, "(river|River)") ~ "River",
                              str_detect(hv201_source, "(Open|open|not|Unprotected|unprotected|fountain|Non|land|^Public water$)") ~ "Unprotected",
                              str_detect(hv201_source, "(Covered|covered|Protected|protected|Improved)") ~ "Protected",
                              str_detect(hv201_source, "(Piped|pipe|tap|Tap|faucet)") ~ "Piped",
                              str_detect(hv201_source, "(Borehole|Barehole|borehole|Borehl|stand|bore hole|Manual|Tube|Drilling)") ~ "Borehole",
                              str_detect(hv201_source, "(Piped|pipe|tap|Tap|house|courtyard)") ~ "Piped",
                              str_detect(hv201_source, "(Well|well)") ~ "Well",
                              str_detect(hv201_source, "(Spring|^Other spring$)") ~ "Spring",
                              str_detect(hv201_source, "(Sachet|sachet|Water bag|Bag water|plastic bag|Satchel)") ~ "Sachet",
                              str_detect(hv201_source, "(Lake|lake|Dam|Resevoir|Dugout|Canal|Gravity|road|Forage|cesspool|Irrigation|surface)") ~ "Surface water",
                              str_detect(hv201_source, "(Vendor|vendor|Purchased|Tanker|TANKER|tanker|Bicycle|Motorcycle|Cart|vender|station|merchant|seller|Arranged)") ~ "Vendor",
                              str_detect(hv201_source, "(rainwater|Rainwater|Rain-water|RAINWATER|rain water)") ~ "Rainwater",
                              str_detect(hv201_source, "(other|Other|OTHER)") ~ "Other",
                              str_detect(hv201_source, "(Neighbor|Neighbour|neighbor|Naighbor)") ~ "Piped",
                              TRUE ~ hv201_source))
            
# Categorize the source of the water. 
data_aim1 <- data_aim1 %>% mutate(hv201_improved = case_when(
                              hv201_sourcecat %in% c("Unprotected", "Vendor", "River", "Bottled water",
                                                     "Surface water", "Spring", "Sachet", "Well") ~ "Unimproved",
                              hv201_sourcecat %in% c("Protected", "Borehole", "Piped", "Rainwater") ~ "Improved",
                              TRUE ~ "UNKNOWN"))
      
# Check the coding
data_aim1 %>% select(hv201_source, hv201_sourcecat, hv201_improved) %>% 
              arrange(hv201_source, hv201_sourcecat, hv201_improved) %>% 
              distinct(hv201_source, hv201_sourcecat, hv201_improved, .keep_all = FALSE)

# HV236 Person Collecting -------------------------------------------------

# Get the name_years of the surveys that collected hv236 data
to_extract <- data_aim1 %>% 
              filter(!is.na(hv236)) %>% 
              distinct(name_year)

# Semi join it to the list of countries to get the country names easily
to_extract <- semi_join(countries, to_extract)

# Create a list to bind all the DFs together at the end.
codebook <- list()

# Start of for_loop
for (i in 1:nrow(to_extract)){

country <- to_extract$country[i]
name_year <- to_extract$name_year[i]

# Set the working directory to the specific country  
setwd(paste0("~/data-mdavis65/steven_sola/", country,"/",name_year,"/",name_year,"_hh"))

# Read in survey data (Household member)
files <- list.files(getwd(), pattern="(\\.MAP|\\.map)", recursive = TRUE)

# Set the locale for the files (helps with reading in weird formatting)
locale <- locale(encoding = "latin1")

# Read in the codebook files
person_collect <- read_csv(files, show_col_types = F, col_names = "HV236", locale = locale)

# Slice the new csv from the codebook based on common characteristics
# Finally, get rid of all the missing labels
person_collect <- person_collect %>% slice((grep("fetching", HV236)+1):(grep("HV237", HV236)-1)) %>% 
                        separate(HV236, c("code", "hv236_person"), sep = "\\s", extra = "merge") %>% 
                        filter(!is.na(hv236_person))

# There's a "Missing" value that is a remnant...get rid of it.                        
person_collect <- person_collect %>% slice(1:(grep("Missing", hv236_person)-1))

# Trim the white space in the hv236_person variable
person_collect$hv236_person <- trimws(person_collect$hv236_person)

# Add in the name_year variable
person_collect$name_year <- name_year

# Save all the df together to bind together
codebook[[i]] <- person_collect

# Print the name that is finished
print(paste0(name_year, " has finished processing"))

# Remove everything except two objects to save on memory costs
rm(list = setdiff(ls(), c("countries", "codebook", "data_aim1", "to_extract")))

}

# bind all the DFs generated together
codebook <- do.call(rbind, codebook)

# Set the code in the codebook to numeric to match the full dataset
codebook$code <- as.numeric(codebook$code)

# Left join the hv201_sources in the codebook to the full dataset
data_aim1 <- data_aim1 %>% left_join(codebook, by = join_by(name_year, hv236 == code))


# Clean the Person Carrying Water Variable --------------------------------
tabyl(data_aim1$hv236_person)

data_aim1 <- data_aim1 %>% mutate(hv236_person = case_when(
                 hv236_person %in% c("Other", "other", "Don't know",
                                     "Door to door water seller", "Water vendor") ~ "Unknown/Other",
                 hv236_person %in% c("All members", "Any household member") ~ "Any member",
                 hv236_person == "Female and male children equally" ~ "Female and male child under 15 years old",
                 TRUE ~ hv236_person))

# Clean Water Walk Variable -----------------------------------------------
data_aim1 <- data_aim1 %>% mutate(hv204 = case_when(
                           hv204 >= 996 ~ NA_integer_,
                           TRUE ~ hv204))

# Clean Koppen-Geiger -----------------------------------------------------

# Categorize the Koppen-Geiger Climate Classification System into fine details
data_aim1 <- data_aim1 %>% mutate(kgc_fine = case_when(
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
data_aim1 <- data_aim1 %>% mutate(kgc_course = case_when(
                              kgc %in% c("Af", "Am", "As", "Aw") ~ "Tropical",
                              kgc %in% c("BSh", "BSk", "BWh", "BWk") ~ "Dry",
                              kgc %in% c("Cfa", "Cfb", "Csa", "Cwa", "Cwb") ~ "Temperate",
                              kgc == "Climate Zone info missing" ~ NA_character_,
                              TRUE ~ NA_character_))


# Coalesce the wlthind5 and hv270 variables -------------------------------

data_aim1 <- data_aim1 %>%
             mutate(hv270 = coalesce(wlthind5, hv270))


# Output the datasets -----------------------------------------------------

# Separate the data between Rural and Urban
cat("There are", nrow(data_aim1 %>% filter(URBAN_RURA == "R")), "rural households")
cat("There are", nrow(data_aim1 %>% filter(URBAN_RURA == "U")), "urban households")
cat("There are", nrow(data_aim1 %>% filter(is.na(URBAN_RURA))), "households with missing U/R data")

# Save the full dataset
saveRDS(data_aim1, file = "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/data_aim1.Rdata")

# Save the rural dataset
rural <- data_aim1 %>% filter(URBAN_RURA == "R")
saveRDS(rural, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/rural.Rdata")

# Save the urban dataset
urban <- data_aim1 %>% filter(URBAN_RURA == "U")
saveRDS(urban, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/urban.Rdata")