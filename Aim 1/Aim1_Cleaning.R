# Header ------------------------------------------------------------------

# Load Libraries
library(tidyverse)
library(readr)
library(readxl)

# Set the location
#location <- "personal"
location <- "HPC"

# Load Libraries
library(tidyverse)

if (location == "personal") {
  # Set Working Directory (Personal)
  setwd("C:/Users/Steven/OneDrive - Johns Hopkins/Dissertation/Dissertation/Data")
}

if (location == "HPC") { 
  # Set Working Directory (HPC)
  setwd("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH")
}


# HV201 Source ------------------------------------------------------------

# Read in the file that lists all countries
countries <- read_excel("Country_Index_HPC.xlsx") 

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

# Remove everything except two objects to save on memory costs
rm(list = setdiff(ls(), c("countries", "codebook")))
}

# bind all the DFs generated together
codebook <- do.call(rbind, codebook)

# Set the 96's in the codebook to "Other"
codebook <- codebook %>% mutate(
                hv201_source = if_else(hv201_source == 96, "Other", hv201_source))

# Read in the full dataset
data_aim1 <- readRDS("~/data-mdavis65/steven_sola/4_HHlevel/hh_combined.Rdata")

# Set the code in the codebook to numeric to match the full dataset
codebook$code <- as.numeric(codebook$code)

# Left join the hv201_sources in the codebook to the full dataset
data_aim1 <- data_aim1 %>% left_join(codebook, by = join_by(name_year, hv201 == code))

# Categorize the source of the water. 
data_aim1 <- data_aim1 %>% 
                mutate(hv201_sourcecat = if_else(str_detect(hv201_source, "(river|River)"), "River", 
                                         if_else(str_detect(hv201_source, "(Open|open|not|Unprotected|unprotected|fountain|Non|land|^Public water$)"), "Unprotected",
                                         if_else(str_detect(hv201_source, "(Covered|covered|Protected|protected|Improved)"), "Protected",
                                         if_else(str_detect(hv201_source, "(Piped|pipe|tap|Tap|faucet)"), "Piped",
                                         if_else(str_detect(hv201_source, "(Borehole|Barehole|borehole|Borehl|stand|bore hole|Manual|Tube|Drilling)"), "Borehole",
                                         if_else(str_detect(hv201_source, "(Piped|pipe|tap|Tap|house|courtyard)"), "Piped",
                                         if_else(str_detect(hv201_source, "(Well|well)"), "Well",
                                         if_else(str_detect(hv201_source, "(Spring|^Other spring$)"), "Spring",
                                         if_else(str_detect(hv201_source, "(Sachet|sachet|Water bag|Bag water|plastic bag|Satchel)"), "Sachet",
                                         if_else(str_detect(hv201_source, "(Lake|lake|Dam|Resevoir|Dugout|Canal|Gravity|road|Forage|cesspool|Irrigation|surface)"), "Surface water",
                                         if_else(str_detect(hv201_source, "(Vendor|vendor|Purchased|Tanker|TANKER|tanker|Bicycle|Motorcycle|Cart|vender|station|merchant|seller|Arranged)"), "Vendor",
                                         if_else(str_detect(hv201_source, "(rainwater|Rainwater|Rain-water|RAINWATER|rain water)"), "Rainwater",
                                         if_else(str_detect(hv201_source, "(other|Other|OTHER)"), "Other",
                                         if_else(str_detect(hv201_source, "(Neighbor|Neighbour|neighbor|Naighbor)"), "Piped", hv201_source)))))))))))))))
            
# Categorize the source of the water. 
data_aim1 <- data_aim1 %>%
                 mutate(hv201_improved = case_when(
                            hv201_sourcecat %in% c("Unprotected", "Vendor", "River", "Bottled water",
                                                   "Surface water", "Spring", "Sachet", "Well") ~ "Unimproved",
                            hv201_sourcecat %in% c("Protected", "Borehole", "Piped", "Rainwater") ~ "Improved",
                            TRUE ~ "UNKNOWN"))

# Check the coding
data_aim1 %>% select(hv201_source, hv201_sourcecat, hv201_improved) %>% arrange(hv201_source, hv201_sourcecat, hv201_improved) %>% 
         distinct(hv201_source, hv201_sourcecat, hv201_improved, .keep_all = FALSE)

# Separate the data between Rural and Urban
cat("There are", nrow(data_aim1 %>% filter(URBAN_RURA == "R")), "rural households")
cat("There are", nrow(data_aim1 %>% filter(URBAN_RURA == "U")), "urban households")
cat("There are", nrow(data_aim1 %>% filter(is.na(URBAN_RURA))), "households with missing U/R data")

# Save the full dataset
saveRDS(data_aim1, file = "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/data_aim1wsource.Rdata")

# Save the rural dataset
rural <- data_aim1 %>% filter(URBAN_RURA == "R")
saveRDS(rural, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/rural.Rdata")

# Save the urban dataset
urban <- data_aim1 %>% filter(URBAN_RURA == "U")
saveRDS(urban, "~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/urban.Rdata")