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

countries <- read_excel("Country_Index_HPC.xlsx") 

# %>% filter(name_year == "NG_15")

codebook <- list()

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

water_source <- water_source %>% slice((grep("(PIPED|Piped|piped)", HV201)):(grep("(HV202|HV201a|HV204|HV205)", HV201)-1))

# water_source$HV201 <- str_replace_all(water_source$HV201, "\\(m\\) ", "")
# water_source$HV201 <- str_replace_all(water_source$HV201, "\\(na\\)", "Missing")

water_source <- water_source %>% separate(HV201, c("code", "label"), sep = "\\s", extra = "merge")

water_source <- water_source %>% filter(!is.na(label))

if(name_year != "NG_15") {
water_source <- water_source %>% slice(1:(grep("(Missing|applicable|NotAppl)", label))-1)
}

# Add in the name_year variable
water_source$name_year <- name_year

# remove the strange characters from MZ_97 and CI_9899
if(name_year == "MZ_97") {
  water_source$label <- gsub("ÿû ","", water_source$label)
}

#CI_9899 is messed up, have to manually enter from codebook
if(name_year == "CI_9899") {
  water_source$label <- gsub("ÿ","", water_source$label)
}

# Save all the df together to bind together
codebook[[i]] <- water_source

print(water_source)

rm(list = setdiff(ls(), c("countries", "codebook")))

}

codebook <- do.call(rbind, codebook)

codebook$label <- trimws(codebook$label)


# read in the full database
setwd("./../../../4_HHlevel")

full <- readRDS("hh_combined.Rdata")

# join the codebook to the full dataset
partial <- full %>% select(name_year, hv201)

codebook$code <- as.numeric(codebook$code)

partial_join <- partial %>% left_join(codebook, by = join_by(name_year, hv201 == code))

partial_join <- partial_join %>% 
  mutate(label = if_else(hv201 == 96, "Other", label))

lala <- partial_join %>% distinct(name_year, hv201, label)



lala2 <- partial_join %>% filter(is.na(label)) %>% distinct(name_year, hv201, label)



data <- partial_join


# Read in the sample data
data <- readRDS("hh_combined_sample.Rdata")




# Read in the previously saved df
source <- readRDS("data_hv201source.Rdata")

# Remove these two artifacts from the codebook
data$hv201_source <- gsub(" \\{CG\\}", "", data$hv201_source)
data$hv201_source <- gsub(" \\?", "", data$hv201_source)

# Categorize the source of the water. 
data <- data %>% 
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
                                   if_else(str_detect(hv201_source, "(rainwater|Rainwater|Rain-water|RAINWATER)"), "Rainwater",
                                   if_else(str_detect(hv201_source, "(other|Other|OTHER)"), "Other",
                                   if_else(str_detect(hv201_source, "(Neighbor|Neighbour|neighbor|Naighbor)"), "Piped", hv201_source)))))))))))))))

# Categorize the source of the water. 
data <- data %>%
          mutate(hv201_improved = case_when(
                      hv201_sourcecat %in% c("Unprotected", "Vendor", "River", "Bottled water",
                                             "Surface water", "Spring", "Sachet", "Well") ~ "Unimproved",
                      hv201_sourcecat %in% c("Protected", "Borehole", "Piped", "Rainwater") ~ "Improved",
                      TRUE ~ "UNKNOWN"))

# Check the coding
data %>% select(hv201_source, hv201_sourcecat, hv201_improved) %>% arrange(hv201_source, hv201_sourcecat, hv201_improved) %>% 
         distinct(hv201_source, hv201_sourcecat, hv201_improved, .keep_all = FALSE)

# Separate the data between Rural and Urban
cat("There are", nrow(data %>% filter(URBAN_RURA == "R")), "rural households")
cat("There are", nrow(data %>% filter(URBAN_RURA == "U")), "urban households")
cat("There are", nrow(data %>% filter(is.na(URBAN_RURA))), "households with missing U/R data")

rural <- data %>% filter(URBAN_RURA == "R")
saveRDS(rural, "~/data-mdavis65/steven_sola/4_HHlevel/rural.Rdata")

urban <- data %>% filter(URBAN_RURA == "U")
saveRDS(urban, "~/data-mdavis65/steven_sola/4_HHlevel/urban.Rdata")

