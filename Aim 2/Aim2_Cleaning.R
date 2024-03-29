# Header ------------------------------------------------------------------

# Set the location
#location <- "personal"
location <- "HPC"

# Load Libraries
library(tidyverse)
library(haven)
library(lubridate)
library(stringr)
library(sf)
library(readxl)
library(data.table)
library(ethiopianDate)

countries <- read_excel("Country_Index_HPC.xlsx")

for(entry in 1:nrow(countries)) {

# Specify the files to work on
country <- countries$country[[entry]]
name_year <- countries$name_year[[entry]]

# Start message
print(paste0(name_year, " has started processing"))

if (location == "personal") {
  # Set Working Directory (Personal)
  setwd(paste0("C:/Users/Steven/OneDrive - Johns Hopkins/Dissertation/Dissertation/Data/Aim 1/",
               country,"/",name_year))
}

if (location == "HPC") { 
  # Set Working Directory (HPC)
  setwd(paste0("~/data-mdavis65/steven_sola/",country,"/",name_year))
}

# Load Files --------------------------------------------------------------

#Read in survey data (Household member)
files <- list.files(pattern=c(".DTA|\\.shp$"), recursive = TRUE)
files

# Filter the list for the Household Member level, save to environment
hhmem <- str_subset(files, "hhmember") %>% read_dta(., encoding = "latin1")

# Filter the list for the Household level, save to environment
hh <- str_subset(files, "HR") %>% read_dta(., encoding = "latin1")

# Filter the list for the Child level, save to environment
if (any(grepl("child", unlist(files)))) {
  child <- str_subset(files, "child") %>% read_dta(., encoding = "latin1")
}

# Filter the list for the Birth level, save to environment
if (any(grepl("birth", unlist(files)))) {
  birth <- str_subset(files, "birth") %>% read_dta(., encoding = "latin1")
}

# Filter the list for the Height/Weight level, save to environment
if (any(grepl("heightweight", unlist(files)))) {
  hw <- str_subset(files, "heightweight") %>% read_dta(., encoding = "latin1")
}

# Filter the list for the wealth, save to environment
if (any(grepl("wealth", unlist(files)))) {
  wealth <- str_subset(files, "wealth") %>% read_dta(., encoding = "latin1")
}

# Filter the list for the Spatial, save to environment
if (any(grepl("gps", unlist(files)))) {
  spatial <- str_subset(files, "gps") %>% st_read()
}

# Merging -----------------------------------------------------------------

# Remove empty rows of HH dataset
hh <- hh[,colSums(is.na(hh))<nrow(hh)]


# Preferable to use birth since it has more data than child
# However, there are some datasets that don't have birth, but have child
# Use birth if available, else use child
if (exists("birth")) {
  offspring <- birth
  cat('"Birth" is being used')
} else if (exists("child")) {
  offspring <- child
  cat('"Child" is being used')
}


if (name_year == "CM_91") {

# Trim the whitespace of the HHID and Case ID variables
hh$hhid_clean <- trimws(hh$hhid)
offspring$caseid_clean <- trimws(offspring$caseid)

# Manually change a merged caseid number
offspring <- offspring %>% 
  mutate(caseid_clean = ifelse(caseid_clean == "99132 1  4", "99 132 1 4", caseid_clean)) %>% 
  mutate(caseid_clean = ifelse(caseid_clean == "99132 1  2", "99 132 1 2", caseid_clean)) %>% 
  mutate(caseid_clean = ifelse(caseid_clean == "104 8727  2", "104 872 7  2", caseid_clean))%>% 
  mutate(caseid_clean = ifelse(caseid_clean == "104 8811  2", "104 881 1  2", caseid_clean))

# Remove the numbers after the last space (twice)
offspring$caseid_clean <- gsub(" [^ ]*$", "", offspring$caseid_clean) %>% trimws()
# offspring$caseid_clean <- gsub(" [^ ]*$", "", offspring$caseid_clean) %>% trimws()

# Separate out 44 and 73 households that were merged
hh <- hh %>% 
  mutate(hhid_clean = ifelse(hhid_clean == "4440010", "44 400 1", hhid_clean)) %>% 
  mutate(hhid_clean = ifelse(hhid_clean == "7312610", "73 126 1", hhid_clean)) %>% 
  mutate(hhid_clean = ifelse(hhid_clean == "104 8613", "104 86 1", hhid_clean)) %>% 
  mutate(hhid_clean = ifelse(hhid_clean == "104 8639", "104 86 3", hhid_clean)) %>% 
  mutate(hhid_clean = ifelse(hhid_clean == "104 8715", "104 87 1", hhid_clean)) %>% 
  mutate(hhid_clean = ifelse(hhid_clean == "104 8727", "104 87 2", hhid_clean)) %>% 
  mutate(hhid_clean = ifelse(hhid_clean == "104 8811", "104 88 1", hhid_clean)) %>% 
  mutate(hhid_clean = ifelse(hhid_clean == "46 1812", "46 18 1", hhid_clean)) %>% 
  mutate(hhid_clean = ifelse(hhid_clean == "47  113", "47 11 3", hhid_clean)) %>% 
  mutate(hhid_clean = ifelse(hhid_clean == "80 4010", "80 40 1", hhid_clean)) %>% 
  mutate(hhid_clean = ifelse(hhid_clean == "88 5611", "88 561 1", hhid_clean)) %>% 
  mutate(hhid_clean = ifelse(hhid_clean == "113 3030", "113 30 3", hhid_clean)) %>% 
  mutate(hhid_clean = ifelse(hhid_clean == "122 6711", "122 67 1", hhid_clean)) %>% 
  mutate(hhid_clean = ifelse(hhid_clean == "142 4412", "142 44 1", hhid_clean)) 

# Remove the numbers after the last space
# hh$hhid_clean <- gsub(" [^ ]*$", "", hh$hhid_clean) %>% trimws()

# Detect the number of digits in a row,
# If 4 digits in a row, add a space after the first digit
# If 5 digits in a row, add a space after the second digit
# If 6 digits in a row, add a space after the third digit
# If doesn't meet any conditions, original HHID

hh <- hh %>% 
  mutate(
    hhid_clean = case_when(
      str_detect(hhid_clean, "^\\d{4,4}$") ~ gsub(hhid_clean, pattern = "(.{1})(.*)", replacement = "\\1 \\2"),
      str_detect(hhid_clean, "^\\d{5,5}$") ~ gsub(hhid_clean, pattern = "(.{2})(.*)", replacement = "\\1 \\2"),
      str_detect(hhid_clean, "^\\d{6,6}$") ~ gsub(hhid_clean, pattern = "(.{3})(.*)", replacement = "\\1 \\2"),
      TRUE ~ hhid_clean
    )
  )

hh <- hh %>% 
  separate(hhid_clean, into = c("hhid_clean", "hv002"), sep = "\\s+") %>% 
  relocate(hv002, .after = hv001) %>%
  select(-hhid_clean)

hh$hv002 <- as.numeric(hh$hv002)

class(hh$hv002)
}





# Merge hhmem data to hh data
hh <- hh %>% arrange(hhid)
hhmem <- hhmem %>% arrange(hhid)
hhmem_join <- left_join(hhmem, hh, keep = FALSE)

# Sort, then merge births and Height / Weight Data
# Use HWCASEID and HWLINE, from the Height and Weight file, 
# with CASEID and BIDX, from the Births Recode file to merge it with the Births’ data
if (exists("heightweight")) {
  birth <- birth %>% arrange(caseid, bidx)
  hw <- hw %>% arrange(hwhhid, hwline)
  birth <- left_join(birth, hw, by = c("caseid" = "hwhhid", "bidx" = "hwline"))
}





# Merge Wealth to hhmem
if (exists("wealth")) {
wealth <- wealth %>% arrange(whhid)
hhmem_join <- hhmem_join %>% arrange(hhid)
hhmem_join <- left_join(hhmem_join, wealth, by = c("hhid" = "whhid"))
}

# Merge all the datasets
hhmem_join <- hhmem_join %>% arrange(hv001, hv002, hvidx)
offspring <- offspring %>% arrange(v001, v002, v003)
full <- left_join(hhmem_join, offspring, by = c("hv001" = "v001", "hv002" = "v002", "hvidx" = "v003"))

# ERROR HANDLING
# Stop if the join wasn't preformed as expected
stopifnot(nrow(hhmem_join) + nrow(offspring) - 
            nrow(semi_join(hhmem_join, offspring, by = c("hv001" = "v001", "hv002" = "v002", "hvidx" = "v003"))) == 
            nrow(full))

if (exists("spatial")) {
  # join spatial to data
  full <- left_join(full, spatial, by = c("hv001" = "DHSCLUST"))
}

# Dates -------------------------------------------------------------------

# Month of interview
# CMC = Number of months since Jan 1 1900. Jan 1 1900 is CMC = 1
full$monthinterview <- as.Date("1900-01-01")
full$monthinterview <- full$monthinterview %m+% months(full$hv008-1)

# Add Date to the interview month
full$dateinterview <- full$monthinterview + (full$hv016-1)

# This section will run if the country is Ethiopia
# This will convert the dates from the Ethiopian calendar to Gregorian calendar
if (country == "Ethiopia") {
  
  # Create new variables so that R doesn't get confused with lubridate functions.
  full <- full %>% 
    mutate(year_eth = hv007, month_eth = lubridate::month(monthinterview), day_eth = day(dateinterview))
  
  # Create Gregorian date based off Ethiopian date
  full <- full %>% 
    mutate(dateinterview_greg = ethiopianToGregorian(year = as.numeric(.$year_eth),
                                                     month = as.numeric(.$month_eth),
                                                     date = as.numeric(.$day_eth)))
  
  # Ensure the date is in Date format
  full$dateinterview <- as.Date(full$dateinterview_greg)
  
  # Push the new dates to the old date variables
  full$hv007 <- year(full$dateinterview)
  full$hv006 <- lubridate::month(full$dateinterview)
  full$hv016 <- day(full$dateinterview)
  print("Dates were converted from the Ethiopian to the Gregorian Calendar")
}

# Insert Weather ----------------------------------------------------------

if (exists("spatial")) {

# Set working directory for the weather data
setwd("~/data-mdavis65/steven_sola/2_Weather_Processed/Rdata")

# Import the 60-day weather into the dataset
load(paste0(name_year, "_weatherfinal"))

# Remove the unneeded columns
weather_final <- weather_final %>% 
                    select(-c(tz_name, date_time, zone, is_dst,
                              utc_offset_h, utc_offset))

# Check for any missings and stop if any are found
stopifnot(sum(is.na(weather_final)) == 0)

# Calculate the Weather Extremes --------------------------------------------------

# Variables from ERA5-Land
accum_var <- c("e", "sro", "ssro", "tp")
instant_var <- c("t2m", "skt", "d2m", "lai_lv", "lai_hv")

# For loop for finding the total for the past 1 week, 2 weeks, 4 weeks, and 8 weeks
# As well as the 90th, 95th, 99th, and 99.9th percentile
for (weather_var_accum in accum_var) {
  weather_final <- weather_final %>%  
    rowwise() %>%
    mutate(!!paste0(weather_var_accum,"_totalminus7"):=sum(across(num_range(paste0(weather_var_accum,"_"), 1:7)))) %>%
    mutate(!!paste0(weather_var_accum,"_totalminus14"):=sum(across(num_range(paste0(weather_var_accum,"_"), 1:14)))) %>%
    mutate(!!paste0(weather_var_accum,"_totalminus30"):=sum(across(num_range(paste0(weather_var_accum,"_"), 1:30)))) %>%
    mutate(!!paste0(weather_var_accum,"_totalminus60"):=sum(across(num_range(paste0(weather_var_accum,"_"), 1:60)))) %>% 
    mutate(!!paste0(weather_var_accum,"_totalminus7_90"):=.data[[!!paste0(weather_var_accum,"_totalminus7")]] * 0.9) %>% 
    mutate(!!paste0(weather_var_accum,"_totalminus7_95"):=.data[[!!paste0(weather_var_accum,"_totalminus7")]] * 0.95) %>% 
    mutate(!!paste0(weather_var_accum,"_totalminus7_99"):=.data[[!!paste0(weather_var_accum,"_totalminus7")]] * 0.99) %>% 
    mutate(!!paste0(weather_var_accum,"_totalminus7_999"):=.data[[!!paste0(weather_var_accum,"_totalminus7")]] * 0.999) %>% 
    mutate(!!paste0(weather_var_accum,"_totalminus14_90"):=.data[[!!paste0(weather_var_accum,"_totalminus14")]] * 0.9) %>% 
    mutate(!!paste0(weather_var_accum,"_totalminus14_95"):=.data[[!!paste0(weather_var_accum,"_totalminus14")]] * 0.95) %>% 
    mutate(!!paste0(weather_var_accum,"_totalminus14_99"):=.data[[!!paste0(weather_var_accum,"_totalminus14")]] * 0.99) %>% 
    mutate(!!paste0(weather_var_accum,"_totalminus14_999"):=.data[[!!paste0(weather_var_accum,"_totalminus14")]] * 0.999) %>% 
    mutate(!!paste0(weather_var_accum,"_totalminus30_90"):=.data[[!!paste0(weather_var_accum,"_totalminus30")]] * 0.9) %>% 
    mutate(!!paste0(weather_var_accum,"_totalminus30_95"):=.data[[!!paste0(weather_var_accum,"_totalminus30")]] * 0.95) %>% 
    mutate(!!paste0(weather_var_accum,"_totalminus30_99"):=.data[[!!paste0(weather_var_accum,"_totalminus30")]] * 0.99) %>% 
    mutate(!!paste0(weather_var_accum,"_totalminus30_999"):=.data[[!!paste0(weather_var_accum,"_totalminus30")]] * 0.999)%>%
    mutate(!!paste0(weather_var_accum,"_totalminus60_90"):=.data[[!!paste0(weather_var_accum,"_totalminus60")]] * 0.9) %>% 
    mutate(!!paste0(weather_var_accum,"_totalminus60_95"):=.data[[!!paste0(weather_var_accum,"_totalminus60")]] * 0.95) %>% 
    mutate(!!paste0(weather_var_accum,"_totalminus60_99"):=.data[[!!paste0(weather_var_accum,"_totalminus60")]] * 0.99) %>% 
    mutate(!!paste0(weather_var_accum,"_totalminus60_999"):=.data[[!!paste0(weather_var_accum,"_totalminus60")]] * 0.999)
}

for (weather_var_instant in instant_var) {
  weather_final <- weather_final %>%  
    rowwise() %>%
    mutate(!!paste0(weather_var_instant,"_avgminus7"):=(sum(across(num_range(paste0(weather_var_instant,"_"), 1:7))))/7) %>%
    mutate(!!paste0(weather_var_instant,"_avgminus14"):=(sum(across(num_range(paste0(weather_var_instant,"_"), 1:14))))/14) %>%
    mutate(!!paste0(weather_var_instant,"_avgminus30"):=(sum(across(num_range(paste0(weather_var_instant,"_"), 1:30))))/30) %>%
    mutate(!!paste0(weather_var_instant,"_avgminus60"):=(sum(across(num_range(paste0(weather_var_instant,"_"), 1:60))))/60) %>% 
    mutate(!!paste0(weather_var_instant,"_avgminus7_90"):=.data[[!!paste0(weather_var_instant,"_avgminus7")]] * 0.9) %>% 
    mutate(!!paste0(weather_var_instant,"_avgminus7_95"):=.data[[!!paste0(weather_var_instant,"_avgminus7")]] * 0.95) %>% 
    mutate(!!paste0(weather_var_instant,"_avgminus7_99"):=.data[[!!paste0(weather_var_instant,"_avgminus7")]] * 0.99) %>% 
    mutate(!!paste0(weather_var_instant,"_avgminus7_999"):=.data[[!!paste0(weather_var_instant,"_avgminus7")]] * 0.999) %>% 
    mutate(!!paste0(weather_var_instant,"_avgminus14_90"):=.data[[!!paste0(weather_var_instant,"_avgminus14")]] * 0.9) %>% 
    mutate(!!paste0(weather_var_instant,"_avgminus14_95"):=.data[[!!paste0(weather_var_instant,"_avgminus14")]] * 0.95) %>% 
    mutate(!!paste0(weather_var_instant,"_avgminus14_99"):=.data[[!!paste0(weather_var_instant,"_avgminus14")]] * 0.99) %>% 
    mutate(!!paste0(weather_var_instant,"_avgminus14_999"):=.data[[!!paste0(weather_var_instant,"_avgminus14")]] * 0.999) %>% 
    mutate(!!paste0(weather_var_instant,"_avgminus30_90"):=.data[[!!paste0(weather_var_instant,"_avgminus30")]] * 0.9) %>% 
    mutate(!!paste0(weather_var_instant,"_avgminus30_95"):=.data[[!!paste0(weather_var_instant,"_avgminus30")]] * 0.95) %>% 
    mutate(!!paste0(weather_var_instant,"_avgminus30_99"):=.data[[!!paste0(weather_var_instant,"_avgminus30")]] * 0.99) %>% 
    mutate(!!paste0(weather_var_instant,"_avgminus30_999"):=.data[[!!paste0(weather_var_instant,"_avgminus30")]] * 0.999)%>%
    mutate(!!paste0(weather_var_instant,"_avgminus60_90"):=.data[[!!paste0(weather_var_instant,"_avgminus60")]] * 0.9) %>% 
    mutate(!!paste0(weather_var_instant,"_avgminus60_95"):=.data[[!!paste0(weather_var_instant,"_avgminus60")]] * 0.95) %>% 
    mutate(!!paste0(weather_var_instant,"_avgminus60_99"):=.data[[!!paste0(weather_var_instant,"_avgminus60")]] * 0.99) %>% 
    mutate(!!paste0(weather_var_instant,"_avgminus60_999"):=.data[[!!paste0(weather_var_instant,"_avgminus60")]] * 0.999)
}


# Merge Weather and Survey ------------------------------------------------

# Join the weather data to the full dataset
weather_final <- weather_final %>% arrange(LATNUM, LONGNUM, hv001, dateinterview)
full <- full %>% arrange(LATNUM, LONGNUM, hv001, dateinterview)
full <- left_join(full, weather_final, by = c("LATNUM", "LONGNUM", "hv001", "dateinterview"))

}

# Add in the name_year column, relocate to the beginning
full$name_year <- name_year

full <- full %>% relocate(name_year, .before = hhid)

# Drop all the HV1xx variables
full <- full %>% select(-contains(c("HV1", "hvidx", "hml", "hb", "idx", "sh", "ha", "hc", "geometry", "sb")))

# Write the final dataset to the main Senegal folder
fwrite(full, file = paste0("~/data-mdavis65/steven_sola/3_Survey_Weather/CSV/", name_year,"_weatherfinal.csv"))
fwrite(full, file = paste0("~/scr4-mdavis65/ssola1/Survey_Weather/CSV/", name_year, "_weatherfinal.csv"))

save(full, file= paste0("~/data-mdavis65/steven_sola/3_Survey_Weather/Rdata/", name_year, "_weatherfinal"))
save(full, file= paste0("~/scr4-mdavis65/ssola1/Survey_Weather/Rdata/", name_year, "_weatherfinal"))

# Finish message
print(paste0(name_year, " has finished processing"))

rm (list=setdiff(ls(), c("countries", "location")))

}