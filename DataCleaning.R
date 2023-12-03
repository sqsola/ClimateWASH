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

# Specify the files to work on
country <- "Uganda"
name_year <- "UG_1819"

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
  hw <- str_subset(files, "wealth") %>% read_dta(., encoding = "latin1")
}

# Filter the list for the Spatial, save to environment
if (any(grepl("gps", unlist(files)))) {
spatial <- str_subset(files, "gps") %>% st_read()
}

# Merging -----------------------------------------------------------------

# Remove empty rows of HH dataset
hh <- hh[,colSums(is.na(hh))<nrow(hh)]

# Merge hhmem data to hh data
hhmem_join <- merge(hhmem, hh)

if (exists("heightweight")) {
# Sort, then merge births and Height / Weight Data
# Use HWCASEID and HWLINE, from the Height and Weight file, 
# with CASEID and BIDX, from the Births Recode file to merge it with the Births’ data
birth <- birth %>% arrange(caseid, bidx)
hw <- hw %>% arrange(hwhhid, hwline)
birth <- left_join(birth, hw, by = c("caseid" = "hwhhid", "bidx" = "hwline"))
}

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
    mutate(year_eth = hv007, month_eth = month(monthinterview), day_eth = day(dateinterview))
  
  # Create Gregorian date based off Ethiopian date
  full <- full %>% 
    mutate(dateinterview_greg = ethiopianToGregorian(year = as.numeric(.$year_eth),
                                                     month = as.numeric(.$month_eth),
                                                     date = as.numeric(.$day_eth)))
  
  # Ensure the date is in Date format
  full$dateinterview <- as.Date(full$dateinterview_greg)
  
  # Push the new dates to the old date variables
  full$hv007 <- year(full$dateinterview)
  full$hv006 <- month(full$dateinterview)
  full$hv016 <- day(full$dateinterview)
  print("Dates were converted from the Ethiopian to the Gregorian Calendar")
}

# Insert Weather ----------------------------------------------------------

# Set working directory for the weather data
setwd("~/data-mdavis65/steven_sola/2_Weather_Processed/Rdata")

# Import the 60-day weather into the dataset
load(paste0(name_year, "_weatherfinal"))

# Check for any missings and stop if any are found
stopifnot(sum(is.na(weather_final)) == 0)















# Clean the 60 day weather data
## Remove unwanted variables
weather_precip_SN05 <- select(weather_precip_SN05, -c(MM, DD, DOY, YEAR))

### If there are 0 Lat and 0 Long, Remove

## Remove unwanted characters from entire dataframe
weather_precip_SN05 <- as.data.frame(lapply(weather_precip_SN05, function(x) gsub("[c()]","", x)))

## Remove extra values in the cell for lat/long
weather_precip_SN05$LAT<- gsub("\\,.*","",weather_precip_SN05$LAT)
weather_precip_SN05$LON<- gsub("\\,.*","",weather_precip_SN05$LON)

## Deliminate the PRECTOTCORR and EVLAND Variables into separate variables
weather_precip_SN05 <- weather_precip_SN05 %>% 
                         separate(PRECTOTCORR, into = paste0("P", 1:61), sep = ",")

weather_precip_SN05 <- weather_precip_SN05 %>% 
                         separate(EVLAND, into = paste0("EV", 1:61), sep = ",")

## Convert Evaporation into mm/day
weather_precip_SN05 <- weather_precip_SN05 %>% mutate_at(vars(contains('EV')), as.numeric) %>% 
  mutate_at(vars(contains('EV')), ~ (. / 1000000) * 86400) 

## Deliminate the Date Variable into separate variables
weather_precip_SN05 <- weather_precip_SN05 %>% 
                         separate(YYYYMMDD, into = paste0("Y", 1:61), sep = ",")

## Keep only "Date61" (date of the survey)
weather_precip_SN05 <- weather_precip_SN05 %>% 
                         rename("SurveyDate" = "Y61") %>% #Rename the survey date variable
                         select(-grep("Y", names(.))) #Drop all other dates

## Convert date into readable date
weather_precip_SN05 <- weather_precip_SN05 %>% 
                         mutate(SurveyDate = as.numeric(SurveyDate)) %>%   # ensure class is numeric
                         mutate(SurveyDate = as.Date(SurveyDate, origin = "1970-01-01")) # convert to date using Excel origin                       
                          
## Rename variables
names(weather_precip_SN05)[4:64] = paste0("precip_minus", 60:0)
names(weather_precip_SN05)[65:125] = paste0("evap_minus", 60:0)

## Turn variables into numeric
weather_precip_SN05 <- weather_precip_SN05 %>% 
                         mutate_if(is.character, as.numeric)

## Sum of precipitations with 90, 95, and 99 percentiles
### Overall
weather_precip_SN05$precip_overalltotal <- rowSums(weather_precip_SN05[ , 4:64])
weather_precip_SN05$precip_overalltotal_90 <- weather_precip_SN05$precip_overalltotal * 0.90
weather_precip_SN05$precip_overalltotal_95 <- weather_precip_SN05$precip_overalltotal * 0.95
weather_precip_SN05$precip_overalltotal_99 <- weather_precip_SN05$precip_overalltotal * 0.99
weather_precip_SN05$precip_overalltotal_999 <- weather_precip_SN05$precip_overalltotal * 0.999

### -60 days (column 4) to -30 days (column 34)
weather_precip_SN05$precip_minus2mototal <- rowSums(weather_precip_SN05[ , 4:34])
weather_precip_SN05$precip_minus2mototal_90 <- weather_precip_SN05$precip_minus2mototal * 0.90
weather_precip_SN05$precip_minus2mototal_95 <- weather_precip_SN05$precip_minus2mototal * 0.95
weather_precip_SN05$precip_minus2mototal_99 <- weather_precip_SN05$precip_minus2mototal * 0.99
weather_precip_SN05$precip_minus2mototal_999 <- weather_precip_SN05$precip_minus2mototal * 0.999

### -29 days (column 34) to -0 days (day of survey / column 64)
weather_precip_SN05$precip_minus1mototal <- rowSums(weather_precip_SN05[ , 35:64])
weather_precip_SN05$precip_minus1mototal_90 <- weather_precip_SN05$precip_minus1mototal * 0.90
weather_precip_SN05$precip_minus1mototal_95 <- weather_precip_SN05$precip_minus1mototal * 0.95
weather_precip_SN05$precip_minus1mototal_99 <- weather_precip_SN05$precip_minus1mototal * 0.99
weather_precip_SN05$precip_minus1mototal_999 <- weather_precip_SN05$precip_minus1mototal * 0.999

## Sum of evaporations
### Overall
weather_precip_SN05$evap_overalltotal <- rowSums(weather_precip_SN05[ , 65:125])

### -60 days (column 65) to -30 days (column 95)
weather_precip_SN05$evap_minus2mototal <- rowSums(weather_precip_SN05[ , 65:95])

### -29 days (column 96) to -0 days (day of survey / column 125)
weather_precip_SN05$evap_minus1mototal <- rowSums(weather_precip_SN05[ , 96:125])

## dedupe
weather_precip_SN05 <- weather_precip_SN05[!duplicated(weather_precip_SN05), ]

# Join the weather data to the full dataset
weather_precip_SN05 <- weather_precip_SN05[with(weather_precip_SN05, order(LAT, LON, SurveyDate)), ]
full_05_spatial <- full_05_spatial[with(full_05_spatial, order(LATNUM, LONGNUM, dateinterview)), ]
full_05_weather <- right_join(full_05_spatial, weather_precip_SN05, 
                         by = c("LONGNUM" = "LON", "LATNUM" = "LAT", "dateinterview" = "SurveyDate"))

# Import the climate classifications into code
load("climate_classification_SN05.Rdata")

## dedupe
climate_class_SN05 <- climate_class_SN05[!duplicated(climate_class_SN05), ] %>% 
                        mutate(ClimateZ = trimws(as.character(ClimateZ)))

# Join the weather data to the full dataset
climate_class_SN05 <- climate_class_SN05[with(climate_class_SN05, order(hv001)), ]
full_05_weather <- full_05_weather[with(full_05_weather, order(hv001)), ]
final_05 <- left_join(full_05_weather, climate_class_SN05, by = c( "hv001" = "hv001"))

# Drop all the HV1xx variables
final_05 <- final_05 %>% select(-contains(c("HV1", "hvidx", "hml", "hb", "idx", "sh", "ha", "hc", "geometry")))

# Write the final dataset to the main Senegal folder
save(final_05, file="C:/Users/Steven/OneDrive - Johns Hopkins/Dissertation/Dissertation/Data/Aim 1/Senegal/SN_05.Rdata")
