# Load Libraries
library(tidyverse)
library(haven)
library(lubridate)
library(stringr)
library(sf)

# Specify the files to work on
country <- "Senegal"
name_year <- "SN_05"

# Set Working Directory (HPC)
#setwd(paste0("~/data-mdavis65/steven_sola/",country,"/",name_year))

# Set Working Directory (Personal)
setwd(paste0("C:/Users/Steven/OneDrive - Johns Hopkins/Dissertation/Dissertation/Data/Aim 1/",
             country,"/",name_year))

#Read in survey data (Household member)
files <- list.files(pattern=c(".DTA|\\.shp$"), recursive = TRUE)
files

# Filter the list for the Household Member level, save to environment
file_hhmember <- str_subset(files, "PR")
assign(paste0("hhmem_", name_year), read_dta(file_hhmember))

# Filter the list for the Household level, save to environment
file_hh <- str_subset(files, "HR")
assign(paste0("hh_", name_year), read_dta(file_hh))

# Filter the list for the Child level, save to environment
file_child <- str_subset(files, "KR")
assign(paste0("child_", name_year), read_dta(file_child))

# Filter the list for the Birth level, save to environment
file_birth <- str_subset(files, "BR")
assign(paste0("child_", name_year), read_dta(file_birth))

# Filter the list for the Height/Weight level, save to environment
file_hw <- str_subset(files, "HW")
assign(paste0("hw_", name_year), read_dta(file_hw))

# Filter the list for the Spatial, save to environment
file_spatial <- str_subset(files, "GE")
assign(paste0("spatial_", name_year), st_read(file_spatial))





# Remove empty rows of HH dataset
hh_05 <- hh_05[,colSums(is.na(hh_05))<nrow(hh_05)]

# Merge hhmem data to hh data
#hhmem_05_join <- merge(hhmem_05, hh_05)

# Commonalities between birth and join
#common_birthchild <- intersect(names(birth_05), names(child_05))

# Sort, then merge births and Height / Weight Data
# Use HWCASEID and HWLINE, from the Height and Weight file, with CASEID and BIDX, from the Births Recode file to merge it with the Births’ data
#birth_05 <- birth_05[with(birth_05, order(caseid, bidx)), ]
#hw_05 <- hw_05[with(hw_05, order(hwcaseid, hwline)), ]
#birth_hw_05 <- left_join(birth_05, hw_05, by = c("caseid" = "hwcaseid", "bidx" = "hwline"))

# Merge all the datasets
#hhmem_05_join <- hhmem_05_join[with(hhmem_05_join, order(hv001, hv002, hvidx)), ]
#birth_hw_05 <- birth_hw_05[with(birth_hw_05, order(v001, v002, v003)), ]
#full_05 <- left_join(hhmem_05_join, birth_hw_05, by = c("hv001" = "v001", "hv002" = "v002", "hvidx" = "v003"))

# Month of interview
# CMC = Number of months since Jan 1 1900. Jan 1 1900 is CMC = 1
full_05 <- hh_05
full_05$monthinterview <- as.Date("1900-01-01")
full_05$monthinterview <- full_05$monthinterview %m+% months(full_05$hv008-1)
table(full_05$monthinterview)

# you can check based off the age of the child. DOB of child and month of interview
# all children under the age of 59 months, they should be included.

# Add Date to the interview month
full_05$dateinterview <- full_05$monthinterview + (full_05$hv016-1)

# 60 Days before and after date of the interview
full_05$dateinterview_minus60 <- full_05$dateinterview - 60
full_05$dateinterview_minus60 <- as.Date(full_05$dateinterview_minus60, format = "%Y-%m-%d")
full_05$dateinterview_plus60 <- full_05$dateinterview + 60
full_05$dateinterview_plus60 <- as.Date(full_05$dateinterview_plus60, format = "%Y-%m-%d")

# join spatial to data
full_05_spatial <- left_join(full_05, spatial_05, by = c("hv001" = "DHSCLUST"))

# Get unique combos of lat, long, and dates
unique_df_05 <- unique(full_05_spatial[c("LATNUM", "LONGNUM", "hv001", 
                                             "dateinterview", "dateinterview_minus60")])

unique_df_05 <- unique_df_05[!duplicated(unique_df_05), ]

# Remove rows with 0 lat and long
unique_df_05 <- unique_df_05 %>% filter(LATNUM != 0| LONGNUM != 0)

#*------------------------------------------------------------------------------------*

#*------------------------------------------------------------------------------------*

# Import the one-day weather into the dataset
load("weather_SN05.Rdata")

# Import the 60-day weather into the dataset
load("weather_precip_SN05.Rdata")

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
