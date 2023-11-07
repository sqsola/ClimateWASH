#############
# FUNCTIONS #
#############

#*-----------------------------------------------------*#

# Assign unique combos to variables
var1 <- unique_df_05$LONGNUM
var2 <- unique_df_05$LATNUM
var3 <- unique_df_05$dateinterview_minus60
var4 <- unique_df_05$dateinterview

# NASAPower Function, Part 1
# QV2M = 2 Meter Specific Humidity
# RH2M = 2 Meter Relative Humidity
# T2M = 2 Meter Air Temp (Kelvin)
# GWETTOP = Surface Soil Wetness (0 = dry, 1 = wet)

get_nasapower_1 <- function(var1, var2, var4){
  get_power(community = "ag",
            lonlat = c(var1, var2),
            pars = c("QV2M", "RH2M", "T2M",
                     "GWETTOP"),
            dates = var4,
            temporal_api = "daily")
}

weather_SN05 <- mapply(get_nasapower_1, var1, var2, var4)

# Transpose and convert to dataframe
weather_SN05 <- t(weather_SN05) %>% as.data.frame(.)

# Convert to Character and write CSV to save results
save(weather_SN05, file="weather_SN05.Rdata")

#*-----------------------------------------------------*#


#*-----------------------------------------------------*#

# NASAPower Function, Part 2
# 60 day precipitation and evaporation
get_nasapower_2 <- function(var1, var2, var3, var4){
  get_power(community = "ag",
            lonlat = c(var1, var2),
            pars = c("PRECTOTCORR", "EVLAND"),
            dates = c(var3, var4),
            temporal_api = "daily")
}

weather_precip_SN05 <- mapply(get_nasapower_2, var1, var2, var3, var4)

# Transpose the data and save as a data frame
weather_precip_SN05 <- t(weather_precip_SN05) %>% as.data.frame(.)
  
# Convert to Character and write CSV to save results
save(weather_precip_SN05, file="weather_precip_SN05.Rdata")

#*-----------------------------------------------------*#


#*-----------------------------------------------------*#

# Assign Koppen Geiger Classifications
climate_class <- unique_df_05 %>% 
  select(hv001, LONGNUM, LATNUM)

climate_class <- data.frame(climate_class,
                            rndCoord.lon = RoundCoordinates(climate_class$LONGNUM, res = 'fine', latlong = 'lon'),
                            rndCoord.lat = RoundCoordinates(climate_class$LATNUM, res = 'fine', latlong = 'lat'))

climate_class <- climate_class %>% 
                 select(hv001, rndCoord.lon, rndCoord.lat)

climate_class_SN05 <- data.frame(climate_class, ClimateZ = LookupCZ(climate_class, res = "fine", rc = FALSE))

save(climate_class_SN05, file="climate_classification_SN05.Rdata")

#*-----------------------------------------------------*#

# Set Working Directory
setwd("C:/Users/Steven/OneDrive - Johns Hopkins/Dissertation/Dissertation/Data/Aim 1/Senegal/SN_05")

library(kgc)
library(dplyr)
library(tidyr)
library(haven)
library(lubridate)
library(nasapower)
library(sf)

##############################
#                            #
# Senegal 92-93 Data (DHS-2) #
#                            #
##############################

# Read in survey data (1992-1993)
#hhmem_05 <- read_dta("SN_05_hhmember/SNPR21FL.DTA")
hh_05 <- read_dta("SN_05_hh/SNHR4AFL.DTA")
#child_05 <- read_dta("SN_05_child/SNKR21FL.DTA")
#birth_05 <- read_dta("SN_05_birth/SNBR21FL.dta")
#hw_05 <- read_dta("SN_05_heightweight/SNHW21FL.DTA")
spatial_05 <- st_read("SN_05_gps/SNGE4BFL.shp")

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
