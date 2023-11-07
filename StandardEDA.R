# Load Libraries
library(dplyr)
library(tidyr)
library(haven)
library(sf)
library(lubridate)
library(nasapower)
library(kgc)

##############################
#                            #
# Senegal 92-93 Data (DHS-2) #
#                            #
##############################

# Read in survey data (1992-1993)
hhmem_9293 <- read_dta("SN_9293/SN_9293_hhmember/SNPR21FL.DTA")
hh_9293 <- read_dta("SN_9293/SN_9293_hh/SNHR21FL.DTA")
child_9293 <- read_dta("SN_9293/SN_9293_child/SNKR21FL.DTA")
birth_9293 <- read_dta("SN_9293/SN_9293_birth/SNBR21FL.dta")
hw_9293 <- read_dta("SN_9293/SN_9293_heightweight/SNHW21FL.DTA")
spatial_9293 <- st_read("SN_9293/SN_9293_gps/SNGE23FL.shp")

# Remove empty rows of HH dataset
hh_9293 <- hh_9293[,colSums(is.na(hh_9293))<nrow(hh_9293)]

# Merge hhmem data to hh data
hhmem_9293_join <- merge(hhmem_9293, hh_9293)

# Commonalities between birth and join
common_birthchild <- intersect(names(birth_9293), names(child_9293))

# Sort, then merge births and Height / Weight Data
# Use HWCASEID and HWLINE, from the Height and Weight file, with CASEID and BIDX, from the Births Recode file to merge it with the Births’ data
birth_9293 <- birth_9293[with(birth_9293, order(caseid, bidx)), ]
hw_9293 <- hw_9293[with(hw_9293, order(hwcaseid, hwline)), ]
birth_hw_9293 <- left_join(birth_9293, hw_9293, by = c("caseid" = "hwcaseid", "bidx" = "hwline"))

# Merge all the datasets
hhmem_9293_join <- hhmem_9293_join[with(hhmem_9293_join, order(hv001, hv002, hvidx)), ]
birth_hw_9293 <- birth_hw_9293[with(birth_hw_9293, order(v001, v002, v003)), ]
full_9293 <- left_join(hhmem_9293_join, birth_hw_9293, by = c("hv001" = "v001", "hv002" = "v002", "hvidx" = "v003"))

# Month of interview
# CMC = Number of months since Jan 1 1900. Jan 1 1900 is CMC = 1
full_9293$monthinterview <- as.Date("1900-01-01")
full_9293$monthinterview <- full_9293$monthinterview %m+% months(full_9293$hv008-1)
table(full_9293$monthinterview)

# you can check based off the age of the child. DOB of child and month of interview
# all children under the age of 59 months, they should be included.


# Add Date to the interview month
full_9293$dateinterview <- full_9293$monthinterview + (full_9293$hv016-1)


# CHECK THIS , WHY "-1"


# 60 Days before and after date of the interview
full_9293$dateinterview_minus60 <- full_9293$dateinterview - 60
full_9293$dateinterview_minus60 <- as.Date(full_9293$dateinterview_minus60, format = "%Y-%m-%d")
full_9293$dateinterview_plus60 <- full_9293$dateinterview + 60
full_9293$dateinterview_plus60 <- as.Date(full_9293$dateinterview_plus60, format = "%Y-%m-%d")

# join spatial to data
full_9293_spatial <- left_join(full_9293, spatial_9293, by = c("hv001" = "DHSCLUST"))

# Get unique combos of lat, long, and dates
unique_df_9293 <- unique(full_9293_spatial[c("LATNUM", "LONGNUM", "hv001", 
                                             "dateinterview", "dateinterview_minus60")])

#*-----------------------------------------------------*#


# Assign unique combos to variables
var1 <- unique_df_9293$LONGNUM
var2 <- unique_df_9293$LATNUM
var3 <- unique_df_9293$dateinterview_minus60
var4 <- unique_df_9293$dateinterview

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

weather <- mapply(get_nasapower_1, var1, var2, var4)

# Transpose and convert to dataframe
weather <- t(weather) %>% as.data.frame(.)

# Convert to Character and write CSV to save results
weather <- apply(weather,2,as.character)
write.csv(weather, "SN_9293/SN9293_weather.csv")


#*-----------------------------------------------------*#


# Import the .CSV back into the dataset
weather_SN9293 <- read.csv("SN_9293/SN9293_weather.csv")


#*-----------------------------------------------------*#


# NASAPower Function, Part 2
# 60 day precipitation
get_nasapower_2 <- function(var1, var2, var3, var4){
  get_power(community = "ag",
            lonlat = c(var1, var2),
            pars = "PRECTOTCORR",
            dates = c(var3, var4),
            temporal_api = "daily")
}

weather_2 <- mapply(get_nasapower_2, var1, var2, var3, var4)

# Transpose the data and save as a data frame
weather_2 <- t(weather_2) %>% as.data.frame(.)

# Convert to Character and write CSV to save results
weather_2 <- apply(weather_2,2,as.character)
write.csv(weather_2, "SN_9293/SN9293_weather_precip.csv")


#*-----------------------------------------------------*#


# Import the .CSV back into the dataset
weather_precip_SN9293 <- read.csv("SN_9293/SN9293_weather_precip.csv")

# Clean the weather data
## Remove unwanted variables
weather_precip_SN9293 <- select(weather_precip_SN9293, -c(MM, DD, DOY, YEAR, X, YYYYMMDD))


## Remove unwanted characters from entire dataframe
weather_precip_SN9293 <- as.data.frame(lapply(weather_precip_SN9293, function(x) gsub("[c()]","", x)))


## Remove extra values in the cell for lat/long
weather_precip_SN9293$LAT<- gsub("\\,.*","",weather_precip_SN9293$LAT)
weather_precip_SN9293$LON<- gsub("\\,.*","",weather_precip_SN9293$LON)

## Deliminate the PRECTOTCORR Variable into separate variables
weather_precip_SN9293 <- weather_precip_SN9293 %>% 
                           separate(PRECTOTCORR, into = paste0("V", 1:61), sep = ",")

## Rename variables
names(weather_precip_SN9293)[3:63] = paste0("precip_minus", 60:0)

# Assign Koppen Geiger Classifications
climate_class <- unique_df_9293 %>% 
                  select(hv001, LONGNUM, LATNUM)

climate_class <- data.frame(climate_class,
                      rndCoord.lon = RoundCoordinates(climate_class$LONGNUM, res = 'fine', latlong = 'lon'),
                      rndCoord.lat = RoundCoordinates(climate_class$LATNUM, res = 'fine', latlong = 'lat'))

climate_class <- climate_class %>% 
                  select(hv001, rndCoord.lon, rndCoord.lat)

climate_class_SN9293 <- data.frame(climate_class, ClimateZ = LookupCZ(climate_class, res = "fine", rc = FALSE))

write.csv(climate_class_SN9293, "SN_9293/climate_classification_SN9293.csv")

# Import the climate classifications into code
climate_class_SN9293 <- read.csv("SN_9293/climate_classification_SN9293.csv")












# Merge
combined <- merge(join_spatial, weather)




