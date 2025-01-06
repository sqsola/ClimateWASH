# Set the location
#location <- "personal"
location <- "HPC"

# Load Packages
library(plyr)
library(data.table)
library(lubridate)
library(haven)
library(magrittr)
library(sf)
library(ncdf4)
library(shinythemes)
library(tidyverse)

if (location == "personal") {
library(ethiopianDate)
library(lutz)
library(kgc)
  }
 
if (location == "HPC") { 
library(ethiopianDate, lib.loc = "/home/ssola1/rlibs/R")
library(lutz, lib.loc = "/home/ssola1/rlibs/R")
library(kgc, lib.loc = "/home/ssola1/rlibs/R")
  }

# Specify the files to work on
source("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/countries.R")
# country <- "Ethiopia"
# name_year <- "ET_05"

# Start system time
Start <- Sys.time()

for (i in 1:nrow(countries)) {
  country <- countries$country[i]
  name_year <- countries$name_year[i]

if (location == "personal") {
# Set Working Directory (Personal)
setwd(paste0("C:/Users/Steven Sola/OneDrive - Johns Hopkins/Dissertation/Dissertation/Data/Aim 1/",country,"/",name_year))
  }

if (location == "HPC") { 
# Set Working Directory (HPC)
setwd(paste0("~/data-mdavis65/steven_sola/",country,"/",name_year))
  }

#Read in survey data (Household member)
files <- list.files(pattern=c(".DTA|\\.shp$"), recursive = TRUE)

# Filter the list for the Household level, save to environment
file_hh <- str_subset(files, "HR")
hh <- read_dta(file_hh)

# Filter the list for the Spatial, save to environment
file_spatial <- str_subset(files, "GE")
spatial <- st_read(file_spatial)

# Remove empty rows of HH dataset
hh <- hh[,colSums(is.na(hh))<nrow(hh)]

# Month of interview
# CMC = Number of months since Jan 1 1900. Jan 1 1900 is CMC = 1
full_survey <- hh
full_survey$monthinterview <- as.Date("1900-01-01")
full_survey$monthinterview <- full_survey$monthinterview %m+% months(full_survey$hv008-1)

# Add Date to the interview month
full_survey$dateinterview <- full_survey$monthinterview + (full_survey$hv016-1)

# This section will run if the country is Ethiopia
# This will convert the dates from the Ethiopian calendar to Gregorian calendar
if (country == "Ethiopia") {

# Create new variables so that R doesn't get confused with lubridate functions.
full_survey <- full_survey %>% 
  mutate(year_eth = hv007, month_eth = month(monthinterview), day_eth = day(dateinterview))

# Create Gregorian date based off Ethiopian date
full_survey <- full_survey %>% 
  mutate(dateinterview_greg = ethiopianToGregorian(year = as.numeric(.$year_eth),
                                                   month = as.numeric(.$month_eth),
                                                   date = as.numeric(.$day_eth)))

# Ensure the date is in Date format
full_survey$dateinterview <- as.Date(full_survey$dateinterview_greg)

# Push the new dates to the old date variables
full_survey$hv007 <- year(full_survey$dateinterview)
full_survey$hv006 <- month(full_survey$dateinterview)
full_survey$hv016 <- day(full_survey$dateinterview)

print("Dates were converted from the Ethiopian to the Gregorian Calendar")

  }

# 60 Days before date of the interview
full_survey$dateinterview_minus60 <- full_survey$dateinterview - 60
full_survey$dateinterview_minus60 <- as.Date(full_survey$dateinterview_minus60, format = "%Y-%m-%d")

# 1 day after the date of the interview
full_survey$dateinterview_plus1 <- full_survey$dateinterview + 1
full_survey$dateinterview_plus1 <- as.Date(full_survey$dateinterview_plus1, format = "%Y-%m-%d")

# Remove duplicates
spatial <- spatial[!duplicated(spatial), ]

# Only accept the coordinates from GPS
spatial <- spatial %>% filter(SOURCE != "MIS")

# Filter out all infinitesimally small GPS points
spatial <- spatial %>% filter(!between(LATNUM, -0.0001, 0.0001))

# join spatial to data
full_survey_spatial <- left_join(full_survey, spatial, by = c("hv001" = "DHSCLUST"))

# Get unique combos of lat, long, and dates
unique_obs <- unique(full_survey_spatial[c("LATNUM", "LONGNUM", "hv001", 
                                         "dateinterview", "dateinterview_minus60",
                                         "dateinterview_plus1")])

# Remove rows with 0 lat and long
unique_obs <- unique_obs %>% filter(LATNUM != 0 | LONGNUM != 0)

# Create list for the For Loop
tz_list <-  list()

for (time in 1:nrow(unique_obs)) {
  tz_list[[time]] <- tz_offset(unique_obs$dateinterview[[time]], 
                               tz = tz_lookup_coords(unique_obs$LATNUM[[time]], unique_obs$LONGNUM[[time]], 
                                                     method = "fast", warn = FALSE))
}

# Combine results of for loop, bind back to unique_obs
unique_obs <- do.call(rbind, tz_list) %>% cbind(unique_obs, .)

# Set Working Directory
setwd("~")

# Read in the files, and combine them
files <- Sys.glob(file.path("~/data-mdavis65/steven_sola/1_Weather", "*.nc"))

# Empty frame to bind data
weather_survey <- list()

for (obs in 1:nrow(unique_obs)) {
  
  # Change the format to month, date, year
  # MAY NOT NEED TO CHANGE THE FORMATTING WITH LUBRIDATE
  date_start <- unique_obs$dateinterview_minus60[[obs]]
  date_end <- unique_obs$dateinterview[[obs]]
  
  # Set the lat/long
  lat <- unique_obs$LATNUM[[obs]]
  lon <- unique_obs$LONGNUM[[obs]]
  
  # Match the starting and ending dates with the imported list
  match_dates <- format(seq(date_start, date_end, by = 'day'), '%Y_%m_%d')
  
  # Subset those files from the matched dates
  # Reverse so that survey date is first, 60 days before is last file
  files_subset <- str_subset(files, paste0(match_dates, collapse = '|')) %>% rev()
  
  # Variables to extract from data
  accum_var <- c("e", "sro", "ssro", "tp")
  instant_var <- c("t2m", "skt", "lai_lv", "lai_hv", "d2m")
  variables <- c(accum_var, instant_var)
  
  # UTC Offset Number
  utc_offset <- unique_obs$utc_offset_h[[obs]]
  
  # Empty datalists to populate later
  weather_var_0 <- list()
  weather_var_1 <- list()
  
  weather_60day_0 <- list()
  weather_60day_1 <- list()

  for(file in files_subset) {
    
    # Set Working Directory
    setwd("~")

    # Open the NC file to extract data
    nc_day0 <- nc_open(file)
    
    # Get the time from the NC file, origin is hours since 1900
    time_day0 <- as.POSIXct(nc_day0$dim$time$vals*3600, origin = "1900-01-01 00:00:00", tz = "UTC")
    
    #get the date for the current loop, and add one day, format it
    date_plus1 <- format(date(time_day0)[1] + 1, "%Y_%m_%d")

    # Changing the working directory to where the files are located
    setwd("~/data-mdavis65/steven_sola/1_Weather")
    
    # Read in the date_plus1 file
    nc_day1 <- nc_open(paste0(date_plus1,".nc"))
    
    # Ensure that the date is immediately after first file
    time_day1 <- as.POSIXct(nc_day1$dim$time$vals*3600, origin = "1900-01-01 00:00:00", tz = "UTC")
    
    # Error code if dates aren't a difference of 1
    date_0 <- date(time_day0)[1]
    date_0 <- ymd(date_0)
    
    date_1 <- date(time_day1)[1]
    date_1 <- ymd(date_1)
  
    if(date_1 - date_0 != 1) stop("Difference between the dates is not 1")
    
    # Extract date and then add 1 to the date and use that to open the next day's nc file
    for(var in variables) {
      
      # Get values from closest lon/lat, and read all variables across the dimensions
      weather_var_0[[var]] <- ncvar_get(nc_day0, varid = var,
                                        start= c(which.min(abs(nc_day0$dim$longitude$vals - lon)) ,
                                                 which.min(abs(nc_day0$dim$latitude$vals - lat)),
                                                 1),
                                        count = c(1,1,-1))

      weather_var_1[[var]] <- ncvar_get(nc_day1, varid = var,
                                        start= c(which.min(abs(nc_day1$dim$longitude$vals - lon)),
                                                 which.min(abs(nc_day1$dim$latitude$vals - lat)),
                                                 1),
                                        count = c(1,1,-1))
    }
    
    # Bind all the variable data together
    weather_60day_0[[file]] <-  do.call(rbind, weather_var_0)
    weather_60day_1[[file]] <-  do.call(rbind, weather_var_1)
    
    # Close NC files
    nc_close(nc_day0)
    nc_close(nc_day1)
    
  }
  
  # Bind all the variables from all the files together, transpose, save as df
  weather_60day_0 <-  do.call(rbind, weather_60day_0) %>% t() %>% as.data.frame()
  weather_60day_1 <-  do.call(rbind, weather_60day_1) %>% t() %>% as.data.frame()
  weather_60day_48hr <- rbind(weather_60day_0, weather_60day_1)
  
  # Rename the column names
  colnames(weather_60day_0) <- make.unique(names(weather_60day_0), sep = "_")
  colnames(weather_60day_1) <- make.unique(names(weather_60day_1), sep = "_")
  colnames(weather_60day_48hr) <- make.unique(names(weather_60day_48hr), sep = "_")
  
  # add UTC offset
  weather_60day_48hr <- cbind(utc_offset, weather_60day_48hr)
  
  # Separate out the accumulations and the instantaneous variables
  accum <- weather_60day_48hr %>% select(starts_with(accum_var))
  instant <- weather_60day_48hr %>% select("utc_offset", starts_with(instant_var))
  
  # ACCUMULATION VARIABLES #
    # If the Offset is 0, just take the Day 1 cumulative
    # If the Offset is 1 or more:
    #  - Take the Day 1 cumulative
    #  - Take the number of offset hours to decide number of rows for 2nd day
    #  - From the second day, take the last row.
    #  - per ECMWFR, accumulations are from beginning to end of step
    #  - Add the totals from Day 1 and Day 2
    
    if (utc_offset == 0) {
          accum <- accum %>% slice(25)
    } else if (utc_offset >= 1) {
          accum_day1 <- accum %>% slice(25)
          
          accum_day2 <- accum[25:(utc_offset+25),] %>% 
          slice_tail(n = 1) 
          
          accum <- rbind(accum_day1, accum_day2) %>% 
                            summarise(across(everything(), ~ last(.x) + first(.x)))
    }

  # INSTANTANEOUS VARIABLES #
  # Take the UTC number, and extract the 24 hours following that number
  instant <- instant[(instant$utc_offset[1]):(instant$utc_offset[1]+24),] %>% 
                            summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))
  
  # Combine the accumulation and instantaneous variables
  weather_60day_tzcorrect <- cbind(instant, accum)
  
  # Check whether lengths are equal
  stopifnot(length(weather_60day_tzcorrect) == length(weather_60day_48hr))
  
  # Add in previous variables from survey data
  weather_survey <- rbind(weather_survey, weather_60day_tzcorrect)

  # Progress Report
  cat(paste0(round(obs / nrow(unique_obs) * 100), "% completed", ", Elapsed Time: ", 
             round(as.difftime(Sys.time()-Start, format = "%T"), digits = 2), "\n"))
  }

# Bind processed data back to original df
weather_survey_coords <- cbind(unique_obs, weather_survey)

# Convert Kelvin to Celsius for skin temp, 2m temp, 2m dewpoint
weather_survey_coords_cel <- weather_survey_coords %>% 
                                mutate_at(vars(starts_with(c("skt","t2m","d2m"))),  ~.x - 273.15)

# Convert m to cm for surface runoff (sro) and total precipitation (tp)
weather_survey_coords_cel_cm <- weather_survey_coords_cel %>% 
                                 mutate_at(vars(starts_with(c("sro", "tp"))),  ~.x * 100)

# Assign Köppen-Geiger Zone
weather_survey_coords_cel_cm <- weather_survey_coords_cel_cm %>% 
                                   mutate(rndCoord.lon = RoundCoordinates(.$LONGNUM, res = "fine", latlong = "lon"),
                                          rndCoord.lat = RoundCoordinates(.$LATNUM, res = "fine", latlong = "lat"))

weather_final <- weather_survey_coords_cel_cm %>% 
                      mutate(kgc = LookupCZ(., res = "fine", rc = FALSE))

# Save the outputs
fwrite(weather_final, paste0("~/data-mdavis65/steven_sola/2_Weather_Processed/CSV/", name_year,"_weatherfinal_cm.csv"))
fwrite(weather_final, paste0("~/scr4-mdavis65/ssola1/Weather_Processed/CSV/", name_year, "_weatherfinal_cm.csv"))

saveRDS(weather_final, file = paste0("~/data-mdavis65/steven_sola/2_Weather_Processed/Rdata/", name_year, "_weatherfinal_cm.rds"))
saveRDS(weather_final, file = paste0("~/scr4-mdavis65/ssola1/Weather_Processed/Rdata/", name_year, "_weatherfinal_cm.rds"))

# Finish message
print(paste0(name_year, " has finished processing"))

# Cumulative time for processing files
round(as.difftime(Sys.time()-Start, format = "%T"), digits = 2)

}
