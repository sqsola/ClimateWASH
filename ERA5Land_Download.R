# Load Packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(ecmwfr)
        
# LOAD FILES
files <- Sys.glob(file.path("~/data-mdavis65/steven_sola/1_Weather", "*.nc"))
files_2 <- Sys.glob(file.path("~/data-mdavis65/steven_sola/00_NewWeather", "*.nc"))

files <- c(files_2, files)

# Delete fluff before the date
files <- gsub(".*Weather/","",files)

# Delete fluff after the date
files <- gsub("\\.nc*","",files)

# Save as dataframe
files <- data.frame(filename = files, stringsAsFactors = FALSE)

# change to a date
files$filename <- lubridate::ymd(files$filename)

# Sort by date
sort(files$filename)

# Get the comparison date range
date_range <- seq(lubridate::ymd("1990-02-15"), lubridate::ymd("2022-12-31"), by = "day") 

# Compare
missing <- date_range[!date_range %in% files$filename]

missing <- data.frame(filename = missing, stringsAsFactors = FALSE)

missing$year <- substr(missing$filename,1,4)
missing$month <- substr(missing$filename,6,7)
missing$day <- substr(missing$filename,9,10)

#Order the files
missing <- missing %>% arrange(year, month, day)

# set a key to the keychain
wf_set_key(user = "XXX",
           key = "XXX",
           service = "cds")

for(i in 1:nrow(missing)){

  file <- wf_request(
    user    = "XXXX",   # user ID (for authentification)
    request = list(
      "dataset_short_name" = "reanalysis-era5-land",
      "product_type" = "reanalysis",
      "variable" =  c("2t", "skt", "e", "sro", "ssro", "tp",
                      "leaf_area_index_low_vegetation", "leaf_area_index_high_vegetation",
                      "2d"),
      "year" = missing$year[[i]],
      "month" = missing$month[[i]],
      "day" = missing$day[[i]],
      "time" = c("00:00", "01:00", "02:00", "03:00",
                 "04:00", "05:00", "06:00", "07:00",
                 "08:00", "09:00", "10:00", "11:00",
                 "12:00", "13:00", "14:00", "15:00",
                 "16:00", "17:00", "18:00", "19:00",
                 "20:00", "21:00", "22:00", "23:00"),
      "area" = "27.6/-18.1/-35.1/51.1",
      "format" = "netcdf",
      "target" = paste0(paste(missing$year[[i]], missing$month[[i]], missing$day[[i]], sep = "_"),".nc")),
    transfer = TRUE,
    path     = "~/data-mdavis65/steven_sola/00_NewWeather",
    time_out = 36000
  )
}