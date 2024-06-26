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

# Read in the file that lists all countries
source("countries.R") 

countries <- countries %>% filter(country == "Ethiopia") %>% slice(3:5)

for(entry in 1:nrow(countries)) {
  
# Specify the files to work on
country <- countries$country[[entry]]
name_year <- countries$name_year[[entry]]

# Start message
cat(paste0("\n", name_year, " has started processing", "\n"))

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

# Filter the list for the Household level, save to environment
hh <- str_subset(files, "HR") %>% read_dta(., encoding = "latin1")

# Filter the list for the wealth, save to environment
if (any(grepl("wealth", unlist(files)))) {
  wealth <- str_subset(files, "wealth") %>% read_dta(., encoding = "latin1")
  
  # stop if the nrow wealth doesn't match the nrow hh
  stopifnot(nrow(hh) == nrow(wealth))
}

# Filter the list for the Spatial, save to environment
if (any(grepl("gps", unlist(files)))) {
  spatial <- str_subset(files, "gps") %>% st_read()
}

# Merging -----------------------------------------------------------------

# Remove empty rows of HH dataset
hh <- hh[,colSums(is.na(hh))<nrow(hh)]

# Merge Wealth to hhmem
if (exists("wealth")) {
wealth <- wealth %>% arrange(whhid)
hh <- hh %>% arrange(hhid)
hh <- left_join(hh, wealth, by = c("hhid" = "whhid"))
}

if (exists("spatial")) {
  # join spatial to data
  full <- left_join(hh, spatial, by = c("hv001" = "DHSCLUST"))
}

if (!exists("spatial")) {
  # join spatial to data
  full <- hh
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
  cat("\n", "Dates were converted from the Ethiopian to the Gregorian Calendar", "\n")
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
# As well as the 1 week lag, 2 week lag, 1-2 week lag, 2-3 week lag, and 3 week lag
# As well as the 90th, 95th, 99th, and 99.9th percentile
for (weather_var_accum in accum_var) {
        weather_final <- weather_final %>%
                 rowwise() %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus7"):=sum(across(num_range(paste0(weather_var_accum,"_"), 1:7)))) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus14"):=sum(across(num_range(paste0(weather_var_accum,"_"), 1:14)))) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus30"):=sum(across(num_range(paste0(weather_var_accum,"_"), 1:30)))) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus60"):=sum(across(num_range(paste0(weather_var_accum,"_"), 1:60)))) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus8_14"):=sum(across(num_range(paste0(weather_var_accum,"_"), 8:14)))) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus15_21"):=sum(across(num_range(paste0(weather_var_accum,"_"), 15:21)))) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus8_21"):=sum(across(num_range(paste0(weather_var_accum,"_"), 8:21)))) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus15_28"):=sum(across(num_range(paste0(weather_var_accum,"_"), 15:28)))) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus22_28"):=sum(across(num_range(paste0(weather_var_accum,"_"), 22:28)))) %>%

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
                 mutate(!!paste0(weather_var_accum,"_totalminus60_999"):=.data[[!!paste0(weather_var_accum,"_totalminus60")]] * 0.999) %>%

                 mutate(!!paste0(weather_var_accum,"_totalminus8_14_90"):=.data[[!!paste0(weather_var_accum,"_totalminus8_14")]] * 0.9) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus8_14_95"):=.data[[!!paste0(weather_var_accum,"_totalminus8_14")]] * 0.95) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus8_14_99"):=.data[[!!paste0(weather_var_accum,"_totalminus8_14")]] * 0.99) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus8_14_999"):=.data[[!!paste0(weather_var_accum,"_totalminus8_14")]] * 0.999) %>%

                 mutate(!!paste0(weather_var_accum,"_totalminus15_21_90"):=.data[[!!paste0(weather_var_accum,"_totalminus15_21")]] * 0.9) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus15_21_95"):=.data[[!!paste0(weather_var_accum,"_totalminus15_21")]] * 0.95) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus15_21_99"):=.data[[!!paste0(weather_var_accum,"_totalminus15_21")]] * 0.99) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus15_21_999"):=.data[[!!paste0(weather_var_accum,"_totalminus15_21")]] * 0.999) %>%

                 mutate(!!paste0(weather_var_accum,"_totalminus8_21_90"):=.data[[!!paste0(weather_var_accum,"_totalminus8_21")]] * 0.9) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus8_21_95"):=.data[[!!paste0(weather_var_accum,"_totalminus8_21")]] * 0.95) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus8_21_99"):=.data[[!!paste0(weather_var_accum,"_totalminus8_21")]] * 0.99) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus8_21_999"):=.data[[!!paste0(weather_var_accum,"_totalminus8_21")]] * 0.999) %>%

                 mutate(!!paste0(weather_var_accum,"_totalminus15_28_90"):=.data[[!!paste0(weather_var_accum,"_totalminus15_28")]] * 0.9) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus15_28_95"):=.data[[!!paste0(weather_var_accum,"_totalminus15_28")]] * 0.95) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus15_28_99"):=.data[[!!paste0(weather_var_accum,"_totalminus15_28")]] * 0.99) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus15_28_999"):=.data[[!!paste0(weather_var_accum,"_totalminus15_28")]] * 0.999) %>% 

                 mutate(!!paste0(weather_var_accum,"_totalminus22_28_90"):=.data[[!!paste0(weather_var_accum,"_totalminus22_28")]] * 0.9) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus22_28_95"):=.data[[!!paste0(weather_var_accum,"_totalminus22_28")]] * 0.95) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus22_28_99"):=.data[[!!paste0(weather_var_accum,"_totalminus22_28")]] * 0.99) %>%
                 mutate(!!paste0(weather_var_accum,"_totalminus22_28_999"):=.data[[!!paste0(weather_var_accum,"_totalminus22_28")]] * 0.999)
}

for (weather_var_instant in instant_var) {
          weather_final <- weather_final %>%
                  rowwise() %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus7"):=(sum(across(num_range(paste0(weather_var_instant,"_"), 1:7))))/7) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus14"):=(sum(across(num_range(paste0(weather_var_instant,"_"), 1:14))))/14) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus30"):=(sum(across(num_range(paste0(weather_var_instant,"_"), 1:30))))/30) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus60"):=(sum(across(num_range(paste0(weather_var_instant,"_"), 1:60))))/60) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus8_14"):=(sum(across(num_range(paste0(weather_var_instant,"_"), 8:14))))/7) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus15_21"):=(sum(across(num_range(paste0(weather_var_instant,"_"), 15:21))))/7) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus8_21"):=(sum(across(num_range(paste0(weather_var_instant,"_"), 8:21))))/14) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus15_28"):=(sum(across(num_range(paste0(weather_var_instant,"_"), 15:28))))/14) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus22_28"):=(sum(across(num_range(paste0(weather_var_instant,"_"), 22:28))))/7) %>%

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
                  mutate(!!paste0(weather_var_instant,"_avgminus60_999"):=.data[[!!paste0(weather_var_instant,"_avgminus60")]] * 0.999) %>%

                  mutate(!!paste0(weather_var_instant,"_avgminus8_14_90"):=.data[[!!paste0(weather_var_instant,"_avgminus8_14")]] * 0.9) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus8_14_95"):=.data[[!!paste0(weather_var_instant,"_avgminus8_14")]] * 0.95) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus8_14_99"):=.data[[!!paste0(weather_var_instant,"_avgminus8_14")]] * 0.99) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus8_14_999"):=.data[[!!paste0(weather_var_instant,"_avgminus8_14")]] * 0.999) %>%

                  mutate(!!paste0(weather_var_instant,"_avgminus15_21_90"):=.data[[!!paste0(weather_var_instant,"_avgminus15_21")]] * 0.9) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus15_21_95"):=.data[[!!paste0(weather_var_instant,"_avgminus15_21")]] * 0.95) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus15_21_99"):=.data[[!!paste0(weather_var_instant,"_avgminus15_21")]] * 0.99) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus15_21_999"):=.data[[!!paste0(weather_var_instant,"_avgminus15_21")]] * 0.999) %>%

                  mutate(!!paste0(weather_var_instant,"_avgminus8_21_90"):=.data[[!!paste0(weather_var_instant,"_avgminus8_21")]] * 0.9) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus8_21_95"):=.data[[!!paste0(weather_var_instant,"_avgminus8_21")]] * 0.95) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus8_21_99"):=.data[[!!paste0(weather_var_instant,"_avgminus8_21")]] * 0.99) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus8_21_999"):=.data[[!!paste0(weather_var_instant,"_avgminus8_21")]] * 0.999) %>%

                  mutate(!!paste0(weather_var_instant,"_avgminus15_28_90"):=.data[[!!paste0(weather_var_instant,"_avgminus15_28")]] * 0.9) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus15_28_95"):=.data[[!!paste0(weather_var_instant,"_avgminus15_28")]] * 0.95) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus15_28_99"):=.data[[!!paste0(weather_var_instant,"_avgminus15_28")]] * 0.99) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus15_28_999"):=.data[[!!paste0(weather_var_instant,"_avgminus15_28")]] * 0.999) %>% 

                  mutate(!!paste0(weather_var_instant,"_avgminus22_28_90"):=.data[[!!paste0(weather_var_instant,"_avgminus22_28")]] * 0.9) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus22_28_95"):=.data[[!!paste0(weather_var_instant,"_avgminus22_28")]] * 0.95) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus22_28_99"):=.data[[!!paste0(weather_var_instant,"_avgminus22_28")]] * 0.99) %>%
                  mutate(!!paste0(weather_var_instant,"_avgminus22_28_999"):=.data[[!!paste0(weather_var_instant,"_avgminus22_28")]] * 0.999)
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

# Remove all labels from the SPSS datasets
full <- zap_labels(full)

# Drop all the HV1xx variables
full <- full %>% select(-contains(c("HV1", "hvidx", "hml", "hb", "idx", "sh", "ha", "hc", "geometry", "sb"))) %>% 
                 select(-starts_with(c("sm", "scweight", "hfs", "sent", "s20", "chl",
                                       "mlweight", "ho", "schweigh", "hd",
                                       "spa", "sus", "sw", "sah", "OBJECTID", "sc", "sv",
                                       "hdis", "sgrad", "sleve", "hvc", "sgam", "sden",
                                       "miil", "elig", "hk", "hy", "he", "sn", "ret",
                                       "sy", "sa", "tpv", "sp", "stest", "hm",
                                       "slquest", "slint", "sltrans", "stot", "ssc", "sl",
                                       "rsc", "hs", "ssy", "sdeath", "seel", "sdhs", "s12", "s21"))) %>%
                 select(-matches("[s][0-9]{3}", "[hs][0-9]{3}"))


# Write the final dataset to the main folder
fwrite(full, file = paste0("~/data-mdavis65/steven_sola/4_HHlevel/CSV/", name_year,"_hhweather.csv"))
fwrite(full, file = paste0("~/scr4-mdavis65/ssola1/HHlevel/CSV/", name_year, "_hhweather.csv"))

saveRDS(full, file= paste0("~/data-mdavis65/steven_sola/4_HHlevel/Rdata/", name_year, "_hhweather.Rdata"))
saveRDS(full, file= paste0("~/scr4-mdavis65/ssola1/HHlevel/Rdata/", name_year, "_hhweather.Rdata"))

# Finish message
cat(paste0("\n", name_year, " has finished processing", "\n"))

# Finish message
print(paste0(name_year, " has finished processing"))

rm (list=setdiff(ls(), c("countries", "location")))

}

# Combing HH Files --------------------------------------------------------

# Set the working directory to where the Rdata files are stored
setwd("~/data-mdavis65/steven_sola/4_HHlevel/Rdata")

# Load in the Rdata files
hh_files <- list.files(pattern = "*.Rdata", full.names = T)

# Read in the files into the environment
all_data <- lapply(hh_files, readRDS, .GlobalEnv)

# combine all the files together
hh_combined <- rbindlist(all_data, use.names = T, fill = T)

saveRDS(hh_combined, "~/data-mdavis65/steven_sola/4_HHlevel/hh_combined.Rdata")
fwrite(hh_combined, file = "~/data-mdavis65/steven_sola/4_HHlevel/hh_combined.csv")

# Take a sample of the hh_combined file
hh_combined_sample <- hh_combined[sample(nrow(hh_combined), size = 6000, replace = F),]

# Save the files
saveRDS(hh_combined_sample, "~/data-mdavis65/steven_sola/4_HHlevel/hh_combined_sample.Rdata")
fwrite(hh_combined_sample, file = "~/data-mdavis65/steven_sola/4_HHlevel/hh_combined_sample.csv")
