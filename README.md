## README

The project was created for my PhD dissertation work. I hope it is of service to other people. A goal of my research is to make my findings as reproducible as possible to ensure accuracy and to allow people to build upon it in the future.

<p xmlns:cc="http://creativecommons.org/ns#">

This work is licensed under <a href="http://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY-NC-SA 4.0<img src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" style="height:22px!important;margin-left:3px;vertical-align:text-bottom;"/><img src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" style="height:22px!important;margin-left:3px;vertical-align:text-bottom;"/><img src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" style="height:22px!important;margin-left:3px;vertical-align:text-bottom;"/><img src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1" style="height:22px!important;margin-left:3px;vertical-align:text-bottom;"/></a>

</p>

## Data Sources

#### DHS (Demographic and Health Surveys)

As of 2025, the DHS have collected data from 400 surveys in over 90 countries. These surveys are used to monitor population, health, and nutrition programs. These surveys are implemented in coordination with USAID, ICF, and their host country partners. These datasets are free to use for researchers who have a specific and defined research question. I would encourage prospective users to follow the instructions at [this link](https://dhsprogram.com/data/Using-Datasets-for-Analysis.cfm) for more information.

#### ERA-5 Land (Copernicus Climate Change Service)

ERA-5 Land is a reanalysis dataset from the Copernicus Climate Change Service, which is implemented by the European Centre for Medium-Range Weather Forecasts (ECMWFR). These data are at a spatial resolution of 0.1° x 0.1° (approximately 9km x 9km), and range from 1950 until 5 days prior to the present date. The files are available in either NetCDF v.4 or GRIB2 file formats. This project uses the NetCDF file format.

#### A Note on Data Structure

DHS come in "Recode" files, which are used to standardize questions throughout different countries and phases. If you are interested in using DHS data, I highly encourage you to read [this page](https://dhsprogram.com/data/Dataset-Types.cfm) about their data structure.

After I downloaded the data, I renamed each folder so it was more intelligible. I used the country codes already in use by DHS. Additionally, I renamed the dataset names to the following:

| Abbreviation | Folder Name  |
|--------------|--------------|
| HR           | hh           |
| PR           | hhmem        |
| KR           | child        |
| BR           | birth        |
| HW           | heightweight |
| WI           | wealth       |

As a result, to get to the survey, you would have to navigate through the following file path: Senegal --\> SN_9293 --\> SN_9293_hh --\> SNHR21FL.DTA

## Files

### **Home Folder**

#### ERA5Land_Download.R

The ERA5Land.R file uses the `ecmwfr` package to download weather data to the local machine. This code starts by loading the files that have already been downloaded and "cleaning" the file names so that only the dates remain from the file names.

After this step, an artificial dataframe is created with the desired dates (in this case, we want to download weather from between February 5, 1990, and December 31, 2023.

The already-downloaded files are compared to this artificial dataset to see which dates have not yet been download. All missing dates are saved to a dataframe, and this dataframe is looped through the `ecmwfr` package to download the data. After each session (as downloading weather data could take a long time), the user is able to pick up again where they started, as the "missing" dates will get updated when the code is re-run.

#### Weather_Extraction.R

This script is designed to load in the survey data and extract the weather data for each cluster on their specific day. The weather data is taken from the ERA5-Land database, implemented by the European Centre for Medium-Range Weather Forecasts (ECMWF). More information about the weather variables used in this project can be found below, and at [this website](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview).

Accumulation Variables

| Variable (short name)      | Explanation                                                                                                                                                                                                  |
|---------------------------|---------------------------------------------|
| Total Evaporation (e)      | Accumulated amount of water that has evaporated from the Earth's surface.                                                                                                                                    |
| Surface Run Off (sro)      | Some water from rainfall, melting snow, or deep in the soil, stays stored in the soil. Otherwise, the water drains away, either over the surface (surface runoff), or under the ground (sub-surface runoff). |
| Sub-Surface Run Off (ssro) | Some water from rainfall, melting snow, or deep in the soil, stays stored in the soil. Otherwise, the water drains away, either over the surface (surface runoff), or under the ground (sub-surface runoff). |
| Total Precipitation (tp)   | Accumulated liquid and frozen water, including rain and snow, that falls to the Earth's surface.                                                                                                             |

Instantaneous Variables

| Variable (short name)                      | Explanation                                                                                                                |
|---------------------------|---------------------------------------------|
| 2m Temperature (t2m)                       | Temperature of air at 2 meters above the surface of land.                                                                  |
| Skin Temperature (skt)                     | Temperature at the surface of the Earth.                                                                                   |
| 2m Dewpoint (d2m)                          | Temperature to which the air, at 2 metres above the surface of the Earth, would have to be cooled for saturation to occur. |
| Leaf Area Index - low vegetation (lai_lv)  | One-half of the total green leaf area per unit horizontal ground surface area for low vegetation type.                     |
| Leaf Area Index - high vegetation (lai_hv) | One-half of the total green leaf area per unit horizontal ground surface area for high vegetation type.                    |

Additionally, this script makes the following conversions:

-   "skt", "t2m", and "d2m" variables from Kelvin to Celsius by subtracting 273.15 from each value.

-   "sro" and "tp" from meters to centimeters by multiplying each value by 100.

This script also assigns the Köppen-Geiger climate classification for each cluster at the "fine" resolution (nearest 100 decimal degree seconds) using the [kgc](https://cran.r-project.org/web/packages/kgc/kgc.pdf) package.

#### FileExtension_Cleaning.R

This script is a basic one which ensures all the .DTA file extensions (from the STATA programming language) are all uppercase. There are some countries in which the file extension is .dta, and the code will not run in these instances, however rare they are.

#### CodeGraveyard.R

Bits of code that I don't need anymore, but can't seem to get rid of in case I need to use them later.

## Study Aims

### **Aim 1**

Title: The Influence of Precipitation, Runoff, and Temperature on Water Collection Labor in sub-Saharan Africa, 1990-2022.

### **Aim 2**

Title: Association of Animal Ownership with under-5 Diarrhea Prevalence in sub-Saharan Africa

### **Aim 3**

Title: Association of Animal Ownership and Extreme Precipitation Events with under-5 Diarrhea Prevalence in sub-Saharan Africa

### **Aim 4**

Title: TBD

## Study Aims

#### Combine_Households.R

This code takes each "hh" (household-level) file and adds in the spatial and wealth variables (if they exist). After that, it loads in the weather data for each specific household/cluster, and creates new variables for each variable for the past 7, 14, 30, and 60 days. For the "accumulation" variables, it creates totals over the time periods, and for the "instantaneous" variables, it creates averages over the time period. See above for which variables belong to each grouping. Finally, it combines all the households together to form the final, uncleaned dataset.

#### Aim1_Cleaning.R

Taking the output from the "Combine_Households.R" file and cleaning the dataset. The main features of this code is focused on the variable "HV201", which is where households get their water for drinking. All 184 surveys used to create my dataset have unique codings for this variable. As a result, I had to write a for_loop that took the codebook for each survey (.MAP file), extracted the coding for each unique HV201, and combined them into a dataframe. That df was then left_joined with my main data so that each household had the correct coding for HV201. After that, I classified each as "improved" and "unimproved" according to the WHO classification scheme. Finally, I separated the dataset until "urban" and "rural" based on the variable "URBAN_RURA".

#### Aim1_EDA_Analysis.R

This code does the exploratory data analysis (EDA) and further analysis for Aim 1.

### **Aim 2**

Aim 1 is focused on prevalence of diarrhea in children under the age of 5 in the household and its associations with animal ownership.

#### Aim2_Cleaning.R

Work in progress...
