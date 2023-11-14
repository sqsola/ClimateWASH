## README

The project was created for my PhD dissertation work. I hope it is of service to other people. A goal of my research is to make my findings as reproducible as possible to ensure accuracy and to allow people to build upon it in the future.

<p xmlns:cc="http://creativecommons.org/ns#">

This work is licensed under <a href="http://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY-NC-SA 4.0<img src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" style="height:22px!important;margin-left:3px;vertical-align:text-bottom;"/><img src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" style="height:22px!important;margin-left:3px;vertical-align:text-bottom;"/><img src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" style="height:22px!important;margin-left:3px;vertical-align:text-bottom;"/><img src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1" style="height:22px!important;margin-left:3px;vertical-align:text-bottom;"/></a>

</p>

## Data Sources

#### DHS (Demographic and Health Surveys)

As of 2023, the DHS have collected data from 400 surveys in over 90 countries. These surveys are used to monitor population, health, and nutrition programs. These surveys are implemented in coordination with USAID and their host country partners. These datasets are free to use for researchers who have a specific and defined research question. I would encourage prospective users to follow the instructions at [this link](https://dhsprogram.com/data/Using-Datasets-for-Analysis.cfm) for more information.

#### ERA-5 Land (Copernicus Climate Change Service)

ERA-5 Land is a reanalysis dataset from the Copernicus Climate Change Service, which is implemented by the European Centre for Medium-Range Weather Forecasts. These data are at a spatial resolution of 0.1° x 0.1° (approximately 9km x 9km), and range from 1950 until 5 days prior to the present date. The files are available in either NetCDF v.4 or GRIB2 file formats. This project uses the NetCDF file format.

#### A Note on Data Structure

DHS Surveys come in "Recode" files, which are used to standardize questions throughout different countries and phases. If you are interested in using DHS data, I highly encourage you to read [this page](https://dhsprogram.com/data/Dataset-Types.cfm) about their data structure.

After I downloaded the data, I renamed each folder so it was more intelligible. I used the country codes already in use by DHS. Additionally, I renamed the dataset names to the following:

| Abbreviation | Folder Name  |
|--------------|--------------|
| HR           | hh           |
| PR           | hhmem        |
| KR           | child        |
| BR           | birth        |
| HW           | heightweight |
| WI           | wealth       |

As a result, to get to the survey, you would have to travel: Senegal --\> SN_9293 --\> SN_9293_hh --\> SNHR21FL.DTA

## Files

#### ERA5Land.R

#### Weather_Extraction_Data.R

#### Weather_Extraction_Data.R

#### StandardEDA.R

#### DTA_Cleaning.R
