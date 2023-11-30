# Load Packages
library(tidyverse)
library(lubridate)
library(magrittr)

# Set Working Directory
setwd("~/data-mdavis65/steven_sola/")

# Read in the files, and combine them
dta <- list.files("/data/mdavis65/steven_sola", pattern="*.SHP", full.names = TRUE, recursive = TRUE)
newfiles <- gsub(".dta$", ".DTA", dta)
file.rename(dta, newfiles)