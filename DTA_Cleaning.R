#########################
# Required for Server R #
#########################

module <- function(...){
  arglist <- as.list(match.call())
  ## change this from 'module' to 'r'
  arglist[1] <- 'r'
  args <- paste(arglist, collapse = ' ', sep = '')
  
  binary <- "/data/apps/linux-centos8-cascadelake/gcc-9.3.0/lmod-8.3-tbez7qmxvu3dwikqbs3hdafke5vcsbxv/lmod/lmod/libexec/lmod"
  
  cmd <- paste(binary, args, sep = ' ')
  
  hndl <- pipe(cmd)
  eval(expr = parse(file = hndl))
  
  close(hndl)
  
  invisible(0)
  
  # Update Library
  if  ( grepl(":", Sys.getenv("R_LIBS"), fixed = TRUE) ) {
    new <- strsplit(path.expand(Sys.getenv("R_LIBS")), split = ":")
    new <- unlist(new,recursive=F)
  } else {
    new <- Sys.glob(path.expand(Sys.getenv("R_LIBS")))
  }
  
  paths <- c(new, .Library.site, Sys.getenv("R_LIBS_USER"))
  paths <- paths[dir.exists(paths)]
  .lib.loc <<- unique(normalizePath(paths, "/"))
  .libPaths(.lib.loc)
}

# Dynamic Loading
dyn.load("/data/apps/extern/udunits/2.2.28/lib/libudunits2.so.0")
dyn.load("/data/apps/extern/anaconda/envs/gdal/3.4.1/lib/libgdal.so.30")
dyn.load("/home/ssola1/rlibs/4.0.2/gcc/9.3.0/tibble/libs/tibble.so")

module("load", "r/4.0.2")
module("load", "r-shiny/1.3.2")
module("load", "r-tidyverse")
module("load", "r-stringi/1.4.3")

################
# WORKING CODE #
################

# Load Packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(magrittr)

# Set Working Directory
setwd("~/data-mdavis65/steven_sola/")

# Read in the files, and combine them
dta <- list.files("/data/mdavis65/steven_sola", pattern="*.SHP", full.names = TRUE, recursive = TRUE)
newfiles <- gsub(".dta$", ".DTA", dta)
file.rename(dta, newfiles)

