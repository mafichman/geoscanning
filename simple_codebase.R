# Abbreviated Code Base For Cleaning Geo-Data
# Michael Fichman / Falk Lab / Geoscanning
# 11/16/2020

# Chain the intake functions together
# preceded by: running functions, loading libraries
# can these functions be sent to a github repo and loaded externally?
# after this will be - writing out the workspace, csv and shp


library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(lubridate)
library(jsonlite)
library(devtools)

devtools::install_github("nyu-mhealth/Mobility")
library(Mobility)

# Install functions from R folder in the geoscanning repo

source("R/uploadGeodata.R")
source("R/cleanDates.R")
source("R/spaceTimeLags.R") 
source("R/intakeRetailers.R")
source("R/bufferAndJoin.R")
source("R/joinTracts.R")
source("R/indirectMLM.R")

# Upload data and add space/time indicators
# Specify the following parameters:
# 1. uploadGeodata - specify the filepath (default is our test data)
# 2. Add a "cleanDates" call for each user with a time window specifying beginning and end times
# using a YYYY-MM-DD HH:MM:SS format
# 3. Specify the stayevent parameters - dist.threshold in meters, time.units in minutes
# 4. Specify the time window for measuring radiusofgyration

# Output is an sf object

cleanData <- uploadGeodata("Data/Geotracking/multi_json_test") %>%
  cleanDates(., "2019-04-22 21:30:45", "2019-08-05 15:17:47", "file1.json") %>% # parameter
  cleanDates(., "2019-04-22 21:30:45", "2019-08-05 15:17:47", "file2.json") %>% # parameter
  stayevent(., 
            coor = c("lon","lat"), 
            time = "datetime", 
            dist.threshold = 100/3.28084, # conversion to feet PARAMETER
            time.threshold = 5, # PARAMETER
            time.units = "mins", 
            groupvar = "filename") %>%
  mutate(rg_hr = radiusofgyration(., 
                                  coor = c("lon","lat"), 
                                  time = "datetime", 
                                  time.units = "hour", # PARAMETER
                                  groupvar = "filename")) %>%
  spaceTimeLags(.) # BM: could add a parameter for crs here
# BM: Warning message from NYU function:
# `group_by_()` is deprecated as of dplyr 0.7.0.
# Please use `group_by()` instead.
# See vignette('programming') for more help

#######

# Output workspace, shp, csv

# Specify a filepath outside the geoscanning repo (on your machine perhaps)
# to write out a workspace and/or csv and shapefile versions of your data

# save.image("your_file_path/fileName.RData")
# write.csv(cleanData %>% as.data.frame(), "yourfile_path/fileName.csv")

#######

# Load Retailers

# Parameters:
# 1. specify the location of the most recent retailer data set

retailers <- intakeRetailers("Data/Retailers/all_Retailers_10_20_20.csv")

# Associate retailers and Census tract info to geotracking observations

# Parameters:
# bufferAndJoin takes four parameters:
# 1. a retailer database (retailers)
# 2. the geotracking data (here as `.`)
# 3. A crs (coordinate reference system) - keep default 2272 for Philadelphia area (linear unit - feet)
# 4. A buffer size in the linear units of the crs - (defaulted below to 100 feet)

cleanData_Retailers_Tracts <- cleanData %>%
  bufferAndJoin(retailers, ., 2272, 100) %>%
  joinTracts(.)

# Output workspace, shp, csv

# Specify a filepath outside the geoscanning repo (on your machine perhaps)
# to write out a workspace and/or csv and shapefile versions of your data

# save.image("your_file_path/fileName.RData")
# write.csv(cleanData_Retailers_Tracts %>% as.data.frame(), "yourfile_path/fileName.csv")
  