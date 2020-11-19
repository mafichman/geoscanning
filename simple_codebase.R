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

# Upload data and add space/time indicators
# Specify the following parameters:
# 1. uploadGeodata - specify the filepath (default is our test data)
# 2. Add a "cleanDates" call for each user with a time window specifying beginning and end times
# using a YYYY-MM-DD HH:MM:SS format
# 3. Specify the stayevent parameters - dist.threshold in meters, time.units in minutes
# 4. Specify the time window for measuring radiusofgyration

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
  spaceTimeLags(.)

#######

# Output workspace, shp, csv

# Specify a filepath outside the geoscanning repo (on your machine perhaps)
# to write out a workspace and/or csv and shapefile versions of your data

# save.image("your_file_path/fileName.RData")
# st_write(cleanData, "your_file_path/fileName.shp")
# write.csv(cleanData %>% as.data.frame() %>% "yourfile_path/fileName.csv")

#######

# Load Retailers

# Parameters - specify the location of the most recent retailer data set

retailers <- intakeRetailers("Data/Retailers/all_Retailers_10_20_20.csv")

# Associate retailers and Census tract info to geotracking observations

cleanData_Retailers_Tracts <- cleanData %>%
  bufferAndJoin(retailers, ., 2272, 100) %>%
  joinTracts(.)

# Output workspace, shp, csv

# Specify a filepath outside the geoscanning repo (on your machine perhaps)
# to write out a workspace and/or csv and shapefile versions of your data

# save.image("your_file_path/fileName.RData")
# st_write(cleanData_Retailers_Tracts, "your_file_path/fileName.shp")
# write.csv(cleanData_Retailers_Tracts %>% as.data.frame() %>% "yourfile_path/fileName.csv")
  