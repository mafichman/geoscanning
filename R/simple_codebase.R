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
library(viridis)
library(lubridate)
library(mapview)
library(ggmap)
library(jsonlite)
library(RSocrata)
library(leaflet)
library(leaflet.providers)
library(devtools)

devtools::install_github("nyu-mhealth/Mobility")
library(Mobility)


uploadGeodata <- function(filePath){
  
  require(tidyverse)
  require(jsonlite)
  
  loadJSON <- function(jsonFilename){fromJSON(jsonFilename, 
                                              simplifyDataFrame = TRUE, 
                                              flatten = TRUE)%>%
      .[1] %>% 
      as.data.frame() %>%
      mutate(lat = locations.latitudeE7 / 1e7, # put the decimal in the right place for the lat
             lon = locations.longitudeE7 / 1e7, # put the decimal in the right place for the lat
             datetime = as.POSIXct(as.numeric(locations.timestampMs)/1000, # go from unix time to POSIXct date time
                                   origin = "1970-01-01")) %>%
      select(datetime, lat, lon, locations.altitude, 
             locations.velocity, locations.accuracy) # select only what you need
    
  }
  
  paths <- dir(filePath, pattern = "\\.json$", full.names = TRUE)
  
  names(paths) <- basename(paths)
  
  myData <- map_dfr(paths, loadJSON, .id = "filename")
  
  return(myData)
  
}

cleanDates <- function(df, dateRangeMin, dateRangeMax, fileName){
  
  require(tidyverse)
  require(lubridate)
  
  cleaned.df <- df %>%
    mutate(keep = case_when(filename == fileName & 
                              datetime > dateRangeMin & 
                              datetime < dateRangeMax ~ 1)) %>%
    filter((is.na(keep) == FALSE & filename == fileName) | filename != fileName)%>%
    dplyr::select(-keep)
  
  return(cleaned.df)
  
}


spaceTimeLags <- function(dataframe){
  
  require(tidyverse)
  require(sf)
  require(lubridate)
  
  # Convert data frame to sf
  dataframe <- dataframe %>%
    filter(is.na(lat) == FALSE & is.na(lon) == FALSE) %>%
    st_as_sf(., coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    ungroup() %>%
    st_transform(crs = 2272) %>%
    mutate(y_ft=map_dbl(geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
           x_ft=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
    group_by(filename)%>%
    arrange(datetime)%>%
    mutate(lagDist_ft = sqrt(((lag(x_ft) - x_ft)^2) + ((lag(y_ft) - y_ft)^2)),
           lagTime = abs(datetime - lag(datetime)), 
           lagmph = (lagDist_ft/5280) / (as.numeric(lagTime) / (60*60)),
           leadDist_ft = sqrt(((lead(x_ft) - x_ft)^2) + ((lead(y_ft) - y_ft)^2)),
           leadTime = abs(datetime - lead(datetime)), 
           leadmph = (leadDist_ft/5280) / (as.numeric(leadTime) / (60*60)),
           lead_lag_avg_mph = (leadmph + lagmph)/2,
           mean_3_lagDist_ft = (lag(lagDist_ft, 1) + lag(lagDist_ft, 2) + lag(lagDist_ft, 3))/3) %>%
    dplyr::select(-c(x_ft, y_ft, leadDist_ft, leadmph))
  
  return(dataframe)
}

testAll <- uploadGeodata("//jove.design.upenn.edu/Dept-Shares/prax/01 Project Folders/2019_Annenberg_GeoScanning/inputData/Test kml and json timeline data/Test kml and json timeline data/multi_json_test") %>%
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

#######

# Buffer and join routine

bufferAndJoin <- function(retailersData, geotrackingData, inputCRS, inputDistance){
  
  retailers_buffer <- retailersData %>%
    filter(is.na(lat) == FALSE) %>%
    st_as_sf(., coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs = inputCRS) %>%
    st_buffer(., inputDistance) # the number here is the buffer size in feet
  
  invalidShapes <- retailers_buffer %>%
    mutate(valid_shape = st_is_valid(.)) %>%
    filter(is.na(valid_shape) == TRUE) %>%
    nrow()
  
  print("Retailer data contain")
  print(invalidShapes)
  print("invalid shapes")
  
  cleanData_join_buffers <- st_join(geotrackingData %>%
                                      st_transform(crs = 4326), 
                                    retailers_buffer %>%
                                      st_transform(crs = 4326), 
                                    join = st_intersects, 
                                    left = TRUE)
  
  return(cleanData_join_buffers)
  
}