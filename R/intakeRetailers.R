# intakeRetailers.R
# Takes outputs from Geoscanning Retailer Code published as RMD

# Parameters:
# filepath - the location of the most recent retailer codebase, csv file

# Output:
# csv

# Next step is to convert to sf, buffer and join to geotracking observations


intakeRetailers <- function(filepath){
  
require(tidyverse)
  require(sf)
  
canonical_names <- c("county", "trade_name", "account", "license_type",
                       "expiration_date", "lat", "lon", "address_full",
                       "publish_date", "state", "expired_y_n")
  
  retailers <- read.csv(filepath) %>%
    dplyr::select(all_of(canonical_names)) %>%
    mutate_if(is.factor, as.character) %>%
    mutate(account = as.character(account)) %>%
    mutate(expiration_date = ymd(expiration_date),
           publish_date = ymd(publish_date))
  
ifelse(names(retailers) != canonical_names, 
       print("WARNING: Column names are non-standard"), 
       print("Column names match standard names"))

print("Data contain")

retailers %>%
  filter(is.na(lat) == TRUE) %>%
  nrow() %>%
  print(.)

print("NA geodata observations")
  
  return(retailers)
  
}

# Vignette
# retailersTest <- intakeRetailers("//jove.design.upenn.edu/Dept-Shares/prax/01 Project Folders/2019_Annenberg_GeoScanning/dataOutputs/all_Retailers_10_20_20.csv")
