# Update DE Retailers Only

# Code is fully documented in markdown entitled clean_retailers_v3.Rmd / clean_retailers_v3.html

# --- Load Packages ----

library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(lubridate)
library(ggmap)
library(jsonlite)
library(RSocrata)
library(readxl)
library(leaflet)
library(leaflet.providers)
library(readxl)

# Load API key

register_google(key = "YOUR KEY GOES HERE")

# Load canonical names

canonical_names <- c("county", "trade_name", "account", "license_type",
                     "expiration_date", "lat", "lon", "address_full",
                     "publish_date", "state", "expired_y_n")

# Load states for trouble shooting

states_shp <- states()  %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

pa_shp <- states_shp %>%
  filter(STUSPS == "PA")

nj_shp <- states_shp %>%
  filter(STUSPS == "NJ")

de_shp <- states_shp %>%
  filter(STUSPS == "DE")

# Read new data from socrata

## NOTE THAT THE cateogries have changed for how they describe tobacco retailers!

retailers_Socrata_de <- read.socrata("https://data.delaware.gov/resource/5zy2-grhr.json") %>%
  filter(str_detect(category, "CIGARETTE/TOBACCO PRODUCTS SELLER") | 
           str_detect(category, "TOBACCO RETAILER")) %>%
  mutate(publish_date = ymd(current_license_valid_from),
         expiration_date = ymd(current_license_valid_to),
         expired_y_n = "ACTIVE",
         trade_name = business_name,
         county = NA,
         state = "DE") %>%
  rename(lat = geocoded_location.latitude,
         lon = geocoded_location.longitude,
         account = license_number,
         license_type = category) %>%
  mutate(zip = ifelse(str_length(zip) == 9, str_sub(zip, end = -5), zip)) %>%
  filter(zip >= 19701 & zip <= 19980) %>%
  unite(., "address_full", sep = " ",
        c("address_2", "city", "state", "zip")) %>%
  mutate(state = "DE") %>%
  dplyr::select(-business_name, -current_license_valid_from, 
                -current_license_valid_to, -country, -geocoded_location.human_address,
                -address_1) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(address_full = str_replace(address_full, "&", "AND"))

# Load stored data

stored_de <- read.csv("~/GitHub/geoscanning/Data/Retailers/all_Retailers_5_26_21.csv") %>%
  dplyr::select(canonical_names) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(expiration_date = ymd(expiration_date),
         publish_date = ymd(publish_date)) %>%
  filter(state == "DE")
