# Update PA Retailers Only

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

## 4.1. Load new data from Socrata

retailers_Socrata_pa <- read.socrata("https://data.pa.gov/resource/ut72-sft8.json") %>%
  filter(county != "UNKNOWN/OUT OF STATE",
         license_type != "Vending") %>%
  mutate(publish_date = today(tzone = "America/New_York"),
         state = "PA",
         expiration_date = ymd(expiration_date)) %>%
  rename(lat = location_1.latitude,
         lon = location_1.longitude,
         address_full = location_1.human_address) %>%
  dplyr::select(-legal_name, -postal_code, -country) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(address_full = str_replace(address_full, "&", "AND"))

## If you are loading data from file, you can use the following code:
# Make sure to change the input date to be the date of DL for the data set

retailers_Socrata_pa <- read.csv("~/GitHub/geoscanning/Data/Retailers/PA/2023_08/Tobacco_Products_Tax_Licenses_Current_Monthly_County_Revenue.csv") %>%
  rename(county = County,
         legal_name = Legal.Name,
         trade_name = Trade.Name,
         postal_code = Postal.Code,
         country = Country,
         account = Account.Code,
         license_type = License.Type,
         expiration_date = Expiration.Date,
         location_1.human_address = Address...Lat.Long) %>%
  filter(county != "UNKNOWN/OUT OF STATE",
         license_type != "Vending") %>%
  mutate(coordinates = str_extract(location_1.human_address, "\\((-?[0-9.]+), (-?[0-9.]+)\\)")) %>%
  separate(coordinates, into = c("latitude", "longitude"), sep = ",\\s*", convert = TRUE) %>%
  mutate(lat = str_remove(latitude, "\\("),
         lon = str_remove(longitude, "\\)")) %>%
  mutate(cleaned_text = str_replace_all(location_1.human_address, "\n", " ")) %>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\(.*?\\)")) %>%
  separate(cleaned_text, into = c("address"), sep = "\\s{2,}", extra = "drop") %>%
  dplyr::select(-legal_name, -postal_code, -country,
                -latitude, -longitude, -location_1.human_address) %>%
  mutate(publish_date = ymd("2023-08-01"),
         state = "PA",
         expiration_date = mdy(expiration_date)) %>%
  rename(address_full = address) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(address_full = str_replace(address_full, "&", "AND"))


# Load stored retailer database, filter for only PA observations

stored_pa <- read.csv("~/GitHub/geoscanning/Data/Retailers/all_Retailers_8_30_23_2023_07.csv") %>%
  dplyr::select(canonical_names) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(expiration_date = ymd(expiration_date),
         publish_date = ymd(publish_date)) %>%
  filter(state == "PA")

# Join the new and the stored data

joined_pa <- full_join(stored_pa, 
                       retailers_Socrata_pa, by = c("account", "trade_name")) %>%
  mutate(expired_y_n = ifelse(is.na(expiration_date.y) == TRUE, "EXPIRED", "ACTIVE"),
         expiration_date = if_else(expired_y_n == 'EXPIRED', expiration_date.x, ymd(expiration_date.y)),
         lat = ifelse(is.na(lat.x) == FALSE, lat.x, lat.y),
         lon = ifelse(is.na(lon.x) == FALSE, lon.x, lon.y),
         address_full = ifelse(is.na(address_full.x) == FALSE, address_full.x, address_full.y),
         county = ifelse(is.na(county.y) == TRUE, county.x, county.y),
         publish_date = if_else(is.na(publish_date.x) == TRUE, publish_date.y, publish_date.x),
         license_type = ifelse(is.na(license_type.y) == TRUE, license_type.x, license_type.y),
         state = "PA") %>%
  dplyr::select(-lat.x, -lon.x, -lat.y, -lon.y, 
                -address_full.x, -address_full.y, -county.x, -county.y,
                -publish_date.x, -publish_date.y, -state.x, -state.y, -license_type.x, -license_type.y,
                -expiration_date.x, -expiration_date.y)

# See how many NAs we have in our geolocations

summary(is.na(joined_pa$lat))

# Do a first pass at geocoding

to_geocode_pa <- joined_pa %>%
  filter(is.na(lat) == TRUE)

geocoded_pa <- geocode(to_geocode_pa$address_full, source =  "google") %>%
  cbind(., to_geocode_pa %>%
          select(-lat, -lon)) %>%
  rbind(joined_pa %>%
          filter(is.na(lat) == FALSE), .)

summary(is.na(geocoded_pa$lat))

# If there is nothing to geocode, run the following
# geocoded_pa <- joined_pa

# Perform an error check for bad geocodes

errors_pa <- st_join(geocoded_pa %>% 
                       filter(is.na(lat) == FALSE) %>% 
                       st_as_sf(., coords = c("lon", "lat"), crs = 4326), 
                     pa_shp, 
                     join = st_within, 
                     left = TRUE) %>%
  filter(is.na(STUSPS) == TRUE) %>%
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat=map_dbl(geometry, ~st_centroid(.x)[[2]]))%>%
  dplyr::select(canonical_names) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  rbind(., geocoded_pa %>%
          filter(is.na(lat) == TRUE))

# Manually geocode failures

# the filters are for retailers or manufacturers not in PA, the mutates are for lat/lon failures for places in PA
# Keep these filters on and add more as needed - these suppliers show up in PA on and off

geocoded_pa_fixed <- geocoded_pa %>%
  filter(account != "02**1873") %>%
  filter(account != "33**3114") %>%
  filter(account != "63**8422") %>%
  filter(account != "32**8418") %>%
  filter(account != "64**8393") %>%
  filter(account != "28**0275") %>%
  filter(account != "06**4558") %>%
  filter(account != "24**8466") %>%
  filter(account != "22**8558") %>%
  filter(account != "46**3444") # %>%
  #mutate(lat = ifelse(account == "65**7908", 40.32395389003288,     lat),
  #       lon = ifelse(account == "65**7908", -79.78319044322932, lon))
  

# Append to the rest of the retailers and write it out

allStates_updated <- read.csv("~/GitHub/geoscanning/Data/Retailers/all_Retailers_8_30_23_2023_07.csv") %>%
  dplyr::select(canonical_names) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(account = as.character(account)) %>%
  mutate(expiration_date = ymd(expiration_date),
         publish_date = ymd(publish_date)) %>%
  filter(state != "PA") %>%
  rbind(., geocoded_pa_fixed)

write.csv(allStates_updated, "~/GitHub/geoscanning/Data/Retailers/all_Retailers_8_30_23_2023_08.csv")
