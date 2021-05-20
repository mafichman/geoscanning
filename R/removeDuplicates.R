# removeDuplicates.R

# INCOMPLETE PENDING COMMENTS FROM TOM

# This function removes duplicated timestamps using a logic
# which tries to determine which duplicate is most likely to be an outlier based
# on time/space lag readings

# requirements - sf, tidyverse, lubridate

# Testing this out - uploading the clean test data set
cleanData <- uploadGeodata("Data/Geotracking/multi_json_test") %>%
  cleanDates(., "2019-04-22 21:30:45", "2019-08-05 15:17:47", "file1.json") %>% # parameter
  cleanDates(., "2019-04-22 21:30:45", "2019-08-05 15:17:47", "file2.json")

nrow(cleanData) == length(unique(cleanData$datetime))

#There are no duplicates, so we will randomly make some - generate some random
# numbers in a column, for 5% of them we will make their datetime equal to the 
# lead datetime
# Then reset the datetime as dttm (it loses this for some reason)

cleanData <- cleanData %>%
  mutate(random = runif(nrow(cleanData), min = 0, max = 40),
         datetime = ifelse(random > 38, lead(datetime), datetime),
         datetime = as.POSIXct(as.numeric(datetime), # go from unix time to POSIXct date time
                               origin = "1970-01-01"))

nrow(cleanData) == length(unique(cleanData$datetime))

# create a variable for myCRS - this will go in the function

myCRS <- 2272

# Calculate lag distance, group by file and timestamp and then arrange
# in ascending order of lag distance
# keep only the first observation
# then ungroup

# THIS DOESN'T QUITE WORK BECAUSE THE LAGS ARE CREATED USING THE DUPLICATES

test <- cleanData %>%
  filter(is.na(lat) == FALSE & is.na(lon) == FALSE) %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  ungroup() %>%
  st_transform(crs = myCRS) %>%
  mutate(y_ft=map_dbl(geometry, ~st_centroid(.x)[[1]]),
         x_ft=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
  group_by(filename)%>%
  arrange(datetime)%>%
  mutate(lagDist_ft = sqrt(((lag(x_ft) - x_ft)^2) + ((lag(y_ft) - y_ft)^2))) %>%
  group_by(filename, datetime) %>%
  arrange(lagDist_ft) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(-c(x_ft, y_ft, lagDist_ft)) %>%
  ungroup()