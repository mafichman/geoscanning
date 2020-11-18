# bufferAndJoin.R

# parameters
# retailersData - a csv with lat/lon that conforms to our naming scheme
# geotrackingData - 
# inputCRS - An an appropriate coordinate reference system for the spatial extent of the data
# inputDistance - Buffer distance for the retailers, in the native linear unit of the coordinate system

# Returns an sf object consisting of each observation joined to retailers whose buffers they interesect
# Observations which join two multiple overlapping retailers will appear multiple times

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

# Vignette

testBuffers <- bufferAndJoin(retailersTest, testAll, 2272, 100)
