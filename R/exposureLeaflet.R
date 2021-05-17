# exposureLeaflet.R

# Visualizes geotracking observations subdivided by subject
# With stay events visualized by size and exposures in red
# Parameters - dataSet
# dataSet needs to have gone through retailer spatial join

exposureLeaflet <- function(dataSet){
  
  require(tidyverse)
  require(leaflet)
  require(leaflet.providers)
  require(leaflet.extras)
  
  dataSet.df <- dataSet %>% 
    as.data.frame() %>% ungroup() %>%
    mutate(filename = as.factor(filename)) %>% #BM: I added this because it appears to be necessary for split() in line 35.
    filter(is.na(trade_name)== FALSE & lead_lag_avg_mph < 30) %>% 
    group_by(filename, lat, lon, datetime) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(is_stay_event = ifelse(is.na(stayeventgroup) == FALSE, 1, 0),
           likely_active_exposure = ifelse(datetime < expiration_date, 1, 0)) %>%
    rbind(., dataSet %>%
            filter(is.na(trade_name)== TRUE) %>%
    mutate(is_stay_event = ifelse(is.na(stayeventgroup) == FALSE, 1, 0),
           likely_active_exposure = 0))

  
  l <- leaflet() %>% 
    addProviderTiles(providers$Esri.WorldTopoMap) %>%
    setView(lng = mean(dataSet$lon, na.rm = TRUE), mean(dataSet$lat, na.rm = TRUE), zoom = 07) %>%
    addScaleBar(position = "topleft")
  #BM: The first argument below generates a df with 2 observations of 84 variables. Attempting to use View on that df froze my RStudio.
  dataSet.df2 = split(dataSet.df %>% as.data.frame(), dataSet.df$filename)

  
  names(dataSet.df2) %>%
    purrr::walk( function(df) {
      
      pal <- colorFactor(c("navy", "red"), domain = c(1, 0))
      
      l <<- l %>%
        addCircleMarkers(data=dataSet.df2[[df]],
                         lng=~lon, 
                         lat=~lat,
                         radius = ~ifelse(is_stay_event == 1, 8, 3), 
                         fillOpacity =~ .5,
                         color =~pal(likely_active_exposure),
                         label=~paste("datetime:", datetime, ", Active (0/1)", likely_active_exposure, trade_name ),
                         group = df)
    })
  
  
  l %>%
    addLayersControl(
      overlayGroups = names(dataSet.df2),
      options = layersControlOptions(collapsed = FALSE))
}

# Vignette

# exposureLeaflet(cleanData_Retailers_Tracts)