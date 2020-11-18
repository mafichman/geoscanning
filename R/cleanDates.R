# cleanDates.R

# Clean Dates to remove geodata outside study range

# Takes outputs from function uploadGeodata

# Parameters:
# df - a dataframe output from uploadGeodata
# dateRangeMin - a minimum bound (noninclusive) of the date range. format "yyyy-mm-dd hh:mm:ss" using a 24hr time for hour
# dateRangeMax - a minimum bound (noninclusive) of the date range. format "yyyy-mm-dd hh:mm:ss" using a 24hr time for hour
# fileName - a name for the the json file for the subject in question e.g. "file1.json" - found in the column filename of the output from uploadGeodata


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

#  Vignettes:
#  Clean dates for a single subject, filename 'file1.json``
#  testCleanDatesIndividual <- cleanDates(test, "2019-04-22 21:30:45", "2019-08-05 15:17:47", "file1.json")

# Clean dates for multiple subjects
# testCleanDatesMultiple <- test %>%
#                          cleanDates(., "2019-04-22 21:30:45", "2019-08-05 15:17:47", "file1.json") %>%
#                          cleanDates(., "2019-04-22 21:30:45", "2019-08-05 15:17:47", "file2.json")