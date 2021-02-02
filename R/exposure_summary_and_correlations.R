# First thing needed is outlier ID

library(anomalize)

# We detect observations outside the interquartile range using the `anomalize` package - 
# which is an outlier analysis package designed to play well with the tidyverse.

# We start by creating a data frame called `outliers` where we use the 
# `iqr` function to create new columns (denoted as variable.outlier) that simply 
# consist of "Yes" or "No" observations that denote whether the relevant observation 
# is outside the interquartile range for that variable across our entire data set.

# The parameters of the `iqr` function are described as follows in the `anomlalize` 
# [reference material](https://business-science.github.io/anomalize/reference/anomalize.html):
  
#  "The IQR Method uses an interquartile range of 25 the median. With the default alpha = 0.05, 
# the limits are established by expanding the 25/75 baseline by an IQR Factor of 3 (3X). 
# The IQR Factor = 0.15 / alpha (hence 3X with alpha = 0.05). 
# To increase the IQR Factor controling the limits, decrease the alpha, 
# which makes it more difficult to be an outlier. 
# Increase alpha to make it easier to be an outlier."

cleanData <- cleanData %>%
  mutate(rg_hr.outlier = iqr(rg_hr, verbose = FALSE),
         lead_lag_avg_mph.outlier = iqr(lead_lag_avg_mph, verbose = FALSE),
         lagDist_ft.outlier = iqr(lagDist_ft, verbose = FALSE),
         mean_3_lagDist_ft.outlier = iqr(mean_3_lagDist_ft, verbose = FALSE))


# Code adapted from Ken Steif - Public Policy Analytics

correlation.long <-
  st_drop_geometry(cleanData) %>%
  filter(rg_hr.outlier == "No" &
           lead_lag_avg_mph.outlier == "No" &
           lagDist_ft.outlier == "No" &
           mean_3_lagDist_ft.outlier == "No") %>%
  dplyr::select(rg_hr, lead_lag_avg_mph, mean_3_lagDist_ft, lagDist_ft, hour) %>%
  gather(Variable, Value, -rg_hr)

correlation.cor <-
  correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, rg_hr, use = "complete.obs"))

ggplot(correlation.long %>%
         filter(rg_hr < 20000), aes(Value, rg_hr)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~Variable, ncol = 2, scales = "free") +
  labs(title = "rg_hr as function of continuous variables")


ggplot(cleanData)+
  geom_freqpoly(aes(hour)) +
  facet_grid(dotw~filename, scales = "free")+
  labs(
    title = "Time series of geo-location hits by subject",
    y = "Number of observations",
    x = "Hour")

ggplot(cleanData)+
  geom_point(aes(x =hour, y = rg_hr)) +
  facet_grid(filename~dotw, scales = "free")+
  labs(
    title = "Time series of geo-location hits by subject",
    y = "Radius of gyration",
    x = "Hour")

cleanData_Retailers_Tracts %>% 
  as.data.frame() %>% ungroup() %>%
  filter(is.na(trade_name)== FALSE) %>% 
  group_by(filename, lat, lon, datetime) %>% 
  slice(1) %>% 
  ungroup() %>% 
  group_by(filename) %>% 
  tally()