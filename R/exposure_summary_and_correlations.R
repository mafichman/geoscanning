# Code adapted from Ken Steif - Public Policy Analytics



correlation.long <-
  st_drop_geometry(cleanData) %>%
  dplyr::select(rg_hr, lead_lag_avg_mph, hour) %>%
  gather(Variable, Value, -rg_hr)

correlation.cor <-
  correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, rg_hr, use = "complete.obs"))

ggplot(correlation.long, aes(Value, rg_hr)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~Variable, ncol = 2, scales = "free") +
  labs(title = "Title")



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
    y = "Number of observations",
    x = "Hour")

cleanData_Retailers_Tracts %>% 
  as.data.frame() %>% ungroup() %>%
  filter(is.na(trade_name)== FALSE) %>% 
  group_by(filename, lat, lon, datetime) %>% 
  slice(1) %>% 
  ungroup() %>% 
  group_by(filename) %>% 
  tally()