library(ggplot2)
library(ggmap)
library(tigris)
library(janitor)
library(sf)
library(spdep)
library(dplyr)
library(lubridate)
library(kableExtra)

library(tmap)
### HEAT DATA TABLES 

offense_against <- unique(la_crime$offense_against)

# total crime count
crime <- la_crime %>%
  st_drop_geometry() %>%
  group_by(year, offense_against, offense_group, offense_type) 

# generate table 1-2
crime %>% tabyl(offense_group, year) %>%
  adorn_totals(where = "row") %>%
  adorn_totals(where = "col") %>%
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting() %>%                  # convert to percents
  adorn_ns(position = "front") %>% 
  knitr::kable() %>%
  kable_styling("basic")

crime_count_type <- la_crime %>%
  group_by(month=floor_date(as.Date(date), unit="month"), offense_against, offense_group, offense_type) %>%
  summarize(n=n()) 
  
crime_count_offense_against <- crime %>%
  group_by(year, offense_against) %>%
  summarize(n=n())

# line graph of crime over time 

crime_count_offense_against %>% 
  ggplot(aes(x=as.Date(as.character(year), format="%Y-%M"), y=n, color=offense_against)) +
  geom_point() +
  geom_line() +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  theme(axis.text.x.top = element_text(vjust = 0.5))
  #facet_wrap(~offense_group)

for(offense in offense_against) { 
  print(offense)
  
  print(crime_count_type %>%
          filter(offense_against == offense) %>%
          ggplot(aes(x=month, y=n, color=offense_type, group=offense_type)) +
          geom_point() +
          geom_line() + 
          # scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
          theme(
            plot.title = element_text(size=11), 
            axis.text.x = element_text(size=9, angle = 90, 
                                       vjust = 0.5, 
                                       hjust=1, 
                                       lineheight=0.75)) +
          scale_x_date(NULL, date_labels = "%b %y",breaks="month") +
          ggtitle(offense))
}

crime_tract_2019 <- crime_tract_ag %>% filter(year==2019)

# map count
tm_shape(crime_tract_2019) + 
  tm_polygons(col="n", size=.1, style="fisher") +
  # tm_shape(la_water) +
  # tm_polygons(col="lightblue", border.alpha = 0, alpha=.5) +
  # tm_shape(la_parks) +
  # tm_polygons(col="#6E9A81", border.alpha = 0, alpha=.5) +
  tm_compass(size=1) +
  tm_scale_bar(position = c("left", "bottom"))

# map crime rate per 1000
tm_shape(crime_tract_2019) + 
  tm_polygons(col="crime_rate", size=.1, style="fisher") +
  # tm_shape(la_water) +
  # tm_polygons(col="lightblue", border.alpha = 0, alpha=.5) +
  # tm_shape(la_parks) +
  # tm_polygons(col="#6E9A81", border.alpha = 0, alpha=.5) +
  tm_compass(size=1) +
  tm_scale_bar(position = c("left", "bottom"))

# descriptive statistics for crime categories

# count and crime rate (mean, max, min) of each crime category for all years 
## crime rate aggregated
crime_catg_desc_data <- crime_tract_ag_type %>% 
  group_by(offense_against, offense_group, offense_type) %>% 
  summarise(
    n_mean = mean(n),
    n_max = max(n),
    n_min = min(n),
    n_sd = sd(n),
    crime_rate_mean = mean(crime_rate),
    crime_rate_max = max(crime_rate),
    crime_rate_min = min(crime_rate),
    crime_rate_sd = sd(crime_rate)
  ) %>% 
  st_drop_geometry()


for(offense in offense_against) { 
  print(offense)
  print(crime_tract_ag_type %>%
    filter(offense_against == offense) %>%
    ggplot(aes(x=offense_type, y=n, fill=offense_type)) +
    geom_boxplot() +
    # scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
    theme(
      legend.position="none",
      plot.title = element_text(size=11), 
      axis.text.x = element_text(size=9, angle = 90, 
                                 vjust = 0.5, 
                                 hjust=1, 
                                 lineheight=0.75)) +
    ggtitle(offense))
}

crime_tract_ag_type %>%
    ggplot(aes(x=offense_type, y=crime_rate, fill=offense_type)) +
    geom_boxplot() +
    # scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
    theme(
      legend.position="none",
      plot.title = element_text(size=11), 
      axis.text.x = element_text(size=9, angle = 90, 
                                 vjust = 0.5, 
                                 hjust=1, 
                                 lineheight=0.75))

write.csv(crime_catg_desc_data, "./data/crime_desc_data.csv")

# look at temporal aspects of data 
# crime count over time
crime_year <- crime_tract %>%
  st_drop_geometry() %>%
  #group_by(year, offense_against, offense_group, offense_type, GEOID) %>%
  group_by(year, date, time, offense_against) %>%
  summarise(n = n()) %>%
  mutate(date = as.Date(date),
         time = as.POSIXct(time, format = "%H:%M:%S"))


# plot crimes by date for year 2019
ggplot(crime_year %>% filter(year == 2019), aes(x=date, y=n, alpha=.5)) +
  geom_point(aes(colour=offense_against)) +
  theme(axis.text.x = element_text(size=6, angle = 90, 
                                   vjust = 0.5, 
                                   hjust=1, 
                                   lineheight=0.75)) +
  scale_x_date(date_breaks = "week", limits = NULL)
  


# plot crime by time
ggplot(crime_year %>% filter(year == 2019), aes(x=time, y=n, alpha=.5)) +
  geom_point(aes(colour=offense_against)) +
  theme(axis.text.x = element_text(size=6, angle = 90, 
                                   hjust=1, 
                                   lineheight=0.75)) +
  scale_x_time()

# moran's I 
coords <- st_coordinates(crime_tract_2019) # Create a spatial weights list using Queen contiguity
nb <- poly2nb(crime_tract_2019)  # Create neighbors list
lw <- nb2listw(nb, style = "W")  # Convert to a spatial weights list

moran_test_crime_rate <- moran.test(crime_tract_2019$crime_rate, lw)
print(moran_test_crime_rate)

moran_test_crime_ct <- moran.test(crime_tract_2019$n, lw)
print(moran_test_crime_ct)

