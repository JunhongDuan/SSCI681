library(ggplot2)
library(ggmap)
library(tigris)
library(janitor)
library(sf)
library(spdep)

library(kableExtra)

library(tmap)
### HEAT DATA TABLES 

# total crime count
crime_count <- la_crime %>%
  st_drop_geometry() %>%
  group_by(year, offense_against, offense_group, offense_type) 

# generate table 1-2
crime_count %>% tabyl(offense_group, year) %>%
  adorn_totals(where = "row") %>%
  adorn_totals(where = "col") %>%
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting() %>%                  # convert to percents
  adorn_ns(position = "front") %>% 
  knitr::kable() %>%
  kable_styling("basic")
  
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

# moran's I 
coords <- st_coordinates(crime_tract_2019) # Create a spatial weights list using Queen contiguity
nb <- poly2nb(crime_tract_2019)  # Create neighbors list
lw <- nb2listw(nb, style = "W")  # Convert to a spatial weights list

moran_test_crime_rate <- moran.test(crime_tract_2019$crime_rate, lw)
print(moran_test_crime_rate)

moran_test_crime_ct <- moran.test(crime_tract_2019$n, lw)
print(moran_test_crime_ct)
