library(ggplot2)
library(ggmap)
library(tigris)
library(janitor)
library(sf)
library(kableExtra)
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
  
# moran's I 