library(crimedata)
libary(tidyverse)
library(dplyr)
library(lubridate)

crime_data <- get_crime_data(
  years = 2010:2019, 
  cities = c("Los Angeles"), 
  type = "core", 
  output="sf"
)

la_crime <- crime_data %>%
  tidyr::separate(date_single, c("date", "time"), sep = " ", remove = TRUE) %>%
  mutate(year = year(date))
