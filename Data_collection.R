library(crimedata)
library(tidyverse)
library(dplyr)
library(lubridate)
library(tigris)
library(osmdata)
library(osmextract)
library(tidycensus)


library(sf)
library(crsuggest)

state <- "CA"
county <- "Los Angeles"

# ------ GET MAP FEATURES ------ #

# osm parks 
osm_parks <- getbb("Los Angeles", display_name_contains = "United States") %>%
  opq() %>% 
  add_osm_feature(key = 'leisure', value="park") %>% 
  osmdata_sf() 

la_parks <- osm_parks$osm_polygons %>%
  st_transform(proj_crs) %>%
  st_union() # treat parks as one object

# tigris water
la_water <- area_water(state="CA", county="Los Angeles", year=2019) %>%
  st_transform(proj_crs)

# ------ GET CRIME DATA ------ #

# get crime data
crime_data <- get_crime_data(
  years = 2010:2019, 
  cities = c("Los Angeles"), 
  type = "core", 
  output="sf"
)

# get crs
proj_crs <- as.numeric(suggest_crs(crime_data)$crs_code[1])

la_crime <- crime_data %>%
  tidyr::separate(date_single, c("date", "time"), sep = " ", remove = TRUE) %>%
  st_transform(crs=proj_crs) %>%
  mutate(year = year(date))


# la geographic data for one year, TODO: make table of all years
la_pop <- get_acs(
  geography = "tract",
  variables = "B01003_001", # Total population
  state = state,
  county = county,
  year = 2019, 
  survey = "acs5" 
) 

la_tracts <- tracts(county = "Los Angeles", year = 2019, state = "CA") %>%
  merge(la_pop, by="GEOID") %>%
  st_transform(proj_crs)

crime_tract <- la_tracts %>%
  st_join(la_crime, join = st_contains) 

crime_tract_ag <- crime_tract %>%
  st_drop_geometry() %>%
  #group_by(year, offense_against, offense_group, offense_type, GEOID) %>%
  group_by(year, GEOID) %>%
  summarise(n = n()) %>%
  merge(la_tracts, by="GEOID") %>%
  filter(estimate > 10) %>% # population per tract must be greater than 10
  mutate(crime_rate= 1000*n/estimate) %>% # crime rate
  st_as_sf()

# aggregated by type
crime_tract_ag_type <- crime_tract %>%
  st_drop_geometry() %>%
  #group_by(year, offense_against, offense_group, offense_type, GEOID) %>%
  group_by(offense_type, offense_group, offense_against, GEOID) %>%
  summarise(n = n()) %>%
  merge(la_tracts, by="GEOID") %>%
  filter(estimate > 1) %>% # population per tract must be greater than 10
  mutate(crime_rate= 1000*n/estimate) %>% # crime rate
  st_as_sf()


# merge crime data for model 
merged_data <- read.csv("./Data/merged_data.csv") %>%
  mutate(year = as.numeric(year))

# create a key of complete GEOID, day, month, and year values
generate_date_combinations <- function(geoid, year) {
  # Create a data frame with all months
  month_days <- data.frame(
    month = 1:12,
    days_in_month = sapply(1:12, function(m) {
      as.integer(format(as.Date(paste(year, m + 1, "01", sep = "-")) - 1, "%d"))
    })
  )
  
  # Expand to include days
  month_days_expanded <- month_days %>%
    rowwise() %>%
    mutate(day = list(1:days_in_month)) %>%
    unnest(day)
  
  # Add GEOID and year to the dataset
  month_days_expanded <- month_days_expanded %>%
    mutate(GEOID = geoid, year = year) %>%
    select(GEOID, year, month, day)
  
  return(month_days_expanded)
}

date_key <- crime_tract %>%
  select(GEOID, YEAR) %>%
  unique() %>%
  rowwise() %>%
  do(generate_date_combinations(.$GEOID, .$year)) %>%
  ungroup()

merged_data_crime <- crime_tract %>%
  st_drop_geometry() %>%
  select(GEOID, year, date, offense_against) %>%
  mutate(month=month(floor_date(as.Date(date), unit="month")), day=day(floor_date(as.Date(date), unit="day")), GEOID = as.numeric(GEOID)) %>% 
  group_by(GEOID, year, month, day, offense_against) %>%
  summarize(n=n()) %>%
  # merge key
  merge(merged_data, by=c("GEOID", "year"), all.x=T, all.y=T) #%>%
  # filter(!is.na(year)) %>%
  # mutate(n=ifelse(is.na(n), 0, n)) 


write.csv(merged_data_crime, "./Data/merged_data_crime.csv", row.names=F)

