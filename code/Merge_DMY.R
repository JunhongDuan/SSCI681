#######################################
#######################################
#### DAILY DATA  - Advance Quant #### 
###############  11/30/24 #############
#######################################
#######################################


# Install packages and open libraries
library(remotes) 
library(AOI)
library(climateR)
library(zonal)
library(tidycensus)
library(ggplot2)
library(ggspatial)
library(tidyverse)
library(zoo)
library(mapdata)
library(maps)
library(dplyr)
library(tidyr)
library(gstat)
library(raster)
library(scales)
library(terra)
library(sf)
library(geos)
library("lattice")
library(dlnm)
library(splines)
library(Epi)

 remotes::install_github("mikejohnson51/AOI") # to specify area (location) of interest
 remotes::install_github("mikejohnson51/climateR") # to get climate data
 remotes::install_github("mikejohnson51/zonal") # to summarize climate data by geographies

 
 # ----------- # START # ----------- #

# Set a start and end. Verify num of years
myStart <- "2010-01-01"
#myStart <- "2019-01-01"
myEnd <- "2019-12-31"
# calculate the number of years of data
n_yr <- round(as.numeric(difftime(as.Date(myEnd),as.Date(myStart) ))/365.25)
n_yr

myStart <- lubridate::ymd("2010-01-01")
#myStart <- "2019-01-01"
myEnd <- lubridate::ymd("2019-12-31")

# Shapfile Census Tract
LAcity_shapefile <- st_read("./Data/Shapefile/Los_Angeles_City_shapefile_2018/Los_Angeles_City_shapefile_2018.shp")
census_tract <-"./Data/Shapefile/2020_Census_Tracts/2020_Census_Tracts.shp"
la_gps <- st_read(census_tract)
st_geometry(la_gps)
plot(la_gps$geometry)

# Clipping LA city from Census tracts 
la_gps$GEOID_numeric <- as.numeric(la_gps$OBJECTID)
if (st_crs(la_gps) != st_crs(LAcity_shapefile)) {
  LAcity_shapefile <- st_transform(LAcity_shapefile, st_crs(la_gps))
}
LA_census_tracts <- st_intersection(la_gps, LAcity_shapefile)
plot(st_geometry(LA_census_tracts))


# Census tract ID
id_tract <- subset(as.data.frame(LA_census_tracts), select=c(OBJECTID,CT20, LABEL))
head(id_tract)


#### DOWNLOAD DATA #### - min/max/mean
# tmmn: (Minimum Near-Surface Air Temperature)
gridmet_tmmn <- getGridMET(AOI = LA_census_tracts,
                           varname = "tmmn",
                           startDate = myStart,
                           endDate = myEnd)


# tmmx: (Maximum Near-Surface Air Temperature)
gridmet_tmmx <- getGridMET(AOI = LA_census_tracts,
                           varname = "tmmx",
                           startDate = myStart,
                           endDate = myEnd)

# sph: (Near-Surface Specific Humidity)
gridmet_sph <- getGridMET(AOI = LA_census_tracts,
                          varname = "sph",
                          startDate = myStart,
                          endDate = myEnd)


## Summarize gridded tmmn data to the census tract level

# Minimum
gridmet_tmmn_co_min <- execute_zonal(data = gridmet_tmmn,
                                     geom = LA_census_tracts,
                                     fun = "min",
                                     ID = "CT20")
# Mean
gridmet_tmmn_co_mean <- execute_zonal(data = gridmet_tmmn,
                                      geom = LA_census_tracts,
                                      fun = "mean",
                                      ID = "CT20")
# Tmmx  using the mean
gridmet_tmmx_co_mean <- execute_zonal(data = gridmet_tmmx,
                                      geom = LA_census_tracts,
                                      fun = "mean",
                                      ID = "CT20")
# Sph using mean
gridmet_sph_co_mean <- execute_zonal(data = gridmet_sph,
                                     geom = LA_census_tracts,
                                     fun = "mean",
                                     ID = "CT20")

plot(gridmet_tmmx_co_mean[grepl("tmmx_", names(gridmet_tmmx_co_mean))]) # Sample plot to check
head(gridmet_sph_co_mean, n=c(6,20))

############# FUNCTION TO CREATE CLEAN AS A LONG DAILY PANEL ###################
# Function to transforms from wide to long - averages by county-year-month (for figures)
reformat_gridmet <- function(mydat  = gridmet_tmmn_co_min,
                             metric = "tmmn",
                             metric_name = "tmmn_min"
){
  
  # subset to just variables of interest
  mydat <- subset(as.data.frame(mydat),
                  select=c("CT20","LABEL", names(mydat)[grepl(metric, names(mydat))])
  )
  
  # reshape from wide to long format
  mydatlong <- reshape(mydat,
                       direction = "long",
                       varying = list(names(mydat)[3:ncol(mydat)]),
                       v.names = "myvar",
                       idvar = c("CT20","LABEL"),
                       timevar = "date",
                       times = seq(as.Date(myStart), as.Date(myEnd),by=1)
  )
  
  # create a year-month variable, for averaging by month
  mydatlong$date_ym <- format(mydatlong$date, "%Y-%m-%d")
  mydatlong$date_ym_fip <- paste(mydatlong$date_ym, mydatlong$CT20, sep="-")
  mydat_como <- aggregate(mydatlong$myvar,by=list(mydatlong$date_ym_fip), mean) # average by month
  dim(mydat_como)
  head(mydat_como)
  # create new id variables for Year, Month, CT20
  temp <- matrix(unlist(strsplit(mydat_como$Group.1,split="-")),ncol=4,byrow=T)
  colnames(temp) <- c("Year","Month","Day","CT20")
  mydat_como <- cbind(temp,mydat_como); rm(temp)
  mydat_como$Group.1 <- NULL
  head(mydat_como)
  # rename variables
  names(mydat_como)[which(names(mydat_como)=="x")] <- metric_name
  # change Year and Month variables to integer types
  mydat_como$Year <- as.integer(mydat_como$Year)
  mydat_como$Month <- as.integer(mydat_como$Month)
  mydat_como$Month <- as.integer(mydat_como$Day)
  
  # order by county, year, month - a bit slow
  mydat_como <- mydat_como[order(mydat_como$CT20,mydat_como$Year,mydat_como$Month,mydat_como$Day),]
  rownames(mydat_como) <- NULL # clean up row names
  mydat_como
}
#heat_maxes <- reformat_gridmet(gridmet_tmmx_co_mean, metric = "tmmx", metric_name = "tmmx_mean_K") # For Heatwave days
d_tmmn_min <- reformat_gridmet(gridmet_tmmn_co_min, metric = "tmmn", metric_name = "tmmn_min_K")
d_tmmn_mean <- reformat_gridmet(gridmet_tmmn_co_mean, metric = "tmmn", metric_name = "tmmn_mean_K")
d_tmmx_mean <- reformat_gridmet(gridmet_tmmx_co_mean, metric = "tmmx", metric_name = "tmmx_mean_K")
d_sph_mean <- reformat_gridmet(gridmet_sph_co_mean, metric = "sph", metric_name = "sph_mean")
head(heat_maxes)

# Merge 
dtemp <- cbind(d_tmmn_min,tmmn_mean_K=d_tmmn_mean[,5],tmmx_mean_K=d_tmmx_mean[,5])
head(dtemp)
nrow(dtemp)

# Heatwave days
dtemp$date <- paste(dtemp$Year, dtemp$Month, sep = "-")
dtemp$date <- as.Date(paste(dtemp$date, "01", sep = "-"))
dtemp$date <- format(dtemp$date, "%Y-%m")


# convert temperature units from K to Fahrenheit
convert_KtoF <- function(K){ (K-273.15)*9/5 + 32 } # function to perform conversion
# create new Fahrenheit versions of the variables
heat_maxes[,c("tmmn_min_F","tmmn_mean_F","tmmx_mean_F")] <-
  convert_KtoF(dtemp[,c("tmmn_min_K","tmmn_mean_K","tmmx_mean_K")])


# Define the quantile threshold (e.g., 75th percentile)
quantile_threshold <- 0.95

# Calculate quantiles by group and create a binary variable
dtemp <- dtemp %>%
  group_by(CT20) %>%
  mutate(
    quantile_value = quantile(tmmx_mean_F, probs = quantile_threshold, na.rm = TRUE),
    above_quantile = ifelse(tmmx_mean_F >= quantile_value, 1, 0)  # Binary (1/0)
  )
head(heatdays)

# Merge with humidity
dtemp <- merge(dtemp,d_sph_mean,by=c("CT20","Year","Month","Day"))

# Export
write.csv(dtemp, "./Data/crimtemp_DMY.csv", row.names = FALSE)
