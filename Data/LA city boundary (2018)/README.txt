The zip folder is the boundary of LA City

If you need LA city census tract:
run these codes in R:

```{r}
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)

LAcity_shapefile <- st_read("../Data/Los_Angeles_City_shapefile_2018.shp")
#str(LAcity_shapefile)
CA_census_tract_shp <- st_read('../Data/CA_census tract_2018_boundary_shapefile/tl_2018_06_tract/tl_2018_06_tract.shp')
#str(CA_census_tract_shp)
CA_census_tract_shp$GEOID_numeric <- as.numeric(CA_census_tract_shp$GEOID)
#head(CA_census_tract_shp$GEOID_numeric)
if (st_crs(CA_census_tract_shp) != st_crs(LAcity_shapefile)) {
  LAcity_shapefile <- st_transform(LAcity_shapefile, st_crs(CA_census_tract_shp))
}
LA_census_tracts <- st_intersection(CA_census_tract_shp, LAcity_shapefile)
# print(LA_census_tracts)
plot(st_geometry(LA_census_tracts))
```
