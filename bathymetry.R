#Working with DEM data
#Must install packages ggplot2, stars, dplyr, sf, data.table, mapview

#DEM

library(ggplot2)

library(stars)
bath <- read_stars("FRT_CRM.tiff", proxy = TRUE)

st_crs(bath)
st_crs

bath_bbox <- st_bbox(bath)
bath_bbox

library(dplyr)
ggplot() +
  geom_stars(data = bath)

#CREMP Reef locations to overlay

library(sf)
library(data.table)
rdata <- fread('Station_List_Master_200712.csv')

reefs <- st_as_sf(rdata, coords = c("lonDD", "latDD"), 
                  crs = st_crs(bath)
                  )

#Plot of combined bathymetry + reef locations 
  
ggplot() +
  geom_stars(data = bath) +
  geom_sf(data = reefs, size = 0.5, color = 'red')


#Mapview of combined bathymetry + reef locations
#Reef locations color varies by offshore depth

library(mapview)
mapview(bath)

mapview(bath, legend = FALSE, alpha = 0.3, 
        maxpixels = 4891525) +
  mapview(reefs, legend = TRUE, zcol = "OffshoreDepth_ft")


#Optional mapview add: map.types = 'Esri.WorldImagery' 
