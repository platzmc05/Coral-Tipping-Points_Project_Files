## Script name: CREMP_Coral_Data_Script.R
##
## Purpose of script:read, tidy, and perform data visualizations of CREMP data from 2012-2020 
##
## Author: Michelle C. Platz 
##
## Date Created: 2021-07-21
##
## Copyright (c) Michelle Platz, 2021
## Email: mplatz@usf.edu
##
## ---------------------------
##
## Notes:
##  Data from 40 reefs in FRT throughout upper, middle, and lower keys from 2012-2020 
##  Data source: CREMP
## ---------------------------

# install packages 
install.packages("tidyverse")
install.packages("stringr")

#create subsets of data by subRegionId
library(dplyr)
library(stringr)

# Read in the data 
CREMP_Raw <- read.csv('CREMP_Pcover.csv')

# rename the ï..Sample.Year column to Sample.Year
names(CREMP_Raw)[1] <- "Sample.Year"
colnames(CREMP_Raw)#view column names

#Upper Keys 
UK <- filter(CREMP_Raw, grepl('UK', subRegionId))
#Middle Keys 
MK <- filter(CREMP_Raw, grepl('MK', subRegionId))
# Lower Keys
LK <- filter(CREMP_Raw, grepl('LK', subRegionId))


#Plot all coral species richness (Y) over year (x)
library(ggplot2)
ggplot(CREMP_Raw,
  aes(x = Sample.Year, y = Species.Richness, color=subRegionId)) +
  geom_point(stat = 'summary',fun = mean)+
  geom_smooth(method = 'lm')




#Plot percent coral cover (Y) over year (x)
library(ggplot2)
ggplot(CREMP_Raw,
       aes(x = Sample.Year, y = Stony.coral, color=subRegionId)) +
  geom_point(stat = 'summary',fun = median)+
  geom_smooth(method = 'lm')





# calculate the mean species richness observed each year
SY_2012 <- CREMP_Raw[CREMP_Raw$Sample.Year == 2012,]$Species.Richness
SY_2012 <- CREMP_Raw[CREMP_Raw$Sample.Year == 2012,]$sitename

# add vertical line to indicate hurricane or other event: geom_vline(xintercept = 5)

# plot only the median species richness
 

ggplot(CREMP_Raw,
  aes(x = Sample.Year, y = Stony.coral, color = )) +
  geom_point()


# plot reef locations as spatial feature 
install.packages("sf")
library(sf)
dev.off()
reef_location <- st_as_sf(CREMP_Raw,
                 coords = c('lonDD', 'latDD'),
                 crs = 4326)
plot(reef_location['Stony.coral'])   
plot(reef_location['Species.Richness'])   

# Plot all reefs with all data available 
install.packages("mapview")
library(mapview)
mapview(reef_location)
mapview(reef_location['Species.Richness'])

# pull out just the data for each year 
