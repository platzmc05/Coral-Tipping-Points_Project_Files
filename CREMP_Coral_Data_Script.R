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
install.packages('gganimate')
devtools::install_github('thomasp85/gganimate')

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
library(gganimate)
ggplot(CREMP_Raw,
  aes(x = Sample.Year, y = Species.Richness, color=subRegionId)) +
  geom_point(stat = 'summary',fun = mean)+
  geom_line(stat = 'summary',fun = mean)+
  transition_reveal(Sample.Year)


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

# add vertical line to indicate hurricane or other event: geom_vline(xintercept = 5)




## calculate the mean species richness observed each year
#SY_2012 <- CREMP_Raw[CREMP_Raw$Sample.Year == 2012,]$Species.Richness
SY_2012 <- CREMP_Raw[CREMP_Raw$Sample.Year == 2012,]$Species.Richness#$sitename

mean(CREMP_Raw[CREMP_Raw$Sample.Year==2012 & CREMP_Raw$sitename=="Content Keys             ",]$Species.Richness)
str(CREMP_Raw)

# plot reef locations as spatial feature 
install.packages("sf")
install.packages("viridis")

library(sf)
library(viridis)
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


install.packages("ggspatial")
library("ggspatial")
ggplot(reef_location['Species.Richness'], aes(color=Species.Richness)) +
    labs(title = "Florida Reef Tract Stony Coral Species Richness")+
    annotation_map_tile(type='cartolight')+
    geom_sf()+ 
    transition_time(Sample.Year) + labs(title = "Year: {frame_time}")




#Average species richness by reef site across all years 
reef_location_Avg <- reef_location %>%
  group_by(sitename) %>%
  summarise(
    Species.Richness_Avg = mean(Species.Richness)
  )

#plot
ggplot(reef_location_Avg['Species.Richness_Avg'], aes(color=Species.Richness_Avg)) +
  labs(title = "Florida Reef Tract Stony Coral Species Richness")+
  annotation_map_tile(type='cartolight')+
  geom_sf()


########################### Help With this section ######################################
# create new spatial features for the average species richness by reef site for each year
reef_location_avg <- reef_location %>%
  group_by(sitename,Sample.Year)%>%
  summarise(Species.Richness_Avg = mean(Species.Richness))

names(reef_location_avg)[2] <- "Year"
colnames(reef_location_avg)#view column names
reef_location_avg$Year <- as.numeric(reef_location_avg$Year)


ggplot(reef_location_avg['Species.Richness_Avg'], aes(color=Species.Richness_Avg)) +
  labs(title = "Florida Reef Tract Stony Coral Species Richness")+
  annotation_map_tile(type='cartolight')+
  geom_sf()+ 
  #scale_fill_gradient()
  transition_time(Year) + labs(title = "Year: {frame_time}")

#scale_fill_viridis_d() +
#scale_size(range = c(0, 20))








# create new spatial features for the average species richness by reef site for each year
reef_location_2013 <- reef_location[reef_location$Sample.Year == 2013,] %>%
  group_by(sitename)%>%
  summarise(Species.Richness_Avg = mean(Species.Richness))
            
reef_location_2014 <- reef_location[reef_location$Sample.Year == 2014,] %>%
  group_by(sitename)%>%
  summarise(Species.Richness_Avg = mean(Species.Richness))

reef_location_2015 <- reef_location[reef_location$Sample.Year == 2015,] %>%
  group_by(sitename)%>%
  summarise(Species.Richness_Avg = mean(Species.Richness))

reef_location_2016 <- reef_location[reef_location$Sample.Year == 2016,] %>%
  group_by(sitename)%>%
  summarise(Species.Richness_Avg = mean(Species.Richness))

reef_location_2017 <- reef_location[reef_location$Sample.Year == 2017,] %>%
  group_by(sitename)%>%
  summarise(Species.Richness_Avg = mean(Species.Richness))

reef_location_2018 <- reef_location[reef_location$Sample.Year == 2018,] %>%
  group_by(sitename)%>%
  summarise(Species.Richness_Avg = mean(Species.Richness))

reef_location_2019 <- reef_location[reef_location$Sample.Year == 2019,] %>%
  group_by(sitename)%>%
  summarise(Species.Richness_Avg = mean(Species.Richness))

reef_location_2020 <- reef_location[reef_location$Sample.Year == 2020,] %>%
  group_by(sitename)%>%
  summarise(Species.Richness_Avg = mean(Species.Richness))


#plot 
ggplot(reef_location_2020['Species.Richness_Avg'], aes(color=Species.Richness_Avg)) +
  labs(title = "2020 Florida Reef Tract Stony Coral Species Richness")+
  annotation_map_tile(type='cartolight')+
  geom_sf()



  


