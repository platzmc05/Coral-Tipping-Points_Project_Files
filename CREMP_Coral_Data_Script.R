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

####### Species Richness #########
#Plot all coral species richness (Y) over year (x)
library(ggplot2)
library(gganimate)
install.packages("gifski")
library(gifski)
library(sf)
library(viridis)

subregion_animation <- ggplot(CREMP_Raw,
  aes(x = Sample.Year, y = Species.Richness, color=subRegionId)) +
  theme(legend.position = 'top', plot.title = element_text(hjust = 0.5))+
  geom_point(stat = 'summary',fun = mean)+
  geom_line(stat = 'summary',fun = mean)+
  transition_reveal(Sample.Year)+
  labs(title = 'Florida Reef Tract Stony Coral Species Richness', 
       x = 'Species Richness', y = 'Year')

# save as GIF 
animate(subregion_animation)


# Plot with regression lines 
ggplot(CREMP_Raw,
  aes(x = Sample.Year, y = Species.Richness, color=subRegionId)) +
  geom_point(stat = 'summary',fun = mean)+
  geom_smooth(method = 'lm')

# add vertical line to indicate hurricane or other event: geom_vline(xintercept = 5)


## calculate the mean species richness observed each year
#SY_2012 <- CREMP_Raw[CREMP_Raw$Sample.Year == 2012,]$Species.Richness
SY_2012 <- CREMP_Raw[CREMP_Raw$Sample.Year == 2012,]$Species.Richness#$sitename

mean(CREMP_Raw[CREMP_Raw$Sample.Year==2012 & CREMP_Raw$sitename=="Content Keys             ",]$Species.Richness)
str(CREMP_Raw)

# plot reef locations as spatial feature 
dev.off()
reef_location <- st_as_sf(CREMP_Raw,
                 coords = c('lonDD', 'latDD'),
                 crs = 4326)
plot(reef_location['Stony.coral'])   
plot(reef_location['Species.Richness'])   

# Plot all reefs with all data available 
install.packages("transformr")
library(transformr)
mapview(reef_location)
mapview(reef_location['Species.Richness'])

# pull out just the data for each year 

reef_location_avg <- reef_location %>%
  group_by(sitename,Sample.Year)%>%
  summarise(Species.Richness_Avg = mean(Species.Richness))

######MARY'S CODE ###############
library("ggspatial")
richness.animation <- ggplot(reef_location_avg, aes(color=Species.Richness)) +
  labs(title = "Florida Reef Tract Stony Coral Species Richness")+
  #annotation_map_tile(type='cartolight')+ takes a long time to load with the annotation map
  geom_sf()+ 
  transition_time(Sample.Year) + labs(title = "Year: {frame_time}")

anim_save('richness_animation.gif', animation=richness.animation)

ggplot(reef_location%>%filter(Sample.Year=='2017'), aes(color=Species.Richness)) +
  labs(title = "Florida Reef Tract Stony Coral Species Richness")+
  annotation_map_tile(type='cartolight')+
  geom_sf() +
  scale_color_viridis_c()

## Make a graph for each year
reef.years <- unique(reef_location$Sample.Year)

# specify where to save files: 
dir.out <- 'C:/Users/platz/OneDrive - University of South Florida/Research/SESYNC/Scripts/' # where files will save
for(i in reef.years){
  p<- ggplot(reef_location_avg%>%filter(Sample.Year==i), aes(color=Species.Richness_Avg)) +
    labs(title = "Florida Reef Tract Stony Coral Species Richness")+
    annotation_map_tile(type='cartolight')+
    geom_sf() +
    scale_color_viridis_c(limits = c(3, 18), option = "H" )+
    labs(subtitle = i)
  
  fp <- file.path(dir.out, paste('Year', i, ".png", sep=''))
  
  ggsave(plot = p, 
         filename = fp, 
         device = "png") 
}

library(magick)

imgs <- list.files(dir.out, full.names = TRUE, pattern='Year')
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 1)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "coral-richness-year.gif")










library("ggspatial")
ggplot(reef_location['Species.Richness'], aes(color=Species.Richness)) +
    labs(title = "Florida Reef Tract Stony Coral Species Richness")+
    annotation_map_tile(type='cartolight')+
    geom_sf()+ 
    transition_time(Sample.Year) + labs(title = "Year: {frame_time}")






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

ggplot(reef_location, aes(color=Species.Richness)) +
  labs(title = 'Florida Reef Tract Stony Coral Species Richness')+
  annotation_map_tile(type='cartolight')+
  geom_sf()+
  transition_time(Year) + labs(title = 'Year: {frame_time}')


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



  


