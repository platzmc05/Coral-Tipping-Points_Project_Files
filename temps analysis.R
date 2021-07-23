#Temperature Analysis

library(ggplot2)
library(readr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(dplyr)

#CREMPS temps analysis

cremp_temps <- read_csv('../CREMP_Temps/CREMP_Temps.csv')
sites <- read_csv('../CREMP_Temps/Station_List_Master_200712.csv')

cremp_temps <- cremp_temps %>% 
  inner_join(sites, by = "SiteID")

cremp_temps <- cremp_temps %>%
  mutate(date = make_date(Year, Month, Day))

temp_time <- ggplot(cremp_temps, 
                   aes(x=date, y=TempF)) +
  geom_point() +
  geom_smooth(method='lm', aes(color=Subregion, fill=Subregion)) +
  labs(title = "Water Temperature at Reef Locations Over Time") +
  theme(plot.title = element_text(
    hjust = 0.5))
temp_time

#DHW analysis

dhw <- read_csv('../DHW/DHW_monthly.csv')

dhw <- dhw %>%
  mutate(date = make_date(Year, Month))

dhwplot <- ggplot(dhw,
                  aes(x=date)) +
  geom_line(aes(y=Average_of_SST_90th_HS, color = "90th Percentile SST")) +
  geom_line(aes(y=Average_of_DHW_from_90th_HS_1, color = "DHW")) + 
  geom_smooth(method = "lm", aes(x=date,y=Average_of_SST_90th_HS), 
              formula = y ~ x) +
  geom_smooth(method = "lm",
              aes(x=date,y=Average_of_DHW_from_90th_HS_1),
              formula = y ~ x) +
  labs(color="Legend",x="date",y="SST(F) & DHW")+
  theme_minimal() +
  labs(title = "Regional SST and Degree Heating Weeks Over Time") + 
  theme(plot.title = element_text(
      hjust = 0.5))
  
dhwplot

library(gganimate)
#transition_reveal()
#theme_minimal() +
