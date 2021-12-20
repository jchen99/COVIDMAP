#####COVID-19 Map#####
#####FROM NYT DATA####

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(usmap)
library(viridis)


#set and check your working directory
getwd()
setwd("L:/Lab-Wiestner/Jonathan/RNASEQ/RScripts/COVIDMAP")

#load the dataset
covid <- read_csv("us-counties-recent.csv")


#Filtering
covid_sum <- covid %>% 
  filter(date == as.Date("2021-11-20")) %>% 
  group_by(state) %>%
  summarise(death_sum = sum(deaths))

us_map <- usmap::us_map(region = "states")

#Data Visualization
usmap::plot_usmap(data = covid_sum, values = "death_sum", color = "grey40") +
  scale_fill_continuous(type='viridis', label = scales:: comma) +
  labs(title = "COVID-19 - total number of deaths",
       subtitle = "as of November 31, 2021",
       caption = "data from: NYT", 
       fill = "deaths (n)") + 
  theme_classic()+
  theme(panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position = "right",
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())