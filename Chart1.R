#Chart 1
rm(list = ls())

shootings_data <- read.csv("shootingsdataset.csv")
library("dplyr")
library("tidyverse")

shootings_data[shootings_data == "-"] <- NA
shootings_data[shootings_data == "Unknown"] <- NA
shootings_data[shootings_data == "TBD"] <- NA
shootings_data

  install.packages("maps")
  
  # Load libraries
  library("ggplot2")
#  library("plotly")
  library("dplyr")
  state_shape <- map_data("state")
  
  
no_na_lat_long <- shootings_data %>%
    drop_na(latitude) %>%
    drop_na(longitude)

num_lat_long <- transform(no_na_lat_long,
                                latitude = as.numeric(latitude))
num_lat_long <- transform(num_lat_long, longitude = as.numeric(longitude))

str(num_lat_long)
  
ggplot(data = state_shape) +
  geom_polygon(aes(x = long, 
                   y = lat, 
                   group = group)) +
  geom_point(data = num_lat_long, aes(x = longitude, 
                   y = latitude, color = "red")) +
  coord_map(xlim = c(-125, -65), ylim = c(50, 25)) +
  labs(
    title = "Locations of US Mass Shootings", 
    x = "Longitude", 
    y = "Latitude", 
    color = "Location") +
  theme(
    plot.background = element_blank(),
    legend.position = "none")
  

  
  
  blank_theme









blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank(), # remove border around plot
  )
