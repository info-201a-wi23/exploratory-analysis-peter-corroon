#Chart 1
rm(list = ls())

shootings_data <- read.csv("shootingsdataset.csv")


library("dplyr")
library("tidyverse")


shootings_data[shootings_data == "-"] <- NA
shootings_data[shootings_data == "Unknown"] <- NA
shootings_data[shootings_data == "TBD"] <- NA
shootings_data

num_shootings_data <- transform(shootings_data,
                                total_victims = as.numeric(total_victims))
num_shootings_data <- transform(num_shootings_data, 
                                injured = as.numeric(injured))
num_shootings_data <- transform(num_shootings_data, 
                                fatalities = as.numeric(fatalities))
num_shootings_data <- transform(num_shootings_data, 
                                age_of_shooter = as.numeric(age_of_shooter))
str(num_shootings_data)
mean(num_shootings_data$age_of_shooter, na.rm = TRUE)
max(shootings_data$age_of_shooter, na.rm = TRUE)




  

#CREATE MAP
  install.packages("maps")
  
  # Load libraries
  library("ggplot2")
  library("plotly")
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
  coord_map(xlim = c(-125, -60), ylim = c(50, 25)) +
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









ggplot(no_na_lat_long) +
  geom_point(aes(x = longitude, y = latitude))


#CHART OF WHERE THINGS HAPPEN
shootings_data$location.1 <- lapply(shootings_data$location.1, tolower)
ggplot(shootings_data) +
  geom_point(aes(x = year, y = fatalities, color = location.1))



ends_state_abbv <- locations[str_detect(locations, ", ..$")]
states <- str_sub(ends_state_abbv, -2)






ggplot(num_shootings_data) +
  geom_bar(x = prior_signs_mental_health_issues, y = fatalities, na.rm = TRUE)

shootings_data <- shootings_data %>%
  lapply(location.1, tolower)


lapply(location.1, tolower)



How has the frequency of mass shootings changed over time?
  How does the occurrence of mass shootings differ by location?
  How does the number of incidents change by gender?
  Does the type of weapon correlate with patterns in the number of dead or injured?
  Are guns obtained in these shootings done so legally?
  Is there a connection between mass shootings and mental health issues?

  
  
  #if (str_detect("//..$") = TRUE) {
  ends_last_two <- date[str_detect(date, "//..$")]
last_two <- str_sub(ends_last_two, -2)
last_four <- paste0("20", last_two)
str_replace("//..$", last_four)



