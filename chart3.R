#Chart 3
#load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(parsedate)
library(lubridate)

#read csv
shootings_df <- read.csv("Mass-Shootings-Database-copy.csv", stringsAsFactors = FALSE) %>% 
  filter(year != "2023") 

#clean data
shootings_df[shootings_df == "-"] <- NA
shootings_df[shootings_df == "Unknown"] <- NA
shootings_df[shootings_df == "TBD"] <- NA
shootings_df$total_victims[shootings_df$total_victims == "TK"] <- 10

shootings_df$date <- parse_date(dates = shootings_df$date)

shootings_df$total_victims <- as.numeric(shootings_df$total_victims)

shootings_df <- shootings_df[order(shootings_df$date),]

shootings_df <- shootings_df %>%
  mutate(victim_cum = cumsum(total_victims))

#line graph
ggplot(shootings_df) +
  geom_line(aes(x = date, y = victim_cum, color = "red")) +
  labs(title = "Total Victims of Mass Shootings Over Time", x = "Date", y = "Number of Victims") +
  theme(legend.position = "none")




