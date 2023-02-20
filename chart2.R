
#load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)

#read in data
shootings_df <- read.csv("Mass-Shootings-Database-copy.csv", stringsAsFactors = FALSE)


shootings_df[shootings_df == "-"] <- NA
shootings_df[shootings_df == "Unknown"] <- NA
shootings_df[shootings_df == "TBD"] <- NA

num_shootings_df <- transform(shootings_df,
                                total_victims = as.numeric(total_victims))
num_shootings_df <- transform(num_shootings_df, 
                                injured = as.numeric(injured))
num_shootings_df <- transform(num_shootings_df, 
                                fatalities = as.numeric(fatalities))
num_shootings_df <- transform(num_shootings_df, 
                                age_of_shooter = as.numeric(age_of_shooter))
#str(num_shootings_df)


#create more general column for gun type
shootings <- num_shootings_df %>% mutate(gun_type = gsub(".*;.*","More Than One Type", weapon_type))
shootings$gun_type <- gsub(".*;.*","More Than One Type", shootings$gun_type)
shootings$gun_type <- gsub(".*,.*","More Than One Type", shootings$gun_type)
shootings$gun_type <- gsub(".* and .*","More Than One Type", shootings$gun_type)
shootings$gun_type <- gsub(".*semiautomatic handgun.*","Semiautomatic Handgun", shootings$gun_type)
shootings$gun_type <- gsub(".*handgun.*","Handgun", shootings$gun_type)
shootings$gun_type <- gsub(".*semiautomatic pistol.*","Semiautomatic Handgun", shootings$gun_type)
shootings$gun_type <- gsub(".*pistol.*","Handgun", shootings$gun_type)
shootings$gun_type <- gsub(".*revolver.*","Handgun", shootings$gun_type)
shootings$gun_type <- gsub(".*shotgun.*","Shotgun", shootings$gun_type)
shootings$gun_type <- gsub("semiautomatic rifle*","Semiautomatic Rifle", shootings$gun_type)
shootings$gun_type <- gsub("Semiautomatic Rifles*","Semiautomatic Rifle", shootings$gun_type)
shootings$gun_type <- gsub(".*rifle.*","Rifle", shootings$gun_type)


#filter for only up to 2022
shootings$date <- as.Date(shootings$date, "%m/%d/%y")
shootings <- shootings %>% filter(date != "2022-06-01") %>% filter(year <= "2022")

#new df
fatalities <- shootings %>% group_by(gun_type) %>% summarise(total_injured = sum(injured), total_fatalities = sum(fatalities)) 

#bar chart
df_long <- melt(fatalities, id.var = "gun_type")

ggplot(df_long, aes(x = gun_type, y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  labs(title = "Injuries and Deaths by Gun Type",
       x = "Gun Type",
       y = "Casualties") + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_discrete(name="Casualty",
                        labels=c("Injuries", "Fatalities")) +
  theme(legend.background = element_rect(fill= "grey90"))




