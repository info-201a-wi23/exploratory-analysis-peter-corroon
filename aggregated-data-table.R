#Calculating 5 summary values

#Load in the data set
ms_df <- read.csv("~/Desktop/mass-shootings.csv", stringsAsFactors = FALSE) %>% 
  filter(year != "2023") 

View(ms_df)

#Load in necessary libraries
library("dplyr")
library("tidyverse")
library("ggplot2")

#Add NA where there are missing values in the dataset
ms_df[ms_df == "-"] <- NA
ms_df[ms_df == "Unknown"] <- NA
ms_df[ms_df == "TBD"] <- NA
ms_df[ms_df == "TK"] <- 4

#Make victims and injuries numeric
ms_df$injured = as.numeric(ms_df$injured, na.rm = TRUE)
ms_df$total_victims = as.numeric(ms_df$total_victims, na.rm = TRUE)

#Create a df displaying the total fatalities per year
fatalities_by_year <- ms_df %>% 
  group_by(year) %>% 
  summarize(fatalities = sum(fatalities)) 
View(fatalities_by_year)

#Create a df displaying the total injuries per year
injuries_by_year <- ms_df %>% 
  group_by(year) %>% 
  summarize(injured= sum(injured, na.rm = TRUE)) 
View(injuries_by_year)

#create a df displaying the total victims per year
victims_by_year <- ms_df %>% 
  group_by(year) %>% 
  summarize(total_victims = sum(total_victims, na.rm = TRUE)) 
View(victims_by_year)

#Create aggregated data table
agg_table <- ms_df %>% 
  select(year, total_victims, injured, fatalities) %>% 
  arrange(desc(fatalities)) %>% 
  slice_head(n = 5)

agg_table
