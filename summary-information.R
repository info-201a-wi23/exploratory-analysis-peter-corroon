#Calculating 5 summary values

#Load in the data set
ms_df <- read.csv("~/Desktop/mass-shootings.csv", stringsAsFactors = FALSE) %>% 
  filter(year != "2023")
View(ms_df)

#Load in necessary libraries
library("dplyr")
library("tidyverse")

#Add NA where there are missing values in the dataset
ms_df[ms_df == "-"] <- NA
ms_df[ms_df == "Unknown"] <- NA
ms_df[ms_df == "TBD"] <- NA
ms_df[ms_df == "TK"] <- 4

#Create a list of summary information variables
summary_info <- list()

#Calculate the total number of mass shootings up to the year 2023
summary_info$num_mass_shootings <- ms_df %>% 
  nrow()

#Calculate the total number of fatalities from these shootings up to the year 2023
summary_info$num_fatalities <- ms_df %>% 
  summarize(total_fatalities = sum(fatalities))

#Calculate which year had the highest number of mass shootings, how many did it have?
summary_info$year_most_shootings <- ms_df %>% 
  group_by(year) %>% 
  summarize(count = n()) %>% 
  filter(count == max(count))

#Calculate which case had the highest number of total victims, how many were there?
summary_info$case_most_victims <- ms_df %>% 
  filter(fatalities == max(fatalities)) %>% 
  pull(case, total_victims)

#Calculate the number of cases used semiautomatic weapons
summary_info$percent_semiauto <- ms_df %>% 
  filter(grepl('semiautomatic', weapon_type)) %>% 
  nrow()

#Calculate the number of cases in which shooters were said have signs of mental health issues
summary_info$mental_health <- ms_df %>% 
  filter(prior_signs_mental_health_issues == "Yes") %>% 
  nrow()

  
  
  