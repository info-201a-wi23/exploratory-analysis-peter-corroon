#Summary Information 
#Use dplyr to produce a list of 5 summary values from the data set
# ------------------------------------------------------------------------------

#Load in necessary libraries
library("dplyr")
library("tidyverse")
library("ggplot2")

#Load in the data set
ms_df <- read.csv("~/Desktop/mass-shootings.csv", stringsAsFactors = FALSE) %>% 
  filter(year != "2023") 

#Add NA where there are missing values in the dataset; fix total_victims for "Tulsa medical center shooting"
ms_df[ms_df == "-"] <- NA
ms_df[ms_df == "Unknown"] <- NA
ms_df[ms_df == "TBD"] <- NA
ms_df[ms_df == "TK"] <- 4

#Make 'total_victims' and 'injured' numeric variables
ms_df$injured = as.numeric(ms_df$injured, na.rm = TRUE)
ms_df$total_victims = as.numeric(ms_df$total_victims, na.rm = TRUE)


#Create a list of summary information variables
summary_info <- list()

#Calculate the total number of mass shootings
summary_info$num_mass_shootings <- ms_df %>% 
  nrow()

#Calculate the total number of fatalities
summary_info$num_fatalities <- ms_df %>% 
  summarize(total_fatalities = sum(fatalities)) %>% 
  pull(total_fatalities)

#Calculate how many shootings took place at schools
summary_info$school_shootings <- ms_df %>% 
  group_by(location.1) %>% 
  filter(location.1 == "School") %>% 
  nrow()

#Calculate which year had the highest number of mass shootings, how many did it have?
summary_info$year_most_shootings <- ms_df %>% 
  group_by(year) %>% 
  summarize(count = n()) %>% 
  filter(count == max(count)) %>% 
  pull(year, count)

#Calculate which case had the highest number of total victims, how many were there?
summary_info$case_most_victims <- ms_df %>% 
  filter(fatalities == max(fatalities)) %>% 
  pull(case, total_victims)

#Calculate the percentage of of cases that used semiautomatic weapons
num_shootings_semiauto <- ms_df %>% 
  filter(grepl('semiautomatic', weapon_type)) %>% 
  nrow()

summary_info$percent_semiauto <- round((num_shootings_semiauto/summary_info$num_mass_shootings) * 100)



  
  
  