#Summary Information 
#Use dplyr to produce a list of 5 summary values from the data set
# ------------------------------------------------------------------------------

#Load in necessary libraries
library("dplyr")
library("tidyverse")
library("ggplot2")

#Load in the data set
ms_df <- read.csv("Mass-Shootings-Database-copy.csv", stringsAsFactors = FALSE) %>% 
  filter(year != "2023") 

#Clean the data:
#Add NA where there are missing values in the dataset
ms_df[ms_df == "-"] <- NA
ms_df[ms_df == "Unknown"] <- NA
ms_df[ms_df == "TBD"] <- NA

#Fix total_victims for "Tulsa medical center shooting"
ms_df[ms_df == "TK"] <- 4

#Make 'total_victims' and 'injured' numeric variables
ms_df$injured = as.numeric(ms_df$injured, na.rm = TRUE)
ms_df$total_victims = as.numeric(ms_df$total_victims, na.rm = TRUE)

#Create a list of summary information variables
#1. The total number of mass shootings
num_mass_shootings <- ms_df %>% 
  nrow()

#2. The total number of fatalities
num_fatalities <- ms_df %>% 
  summarize(total_fatalities = sum(fatalities)) %>% 
  pull(total_fatalities)

#3. The number of mass shootings that happened at schools
school_shootings <- ms_df %>% 
  group_by(location.1) %>% 
  filter(location.1 == "School") %>% 
  nrow()

#4. The year with the highest number of mass shootings, and how many
year_most_shootings <- ms_df %>% 
  group_by(year) %>% 
  summarize(count = n()) %>% 
  filter(count == max(count)) 

#5. The case with the highest number of total victims, and how many
case_most_victims <- ms_df %>% 
  filter(total_victims == max(total_victims)) %>% 
  pull(case)

case_most_victims_amount <- ms_df %>% 
  filter(total_victims == max(total_victims)) %>% 
  pull(total_victims)

#6. The percentage of of cases that involved semiautomatic weapons
num_shootings_semiauto <- ms_df %>% 
  filter(grepl('semiautomatic', weapon_type)) %>% 
  nrow()

percent_semiauto <- round((num_shootings_semiauto/num_mass_shootings) * 100)
  
#Add the variables into a list:
summary_info <- list()
summary_info$num_mass_shootings <- num_mass_shootings
summary_info$num_fatalities <- num_fatalities
summary_info$school_shootings <- school_shootings
summary_info$year_most_shootings <- year_most_shootings 
summary_info$case_most_victims <- case_most_victims
summary_info$case_most_victims_amount <- case_most_victims_amount
summary_info$percent_semiauto <- percent_semiauto