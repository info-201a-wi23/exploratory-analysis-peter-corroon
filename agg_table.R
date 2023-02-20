#AGGREGATE TABLE
#Use dplyr to produce a table of aggregate information from the data set
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

#Add a column that counts each row as one mass shooting (this is simply to count the number of mass shootings for year in the aggregate table)
ms_df <- ms_df %>% 
  mutate("mass_shootings" = 1)

#Create aggregated data table showing:
# - each year
# - number of shootings
# - total number of victims
# - total number of injuries
# - total number of fatalities
agg_table <- ms_df %>% 
  group_by(year) %>% 
  summarize(across(c(mass_shootings, total_victims, injured, fatalities), sum, na.rm = TRUE)) %>% 
  arrange(desc(year)) %>% 
  slice_head(n = 13)

agg_table
