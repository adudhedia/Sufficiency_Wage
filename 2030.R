#Install Packages
install.packages("tidyverse")
install.packages("ipumsr")
install.packages("R.utils") 
install.packages("writexl")
install.packages("readxl")
install.packages("openxlsx")
install.packages("janitor")
install.packages("xlsx")
install.packages("rJava")

#Load Packages
library(R.utils)
library(ipumsr)
library(tidyverse)
library(dplyr)
library(purrr)
library(writexl)
library(readxl)
library(openxlsx)
library(xml2)
library(rvest)
library(readr)
library(janitor)
library(scales)
library(rJava)
library(xlsx)
library(readxl)
library(httr)

#Setting working directory and API key
setwd("~/LivingWage")
set_ipums_api_key("Your Own API Key", save = TRUE) # Please Use your own API

# Extracting necessary Data from IMPUMS

data <- 
  define_extract_usa(
    "USA extract for API vignette",
    c("us2019c"), # 2015-2019 ACS 5-Year
    c("STATEFIP","COUNTYFIP","NCHILD","FAMSIZE","EMPSTAT","HHINCOME", "SEX", "SCHOOL",
      "NFAMS", "FAMUNIT", "AGE", "RElATE", "ADJUST", "RACHSING", "EDUC", "CITIZEN")
  ) %>% 
  submit_extract() %>% 
  wait_for_extract() %>% 
  download_extract() %>% 
  read_ipums_micro()

# Summary of the Data
str(data)
head(data)

# Cleaning the Data (1 Family per Household)
data_final2 <- data %>% filter(STATEFIP == 6) %>% # State - California
  filter(COUNTYFIP == 77) %>% # County - Merced, San Joaquin and Stanislaus
  filter(HHINCOME < 9999999) %>% # Income less than 9.99 Million
  filter(NFAMS == 1) %>% # Number of Families per household = 1
  filter(FAMSIZE < 10) %>% # Having a maximum family size of 9 (4 Adults and 3 Children)
  mutate(ADJUSTED_INCOME = ADJUST * HHINCOME) %>% # Adjusting Income with the Adjust factor
  group_by(SERIAL) %>% 
  mutate(NAdults = sum(AGE > 17 & AGE < 65), # Number of Adults (Age 18 & above)
         Working_Adults = sum(AGE > 17 & EMPSTAT == 1), # Number of Adults who have employment status as employed
         NChildren = sum(AGE < 18),
         Infants = sum(AGE <= 2),
         Preschoolers = sum(AGE >= 3 & AGE <= 5),
         Schoolers = sum(AGE >= 6 & AGE <= 12),
         Teenagers = sum(AGE >= 13 & AGE <= 17),
         Student = sum(SCHOOL == 2),
         Seniors = sum(AGE > 64)) %>% # Number of Children (Below the age of 18)
  filter(NAdults > 0 & NAdults < 4) %>% # Filtering out families who have at least 1 adult and less than 5 adults
  filter(NChildren < 7) %>% #Filtering out households who have more than 3 children
  filter(Working_Adults > 0) # Filtering out households who doesn't have any employed adults
  
data_final1 <- data_final2 %>%  
  mutate(County = case_when(COUNTYFIP == 47 ~ 'Merced County', ## Changing COUNTYFIP to County Name
                            COUNTYFIP == 77 ~ 'San Joaquin County',
                            COUNTYFIP == 99 ~ 'Stanislaus County'))

data_final1 <- data_final1 %>%  
  mutate(Ethnicity = case_when(RACHSING == 1 ~ 'White', ## Changing race codes to Names
                               RACHSING == 2 ~ 'African American',
                               RACHSING == 3 ~ 'American Indian/Alaska Native',
                               RACHSING == 4 ~ 'Asian/Pacific Islander',
                               RACHSING == 5 ~ 'Hispanic/Latino'))

data_final1 <- data_final1 %>%  
  mutate(Education = case_when(EDUCD <= 1 ~ 'N/A', # Changing Education codes to Names
                               EDUCD > 1 & EDUCD <= 61 ~ 'Without Highschool Diploma',
                               EDUCD > 61 & EDUCD <= 64 ~ 'With Highschool Diploma',
                               EDUCD > 64 & EDUCD <= 80 ~ 'Some College',
                               EDUCD > 80 & EDUCD <= 83 ~ 'Associates Degree',
                               EDUCD > 83 & EDUCD <= 100 ~ 'Some College',
                               EDUCD > 100 ~ 'Bachlors Degree or more'))

data_final1 <- data_final1 %>%  
  mutate(Citizenship = case_when(CITIZEN == 0 ~ 'N/A', ## Changing Citizen codes to Names
                                 CITIZEN == 1 ~ 'Born Abroad of American Parents',
                                 CITIZEN == 2 ~ 'Naturalized Citizen',
                                 CITIZEN == 3 ~ 'Not a Citizen'))

#----------------------------------------------------------------------------------------------------------------------------------------------

# Reading self-sufficiency standard for California in 2021
url <- 'https://selfsufficiencystandard.org//wp-content/uploads/2021/10/CA2021_AllFamilies.xlsx'
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
S_wage <- read_excel(tf, 3L)

# Seeing the structure of the data
str(S_wage)
head(S_wage)

# Cleaning the data
S_wage_Temp <- S_wage[-c(1)]

# Transforming all the necessary columns to numeric
S_wage_Temp <- transform(S_wage_Temp,`Adult(s)` = as.numeric(`Adult(s)`),
                         `Infant(s)` = as.numeric(`Infant(s)`),
                         `Preshooler(s)` = as.numeric(`Preshooler(s)`),
                         `Schoolager(s)` = as.numeric(`Schoolager(s)`),
                         `Teenager(s)` = as.numeric(`Teenager(s)`))

# Converting N/As to 0
S_wage_Temp[is.na(S_wage_Temp)] = 0

# Gathering necessary data County
S_wage_Temp5 <- S_wage_Temp %>% 
  filter(Adult.s. < 4) %>% # Filtering out adults more than 4
  filter(County == "Merced County" | County == "Stanislaus County" | County == "San Joaquin County") %>%
  mutate(NChildren = select(., Infant.s.:Teenager.s.) %>% rowSums(na.rm = TRUE)) %>%
  filter(NChildren <= 6)# Totaling number of children

# Separating 3 counties
SW_MC <- S_wage_Temp5 %>% filter(County == "Merced County")
SW_SC <- S_wage_Temp5 %>% filter(County == "Stanislaus County")
SW_SJC <- S_wage_Temp5 %>% filter(County == "San Joaquin County")

# Adding rows for working adults
S_wage_MC <- data.frame(SW_MC[rep(seq_len(dim(SW_MC)[1]), SW_MC$Adult.s.,),
                              , drop = FALSE], row.names=NULL)
S_wage_SC <- data.frame(SW_SC[rep(seq_len(dim(SW_SC)[1]), SW_SC$Adult.s.,),
                              , drop = FALSE], row.names=NULL)
S_wage_SJC <- data.frame(SW_SJC[rep(seq_len(dim(SW_SJC)[1]), SW_SJC$Adult.s.,),
                                , drop = FALSE], row.names=NULL)


# Combining all SW Tables
SW_All <- rbind(S_wage_MC, S_wage_SC, S_wage_SJC)

SW_All <- as.data.frame(SW_All)

#---------------------------------------------------------------------------------------------------------

# Selecting necessary rows for joining into Sufficiency wage final table
S_wage_Final <- SW_All %>% select(Adult.s., Infant.s., Preshooler.s., Schoolager.s., Teenager.s.,
                                  County, Hourly.Self.Sufficiency.Wage, Monthly.Self.Sufficiency.Wage,
                                  Annual.Self.Sufficiency.Wage, NChildren)

# Joining county data with sufficiency wage table by Number of adults, working adults and Number of children.
## Filtering it as 1 observation per household who is the head of the household
Data_Final_Analysis <- left_join(data_final1, S_wage_Final, 
                                 by = c("NAdults"="Adult.s.", "Infants" = "Infant.s.", "Preschoolers" = "Preshooler.s.", 
                                        "Schoolers" = "Schoolager.s.", "Teenagers" = "Teenager.s.", "County" = "County", 
                                        "NChildren" = "NChildren"))

Data_Final <- Data_Final_Analysis %>% filter(RELATE == 1)
# ----------------------------------------ANALYSIS------------------------------------------------------------------

Struggling_HH <- Data_Final %>%
  filter(ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage)

# Create a vector of wage levels to loop through
wage_levels <- seq(15.50,50, by = 0.25)*2040

# Initialize a list to store the results
results_list <- list()

# Loop through each wage level and calculate the percentage of people whose hourly wage is under that level
for (i in wage_levels) {
  hourly_wage_threshold <- i
  percent_under_threshold <- sum(Struggling_HH$ADJUSTED_INCOME < hourly_wage_threshold) / nrow(Struggling_HH)
  results_list[[i]] <- data.frame(wage_level = i, percent_under_threshold = percent_under_threshold)
}

# Combine the results into a single table
results_table <- do.call(rbind, results_list)

results_table$wage_level <- results_table$wage_level / 2040

# Print the table
print(results_table)

require(openxlsx)
list_of_datasets <- list("SS at different Wage" = results_table)
openxlsx::write.xlsx(list_of_datasets, file = "2030.xlsx")
