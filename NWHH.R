#Install Packages
install.packages("tidyverse")
install.packages("ipumsr")
install.packages("R.utils") 
install.packages("writexl")
install.packages("readxl")
install.packages("openxlsx")
install.packages("janitor")
install.packages("xlsx")

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
data_final_W <- data %>% filter(STATEFIP == 6) %>% # State - California
  filter(COUNTYFIP == 47 | COUNTYFIP == 77 | COUNTYFIP == 99) %>% # County - Merced, San Joaquin and Stanislaus
  filter(HHINCOME < 9999999) %>% # Income less than 9.99 Million
  filter(NFAMS == 1) %>% # Number of Families per household = 1
  filter(FAMSIZE < 8) %>% # Having a maximum family size of 7 (4 Adults and 3 Children)
  mutate(ADJUSTED_INCOME = ADJUST * HHINCOME) %>% # Adjusting Income with the Adjust factor
  group_by(SERIAL) %>% 
  mutate(NAdults = sum(AGE > 17), # Number of Adults (Age 18 & above)
         Working_Adults = sum(AGE > 17 & EMPSTAT == 1), # Number of Adults who have employment status as employed
         NChildren = sum(AGE < 18),
         Infants = sum(AGE <= 2),
         Preschoolers = sum(AGE >= 3 & AGE <= 5),
         Schoolers = sum(AGE >= 6 & AGE <= 12),
         Teenagers = sum(AGE >= 13 & AGE <= 17)) %>% # Number of Children (Below the age of 18)
  filter(NAdults > 0 & NAdults < 5) %>% # Filtering out families who have at least 1 adult and less than 5 adults
  filter(NChildren < 4) %>%  #Filtering out households who have more than 3 children
  filter(Working_Adults > 0) # Filtering out households who doesn't have any employed adults

data_final_NW <- data %>% filter(STATEFIP == 6) %>% # State - California
  filter(COUNTYFIP == 47 | COUNTYFIP == 77 | COUNTYFIP == 99) %>% # County - Merced, San Joaquin and Stanislaus
  filter(HHINCOME < 9999999) %>% # Income less than 9.99 Million
  filter(NFAMS == 1) %>% # Number of Families per household = 1
  filter(FAMSIZE < 8) %>% # Having a maximum family size of 7 (4 Adults and 3 Children)
  mutate(ADJUSTED_INCOME = ADJUST * HHINCOME) %>% # Adjusting Income with the Adjust factor
  group_by(SERIAL) %>% 
  mutate(NAdults = sum(AGE > 17), # Number of Adults (Age 18 & above)
         Working_Adults = sum(AGE > 17 & EMPSTAT == 1), # Number of Adults who have employment status as employed
         NChildren = sum(AGE < 18),
         Infants = sum(AGE <= 2),
         Preschoolers = sum(AGE >= 3 & AGE <= 5),
         Schoolers = sum(AGE >= 6 & AGE <= 12),
         Teenagers = sum(AGE >= 13 & AGE <= 17)) %>% # Number of Children (Below the age of 18)
  filter(NAdults > 0 & NAdults < 5) %>% # Filtering out families who have at least 1 adult and less than 5 adults
  filter(NChildren < 4) %>%  #Filtering out households who have more than 3 children
  filter(Working_Adults == 0) %>% # Filtering out households who doesn't have any employed adults
  mutate(Working_Adults = NAdults)
  
data_final2 <- rbind(data_final_W, data_final_NW)

data_final1 <- data_final2 %>%  
  mutate(County = case_when(COUNTYFIP == 47 ~ 'Merced County', ## Changing COUNTYFIP to County Name
                            COUNTYFIP == 77 ~ 'San Joaquin County',
                            COUNTYFIP == 99 ~ 'Stanislaus County')) %>% 
  mutate(Ethnicity = case_when(RACHSING == 1 ~ 'White', ## Changing race codes to Names
                               RACHSING == 2 ~ 'African American',
                               RACHSING == 3 ~ 'American Indian/Alaska Native',
                               RACHSING == 4 ~ 'Asian/Pacific Islander',
                               RACHSING == 5 ~ 'Hispanic/Latino')) %>%
  mutate(Education = case_when(EDUCD <= 1 ~ 'N/A', # Changing Education codes to Names
                               EDUCD > 1 & EDUCD <= 61 ~ 'Without Highschool Diploma',
                               EDUCD > 61 & EDUCD <= 64 ~ 'With Highschool Diploma',
                               EDUCD > 64 & EDUCD <= 80 ~ 'Some College',
                               EDUCD > 80 & EDUCD <= 83 ~ 'Associates Degree',
                               EDUCD > 83 & EDUCD <= 100 ~ 'Some College',
                               EDUCD > 100 ~ 'Bachlors Degree or more')) %>%
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
  filter(Adult.s. < 5) %>% # Filtering out adults more than 4
  filter(County == "Merced County" | County == "Stanislaus County" | County == "San Joaquin County") %>%
  filter(Adult.s. < 4 & Infant.s. < 4 & Preshooler.s. < 4 & Schoolager.s. < 4 & Teenager.s. < 4) %>%
  mutate(NChildren = select(., Infant.s.:Teenager.s.) %>% rowSums(na.rm = TRUE)) %>%
  filter(NChildren <= 3)# Totaling number of children

# Duplicating Expenses of 3 adults to 4 adults as their expenses are similar [Additional of 300 a month in some cases]
SW4 <- S_wage_Temp5 %>%
  filter(Adult.s. == 3)

SW4$Adult.s. <- 4

SW_Temp_Combined <- rbind(S_wage_Temp5, SW4)

# Separating 3 counties
SW_MC <- SW_Temp_Combined %>% filter(County == "Merced County")
SW_SC <- SW_Temp_Combined %>% filter(County == "Stanislaus County")
SW_SJC <- SW_Temp_Combined %>% filter(County == "San Joaquin County")

# Adding rows for working adults
S_wage_MC <- data.frame(SW_MC[rep(seq_len(dim(SW_MC)[1]), SW_MC$Adult.s.,),
                              , drop = FALSE], row.names=NULL)
S_wage_SC <- data.frame(SW_SC[rep(seq_len(dim(SW_SC)[1]), SW_MC$Adult.s.,),
                              , drop = FALSE], row.names=NULL)
S_wage_SJC <- data.frame(SW_SJC[rep(seq_len(dim(SW_SJC)[1]), SW_MC$Adult.s.,),
                                , drop = FALSE], row.names=NULL)

# Adding working Adults column to the tables
S_wage_MC <- S_wage_MC %>% group_by(Adult.s., Infant.s., Preshooler.s., Schoolager.s., Teenager.s.) %>% 
  mutate(Working_Adults = 1:n()) %>% 
  mutate(Child.Care.Costs = if_else(Working_Adults < Adult.s., 0, Child.Care.Costs))
S_wage_SC <- S_wage_SC %>% group_by(Adult.s., Infant.s., Preshooler.s., Schoolager.s., Teenager.s.) %>% 
  mutate(Working_Adults = 1:n()) %>% 
  mutate(Child.Care.Costs = if_else(Working_Adults < Adult.s., 0, Child.Care.Costs))
S_wage_SJC <- S_wage_SJC %>% group_by(Adult.s., Infant.s., Preshooler.s., Schoolager.s., Teenager.s.) %>% 
  mutate(Working_Adults = 1:n()) %>% 
  mutate(Child.Care.Costs = if_else(Working_Adults < Adult.s., 0, Child.Care.Costs))

# Combining all SW Tables
SW_All <- rbind(S_wage_MC, S_wage_SC, S_wage_SJC)

SW_All <- as.data.frame(SW_All)

#Applying on 50% of the child care costs
SW_All <- SW_All %>% mutate(Child.Care.Costs = Child.Care.Costs*0)

# Calculating Misc. Exp as 10% of all Other Expenses
SW_All <- SW_All %>% mutate(Miscellaneous.costs = rowSums(.[10:14])*0.1)

# Adding all expenses total to Monthly SSW Column
SW_All$Monthly.Self.Sufficiency.Wage <- rowSums(SW_All[ , c(10:15)], na.rm=TRUE)

## ------------------------------CALCULATING TAXES-------------------------------------------------------------

url <- "https://www.bankrate.com/taxes/2021-tax-bracket-rates/" 
page <- read_html(url) #Creates an html document from URL
table1 <- html_table(page, fill = TRUE) #Parses tables into data frames

# Extracting 1st table on the page
Taxes <- table1[1]
Taxes <- do.call(rbind.data.frame, Taxes)

# Checking the Structure of the Taxes Table
str(Taxes)

# Choosing the appropriate columns and rows and cleaning the table for calculations.
Taxes_Temp <- Taxes[-3:-5]
Taxes_Temp <- Taxes_Temp %>% separate(Single, c("First","Last"),sep="to")
Taxes_Temp$`Tax rate` <- gsub("[^0-9]","",Taxes_Temp$`Tax rate`)
Taxes_Temp$First <- gsub("[^0-9]","",Taxes_Temp$First)
Taxes_Temp$Last <- gsub("[^0-9]","",Taxes_Temp$Last)

#Converting all figures to numbers
Taxes_Temp$`Tax rate` <- as.numeric(Taxes_Temp$`Tax rate`)
Taxes_Temp$First <- as.numeric(Taxes_Temp$First)
Taxes_Temp$Last <- as.numeric(Taxes_Temp$Last)
Taxes_Temp[7,3] <- Inf

# Converting Tax rate to decimals
Taxes_Temp$`Tax rate` <- Taxes_Temp$`Tax rate`/100

# Created a function to calculate Federal taxes
income_tax_fed <- function(income,
                           brackets = Taxes_Temp$Last,
                           rates = Taxes_Temp$`Tax rate`) {        
  sum(diff(c(0, pmin(income, brackets))) * rates)
}

##-------------------------------------------------------------------------------

url2 <- "https://www.bankrate.com/taxes/california-state-taxes/#:~:text=California%20has%20nine%20tax%20brackets,the%20California%20Franchise%20Tax%20Board.&text=The%20standard%20deduction%20in%20California,and%20%249%2C606%20for%20married%20households." 
page2 <- read_html(url2) #Creates an html document from URL
table2 <- html_table(page2, fill = TRUE) #Parses tables into data frames

# Extracting 1st table on the page
Taxes_CA <- table2[1]
Taxes_CA <- do.call(rbind.data.frame, Taxes_CA)

# Checking the Structure of the Taxes Table
str(Taxes_CA)

# Choosing the appropriate columns and rows and cleaning the table for calculations.
Taxes_Temp2 <- Taxes_CA[-1,-3]
Taxes_Temp2 <- Taxes_Temp2[-10,]
colnames(Taxes_Temp2) <- c('CA Tax Rates', 'Single')
Taxes_Temp2 <- Taxes_Temp2 %>% separate(Single, c("First","Last"),sep="-")
Taxes_Temp2$`CA Tax Rates` <- gsub("[^0-9]","",Taxes_Temp2$`CA Tax Rates`)
Taxes_Temp2$First <- gsub("[^0-9]","",Taxes_Temp2$First)
Taxes_Temp2$Last <- gsub("[^0-9]","",Taxes_Temp2$Last)

#Converting all figures to numbers
Taxes_Temp2$`CA Tax Rates` <- as.numeric(Taxes_Temp2$`CA Tax Rates`)
Taxes_Temp2$First <- as.numeric(Taxes_Temp2$First)
Taxes_Temp2$Last <- as.numeric(Taxes_Temp2$Last)
Taxes_Temp2[9,3] <- Inf

# Converting Tax rate to decimals
Taxes_Temp2$`CA Tax Rates` <- Taxes_Temp2$`CA Tax Rates`/1000

# Created a function to calculate State taxes (California)
income_tax_CA <- function(income,
                          brackets = Taxes_Temp2$Last,
                          rates = Taxes_Temp2$`CA Tax Rates`) {        
  sum(diff(c(0, pmin(income, brackets))) * rates)
}

#---------------------------------------------------------------------------------------------------------
## Adding fed and state taxes to the taxes column based on the other expenses.
### Taxes are calculated on the total monthly costs and then added back to the monthly costs.
#### Cant' figure out reverse calculation on the costs to have accurate cost of living figures.
##### This would only affect the total annual sufficiency annual wage -/+ upto 6%

SW_All <- SW_All %>% mutate(Taxes = (sapply(SW_All$Monthly.Self.Sufficiency.Wage*12, FUN = income_tax_fed) + 
                                       sapply(SW_All$Monthly.Self.Sufficiency.Wage*12, FUN = income_tax_CA))/12)

SW_All$Monthly.Self.Sufficiency.Wage <- rowSums(SW_All[ , c(10:19)], na.rm=TRUE)

# Calculating Annual (Month * 12) and hourly wage (Month / 176 hours)
## 176 = (30 Days in a year - 8 days of weekends) * 8 Hours
SW_All <- SW_All %>% mutate(Annual.Self.Sufficiency.Wage = Monthly.Self.Sufficiency.Wage * 12) %>%
  mutate(Hourly.Self.Sufficiency.Wage = (Monthly.Self.Sufficiency.Wage / 176)/Working_Adults)

# Rounding off the amounts to cents
SW_All$Monthly.Self.Sufficiency.Wage <- round(SW_All$Monthly.Self.Sufficiency.Wage, digits = 2)
SW_All$Hourly.Self.Sufficiency.Wage <- round(SW_All$Hourly.Self.Sufficiency.Wage, digits = 2)
SW_All$Annual.Self.Sufficiency.Wage <- round(SW_All$Annual.Self.Sufficiency.Wage, digits = 2)

# Selecting necessary rows for joining into Sufficiency wage final table
S_wage_Final <- SW_All %>% select(Adult.s., Infant.s., Preshooler.s., Schoolager.s., Teenager.s.,
                                  County, Hourly.Self.Sufficiency.Wage, Monthly.Self.Sufficiency.Wage,
                                  Annual.Self.Sufficiency.Wage, NChildren, Working_Adults)

# Joining county data with sufficiency wage table by Number of adults, working adults and Number of children.
## Filtering it as 1 observation per household who is the head of the household
Data_Final_Analysis <- left_join(data_final1, S_wage_Final, 
                                 by = c("NAdults"="Adult.s.", "Infants" = "Infant.s.", "Preschoolers" = "Preshooler.s.", 
                                        "Schoolers" = "Schoolager.s.", "Teenagers" = "Teenager.s.", "County" = "County", 
                                        "NChildren" = "NChildren", "Working_Adults" = "Working_Adults"))


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Extracting Poverty Wage of 2021
url <- "https://aspe.hhs.gov/2021-poverty-guidelines" 
page <- read_html(url) #Creates an html document from URL
table1 <- html_table(page, fill = TRUE) #Parses tables into data frames

# Extracting 1st table on the page
P_Wage <- table1[1]
P_Wage <- do.call(rbind.data.frame, P_Wage)

# Cleaning the data table and converting the table to integers
P_Wage <- P_Wage %>% filter(`Persons in family/household` < 8) %>% 
  mutate(`Persons in family/household` = as.integer(`Persons in family/household`)) %>% 
  mutate(`Poverty guideline` = parse_number(`Poverty guideline`))

# Adding Poverty Guidelines to the Data Set
data_Analysis_All <- left_join(Data_Final_Analysis, P_Wage, by = c("FAMSIZE" = "Persons in family/household"))

# Adding Minimum wage to the data set ($15 * 176 Hours a month * 12 Months)
# Data_MC_Final <-  Data_MC_Final %>% mutate(Minimum_Wage = (15*176*12))

data_Analysis <- data_Analysis_All %>% filter(RELATE == 1)

# View and export the final data set

write_xlsx(data_Analysis,
           'Final Data All Counties NW.xlsx')

# ----------------------------------------ANALYSIS------------------------------------------------------------------

# Analyzing the count and the percentage of population under Sufficiency Wage and Poverty Guidelines.
All_County_Analysis <- data_Analysis %>%
  group_by(NAdults, Working_Adults, NChildren) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

MC_Analysis <- data_Analysis %>%
  filter(COUNTYFIP == 47) %>%
  group_by(NAdults, Working_Adults, NChildren) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

SJC_Analysis <- data_Analysis %>%
  filter(COUNTYFIP == 77) %>%
  group_by(NAdults, Working_Adults, NChildren) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

SC_Analysis <- data_Analysis %>%
  filter(COUNTYFIP == 99) %>%
  group_by(NAdults, Working_Adults, NChildren) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

# Extracting the data sets to Excel for further analysis and visualization.
require(openxlsx)
list_of_datasets <- list("All County" = All_County_Analysis, "MC_Analysis" = MC_Analysis,
                         "SJC_Analysis" = SJC_Analysis, "SC_Analysis" = SC_Analysis)
openxlsx::write.xlsx(list_of_datasets, file = "Final Analysis NW.xlsx")

#--------------------------------------------------------------------------------------------------------------------

# Analyzing the count and the percentage of population under Sufficiency Wage and Poverty Guidelines by year
By_Year_All <- data_Analysis %>%
  group_by(MULTYEAR) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

By_Year_MC <- data_Analysis %>%
  filter(COUNTYFIP == 47) %>%
  group_by(MULTYEAR) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

By_Year_SJC <- data_Analysis %>%
  filter(COUNTYFIP == 77) %>%
  group_by(MULTYEAR) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))


By_Year_SC <- data_Analysis %>%
  filter(COUNTYFIP == 99) %>%
  group_by(MULTYEAR) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

# Extracting the data sets to Excel for further analysis and visualization.
require(openxlsx)
list_of_datasets_By_Year <- list("All County" = By_Year_All, "MC_Analysis" = By_Year_MC,
                                 "SJC_Analysis" = By_Year_SJC, "SC_Analysis" = By_Year_SC)
write.xlsx(list_of_datasets_By_Year, file = "Final Analysis By Year NW.xlsx")

#--------------------------------------------------------------------------------------------------------------------

# Analyzing the count and the percentage of population under Sufficiency Wage and Poverty Guidelines by Race
By_Race_All <- data_Analysis %>% 
  group_by(Ethnicity) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

By_Race_MC <- data_Analysis %>%
  filter(COUNTYFIP == 47) %>%
  group_by(Ethnicity) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

By_Race_SJC <- data_Analysis %>%
  filter(COUNTYFIP == 77) %>%
  group_by(Ethnicity) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

By_Race_SC <- data_Analysis %>%
  filter(COUNTYFIP == 99) %>%
  group_by(Ethnicity) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

# Extracting the data sets to Excel for further analysis and visualization.
require(openxlsx)
list_of_datasets_By_Race <- list("All County" = By_Race_All, "MC_Analysis" = By_Race_MC,
                                 "SJC_Analysis" = By_Race_SJC, "SC_Analysis" = By_Race_SC)
write.xlsx(list_of_datasets_By_Race, file = "Final Analysis By Race NW.xlsx")

#--------------------------------------------------------------------------------------------------------------------

# Analyzing the count and the percentage of population under Sufficiency Wage and Poverty Guidelines by Education Attainment
By_Edu_All <- data_Analysis %>% 
  group_by(Education) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

By_Edu_MC <- data_Analysis %>%
  filter(COUNTYFIP == 47) %>%
  group_by(Education) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

By_Edu_SJC <- data_Analysis %>%
  filter(COUNTYFIP == 77) %>%
  group_by(Education) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

By_Edu_SC <- data_Analysis %>%
  filter(COUNTYFIP == 99) %>%
  group_by(Education) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

# Extracting the data sets to Excel for further analysis and visualization.
require(openxlsx)
list_of_datasets_By_Edu <- list("All County" = By_Edu_All, "MC_Analysis" = By_Edu_MC,
                                "SJC_Analysis" = By_Edu_SJC, "SC_Analysis" = By_Edu_SC)
write.xlsx(list_of_datasets_By_Edu, file = "Final Analysis By Education NW.xlsx")

#--------------------------------------------------------------------------------------------------------------------

# Analyzing the count and the percentage of population under Sufficiency Wage and Poverty Guidelines by Citizenship status
By_Citi_All <- data_Analysis %>% 
  group_by(Citizenship) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

By_Citi_MC <- data_Analysis %>%
  filter(COUNTYFIP == 47) %>%
  group_by(Citizenship) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

By_Citi_SJC <- data_Analysis %>%
  filter(COUNTYFIP == 77) %>%
  group_by(Citizenship) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

By_Citi_SC <- data_Analysis %>%
  filter(COUNTYFIP == 99) %>%
  group_by(Citizenship) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

# Extracting the data sets to Excel for further analysis and visualization.
require(openxlsx)
list_of_datasets_By_Citi <- list("All County" = By_Citi_All, "MC_Analysis" = By_Citi_MC,
                                 "SJC_Analysis" = By_Citi_SJC, "SC_Analysis" = By_Citi_SC)
write.xlsx(list_of_datasets_By_Citi, file = "Final Analysis By Citizenship.xlsx")

#-------------------------------------------------------------------------------------------------------

# Analyzing the count and the percentage of population under Sufficiency Wage and Poverty Guidelines by sex of HOH

By_Sex_male <- data_Analysis %>%
  filter(SEX == 1) %>%
  group_by(County) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

By_Sex_female <- data_Analysis %>%
  filter(SEX == 2) %>%
  group_by(County) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

# Extracting the data sets to Excel for further analysis and visualization.
require(openxlsx)
list_of_datasets_By_Sex <- list("Male" = By_Sex_male, "Female" = By_Sex_female)
write.xlsx(list_of_datasets_By_Sex, file = "Final Analysis By Sex.xlsx")

#-------------------------------------------------------------------------------------------------------

# Analyzing the count and the percentage of population under Sufficiency Wage and Poverty Guidelines by Family type

By_Fam_Marry <- data_Analysis_All %>%
  filter(RELATE == 2) %>%
  group_by(County, NChildren) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

# Created a separate set of SERIAL numbers to remove households who are married/have spouse
By_Fam_Temp <- data_Analysis_All %>%
  filter(RELATE == 2)
By_Fam_Temp <- unique(By_Fam_Temp$SERIAL)

By_Fam_Informal <- data_Analysis_All %>%
  filter(!SERIAL %in% c(By_Fam_Temp)) %>%
  filter(NAdults > 1) %>%
  group_by(County, NChildren) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

# Extracting the data sets to Excel for further analysis and visualization.
require(openxlsx)
list_of_datasets_By_Fam <- list("Married" = By_Fam_Marry, "Informal" = By_Fam_Informal)
write.xlsx(list_of_datasets_By_Fam, file = "Final Analysis By Family Type.xlsx")

#-------------------------------------------------------------------------------------------------------

# Analyzing the count and the percentage of population under Sufficiency Wage and Poverty Guidelines by Single moms
By_Moms <- data_Analysis %>%
  filter(SEX == 2 & NAdults == 1 & NChildren > 0) %>%
  group_by(County, NChildren) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

By_Dads <- data_Analysis %>%
  filter(SEX == 1 & NAdults == 1 & NChildren > 0) %>%
  group_by(County, NChildren) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4))

# Extracting the data sets to Excel for further analysis and visualization.
require(openxlsx)
list_of_datasets_By_Parent <- list("Single Mom" = By_Moms, "Single Dad" = By_Dads)
write.xlsx(list_of_datasets_By_Parent, file = "Final Analysis By Single Parent.xlsx")

#-------------------------------------------------------------------------------------------------------

# Analyzing the count and the percentage of population under Sufficiency Wage and Poverty Guidelines by Housing Costs
# Created a combined table of people and columns of expenses to calculate % of households 
##  spending more than 30% of their Annual Income

Combined_Data <- left_join(data_final1, SW_All, 
                           by = c("NAdults"="Adult.s.", "Infants" = "Infant.s.", "Preschoolers" = "Preshooler.s.", 
                                  "Schoolers" = "Schoolager.s.", "Teenagers" = "Teenager.s.", "County" = "County", 
                                  "NChildren" = "NChildren", "Working_Adults" = "Working_Adults")) %>%
  filter(RELATE == 1)

By_House_All <- Combined_Data %>% 
  group_by(County) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[(ADJUSTED_INCOME*0.30) < (Housing.Costs * 12)]),
    Percentage_sw = round(count_sw / total,4))

By_Income <- data_Analysis %>%
  group_by(County) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    Median_Earnings = median(ADJUSTED_INCOME))

# Extracting the data sets to Excel for further analysis and visualization.
require(openxlsx)
list_of_datasets_By_House <- list("Hosuing Cost" = By_House_All, "Median Income" = By_Income)
write.xlsx(list_of_datasets_By_House, file = "Final Analysis By Housing Cost.xlsx")


# Number of Households we missed.
# Cleaning the Data (1 Family per Household)
data_HH <- data %>% filter(STATEFIP == 6) %>% # State - California
  filter(COUNTYFIP == 47 | COUNTYFIP == 77 | COUNTYFIP == 99) %>% # County - Merced, San Joaquin and Stanislaus
  filter(HHINCOME < 9999999) %>% # Income less than 9.99 Million
  filter(NFAMS == 1) %>% # Number of Families per household = 1
  group_by(SERIAL) %>% 
  mutate(NAdults = sum(AGE > 17), # Number of Adults (Age 18 & above)
         Working_Adults = sum(AGE > 17 & EMPSTAT == 1), # Number of Adults who have employment status as employed
         NChildren = sum(AGE < 18),
         Infants = sum(AGE <= 2),
         Preschoolers = sum(AGE >= 3 & AGE <= 5),
         Schoolers = sum(AGE >= 6 & AGE <= 12),
         Teenagers = sum(AGE >= 13 & AGE <= 17)) %>% # Number of Children (Below the age of 18)
  filter(RELATE == 1)

data_final_HH <- data_HH %>%  
  mutate(County = case_when(COUNTYFIP == 47 ~ 'Merced County', ## Changing COUNTYFIP to County Name
                            COUNTYFIP == 77 ~ 'San Joaquin County',
                            COUNTYFIP == 99 ~ 'Stanislaus County'))

All_HH <- data_final_HH %>%
  group_by(NAdults, Working_Adults, NChildren) %>%
  summarize(total = length(SERIAL))

MC_HH <- data_final_HH %>%
  filter(COUNTYFIP == 47) %>%
  group_by(NAdults, Working_Adults, NChildren) %>%
  summarize(total = length(SERIAL))

SJC_HH <- data_final_HH %>%
  filter(COUNTYFIP == 77) %>%
  group_by(NAdults, Working_Adults, NChildren) %>%
  summarize(total = length(SERIAL))

SC_HH <- data_final_HH %>%
  filter(COUNTYFIP == 99) %>%
  group_by(NAdults, Working_Adults, NChildren) %>%
  summarize(total = length(SERIAL))

require(openxlsx)
list_of_datasets_By_HH <- list("All County" = All_HH, "MC_Analysis" = MC_HH,
                               "SJC_Analysis" = SJC_HH, "SC_Analysis" = SC_HH)
write.xlsx(list_of_datasets_By_HH, file = "Final Analysis By HH.xlsx")
