#Install Packages
install.packages("tidyverse")
install.packages("ipumsr")
install.packages("R.utils") 
install.packages("writexl")
install.packages("readxl")
install.packages("openxlsx")
install.packages("janitor")

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

#Setting working directory and API key
setwd("~/Sufficieny_Wage")
set_ipums_api_key("59cba10d8a5da536fc06b59d2b567c87db0d4dc6848600548a4fdc7e", save = TRUE) # Please Use your own API

# Extracting necessary Data from IMPUMS
data <- 
  define_extract_usa(
    "USA extract for API vignette",
    c("us2019c"), # 2015-2019 ACS 5-Year
    c("STATEFIP","COUNTYFIP","NCHILD","FAMSIZE","EMPSTAT","HHINCOME","NFAMS", "FAMUNIT", "AGE", "RElATE", "ADJUST")
  ) %>% 
  submit_extract() %>% 
  wait_for_extract() %>% 
  download_extract() %>% 
  read_ipums_micro()

# Summary of the Data
str(data)
head(data)

# Cleaning the Data (1 Family per Household)
data_final <- data %>% filter(STATEFIP == 6) %>% 
  filter(COUNTYFIP == 47 | COUNTYFIP == 77 | COUNTYFIP == 99) %>% 
  filter(HHINCOME < 9999999) %>%
  filter(NFAMS == 1) %>%
  filter(FAMSIZE < 8) %>%
  mutate(ADJUSTED_INCOME = ADJUST * HHINCOME) %>%
  group_by(SERIAL) %>%
  mutate(NAdults = sum(AGE > 17), Working_Adults = sum(AGE > 17 & EMPSTAT == 1), NChildren = sum(AGE < 18)) %>%
  filter(NAdults > 0 & NAdults < 5) %>%
  filter(NChildren < 4) %>% 
  filter(Working_Adults > 0)

# Separating the data into 3 counties
Data_Merced_County <- data_final %>% filter (COUNTYFIP == 47)
Data_San_Joaquin_County <- data_final %>% filter (COUNTYFIP == 77)
Data_Stanislaus_County <- data_final %>% filter (COUNTYFIP == 99)


write_xlsx(Data_Merced_County, 'C:\\Users\\adudhedi\\OneDrive - University of the Pacific\\Desktop\\Living Wage\\data.xlsx')


STAT <- aggregate(Data_Merced_County$SERIAL, 
                  by=list(Data_Merced_County$NChildren,Data_Merced_County$NAdults, Data_Merced_County$Working_Adults), 
                  function(x) length(unique(x)))
sum(STAT$x)

S_wage <- read.xlsx("https://selfsufficiencystandard.org//wp-content/uploads/2021/10/CA2021_AllFamilies.xlsx",sheet=3)

str(S_wage)

S_wage_Final <- S_wage[-c(1)]

S_wage_Final <- transform(S_wage_Final,`Adult(s)` = as.numeric(`Adult(s)`),
                          `Infant(s)` = as.numeric(`Infant(s)`),
                          `Preshooler(s)` = as.numeric(`Preshooler(s)`),
                          `Schoolager(s)` = as.numeric(`Schoolager(s)`),
                          `Teenager(s)` = as.numeric(`Teenager(s)`))

S_wage_Final[is.na(S_wage_Final)] = 0

S_wage_Temp <- S_wage_Final %>% 
  filter(Adult.s. < 5 & County == "Merced County") %>%
  filter(Preshooler.s. < 2 & Schoolager.s. < 2 & Teenager.s. < 2) %>%
  filter(Adult.s. < 4 & Infant.s. == 0 & Preshooler.s. == 0 & Schoolager.s. == 0 & Teenager.s. == 0 | 
           Adult.s. < 4 & Infant.s. == 0 & Preshooler.s. == 1 & Schoolager.s. == 0 & Teenager.s. == 0 |
           Adult.s. < 4 & Infant.s. == 0 & Preshooler.s. == 1 & Schoolager.s. == 1 & Teenager.s. == 0 | 
           Adult.s. < 4 & Infant.s. == 0 & Preshooler.s. == 1 & Schoolager.s. == 1 & Teenager.s. == 1 | 
           Adult.s. == 4 & Infant.s. < 4) %>%
  mutate(NChildren = select(., Infant.s.:Teenager.s.) %>% rowSums(na.rm = TRUE))

S_wage_Temp <- S_wage_Temp[-c(2:5)]

S_wage_Temp <- S_wage_Temp %>%
  group_by(Housing.Costs) %>%
  mutate(across(c(Child.Care.Costs), 
                ~ifelse(Adult.s. == 4, 
                        .[Adult.s. == 3], .))) %>%
  ungroup

a <- data.frame(S_wage_Temp[rep(seq_len(dim(S_wage_Temp)[1]), S_wage_Temp$Adult.s.),
                            , drop = FALSE], row.names=NULL)

a <- a %>% group_by(Hourly.Self.Sufficiency.Wage) %>% mutate(Working_Adults = 1:n())


a <- a %>% mutate(Child.Care.Costs = if_else(Working_Adults < Adult.s., 0, Child.Care.Costs))


a$Monthly.Self.Sufficiency.Wage <- rowSums(a[ , c(6:15)], na.rm=TRUE)

b <- a %>% mutate(Annual.Self.Sufficiency.Wage = Monthly.Self.Sufficiency.Wage * 12) %>%
  mutate(Hourly.Self.Sufficiency.Wage = (Monthly.Self.Sufficiency.Wage / 176)/Working_Adults)

b$Monthly.Self.Sufficiency.Wage <- round(b$Monthly.Self.Sufficiency.Wage, digits = 2)
b$Hourly.Self.Sufficiency.Wage <- round(b$Hourly.Self.Sufficiency.Wage, digits = 2)

d <- b %>% select(Adult.s., Working_Adults, NChildren, Annual.Self.Sufficiency.Wage)
c <- left_join(Data_Merced_County, d, by = c("NAdults"="Adult.s.", "Working_Adults"="Working_Adults", "NChildren"="NChildren"))

f <- c %>% filter(RELATE == 1)

url <- "https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2021-poverty-guidelines" 
page <- read_html(url) #Creates an html document from URL
table1 <- html_table(page, fill = TRUE) #Parses tables into data frames
table1

e <- table1[1]
e <- do.call(rbind.data.frame, e)
e <- e %>% filter(`Persons in family/household` < 8) %>% 
  mutate(`Persons in family/household` = as.integer(`Persons in family/household`)) %>% 
  mutate(`Poverty guideline` = parse_number(`Poverty guideline`))

g <- left_join(f, e, by = c("FAMSIZE" = "Persons in family/household"))
h <- g %>% mutate(Minimum_Wage = (15*176*12))

write_xlsx(h, 'C:\\Users\\adudhedi\\OneDrive - University of the Pacific\\Desktop\\Living Wage\\Data_Final.xlsx')

S_1 <- h %>%
  group_by(NAdults, Working_Adults, NChildren) %>%
  summarize(
    total = length(ADJUSTED_INCOME),
    count_sw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Annual.Self.Sufficiency.Wage]),
    count_pw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<`Poverty guideline`]),
    count_mw = length(ADJUSTED_INCOME[ADJUSTED_INCOME<Minimum_Wage]),
    Percentage_sw = round(count_sw / total,4),
    Percentage_pw = round(count_pw / total,4),
    Percentage_mw = round(count_mw / total,4)) %>% 
  adorn_totals("row")

S_1[41,2:3] = 0
S_1[41,8:10] = 0
S_1[41,8] = round(S_1[41,5]/S_1[41,4],4)
S_1[41,9] = round(S_1[41,6]/S_1[41,4],4)
S_1[41,10] = round(S_1[41,7]/S_1[41,4],4)

S_1$Percentage_sw <- label_percent(scale = 100)(S_1$Percentage_sw)
S_1$Percentage_pw <- label_percent(scale = 100)(S_1$Percentage_pw)
S_1$Percentage_mw <- label_percent(scale = 100)(S_1$Percentage_mw)
write_xlsx(S_1, 'C:\\Users\\adudhedi\\OneDrive - University of the Pacific\\Desktop\\Living Wage\\Final_Analysis_MC.xlsx')

str(S_1)
View(S_1)
