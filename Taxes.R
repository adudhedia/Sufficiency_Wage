url <- "https://www.bankrate.com/taxes/tax-brackets/" 
page <- read_html(url) #Creates an html document from URL
table1 <- html_table(page, fill = TRUE) #Parses tables into data frames

# Extracting 1st table on the page
Taxes <- table1[1]
Taxes <- do.call(rbind.data.frame, Taxes)

str(Taxes)

Taxes_Temp <- Taxes[-1,-3:-5]
Taxes_Temp <- Taxes_Temp %>% separate(Single, c("First","Last"),sep="to")
Taxes_Temp$`Tax rate` <- gsub("[^0-9]","",Taxes_Temp$`Tax rate`)
Taxes_Temp$First <- gsub("[^0-9]","",Taxes_Temp$First)
Taxes_Temp$Last <- gsub("[^0-9]","",Taxes_Temp$Last)

Taxes_Temp$`Tax rate` <- as.numeric(Taxes_Temp$`Tax rate`)
Taxes_Temp$First <- as.numeric(Taxes_Temp$First)
Taxes_Temp$Last <- as.numeric(Taxes_Temp$Last)
Taxes_Temp[7,3] <- Inf

Taxes_Temp$`Tax rate` <- Taxes_Temp$`Tax rate`/100

income_tax_fed <- function(income,
                           brackets = Taxes_Temp$Last,
                           rates = Taxes_Temp$`Tax rate`) {        
  sum(diff(c(0, pmin(income, brackets))) * rates)
}

income_tax_fed(26289.12)

##-------------------------------------------------------------------------------

url2 <- "https://www.bankrate.com/taxes/california-state-taxes/#:~:text=California%20has%20nine%20tax%20brackets,the%20California%20Franchise%20Tax%20Board.&text=The%20standard%20deduction%20in%20California,and%20%249%2C606%20for%20married%20households." 
page2 <- read_html(url2) #Creates an html document from URL
table2 <- html_table(page2, fill = TRUE) #Parses tables into data frames

# Extracting 1st table on the page
Taxes_CA <- table2[1]
Taxes_CA <- do.call(rbind.data.frame, Taxes_CA)

str(Taxes_CA)

Taxes_Temp2 <- Taxes_CA[-1,-3]
Taxes_Temp2 <- Taxes_Temp2[-10,]
colnames(Taxes_Temp2) <- c('CA Tax Rates', 'Single')
Taxes_Temp2 <- Taxes_Temp2 %>% separate(Single, c("First","Last"),sep="-")
Taxes_Temp2$`CA Tax Rates` <- gsub("[^0-9]","",Taxes_Temp2$`CA Tax Rates`)
Taxes_Temp2$First <- gsub("[^0-9]","",Taxes_Temp2$First)
Taxes_Temp2$Last <- gsub("[^0-9]","",Taxes_Temp2$Last)

Taxes_Temp2$`CA Tax Rates` <- as.numeric(Taxes_Temp2$`CA Tax Rates`)
Taxes_Temp2$First <- as.numeric(Taxes_Temp2$First)
Taxes_Temp2$Last <- as.numeric(Taxes_Temp2$Last)
Taxes_Temp2[9,3] <- Inf

Taxes_Temp2$`CA Tax Rates` <- Taxes_Temp2$`CA Tax Rates`/1000

income_tax_CA <- function(income,
                           brackets = Taxes_Temp2$Last,
                           rates = Taxes_Temp2$`CA Tax Rates`) {        
  sum(diff(c(0, pmin(income, brackets))) * rates)
}

(income_tax_fed(56288.23)+income_tax_CA(26288.23))/12

