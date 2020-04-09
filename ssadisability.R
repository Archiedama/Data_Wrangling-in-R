install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(stringr)
ssa <- read_csv("C:/Users/archi/OneDrive/Documents/R/ssadisability.csv")
glimpse(ssa)
view(ssa)
#converting to long data
ssa_long <- gather(ssa,month,applications,-Fiscal_Year)
print(ssa_long,n=20)
unique(ssa_long$month)
#Splitting the column name as month and application method
ssa_long <- separate(ssa_long,month, c("month","application_method"),"_")
print(ssa_long,n=20)
unique(ssa_long$month)
#displaying All month names in 3 characters
ssa_long$month <- substr(ssa_long$month,1,3)
#looking into fiscal year column
unique(ssa_long$Fiscal_Year)
#updating FY to 20 to represnt year in proper format
ssa_long$Fiscal_Year <- str_replace(ssa_long$Fiscal_Year,"FY","20")
#creating date value with Fiscal_year and month from two columns using lubridate
#paste function concatenates several string functions into a single string
ssa_long$date <- dmy(paste("01",ssa_long$month,ssa_long$Fiscal_Year))
#updated FY to 20 in the previous steps but fiscal year is between Oct of prev year and sept of current year
advanced_dates <- which(month(ssa_long$date) >= 10)
year(ssa_long$date[advanced_dates]) <- year(ssa_long$date[advanced_dates])-1
summary(ssa_long)
#removing unnecessary columns
ssa_long$Fiscal_Year <- NULL
ssa_long$month <- NULL
ssa_long$application_method <- as.factor(ssa_long$application_method)
ssa <- spread(ssa_long,application_method,applications)
print(ssa,n=20)
#efforts to move application online has been successful ?
ssa$online_percentage <- ssa$Internet/ssa$Total *100
ggplot(data=ssa,mapping=aes(x=date, y=online_percentage)) +geom_line()
