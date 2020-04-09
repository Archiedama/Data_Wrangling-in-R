library(tidyverse)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)
library(dplyr)
getwd()
setwd('C:/Users/archi/OneDrive/Documents/R')
water <- read_csv('C:/Users/archi/OneDrive/Documents/R/Water_Quality_Sampling_Data.csv')
View(water)
glimpse(water)
#restructure the tibble with required columns
water <- tibble('SiteName'=water$SITE_NAME,'SiteType'=water$SITE_TYPE,
                'SampleTime'=water$SAMPLE_DATE,'ParameterType'=water$PARAM_TYPE,
                'Parameter'=water$PARAMETER,'result'=water$RESULT,
                'unit'=water$UNIT)
glimpse(water)
#Finding PH and water temperature parameter records for analysis
unique(water$Parameter)
#there are more than 3000 parameters
#searching for string to find PH and water temperature using string detect
#Finding PH from this large data is very difficult
unique(water[which(str_detect(water$Parameter,'PH')),]$Parameter)
#Looking into Parameter type instead which has 100 unique values
unique(water$ParameterType)
#subset rows which have PH and water temperature parameter type values
filtered_water <- subset(water,(ParameterType=='Alkalinity/Hardness/pH') | 
                           (ParameterType =='Conventionals' ))
glimpse(filtered_water)
#check the corresponding parameter values 
unique(filtered_water$Parameter)
#subsetting the above data to get PH and water temperature
filtered_water <- subset(filtered_water,(Parameter=='pH') | (Parameter =='WATER TEMPERATURE'))
#The data set has been reduced to 29065 from 1274769
glimpse(filtered_water) 
unique(filtered_water$Parameter)
#analysing the data type
summary(filtered_water)
#converting the data types to appropriate forms
filtered_water$SiteType <- as.factor(filtered_water$SiteType)
filtered_water$ParameterType <- as.factor(filtered_water$ParameterType)
filtered_water$Parameter <- as.factor(filtered_water$Parameter)
filtered_water$unit <- as.factor(filtered_water$unit)
#looking into sample time colums
#Converting sample time to datetime datatype using lubridate
filtered_water$SampleTime
filtered_water$SampleTime <- mdy_hms(filtered_water$SampleTime)
#units seems to be in different formats, Water temperature cannot be measured in Feet
#There is one value in feet
subset(filtered_water,unit=='Feet')
#Looks like Fareiheit was wrongly input as Feet
#Finding the row number containing this error
convert <- which(filtered_water$unit=='Feet')
filtered_water$unit[convert] <- 'Deg. Fahrenheit'
#Looking into mg/L unit to see if they belong to C or F
#There is one row with unit greater than 70
glimpse(subset(filtered_water,unit=='MG/L' & result >70))
#convert this value to F
convert <- which(filtered_water$unit=='MG/L' & filtered_water$result>70)
filtered_water$unit[convert] <- 'Deg. Fahrenheit'
#convert the remaining values to celcius
convert1 <- which(filtered_water$unit=='MG/L')
filtered_water$unit[convert1] <- 'Deg. Celsius'
summary(filtered_water)
#remove the column names which have zero values using drop levels
filtered_water$unit <- droplevels(filtered_water$unit)
summary(filtered_water)
#checking outliers in data
ggplot(data=filtered_water, mapping=aes(x=SampleTime,y=result))+geom_point()
#Digging deeper using box plot
#There are 2 points which look like they belong to fareinheit
ggplot(data=filtered_water, mapping=aes(x=unit,y=result))+geom_boxplot()
#convert those two points to Fahrenheit
convert2 <- which(filtered_water$result > 60 & filtered_water$unit=='Deg. Celsius')
filtered_water$unit[convert2] <- 'Deg. Fahrenheit'
#convert all data to a single unit of Celcius
#Before doing this, convert the factor to character, apply F to C formula and convert back to factor
#This is done as NA can be generated
filtered_water$unit <- as.character(filtered_water$unit)
Fahrenheit <- which(filtered_water$unit == 'Deg. Fahrenheit')
#Farenheit to celcius=(32°F ??? 32) × 5/9
filtered_water$result[Fahrenheit] <- (filtered_water$result[Fahrenheit]-32) * (5.0/9.0)
filtered_water$unit[Fahrenheit] <- 'Deg. Celcius'
filtered_water$unit  <- as.factor(filtered_water$unit)
summary(filtered_water)
#There is only one paramter and parameter type, these can be removed
filtered_water <- filtered_water[,c(-4)]


