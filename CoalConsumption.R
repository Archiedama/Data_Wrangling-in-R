#Coal consumption
library(tidyverse)
#read data
coal <- read_csv('C:/Users/archi/OneDrive/Documents/R/coal.csv')
view(coal)
#Removing top two rows from the table
coal <- read_csv('C:/Users/archi/OneDrive/Documents/R/coal.csv',skip=2)
glimpse(coal)
#Renaming column X1 to region
colnames(coal)[1] <- 'region'
glimpse(coal)
#converting wider dataset to longer dataset
?gather
coal_long <- gather(coal,Year,CoalConsumption,-region)
glimpse(coal_long)
#Converting data types
coal_long$Year <- as.integer(coal_long$Year)
coal_long$CoalConsumption <- as.numeric(coal_long$CoalConsumption)
summary(coal_long)
#Each tibble should conatin information about single type of observation unit
#Looks like region column has country, Region and continent
unique(coal_long$region)
noncountries <- c("North America","Central & South America","Antartica","Europe",
                  "Eurasia","Middle East","Africa","Asia & Oceania","World")
#displays all rows, the number 2 represents "Central & South America" and is found in the 8th row of the dataset
match(coal_long$region, noncountries)
#True displayed, where there is a continent name
!is.na(match(coal_long$region, noncountries)) 
#rows which have continents
matches <- which(!is.na(match(coal_long$region, noncountries)))
coal_country <- coal_long[-matches,]
coal_region <- coal_long[matches,]
unique(coal_country$region)
unique(coal_region$region)
coal_long
#World is an extra row which looks like a aggregation of all data which can be removed
library(dplyr)
coal_long <- coal_long %>%
  filter(region != 'World') 
unique(coal_long$region)
coal_region <- coal_region %>%
  filter(region != 'World') 
coal_country <- coal_country %>%
  filter(region != 'World') 
#Visualization-Coal consumption based on regions
ggplot(data=coal_region, mapping=aes(x=Year, y=CoalConsumption)) +
  geom_point(mapping=aes(color=coal_region$region))
ggplot(data=coal_region, mapping=aes(x=Year, y=CoalConsumption)) +
  geom_line(mapping=aes(color=region))
#top coal consuming countries
Top_countries <- coal_country %>%
  group_by(region) %>%
  summarize(CoalConsumption=max(CoalConsumption))%>%
  arrange(desc(CoalConsumption)) %>%
  top_n(10)
Top_countries
ggplot(data=Top_countries,mapping=aes(x=region,y=CoalConsumption,))+
         geom_point()+ theme(axis.text.x = element_text(angle = 90))
  





