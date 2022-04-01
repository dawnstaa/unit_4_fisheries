#Amadi Real Practice 04/1/2022
#Libraries
library(tidyverse)

#Loading data
load('data/RAMLDB v4.495/R Data/DBdata[asmt][v4.495].RData')

#Accessing part of data relevant to this work
unique(timeseries_units_views$TCbest) #TC = Total catch and is in metric tons
#timeseries_units_views has annual total catch for the fish stock


#joining tables from the dataset that will be used in this work to form a sub dataset
#timeseries, tsmetrics and stock tables # ts = time series
fish_stock = timeseries_values_views %>%
  left_join(stock, by=c("stockid")) %>%
  left_join(taxonomy, by = c("tsn", "scientificname")) %>%
  select(stockid, year, TCbest, tsn, scientificname, commonname, region, FisheryType, taxGroup) #selecting needed columns
dim(timeseries_values_views)
head(timeseries_values_views)
dim(fish_stock)
glimpse(fish_stock)
glimpse(taxonomy)

# getting non NAs in data
max(fish_stock$TCbest, na.rm=TRUE)

#dropping Acadian Red fish cos of how the data was input
fish_stock = fish_stock %>%
  filter(stockid != "ACADREDGOMGB")
glimpse(fish_stock)

#plotting to visualise data
ggplot() +
  geom_line(aes(x=year, y=TCbest, color=stockid), data=fish_stock) +
  theme(legend.position = "none")+
  xlab("Year") + ylab("Total Catch (metric tons)") +
  ggtitle("Total Catch of Fish Stock over the Years")#not including legend cos data is huge
ggsave('figures/total_catch_all_stocks.png', device="png", height=4, width=7, units="in") #

#observations
#significant commercial fishing activity seems to have started after about 1925
#with the greatest catch being in that time (1925-1950). This might be due to 
#technological advancement in fishing vessels and gears making larger catch volume possible.
#This and the relative lack of fisheries stock management/regulations then possibly led to a fishing boom that resulted in overfishing of fish stocks (possibly fisheries was not as regulated then)
#which as can be seen from the figure above. There have been some spikes in total catch after that
# but none as high in volume as at the beginning of the intensification of commercial fishing. Besides,
#the catch volume for the spikes have been decreasing as the years go by.


## American lobster (*Homarus americanus*) fishery
#creating subdata for US lobster stock
lobster_stock = fish_stock %>%
  #filter(grepl("US", region)) %>%#filtering by regions that contain "US"
  filter(grepl("American lobster", commonname), !is.na(TCbest))
glimpse(lobster_stock)
head(lobster_stock)
tail(lobster_stock)

A_lobster_regions = unique(lobster_stock$region)
print(paste("The regions with stock assessment data for the American lobster are the",
            A_lobster_regions[1], "and", A_lobster_regions[2]))#printing output in sentence format to answer question

#plotting to visualise American lobster data over the years, (color=region)
# ggplot() +
#   geom_line(aes(x=year, y=TCbest, color=region), data=lobster_stock) +
#   #facet_wrap(~region)+
#   xlab("Year") + ylab("Total Catch (metric tons)") +
#   ggtitle("Total Catch of American Lobster over the Years")#not including legend cos data is huge
# ggsave('figures/total_catch_Americanlobster_stock.png', device="png", height=4, width=7, units="in") 

#plotting to visualise American lobster data over the years, (color=stockid)
ggplot() +
  geom_line(aes(x=year, y=TCbest, color=stockid), data=lobster_stock) +
  facet_wrap(~region)+
  xlab("Year") + ylab("Total Catch (metric tons)") +
  ggtitle("Total Catch of American Lobster over the Years")#not including legend cos data is huge
ggsave('figures/total_catch_Americanlobster_stock.png', device="png", height=4, width=7, units="in") 

#Histogram
ggplot(lobster_stock) + 
  geom_histogram(aes(x=year, fill=region),alpha = 0.5, binwidth=5)+
  facet_wrap(~region)


#The American lobster fishery in the east coast of the US is relatively young 
#compared to that of the Canadian east coast. There is also a general trend of
#increasing total catch of the American lobster in the latter part of the 1900s 
#moving into the 2000s, recent years, for both regions.American lobster fisheries
#in the US started well before the 1900s and has gone through a series of high and low
# total catch volumes. Interestingly, higher total catch has been recorded in the recent 2000s
# relative to previous years.However, this could be as a result of a combination
#of population growth, increased demand for American lobster, and improved trap-pot
#gear and vessels causing an increase in total catch of the American lobster.

# USFishery with highest annual catch
# fish %>% arrange(desc(TCbest)) %>% glimpse() # what had the highest catch?
US_eastcoast_fish_stock = fish_stock %>% 
  group_by(stockid, scientificname, commonname, region) %>% 
  filter(region=="US East Coast") %>%
  summarize(TCbest_max = sum(TCbest, na.rm=T)) %>%
  arrange(desc(TCbest_max))
#glimpse(US_eastcoast_fish_stock)
head(US_eastcoast_fish_stock)
tail(US_eastcoast_fish_stock)

Most_prod_US_eastcoast_stock[1] = US_eastcoast_fish_stock$scientificname[1][US_eastcoast_fish_stock$TCbest_max[1]]
