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


# Fishery with highest annual catch
# fish %>% arrange(desc(TCbest)) %>% glimpse() # what had the highest catch?
fish_stock %>% 
  group_by(stockid, commonname, region) %>% 
  summarize(TCbest_max = sum(TCbest, na.rm=T)) %>%
  arrange(desc(TCbest_max))






