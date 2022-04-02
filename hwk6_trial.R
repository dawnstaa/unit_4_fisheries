#Amadi Real Practice 04/1/2022
#Libraries
library(tidyverse)
library(ggplot2)
install.packages("zoom")
library(zoom)
install.packages("ggforce")
library(ggforce)

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

#dropping Acadian Red fish cos of how the data was calculated and input
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
#creating subdata for American lobster stock
A_lobster_stock = fish_stock %>%
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

#plotting to see trend over the years for American lobster between the two regions
#plotting to visualise American lobster data over the years, (color=stockid)
ggplot() +
  geom_line(aes(x=year, y=TCbest, color=stockid), data=A_lobster_stock) +
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

# Question 2: Most productive US Fishery (using highest annual catch)
# fish %>% arrange(desc(TCbest)) %>% glimpse() # what had the highest catch?
US_eastcoast_fish_stock = fish_stock %>% 
  group_by(stockid, scientificname, commonname, region) %>% 
  filter(region=="US East Coast") %>%
  summarize(TCbest_max = sum(TCbest, na.rm=T)) %>%
  arrange(desc(TCbest_max))
#glimpse(US_eastcoast_fish_stock)
head(US_eastcoast_fish_stock)
tail(US_eastcoast_fish_stock)

#Most_prod_US_eastcoast_stock = US_eastcoast_fish_stock$scientificname[US_eastcoast_fish_stock$TCbest_max[1]]
Most_prod_US_eastcoast_stock = c(US_eastcoast_fish_stock$scientificname[US_eastcoast_fish_stock$TCbest_max==23973000],
                                 US_eastcoast_fish_stock$commonname[US_eastcoast_fish_stock$TCbest_max==23973000])
#Printing output to screen in a sentence (Ans to Question2)
print(paste(Most_prod_US_eastcoast_stock[2], "(", "*", Most_prod_US_eastcoast_stock[1],"*",")",
            "is the most productive fishery of the US east coast."))

#Q3
#creating subdata for only US lobster stock
US_lobster_stock = fish_stock %>%
  filter(grepl("US", region)) %>%#filtering by regions that contain "US"
  filter(grepl("lobster", commonname), !is.na(TCbest))
head(US_lobster_stock)
glimpse(US_lobster_stock)
tail(US_lobster_stock)

#plotting to visualise US lobster stock over the years, (color=stockid)
ggplot() +
  geom_line(aes(x=year, y=TCbest, color=stockid), data=US_lobster_stock) +
  xlab("Year") + ylab("Total Catch (metric tons)") +
  ggtitle("Timeseries of Total Catch of US Lobster Stocks")#not including legend cos data is huge
ggsave('figures/total_catch_US_lobster_stocks.png', device="png", height=4, width=7, units="in") 
#SNE - South New England, GOM - Gulf of Maine, GB - Georges Bank
#Tthe is an overall increasing trend in the lobster stock of the Gulf of Maine
#but an increasing trend in the South New England lobster stock that starts to dip
#from the late 1900s through to 2007. The lobster stock in Georges Bank has an
#almost flat trend with not sustained significant rise or fall in total catch. 
#Right from the start of 1981, the Gulf of main has had the highest total catch for lobster
#or the most productive lobster fishery of the three lobster fishery locations in the US.

#Section 2 Red Snapper
Red_snapper_stock = timeseries_values_views %>%
  left_join(stock, by=c("stockid")) %>%
  left_join(taxonomy, by = c("tsn", "scientificname")) %>%
  select(stockid, year, TCbest, tsn, scientificname, commonname, region,
         FisheryType, taxGroup, UdivUmsypref, BdivBmsypref) %>%
  filter(stockid == "RSNAPSATLC")

glimpse(Red_snapper_stock)
head(Red_snapper_stock)

#Plotting timeseeries of both BdivBmsypref and UdivUmsypref for Atlantic red snapper
#plotting time series of UdivUmsypref 
#UdivUmsypref - fishing pressure relative to estimated fishing pressure at Maximum Sustainable Yield)
ggplot() +
  geom_line(aes(x=year, y=UdivUmsypref, color=stockid), data=Red_snapper_stock) +
  xlab("Year") + ylab("Fishing Pressure at MSY") +
  ggtitle("Timeseries for Red Snapper Stock - fishing pressure")#not including legend cos data is huge
ggsave('figures/timeseries_Red_snapper_tock_fishnpressure.png', device="png", height=4, width=7, units="in") 
#Red snapper fishing pressure at maximum sustainable yield has a general increasing
#trend until its peak in the early 1980s then decreases sharply in the 1900s and alternatively
#increases and decreases between the 1900s and 2000s.


#plotting time series of BdivBmsypref 
#BdivBmsypref  - stock biomass relative to estimated biomass at Maximum Sustainable Yield
ggplot() +
  geom_line(aes(x=year, y=BdivBmsypref, color=stockid), data=Red_snapper_stock) +
  xlab("Year") + ylab("Biomass at MSY") +
  ggtitle("Timeseries for Red Snapper - Biomass")#not including legend cos data is huge
ggsave('figures/timeseries_Red_snapper_tock_Biomass.png', device="png", height=4, width=7, units="in") 
#Red snapper Bbiomass relative to estimated biomass at maximum sustainable yield was greatest at the start of of the fishery
#, specifically before 1960 and then starts to plummet sharply from about 1955 
#through to about 1980. It has not significantly recovered since. the sharp decrease
#in biomass from about 1955 coincides with the the sharp increase in fishing pressure
#that started about the same time and peaked about the same time frame as well.

#Plotting UdivUmsypref vs. BdivBmsypref
ggplot() +
  geom_line(aes(x=BdivBmsypref, y=UdivUmsypref, color=stockid), data=Red_snapper_stock) +
  geom_smooth()+
  xlab("Biomass at MSY") + ylab("Fishing Pressure at MSY") +
  ggtitle("Fishing Pressure at MSY vs. Biomass at MSY")+
  theme_bw()
ggsave('figures/timeseries_Red_snapper_tock_fishnpressure_vs_Biomass.png', device="png", height=4, width=7, units="in") 

#zooming in 
ggplot() +
  geom_line(aes(x=BdivBmsypref, y=UdivUmsypref, color=stockid), data=Red_snapper_stock) +
  ggtitle("Fishing Pressure at MSY vs. Biomass at Maximum Sustainability Yield")+
  xlab("Biomass at MSY") + ylab("Fishing Pressure at MSY") +
  theme_bw()+
  scale_y_log10()+
  facet_zoom(xlim = c(0, 0.2))
ggsave('figures/timeseries_Red_snapper_tock_fishnpressure_vs_Biomass.png', device="png", height=4, width=7, units="in") 

#High fishing pressure of red snapper at maximum sustainability yield (MSY) corresponds to
#low red snapper biomass at maximum sustainability yield. This implies that the higher
#the fishing pressure, the lower the biomass at maximum sustainability yield 
#(sustained intensified fishing corresponds to low biomass of red snapper caught)


## Logistic regression

#Find all stocks that have collapsed
collapse = timeseries_values_views %>% 
  filter(!is.na(TCbest)) %>%  # Remove NAs (which can't be ignored with cummax())
  group_by(stockid) %>%
  mutate(historical_max_catch = cummax(TCbest),
         current_collapse = TCbest < 0.10 * historical_max_catch) %>%
  summarize(ever_collapsed = any(current_collapse)) %>% # Has stock ever collapsed throughout time series?
  ungroup()


collapse1 = fish_stock %>% 
  filter(!is.na(TCbest)) %>%  # Removing NAs 
  group_by(stockid, tsn, scientificname, commonname, region, FisheryType, taxGroup) %>% # effectively just groups by stockid since other variables are the same for a given stockid
  mutate(historical_max_catch = cummax(TCbest),   
         collapse = TCbest < 0.10 * historical_max_catch) %>%
  summarize(ever_collapsed = any(collapse)) %>%
  ungroup()
# head(collapse1)
# glimpse(collapse1)
#plotting
ggplot() +
  geom_bar(aes(x=FisheryType), data=collapse1) + # counts rows (stocks) of each FisheryType in data
  facet_wrap(~ever_collapsed) + # separates by whether stock has experienced a collapse
  coord_flip()

# Removing time series information and collapsing data 
#collapsing data to "has this stock EVER collapsed?"
model_stock_data = collapse1 %>%
  mutate(FisheryType = as.factor(FisheryType)) %>%
  filter(!is.na(FisheryType))

# Running a logistic regression
glm_1 = glm(ever_collapsed ~ FisheryType, data = model_stock_data, family = "binomial")
summary(glm_1)

# arrange fishery type alphabetically for intercept visibility 
model_stock_data %>% 
  distinct(FisheryType) %>% 
  arrange(FisheryType)

#Since the coefficients for rockfish, gadids and forage fish are positive and significant,
#it implies that they are more likely to collapse relative to flat fish.
#Tuna and marlin are just about on the border with respect to the statistical 
#significance of their negative coefficient. Thus, relative to flatfish, 
#tuna and marlin are less likely to collapse


#generating model predictions of a stock's probability of ever experiencing a collapse
# for each fishery type

# Make predictions on the probability of a stock collapse by fishery type
fishery_type = model_stock_data %>% 
  distinct(FisheryType)

glm_1_predict = predict(glm_1, newdata=fishery_type, type="response", se.fit=TRUE)

# Organize predictions into a tidy table
collapse1_predictions = cbind(fishery_type, glm_1_predict)

# Plot predictions and SE bars
ggplot(aes(x=FisheryType, y=fit, fill=FisheryType), data=collapse1_predictions) +
  geom_bar(stat="identity", show.legend = FALSE) + # stat="count" is default (like histogram)
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=0.2) +
  coord_flip() +
  ylab("Probability of Stock Collapse")+
  ggtitle("Probability of Fish Stock Collapsing")

# Observations

#Rockfish is most likely to collapse, followed by gadids and forage fish.
#Tuna and marlin are next after forage fish in terms of likelihood to collapse. 
#The error bars represent one standard error about the mean






# theme(legend.position = "none") # another way to hide a legend

# Calculate McFadden Pseudo-R^2: 0.2-0.4 = "Excellent fit"
# library(pscl)
# pscl::pR2(model_l)["McFadden"] # lousy fit
# model_l = glm(ever_collapsed ~ region + FisheryType, data = model_data, family = "binomial") # add region
# pscl::pR2(model_l)["McFadden"] # better fit
```
