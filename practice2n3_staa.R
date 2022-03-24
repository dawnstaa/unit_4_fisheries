#Amadi 2 and 3 together 03-17-2022

library(tidyverse)

load('data/RAMLDB v4.495/R Data/DBdata[asmt][v4.495].RData')
glimpse(timeseries_values_views)
glimpse(stock)

#creating a dataframe oin timeseries, tsmetrics (time series metadata) and stock tables
fish = timeseries_values_views %>%
  left_join(stock, by=c("stockid", "stocklong")) %>%
  left_join(taxonomy, by = c("tsn", "scientificname")) %>% # could join metadata table instead
  select(stockid, year, TCbest, tsn, scientificname, commonname, region,
         FisheryType, taxGroup)
#checking
dim(timeseries_values_views)
dim(fish)
glimpse(fish)
head(fish)

max(fish$TCbest, na.rm=T)

#plotting
fish = fish %>%
  filter(stockid != "ACADREDGOMGB")

ggplot() +
  geom_line(aes(x=year, y=TCbest, color=stockid), data=fish) +
  theme(legend.position = "none") #turning off the lengend cos theres too many
ggsave('figures/total_catch_all_stocks.png', device="png", height=4,
       width=7, units="in")

unique(timeseries_units_views$TCbest)
# Fishery with highest annual catch
# fish %>% arrange(desc(TCbest)) %>% glimpse() # what had the highest catch?
fish %>% group_by(stockid, commonname, region) %>% 
  summarize(TCbest_max = sum(TCbest, na.rm=T)) %>% 
  arrange(desc(TCbest_max))

#fish %>% filter(TCbest > 20000000) #trouble shooting
#examine cod collapse
cod_regions = fish %>% 
  filter(scientificname == "Gadus morhua") %>%
  distinct(region)
cod_regions

# Sum best Total Catch estimates for Cod across all Canada East Coast stock assessments       
cod = fish %>% 
  filter(scientificname == "Gadus morhua",
         region == "Canada East Coast", 
         !is.na(TCbest))
summary(cod)

# Plot Canada East Coast cod total catch time series
ggplot(aes(x=year, y=TCbest, color=stockid), data=cod) + 
  geom_line() +
  labs(x= "Year", y= "Total Catch (Metric Tons)", 
       title = "Cod Total Catch in East Canadian Coast")

#Examine fishery collapse across ALL stocks
#collapse dataframe
dat = c(1,3,6,2,3,9,-1)
cummax(dat) # Cummax() returns the cumulative maximum value

# Find all stocks that have collapsed
# Find the historical max total catch for each year in the time series
# cummax() in row i provides the max value in rows 0 - i
# Define collapse as a total catch <= 10% of the historical max catch
##first try just grouping by stock id
collapse = fish %>% 
  filter(!is.na(TCbest)) %>%  # Remove NAs (which can't be ignored with cummax())
  group_by(stockid) %>% # effectively just groups by stockid since other variables are the same for a given stockid
  mutate(historical_max_catch = cummax(TCbest),   
         collapse = TCbest < 0.10 * historical_max_catch) %>%
  summarize(ever_collapsed = any(collapse))# %>%
  #ungroup()
glimpse(collapse)
head(collapse)
##grouping by others plus stock id so it's easy to figure out what corresponds to what
collapse = fish %>% 
  filter(!is.na(TCbest)) %>%  # Remove NAs (which can't be ignored with cummax())
  group_by(stockid, tsn, scientificname, commonname, region, FisheryType, taxGroup) %>% # effectively just groups by stockid since other variables are the same for a given stockid
  mutate(historical_max_catch = cummax(TCbest),   
         collapse = TCbest < 0.10 * historical_max_catch) %>%
  summarize(ever_collapsed = any(collapse)) %>%
  ungroup()#make sure to ungroup after summary

glimpse(collapse)
head(collapse)

##how many of each fishery type has collapsed and which has never collapsed
ggplot() +
  geom_bar(aes(x=FisheryType), data=collapse) + # counts rows (stocks) of each FisheryType in data
  facet_wrap(~ever_collapsed) + # separates by whether stock has experienced a collapse
  coord_flip() #for a paper, turn the x-axis labels at 45degrees angle so they are all legible

### Logistic regression 
#This is a kind of generalised linear model; so you constrain it between 0 and 1 e.g yes/noe, male/female
#you do not have as many conditions to meet

# Remove time series information and collapse data to "has this stock EVER collapsed?"
#converting from a string into a categorical variable or factor to avoid mistakes
model_data = collapse %>%
  mutate(FisheryType = as.factor(FisheryType)) %>%
  filter(!is.na(FisheryType))

# Run a logistic regression (run the model)
model_l = glm(ever_collapsed ~ FisheryType,
              data = model_data, family = "binomial")
summary(model_l)

# arrange fishery type alphabetically to see that intercept represents "Flatfish" 
model_data %>% 
  distinct(FisheryType) %>% 
  arrange(FisheryType) #(full length of fishery types)

#tuna and marlin are less likely to collapse than flatfish
FisheryType = model_data %>% distinct(FisheryType)
model_l_predict = predict(model_l, newdata=FisheryType,
                          type="response", se.fit=TRUE)
#'predict is just a 'response' predicts in the scale of y rather than
# as a linear combination of the x 

# Organize predictions into a tidy table
collapse_predictions = cbind(FisheryType, model_l_predict)

# Plot predictions and SE bars
ggplot(aes(x=FisheryType, y=fit, fill=FisheryType),
       data=collapse_predictions) +
  geom_bar(stat="identity", show.legend = FALSE) + # stat="count" is default (like histogram)
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=0.2) +
  coord_flip() +
  ylab("Probability of stock collapse")
# theme(legend.position = "none") # another way to hide a legend
#ymin=fit-se.fit, ymax=fit+se.fit - lets error bar go down by 1sd and up by 1sd
#coord_flip so the labels don't run into each other


#PCA to reduce the dimensions of the variables


