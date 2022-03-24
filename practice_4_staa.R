# Amadi 03/22/22
#PCA
library(tidyverse)
library(palmerpenguins)

head(penguins)

pen_drop_na = penguins %>%
  drop_na() # NAs are not allowed in a PCA
head(pen_drop_na)
# Isolate the variables I want to use in my PCA
pen_num = pen_drop_na %>%
  select(ends_with("mm"), body_mass_g) 

# # Metadata associated with pen_num (maintain row order!)
# pen_meta = pen_drop_na %>%
#   select(species, sex, island, year)
# head(pen_num)

#Run PCA
pen_pca = prcomp(pen_num, scale.=TRUE, center=TRUE) #scale normalizes the 
#variables so each one has a chance to have an impact. Center makes the data 
#centered over zero
summary(pen_pca) #PC1 explains abt 69% of the variance in the data;
#PC2 explains abt 19% of the variation in the data
# so together we have a good one
print(pen_pca) # shows standard deviations and loadings
class(pen_pca)
str(pen_pca) # Examine structure of the variable
#x is where the principal components are contained;where your eigen values are contained
summary(pen_pca)$importance
proportion_variance = summary(pen_pca)
proportion_variance = summary(pen_pca)$importance[2,1]
#calculate proportion of variance
pen_pca$sdev^2/sum(pen_pca$sdev^2)

pen_pca$rotation
#the values from here are like correlation values (the eigen values)
#most of what PC2 explains has to do with bill length and bill depth

plot(pen_pca)
#PC1 variance comes first

#scree plot
pca_scree = data.frame(pc=(1:4), var = proportion_variance)
ggplot(aes(x=pc, y=var), data=pca_scree) +
  geom_col()+
  geom_point()+
  geom_line()
