library(tidyverse)
library(readxl)
library(NbClust)
library(knitr)
library(tidymodels)
library(flexclust)
library(funtimes)
theme_set(theme_light())

# Read in the original excel datafile
vehicles_original <- read_excel("vehicles.xlsx") %>%
  janitor::clean_names() %>%
  #https://www.rdocumentation.org/packages/janitor/versions/1.2.0/topics/clean_names
  mutate(class = as_factor(class))
# Get a birds eye view of how the dataset looks like
summary(vehicles_original)

vehicles_original %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "van") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'van'")


vehicles_original %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'bus'")


vehicles_original %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: saab")


vehicles_original %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: opel")

vehicles_bus = vehicles_original %>%
  filter(class == "bus") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))
###check the outlier 
vehicles_bus %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "validate Bus-outlier cleaned")

vehicles_van = vehicles_original %>%
  filter(class == "van") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))
###validate the outlier van
vehicles_van %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "van") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "validate Van-outlier cleaned")



vehicles_opel = vehicles_original %>%
  filter(class == "opel") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))
###validate the outlier opel
vehicles_opel %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "validate Opel-outlier cleaned")




vehicles_saab = vehicles_original %>%
  filter(class == "saab") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))
###validate the outlier opel
vehicles_saab %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "validate Saab-outlier cleaned")


##so still the Bus has issues in the distribution ,so we we will adjust 
## quantile(.x, c(.05, .95)) to quantile(.x, c(.05, .85))

vehicles_bus = vehicles_original %>%
  filter(class == "bus") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .85)))))
###check the outlier 
vehicles_bus %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "validate Bus-outlier cleaned")

vehicles_van = vehicles_original %>%
  filter(class == "van") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .85)))))
###validate the outlier van
vehicles_van %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "van") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "validate Van-outlier cleaned")



vehicles_opel = vehicles_original %>%
  filter(class == "opel") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .85)))))
###validate the outlier opel
vehicles_opel %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "validate Opel-outlier cleaned")




vehicles_saab = vehicles_original %>%
  filter(class == "saab") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .85)))))
###validate the outlier opel
vehicles_saab %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "validate Saab-outlier cleaned")




combined = bind_rows(list(vehicles_bus,vehicles_opel,vehicles_saab,vehicles_van)) %>%
  arrange(samples)

combined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers class: 'bus'")

combined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers for class: saab")

combined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers for class: opel")

# Remove the sample name and the class name. Both of these will be remove so that only n
#numerical data is left for the algorithm.
vehicles_data_points = combined %>%
  select(-samples, -class)
# Now that we have the "vehicles_data_points" dataset, scaling is performed
vehicles_scaled = vehicles_data_points %>%
  mutate(across(everything(), scale))

set.seed(123)
# Perform the kmeans using the NbClust function
# Use Euclidean for distance
cluster_euclidean = NbClust(vehicles_scaled,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")
cluster_euclidean_2 = NbClust(vehicles_scaled,distance="euclidean", min.nc=2,max.nc=15,method="kmeans",index="all")


# Use manhattan for distance
cluster_manhattan = NbClust(vehicles_scaled,distance="manhattan", min.nc=2,max.nc=15,method="kmeans",index="all")

cluster_manhattan_2 = NbClust(vehicles_scaled,distance="manhattan", min.nc=2,max.nc=10,method="kmeans",index="all")

## maximum
clusterNo=NbClust(vehicles_scaled,distance="maximum", min.nc=2,max.nc=15,method="kmeans",index="all")

clusterNo_2=NbClust(vehicles_scaled,distance="maximum", min.nc=2,max.nc=10,method="kmeans",index="all")

##Elbow method
k = 2:10

WSS = sapply(k, function(k) {kmeans(vehicles_scaled, centers=k)$tot.withinss})
plot(k, WSS, type="l", xlab= "Number of k", ylab="Within sum of squares")



###Manual K-means
y = combined$class
kc <- kmeans(vehicles_scaled,2)
table(y,kc$cluster)

kc <- kmeans(vehicles_scaled,3)
table(y,kc$cluster)

kc <- kmeans(vehicles_scaled,4)
table(y,kc$cluster)
