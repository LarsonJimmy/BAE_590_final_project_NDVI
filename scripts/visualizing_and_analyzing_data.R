# Data visualization and analysis
# author: Jimmy Larson
# created: 11/18/19
# last edited: 12/5/19

# packages ----
library(tidyverse)
## install.packages("cluster")
library(cluster)    # clustering algorithms
## install.packages("factoextra")
library(factoextra) # clustering algorithms & visualization
library(lubridate)
library(viridis)
library(raster)
library(plotly)
library(broom)
library(modelr)

# load data ----
ndvi <- read_csv("data/washington_site_ndvi.csv")
ndvi_weather <- read_csv("data/washington_site_ndvi_weather.csv")
apr_15_df <- read_csv("data/apr_15_ndvi.csv")
apr_20_df <- read_csv("data/apr_20_ndvi.csv")
apr_30_df <- read_csv("data/apr_30_ndvi.csv")
may_8_df <- read_csv("data/may_8_ndvi.csv")
may_10_df <- read_csv("data/may_10_ndvi.csv")
may_28_df <- read_csv("data/may_28_ndvi.csv")
may_30_df <- read_csv("data/may_30_ndvi.csv")
june_24_df <- read_csv("data/june_24_ndvi.csv")
# k-means cluster analysis for site spatial variability ----
## april 15 ----
### calculate clusters 
apr_15_df %>%
  unite("xy", x:y) -> apr_15_df
apr_15_df$xy_num <- as.numeric(as.factor(apr_15_df$xy))
apr_15_df %>%
  dplyr::select(xy_num, NDVI) %>%
  scale() %>%
  as.data.frame() -> apr_15_scaled
#### optimum clusters
set.seed(123)
fviz_nbclust(apr_15_scaled, kmeans, method = "silhouette")


k2_apr_15 <- kmeans(apr_15_scaled, centers = 2, nstart = 25)
fviz_cluster(k2_apr_15, geom = "point", alpha = 0.4, data = apr_15_scaled)

apr_15_scaled %>%
  as_tibble() %>%
  mutate(cluster = k2_apr_15$cluster,
         date = apr_15_df$date) -> apr_15_scaled
ggplot(apr_15_scaled, aes(xy_num, NDVI, color = factor(cluster)))+
  geom_point(alpha = 0.5)+
  scale_colour_brewer(palette = "Set2")+
  labs(x = "XY Coordinate",
       color = "Cluster")+
  theme_bw()
## april 20 ----
apr_20_df %>%
  unite("xy", x:y) -> apr_20_df
apr_20_df$xy_num <- as.numeric(as.factor(apr_20_df$xy))
apr_20_df %>%
  dplyr::select(xy_num, NDVI) %>%
  scale() %>%
  as.data.frame() -> apr_20_scaled
#### optimum clusters
set.seed(123)
fviz_nbclust(apr_20_scaled, kmeans, method = "silhouette")


k2_apr_20 <- kmeans(apr_20_scaled, centers = 2, nstart = 25)
fviz_cluster(k2_apr_20, geom = "point", alpha = 0.4, data = apr_20_scaled)

apr_20_scaled %>%
  as_tibble() %>%
  mutate(cluster = k2_apr_20$cluster,
         date = apr_20_df$date) -> apr_20_scaled
ggplot(apr_20_scaled, aes(xy_num, NDVI, color = factor(cluster)))+
  geom_point(alpha = 0.5)+
  scale_colour_brewer(palette = "Set2")+
  labs(x = "XY Coordinate",
       color = "Cluster")+
  theme_bw()
## april 30 ----
### calculate clusters 
apr_30_df %>%
  unite("xy", x:y) -> apr_30_df
apr_30_df$xy_num <- as.numeric(as.factor(apr_30_df$xy))
apr_30_df %>%
  dplyr::select(xy_num, NDVI) %>%
  scale() %>%
  as.data.frame() -> apr_30_scaled
#### optimum clusters
set.seed(123)
fviz_nbclust(apr_30_scaled, kmeans, method = "silhouette")


k2_apr_30 <- kmeans(apr_30_scaled, centers = 2, nstart = 25)
fviz_cluster(k2_apr_30, geom = "point", alpha = 0.4, data = apr_30_scaled)

apr_30_scaled %>%
  as_tibble() %>%
  mutate(cluster = k2_apr_30$cluster,
         date = apr_30_df$date) -> apr_30_scaled
ggplot(apr_30_scaled, aes(xy_num, NDVI, color = factor(cluster)))+
  geom_point(alpha = 0.5)+
  scale_colour_brewer(palette = "Set2")+
  labs(x = "XY Coordinate",
       color = "Cluster")+
  theme_bw()
## may 8 ----
may_8_df %>%
  unite("xy", x:y) -> may_8_df
may_8_df$xy_num <- as.numeric(as.factor(may_8_df$xy))
may_8_df %>%
  dplyr::select(xy_num, NDVI) %>%
  scale() %>%
  as.data.frame() -> may_8_scaled
#### optimum clusters
set.seed(123)
fviz_nbclust(may_8_scaled, kmeans, method = "silhouette")


k3_may_8 <- kmeans(may_8_scaled, centers = 3, nstart = 25)
fviz_cluster(k3_may_8, geom = "point", alpha = 0.4, data = may_8_scaled)

may_8_scaled %>%
  as_tibble() %>%
  mutate(cluster = k3_may_8$cluster,
         date = may_8_df$date) -> may_8_scaled
ggplot(may_8_scaled, aes(xy_num, NDVI, color = factor(cluster)))+
  geom_point(alpha = 0.5)+
  scale_colour_brewer(palette = "Set2")+
  labs(x = "XY Coordinate",
       color = "Cluster")+
  theme_bw()
## may 10 ----
may_10_df %>%
  unite("xy", x:y) -> may_10_df
may_10_df$xy_num <- as.numeric(as.factor(may_10_df$xy))
may_10_df %>%
  dplyr::select(xy_num, NDVI) %>%
  scale() %>%
  as.data.frame() -> may_10_scaled
#### optimum clusters
set.seed(123)
fviz_nbclust(may_10_scaled, kmeans, method = "silhouette")


k3_may_10 <- kmeans(may_10_scaled, centers = 3, nstart = 25)
fviz_cluster(k3_may_10, geom = "point", alpha = 0.4, data = may_10_scaled)

may_10_scaled %>%
  as_tibble() %>%
  mutate(cluster = k3_may_10$cluster,
         date = may_10_df$date) -> may_10_scaled
ggplot(may_10_scaled, aes(xy_num, NDVI, color = factor(cluster)))+
  geom_point(alpha = 0.5)+
  scale_colour_brewer(palette = "Set2")+
  labs(x = "XY Coordinate",
       color = "Cluster")+
  theme_bw()
## may 28 ----
may_28_df %>%
  unite("xy", x:y) -> may_28_df
may_28_df$xy_num <- as.numeric(as.factor(may_28_df$xy))
may_28_df %>%
  dplyr::select(xy_num, NDVI) %>%
  scale() %>%
  as.data.frame() -> may_28_scaled
#### optimum clusters
set.seed(123)
fviz_nbclust(may_28_scaled, kmeans, method = "silhouette")


k3_may_28 <- kmeans(may_28_scaled, centers = 3, nstart = 25)
fviz_cluster(k3_may_28, geom = "point", alpha = 0.4, data = may_28_scaled)

may_28_scaled %>%
  as_tibble() %>%
  mutate(cluster = k3_may_28$cluster,
         date = may_28_df$date) -> may_28_scaled
ggplot(may_28_scaled, aes(xy_num, NDVI, color = factor(cluster)))+
  geom_point(alpha = 0.5)+
  scale_colour_brewer(palette = "Set2")+
  labs(x = "XY Coordinate",
       color = "Cluster")+
  theme_bw()
## may 30 ----
may_30_df %>%
  unite("xy", x:y) -> may_30_df
may_30_df$xy_num <- as.numeric(as.factor(may_30_df$xy))
may_30_df %>%
  dplyr::select(xy_num, NDVI) %>%
  scale() %>%
  as.data.frame() -> may_30_scaled
#### optimum clusters
set.seed(123)
fviz_nbclust(may_30_scaled, kmeans, method = "silhouette")

k4_may_30 <- kmeans(may_30_scaled, centers = 4, nstart = 25)
fviz_cluster(k2_may_30, geom = "point", alpha = 0.4, data = may_30_scaled)

may_30_scaled %>%
  as_tibble() %>%
  mutate(cluster = k4_may_30$cluster,
         date = may_30_df$date) -> may_30_scaled
ggplot(may_30_scaled, aes(xy_num, NDVI, color = factor(cluster)))+
  geom_point(alpha = 0.5)+
  scale_colour_brewer(palette = "Set2")+
  labs(x = "XY Coordinate",
       color = "Cluster")+
  theme_bw()
## june 24 ----
june_24_df %>%
  unite("xy", x:y) -> june_24_df
june_24_df$xy_num <- as.numeric(as.factor(june_24_df$xy))
june_24_df %>%
  dplyr::select(xy_num, NDVI) %>%
  scale() %>%
  as.data.frame() -> june_24_scaled
#### optimum clusters
set.seed(123)
fviz_nbclust(june_24_scaled, kmeans, method = "silhouette")


k3_june_24 <- kmeans(june_24_scaled, centers = 3, nstart = 25)
fviz_cluster(k3_june_24, geom = "point", alpha = 0.4, data = june_24_scaled)

june_24_scaled %>%
  as_tibble() %>%
  mutate(cluster = k3_june_24$cluster,
         date = june_24_df$date) -> june_24_scaled
ggplot(june_24_scaled, aes(xy_num, NDVI, color = factor(cluster)))+
  geom_point(alpha = 0.5)+
  scale_colour_brewer(palette = "Set2")+
  labs(x = "XY Coordinate",
       color = "Cluster")+
  theme_bw()
## facet of all dates ----
ndvi_cluster_days <-rbind(apr_15_scaled, apr_20_scaled, apr_30_scaled, may_8_scaled, may_10_scaled, 
                          may_28_scaled,may_30_scaled, june_24_scaled)
date_labs <- c("April 15", "April 20", "April 30", "May 8", "May 10", "May 28", "May 30", "June 24")
names(date_labs) <- ymd(20190415, 20190420, 20190430,20190508, 20190510, 20190528, 20190530, 20190624)

ggplot(ndvi_cluster_days, aes(x = xy_num, y = NDVI, color = factor(cluster)))+
  geom_point(alpha = 0.5)+
  facet_wrap(~date, ncol = 4, labeller = labeller(date = date_labs))+
  scale_colour_brewer(palette = "Set2")+
  labs(x = "XY Coordinate",
       color = "Cluster")+
  theme_bw()
## all dates ----
### calculate clusters 
ndvi %>%
  unite("xy", x:y) -> ndvi
ndvi$date_num <- as.numeric(as.factor(ndvi$date))
ndvi$xy_num <- as.numeric(as.factor(ndvi$xy))
ndvi %>%
  dplyr::select(xy_num, NDVI, date_num) %>%
  scale()%>%
  as.data.frame() -> ndvi_scaled 

fviz_nbclust(ndvi_scaled, kmeans, method = "silhouette")
k2_ndvi <- kmeans(ndvi_scaled, centers = 2, nstart = 25)
fviz_cluster(k2_ndvi, geom = "point", alpha = 0.4, data = ndvi_scaled)

k6_ndvi <- kmeans(ndvi_scaled, centers = 6, nstart = 25)
fviz_cluster(k6_ndvi, geom = "point", alpha = 0.4, data = ndvi_scaled)

### plot
ndvi_scaled %>%
  as_tibble() %>%
  mutate(cluster = k2_ndvi$cluster,
         date = ndvi$date) %>%
  ggplot(aes(xy_num, NDVI, shape = factor(cluster), color = as.factor(date)))+
  geom_point(alpha = 0.5)+
  scale_colour_brewer(palette = "RdYlGn",
                      labels = c("April 15", "April 20", "April 30", "May 8", "May 10",
                                 "May 28", "May 30", "June 24"))+
  labs(x = "XY Coordinate",
       color = "Date",
       shape = "Cluster")+
  theme_bw()

# weather and NDVI analysis ----
## find x and y intercepts for single plot ----
p <- ggplot()+
  geom_raster(data = may_10_df, aes(x = x, y = y, fill = NDVI))+
  scale_fill_viridis_c(direction = -1)+
  theme_bw()
ggplotly(p)
## filter for single plot 
ndvi_weather %>%
  filter(between(y, 5231367, 5231505) & between(x, 736363, 736560.5)) -> orchard_1
### view single plot
ggplot(orchard_1, aes(x = temp, y = NDVI, color = factor(date)))+
  geom_point(alpha = 0.5)+
  facet_wrap(~day)+
  theme_bw()

## Regression model to weather and NDVI ----
### User-defined function for normalizing
normalize <- function(x) {
  n <- (x - min(x)) / (max(x) - min(x))
  return(n)
}
### separate data frames for day of / before
ndvi_before <- ndvi_weather %>%
  filter(day == "day_before")

ndvi_of <- ndvi_weather %>%
  filter(day == "day_of")

### day before model ----
mod_ndvi_weather_before <- lm(NDVI ~ irradiance*temp, data = ndvi_before)
tidy(mod_ndvi_weather_before)
glance(mod_ndvi_weather_before)

#### Normalize ndvi, temp, and irradiance
ndvi_before %>%
  mutate(ndvi_norm = normalize(NDVI),
         temp = normalize(temp),
         irradiance = normalize(irradiance)) -> ndvi_before_norm

#### Check that each column in the new dataframe ranges from 0-1
summary(ndvi_before_norm)

#### plot modeled vs. observed
augment(mod_ndvi_weather_before, data = ndvi_before_norm) %>%
  ggplot()+
  geom_point(aes(x = .fitted, y = ndvi_norm), alpha = 0.5)+
  geom_abline(intercept = 0, slope = 1, color = "red")+
  labs(x = "NDVI - modeled",
       y = "NDVI - observed")

#### plot residuals
augment(mod_ndvi_weather_before, data = ndvi_before_norm) %>%  
  ggplot()+
  geom_point(aes(x = ndvi_norm, y = .resid), alpha = 0.5)+
  geom_ref_line(h = 0)+
  labs(x = "NDVI",
       y = "Residuals")

### day of model ----
mod_ndvi_weather_of <- lm(NDVI ~ irradiance*temp, data = ndvi_of)
tidy(mod_ndvi_weather_of)
glance(mod_ndvi_weather_of)

#### Normalize ndvi, temp, and irradiance
ndvi_of %>%
  mutate(ndvi_norm = normalize(NDVI),
         temp = normalize(temp),
         irradiance = normalize(irradiance)) -> ndvi_of_norm

#### Check that each column in the new dataframe ranges from 0-1
summary(ndvi_of_norm)

#### plot modeled vs. observed
augment(mod_ndvi_weather_of, data = ndvi_of_norm) %>%
  ggplot()+
  geom_point(aes(x = .fitted, y = ndvi_norm), alpha = 0.5)+
  geom_abline(intercept = 0, slope = 1, color = "red")+
  labs(x = "NDVI - modeled",
       y = "NDVI - observed")

#### plot residuals
augment(mod_ndvi_weather_of, data = ndvi_of_norm) %>%  
  ggplot()+
  geom_point(aes(x = ndvi_norm, y = .resid), alpha = 0.5)+
  geom_ref_line(h = 0)+
  labs(x = "NDVI",
       y = "Residuals")
