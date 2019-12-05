# Load and view data
# author: Jimmy Larson
# created: 11/12/19
# last edited: 12/5/19

# packages ----
library(sf)
# install.packages("raster")
library(raster)
library(tidyverse)
library(lubridate)
library(viridis)

# load data ----
apr_15 <- raster("data/WA_0415_NDVI_clip.tif")
apr_15_df <- as.data.frame(apr_15, xy = T, na.rm = T)
apr_20 <- raster("data/WA_0420_NDVI_clip.tif")
apr_20_df <- as.data.frame(apr_20, xy = T, na.rm = T)
apr_30 <- raster("data/WA_0430_NDVI_clip.tif")
apr_30_df <- as.data.frame(apr_30, xy = T, na.rm = T)
may_8 <- raster("data/WA_0508_NDVI_clip1.tif")
may_8_df <- as.data.frame(may_8, xy = T, na.rm = T)
may_10 <- raster("data/WA_0510_NDVI_clip.tif")
may_10_df <- as.data.frame(may_10, xy = T, na.rm = T)
may_28 <- raster("data/WA_0528_NDVI_clip.tif")
may_28_df <- as.data.frame(may_28, xy = T, na.rm = T)
may_30 <- raster("data/WA_0530_NDVI_clip.tif")
may_30_df <- as.data.frame(may_30, xy = T, na.rm = T)
june_24 <- raster("data/WA_0624_NDVI_clip.tif")
june_24_df <- as.data.frame(june_24, xy = T, na.rm = T)

# add date to and merge data frames ----
apr_15_df %>%
  rename(NDVI = WA_0415_NDVI_clip) %>%
  mutate(date = dmy("15 April 2019"))%>%
  filter(NDVI > 0) -> apr_15_df
apr_20_df %>%
  rename(NDVI = WA_0420_NDVI_clip) %>%
  mutate(date = dmy("20 April 2019"))%>%
  filter(NDVI > 0) -> apr_20_df
apr_30_df %>%
  rename(NDVI = WA_0430_NDVI_clip) %>%
  mutate(date = dmy("30 April 2019"))%>%
  filter(NDVI > 0) -> apr_30_df
may_8_df %>%
  rename(NDVI = WA_0508_NDVI_clip1) %>%
  mutate(date = dmy("8 May 2019"))%>%
  filter(NDVI > 0) -> may_8_df
may_10_df %>%
  rename(NDVI = WA_0510_NDVI_clip) %>%
  mutate(date = dmy("10 May 2019"))%>%
  filter(NDVI > 0) -> may_10_df
may_28_df %>%
  rename(NDVI = WA_0528_NDVI_clip) %>%
  mutate(date = dmy("28 May 2019"))%>%
  filter(NDVI > 0) -> may_28_df
may_30_df %>%
  rename(NDVI = WA_0530_NDVI_clip) %>%
  mutate(date = dmy("30 May 2019"))%>%
  filter(NDVI > 0) -> may_30_df
june_24_df %>%
  rename(NDVI = WA_0624_NDVI_clip) %>%
  mutate(date = dmy("24June 2019"))%>%
  filter(NDVI > 0) -> june_24_df

ndvi <- rbind(apr_15_df, apr_20_df, apr_30_df, may_8_df, may_10_df, may_28_df, may_30_df, june_24_df)
# view orchard ----
## april 15
ggplot()+
  geom_raster(data = apr_15_df, aes(x = x, y = y, fill = NDVI))+
  scale_fill_viridis_c(direction = -1)+
  theme_bw()

## may 10
ggplot()+
  geom_raster(data = may_10_df, aes(x = x, y = y, fill = NDVI))+
  scale_fill_viridis_c(direction = -1)+
  theme_bw()

## june 24
ggplot()+
  geom_raster(data = june_24_df, aes(x = x, y = y, fill = NDVI))+
  scale_fill_viridis_c(direction = -1)+
  theme_bw

## all dates
ggplot()+
  geom_raster(data = ndvi, aes(x = x, y = y, fill = NDVI))+
  facet_wrap(~date, ncol = 4)+
  scale_fill_viridis_c(direction = -1)+
  theme_bw()

## plot all dates 
date_labs <- c("April 15", "April 20", "April 30", "May 8", "May 10", "May 28", "May 30", "June 24")
names(date_labs) <- ymd(20190415, 20190420, 20190430,20190508, 20190510, 20190528, 20190530, 20190624)
ggplot()+
  geom_raster(data = ndvi, aes(x = x, y = y, fill = NDVI))+
  facet_wrap(~date, ncol = 4, labeller = labeller(date = date_labs))+
  scale_fill_viridis_c(direction = -1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))
## save the plot
ggsave("orchard_ndvi_raster_plot_all_dates.pdf", path = "plots")
# boxplot of NDVI values by day ----
ggplot(ndvi, aes(x = as.factor(date), y = NDVI))+
  geom_boxplot(fill = "steelblue", alpha = .5)+
  geom_jitter(color = "black", size = 0.4, alpha = 0.5)+
  scale_x_discrete(labels = c("2019-04-15" = "April 15", "2019-04-20" = "April 20", "2019-04-30" = "April 30", "2019-05-08" = "May 8",
                              "2019-05-10" = "May 10", "2019-05-28" = "May 28", "2019-05-30" = "May 30", "2019-06-24" = "June 24"))+
  guides(fill = FALSE)+
  labs(x = "Date")+
  theme_bw()
## save the plot
ggsave("orchard_ndvi_boxplot_all_dates.pdf", path = "plots")
# write csv for df NDVI data ----
write_csv(ndvi_, path = "data/washington_site_ndvi.csv")
write_csv(apr_15_df, path = "data/apr_15_ndvi.csv")
write_csv(apr_20_df, path = "data/apr_20_ndvi.csv")
write_csv(apr_30_df, path = "data/apr_30_ndvi.csv")
write_csv(may_8_df, path = "data/may_8_ndvi.csv")
write_csv(may_10_df, path = "data/may_10_ndvi.csv")
write_csv(may_28_df, path = "data/may_28_ndvi.csv")
write_csv(may_30_df, path = "data/may_30_ndvi.csv")
write_csv(june_24_df, path = "data/june_24_ndvi.csv")
