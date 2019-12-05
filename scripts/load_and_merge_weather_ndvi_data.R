# Load and merge weather data with NDVI
# author: Jimmy Larson
# created: 11/20/19
# last edited: 12/5/19

# load packages ----
library(raster)
library(tidyverse)
library(lubridate)

# load data ----
weather_apr <- read_csv("data/washington_hourly_weather_data_apr_11.csv")
weather_may <- read_csv("data/washington_hourly_weather_data_may_11.csv")
weather_june <- read_csv("data/washington_hourly_weather_data_june_11.csv")
ndvi <- read_csv("data/washington_site_ndvi.csv")

# combine weather dates ----
weather <- rbind(weather_apr, weather_may, weather_june)
weather$date <- mdy(weather$Date) 
# tidy weather data ----
day_of <- ymd(20190415, 20190420, 20190430, 20190508, 20190510, 20190528, 20190530, 20190624)
day_before <- ymd(20190414, 20190419, 20190429, 20190507, 20190509, 20190527, 20190529, 20190623)
days <- c(day_of, day_before)
weather %>%
  na.omit() %>%
  rename(hour = HourPST, min_temp = `Min°F`, avg_temp = `Avg°F`, max_temp = `Max°F`,
         irradiance = `SolarRadW/m²`) %>%
  dplyr::select(date, hour, min_temp, avg_temp, max_temp, irradiance) %>%
  filter(date %in% days & irradiance > 0) -> weather 

## explore weather data
ggplot(weather, aes(x = factor(date), y = irradiance)) +
  geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45))

ggplot(weather, aes(x = factor(date), y = avg_temp))+
  geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45))
## get avg of 10 max solar radiation and temperature data
weather %>%
  group_by(date) %>%
  top_n(10, irradiance) %>%
  dplyr::select(date, irradiance) %>%
  summarise(irradiance = mean(irradiance))-> irradiance

weather %>%
  group_by(date) %>%
  top_n(10, max_temp) %>%
  dplyr::select(date, avg_temp) %>%
  summarise(temp = mean(avg_temp))-> temp

## join ndvi and temp
temp %>%
  filter(date %in% day_before) -> temp_before
temp %>%
  filter(date %in% day_of) -> temp_day
temp_day %>%
  dplyr::mutate(temp_before = temp_before$temp) %>%
  left_join(ndvi) %>%
  gather(temp, temp_before, key = "temp_day", value = "temp") -> ndvi_temp

## join ndvi and irradiance
irradiance %>%
  filter(date %in% day_before) -> irradiance_before
irradiance %>%
  filter(date %in% day_of) -> irradiance_day
irradiance_day %>%
  dplyr::mutate(irradiance_before = irradiance_before$irradiance) %>%
  left_join(ndvi) %>%
  gather(irradiance, irradiance_before, key = "irradiance_day", value = "irradiance") %>%
  dplyr::mutate(temp = ndvi_temp$temp)%>%
  dplyr::rename(day = irradiance_day)-> ndvi_weather

ndvi_weather$day <- fct_recode(ndvi_weather$day, day_of = "irradiance", day_before = "irradiance_before")

## write csv file for ndvi_weather
write_csv(ndvi_weather, path = "data/washington_site_ndvi_weather.csv")

## Data sample to turn in
ndvi_sample <- ndvi_weather[1:100,]
write_csv(ndvi_sample, path = "ndvi_full_dataset_sample.csv")
