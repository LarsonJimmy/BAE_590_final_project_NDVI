# BAE_590_final_project_NDVI
Data from earth explorer for NDVI for orchard site in washington state on 8 days throughout the spring and into early summer.

goal is to see how NDVI changes over the season and how those changes relate with weather data.

*load_and_view_data script loads in all the NDVI datasets, merges them together and views the raster data of the whole orchard site,
this data is viewed as only the positive NDVI values because the negative ones would not be associated with any vegetation.

*load_and_merge_weather_ndvi_data script, combines all weather data, calculates the average solar radiation and temperature for the day of and before each measurement date

*visualizing_and_analyzing_data creates k-means clusters for each date measured and then all dates combined. regression is also done to see how weather conditions both the day of and day before NDVI measurement relates to NDVI
