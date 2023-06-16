library(tidyverse)
library(lubridate)
library(weathercan) # Get weather station details from Environment Canada
library(forecast)

# Third party weather data is sourced from: https://climate.weather.gc.ca/historical_data/search_historic_data_e.html. 
# For Meota we used the North Battleford Climate station
time_zone <- "Canada/Saskatchewan"
data_input <- "C:/Users/smame/OneDrive/Desktop/EMS_Git/ag_sensors/Meota/R_scripts"
interval <- "hour"
interval_text <- "hourly"
setwd(data_input)

# Get the correct station ID, make a df using that ID and the time period of interest, and download
stations_meta() # Check when last updated
# stations_dl() # re-download/update stations data if necessary
station_id <- suppressMessages(as.numeric(stations_search("NORTH BATTLEFORD", dist = 50, interval = interval)[4,3]))

# Now read in the data and tidy it up. Need to choose time period to cover
start_date <- ymd(paste("2023", "06", "07", sep = "-"))
end_date <- Sys.Date()
w_df <- weather_dl(station_ids = station_id, start = start_date, end = end_date)

weather_df <- w_df %>% 
  rename(station_temp = temp,
         datetime = time,
         dew_point = temp_dew,
         rel_humid = rel_hum,
         precip = precip_amt) %>% 
  select(datetime, station_temp, datetime, dew_point, rel_humid, precip, pressure) %>% 
  mutate(datetime = as_datetime(datetime, tz=time_zone)) %>% 
  filter(datetime > start_date & datetime <= end_date) %>% 
  mutate(station_temp = tsclean(station_temp),
         rel_humid = tsclean(rel_humid),
         pressure = tsclean(pressure))

min_date <- min(weather_df$datetime)
min_year <- year(min_date)
min_month <- sprintf("%02d", month(min_date))
min_day <- sprintf("%02d", day(min_date))
max_date <- max(weather_df$datetime)
max_year <- year(max_date)
max_month <- sprintf("%02d", month(max_date))
max_day <- sprintf("%02d", day(max_date))
save_path <- sprintf("../data/North_Battleford_EnvCan_%s%s%s_%s%s%s_%s.csv",
                     min_year, min_month, min_day,
                     max_year, max_month, max_day,
                     interval_text)
write.csv(weather_df, save_path)
