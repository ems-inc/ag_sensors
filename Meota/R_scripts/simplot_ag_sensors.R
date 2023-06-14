rm(list = ls())

library(tidyverse)  # Tidy packages
library(lubridate)  # Working with dates
library(mongolite)  # Accessing MongoDB
library(stringr)    # Working with strings
library(sf)         # Read kml files

# Read in the data ----
# Read the data in directly from Mongo
connection_string <- str_sub(read_file("C:/Users/smame/OneDrive - Environmental Material Science Inc/Data Analytics/Admin/mongo_db.txt"), 2, -2)
sensor_data <- mongo(db="EMS-Database", collection = "Agriculture_Data", url=connection_string)
df_in <- sensor_data$find()
data_input <- "C:/Users/smame/OneDrive/Desktop/EMS_Git/ag_sensors/R_scripts"

# At prior to installation, there was a bunch of programming, troubleshooting, etc.
# these are the data to look at here
# Only keep data post-installation ----
df <- df_in %>% 
  mutate(datetime = mdy_hms(local_datetime)) %>% 
  filter(str_detect(sensor, "Beta|Alpha"))

# Add GPS coordinates ----
# Exported from Google Earth
setwd(data_input)
kml_df <- st_read("../data/Meota.kml")
df_geo <- kml_df %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  select(-Description) %>% 
  rename(sensor = Name,longitude = long,
         latitude = lat) %>% 
  relocate(longitude, .after = latitude) 

df <- df %>% 
  inner_join(df_geo, by = join_by(sensor))

# projections
lon_lat <- as.matrix(df[,c("longitude", "latitude")])
east_north <- sf_project(pts = lon_lat, "+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
df <- df %>%
  mutate(easting = east_north[,1]) %>%
  mutate(northing = east_north[,2])

df %>% 
  ggplot(aes(x = longitude, y = latitude, color = sensor, label = sensor)) + geom_point(alpha = 0) +
  geom_text() + 
  geom_text(data = tibble(longitude = -108.55710, latitude = 53.073988, sensor = "gateway"), aes(x = longitude, y = latitude))

# Visual inspections ----
# field_sensors <- c(1,2,5)
date_val <- "2023-06-07 10:00"

df %>% 
  filter(datetime > date_val, temperature != 0, soilTemperature != 0) %>%
  ggplot(aes(x = datetime, y = temperature, color = sensor)) + geom_line() + geom_point() +
  # geom_line(aes(y = soilTemperature), alpha = 0.6) + geom_point(aes(y = soilTemperature), pch = 15) +
  xlab("Date")

df %>%
  filter(datetime > date_val, irtemperature != 0, sensor == "Alpha") %>%
  ggplot(aes(x = datetime, y = irtemperature, color = sensor)) + geom_line() + geom_point() + #ylim(0,6000) +
  xlab("Date") + ylim(15,30)

df %>% 
  filter(datetime > date_val, soilTemperature != 0) %>% 
  ggplot(aes(x = datetime, y = soilTemperature, color = sensor)) + geom_line() + geom_point() + #ylim(0,6000) +
  xlab("Date")

df %>% 
  filter(datetime > date_val, irtemperature != 0, temperature != 0, soilTemperature != 0) %>% 
  rowwise() %>% 
  mutate(avg_temp = mean(c(temperature, soilTemperature))) %>% 
  ungroup() %>% 
  ggplot(aes(x = datetime, y = avg_temp, color = sensor)) + geom_line() + geom_point() + #ylim(0,6000) +
  xlab("Date")

df %>% 
  filter(datetime > date_val, soilMoisture != 0) %>%
  ggplot(aes(x = datetime, y = soilMoisture, color = sensor)) + geom_line() + geom_point() + 
  # geom_line(aes(y = soilConductivity), alpha = 0.6) + #geom_point(aes(y = soilTemperature), pch = 15) +
  xlab("Date")

df %>% 
  filter(datetime > date_val, humidity != 0) %>%
  ggplot(aes(x = deviceTimeCounter, y = humidity, color = sensor)) + geom_line() + geom_point() + 
  # geom_line(aes(y = soilConductivity), alpha = 0.6) + #geom_point(aes(y = soilTemperature), pch = 15) +
  xlab("Date")

df %>% 
  filter(datetime > date_val, pressure != 0) %>%
  ggplot(aes(x = datetime, y = pressure, color = sensor)) + geom_line() + geom_point() + 
  # geom_line(aes(y = soilConductivity), alpha = 0.6) + #geom_point(aes(y = soilTemperature), pch = 15) +
  xlab("Date")

df %>% 
  filter(datetime > date_val, soilSalinity != 0) %>%
  ggplot(aes(x = datetime, y = soilSalinity, color = sensor)) + geom_line() + geom_point() + 
  # geom_line(aes(y = soilConductivity), alpha = 0.6) + #geom_point(aes(y = soilTemperature), pch = 15) +
  xlab("Date")

df %>% 
  filter(datetime > date_val, soilConductivity != 0) %>%
  ggplot(aes(x = datetime, y = soilConductivity, color = sensor)) + geom_line() + geom_point() + 
  # geom_line(aes(y = soilConductivity), alpha = 0.6) + #geom_point(aes(y = soilTemperature), pch = 15) +
  xlab("Date")

df %>%
  filter(datetime > date_val, soilDissolved != 0) %>%
  ggplot(aes(x = datetime, y = soilDissolved, color = sensor)) + geom_line() + geom_point() +
  # geom_line(aes(y = soilConductivity), alpha = 0.6) + #geom_point(aes(y = soilTemperature), pch = 15) +
  xlab("Date")

df %>%
  filter(datetime > date_val, soilDielectric != 0) %>%
  ggplot(aes(x = timestamp, y = soilDielectric, color = sensor)) + geom_line() + geom_point() +
  # geom_line(aes(y = soilConductivity), alpha = 0.6) + #geom_point(aes(y = soilTemperature), pch = 15) +
  xlab("Date")

df %>%
  filter(datetime > date_val) %>%
  ggplot(aes(x = datetime, y = rssi, color = sensor)) + geom_line() + geom_point() +
  # geom_line(aes(y = soilConductivity), alpha = 0.6) + #geom_point(aes(y = soilTemperature), pch = 15) +
  xlab("Date")

# Soil water potentials from relative humidity ----
# R: gas constant
# MW_h2o: molecular weight of water
# Pa: 1 Pa in cm h2o
# humidity needs to be /100 because it's a % that needs to be converted to a proportion.
MW_h2o <- 0.018015
Pa <- 0.0101972
IGC <- 8.3144598                # Ideal gas constant (m3 Pa / K mol)
K <- 273.15                     # 0deg Kelvin
df <- df %>%
  mutate(humidity_psi = -(IGC * (temperature+K) / MW_h2o) * log(humidity/100) * Pa)

# Step 4: Volumetric water content from water potential using van genuchten equations ----
# From USDA CalcPTFv3.0 manual ("Medium fine"; see table 2 on page 12)
theta_r <- 0.010
theta_s <- 0.439
alpha <- 0.0314
n <- 1.1804
m <- 0.1528
df <- df %>%
  mutate(theta_w = theta_r + ( (theta_s - theta_r) / (1 + (alpha*humidity_psi)^n)^m ))

df %>%
  filter(datetime > "2023-06-08") %>% 
  ggplot(aes(x = datetime, y = theta_w, color = sensor)) + geom_line() + geom_point()

df %>%
  filter(datetime > "2023-06-08", soilMoisture != 0) %>% 
  ggplot(aes(x = datetime, y = soilMoisture, color = sensor)) + geom_line() + geom_point() + 
  geom_line(aes(y = theta_w*100))

df %>%
  filter(datetime > "2023-06-08", soilMoisture != 0) %>% 
  ggplot(aes(x = theta_w*100, y = soilMoisture, color = sensor)) + geom_line() + geom_point()  +
  geom_vline(xintercept = )
  # ylim(15,40) +
  # xlim(25.5,28.5) + 
  geom_abline(slope = 1, intercept = 0)


