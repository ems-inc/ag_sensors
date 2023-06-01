rm(list = ls())

library(tidyverse)
library(lubridate)
library(mongolite)
library(stringr)

# Read in the data ----
# Read the data in directly from Mongo
connection_string <- str_sub(read_file("C:/Users/smame/OneDrive - Environmental Material Science Inc/Data Analytics/Admin/mongo_db.txt"), 2, -2)
sensor_data <- mongo(db="EMS-Database", collection = "Agriculture_Data", url=connection_string)
df_in <- sensor_data$find()

# At prior to installation, there was a bunch of programming, troubleshooting, etc.
# these are the data to look at here
# Only keep data post-installation ----
df <- df_in %>% 
  mutate(datetime = mdy_hms(local_datetime)) %>% 
  filter(str_detect(sensor, "Beta|Alpha"))

# Add GPS coordinates ----
# From Eric's GPS
# df_geo <- data.frame(
#   sensor = factor(c(1, 2, 5)),
#   latitude = c(52.16690264, 52.16737549, 52.16421162),
#   longitude = c(-106.54187016, -106.54193654, -106.53676891)
# )
# df <- df %>%
#   mutate(latitude = NA,
#          longitude = NA) %>%
#   mutate(longitude = replace(longitude, sensor == 1, df_geo[1, 3])) %>%
#   mutate(latitude  = replace(latitude,  sensor == 1, df_geo[1, 2])) %>%
#   mutate(longitude = replace(longitude, sensor == 2, df_geo[2, 3])) %>%
#   mutate(latitude  = replace(latitude,  sensor == 2, df_geo[2, 2])) %>%
#   mutate(longitude = replace(longitude, sensor == 5, df_geo[3, 3])) %>%
#   mutate(latitude  = replace(latitude,  sensor == 5, df_geo[3, 2]))
# 
# df_geo %>% 
#   ggplot(aes(x = longitude, y = latitude, label = sensor)) + geom_point() + geom_text(vjust = -1, hjust = 1) 

# Visual inspections ----
# field_sensors <- c(1,2,5)

df %>% 
  filter(datetime > "2023-05-30", temperature != 0) %>% 
  ggplot(aes(x = datetime, y = temperature, color = sensor)) + geom_line() + geom_point() + 
  geom_line(aes(y = soilTemperature), alpha = 0.6) + #geom_point(aes(y = soilTemperature), pch = 15) +
  xlab("Date")

# df %>% 
#   filter(datetime > "2023-05-28", irtemperature != 0) %>% 
#   ggplot(aes(x = datetime, y = irtemperature, color = sensor)) + geom_line() + geom_point() + #ylim(0,6000) +
#   xlab("Date")

df %>% 
  filter(datetime > "2023-05-28", soilTemperature != 0) %>% 
  ggplot(aes(x = datetime, y = soilTemperature, color = sensor)) + geom_line() + geom_point() + #ylim(0,6000) +
  xlab("Date")



df %>% 
  filter(datetime > "2023-05-28", temperature != 0) %>%
  ggplot(aes(x = datetime, y = soilMoisture, color = sensor)) + geom_line() + geom_point() + 
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
  mutate(humidity_psi = -(IGC * (irtemperature+K) / MW_h2o) * log(humidity/100) * Pa)

# Step 4: Volumetric water content from water potential using van genuchten equations ----
theta_r <- 0.01
theta_s <- 0.538
alpha <- 0.0168
n <- 1.073
m <- 0.068
df <- df %>%
  mutate(theta_w = theta_r + ( (theta_s - theta_r) / (1 + (alpha*humidity_psi)^n)^m ))

df %>%
  filter(datetime > "2023-05-28", temperature != 0) %>% 
  ggplot(aes(x = datetime, y = theta_w, color = sensor)) + geom_line() + geom_point()


