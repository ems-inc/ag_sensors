rm(list = ls())

library(tidyverse)
library(lubridate)
library(mongolite)
library(stringr)

# Read in the data ----
# Read the data in directly from Mongo
connection_string <- str_sub(read_file("C:/Users/smame/OneDrive - Environmental Material Science Inc/Data Analytics/Admin/mongo_db.txt"), 2, -2)
sensor_data <- mongo(db="EMS-Database", collection = "Agriculture_Data", url=connection_string)
df <- sensor_data$find()

# Only keep data post-installation ----
df <- df %>% 
  mutate(datetime = mdy_hms(local_datetime)) %>% 
  filter(datetime >= "2022-11-23")

# Add GPS coordinates ----
# From Eric's GPS
df_geo <- data.frame(
  sensor = factor(c(1, 2, 5)),
  latitude = c(52.16690264, 52.16737549, 52.16421162),
  longitude = c(-106.54187016, -106.54193654, -106.53676891)
)
df <- df %>%
  mutate(latitude = NA,
         longitude = NA) %>%
  mutate(longitude = replace(longitude, sensor == 1, df_geo[1, 3])) %>%
  mutate(latitude  = replace(latitude,  sensor == 1, df_geo[1, 2])) %>%
  mutate(longitude = replace(longitude, sensor == 2, df_geo[2, 3])) %>%
  mutate(latitude  = replace(latitude,  sensor == 2, df_geo[2, 2])) %>%
  mutate(longitude = replace(longitude, sensor == 5, df_geo[3, 3])) %>%
  mutate(latitude  = replace(latitude,  sensor == 5, df_geo[3, 2]))

df_geo %>% 
  ggplot(aes(x = longitude, y = latitude, label = sensor)) + geom_point() + geom_text(vjust = -1, hjust = 1) 


# Calibration and interference coefficients ----
calib_coefs <- list(
  kelvin = 273.15,
  calibration_temperature = 24.52,
  N01 = data.frame(
    n2o_alpha = 0.0017,
    n2o_beta = 0,
    n2o_zero = 1.324765654,
    n2o_span = 2.4855,
    n2o_a = 0.002861783,
    n2o_n = 0.459775519,
    n2o_k = 0.899275885,
    co2_alpha = 0,
    co2_beta = 0,
    co2_zero = 0.606751336,
    co2_span = 0.607,
    co2_a = 0.181502162,
    co2_n = 0.570580948,
    co2_k = 0.124973572
  ),
  N02 = data.frame(
    n2o_alpha = 0.0017,
    n2o_beta = 0,
    n2o_zero = 1.322817792,
    n2o_span = 2.129,
    n2o_a = 0.002716068,
    n2o_n = 0.522059159,
    n2o_k = 1.661045275,
    co2_alpha = 0,
    co2_beta = 0,
    co2_zero = 0.632269022,
    co2_span = 1.153,
    co2_a = 0.200825305,
    co2_n = 0.397820688,
    co2_k = 0.087704798
  ),
  N03 = data.frame(
    n2o_alpha = 0,
    n2o_beta = 0,
    n2o_zero = 1.114758832,
    n2o_span = 0.019818491,
    n2o_a = 0.018210748,
    n2o_n = 0.502215922,
    n2o_k = 1,
    co2_alpha = 0,
    co2_beta = 0,
    co2_zero = 0.739273882,
    co2_span = 1.271,
    co2_a = 0.070912706,
    co2_n = 0.537503111,
    co2_k = 0.098296
  ),
  N04 = data.frame(
    n2o_alpha = 0,
    n2o_beta = 0,
    n2o_zero = 1.180376168,
    n2o_span = 0.01482581,
    n2o_a = 0.014297708,
    n2o_n = 0.414519234,
    n2o_k = 1,
    co2_alpha = 0,
    co2_beta = 0,
    co2_zero = 0.659375302,
    co2_span = 1.203,
    co2_a = 0.043928437,
    co2_n = 0.672816898,
    co2_k = 0.098
  ),
  # N05 failed calibration
  N05 = data.frame(
    n2o_alpha = NA,
    n2o_beta = NA,
    n2o_zero = NA,
    n2o_span = NA,
    n2o_a = NA,
    n2o_n = NA,
    co2_alpha = NA,
    co2_beta = NA,
    co2_zero = NA,
    co2_span = NA,
    co2_a = NA,
    co2_n = NA
  ),
  N06 = data.frame(
    n2o_alpha = 0,
    n2o_beta = 0,
    n2o_zero = 1.153387071,
    n2o_span = 0.033732728,
    n2o_a = 0.025355172,
    n2o_n = 0.415916648,
    n2o_k = 0,
    co2_alpha = 0,
    co2_beta = 0,
    co2_zero = 0.739273882,
    co2_span = 1.271,
    co2_a = 0.21819119,
    co2_n = 0.260112481,
    co2_k = 0.06
  )
)

# Add constants to df ----
df <- df %>%
  mutate(n2o_alpha = NA,
         n2o_beta = NA,
         n2o_zero = NA,
         n2o_span = NA,
         n2o_a = NA,
         n2o_n = NA,
         n2o_k = NA,
         co2_alpha = NA,
         co2_beta = NA,
         co2_zero = NA,
         co2_span = NA,
         co2_a = NA,
         co2_n = NA,
         co2_k = NA) %>%
  mutate(n2o_alpha = replace(n2o_alpha, sensor == 1, calib_coefs[[3]]$n2o_alpha)) %>%
  mutate(n2o_beta  = replace(n2o_beta,  sensor == 1, calib_coefs[[3]]$n2o_beta)) %>%
  mutate(n2o_zero = replace(n2o_zero, sensor == 1, calib_coefs[[3]]$n2o_zero)) %>%
  mutate(n2o_span  = replace(n2o_span,  sensor == 1, calib_coefs[[3]]$n2o_span)) %>%
  mutate(n2o_a = replace(n2o_a, sensor == 1, calib_coefs[[3]]$n2o_a)) %>%
  mutate(n2o_n = replace(n2o_n, sensor == 1, calib_coefs[[3]]$n2o_n)) %>%
  mutate(n2o_k = replace(n2o_k, sensor == 1, calib_coefs[[3]]$n2o_k)) %>%
  mutate(co2_alpha = replace(co2_alpha, sensor == 1, calib_coefs[[3]]$co2_alpha)) %>%
  mutate(co2_beta = replace(co2_beta, sensor == 1, calib_coefs[[3]]$co2_beta)) %>%
  mutate(co2_zero = replace(co2_zero, sensor == 1, calib_coefs[[3]]$co2_zero)) %>%
  mutate(co2_span = replace(co2_span, sensor == 1, calib_coefs[[3]]$co2_span)) %>%
  mutate(co2_a = replace(co2_a, sensor == 1, calib_coefs[[3]]$co2_a)) %>%
  mutate(co2_n = replace(co2_n, sensor == 1, calib_coefs[[3]]$co2_n)) %>%
  mutate(co2_k  = replace(co2_k,  sensor == 1, calib_coefs[[3]]$co2_k)) %>% 
  
  mutate(n2o_alpha = replace(n2o_alpha, sensor == 2, calib_coefs[[4]]$n2o_alpha)) %>%
  mutate(n2o_beta  = replace(n2o_beta,  sensor == 2, calib_coefs[[4]]$n2o_beta)) %>%
  mutate(n2o_zero = replace(n2o_zero, sensor == 2, calib_coefs[[4]]$n2o_zero)) %>%
  mutate(n2o_span  = replace(n2o_span,  sensor == 2, calib_coefs[[4]]$n2o_span)) %>%
  mutate(n2o_a = replace(n2o_a, sensor == 2, calib_coefs[[4]]$n2o_a)) %>%
  mutate(n2o_n = replace(n2o_n, sensor == 2, calib_coefs[[4]]$n2o_n)) %>%
  mutate(n2o_k = replace(n2o_k, sensor == 2, calib_coefs[[4]]$n2o_k)) %>%
  mutate(co2_alpha = replace(co2_alpha, sensor == 2, calib_coefs[[4]]$co2_alpha)) %>%
  mutate(co2_beta = replace(co2_beta, sensor == 2, calib_coefs[[4]]$co2_beta)) %>%
  mutate(co2_zero = replace(co2_zero, sensor == 2, calib_coefs[[4]]$co2_zero)) %>%
  mutate(co2_span = replace(co2_span, sensor == 2, calib_coefs[[4]]$co2_span)) %>%
  mutate(co2_a = replace(co2_a, sensor == 2, calib_coefs[[4]]$co2_a)) %>%
  mutate(co2_n = replace(co2_n, sensor == 2, calib_coefs[[4]]$co2_n)) %>%
  mutate(co2_k  = replace(co2_k,  sensor == 2, calib_coefs[[4]]$co2_k)) %>% 
  
  mutate(n2o_alpha = replace(n2o_alpha, sensor == 3, calib_coefs[[5]]$n2o_alpha)) %>%
  mutate(n2o_beta  = replace(n2o_beta,  sensor == 3, calib_coefs[[5]]$n2o_beta)) %>%
  mutate(n2o_zero = replace(n2o_zero, sensor == 3, calib_coefs[[5]]$n2o_zero)) %>%
  mutate(n2o_span  = replace(n2o_span,  sensor == 3, calib_coefs[[5]]$n2o_span)) %>%
  mutate(n2o_a = replace(n2o_a, sensor == 3, calib_coefs[[5]]$n2o_a)) %>%
  mutate(n2o_n = replace(n2o_n, sensor == 3, calib_coefs[[5]]$n2o_n)) %>%
  mutate(n2o_k = replace(n2o_k, sensor == 3, calib_coefs[[5]]$n2o_k)) %>%
  mutate(co2_alpha = replace(co2_alpha, sensor == 3, calib_coefs[[5]]$co2_alpha)) %>%
  mutate(co2_beta = replace(co2_beta, sensor == 3, calib_coefs[[5]]$co2_beta)) %>%
  mutate(co2_zero = replace(co2_zero, sensor == 3, calib_coefs[[5]]$co2_zero)) %>%
  mutate(co2_span = replace(co2_span, sensor == 3, calib_coefs[[5]]$co2_span)) %>%
  mutate(co2_a = replace(co2_a, sensor == 3, calib_coefs[[5]]$co2_a)) %>%
  mutate(co2_n = replace(co2_n, sensor == 3, calib_coefs[[5]]$co2_n)) %>%
  mutate(co2_k  = replace(co2_k,  sensor == 3, calib_coefs[[5]]$co2_k)) %>% 
  
  mutate(n2o_alpha = replace(n2o_alpha, sensor == 4, calib_coefs[[6]]$n2o_alpha)) %>%
  mutate(n2o_beta  = replace(n2o_beta,  sensor == 4, calib_coefs[[6]]$n2o_beta)) %>%
  mutate(n2o_zero = replace(n2o_zero, sensor == 4, calib_coefs[[6]]$n2o_zero)) %>%
  mutate(n2o_span  = replace(n2o_span,  sensor == 4, calib_coefs[[6]]$n2o_span)) %>%
  mutate(n2o_a = replace(n2o_a, sensor == 4, calib_coefs[[6]]$n2o_a)) %>%
  mutate(n2o_n = replace(n2o_n, sensor == 4, calib_coefs[[6]]$n2o_n)) %>%
  mutate(n2o_k = replace(n2o_k, sensor == 4, calib_coefs[[6]]$n2o_k)) %>%
  mutate(co2_alpha = replace(co2_alpha, sensor == 4, calib_coefs[[6]]$co2_alpha)) %>%
  mutate(co2_beta = replace(co2_beta, sensor == 4, calib_coefs[[6]]$co2_beta)) %>%
  mutate(co2_zero = replace(co2_zero, sensor == 4, calib_coefs[[6]]$co2_zero)) %>%
  mutate(co2_span = replace(co2_span, sensor == 4, calib_coefs[[6]]$co2_span)) %>%
  mutate(co2_a = replace(co2_a, sensor == 4, calib_coefs[[6]]$co2_a)) %>%
  mutate(co2_n = replace(co2_n, sensor == 4, calib_coefs[[6]]$co2_n)) %>%
  mutate(co2_k  = replace(co2_k,  sensor == 4, calib_coefs[[6]]$co2_k)) %>% 
  # Calibration failed for N05, so using N04 values
  mutate(n2o_alpha = replace(n2o_alpha, sensor == 5, calib_coefs[[6]]$n2o_alpha)) %>%
  mutate(n2o_beta  = replace(n2o_beta,  sensor == 5, calib_coefs[[6]]$n2o_beta)) %>%
  mutate(n2o_zero = replace(n2o_zero, sensor == 5, calib_coefs[[6]]$n2o_zero)) %>%
  mutate(n2o_span  = replace(n2o_span,  sensor == 5, calib_coefs[[6]]$n2o_span)) %>%
  mutate(n2o_a = replace(n2o_a, sensor == 5, calib_coefs[[6]]$n2o_a)) %>%
  mutate(n2o_n = replace(n2o_n, sensor == 5, calib_coefs[[6]]$n2o_n)) %>%
  mutate(n2o_k = replace(n2o_k, sensor == 5, calib_coefs[[6]]$n2o_k)) %>%
  mutate(co2_alpha = replace(co2_alpha, sensor == 5, calib_coefs[[6]]$co2_alpha)) %>%
  mutate(co2_beta = replace(co2_beta, sensor == 5, calib_coefs[[6]]$co2_beta)) %>%
  mutate(co2_zero = replace(co2_zero, sensor == 5, calib_coefs[[6]]$co2_zero)) %>%
  mutate(co2_span = replace(co2_span, sensor == 5, calib_coefs[[6]]$co2_span)) %>%
  mutate(co2_a = replace(co2_a, sensor == 5, calib_coefs[[6]]$co2_a)) %>%
  mutate(co2_n = replace(co2_n, sensor == 5, calib_coefs[[6]]$co2_n)) %>%
  mutate(co2_k  = replace(co2_k,  sensor == 5, calib_coefs[[6]]$co2_k)) %>% 
  
  mutate(n2o_alpha = replace(n2o_alpha, sensor == 6, calib_coefs[[8]]$n2o_alpha)) %>%
  mutate(n2o_beta  = replace(n2o_beta,  sensor == 6, calib_coefs[[8]]$n2o_beta)) %>%
  mutate(n2o_zero = replace(n2o_zero, sensor == 6, calib_coefs[[8]]$n2o_zero)) %>%
  mutate(n2o_span  = replace(n2o_span,  sensor == 6, calib_coefs[[8]]$n2o_span)) %>%
  mutate(n2o_a = replace(n2o_a, sensor == 6, calib_coefs[[8]]$n2o_a)) %>%
  mutate(n2o_n = replace(n2o_n, sensor == 6, calib_coefs[[8]]$n2o_n)) %>%
  mutate(n2o_k = replace(n2o_k, sensor == 6, calib_coefs[[8]]$n2o_k)) %>%
  mutate(co2_alpha = replace(co2_alpha, sensor == 6, calib_coefs[[8]]$co2_alpha)) %>%
  mutate(co2_beta = replace(co2_beta, sensor == 6, calib_coefs[[8]]$co2_beta)) %>%
  mutate(co2_zero = replace(co2_zero, sensor == 6, calib_coefs[[8]]$co2_zero)) %>%
  mutate(co2_span = replace(co2_span, sensor == 6, calib_coefs[[8]]$co2_span)) %>%
  mutate(co2_a = replace(co2_a, sensor == 6, calib_coefs[[8]]$co2_a)) %>%
  mutate(co2_n = replace(co2_n, sensor == 6, calib_coefs[[8]]$co2_n)) %>%
  mutate(co2_k  = replace(co2_k,  sensor == 6, calib_coefs[[8]]$co2_k))

# df_tmp <- 
#   data.frame(irSignal1 = 337063.7625,
#              irSignal3 = 201303,
#              irSignal4 = 443532.8063,
#              temperature = 24.52,
#              irtemperature = 24.52,
#              n2o_alpha = 0.0017,
#              n2o_beta = 0,
#              n2o_zero = 1.324765654,
#              n2o_span = 2.4855,
#              n2o_a = 0.002861783,
#              n2o_n = 0.459775519,
#              n2o_k = 0.919196428,
#              co2_alpha = 0,
#              co2_beta = 0,
#              co2_zero = 0.606751336,
#              co2_span = 0.607,
#              co2_a = 0.181502162,
#              co2_n = 0.570580948,
#              co2_k = 0.342671328)
# 
# df_tmp %>% 
#   mutate(Cn2o = (-log(1-(1-(irSignal4/(irSignal1*n2o_zero)))/n2o_span)/n2o_a)^(1/n2o_n),
#          n2o_alpha_comp = (irSignal4/(irSignal1*n2o_zero))*(1 + n2o_alpha*(irtemperature - calib_coefs[[2]])),
#          n2o_beta_comp = n2o_span + (n2o_beta*((irtemperature - calib_coefs[[2]])/(calib_coefs[[2]] + calib_coefs[[1]]))),
#          Cco2 = (-log(1-(1-(irSignal3/(irSignal1*co2_zero)))/co2_span)/co2_a)^(1/co2_n),
#          co2_alpha_comp = (irSignal3/(irSignal1*co2_zero))*(1 + co2_alpha*(irtemperature - calib_coefs[[2]])),
#          co2_beta_comp = co2_span + (co2_beta*((irtemperature - calib_coefs[[2]])/(calib_coefs[[2]] + calib_coefs[[1]]))),
#          n2o_norm_absorb = log(1/n2o_alpha_comp),
#          co2_norm_absorb = log(1/co2_alpha_comp),
#          n2o_norm_absorb_int = (n2o_norm_absorb-co2_norm_absorb*co2_k)/(1-n2o_k*co2_k),
#          co2_norm_absorb_int = (co2_norm_absorb-n2o_norm_absorb*n2o_k)/(1-co2_k*n2o_k),
#          conc_n2o_corr = ifelse(n2o_norm_absorb_int <= 0, 0, ((-log(1-((1-exp(-n2o_norm_absorb_int))/n2o_beta_comp)))/n2o_a)^(1/n2o_n)),
#          conc_co2_corr = ifelse(co2_norm_absorb_int <= 0, 0, ((-log(1-((1-exp(-co2_norm_absorb_int))/co2_beta_comp)))/co2_a)^(1/co2_n)))

df <- df %>% 
  mutate(sensor = factor(sensor),
         conc_n2o = (-log(1-(1-(irSignal4/(irSignal1*n2o_zero)))/n2o_span)/n2o_a)^(1/n2o_n),
         n2o_alpha_comp = (irSignal4/(irSignal1*n2o_zero))*(1 + n2o_alpha*(irtemperature - calib_coefs[[2]])),
         n2o_beta_comp = n2o_span + (n2o_beta*((irtemperature - calib_coefs[[2]])/(calib_coefs[[2]] + calib_coefs[[1]]))),
         conc_co2 = (-log(1-(1-(irSignal3/(irSignal1*co2_zero)))/co2_span)/co2_a)^(1/co2_n),
         co2_alpha_comp = (irSignal3/(irSignal1*co2_zero))*(1 + co2_alpha*(irtemperature - calib_coefs[[2]])),
         co2_beta_comp = co2_span + (co2_beta*((irtemperature - calib_coefs[[2]])/(calib_coefs[[2]] + calib_coefs[[1]]))),
         n2o_norm_absorb = log(1/n2o_alpha_comp),
         co2_norm_absorb = log(1/co2_alpha_comp),
         n2o_norm_absorb_int = (n2o_norm_absorb-co2_norm_absorb*co2_k)/(1-n2o_k*co2_k),
         co2_norm_absorb_int = (co2_norm_absorb-n2o_norm_absorb*n2o_k)/(1-co2_k*n2o_k),
         conc_n2o_corr = ifelse(n2o_norm_absorb_int <= 0, 0, ((-log(1-((1-exp(-n2o_norm_absorb_int))/n2o_beta_comp)))/n2o_a)^(1/n2o_n)),
         conc_co2_corr = ifelse(co2_norm_absorb_int <= 0, 0, ((-log(1-((1-exp(-co2_norm_absorb_int))/co2_beta_comp)))/co2_a)^(1/co2_n)))

# Visual inspections ----
field_sensors <- c(1,2,5)

df %>% 
  ggplot(aes(x = datetime, y = conc_n2o, color = sensor)) + geom_line() + geom_point() + ylim(0,6000) +
  xlab("Date")

df %>% 
  # filter(sensor %in% c(5)) %>% 
  ggplot(aes(x = datetime, y = conc_n2o_corr, color = sensor)) + geom_line() + geom_point() + ylim(0,9000) +
  theme(legend.position = "none") + xlab("Date")

df %>% 
  filter(!is.na(conc_co2)) %>%
  ggplot(aes(x = datetime, y = conc_co2, color = sensor)) + geom_line() + geom_point()

df %>% 
  ggplot(aes(x = datetime, y = conc_co2_corr, color = sensor)) + geom_line() + geom_point() + ylim(0,0.5)

df %>% 
  # filter(sensor %in% field_sensors) %>% 
  ggplot(aes(x = datetime, y = humidity, color = sensor)) + geom_line() + geom_point()

# df %>%
#   filter(irSignal1 != 0) %>% 
#   ggplot(aes(x = datetime, y = irSignal1, color = sensor)) + geom_line() + geom_point() + ylim(0,500000)
# 
# df %>% 
#   filter(irSignal2 != 0) %>% 
#   ggplot(aes(x = datetime, y = irSignal2, color = sensor)) + geom_line() + geom_point() + ylim(0,500000)
# 
# df %>% 
#   filter(irSignal3 != 0) %>% 
#   ggplot(aes(x = datetime, y = irSignal3, color = sensor)) + geom_line() + geom_point() + ylim(0,500000)
# 
# df %>% 
#   filter(irSignal4 != 0) %>% 
#   ggplot(aes(x = datetime, y = irSignal4, color = sensor)) + geom_line() + geom_point() + ylim(0,500000)

df %>%
  filter(irtemperature < 50, sensor %in% field_sensors) %>% 
  ggplot(aes(x = datetime, y = irtemperature, color = sensor)) + geom_line() + geom_point()

df %>%
  filter(soilTemperature < 10, sensor %in% field_sensors) %>%
  ggplot(aes(x = datetime, y = soilTemperature, color = sensor)) + geom_line() + geom_point()

# These just appear to be hard-coded ~room temps
df %>%
  filter(temperature != 0, sensor %in% field_sensors) %>%
  ggplot(aes(x = datetime, y = temperature, color = sensor)) + geom_line() + geom_point()

df %>%
  filter(pressure != 0) %>% 
  ggplot(aes(x = datetime, y = pressure, color = sensor)) + geom_line() + geom_point()

df %>%
  ggplot(aes(x = datetime, y = rssi, color = sensor)) + geom_line() + geom_point()

df %>%
  ggplot(aes(x = datetime, y = snr, color = sensor)) + geom_line() + geom_point()

df %>%
  filter(soilConductivity != 0) %>% 
  ggplot(aes(x = datetime, y = soilConductivity, color = sensor)) + geom_line() + geom_point()

df %>%
  filter(soilDielectric != 0) %>% 
  ggplot(aes(x = datetime, y = soilDielectric, color = sensor)) + geom_line() + geom_point()

df %>%
  filter(soilDissolved != 0) %>% 
  ggplot(aes(x = datetime, y = soilDissolved, color = sensor)) + geom_line() + geom_point()

df %>%
  filter(soilMoisture != 0) %>% 
  ggplot(aes(x = datetime, y = soilMoisture, color = sensor)) + geom_line() + geom_point()

df %>%
  filter(soilSalinity != 0) %>% 
  ggplot(aes(x = datetime, y = soilSalinity, color = sensor)) + geom_line() + geom_point()


names(df)

df %>% 
  # filter(sensor %in% field_sensors) %>% 
  ggplot(aes(x = datetime, y = soilMoisture/10, color = sensor)) + geom_point()

df %>% 
  filter(sensor == 1) %>% 
  ggplot(aes(x = soilMoisture/10, y = theta_w, color = sensor)) + geom_point()



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
  filter(sensor %in% field_sensors) %>%
  ggplot(aes(x = datetime, y = theta_w, color = sensor)) + geom_line() + geom_point()


