rm(list = ls())

library(tidyverse)
library(lubridate)

# Read in the data ----
data_input <- "C:/Users/smame/OneDrive/Desktop/EMS_Git/ag_sensors/R_scripts"
setwd(data_input)
df <- read.csv("../data/Kernen_20221123_20230102.csv")

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
    n2o_alpha = ,
    n2o_beta = ,
    n2o_zero = ,
    n2o_span = ,
    n2o_a = ,
    n2o_n = ,
    n2o_k = ,
    co2_alpha = ,
    co2_beta = ,
    co2_zero = ,
    co2_span = ,
    co2_a = ,
    co2_n = ,
    co2_k = 
  ),
  N04 = data.frame(
    n2o_alpha = ,
    n2o_beta = ,
    n2o_zero = ,
    n2o_span = ,
    n2o_a = ,
    n2o_n = ,
    n2o_k = ,
    co2_alpha = ,
    co2_beta = ,
    co2_zero = ,
    co2_span = ,
    co2_a = ,
    co2_n = ,
    co2_k = 
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
    n2o_alpha = ,
    n2o_beta = ,
    n2o_zero = ,
    n2o_span = ,
    n2o_a = ,
    n2o_n = ,
    n2o_k = ,
    co2_alpha = ,
    co2_beta = ,
    co2_zero = ,
    co2_span = ,
    co2_a = ,
    co2_n = ,
    co2_k = 
  ),
  
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
  mutate(n2o_alpha = replace(n2o_alpha, sensor == 5, calib_coefs[[4]]$n2o_alpha)) %>%
  mutate(n2o_beta  = replace(n2o_beta,  sensor == 5, calib_coefs[[4]]$n2o_beta)) %>%
  mutate(n2o_zero = replace(n2o_zero, sensor == 5, calib_coefs[[4]]$n2o_zero)) %>%
  mutate(n2o_span  = replace(n2o_span,  sensor == 5, calib_coefs[[4]]$n2o_span)) %>%
  mutate(n2o_a = replace(n2o_a, sensor == 5, calib_coefs[[4]]$n2o_a)) %>%
  mutate(n2o_n = replace(n2o_n, sensor == 5, calib_coefs[[4]]$n2o_n)) %>%
  mutate(n2o_k = replace(n2o_k, sensor == 5, calib_coefs[[4]]$n2o_k)) %>%
  mutate(co2_alpha = replace(co2_alpha, sensor == 5, calib_coefs[[4]]$co2_alpha)) %>%
  mutate(co2_beta = replace(co2_beta, sensor == 5, calib_coefs[[4]]$co2_beta)) %>%
  mutate(co2_zero = replace(co2_zero, sensor == 5, calib_coefs[[4]]$co2_zero)) %>%
  mutate(co2_span = replace(co2_span, sensor == 5, calib_coefs[[4]]$co2_span)) %>%
  mutate(co2_a = replace(co2_a, sensor == 5, calib_coefs[[4]]$co2_a)) %>%
  mutate(co2_n = replace(co2_n, sensor == 5, calib_coefs[[4]]$co2_n)) %>%
  mutate(co2_k  = replace(co2_k,  sensor == 5, calib_coefs[[4]]$co2_k))

df_tmp <- 
  data.frame(irSignal1 = 337063.7625,
             irSignal3 = 201303,
             irSignal4 = 443532.8063,
             temperature = 24.52,
             irtemperature = 24.52,
             n2o_alpha = 0.0017,
             n2o_beta = 0,
             n2o_zero = 1.324765654,
             n2o_span = 2.4855,
             n2o_a = 0.002861783,
             n2o_n = 0.459775519,
             n2o_k = 0.919196428,
             co2_alpha = 0,
             co2_beta = 0,
             co2_zero = 0.606751336,
             co2_span = 0.607,
             co2_a = 0.181502162,
             co2_n = 0.570580948,
             co2_k = 0.342671328)

df_tmp %>% 
  mutate(Cn2o = (-log(1-(1-(irSignal4/(irSignal1*n2o_zero)))/n2o_span)/n2o_a)^(1/n2o_n),
         n2o_alpha_comp = (irSignal4/(irSignal1*n2o_zero))*(1 + n2o_alpha*(irtemperature - calib_coefs[[2]])),
         n2o_beta_comp = n2o_span + (n2o_beta*((irtemperature - calib_coefs[[2]])/(calib_coefs[[2]] + calib_coefs[[1]]))),
         Cco2 = (-log(1-(1-(irSignal3/(irSignal1*co2_zero)))/co2_span)/co2_a)^(1/co2_n),
         co2_alpha_comp = (irSignal3/(irSignal1*co2_zero))*(1 + co2_alpha*(irtemperature - calib_coefs[[2]])),
         co2_beta_comp = co2_span + (co2_beta*((irtemperature - calib_coefs[[2]])/(calib_coefs[[2]] + calib_coefs[[1]]))),
         n2o_norm_absorb = log(1/n2o_alpha_comp),
         co2_norm_absorb = log(1/co2_alpha_comp),
         n2o_norm_absorb_int = (n2o_norm_absorb-co2_norm_absorb*co2_k)/(1-n2o_k*co2_k),
         co2_norm_absorb_int = (co2_norm_absorb-n2o_norm_absorb*n2o_k)/(1-co2_k*n2o_k),
         conc_n2o_corr = ifelse(n2o_norm_absorb_int <= 0, 0, ((-log(1-((1-exp(-n2o_norm_absorb_int))/n2o_beta_comp)))/n2o_a)^(1/n2o_n)),
         conc_co2_corr = ifelse(co2_norm_absorb_int <= 0, 0, ((-log(1-((1-exp(-co2_norm_absorb_int))/co2_beta_comp)))/co2_a)^(1/co2_n)))

df <- df %>% 
  mutate(conc_n2o = (-log(1-(1-(irSignal4/(irSignal1*n2o_zero)))/n2o_span)/n2o_a)^(1/n2o_n),
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
df %>% 
  ggplot(aes(x = datetime, y = conc_n2o, color = sensor)) + geom_line() + geom_point()

df %>% 
  ggplot(aes(x = datetime, y = conc_n2o_corr, color = sensor)) + geom_line() + geom_point()

df %>% 
  filter(!is.na(conc_co2)) %>%
  ggplot(aes(x = datetime, y = conc_co2, color = sensor)) + geom_line() + geom_point()

df %>% 
  ggplot(aes(x = datetime, y = conc_co2_corr, color = sensor)) + geom_line() + geom_point()

df %>% 
  ggplot(aes(x = datetime, y = humidity, color = sensor)) + geom_line() + geom_point()

df %>%
  filter(irSignal1 != 0) %>% 
  ggplot(aes(x = datetime, y = irSignal1, color = sensor)) + geom_line() + geom_point()

df %>% 
  filter(irSignal2 != 0) %>% 
  ggplot(aes(x = datetime, y = irSignal2, color = sensor)) + geom_line() + geom_point()

df %>% 
  filter(irSignal3 != 0) %>% 
  ggplot(aes(x = datetime, y = irSignal3, color = sensor)) + geom_line() + geom_point()

df %>% 
  filter(irSignal4 != 0) %>% 
  ggplot(aes(x = datetime, y = irSignal4, color = sensor)) + geom_line() + geom_point()

df %>%
  filter(irtemperature < 50) %>% 
  ggplot(aes(x = datetime, y = irtemperature, color = sensor)) + geom_line() + geom_point()

df %>%
  # filter(irtemperature < 100) %>% 
  ggplot(aes(x = datetime, y = soilTemperature, color = sensor)) + geom_line() + geom_point()

df %>%
  ggplot(aes(x = datetime, y = pressure, color = sensor)) + geom_line() + geom_point()

df %>%
  ggplot(aes(x = datetime, y = rssi, color = sensor)) + geom_line() + geom_point()

df %>%
  ggplot(aes(x = datetime, y = snr, color = sensor)) + geom_line() + geom_point()

df %>%
  ggplot(aes(x = datetime, y = soilConductivity, color = sensor)) + geom_line() + geom_point()

df %>%
  ggplot(aes(x = datetime, y = soilDielectric, color = sensor)) + geom_line() + geom_point()

df %>%
  ggplot(aes(x = datetime, y = soilDissolved, color = sensor)) + geom_line() + geom_point()

df %>%
  ggplot(aes(x = datetime, y = soilMoisture, color = sensor)) + geom_line() + geom_point()

df %>%
  ggplot(aes(x = datetime, y = soilSalinity, color = sensor)) + geom_line() + geom_point()

df %>%
  ggplot(aes(x = datetime, y = soilTemperature, color = sensor)) + geom_line() + geom_point()

df %>%
  ggplot(aes(x = datetime, y = temperature, color = sensor)) + geom_line() + geom_point()

