rm(list = ls())

library(tidyverse)
library(lubridate)

df <- read.csv("C:/Users/smame/OneDrive/Desktop/EMS_Git/ag_sensors/data/salinity_calibration_20230602.csv")

df_spike <- tibble(
  datetime = c(
    "2023-06-02 13:10:00",
    "2023-06-02 13:13:01",
    "2023-06-02 13:15:52",
    "2023-06-02 13:17:39",
    "2023-06-02 13:19:49",
    "2023-06-02 13:23:12",
    "2023-06-02 13:26:53",
    "2023-06-02 13:29:09",
    "2023-06-02 13:32:18"
  ),
  kg_added = c(0,0.3,0.53,1.07,1.0,2.64,3.20,2.70,2.62),
  kg_total = c(0,0.3,0.83,1.9,2.9,5.54,8.74,11.4,14.06)
) %>% 
  mutate(datetime = ymd_hms(datetime, tz = "Canada/Saskatchewan"))

min_sal <- min(df$Salinity[ymd_hms(df$datetime, tz = "Canada/Saskatchewan")>= df_spike$datetime[1]])
min_con <- min(df$electrical.conductivity[ymd_hms(df$datetime, tz = "Canada/Saskatchewan")>= df_spike$datetime[1]])

df <- df %>%
  mutate(datetime = ymd_hms(datetime, tz = "Canada/Saskatchewan")) %>%
  filter(datetime >= "2023-06-02 13:10:00") %>%
  mutate(spike = case_when(
    (datetime >= df_spike$datetime[1] & datetime < df_spike$datetime[2]) ~  df_spike$kg_total[1],
    (datetime >= df_spike$datetime[2] & datetime < df_spike$datetime[3]) ~  df_spike$kg_total[2],
    (datetime >= df_spike$datetime[3] & datetime < df_spike$datetime[4]) ~  df_spike$kg_total[3],
    (datetime >= df_spike$datetime[4] & datetime < df_spike$datetime[5]) ~  df_spike$kg_total[4],
    (datetime >= df_spike$datetime[5] & datetime < df_spike$datetime[6]) ~  df_spike$kg_total[5],
    (datetime >= df_spike$datetime[6] & datetime < df_spike$datetime[7]) ~  df_spike$kg_total[6],
    (datetime >= df_spike$datetime[7] & datetime < df_spike$datetime[8]) ~  df_spike$kg_total[7],
    (datetime >= df_spike$datetime[8] & datetime < df_spike$datetime[9]) ~  df_spike$kg_total[8],
    (datetime >= df_spike$datetime[9]) ~  df_spike$kg_total[9])) %>% 
  mutate(Salinity_corr = Salinity - min_sal)

df %>% 
  group_by(spike) %>% 
  reframe(Salinity_corr = mean(Salinity_corr)) %>% 
  ggplot(aes(x = spike*1000, y = Salinity_corr)) + geom_point() +
  geom_abline(slope=1, intercept=0) +
  labs(x = bquote(Observed~salinity~(mg~L^-1)),
       y = bquote(Estimated~salinity~(mg~L^-1))) +
  theme_bw()

