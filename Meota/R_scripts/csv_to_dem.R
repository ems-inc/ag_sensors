library(raster)
library(sp)
library(tidyverse)

# f <- "C:/Users/smame/OneDrive/Desktop/EMS_Git/ag_sensors/Meota/data/EMS-Database_Site_elevation_data_Meota.csv"
# f <- "C:/Users/smame/OneDrive/Desktop/EMS_Git/ag_sensors/Meota/data/Simplot_DEM_xyz.csv"
f <- "C:/Users/smame/OneDrive/Desktop/EMS_Git/ag_sensors/Meota/data/Simplot_Yield2022_xyz.csv"
pts <- read.csv(f)

# # DEM
# pts <- pts %>% 
#   mutate(z = Elevation_/3.28084) %>% 
#   dplyr::select(-Elevation_)

# Yield
pts <- pts %>% 
  dplyr::select(x,y,Yld_Vol_We) %>% 
  rename(z = Yld_Vol_We)

# create a SpatialPointsDataFrame
coordinates(pts) = ~x+y 									   

# create an empty raster object to the extent of the points
# rast <- raster(ext=extent(pts), resolution=23.95)
rast <- raster(ext=extent(pts), resolution=20)

# rasterize your irregular points 
rasOut <- rasterize(pts, rast, pts$z, fun = mean) # we use a mean function here to regularly grid the irregular input points

#write it out as a geotiff
# output_file <- "C:/Users/smame/OneDrive/Desktop/EMS_Git/ag_sensors/Meota/data/Meota_DEM_24m.tif"
# output_file <- "C:/Users/smame/OneDrive/Desktop/EMS_Git/ag_sensors/Meota/data/Simplot_DEM_20m.tif"
output_file <- "C:/Users/smame/OneDrive/Desktop/EMS_Git/ag_sensors/Meota/data/Simplot_Yield2022_20m.tif"
writeRaster(rasOut, output_file, format = "GTiff", overwrite=TRUE)
