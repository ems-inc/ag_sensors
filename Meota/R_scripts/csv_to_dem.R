library(raster)
library(sp)

f <- "C:/Users/smame/OneDrive/Desktop/EMS_Git/ag_sensors/Meota/data/EMS-Database_Site_elevation_data_Meota.csv"
pts <- read.csv(f)

# create a SpatialPointsDataFrame
coordinates(pts) = ~x+y 									   

# create an empty raster object to the extent of the points
rast <- raster(ext=extent(pts), resolution=23.95)

# rasterize your irregular points 
rasOut <- rasterize(pts, rast, pts$z, fun = mean) # we use a mean function here to regularly grid the irregular input points

#write it out as a geotiff
output_file <- "C:/Users/smame/OneDrive/Desktop/EMS_Git/ag_sensors/data/Meota_DEM_24m.tif"
writeRaster(rasOut, output_file, format = "GTiff")