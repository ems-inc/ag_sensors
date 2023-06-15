library(tidyverse)

# Plant available water
## Sandy loam (sl) soil example, following Henry's Handbook of Soil and Water (2003)

# Field capacity (fc)
# The amount of soil moisture or water content held in the soil after excess water 
# has drained away and the rate of downward movement has decreased. This usually 
# takes place 2–3 days after rain or irrigation in pervious soils of uniform structure and texture. 
# Note that:
# Pw = percent by weight (%/%)
# Pv = percent by volume (%/%)
# Note that Pv = Pw*bd

# Wilting point (wp)
# the minimum amount of water in the soil that the plant requires not to wilt. If the 
# soil water content decreases to this or any lower point a plant wilts and can no longer 
# recover its turgidity when placed in a saturated atmosphere for 12 hours. 

# Bulk density (bd)
# calculated as the dry weight of soil divided by its volume. This volume includes the 
# volume of soil particles and the volume of pores among soil particles. Bulk density 
# is typically expressed in g/cm3.

# Here are some typical BD values:
tibble(
  texture_class = c(
    "sand",
    "loamy sand",
    "sandy loam",
    "loam",
    "sandy clay loam",
    "silty clay loam",
    "silty loam",
    "clay loam",
    "silty clay",
    "sandy clay",
    "clay"
  ),
  bd_g_cm3 = c(1.65, 1.6, 1.55, 1.5, 1.5, 1.5, 1.5, 1.45, 1.45, 1.4, 1.35)
)

fc <- 0.15
wp <- 0.10
bd <- 1.50  # From Henry (2003)

vmc_fc <- fc*bd   # Volumetric water content at field capacity
vmc_wp <- wp*bd   # Volumetric water content at wilting point

# Use volumetric water content (VWC) at FC and WP to determine plant available water
h2o_avail <- vmc_fc - vmc_wp # % by volume

# Therefore 
