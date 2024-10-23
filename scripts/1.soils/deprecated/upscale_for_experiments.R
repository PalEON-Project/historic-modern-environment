## Upscaling to 8 km scale

rm(list = ls())

library(sp)
library(ncdf4)

# Load one of the raw data experiments
# they differ in method and depth
load('data/raw/gssurgo_average_030.RData')

# Add coordinates to make spatial points data frame
coordinates(df_IL) <- ~ x + y

test <- aggregate(df_IL,
                  df_IL,
                  FUN = mean,
                  join = function(x, y) st_is_within_distance(x, y, 8000))


# Load STEPPS data
# this provides the grid we will be averaging to
# for minnesota, wisconsin, upper michigan
fc <- ncdf4::nc_open('~/Google Drive 2/longterm_feedbacks/FossilPollen/Data/msb-paleon-2/2Kyrs_Comp_Mean_Level2_v1.0.nc')

# Get lon, lat
# x and y are flipped to get the right easting and northing directions
y <- ncvar_get(fc, 'x')
x <- ncvar_get(fc, 'y')

coords <- cbind(x, y)
coords <- as.data.frame(coords)

coordinates(coords) <- ~ x + y
coordinates(df_IN) <- ~ x + y
coordinates(df_MI) <- ~ x + y
coordinates(df_MN) <- ~ x + y
coordinates(df_WI) <- ~ x + y

