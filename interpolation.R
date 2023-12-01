# Load necessary libraries
library(sf)
library(gstat)

#https://nceas.github.io/oss-lessons/spatial-data-gis-law/4-tues-spatial-analysis-in-r.html

# Assuming you have a grouped data frame called gracilaria_data
# Make sure the columns GPSLongitude and GPSLatitude are of numeric type

# Ungroup the data frame
gracilaria_data <- ungroup(gracilaria_data)

# Convert to sf object
gracilaria_sf <- st_as_sf(gracilaria_data, coords = c("GPSLongitude", "GPSLatitude"), crs = 4326)

# Check if the coordinates are numeric
if (!all(sapply(st_coordinates(gracilaria_sf), is.numeric))) {
  stop("GPS coordinates must be numeric.")
}

# Define the CRS for gracilaria_sf
st_crs(gracilaria_sf) <- st_crs("+proj=longlat +datum=WGS84")

# Establish an extent within which you want to interpolate
# -99/24 to -80/32.
x_range <- c(-65.815, -65.802)  # min/max longitude of the interpolation area
y_range <- c(-45.027, -45.018)  # min/max latitude of the interpolation area

# Create an empty grid of values ranging from the xmin-xmax, ymin-ymax
grd <- expand.grid(x = seq(from = x_range[1], to = x_range[2], by = 0.00009),
                   y = seq(from = y_range[1], to = y_range[2], by = 0.00009))  # expand points to grid

# Convert grd object to an sf object
grd_sf <- st_as_sf(grd, coords = c("x", "y"), crs = st_crs(gracilaria_sf))

# Interpolate the data using gstat::idw
idw_pow1 <- gstat::idw(formula = count ~ 1, locations = gracilaria_sf, newdata = grd_sf,idp = 0.5)

# Plot the grid with points overlayed
# Plot the result of IDW interpolation
plot(grd_sf, col = "grey")
plot(idw_pow1, add = TRUE, main = "IDW Interpolation")
plot(gracilaria_sf, pch = 15, col = "red", cex = 0.5, add = TRUE)



