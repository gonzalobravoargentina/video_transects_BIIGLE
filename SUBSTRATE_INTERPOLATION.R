# SUBSTRATES MAPS----
# Load required libraries
library(rgdal)
library(gstat)
library(raster)
library(sp)
library(leaflet)
library(here)
library(dplyr)

# Load the KML polygon for Bahía Arredondo
bahia_arredondo <- readOGR(dsn = "Arredondo_v2.kml")

# Filter only bottom types
video.annotations.Paralenz_bottomtype <- subset(video.annotations.Paralenz, label_2 == "Substrate")

# Filter and clean the data, selecting relevant columns
data_substrate <- video.annotations.Paralenz_bottomtype %>%
  dplyr::select(GPSLongitude, GPSLatitude, Substrate_type)



# -------- Interpolation with all substrate types ----

# Create a SpatialPointsDataFrame using the original data
coordinates(data_substrate) <- ~ GPSLongitude + GPSLatitude

# Generate a regular grid within the KML polygon boundaries
grid <- spsample(bahia_arredondo, type = "regular", cellsize = resolution)

# Create a SpatialPixelsDataFrame for the grid
grid_df <- data.frame(coordinates(grid))
coordinates(grid_df) <- ~ x1 + x2
gridded(grid_df) <- TRUE

# Perform IDW interpolation with idp = 0.11
idw_interpolation_all <- idw(formula = Substrate_type ~ 1,
                             locations = data_substrate,
                             newdata = grid_df,
                             idp = 0.11)

# Convert the interpolation result to a raster
raster_all <- rasterFromXYZ(as.data.frame(idw_interpolation_all)[, c("x1", "x2", "var1.pred")])

# Assign projection (WGS84) to the raster
projection(raster_all) <- CRS("+proj=longlat +datum=WGS84")

# Mask the raster to fit within the KML polygon
raster_all <- mask(raster_all, bahia_arredondo)

# Define color palette for all substrate types
color_palette_all <- colorNumeric(
  palette = c("#D2B48C", "#CD853F", "#A52A2A", "#FF0000"),
  domain = values(raster_all),
  na.color = "transparent"
)

# -------- Create the Leaflet Map

leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_all, colors = color_palette_all, opacity = 0.8) %>%
  addCircleMarkers(data = as.data.frame(data_substrate),
                   lng = ~GPSLongitude,
                   lat = ~GPSLatitude,
                   color = ~color_palette_all(Substrate_type),
                   radius = 5,
                   fillOpacity = 1,
                   stroke = FALSE,
                   popup = ~paste("Substrate Type: ", Substrate_type)) %>%
  addLegend(
    position = "bottomright",
    colors = c('#D2B48C', '#CD853F', '#A52A2A', '#FF0000'),
    labels = c(
      "1 = Unconsolidated (soft): Fine sand (no shell fragments)",
      "2 = Unconsolidated (soft): Coarse sand (with shell fragments)",
      "3 = Unconsolidated (soft): Gravel (2-10mm)",
      "4 = Consolidated (hard): Rock"
    ),
    title = "Substrate Types",
    opacity = 1
  )



# -------- Interpolation without Rock ----

# Filter and clean the data, selecting relevant columns
data_substrate <- video.annotations.Paralenz_bottomtype %>%
  dplyr::select(GPSLongitude, GPSLatitude, Substrate_type)

# Remove rock (Substrate Type 4) from the data
data_sin_rock <- subset(data_substrate, Substrate_type != 4)

# Ensure it's a plain data frame before setting coordinates
data_sin_rock <- as.data.frame(data_sin_rock)

# Create a SpatialPointsDataFrame from the filtered data
coordinates(data_sin_rock) <- ~ GPSLongitude + GPSLatitude

# Define grid resolution (adjust as needed)
resolution <- 0.000008  # Grid cell size

# Generate a regular grid within the KML polygon boundaries
grid <- spsample(bahia_arredondo, type = "regular", cellsize = resolution)

# Create a SpatialPixelsDataFrame for the grid
grid_df <- data.frame(coordinates(grid))
coordinates(grid_df) <- ~ x1 + x2
gridded(grid_df) <- TRUE

# Interpolate using Inverse Distance Weighting (IDW)
idw_interpolation <- idw(formula = Substrate_type ~ 1,
                         locations = data_sin_rock,
                         newdata = grid_df,
                         idp = 3)

# Convert the interpolation result to a raster
raster_sinroca <- rasterFromXYZ(as.data.frame(idw_interpolation)[, c("x1", "x2", "var1.pred")])

# Assign projection (WGS84)
projection(raster_sinroca) <- CRS("+proj=longlat +datum=WGS84")

# Round raster values to make them discrete
raster_sinroca[] <- round(raster_sinroca[])
raster_sinroca[raster_sinroca[] < 1] <- 1
raster_sinroca[raster_sinroca[] > 3] <- 3

# Mask the raster to fit within the KML polygon
raster_sinroca <- mask(raster_sinroca, bahia_arredondo)

# Define color palette for substrate types without rock
color_palette_sinroca <- colorFactor(
  palette = c("#D2B48C", "#CD853F", "#A52A2A"),
  domain = 1:3,
  na.color = "transparent"
)

# -------- Interpolation for Rock Only ---- 

# Filter and clean the data, selecting relevant columns
data_substrate <- video.annotations.Paralenz_bottomtype %>%
  dplyr::select(GPSLongitude, GPSLatitude, Substrate_type)

# Reclassify data: rock (4) as 1, everything else as 0
data_roca <- data_substrate %>%
mutate(Substrate_type = ifelse(Substrate_type == 4, 1, 0))

# Ensure it's a plain data frame before setting coordinates
data_roca <- as.data.frame(data_roca)

# Create a SpatialPointsDataFrame from the filtered data
coordinates(data_roca) <- ~ GPSLongitude + GPSLatitude


# Interpolate using IDW for the rock data
idw_interpolation_roca <- idw(formula = Substrate_type ~ 1,
                              locations = data_roca,
                              newdata = grid_df,
                              idp = 3)

# Convert the rock interpolation to a raster
raster_roca <- rasterFromXYZ(as.data.frame(idw_interpolation_roca)[, c("x1", "x2", "var1.pred")])

# Assign projection (WGS84)
projection(raster_roca) <- CRS("+proj=longlat +datum=WGS84")

# Round raster values and set boundaries (0 or 1)
raster_roca[] <- round(raster_roca[])
raster_roca[raster_roca[] < 0] <- 0
raster_roca[raster_roca[] > 1] <- 1

# Mask the rock raster using the KML polygon
raster_roca <- mask(raster_roca, bahia_arredondo)

# Define color palette for rock (red color)
color_palette_roca <- colorFactor(
  palette = c("#FF0000"),
  domain = 1,
  na.color = "transparent"
)

# -------- Create Leaflet Maps

# Map for substrate types without rock
leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 0.8)

# Map for rock only
leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity = 0.8)

# Combined map with both rasters----
leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 0.8) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity = 0.8)



# Map including original points (for validation)----
color_palette_completo <- colorFactor(
  palette = c("#D2B48C", "#CD853F", "#A52A2A", "#FF0000"),
  domain = 1:4,
  na.color = "transparent"
)


substrate_map <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 0.8) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity = 0.8) %>%
  addCircleMarkers(data = as.data.frame(data_substrate),
                   lng = ~GPSLongitude,
                   lat = ~GPSLatitude,
                   color = ~color_palette_completo(Substrate_type),
                   radius = 5,
                   fillOpacity = 1,
                   stroke = FALSE,
                   popup = ~paste("Substrate Type: ", Substrate_type)) %>%
  addLegend(
    position = "bottomright",
    colors = c('#D2B48C', '#CD853F', '#A52A2A', '#FF0000'),  # Colores específicos
    labels = c(
      "1 = Unconsolidated (soft): Fine sand (no shell fragments)",
      "2 = Unconsolidated (soft): Coarse sand (with shell fragments)",
      "3 = Unconsolidated (soft): Gravel (2-10mm)",
      "4 = Consolidated (hard): Rock"
    ),
    title = "Substrate Types",
    opacity = 1
  )



