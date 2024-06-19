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










# Cargar las librerías necesarias
library(sf)
library(gstat)
library(automap)
library(ggplot2)
library(dplyr)

# Asegúrate de tener tus datos en un data frame llamado gracilaria_data
# gracilaria_data debe contener columnas llamadas GPSLongitude, GPSLatitude y count

# Verificar que las coordenadas sean de tipo numérico
gracilaria_data$GPSLongitude <- as.numeric(gracilaria_data$GPSLongitude)
gracilaria_data$GPSLatitude <- as.numeric(gracilaria_data$GPSLatitude)

# Convertir a un objeto sf
gracilaria_sf <- st_as_sf(gracilaria_data, coords = c("GPSLongitude", "GPSLatitude"), crs = 4326)

# Definir el CRS para gracilaria_sf
st_crs(gracilaria_sf) <- st_crs("+proj=longlat +datum=WGS84")

# Establecer el rango en el cual quieres interpolar
x_range <- c(-65.815, -65.802)  # min/max longitud del área de interpolación
y_range <- c(-45.027, -45.018)  # min/max latitud del área de interpolación

# Crear una cuadrícula de valores dentro del rango definido
grd <- expand.grid(x = seq(from = x_range[1], to = x_range[2], by = 0.00009),
                   y = seq(from = y_range[1], to = y_range[2], by = 0.00009))

# Convertir el objeto grd a un objeto sf
grd_sf <- st_as_sf(grd, coords = c("x", "y"), crs = st_crs(gracilaria_sf))

# Interpolar los datos usando automap
auto_result <- autoKrige(count ~ 1, gracilaria_sf, grd_sf)

# Convertir el resultado a un data frame
auto_result_df <- as.data.frame(auto_result$krige_output)

# Extraer las coordenadas de los objetos POINT
coords <- st_coordinates(st_as_sf(auto_result$krige_output))

# Crear un nuevo data frame con las coordenadas y los valores interpolados
interpolated_df <- data.frame(Longitude = coords[, 1],
                              Latitude = coords[, 2],
                              Interpolated_Count = auto_result_df$var1.pred)

# Verificar los datos interpolados
print(head(interpolated_df))

# Graficar el resultado usando ggplot2
ggplot() +
  geom_tile(data = interpolated_df, aes(x = Longitude, y = Latitude, fill = Interpolated_Count)) +
  scale_fill_viridis_c() +
  coord_fixed() +
  labs(title = "Kriging Interpolation of Species Counts",
       x = "Longitude", y = "Latitude", fill = "Interpolated Count") +
  theme_minimal() +
  geom_point(data = gracilaria_data, aes(x = GPSLongitude, y = GPSLatitude, color = count), size = 2) +
  scale_color_viridis_c() +
  theme(legend.position = "bottom")


# Cargar las librerías necesarias
library(sf)
library(gstat)
library(sp)
library(ggplot2)
library(dplyr)
library(automap)

# Asegúrate de tener tus datos en un data frame llamado gracilaria_data
# gracilaria_data debe contener columnas llamadas GPSLongitude, GPSLatitude y count

# Verificar que las coordenadas sean de tipo numérico
gracilaria_data$GPSLongitude <- as.numeric(gracilaria_data$GPSLongitude)
gracilaria_data$GPSLatitude <- as.numeric(gracilaria_data$GPSLatitude)

# Convertir a un objeto sf
gracilaria_sf <- st_as_sf(gracilaria_data, coords = c("GPSLongitude", "GPSLatitude"), crs = 4326)

# Definir el CRS para gracilaria_sf
st_crs(gracilaria_sf) <- st_crs("+proj=longlat +datum=WGS84")

# Transformar a coordenadas planas (UTM) para la interpolación
gracilaria_sf <- st_transform(gracilaria_sf, crs = 32721)  # CRS para UTM zona 21S, adecuado para Argentina

# Crear la cuadrícula de interpolación
bbox <- st_bbox(gracilaria_sf)
x_range <- seq(bbox["xmin"], bbox["xmax"], by = 10)  # Incremento de 10 metros para una cuadrícula más densa
y_range <- seq(bbox["ymin"], bbox["ymax"], by = 10)  # Incremento de 10 metros para una cuadrícula más densa
grd <- expand.grid(x = x_range, y = y_range)
coordinates(grd) <- ~ x + y
gridded(grd) <- TRUE
proj4string(grd) <- CRS(st_crs(gracilaria_sf)$proj4string)

# Convertir a objeto Spatial para la interpolación
gracilaria_sp <- as(gracilaria_sf, "Spatial")

# Interpolar los datos usando automap::autoKrige
auto_result <- autoKrige(count ~ 1, gracilaria_sp, grd)

# Convertir el resultado a un data frame para ggplot
krige_result <- auto_result$krige_output
krige_df <- as.data.frame(krige_result)
names(krige_df)[1:3] <- c("Longitude", "Latitude", "Interpolated_Count")

# Transformar de nuevo a coordenadas geográficas (longitud/latitud)
coordinates(krige_df) <- ~ Longitude + Latitude
proj4string(krige_df) <- CRS(st_crs(gracilaria_sf)$proj4string)
krige_df <- spTransform(krige_df, CRS("+proj=longlat +datum=WGS84"))
krige_df <- as.data.frame(krige_df)

# Verificar los datos interpolados
print(head(krige_df))

# Graficar el resultado usando ggplot2
ggplot() +
  geom_tile(data = krige_df, aes(x = Longitude, y = Latitude, fill = Interpolated_Count)) +
  scale_fill_viridis_c() +
  coord_fixed() +
  labs(title = "Kriging Interpolation of Species Counts",
       x = "Longitude", y = "Latitude", fill = "Interpolated Count") +
  theme_minimal() +
  geom_point(data = gracilaria_data, aes(x = GPSLongitude, y = GPSLatitude, color = count), size = 2) +
  scale_color_viridis_c() +
  theme(legend.position = "bottom")
# Graficar el resultado usando ggplot2
ggplot(krige_df, aes(x = Longitude, y = Latitude, fill = Interpolated_Count)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Kriging Interpolation of Species Counts",
       x = "Longitude", y = "Latitude", fill = "Interpolated Count") +
  theme_minimal() +
  geom_point(data = gracilaria_data, aes(x = GPSLongitude, y = GPSLatitude, color = count), size = 2) +
  scale_color_viridis_c() +
  theme(legend.position = "bottom")

# Asumiendo que `krige_df` contiene los resultados interpolados correctamente

# Verificar los datos interpolados
print(head(krige_df))

# Graficar el resultado usando ggplot2
ggplot() +
  geom_tile(data = krige_df, aes(x = Longitude, y = Latitude, fill = Interpolated_Count)) +
  scale_fill_viridis_c() +
  labs(title = "Kriging Interpolation of Species Counts",
       x = "Longitude", y = "Latitude", fill = "Interpolated Count") +
  theme_minimal() +
  geom_point(data = gracilaria_data, aes(x = GPSLongitude, y = GPSLatitude, color = count), size = 2) +
  scale_color_viridis_c() +
  theme(legend.position = "bottom")


library(ggplot2)

# Asumiendo que `krige_df` contiene los resultados interpolados correctamente

ggplot(krige_df, aes(x = Longitude, y = Latitude, fill = Interpolated_Count)) +
  geom_tile() 

