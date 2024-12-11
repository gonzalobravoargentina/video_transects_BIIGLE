# Load required libraries
library(dplyr)
library(geosphere)

# **Step 1: Assign substrate type to all annotations**
# Assign substrate type to annotations based on raster values for "Rock" and "Unconsolidated (soft)"
video.annotations.Paralenz$tipo_fondo <- NA
video.annotations.Paralenz$tipo_fondo[
  raster::extract(raster_sinroca, video.annotations.Paralenz[, c("GPSLongitude", "GPSLatitude")]) > 0
] <- "Unconsolidated (soft)"
video.annotations.Paralenz$tipo_fondo[
  raster::extract(raster_roca, video.annotations.Paralenz[, c("GPSLongitude", "GPSLatitude")]) > 0
] <- "Rock"

# **Step 2: Filter annotations to exclude non-biological labels**
# Exclude rows where the primary label is "Physical" (non-biological annotations)
video.annotations.Paralenz_filtered <- subset(video.annotations.Paralenz, label_1 != "Physical")

# **Step 3: Count annotations by substrate type and label_name**
# Group by substrate type (tipo_fondo) and label_name, and count the number of annotations
annotation_counts <- video.annotations.Paralenz_filtered %>%
  dplyr::group_by(tipo_fondo, label_name) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop")


# Ensure all possible combinations of tipo_fondo and label_name are present
complete_grid <- expand.grid(
  tipo_fondo = c("Rock", "Unconsolidated (soft)"),
  label_name = unique(video.annotations.Paralenz_filtered$label_name)
)

# Merge the counts with the complete grid and fill missing combinations with 0
annotation_counts <- complete_grid %>%
  left_join(annotation_counts, by = c("tipo_fondo", "label_name")) %>%
  mutate(count = ifelse(is.na(count), 0, count))

# **Step 4: Calculate total sampled area for each substrate type**
# Filter only rows relevant for substrate distance calculation (ignore transects here)
filtered_annotations <- video.annotations.Paralenz %>%
  filter(label_name %in% c("Coarse sand (with shell fragments)", 
                           "Fine sand (no shell fragments)", 
                           "Gravel (2-10mm)", 
                           "Rock")) %>%
  mutate(label_name = ifelse(label_name %in% c("Coarse sand (with shell fragments)", 
                                               "Fine sand (no shell fragments)", 
                                               "Gravel (2-10mm)"), 
                             "Unconsolidated (soft)", "Rock"))

# Calculate the distances between consecutive points
filtered_annotations <- filtered_annotations %>%
  arrange(Transect, Time) %>%
  group_by(Transect) %>%
  mutate(
    lat_lag = lead(GPSLatitude),           # Latitude of the next point
    long_lag = lead(GPSLongitude),         # Longitude of the next point
    distance = ifelse(is.na(lat_lag), 0,   # Distance is 0 for the last point
                      distHaversine(cbind(GPSLongitude, GPSLatitude), cbind(long_lag, lat_lag)))
  ) %>%
  ungroup()

# Summarize total distances for each substrate type
substrate_distances <- filtered_annotations %>%
  group_by(label_name) %>%
  summarise(
    total_distance = sum(distance, na.rm = TRUE),   # Total distance covered
    .groups = "drop"
  ) %>%
  dplyr::rename(tipo_fondo = label_name) %>%       # Rename for consistency
  mutate(area = total_distance * 2)            # Calculate total area (distance * width)

# Calculate overall sampled area
overall_area <- sum(substrate_distances$area, na.rm = TRUE)

# **Step 5: Calculate density for each label_name by substrate type and overall**
# Combine annotation counts with substrate areas
density_data <- annotation_counts %>%
  left_join(substrate_distances, by = "tipo_fondo") %>%
  mutate(
    density = ifelse(is.na(area) | area == 0, NA, count / area),          # Avoid division by zero
    density_per_1000m2 = density * 1000                                  # Convert density to individuals per 1000 m²
  )

# Calculate overall density for each label_name
overall_density <- video.annotations.Paralenz_filtered %>%
  group_by(label_name) %>%
  summarise(
    count = n(),                                         # Total count across all substrates
    density = count / overall_area,                     # Density using overall sampled area
    density_per_1000m2 = density * 1000,                # Convert density to individuals per 1000 m²
    .groups = "drop"
  ) %>%
  mutate(tipo_fondo = "Overall")                        # Add "Overall" as a substrate category

# **Step 6: Combine substrate-specific and overall densities**
# Combine the substrate-specific and overall densities into one final data frame
final_density_data <- density_data %>%
  select(tipo_fondo, label_name, count, area, density, density_per_1000m2) %>%
  bind_rows(overall_density %>% 
              select(tipo_fondo, label_name, count, density, density_per_1000m2) %>%
              mutate(area = overall_area))             # Add overall area for consistency

# **Step 7: Final result**
# Final data frame with densities for each label_name and substrate type, including overall
final_density_data


# **Step 8: Export results**
# Save the summarized density data to a CSV file
#write.csv(final_density_data, "density_summary.csv", row.names = FALSE)

