
#This script is designed to process data collected during video transects by integrating video annotations from the BIIGLE software with GPS tracking data from a GARMIN GPS device. The primary goal is to merge the GPS coordinates with the corresponding video annotations based on their timestamps, providing a georeferenced record of the observed events.

#Key Steps:
# 1- Read Video Annotations from BIIGLE Software: The script begins by importing video annotation data exported from BIIGLE. BIIGLE is an online platform used for annotating video footage, where specific events such as fish sightings, bottom types, invertebrates, and algae are labeled with precise timestamps. This step ensures that all annotations made during the video survey are available for further processing.
# 2- Read GPX Track from GARMIN GPS:Next, the script reads the GPX track data recorded by the GARMIN GPS device used during the video transects. The GPX (GPS Exchange Format) file contains a series of geographic coordinates (latitude and longitude) along with timestamps. This data represents the path followed by the tow boat and the diver during the survey.
# 3- Merge GPS Position with Video Annotations Using Time: The final step involves synchronizing the video annotations with the GPS data based on their timestamps. By matching the times in both datasets, the script can accurately assign geographic coordinates to each annotated event in the video. This merging process is crucial for creating a georeferenced dataset that maps observed events to specific locations on the seafloor.


#READ VIDEO ANNOTATIONS-------
# Source https://biigle.de/projects/477

#Set folder with data
Annotations <- "Biigle annotations"


#Read file in Biigle annotations folder 
library(readr)
video.annotations <- read.csv(file.path(Annotations,"2736-google-cloud.csv"))

#time is in json array []
#we use jsonlite to transform into r column 
#https://stackoverflow.com/questions/62573317/convert-json-array-to-r-column/62574065#62574065

library(jsonlite)
video.annotations <- lapply(1:nrow(video.annotations), function(x) {
  data.frame(video.annotations[x,1:14],fromJSON(as.character(video.annotations[x,"frames"])),
             row.names = NULL) })

video.annotations<- do.call(rbind,video.annotations)

#change name to frame column 
colnames(video.annotations)[15] <- "frame in secons"
video.annotations$frame.secs <- round(video.annotations$`frame in secons`)

#ADD a column with the video names as found in the Paralenz log file
library(stringr)
video.annotations$video_name <- paste0("MOV_",str_sub(video.annotations$video_filename,-11,-5))

library(tidyr)
# Use the separate function to split the label_hierarchy column into multiple columns
# Determine the number of new columns needed
# To do this, find the maximum number of splits possible in the label_hierarchy column
max_splits <- max(sapply(strsplit(video.annotations$label_hierarchy, ">"), length))

# Split the label_hierarchy column into multiple columns
video.annotations <- separate(video.annotations, label_hierarchy, 
                              into = paste0("label_", 1:max_splits), 
                              sep = ">", 
                              fill = "right", 
                              remove = FALSE)


# Remove spaces in the new columns
# Apply gsub function to each column to remove leading and trailing spaces
for (i in 1:max_splits) {
  video.annotations[[paste0("label_", i)]] <- gsub("^\\s+|\\s+$", "", video.annotations[[paste0("label_", i)]])
}

library(dplyr)
# Add the Substrate_type column
video.annotations <- video.annotations %>%
  mutate(Substrate_type = case_when(
    label_name == "Rock" ~ 4,
    label_name == "Gravel (2-10mm)" ~ 3,
    label_name == "Fine sand (no shell fragments)" ~ 1,
    label_name == "Coarse sand (with shell fragments)" ~ 2,
    TRUE ~ NA_integer_
  )) %>%
  select(video_annotation_label_id, label_id,label_name, Substrate_type, everything())  # Move Substrate_type after CATAMI_DISPLAY_NAME



# READ .GPX -------------------
# GPS MODEL: GARMIN
# recorded one point each 5 secs


library(sp) 
# Get a list of all GPX files in the "GPX files" folder
filesGPX <- list.files(path = "GPX files", pattern = "*.gpx", full.names = TRUE)

library(rgdal)
# Read GPX files and store them in a list
gpx <- lapply(filesGPX, function(f) { readOGR(dsn = f, layer = "track_points") })

# Create a list with coordinates and time from all GPX files
gpxlist <- lapply(gpx, function(f) { data.frame(coordinates(f), f$time) })

#Create one DATAFRAME with the gps data 
library(purrr)
track <- dplyr::bind_rows(gpxlist)


#As the GPX files are in UTC time, we must covert to the local time as the photos
library(lubridate)
track$timeUTC <- strptime(track$f.time,format = "%Y/%m/%d %H:%M:%OS",tz="UTC")

#local time
track$timeLOCAL <- force_tzs(track$timeUTC, "UTC", tzone_out = "America/Argentina/Catamarca", roll = FALSE)

colnames(track) <- c("lon","lat","f.time","timeUTC","timeLOCAL")

#remove the GPX files from de environment 
rm(gpxlist,gpx)

#LAYBACK CORRECTION----
# As the GPS is in the boat, the diver past 30 sec later so we Adjust the GPS position by 30 seconds
track$timeLOCAL_corrected <- track$timeLOCAL + 30


#READ PARALENZ LOG------ 
library(plyr)
library(readr)

#get a list of all the CSV files
filesPARALENZ<- list.files(path = "Paralenz log", pattern = "*.CSV", full.names = TRUE)

#Read all CSV files an import all in one dataframe
PARALENZ = ldply(filesPARALENZ, read_csv)

video <- as.data.frame(unique(PARALENZ$`Image/video-file`))

#Transforme time column to POSIXlt (local time Argentina)
PARALENZ$timeLOCAL <- strptime(PARALENZ$Time, "%Y:%m:%d %H:%M:%S")

#Transforme depth column into numeric 
PARALENZ$Depth <- as.numeric(PARALENZ$Depth)

#created column videos as factor
PARALENZ$video <- as.factor(PARALENZ$`Image/video-file`)

#add columns with seconds of video (each video of ~10 min = 60 secs). In log the data is stored each sec. 
library(dplyr)
PARALENZ <- PARALENZ %>% group_by(video) %>% dplyr::mutate(frame.secs = seq_len(n()))

colnames(PARALENZ)[4] <- "video_name"
video <- as.data.frame(unique(PARALENZ$video_name))


#MERGE PARALENZ log and Biigle annotations
library(dplyr)
video.annotations.Paralenz <- left_join(video.annotations,PARALENZ , by=c("frame.secs","video_name"))

#complete some CATAMI names in the column label_name
library(dplyr)
video.annotations.Paralenz <- video.annotations.Paralenz %>%
  mutate(label_name = case_when(
    label_name == "Green" ~ "Macroalgae:Filamentous/filiform:Green",
    label_name == "Filamentous / filiform" ~ "Macroalgae:Filamentous/filiform",
    label_name == "Benthic" ~ "Echinoderms:Sea cucumbers",
    label_name == "Simple" ~ "Sponges:Massiveforms:Simple",
    label_name == "Solitary" ~ "Ascidians:Unstalked:Solitary",
    label_name == "Encrusting" ~ "Sponges:Crusts:Encrusting",
    label_name == "Other polychaetes"~ "Aphrodita sp.",
    TRUE ~ as.character(label_name)  # Mantener cualquier otro valor como está
  ))


#remove files from the global environment
rm(PARALENZ,video,video.annotations)

#Merge GPS data with video annotations based on TIMESTAMPS------
#MERGE gpx Track and annotations layback already corrected
for (i in 1:length(track$timeLOCAL_corrected)){
  isbewteen<-between(video.annotations.Paralenz$timeLOCAL, track$timeLOCAL_corrected[i], track$timeLOCAL_corrected[i]+5)
  video.annotations.Paralenz$GPSLongitude[isbewteen ]<-track$lon[i]
  video.annotations.Paralenz$GPSLatitude[isbewteen ]<-track$lat[i]
  video.annotations.Paralenz$timegpsUTC[isbewteen ]<-track$f.time[i]
}


#TRANSECTS DATA---------
# Add transect name (T1 (North) to T8 (South)) , this name order does not correspond with time order
video.annotations.Paralenz <- video.annotations.Paralenz %>%
  mutate(Transect = case_when(
    video %in% c("MOV_0006_GB", "MOV_0007_GB") ~ "T1",
    video %in% c("MOV_0005_GB", "MOV_0004_GB") ~ "T2",
    video %in% c("MOV_0002_GB", "MOV_0001_GB") ~ "T3",
    video %in% c("MOV_0496_GT", "MOV_0495_GT") ~ "T4",
    video %in% c("MOV_0497_GT", "MOV_0498_GT") ~ "T5",
    video %in% c("MOV_0002_GT", "MOV_0001_GT") ~ "T6",
    video %in% c("MOV_0006_GT", "MOV_0005_GT", "MOV_0004_GT") ~ "T7",
    video %in% c("MOV_0007_GT", "MOV_0008_GT") ~ "T8",
    TRUE ~ NA_character_ # Asignar NA para cualquier otro valor
  ))

#Calculate the time that took each transect 
transect_times <- video.annotations.Paralenz %>%
  group_by(Transect) %>%
  summarise(start_time = min(timeLOCAL),
            end_time = max(timeLOCAL),
            duration = difftime(max(timeLOCAL), min(timeLOCAL), units = "mins"))

#Transect column to factor
video.annotations.Paralenz$Transect <- factor(video.annotations.Paralenz$Transect, levels = c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8"))

# Create a DataFrame
transect_times <- data.frame(Transect = c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8"),
                             duration_minutes = NA_real_)

#Calculate the time for each transect
for (transect in unique(video.annotations.Paralenz$Transect)) {
  transect_data <- video.annotations.Paralenz %>%
    filter(Transect == transect) %>%
    arrange(timeLOCAL)
  
  start_time <- min(transect_data$timeLOCAL)
  end_time <- max(transect_data$timeLOCAL)
  duration_minutes <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  transect_times[transect_times$Transect == transect, "duration_minutes"] <- duration_minutes
}


#Calculate each transect distance 
library(dplyr)
library(geosphere)

video.annotations.Paralenz$GPSLongitude <- as.numeric(video.annotations.Paralenz$GPSLongitude)
video.annotations.Paralenz$GPSLatitude <- as.numeric(video.annotations.Paralenz$GPSLatitude)


# Create a DataFrame
transect_distances <- data.frame(Transect = c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8"),
                                 distance_km = NA_real_)

#Calculate the distance of each transect
for (transect in unique(video.annotations.Paralenz$Transect)) {
  transect_data <- video.annotations.Paralenz %>%
    filter(Transect == transect) %>%
    arrange(timeLOCAL)
  
  # Obtener las coordenadas iniciales y finales
  start_coord <- transect_data[1, c("GPSLongitude", "GPSLatitude")]
  end_coord <- transect_data[nrow(transect_data), c("GPSLongitude", "GPSLatitude")]
  
  # Calcular la distancia en metros usando la fórmula de Haversine
  distance_m <- distHaversine(start_coord, end_coord)
  
  # Actualizar transect_distances con la distancia calculada en metros
  transect_distances[transect_distances$Transect == transect, "distance_m"] <- distance_m
}


transect_data <- merge(transect_distances, transect_times, by = "Transect")
transect_data$duration_hours <- transect_data$duration_minutes / 60
transect_data$distance_km <- transect_data$distance_m / 1000
transect_data$speed_kph <- transect_data$distance_km / transect_data$duration_hours


# Create a function to calculate the standard error
std_error <- function(x) {
  sd(x) / sqrt(length(x))
}

# Calculate the statistics
means <- sapply(transect_data[,-1], mean, na.rm = TRUE)  # Means (excluding the Transect column)
std_errors <- sapply(transect_data[,-1], std_error)      # Standard errors (excluding the Transect column)
sums <- sapply(transect_data[,-1], sum, na.rm = TRUE)    # Sums (excluding the Transect column)

# Create a data frame with the statistics
Transect_stats <- data.frame(
  Transect = c("Mean", "Std_Error", "Sum"),
  distance_km = c(means["distance_km"], std_errors["distance_km"], sums["distance_km"]),
  distance_m = c(means["distance_m"], std_errors["distance_m"], sums["distance_m"]),
  duration_minutes = c(means["duration_minutes"], std_errors["duration_minutes"], sums["duration_minutes"]),
  duration_hours = c(means["duration_hours"], std_errors["duration_hours"], sums["duration_hours"]),
  speed_kph = c(means["speed_kph"], std_errors["speed_kph"], sums["speed_kph"])
)



#merge transect distance and time info into large dataframe
video.annotations.Paralenz <- merge(video.annotations.Paralenz, transect_data, by = "Transect", all.x = FALSE)

rm(transect_data,transect_distances,transect_times, end_coord,end_time,start_coord,start_time,stats_df)

#Create DATA files with count of each category-------
library(dplyr)
# Create a new dataframe with the count of each category per second and keep the other columns
video.annotations.Paralenz_count <- video.annotations.Paralenz %>%
  mutate(truncated_time = trunc(timeLOCAL, "secs")) %>%
  add_count(truncated_time, label_name, name = "count") %>%
  distinct(truncated_time, label_name, .keep_all = TRUE)

video.annotations.Paralenz_count <- video.annotations.Paralenz_count %>%
  mutate(count = as.numeric(count))

total_count <- sum(video.annotations.Paralenz_count$count)

#Export FULL DATA 
#set working directoty in Data folder
#write.csv(video.annotations.Paralenz_count,"Full_DATA.csv")
#write.csv(video.annotations.Paralenz,"Count_DATA.csv")
#gracilaria_data <- subset(video.annotations.Paralenz_count,label_name=="Gracilaria")
#write.csv(gracilaria_data,"Gracilaria_DATA.csv")

#CHECKLIST-----
Check_list <- video.annotations.Paralenz %>%
  add_count(label_name, name = "count") %>%
  distinct(label_name, .keep_all = TRUE) %>%
  select(label_1,label_2,label_3,label_name,  count)

#write.csv(Check_list, "Check_list.csv")


#TRACK MAPS-----
#MAP with the track of towboat
library(leaflet)
library(leaflet.extras)
library(webshot)
library(htmlwidgets)


map_track <- leaflet() %>%
  addTiles() %>%
  addCircles(data = track, ~lon, ~lat, radius = 1, color = "orange", stroke = FALSE) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addScaleBar() %>%
  setView(lng = mean(track$lon), lat = mean(track$lat), zoom = 13.5)

# Guardar el mapa como archivo HTML
#saveWidget(map, "map.html", selfcontained = TRUE)

# Convertir el archivo HTML a PDF
#webshot("map.html", file = "map.pdf")


#view photos in a map
#Point of map center
x1 <- -65.809794
y1 <- -45.020866

#Color for video Name 
#set colors for videos names:
pal <- colorFactor(
  palette = 'Dark2',
  domain = video.annotations.Paralenz$video
)

library(leaflet)
map_transects <- leaflet(video.annotations.Paralenz) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~video_name,color = ~pal(video),radius = 2)%>%
addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)

# ALGAE Community Maps-----
library(ggplot2)
library(dplyr)
library(sf)
library(dplyr)
library(leaflet)

# Filtrar las comunidades específicas
communities <- c("Community Ulva", "Community Macrocystis", 
                 "Community Filamentous", "Community Dictyota", "Community Codium")

filtered_annotations <- video.annotations.Paralenz %>%
  filter(label_name %in% communities)

# Definir una paleta de colores para cada comunidad
color_palette <- colorFactor(palette = c("red", "blue", "green", "purple", "orange"), 
                             levels = communities)

# Crear el mapa
leaflet(filtered_annotations) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, 
                   color = ~color_palette(label_name), 
                   popup = ~label_name, 
                   radius = 5) %>%
  addLegend("bottomright", 
            pal = color_palette, 
            values = ~label_name, 
            title = "Communities",
            opacity = 1) %>%
  setView(lng = mean(filtered_annotations$GPSLongitude), 
          lat = mean(filtered_annotations$GPSLatitude), 
          zoom = 15)

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

# -------- Interpolation without Rock (Substrate Type 4)

# Remove rock (Substrate Type 4) from the data
data_sin_rock <- subset(data_substrate, Substrate_type != 4)

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

# -------- Interpolation for Rock Only 

# Reclassify data: rock (4) as 1, everything else as 0
data_roca <- data_substrate %>%
  mutate(Substrate_type = ifelse(Substrate_type == 4, 1, 0))

# Create a SpatialPointsDataFrame for the reclassified data
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

# Combined map with both rasters
leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 0.8) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity = 0.8)

# Map including original points (for validation)
color_palette_completo <- colorFactor(
  palette = c("#D2B48C", "#CD853F", "#A52A2A", "#FF0000"),
  domain = 1:4,
  na.color = "transparent"
)

leaflet() %>%
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
                   popup = ~paste("Substrate Type: ", Substrate_type))



# -------- SPECIES MAPS------


# 1 Leurocyclus tuberculosus
map_Leurocyclustuberculosus <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 0.8) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity = 1) %>%
  addCircleMarkers(data = subset(video.annotations.Paralenz, label_name == "Leurocyclus tuberculosus"),
                   lng = ~GPSLongitude,
                   lat = ~GPSLatitude,
                   color = "Black",
                   radius = 2)



#2 Carcinus maenas
map_Carcinusmaenas <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 0.8) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity = 1) %>%
  addCircleMarkers(data = subset(video.annotations.Paralenz, label_name == "Carcinus maenas"),
                   lng = ~GPSLongitude,
                   lat = ~GPSLatitude,
                   color = "Black",
                   radius = 2)

#3 Peltarion spinulosum
map_Peltarionspinulosum <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 0.8) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity = 1) %>%
  addCircleMarkers(data = subset(video.annotations.Paralenz, label_name == "Peltarion spinulosum"),
                   lng = ~GPSLongitude,
                   lat = ~GPSLatitude,
                   color = "Black",
                   radius = 2)

#4 Bony fishes
map_Bonyfishes <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 0.8) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity = 1) %>%
  addCircleMarkers(data = subset(video.annotations.Paralenz, label_name == "Bony fishes"),
                   lng = ~GPSLongitude,
                   lat = ~GPSLatitude,
                   color = "Black",
                   radius = 2)


#5 Antholoba achates
map_Antholobaachates <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 0.8) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity = 1) %>%
  addCircleMarkers(data = subset(video.annotations.Paralenz, label_name == "Antholoba achates"),
                   lng = ~GPSLongitude,
                   lat = ~GPSLatitude,
                   color = "Black",
                   radius = 2)

#5 Odontocymbiola magellanica
map_Odontocymbiolamagellanica <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 0.8) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity =1) %>%
  addCircleMarkers(data = subset(video.annotations.Paralenz, label_name == "Odontocymbiola magellanica"),
                   lng = ~GPSLongitude,
                   lat = ~GPSLatitude,
                   color = "Black",
                   radius = 2)

#7 Metridium
map_Metridium <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 0.8) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity = 1) %>%
  addCircleMarkers(data = subset(video.annotations.Paralenz, label_name == "Metridium"),
                   lng = ~GPSLongitude,
                   lat = ~GPSLatitude,
                   color = "Black",
                   radius = 2)

#8 Sea stars
map_Seastars <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 0.8) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity = 1) %>%
  addCircleMarkers(data = subset(video.annotations.Paralenz, label_name == "Sea stars"),
                   lng = ~GPSLongitude,
                   lat = ~GPSLatitude,
                   color = "Black",
                   radius = 2)

#9 Polyzoa opuntia
map_Polyzoaopuntia <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 0.8) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity = 1) %>%
  addCircleMarkers(data = subset(video.annotations.Paralenz, label_name == "Polyzoa opuntia"),
                   lng = ~GPSLongitude,
                   lat = ~GPSLatitude,
                   color = "Black",
                   radius = 2)

#10 Macrocystis pyrifera
map_Macrocystispyrifera <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 1) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity = 1) %>%
  addCircleMarkers(data = subset(video.annotations.Paralenz, label_name == "Macrocystis pyrifera"),
                   lng = ~GPSLongitude,
                   lat = ~GPSLatitude,
                   color = "Black",
                   radius = 1.5)

# 11 Gracilaria
gracilaria_data <- subset(video.annotations.Paralenz_count,label_name=="Gracilaria")

# Asignar colores más fuertes a los lugares con más anotaciones
color_palette_Gracilaria <- colorRampPalette(c("white", "gray"))

# Create the map
map_gracilaria <- leaflet() %>%addTiles() %>% addProviderTiles("Stadia.AlidadeSmooth") %>% setView(lng = mean(bbox(bahia_arredondo)[1, ]), lat = mean(bbox(bahia_arredondo)[2, ]), zoom = 14.5)  %>% addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 1) %>%setView(lng = mean(bbox(bahia_arredondo)[1, ]), lat = mean(bbox(bahia_arredondo)[2, ]), zoom = 14.5) %>% addRasterImage(raster_roca, colors = color_palette_roca, opacity = 1) 

# Add circles with colors based on the number of annotations
map_gracilaria <- addCircleMarkers(
  map_gracilaria,
  data = gracilaria_data,
  lng = ~GPSLongitude,
  lat = ~GPSLatitude,
  radius = ~ifelse(count == 5, 6, count * 2),  # Adjust circle size
  color = ~color_palette_Gracilaria(5)[count],
  fillOpacity = 1
)

# Add a legend with title
map_gracilaria <- addLegend(
  map_gracilaria,
  position = "bottomright",
  colors = color_palette_Gracilaria(5),
  labels = c(">0 - 10%", "11 - 30%", "31 - 50%", "51 - 75%", "76 - 100%"),
  opacity = 0.7,
  title = "<i>Gracilaria</i> <br> Cover"
)

map_gracilaria

#11 Anasterias antarctica
map_Anasteriasantarctica <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 1) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity = 1) %>%
  addCircleMarkers(data = subset(video.annotations.Paralenz, label_name == "Anasterias antarctica"),
                   lng = ~GPSLongitude,
                   lat = ~GPSLatitude,
                   color = "Black",
                   radius = 1.5)



#DENSITY DATA----
# Load required libraries
library(dplyr)
library(sf)
library(raster)
library(geosphere)

# **Step 1: Convert data points to a spatial object**
# Convert GPS coordinates into an sf spatial object
puntos_sf <- st_as_sf(video.annotations.Paralenz, 
                      coords = c("GPSLongitude", "GPSLatitude"), 
                      crs = 4326)

# **Step 2: Filter relevant species data**
# Keep only rows where the main label is "Biota"
puntos_sf <- puntos_sf %>% filter(label_1 == "Biota")

# **Step 3: Extract substrate values**
# Extract values from rasters for soft substrate and rock under each point
valores_sinroca <- extract(raster_sinroca, puntos_sf)
valores_roca <- extract(raster_roca, puntos_sf)

# **Step 4: Assign substrate type to points**
# Create a column to classify substrate type based on extracted raster values
puntos_sf$tipo_fondo <- NA
puntos_sf$tipo_fondo[valores_sinroca > 0] <- "Unconsolidated (soft)"
puntos_sf$tipo_fondo[valores_roca > 0] <- "Rock"

# **Step 5: Assign transects to filtered points**
# Filter the original annotations to include only "Biota"
video_annotations_biota <- video.annotations.Paralenz %>%
  filter(label_1 == "Biota")

# Ensure rows match in order with puntos_sf and assign the Transect column
puntos_sf$Transect <- video_annotations_biota$Transect

# **Step 6: Filter valid points**
# Retain only points with an assigned substrate type
puntos_sf <- puntos_sf[!is.na(puntos_sf$tipo_fondo),]

# **Step 7: Count points by species, substrate type, and transect**
# Group data by species, substrate type, and transect, and count occurrences
conteo_fondo_especie_transecta <- puntos_sf %>%
  group_by(label_name, tipo_fondo, Transect) %>%
  summarise(conteo = n(), .groups = "drop")

# **Step 8: Create a complete grid**
# Ensure all possible combinations of species, transects, and substrate types are present
complete_grid <- expand.grid(
  label_name = unique(conteo_fondo_especie_transecta$label_name),
  Transect = unique(conteo_fondo_especie_transecta$Transect),
  tipo_fondo = unique(conteo_fondo_especie_transecta$tipo_fondo)
)

# **Step 9: Fill missing data with zero counts**
# Combine the complete grid with the existing counts and assign 0 to missing combinations
conteo_fondo_especie_transecta_complete <- complete_grid %>%
  left_join(conteo_fondo_especie_transecta, by = c("label_name", "Transect", "tipo_fondo")) %>%
  mutate(conteo = ifelse(is.na(conteo), 0, conteo))  # Set count to 0 if missing

# **Step 10: Calculate distance between consecutive points within each transect**
# Filter data to include only relevant substrate categories
data <- video.annotations.Paralenz %>%
  arrange(Transect, Time) %>%
  filter(label_name %in% c("Coarse sand (with shell fragments)", 
                           "Fine sand (no shell fragments)", 
                           "Gravel (2-10mm)", 
                           "Rock"))

# Add columns for latitude and longitude of the next point and calculate distances
data <- data %>%
  group_by(Transect) %>%
  mutate(
    lat_lag = lead(GPSLatitude),          # Latitude of the next point
    long_lag = lead(GPSLongitude),        # Longitude of the next point
    distance = ifelse(is.na(lat_lag), 0,  # Distance is 0 for the last point
                      distHaversine(cbind(GPSLongitude, GPSLatitude), cbind(long_lag, lat_lag)))
  ) %>%
  ungroup()

# **Step 11: Reclassify certain substrate types and aggregate distances**
# Group "Coarse sand", "Fine sand", and "Gravel" as "Unconsolidated (soft)"
data <- data %>%
  mutate(label_name = ifelse(label_name %in% c("Coarse sand (with shell fragments)", 
                                               "Fine sand (no shell fragments)", 
                                               "Gravel (2-10mm)"), 
                             "Unconsolidated (soft)", label_name))

# Summarize total distance for each substrate type within each transect
substrate_distances <- data %>%
  group_by(Transect, label_name) %>%
  summarise(total_distance = sum(distance, na.rm = TRUE), .groups = "drop")

# Rename columns for clarity
names(substrate_distances) <- c("Transect", "substrate_type", "distance")

# **Step 12: Add distance and calculate area**
# Merge substrate distances with species counts and calculate the area (distance * width)
conteo_fondo_especie_transecta_complete <- conteo_fondo_especie_transecta_complete %>%
  left_join(substrate_distances, by = "Transect") %>%
  mutate(area = distance * 2)  # Area = distance * 2m (transect width)

# **Step 13: Filter matching rows between substrate type and fondo type**
conteo_fondo_especie_transecta_complete <- conteo_fondo_especie_transecta_complete %>%
  filter(tipo_fondo == substrate_type)

# **Step 14: Sum species counts per transect, regardless of substrate type**
conteo_total_por_transecta <- conteo_fondo_especie_transecta_complete %>%
  group_by(label_name, Transect) %>%
  summarise(
    conteo = sum(conteo, na.rm = TRUE),
    tipo_fondo = "ALL",              # Assign "ALL" to substrate type
    substrate_type = "ALL",
    distance = sum(distance, na.rm = TRUE),
    area = sum(area, na.rm = TRUE),
    .groups = "drop"
  )

# Combine "ALL" rows with the original dataframe
conteo_fondo_especie_transecta_complete_con_ALL <- conteo_fondo_especie_transecta_complete %>%
  bind_rows(conteo_total_por_transecta)

# **Step 15: Calculate density in individuals/m² and individuals/1000m²**
conteo_fondo_especie_transecta_complete_con_ALL <- conteo_fondo_especie_transecta_complete_con_ALL %>%
  mutate(
    density = conteo / area,           # Density in individuals/m²
    density_1000m2 = density * 1000   # Density in individuals/1000m²
  )

# **Step 16: Calculate average density per species and substrate type**
densidad_promedio <- conteo_fondo_especie_transecta_complete_con_ALL %>%
  group_by(label_name, tipo_fondo) %>%
  summarise(
    mean_density = mean(density_1000m2, na.rm = TRUE),  # Average density (ind/1000m²)
    sd_density = sd(density_1000m2, na.rm = TRUE),      # Standard deviation of density
    n = n(),                                           # Number of replicates (transects)
    .groups = "drop"
  )

# **Step 17: Export results**
# Save the summarized density data to a CSV file
#write.csv(densidad_promedio, "density_summary.csv", row.names = FALSE)



#Species richness per substrate type----
video.annotations.Paralenz_Biota <- subset(video.annotations.Paralenz, label_1=="Biota")


bottom_types_biota <- data.frame(
  CATAMI_NAME = c("Polyzoa opuntia", "Ascidians:Unstalked:Solitary", "Antholoba achates", "Metridium", "Carcinus maenas", "Leucippa pentagona", "Leurocyclus tuberculosus", "Ovalipes trimaculatus", "Peltarion spinulosum", "Echinoderms:Sea cucumbers", "Allostichaster capensis", "Anasterias antarctica", "Cosmasterias lurida", "Patagonotothen", "Sebastes oculatus", "Community Codium", "Gracilaria", "Macroalgae:Filamentous/filiform", "Macroalgae:Filamentous/filiform:Green", "Macrocystis pyrifera", "Undaria pinnatifida", "Community Dictyota", "Community Ulva", "Bivalves", "Odontocymbiola magellanica", "Sponges:Crusts:Encrusting", "Sponges:Massiveforms:Simple", "Aphrodita sp."),
  Substrate_type = c("Rock", "Rock", "Rock", "Rock", "All", "Rock", "Sand", "Sand", 
                     "Sand", "Sand", "Rock", "Rock","Rock", "All", "Rock", "Rock", "Sand", 
                     "Rock", "Rock", "Rock", "Rock", "Rock", "Rock", "Rock", "All", 
                     "Rock", "Rock", "All")
)


video.annotations.Paralenz_Biota <- merge(video.annotations.Paralenz_Biota, bottom_types_biota,
                                          by.x = "label_name", by.y = "CATAMI_NAME", 
                                          all.x = F)

library(ggplot2)
library(dplyr)

# Agrupar por 'Substrate_type.y' y contar los distintos 'label_name' por tipo de fondo
species_count <- video.annotations.Paralenz_Biota %>%
  group_by(Substrate_type.y) %>%
  summarise(Species_count = n_distinct(label_name))

ggplot(species_count, aes(x = Substrate_type.y, y = Species_count, group = 1)) +
  geom_point(size = 4, color = "blue") +  # Puntos
  geom_line(color = "blue", size = 1) +   # Líneas que conectan los puntos
  labs(title = "Cantidad de especies por tipo de fondo", 
       x = "Tipo de fondo", 
       y = "Cantidad de especies (label_name)") +
  theme_minimal()
