
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
    label_name == "Coquina / shellhash" ~ 6,
    label_name == "Rock" ~ 5,
    label_name == "Cobbles" ~ 4,
    label_name == "Gravel (2-10mm)" ~ 3,
    label_name == "Fine sand (no shell fragments)" ~ 2,
    label_name == "Coarse sand (with shell fragments)" ~ 1,
    TRUE ~ NA_integer_
  )) %>%
  select(video_annotation_label_id, label_id,label_name, Substrate_type, everything())  # Move Substrate_type after CATAMI_DISPLAY_NAME



# READ .GPX -------------------
# GPS MODEL: GARMIN
# recorded one point each 5 secs

#Option 1
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


#LAYBACK CORRECTION----
# As the GPS is in the boat, the diver past 30 sec later so we Adjust the GPS position by 30 seconds
track$timeLOCAL_corrected <- track$timeLOCAL + 30


#Option 2
#get a list of all the CSV files in the "GPS" folder
#filesGPX <- list.files(path = "GPX files", pattern = "*.gpx", full.names = TRUE)

#read gpx files and store them all in a list
#library(rgdal)
#gpx <- lapply(setNames(filesGPX, make.names(gsub("*.gpx$", "",filesGPX))), 
#              function(f) { readOGR(dsn=f, layer="track_points") }
#)

#create a list with coordinates and time from all de gpx files
#gpxlist <- lapply(gpx,function(f) { data.frame(f@coords,f$time) })


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

#Eliminate NA rows in column Image/video-file
#PARALENZ <- na.omit(PARALENZ, cols = "Image/video-file")

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
rm(PARALENZ,video,video.annotations,gpx,gpxlist)

#Merge GPS position with video annotations using time------
#MERGE gpx Track and annotations layback already corrected
for (i in 1:length(track$timeLOCAL_corrected)){
  isbewteen<-between(video.annotations.Paralenz$timeLOCAL, track$timeLOCAL_corrected[i], track$timeLOCAL_corrected[i]+5)
  video.annotations.Paralenz$GPSLongitude[isbewteen ]<-track$lon[i]
  video.annotations.Paralenz$GPSLatitude[isbewteen ]<-track$lat[i]
  video.annotations.Paralenz$timegpsUTC[isbewteen ]<-track$f.time[i]
}

# Merge GPS position with video annotations using time
# MERGE gpx Track and annotations with correcting layback 
#layback_seconds <- 30  # Layback in secs
#for (i in 1:length(track$timeLOCAL_corrected)) {
#  is_between <- between(video.annotations.Paralenz$timeLOCAL, 
#                        track$timeLOCAL_corrected[i] - layback_seconds, 
#                        track$timeLOCAL_corrected[i] + 5)
#    video.annotations.Paralenz$GPSLongitude[is_between] <- track$lon[i]
#  video.annotations.Paralenz$GPSLatitude[is_between] <- track$lat[i]
#  video.annotations.Paralenz$timegpsUTC[is_between] <- track$f.time[i]
#}

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


# mean
mean <- colMeans(transect_data[, sapply(transect_data, is.numeric)], na.rm = TRUE)


#merge transect distance and time info into large dataframe
video.annotations.Paralenz <- merge(video.annotations.Paralenz, transect_data, by = "Transect", all.x = FALSE)


#Create Data files-------
library(dplyr)
# Create a new dataframe with the count of each category per second and keep the other columns
video.annotations.Paralenz_count <- video.annotations.Paralenz %>%
  mutate(truncated_time = trunc(timeLOCAL, "secs")) %>%
  add_count(truncated_time, label_name, name = "count") %>%
  distinct(truncated_time, label_name, .keep_all = TRUE)

video.annotations.Paralenz_count <- video.annotations.Paralenz_count %>%
  mutate(count = as.numeric(count))



#Export FULL DATA 
#set working directoty in Data folder
#write.csv(video.annotations.Paralenz_count,"Full_DATA.csv")
#write.csv(video.annotations.Paralenz,"Count_DATA.csv")
gracilaria_data <- subset(video.annotations.Paralenz_count,label_name=="Gracilaria")
#write.csv(gracilaria_data,"Gracilaria_DATA.csv")

#CHECKLIST-----
Check_list <- video.annotations.Paralenz %>%
  add_count(label_name, name = "count") %>%
  distinct(label_name, .keep_all = TRUE) %>%
  select(label_1,label_2,label_3,label_name,  count)

#write.csv(Check_list, "Check_list.csv")


#MAPS-----
#MAP with the track of towboat
library(leaflet)
library(leaflet.extras)
library(webshot)
library(htmlwidgets)

# Crear el mapa en leaflet
map <- leaflet() %>%
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
leaflet(video.annotations.Paralenz) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~video_name,color = ~pal(video),radius = 2)%>%
addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)

#See Crabs in transect 
leaflet(subset(video.annotations.Paralenz,label_name=="Rock")) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,radius = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)

#See Ripples in transect 
leaflet(subset(video.annotations.Paralenz,label_name=="Ripples (<10cm height)")) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,radius = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)

#See Odontocymbiola magellanica in transect 
leaflet(subset(video.annotations.Paralenz,label_name=="Odontocymbiola magellanica")) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,radius = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)

leaflet(subset(video.annotations.Paralenz,label_name=="Polyzoa opuntia")) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,radius = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)

leaflet(subset(video.annotations.Paralenz,label_name=="Metridium")) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,radius = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)


#Crabs
leaflet(subset(video.annotations.Paralenz,label_name=="Leurocyclus tuberculosus")) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,radius = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)

leaflet(subset(video.annotations.Paralenz,label_name=="Leucippa pentagona")) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,radius = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)

leaflet(subset(video.annotations.Paralenz,label_name=="Carcinus maenas")) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,radius = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)

leaflet(subset(video.annotations.Paralenz,label_name=="Peltarion spinulosum")) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,radius = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)

leaflet(subset(video.annotations.Paralenz,label_name=="Ovalipes trimaculatus")) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,radius = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)


# Community Maps-----
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


#BOTTOM TYPE MAPS------

x1 <- -65.809794
y1 <- -45.020866

#Color for video Name 
#set colors for videos names:
video.annotations.Paralenz_bottomtype <- subset(video.annotations.Paralenz, label_2=="Substrate")


pal <- colorFactor(
  palette = 'Set3',
  domain = video.annotations.Paralenz_bottomtype$label_3
)

library(leaflet)
leaflet(video.annotations.Paralenz_bottomtype) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,color = ~pal(label_3),radius = 5, fillOpacity = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)






#DENSITY MAPS-----
# As each transect was of 2 m width we can calculate de abundance /m2 of mobile invertebrates
# 

invertebrates <- subset(video.annotations.Paralenz_count, CATAMI_TYPE=="Biota" & CATAMI_GROUP!="Macroalgae")
# Ensure the Transect column is a factor
invertebrates$Transect <- as.factor(invertebrates$Transect)

# Calculate the sampled area in square meters for each transect
transect_areas <- aggregate(distance_m ~ Transect, data = invertebrates, FUN = function(x) x[1] * 2)
colnames(transect_areas) <- c("Transect", "area_m2")

# Merge the calculated area with the invertebrates dataframe
invertebrates <- merge(invertebrates, transect_areas, by = "Transect", all.x = F)

# Count the number of times each category appears in each transect
organism_counts <- aggregate(count ~ Transect + label_name, data = invertebrates, sum)

# Calculate the density by dividing the number of organisms by the sampled area
organism_counts <- merge(organism_counts, transect_areas, by = "Transect", all.x = TRUE)
organism_counts$density_per_m2 <- organism_counts$count / organism_counts$area_m2

# View the final result
head(organism_counts)  # Display the first few rows of the resulting dataframe


  
#See Gracilaria in transect, in the case of gracilaria we have from 1 to 5 annotation per frame indicating density. 

library(dplyr)
library(leaflet)

gracilaria_data <- subset(video.annotations.Paralenz_count,label_name=="Gracilaria")

#write.csv(gracilaria_data,"Gracilaria_data.csv")

# Asignar colores más fuertes a los lugares con más anotaciones
color_palette <- colorRampPalette(c("lightblue", "blue"))

# Create the map
map <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  setView(lng = x1, lat = y1, zoom = 15)

# Add circles with colors based on the number of annotations
map <- addCircleMarkers(
  map,
  data = gracilaria_data,
  lng = ~GPSLongitude,
  lat = ~GPSLatitude,
  radius = ~ifelse(count == 5, 6, count * 2),  # Adjust circle size
  color = ~color_palette(5)[count],
  fillOpacity = 0.7
)

# Add a legend with title
map <- addLegend(
  map,
  position = "bottomright",
  colors = color_palette(5),
  labels = c("1 Annotation", "2 Annotations", "3 Annotations", "4 Annotations", "5 Annotations"),
  opacity = 0.7,
  title = "Gracilaria Density"
)

# Show the map
map



