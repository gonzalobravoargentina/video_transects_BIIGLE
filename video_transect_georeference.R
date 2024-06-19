
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


#separeted labels 
# Separate the values into different columns
library(tidyr)
video.annotations <- video.annotations %>%
  separate(label_hierarchy, into = c("CATAMI_TYPE", "CATAMI_GROUP", "CATAMI_DISPLAY_NAME"), sep = " > ", extra = "merge")


library(dplyr)
# Add the Substrate_type column
video.annotations <- video.annotations %>%
  mutate(Substrate_type = case_when(
    label_name == "Ripples (<10cm height)" ~ 8,
    label_name == "Rock" ~ 7,
    label_name == "Cobbles" ~ 6,
    label_name == "Pebble (10-64mm)" ~ 5,
    label_name == "Gravel (2-10mm)" ~ 4,
    label_name == "Fine sand (no shell fragments)" ~ 3,
    label_name == "Coarse sand (with shell fragments)" ~ 2,
    label_name == "Bioturbated" ~ 1,
    TRUE ~ NA_integer_
  )) %>%
  select(video_annotation_label_id, label_id,label_name,CATAMI_TYPE, CATAMI_GROUP, CATAMI_DISPLAY_NAME, Substrate_type, everything())  # Move Substrate_type after CATAMI_DISPLAY_NAME

# READ .GPX -------------------
# GPS MODEL: GARMIN
# recorded one point each 5 secs

#Option 1------
#get a list of all the CSV files in the "GPS" folder
filesGPX <- list.files(path = "GPX files", pattern = "*.gpx", full.names = TRUE)

#read gpx files and store them all in a list
library(rgdal)
gpx <- lapply(setNames(filesGPX, make.names(gsub("*.gpx$", "",filesGPX))), 
              function(f) { readOGR(dsn=f, layer="track_points") }
)

#create a list with coordinates and time from all de gpx files
gpxlist <- lapply(gpx,function(f) { data.frame(f@coords,f$time) })


#Option 2------
library(sp)
# Get a list of all GPX files in the "GPX files" folder
filesGPX <- list.files(path = "GPX files", pattern = "*.gpx", full.names = TRUE)

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


# As the GPS is in the boat, the diver past 10 sec later so we Adjust the GPS position by 10 seconds
track$timeLOCAL_corrected <- track$timeLOCAL + 10


#PLOT the track in map
library(leaflet)
leaflet(track) %>% addTiles() %>%
  addCircles(~ lon, ~ lat, radius = 1,color = "orange",stroke = FALSE)%>%
  addProviderTiles("Esri.WorldImagery") 




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


#Merge GPS position with video annotations using time------
#MERGE gpx Track and annotations (without layback)
#for (i in 1:length(track$timeLOCAL_corrected)){
#  isbewteen<-between(video.annotations.Paralenz$timeLOCAL, track$timeLOCAL_corrected[i], track$timeLOCAL_corrected[i]+5)
#  video.annotations.Paralenz$GPSLongitude[isbewteen ]<-track$lon[i]
#  video.annotations.Paralenz$GPSLatitude[isbewteen ]<-track$lat[i]
#  video.annotations.Paralenz$timegpsUTC[isbewteen ]<-track$f.time[i]
#}

# Merge GPS position with video annotations using time
# MERGE gpx Track and annotations with a layback 
layback_seconds <- 30  # Layback in secs

for (i in 1:length(track$timeLOCAL_corrected)) {
  is_between <- between(video.annotations.Paralenz$timeLOCAL, 
                        track$timeLOCAL_corrected[i] - layback_seconds, 
                        track$timeLOCAL_corrected[i] + 5)
  
  video.annotations.Paralenz$GPSLongitude[is_between] <- track$lon[i]
  video.annotations.Paralenz$GPSLatitude[is_between] <- track$lat[i]
  video.annotations.Paralenz$timegpsUTC[is_between] <- track$f.time[i]
}


library(dplyr)
# Create a new dataframe with the count of each category per second and keep the other columns
video.annotations.Paralenz_count <- video.annotations.Paralenz %>%
  mutate(truncated_time = trunc(timeLOCAL, "secs")) %>%
  add_count(truncated_time, label_name, name = "count") %>%
  distinct(truncated_time, label_name, .keep_all = TRUE)


library(dplyr)
video.annotations.Paralenz_count <- video.annotations.Paralenz_count %>%
  mutate(label_name = case_when(
    label_name == "Green" ~ "Macroalgae:Filamentous/filiform:Green",
    label_name == "Filamentous / filiform" ~ "Macroalgae:Filamentous/filiform",
    label_name == "Benthic" ~ "Echinoderms:Sea cucumbers",
    label_name == "Simple" ~ "Sponges:Massiveforms:Simple",
    label_name == "Solitary" ~ "Ascidians:Unstalked:Solitary",
    label_name == "Encrusting" ~ "Sponges:Crusts:Encrusting",
    TRUE ~ as.character(label_name)  # Mantener cualquier otro valor como está
  ))
#write.csv(video.annotations.Paralenz_count,"Counts.csv")


Check_list <- video.annotations.Paralenz_count %>%
  distinct(label_name, CATAMI_TYPE) %>%
  arrange(CATAMI_TYPE, label_name)


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

#See rock in transect 
leaflet(subset(video.annotations.Paralenz,label_name=="Ripples (<10cm height)")) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,radius = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)



#BOTTOM TYPE MAPS------

x1 <- -65.809794
y1 <- -45.020866

#Color for video Name 
#set colors for videos names:
video.annotations.Paralenz_bottomtype <- subset(video.annotations.Paralenz, CATAMI_GROUP=="Substrate")
pal <- colorFactor(
  palette = 'Set3',
  domain = video.annotations.Paralenz_bottomtype$label_name
)

library(leaflet)
leaflet(video.annotations.Paralenz_bottomtype) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,color = ~pal(label_name),radius = 8, fillOpacity = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)



#DENSITY MAPS-----
#See Gracilaria in transect, in the case of gracilaria we have from 1 to 5 annotation per frame indicating density. 

library(dplyr)
library(leaflet)

gracilaria_data <- subset(video.annotations.Paralenz_count,label_name=="Gracilaria")

#write.csv(gracilaria_data,"gracilaria_data.csv")

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


#Save data in a csv file.
#write.csv(video.annotations.Paralenz_count, file = "Data.csv")

