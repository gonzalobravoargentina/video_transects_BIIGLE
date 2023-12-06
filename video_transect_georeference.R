
#Scrip:
# Read video annotations from Biigle Software 
# Read GPX track from GARMIN GPS used for video transects
# Merge GPS position with video annotations using time 


#READ VIDEO ANNOTATIONS-------
# Source https://biigle.de/projects/477

#Set forlders with data
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
#MERGE gpx Track and annotations
for (i in 1:length(track$timeLOCAL_corrected)){
  isbewteen<-between(video.annotations.Paralenz$timeLOCAL, track$timeLOCAL_corrected[i], track$timeLOCAL_corrected[i]+5)
  video.annotations.Paralenz$GPSLongitude[isbewteen ]<-track$lon[i]
  video.annotations.Paralenz$GPSLatitude[isbewteen ]<-track$lat[i]
  video.annotations.Paralenz$timegpsUTC[isbewteen ]<-track$f.time[i]
}


library(dplyr)
# Create a new dataframe with the count of each category per second and keep the other columns
video.annotations.Paralenz_count <- video.annotations.Paralenz %>%
  mutate(truncated_time = trunc(timeLOCAL, "secs")) %>%
  add_count(truncated_time, label_name, name = "count") %>%
  distinct(truncated_time, label_name, .keep_all = TRUE)


#MAPS-----
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
leaflet(subset(video.annotations.Paralenz,label_name=="Leurocyclus tuberculosus")) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,radius = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)

#See rock in transect 
leaflet(subset(video.annotations.Paralenz,label_name=="Rock")) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,radius = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)


#DENSITY MAPS-----
#See Gracilaria in transect, in the case of gracilaria we have from 1 to 5 annotation per frame indicating density. 

library(dplyr)
library(leaflet)

gracilaria_data <- subset(video.annotations.Paralenz_count,label_name=="Gracilaria")

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
write.csv(video.annotations.Paralenz_count, file = "Data.csv")

