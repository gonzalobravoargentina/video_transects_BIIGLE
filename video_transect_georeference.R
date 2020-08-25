
#Scrip:
# Read video annotations from Biiglr
# Read GPX track from the video transects
# Georeference video annotations


#READ VIDEO ANNOTATIONS-------
# Source https://biigle.de/projects/477


#Set the working directory to the folder "Biigle annotations" and read MOV_0001_GB.csv
library(tcltk)
setwd(tk_choose.dir(getwd(), "Choose a suitable folder"))
video.annotations <- read.csv("MOV_0001_GB.csv")

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

#use original video name as in Paralenz log 
video.annotations$video_name <- "MOV_0001"


# READ .GPX -------------------
# GPS MODEL: 
# recorded one point each 3 secs
#Choose as WD folder GPX_files
library(tcltk)
setwd(tk_choose.dir(getwd(), "Choose a suitable folder"))
#in this example we have only one .gpx file , however the code is designed for a list of .gpx
filesGPX <- list.files(pattern = "*.gpx")#get a list of files .gpx in wd

#read gpx files and store them all in a list
library(rgdal)
gpx <- lapply(setNames(filesGPX, make.names(gsub("*.gpx$", "",filesGPX))), 
              function(f) { readOGR(dsn=f, layer="track_points") }
)

#create a list with coordinates and time from all de gpx files
gpxlist <- lapply(gpx,function(f) { data.frame(f@coords,f$time) })

#Create one DATAFRAME with the gps data 
library(purrr)
track <- dplyr::bind_rows(gpxlist)


#As the GPX files are in UTC time, we must covert to the local time as the photos
library(lubridate)
track$timeUTC <- strptime(track$f.time,format = "%Y/%m/%d %H:%M:%OS",tz="UTC")

#local time
track$timeLOCAL <- force_tzs(track$timeUTC, "UTC", tzone_out = "America/Argentina/Catamarca", roll = FALSE)

colnames(track) <- c("lon","lat","f.time","timeUTC","timeLOCAL")

#PLOT the track in map
library(leaflet)
leaflet(track) %>% addTiles() %>%
  addCircleMarkers(~ lon, ~ lat, radius = 2)%>%
  addProviderTiles("Esri.WorldImagery") 



#READ PARALENZ LOG------ 
library(plyr)
library(readr)
#get a list of CSV files
setwd(tk_choose.dir(getwd(), "Choose a suitable folder"))
filesPARALENZ <- list.files(pattern = "*.CSV",full.names=TRUE)

#Read all CSV files an import all in one dataframe
PARALENZ = ldply(filesPARALENZ, read_csv)

#Transforme time column to POSIXlt (local time Argentina)
PARALENZ$timeLOCAL <- strptime(PARALENZ$Time, "%Y:%m:%d %H:%M:%S")

#Transforme depth column into numeric 
PARALENZ$Depth <- as.numeric(PARALENZ$Depth)

#Eliminate NA rows in column Image/video-file
PARALENZ <- na.omit(PARALENZ, cols = "Image/video-file")

#created column videos as factor
PARALENZ$video <- as.factor(PARALENZ$`Image/video-file`)

#add columns with seconds of video (each video of ~10 min = 60 secs)
library(dplyr)
PARALENZ <- PARALENZ %>% group_by(video) %>% mutate(frame.secs = seq_len(n()))

colnames(PARALENZ)[4] <- "video_name"



#MERGE PARALENZ log and Biigle annotations
#select the video you want to merge 
PARALENZ_MOV_0001 <- subset(PARALENZ,video_name=="MOV_0001")
#loop for merging PARALENZ AND METADATA using a time between +- 3 secs
for (i in 1:length(PARALENZ_MOV_0001$frame.secs)){
  isbewteen<-between(video.annotations$frame.secs, PARALENZ_MOV_0001$frame.secs[i], PARALENZ_MOV_0001$frame.secs[i]+3)
  video.annotations$video_name.PARALENZ[isbewteen ]<-PARALENZ_MOV_0001$video_name[i]
  video.annotations$depth.PARALENZ[isbewteen ]<-PARALENZ_MOV_0001$Depth[i]
  video.annotations$Temp.PARALENZ[isbewteen ]<-PARALENZ_MOV_0001$Temperature[i]
  video.annotations$dive.time.PARALENZ[isbewteen ]<-PARALENZ_MOV_0001$Time[i]
}

#Transform to time format 
video.annotations$dive.time.PARALENZ <- strptime(video.annotations$dive.time.PARALENZ, "%Y:%m:%d %H:%M:%S")


#MERGE gpx Track and annotations
for (i in 1:length(track$timeLOCAL)){
  isbewteen<-between(video.annotations$dive.time.PARALENZ, track$timeLOCAL[i], track$timeLOCAL[i]+5)
  video.annotations$GPSLongitude[isbewteen ]<-track$lon[i]
  video.annotations$GPSLatitude[isbewteen ]<-track$lat[i]
  video.annotations$timegpsUTC[isbewteen ]<-track$f.time[i]
}


#view photos in a map
#Point of map center
x1 <- -65.809794
y1 <- -45.020866
#color gradiente for depth
library(leaflet)
leaflet(video.annotations) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,radius = 1)%>%
addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)


#gracilaria
leaflet(subset(video.annotations,label_name=="Gracilaria")) %>% addTiles() %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, popup = ~label_name,radius = 1)%>%
  addProviderTiles("Esri.WorldImagery") %>%  
  setView(lng = x1, lat = y1, zoom = 15)


