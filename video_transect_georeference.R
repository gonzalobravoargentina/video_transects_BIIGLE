
#Scrip:
# Read video annotations from Biigle Software 
# Read GPX track from GARMIN GPS used for video transects
# Merge GPS position with video annotations using time 


#READ VIDEO ANNOTATIONS-------
# Source https://biigle.de/projects/477

#Read file in Biigle annotations folder 
setwd(paste0(getwd(),"/Biigle annotations"))
video.annotations <- read.csv("MOV_0001_MOV_0002_GB.csv")

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
video.annotations$video_name <- paste0("MOV_",str_sub(video.annotations$video_filename,-11,-8))


# READ .GPX -------------------
# GPS MODEL: GARMIN
# recorded one point each 5 secs
#Choose as WD folder GPX_files
setwd("..")
setwd(paste0(getwd(),"/GPX files"))

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
  addCircles(~ lon, ~ lat, radius = 1,color = "orange",stroke = FALSE)%>%
  addProviderTiles("Esri.WorldImagery") 




#READ PARALENZ LOG------ 
library(plyr)
library(readr)
#Set wd to "Parlenz log  Folder"
setwd("..")
setwd(paste0(getwd(),"/Paralenz log"))


#get a list of all the CSV files
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

#add columns with seconds of video (each video of ~10 min = 60 secs). In log the data is stored each sec. 
library(dplyr)
PARALENZ <- PARALENZ %>% group_by(video) %>% dplyr::mutate(frame.secs = seq_len(n()))

colnames(PARALENZ)[4] <- "video_name"



#MERGE PARALENZ log and Biigle annotations
library(dplyr)
video.annotations.Paralenz <- left_join(video.annotations,PARALENZ , by=c("frame.secs","video_name"))


#Merge GPS position with video annotations using time------
#MERGE gpx Track and annotations
for (i in 1:length(track$timeLOCAL)){
  isbewteen<-between(video.annotations.Paralenz$timeLOCAL, track$timeLOCAL[i], track$timeLOCAL[i]+5)
  video.annotations.Paralenz$GPSLongitude[isbewteen ]<-track$lon[i]
  video.annotations.Paralenz$GPSLatitude[isbewteen ]<-track$lat[i]
  video.annotations.Paralenz$timegpsUTC[isbewteen ]<-track$f.time[i]
}

#set original wd 
setwd("..")



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

#https://www.seascapemodels.org/rstats/2016/11/23/mapping-abundance-photos.html#navbar
