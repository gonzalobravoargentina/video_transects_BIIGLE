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

rm(transect_data,transect_distances,transect_times, end_coord,end_time,start_coord,start_time)

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

