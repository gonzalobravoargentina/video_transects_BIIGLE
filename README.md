# Script Overview

This script performs the following tasks:

## Read Video Annotations
- Reads video annotations from Biigle Software.

## Read GPX Track
- Reads GPX track from GARMIN GPS used for video transects.

## Merge GPS Position with Video Annotations
- Merges GPS position with video annotations using time.

## Maps and Visualization
- Plots the GPS track on a map using leaflet.
- Adjusts the GPS position by 10 seconds to account for diver movement.
- Creates maps highlighting specific annotations like crabs, rocks, and Gracilaria with varying circle sizes and colors based on annotation density.

## Save Data
- Saves the merged data in a CSV file named "Data.csv".

### Usage
1. Run the script to load and process the data.
2. Explore the generated maps for video and annotation visualization.
3. The merged data is saved in "Data.csv" for further analysis.

### Dependencies
- R packages: readr, jsonlite, rgdal, purrr, lubridate, leaflet, plyr, dplyr.

![](https://github.com/gonzalobravoargentina/video_transects_BIIGLE/blob/master/Map_example.png)
