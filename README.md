# Video Transects Data Processing

This script ![Full_code.R](https://github.com/gonzalobravoargentina/video_transects_BIIGLE/blob/master/Full_code.R) is designed to process data collected during video transects by integrating video annotations from the BIIGLE software with GPS tracking data from a GARMIN GPS device. The primary goal is to merge the GPS coordinates with the corresponding video annotations based on their timestamps, providing a georeferenced record of the observed events and calculating species densities based on substrate types.

## Key Steps:

### 1. **Read Video Annotations from BIIGLE Software 🎥**
The script begins by importing video annotation data exported from BIIGLE. BIIGLE is an online platform used for annotating video footage, where specific events such as fish sightings, bottom types, invertebrates, and algae are labeled with precise timestamps. This step ensures that all annotations made during the video survey are available for further processing.

### 2. **Read GPX Track from GARMIN GPS 📡**
The script reads the GPX track data recorded by the GARMIN GPS device used during the video transects. The GPX (GPS Exchange Format) file contains a series of geographic coordinates (latitude and longitude) along with timestamps. This data represents the path followed by the tow boat and the diver during the survey.

### 3. **Merge GPS Position with Video Annotations Using Time ⏱️**
This step involves synchronizing the video annotations with the GPS data based on their timestamps. By matching the times in both datasets, the script can accurately assign geographic coordinates to each annotated event in the video. This merging process is crucial for creating a georeferenced dataset that maps observed events to specific locations on the seafloor.

### 4. **Calculate Species Density 📊**
Based on the annotation counts and the area for each substrate type, the script calculates the density of species per substrate. It then computes the density per 1000 m² to standardize the results.

### 5. **Calculate Overall Density 🌍**
The script also calculates the overall species density across all substrate types by aggregating the counts and area for each label name.

### 6. **Combine Specific and Overall Densities 🔀**
The final output combines substrate-specific densities and overall densities into one dataset, allowing for comparisons between different substrate types and across all transects.

### 7. **Export Results 📤**
Finally, the script outputs the summarized density data to a CSV file, which includes the species count, density, and density per 1000 m² for each substrate type and overall.

## Example Map

Here is an example of a map showing the locations of the video transects and corresponding annotations:

![](https://github.com/gonzalobravoargentina/video_transects_BIIGLE/blob/master/Map_example.png)

---



