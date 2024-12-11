# **MAIN_WORKFLOW.R**
# This script orchestrates the entire data analysis workflow by sequentially running individual scripts.
# Each script performs a specific task and is designed to work modularly for easy updates or debugging.

# Clear the environment to ensure no conflicts between scripts

# Set the working directory to the folder where all scripts are located
# Modify the path below as per your directory structure
# setwd("path/to/your/working/directory")

# **Step 1: Load necessary libraries**
# Ensure all required libraries are installed and loaded
required_packages <- c("dplyr", "sf", "raster", "geosphere", "ggplot2", "shiny", "leaflet")
lapply(required_packages, require, character.only = TRUE)

# **Step 2: Run the data-reading script**
# Script: READ_DATA.r
# This script reads and processes raw data from BIIGLE annotations and GARMIN GPS.

message("Running READ_DATA.r...")
source("READ_DATA.r")  # Execute the script

# **Step 3: Run the substrate interpolation script**
# Script: SUBSTRATE_INTERPOLATION.r
# This script performs spatial interpolations to map substrate types (e.g., rock, sand) across the bay.

message("Running SUBSTRATE_INTERPOLATION.r...")
source("SUBSTRATE_INTERPOLATION.r")  # Execute the script

# **Step 4: Run the species occurrence mapping script**
# Script: SPECIES_MAPS.r
# This script creates occurrence maps for various species observed in the bay.

message("Running SPECIES_MAPS.r...")
source("SPECIES_MAPS.r")  # Execute the script

# **Step 5: Display maps interactively using shiny**
message("Launching interactive map viewer...")

# Define UI for displaying species maps
ui <- fluidPage(
  titlePanel("Species and Substrate Distribution Maps"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Select a Map to View"),
      selectInput(
        inputId = "species_map",
        label = "Species or Substrate:",
        choices = c(
          "Substrate Map" = "substrate_map",
          "Leurocyclus tuberculosus" = "map_Leurocyclustuberculosus",
          "Carcinus maenas" = "map_Carcinusmaenas",
          "Peltarion spinulosum" = "map_Peltarionspinulosum",
          "Bony fishes" = "map_Bonyfishes",
          "Antholoba achates" = "map_Antholobaachates",
          "Odontocymbiola magellanica" = "map_Odontocymbiolamagellanica",
          "Metridium" = "map_Metridium",
          "Sea stars" = "map_Seastars",
          "Polyzoa opuntia" = "map_Polyzoaopuntia",
          "Macrocystis pyrifera" = "map_Macrocystispyrifera",
          "Undaria pyrifera" = "map_Undariapinnatifida",
          "Gracilaria" = "map_gracilaria",
          "Anasterias antarctica" = "map_Anasteriasantarctica"
        )
      )
    ),
    mainPanel(
      leafletOutput("map_output", height = "600px")
    )
  )
)

# Define server logic for rendering the selected map
server <- function(input, output) {
  
  # Render the selected map
  output$map_output <- renderLeaflet({
    eval(parse(text = input$species_map))  # Dynamically render the selected map
  })
}

# Run the shiny app in a blocking mode
shinyApp(ui = ui, server = server)

