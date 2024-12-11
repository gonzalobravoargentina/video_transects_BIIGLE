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

#10_bis Undaria pinnatifida
map_Undariapinnatifida <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  setView(lng = mean(bbox(bahia_arredondo)[1, ]),
          lat = mean(bbox(bahia_arredondo)[2, ]),
          zoom = 14.5) %>%
  addRasterImage(raster_sinroca, colors = color_palette_sinroca, opacity = 1) %>%
  addRasterImage(raster_roca, colors = color_palette_roca, opacity = 1) %>%
  addCircleMarkers(data = subset(video.annotations.Paralenz, label_name == "Undaria pinnatifida"),
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

