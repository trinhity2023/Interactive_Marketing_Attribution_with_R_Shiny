library(leaflet)
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)

# Load data and filter to necessary columns
data <- read.csv("starbucks_data.csv")
data <- data %>%
  select(`latitude`, `longitude`,`dba`,`store_name`, `address`, `city`, `state`, `zip_code`)

# Add a new column to the dataset indicating whether each location should be red
data$color <- ifelse(data$store_name == "Proposed New Starbucks Location", "orange", "#00704A")

# Create map
map2 <- leaflet(height = 800, width = 1000) %>%
  addProviderTiles("CartoDB.Voyager") %>%
  # Add circle markers for store locations
  addCircleMarkers(
    data = data,
    radius = 4,
    fillColor = data$color,
    color = "white",
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 2,
    labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "auto"),
    popup = paste(data$dba, "<br>",
                  data$store_name, "<br>",
                  data$address, "<br>",
                  data$city, 
                  data$state, 
                  data$zip_code, "<br>")
  ) %>% # Add circle markers with multiple columns in popup
  # Add a heatmap layer to show the density of stores
  addHeatmap(
    data = data,
    lng = ~longitude,
    lat = ~latitude,
    blur = 20,
    max = 0.5,
    radius = 8
  ) %>% 
  setView(lng = -75.1652, lat = 39.9526, zoom = 12)

map2 <- addCircleMarkers(map2, 
                         lng = -75.1800, 
                         lat = 39.9528, 
                         color = "orange", 
                         fillColor = "orange", # Set the fillColor to red
                         radius = 8, 
                         popup = "<b>Proposed New Starbucks Location:</b> <br>2400 Chestnut",
                         options = markerOptions(clickable = TRUE, 
                                                 title = "Proposed New Starbucks Location",
                                                 opacity = 1))

map2


