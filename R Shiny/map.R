#map.R
library(leaflet)
library(dplyr)
library(sf)
library(readr)

# Load data and filter to necessary columns
data <- read.csv("starbucks_data.csv")
data <- data %>%
  select(`latitude`, `longitude`, `dba`, `store_name`, `address`, `address_line_2`, `city`, `state`, `zip_code`, `store_hours`)

# Convert to sf object
data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

# Create map
map <- leaflet(data_sf) %>%
  addTiles() %>% 
  addCircleMarkers(radius = 5, color = "white", fillColor = "green", fillOpacity = 0.8, popup =  paste(data$dba, "<br>",
                                                                                                       data$store_name, "<br>",
                                                                                                       data$address, "<br>",
                                                                                                       data$city, 
                                                                                                       data$state, 
                                                                                                       data$zip_code, "<br>")) %>% # Add circle markers with multiple columns in popup
  setView(lng = -75.1652, lat = 39.9526, zoom = 12) 

# Export the map object
map
