#map3.R
library(leaflet)
library(yelpr)
library(leaflet.extras)
library(dplyr)
library(htmltools)
library(purrr)

# Set search parameters
search_categories <- "coffee,tea"
search_limit <- 50
total_results <- 1000

# Set Yelp API key
api_key <- "your api key"

# Set vector of cities to search for
cities <- c("PA", "NY","NJ")

# Initialize list to store business data
yelp_businesses <- list()

# Call Yelp API for each city
for (city in cities) {
  # Call Yelp API with offsets
  for (offset in seq(0, total_results - search_limit, by = search_limit)) {
    yelp_results <- business_search(api_key = api_key,
                                    categories = search_categories,
                                    location = city,
                                    limit = search_limit,
                                    offset = offset)
    yelp_businesses[[city]][[offset / search_limit + 1]] <- yelp_results$businesses
  }
  
  # Combine the results from all API calls for this city
  yelp_businesses[[city]] <- bind_rows(yelp_businesses[[city]])
  
  # Filter businesses with rating of 3.5 and above and exclude Starbucks
  yelp_businesses[[city]] <- yelp_businesses[[city]][!is.na(yelp_businesses[[city]]$rating) & yelp_businesses[[city]]$rating >= 3.5, ]
  yelp_businesses[[city]] <- yelp_businesses[[city]][!grepl("(?i)starbucks", yelp_businesses[[city]]$name), ]
}

# Combine the results from all cities
yelp_businesses <- bind_rows(yelp_businesses)

yelp_businesses <- yelp_businesses[!is.na(yelp_businesses$rating) & yelp_businesses$rating >= 3.5, ]
# Exclude businesses with case-insensitive names containing "Starbucks"
yelp_businesses <- yelp_businesses[!grepl("(?i)starbucks", yelp_businesses$name), ]

# Check if all results are unique
if (any(duplicated(yelp_businesses))) {
  print("There are duplicated results in the data frame.")
} else {
  print("All results in the data frame are unique.")
}

# Load Starbucks data and filter to necessary columns
starbucks_data <- read.csv("starbucks_data.csv")
starbucks_data <- starbucks_data %>%select(`latitude`, `longitude`,`dba`,`store_name`, `address`, `city`, `state`, `zip_code`)

# Add a new column to the dataset indicating whether each location should be red
starbucks_data$color <- ifelse(starbucks_data$store_name == "Proposed New Starbucks Location", "orange", "#00704A")

# Create a new map object
map3 <- leaflet(height = 800, width = 1000) %>%
  addProviderTiles("CartoDB.Voyager")  %>%
  setView(lng = -75.1652, lat = 39.9526, zoom = 12)%>% # Add circle markers with multiple columns in popup
  # Add a heatmap layer to show the density of stores
  addHeatmap(
    data = starbucks_data,
    lng = ~longitude,
    lat = ~latitude,
    blur = 20,
    max = 0.5,
    radius = 8
  ) %>%
  # Add note
  addControl(
    html = "<b>Since Starbucks Yelp rating is 3.5, Starbucks Competitors are defined as businesses that serve Coffee & Tea rated 3.5+.</b>",
    position = "bottomleft"
  ) 

# Add circle markers for Starbucks locations
# Add steelblue circle markers for Yelp businesses
map3 <- map3 %>%
  addCircleMarkers(
    data = yelp_businesses,
    lat = ~coordinates$latitude,
    lng = ~coordinates$longitude,
    fillColor='#5DA5DA',
    radius = 4,
    color='white',
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 2,
    popup = ~paste("<strong>Starbucks Competition </strong>","<br>",
                   "<strong>Name:</strong>", name, "<br>",
                   "<strong>Address:</strong>", location$address1, "<br>",
                   "<strong>Rating:</strong>", rating, "<br>",
                   "<strong>Price:</strong>", price, "<br>",
                   "<strong>Categories:</strong>", categories[[1]]$title,",",categories[[2]]$title,",",categories[[3]]$title, "<br>",
                   "<a href=", url, " target='_blank'>", "View on Yelp", "</a>"))

# Add circle markers for Starbucks locations
map3 <- map3 %>%
  addCircleMarkers(
    data = starbucks_data,
    radius = 4,
    fillColor = starbucks_data$color,
    color = "white",
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 2,
    popup = paste(starbucks_data$dba, "<br>",
                  starbucks_data$store_name, "<br>",
                  starbucks_data$address, "<br>",
                  starbucks_data$city, 
                  starbucks_data$state, 
                  starbucks_data$zip_code, "<br>")
  )

map3 <- addCircleMarkers(map3, 
                         lng = -75.1800, 
                         lat = 39.9528, 
                         color = "orange", 
                         fillColor = "orange", # Set the fillColor to red
                         radius = 8, 
                         popup = "<b>Proposed New Starbucks Location:</b> <br>2400 Chestnut",
                         options = markerOptions(clickable = TRUE, 
                                                 title = "Proposed New Starbucks Location",
                                                 opacity = 1))
# Add a legend to the map
map3 <- addLegend(
  map = map3,
  position = "topright",
  colors = c("#00704A", "orange", "#5DA5DA"),
  labels = c("Current Starbucks Locations as of 3/1/23", "Proposed New Starbucks Location", "Starbucks Competitors in PA,NY,NJ"),
  opacity = 1
)

# Show the map
map3
