library(leaflet)
library(googlesheets)
library(dplyr)
library(ggmap)


#grab the data
GAP_URL <- "https://docs.google.com/spreadsheets/d/1vl1MlIZP2i87TFvSVDUY1zkRgmSfe-_-Ae_I3pr-cQA/edit#gid=2728"
third_party_gap <- GAP_URL %>%
  gs_url()

turnout_data <- third_party_gap %>% 
  gs_read(ws = "WomensMarch")

#geocode 

turnout_data <- turnout_data %>% 
  mutate(march_location = paste(CityTown, " ", StateTerritory, sep = ""))

register_google(key = "AIzaSyA8PiqLPNxvr25Z0lPp0DCMRyiwNfn2rYY")

for(i in 1:nrow(turnout_data)){
  result <- geocode(turnout_data$march_location[i], output = "latlona", source = "google")
  turnout_data$lon[i] <- as.numeric(result[1])
  turnout_data$lat[i] <- as.numeric(result[2])
  turnout_data$geoAddress[i] <- as.character(result[3])
}

turnout_data_na = turnout_data[complete.cases(turnout_data[,1]),]
turnout_data_na = turnout_data_na[-c(335),]

for(i in 1:nrow(turnout_data_na)){
  if(is.na(turnout_data_na$EstimateLow[i])){
    turnout_data_na$popup[i] = paste0("This march was in ", turnout_data_na$march_location[i], " and was put on by ", turnout_data_na$Actor[i], ".", " The organizers and event description can be found ", paste0('<a href="',turnout_data_na$Source1[i],'">here</a>'))
    
  }
  else{
    turnout_data_na$popup[i] = paste0("This march was in ", turnout_data_na$march_location[i], " and was put on by ", turnout_data_na$Actor[i], ". It had an average estimate of ", as.character(round(sum(turnout_data_na$EstimateLow[i],turnout_data_na$EstimateHigh[i])/2)), " protestors. The organizers and event description can be found ", paste0('<a href="',turnout_data_na$Source1[i],'">here</a>'))
  }
  if(is.na(turnout_data_na$Source1[i])){
    turnout_data_na$popup[i] = paste0("This march was in ", turnout_data_na$march_location[i], " and was put on by ", turnout_data_na$Actor[i], ".")
    
  }
}

#now let's map the data

m <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  states$name, states$density
) %>% lapply(htmltools::HTML)

mapping_two <- leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = turnout_data_na$lon[1], lat = turnout_data_na$lat[1], popup=turnout_data_na$popup[1])
  

for(i in c(1:length(turnout_data_na))){
    addMarkers(lng = turnout_data_na$lon[i], lat = turnout_data_na$lat[i], popup=turnout_data_na$popup[i])
}
  
  
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=turnout_data_na$lon, lat=turnout_data_na$lat, popup=turnout_data_na$popup)
m
library(htmlwidgets)
saveWidget(m, 'Womens_march.html', selfcontained = FALSE)

tempDir <- tempfile()
dir.create(tempDir)
htmlFile <- file.path(tempDir, "index.html")
# (code to write some content to the file)

viewer <- getOption("viewer")
viewer(htmlFile)
