
library(raster)
library(leaflet)
library(htmlwidgets)
library(readr)
library(rgdal)

dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR/UAE_boundary"
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
plot(shp_UAE)


#load PM2.5 stations in the UAE
stations_PM25 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_with_met/PM25_sites.csv")


# load coordinates of the NCMS monitoring stations:
NCMS_STATIONS <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_with_met/met_stations_unique.csv") 



map <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addPolygons(stroke = TRUE, data = shp_UAE, fillOpacity = 0,
              weight = 3, color = "#000000",
              group = "ME")  %>%
  
  addCircleMarkers(data = NCMS_STATIONS,
                   lng = ~ longitude, lat = ~ latitude,
                   radius = 5, stroke = FALSE, fillOpacity = 1, popup = ~ station,
                   color = "red",
                   group = "sites_NCMS") %>%
  
    addCircleMarkers(data = stations_PM25,
                   lng = ~ Longitude, lat = ~ Latitude,
                   radius = 5, stroke = FALSE, fillOpacity = 0.5, popup = ~ Site,
                   color = "blue",
                   group = "PM25") %>%
  # addMarkers(data = NCMS_STATIONS_COORDS_selected , lng = ~ longitude, lat = ~ latitude,
  #            popup = ~ station, group = "sites_NCMS_selected") %>%

  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Topographical", "Satellite"),
    overlayGroups = c("sites_NCMS",  "PM25"),
    options = layersControlOptions(collapsed = TRUE)) # %>%
#  hideGroup(c("sites_NCMS")) 

map



# save map
saveWidget(map, "sites_PM_NCSM.html", selfcontained = FALSE)
