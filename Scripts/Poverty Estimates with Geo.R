#Poverty estimates, but with geography

library(dplyr)
library(tidycensus)
library(tidyverse)
library(mapview)
library(leaflet)
library(geojsonio)
library(sf)
library(RColorBrewer)
library(leafem)

### Under 125, for mapping ###

Counties <- read.csv("Data/SACount.csv")
colnames(Counties)[1] <- "GEOID"

TXPoverty <- get_acs(geography = "county",
                     table = "C17002",
                     survey = "acs5",
                     year = 2017,
                     state = "TX",
                     output = "tidy",
                     geometry = TRUE
)

inSA <- subset(TXPoverty, GEOID %in% Counties$GEOID)%>%
  merge(Counties,by="GEOID") %>%
  mutate(CAPS = (County_Name = toupper(County_Name)))

SumUnder125 <- inSA %>%
  filter(variable == "C17002_002" | variable == "C17002_003" | variable == "C17002_004") %>%
  group_by(GEOID) %>%
  mutate(Under125 = sum(estimate), 
         MOE = moe_sum(moe,estimate)) %>%
  filter(variable == "C17002_002") %>%
  select(-estimate, -moe, -variable, -FIPS) %>%
  st_transform(crs = "+init=epsg:4326")

palette_rev <- rev(brewer.pal(10, "PRGn"))
mypal = colorQuantile(palette = palette_rev, domain = SumUnder125$Under125, n = 10)
popup = paste0("Name: ", SumUnder125$County_Name, "<br>", "# of people under 125% FPL: ", SumUnder125$Under125, " +/- ", round(SumUnder125$MOE,2))

leaflet(options = leafletOptions(zoomSnap = 0)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = SumUnder125, 
              fillColor = ~mypal(SumUnder125$Under125), 
              color = "#b2aeae", fillOpacity = 0.7, weight = 1, smoothFactor = 0.2, 
              popup = popup,
              label = SumUnder125$Under125,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "20px",
                direction = "auto")) %>%
  addLegend(pal = mypal, values = SumUnder125$Under125, position = "bottomleft",title = "Number Under 125%") %>%
  addStaticLabels(SumUnder125, label = SumUnder125$CAPS, noHide = F, textsize = "6px")