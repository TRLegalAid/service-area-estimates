#Mapping SAR Closed Cases with Income Eligible Population

library(dplyr)
library(tidycensus)
library(tidyverse)
library(xlsx)
library(readr)
library(readxl)
library(tigris)
library(leaflet)

counts <- read_excel("./Data/TAJF SAR 2020 Cases Closed by County.xlsx") %>%
  select(c(1,4))

Counties <- read.csv("../SACount.csv") %>%
  rename(GEOID = 1)

SA_join <- Counties %>%
  left_join(counts, by = c("County_Name" = "County")) %>%
  mutate(County_Name = str_to_upper(County_Name),
         GEOID = as.character(GEOID))

TXcounties <- counties(state = "TX", cb = FALSE)

#Under200
TXPoverty <- get_acs(geography = "county",
                      table = "C17002",
                      survey = "acs5",
                      year = 2018,
                      state = "TX",
                      output = "tidy",
                      geometry = FALSE
)

inSA <- subset(TXPoverty, GEOID %in% Counties$GEOID)

SumUnder200 <- inSA %>%
  filter(variable == "C17002_002" | variable == "C17002_003" | variable == "C17002_004" | variable == "C17002_005"
         | variable == "C17002_006" | variable == "C17002_007") %>%
  group_by(GEOID) %>%
  mutate(Under200 = sum(estimate),
         Under200MOE = moe_sum(moe,estimate),
         Perc_MOE = Under200MOE/Under200) %>%
  filter(variable == "C17002_002") %>%
  select(-estimate, -moe, -NAME)

SA_join$Total[SA_join$County_Name == "FRIO"] <- 50

SA_join_200 <- SA_join %>%
  left_join(SumUnder200) %>%
  mutate(Cases_Per = Total/Under200*10000)

# write_excel_csv(SA_join_200, "./Output/Stats_SA.csv")


all_merged <- geo_join(TXcounties, SA_join_200, "GEOID","GEOID") %>%
  filter(GEOID %in% inSA$GEOID)

pal_per <- colorQuantile("YlOrRd", domain = all_merged$Cases_Per)
pal <- colorNumeric("YlOrRd", domain = all_merged$Total)


map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = all_merged,
              fillColor = ~pal(Total),
              weight = .5,
              fillOpacity = .8,
              smoothFactor = .8) %>%
  addLegend(position = "topright",
            pal = pal,
            values = all_merged$Total,
            title = "Closed cases")

map

map2 <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = all_merged,
              fillColor = ~pal_per(Cases_Per),
              weight = .5,
              fillOpacity = .8,
              smoothFactor = .8) 
map2

