library(readr)
library(readxl)
library(tidyverse)
library(tigris)
library(sf)
library(leaflet)
library(htmltools)
library(tidycensus)

#Read in data that pairs each office with a county
office_counties <- read_excel("../TRLA_Counties_by_Office.xlsx") %>%
  mutate(Office = str_to_upper(Office))

#Not sure what's up with the Sinton office, so just converting that to Corpus
office_counties$Office[office_counties$Office=="SINTON"] <- "CORPUS CHRISTI"

#Read in a list of every county in our service area that includes GEOIDs
service_area <- read.csv("../SACount.csv") %>%
  rename(GEOID = 1)

#List of attorneys , removing irrelevant offices
attys_civil <- read.csv("./Data/Attorneys_6.1.csv") %>%
  filter(!str_detect(Office, "PD") & !str_detect(Office, "NASHVILLE") & !str_detect(Office, "REMOTE")
         & !str_detect(Office, "DALLAS"))

#Additional text changes so that atty list matches county list
attys_civil$Office[attys_civil$Office == "CORPUS COURTHOUSE"] <- "CORPUS CHRISTI"
attys_civil$Office[attys_civil$Office == "CORPUS PUEBLO"] <- "CORPUS CHRISTI"
attys_civil$Office[attys_civil$Office == "MERCEDES" | attys_civil$Office == "HARLINGEN"] <- "BROWNSVILLE"

#read in Census data
poverty_data <- get_acs(geography = "county",
                     table = "C17002",
                     survey = "acs5",
                     year = 2018,
                     state = "TX",
                     output = "tidy",
                     geometry = FALSE
)

poverty_data <- subset(poverty_data, GEOID %in% Counties$GEOID)


poverty_data <- poverty_data %>%
  filter(variable == "C17002_002" | variable == "C17002_003" | variable == "C17002_004" | variable == "C17002_005"
         | variable == "C17002_006" | variable == "C17002_007") %>%
  group_by(GEOID) %>%
  mutate(Under200 = sum(estimate),
         Under200MOE = moe_sum(moe,estimate),
         Perc_MOE = Under200MOE/Under200) %>%
  filter(variable == "C17002_002") %>%
  select(-estimate, -moe) %>%
  mutate(NAME = str_replace(NAME, " County, Texas", ""))


#Calculate poverty population per office area

#Generate count of attorneys by office; join with the office/county matching data
attys_civil_cty <- attys_civil %>%
  group_by(Office) %>%
  summarise(attys = n()) %>%
  full_join(office_counties) %>%
  group_by(Office) %>%
  
  #Then generate number of attorneys in a region divided by number of counties in the region
  mutate(attys_per_cty = attys/n()) %>%
  
  #Join with poverty data; create a measure for attorneys per 10,000 people under 200% FPL
  left_join(poverty_data, by = c("County" = "NAME")) %>%
  group_by(Office) %>%
  mutate(ppl_per_atty_region = round(Under200/attys,2)) %>%
  mutate(Poverty_pop_region = sum(Under200))



#Create a list of this data specifically per office
attys_civil_office <- attys_civil_cty %>%
  distinct(Office, .keep_all = TRUE) %>%
  mutate(ppl_per_atty_region = round(Poverty_pop_region/attys,2))

TRLA_counties <- counties(state = "TX", cb = FALSE) %>%
  subset(GEOID %in% service_area$GEOID) %>%
  geo_join(attys_civil_cty, by_df = "County", by_sp = "NAME") %>%
  mutate(ppl_per_atty_cty = round(Under200/attys,2))

TRLA_regions <- TRLA_counties %>%
  group_by(Office) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  geo_join(attys_civil_office, by_df = "Office", by_sp = "Office")


bins <- c(0, 100, 200, 500, 1000, 10000, 15000, 20000, Inf)

pal <- colorBin("YlOrRd", domain = TRLA_regions$ppl_per_atty_region, bins = 5)
pal2 <- colorBin("YlOrRd", domain = TRLA_counties$ppl_per_atty_cty, bins = bins)

content <- paste("<p><strong>", TRLA_regions$Office, "</strong></p>",
                 "<strong>","Under 200%:", "</strong>", TRLA_regions$Poverty_pop_region, "<br>",
                 "<strong>","TRLA Attorneys:", "</strong>", TRLA_regions$attys, "<br>",
                "<strong>","Eligible clients per attorney:", "</strong>", TRLA_regions$ppl_per_atty_region) %>%
  lapply(htmltools::HTML)

content2 <- paste("<p><strong>", TRLA_counties$NAME, "</strong></p>",
                  "<strong>","Served by:", "</strong>", TRLA_counties$Office, "<br>",
                  "<strong>","TRLA Attorneys:", "</strong>", TRLA_counties$attys, "<br>",
                 "<strong>","Under 200%:", "</strong>", TRLA_counties$Under200, "±", round(TRLA_counties$Under200MOE,2), "<br>",
                 "<strong>","Eligible clients per attorney:", "</strong>", TRLA_counties$ppl_per_atty_cty) %>%
  lapply(htmltools::HTML)


#REGIONS/OFFICES

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = TRLA_counties,
              fill = FALSE,
              weight = 2) %>%
  addPolygons(data = TRLA_regions,
              fillColor = ~pal(TRLA_regions$ppl_per_atty_region),
              weight = 2,
              fillOpacity = .8,
              smoothFactor = .8,
              label = content)

#COUNTIES

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = TRLA_counties,
              fillColor = ~pal2(TRLA_counties$ppl_per_atty_cty),
              weight = .6,
              fillOpacity = .8,
              smoothFactor = .8,
              label = content2) %>%
  addPolygons(data = TRLA_regions,
              weight = 3,
              fill = FALSE)

