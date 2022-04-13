## Adapting Attys_Offices_Counties.R to produce maps with and without law graduates. 
## MARCH 2022 ATTY List 

library(readr)
library(readxl)
library(tidyverse)
library(tigris)
library(sf)
library(leaflet)
library(htmltools)
library(tidycensus) 
library(htmlwidgets)
library(janitor)

source("Scripts/map-functions.R")


# Load office - county pairings
office_county_matches <- read_excel("./Data/TRLA_Counties_by_Office.xlsx") %>%
  clean_names() %>%
  mutate(office = str_to_upper(office))

# Sinton = Corpus Christi
office_county_matches$office[office_county_matches$office=="SINTON"] <- "CORPUS CHRISTI"

# Load list of counties in TRLA service area with GEOIDS
trla_counties <- read.csv("./Data/SACount.csv") %>% clean_names()


# Load latest civil attorney staff list
attys_list_raw <- read.csv("./Data/staff_lists/March 2022 Attorneys List 1.csv") %>% 
  clean_names() %>% 
  rename(office = ofc, job_title = job, employee_name = empnam) %>%
  select(employee_name, office, job_title)

# Perform any cleaning needed for this month's atty list
attys_list <- attys_list_raw %>% 
  
  # Clean up office names
  mutate(
    office = case_when(
      (office == "CORPUS COURTHOUSE" | office == "CORPUS PUEBLO") ~ "CORPUS CHRISTI",
      (office == "MERCEDES" | office == "HARLINGEN") ~ "BROWNSVILLE",
      office == "EL PASO/ NY" ~ "EL PASO",
      office == "DILLEY" ~ "SAN ANTONIO",
      employee_name == "GILBERT, ALEXANDER H." ~ "AUSTIN",
      TRUE ~ office)
  ) %>%
  
  # Filter out offices outside of the 68 county service area
  filter(
    !str_detect(office, "REMOTE") &  !str_detect(office, "NASHVILLE") & 
      !str_detect(office, "DALLAS") & !str_detect(office, "TAJ") & !str_detect(job_title,"PUBLIC DEFENDER")
  )


# Subset to just the civil attorneys who handle cases (exclude law grads) 
attys_list_nlg <- attys_list %>% filter(job_title != "LAW GRADUATE")


## ----------- Load ACS data

# Register for your unique API key here: https://api.census.gov/data/key_signup.html
# Save your key outside of this project directory
key <- read.delim("../credentials/census_api_key.txt", header = FALSE)

# Add Census API key to your environment
census_api_key(key)


# Load ACS data
poverty_data_raw <- get_acs(geography = "county",
                            table = "C17002",
                            survey = "acs5",
                            year = 2020,
                            key = census_api_key,
                            state = "TX",
                            output = "tidy",
                            geometry = FALSE
)


# # Load ACS data 2019
# poverty_data_raw_19 <- get_acs(geography = "county",
#                             table = "C17002",
#                             survey = "acs5",
#                             year = 2019,
#                             key = census_api_key,
#                             state = "TX",
#                             output = "tidy",
#                             geometry = FALSE
# )


poverty_data <- subset(poverty_data_raw, GEOID %in% trla_counties$i_geoid)


poverty_data <- poverty_data %>%
  filter(variable == "C17002_002" | variable == "C17002_003" | variable == "C17002_004" | variable == "C17002_005"
         | variable == "C17002_006" | variable == "C17002_007") %>%
  group_by(GEOID) %>%
  mutate(Under200 = sum(estimate),
         Under200MOE = moe_sum(moe,estimate),
         Perc_MOE = round(Under200MOE/Under200 * 100, 2)) %>%
  filter(variable == "C17002_002") %>%
  select(-estimate, -moe) %>%
  mutate(NAME = str_replace(NAME, " County, Texas", ""))





## ----------- Produce maps with all civil attorneys

estimates <- get_estimates(poverty_data, attys_list, trla_counties)

county_estimates <- estimates[[1]]
region_estimates <- estimates[[2]]

maps <- build_maps(county_estimates, region_estimates)

county_map <- maps[[1]]
region_map <- maps[[2]]



## ----------- Produce maps without law grads

estimates_nlg <- get_estimates(poverty_data, attys_list_nlg, trla_counties)

county_estimates_nlg <- estimates_nlg[[1]]
region_estimates_nlg <- estimates_nlg[[2]]

maps_nlg <- build_maps(county_estimates_nlg, region_estimates_nlg)

county_map_nlg <- maps_nlg[[1]]
region_map_nlg <- maps_nlg[[2]]


# March 2022
saveWidget(county_map, file="./Output/maps/2022_03_by_county.html")
saveWidget(region_map, file="./Output/maps/2022_03_by_region.html")

saveWidget(county_map_nlg, file="./Output/maps/2022_03_by_county_no_law_grads.html")
saveWidget(region_map_nlg, file="./Output/maps/2022_03_by_region_no_law_grads.html")
