## Adapting Attys_Offices_Counties.R to produce maps with and without law graduates. 
## APRIL 2022 ATTY List 

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



### LOAD AND CLEAN ATTY LIST WITH PRACTICE AREAS

# Load latest civil attorney staff list
attys_list_raw <- read.csv("./Data/staff_lists/April 2022 Attorneys.csv") %>% 
  clean_names() %>% 
  rename(office = ofc, job_title = job, employee_name = empnam) %>%
  select(-c("i_empnum", "email"))

# Some specific one-off edits
attys_list_raw[attys_list_raw$employee_name=="GEORGE, JOSEPH A.", "practice_area"] <- "Consumer Protection"
attys_list_raw[attys_list_raw$employee_name=="GEORGE, JOSEPH A.", "practice_area_2"] <- "Torts & Civil Litigation"

attys_list_raw[attys_list_raw$employee_name=="SMITH, PATRICK A.", "practice_area"] <- "Landlord-Tenant"
attys_list_raw[attys_list_raw$employee_name=="SMITH, PATRICK A.", "practice_area_2"] <- "Consumer Protection"



# Pivot longer on the practice areas
attys_long <- attys_list_raw %>% pivot_longer(
  starts_with("practice_area"),
  names_to = c(".value", "set"),
  names_pattern = "(.)(.)"
) %>%
  select(-c("set")) %>% 
  filter(p != "" & !str_detect(p, "Intake") & !str_detect(p, "Public Defender") & !str_detect(p, "N/A"))

attys_long$p <- attys_long$p %>% trimws()


attys_long <- attys_long %>%
  # Clean up practice area names
  mutate(
    p = case_when(
      p == "MHPP" ~ "Mental Health Programs Project",
      (p == "Dilley Project" | p == "Dilley Detention Project") ~ "Dilley Detention Center",
      p == "Housing - Landlord-Tenant" ~ "Landlord-Tenant",
      p == "Native American" ~ "Native Americans",
      p == "Housing-Home Foreclosure Prevention" ~ "Foreclosure Prevention (Home)",
      (p == "Federally Subsized Housing" | p == "Housing-Federally Subsidized") ~ "Federally Subsidized Housing",
      (p == "Domestic Violance & Family Law Group" | p == "Domestic Violence/Family Law") ~ "Domestic Violence & Family Law Group",
      p == "Public Benefits" ~ "Public Benefits Group",
      p == "Farmworker" ~ "Farm Worker",
      TRUE ~ p
    )
  )

attys_long <- attys_long %>%
  
  # Clean up office names
  mutate(
    office = case_when(
      (office == "CORPUS COURTHOUSE" | office == "CORPUS PUEBLO") ~ "CORPUS CHRISTI",
      (office == "HARLINGEN" | office == "MERCEDES LEGAL") ~ "BROWNSVILLE",
      office == "EL PASO/ NY" ~ "EL PASO",
      office == "DEL RIO PD" ~ "DEL RIO",
      office == "DILLEY" ~ "SAN ANTONIO",
      employee_name == "GILBERT, ALEXANDER H." ~ "AUSTIN",
      TRUE ~ office)
  )


## EXPLORATORY ANALYSIS

# How many attys in each practice area?
View(attys_long %>% count(p))


# How many attys with > 1 practice area?

View(attys_long %>% count(employee_name) %>% count(n))







## ATTY:CLIENT RATIO MAPS

# Perform any cleaning needed for this month's atty list
attys_long_local <- attys_long %>% 

  # Filter out offices outside of the 68 county service area
  filter(
    !str_detect(office, "REMOTE") &  !str_detect(office, "NASHVILLE") & 
      !str_detect(office, "DALLAS") & !str_detect(office, "TAJ") & !str_detect(job_title,"PUBLIC DEFENDER")
  )


# Subset to just the civil attorneys who handle cases (exclude law grads) 
attys_list_nlg <- attys_long %>% filter(job_title != "LAW GRADUATE")







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


# April 2022
saveWidget(county_map, file="./Output/maps/2022_04_by_county.html")
saveWidget(region_map, file="./Output/maps/2022_04_by_region.html")

saveWidget(county_map_nlg, file="./Output/maps/2022_04_by_county_no_law_grads.html")
saveWidget(region_map_nlg, file="./Output/maps/2022_04_by_region_no_law_grads.html")
