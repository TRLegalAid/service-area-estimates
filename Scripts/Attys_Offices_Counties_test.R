## Adapting Attys_Offices_Counties.R to produce versions of the by_region adn by_county maps 
## with and without law graduates. 

library(readr)
library(readxl)
library(tidyverse)
library(tigris)
library(sf)
library(leaflet)
library(htmltools)
library(tidycensus) #install dev version 1.1 with: remotes::install_github("walkerke/tidycensus")
library(htmlwidgets)

# Load office - county pairings
office_county_matches <- read_excel("./Data/TRLA_Counties_by_Office.xlsx") %>%
  clean_names() %>%
  mutate(office = str_to_upper(office))

# Convert Sinton to Corpus Christi
office_county_matches$office[office_county_matches$office=="SINTON"] <- "CORPUS CHRISTI"

# Load list of counties in TRLA service area with GEOIDS
trla_counties <- read.csv("./Data/SACount.csv") %>% clean_names()


# Load latest civil attorney staff list
attys_list <- read.csv("./Data/September 2021 ATTY List.csv") %>% 
  clean_names() %>%
  
  # Filter out offices outside of the 68 county service area
  filter(
    !str_detect(office, "REMOTE") &  !str_detect(office, "NASHVILLE") & 
    !str_detect(office, "DALLAS") & !str_detect(office, "TAJ")
    ) %>% 
  
  # Clean up office names
  mutate(
    office = case_when(
                (office == "CORPUS COURTHOUSE" | office == "CORPUS PUEBLO") ~ "CORPUS CHRISTI",
                (office == "MERCEDES" | office == "HARLINGEN") ~ "BROWNSVILLE",
                 office == "EL PASO/ NY" ~ "EL PASO",
                 office == "VICTORIA LAW CTR (CY" ~ "VICTORIA",
                 office == "DILLEY" ~ "SAN ANTONIO",
                 TRUE ~ office)
    )


# Subset to civil attorneys who handle cases (exclude law grads) 
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
                        year = 2019,
                        key = census_api_key,
                        state = "TX",
                        output = "tidy",
                        geometry = FALSE
)


poverty_data <- subset(poverty_data_raw, GEOID %in% trla_counties$geoid)


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




# ----- Functions to build the spatial data layers

get_estimates <- function(poverty_data, attys, trla_counties) {
  
  #Calculate poverty population per office area
  
  # Get a count of attorneys by office and join the office-county matching data
  attys_county <- attys %>%
    group_by(office) %>%
    summarise(attys = n()) %>%
    full_join(office_county_matches) %>%
    group_by(office) %>%
    
    # Number of attorneys in a region divided by number of counties in the region
    mutate(attys_per_county = attys/n()) %>%
    
    # Join poverty data; create a measure for population under 200% FPL per attorney
    left_join(poverty_data, by = c("county" = "NAME")) %>%
    group_by(office) %>%
    mutate(pop_per_atty_region = round(Under200/attys,2)) %>%
    mutate(poverty_pop_region = sum(Under200))
  
  
  # Create a df of this data per office
  attys_office <- attys_county %>%
    distinct(office, .keep_all = TRUE) %>%
    mutate(pop_per_atty_region = round(poverty_pop_region/attys,2))
  
  
  # Create spatial layer of COUNTY polygons with attorney and poverty data
  county_estimates <- counties(state = "TX", cb = FALSE) %>%
    subset(GEOID %in% trla_counties$geoid) %>%
    geo_join(attys_county, by_df = "county", by_sp = "NAME") %>%
    mutate(pop_per_atty_county = round(Under200/attys,2))
  
  
  # Create spatial layer of REGION polygons with attorney and poverty data
  region_estimates <- county_estimates %>%
    group_by(office) %>%
    summarise(geometry = st_union(geometry)) %>%
    ungroup() %>%
    geo_join(attys_office, by_df = "office", by_sp = "office")
  
  
  estimates <- list(county_estimates, region_estimates)
  
  return(estimates)
  
}



# ------ Build county map function

build_maps <- function(county_estimates, region_estimates) {

    # 1) Build county map
    # Why hardcode the county bins but not region bins?
    bins <- c(0, 100, 200, 500, 1000, 10000, 15000, 20000, Inf)
    
    palette_counties <- colorBin("YlOrRd", domain = county_estimates$pop_per_atty_county, bins = bins)
    
    # Set what appears when you mouseover the counties map
    counties_tooltip <- paste("<p><strong>", county_estimates$NAME, "</strong></p>",
                      "<strong>","Served by:", "</strong>", county_estimates$office, "<br>",
                      "<strong>","TRLA Attorneys:", "</strong>", county_estimates$attys, "<br>",
                      "<strong>","Under 200%:", "</strong>", county_estimates$Under200, "±", round(county_estimates$Under200MOE,2), "<br>",
                      "<strong>","Eligible clients per attorney:", "</strong>", county_estimates$pop_per_atty_county) %>%
      lapply(htmltools::HTML)
    
    
    # COUNTIES MAP
    
    county_map <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = county_estimates,
                  fillColor = ~palette_counties(county_estimates$pop_per_atty_county),
                  weight = .6,
                  fillOpacity = .8,
                  smoothFactor = .8,
                  label = counties_tooltip) %>%
      addPolygons(data = region_estimates,
                  weight = 3,
                  fill = FALSE)
    
    
    # 2) Build region map
    
    palette_regions <- colorBin("YlOrRd", domain = region_estimates$pop_per_atty_region, bins = 5)
    
    # Set what appears when you mouseover the region map
    region_tooltip <- paste("<p><strong>", region_estimates$office, "</strong></p>",
                            "<strong>","Under 200%:", "</strong>", region_estimates$poverty_pop_region, "<br>",
                            "<strong>","TRLA Attorneys:", "</strong>", region_estimates$attys, "<br>",
                            "<strong>","Eligible clients per attorney:", "</strong>", region_estimates$pop_per_atty_region) %>%
      lapply(htmltools::HTML)
    
    
    # REGIONS/OFFICES MAP
    
    region_map <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = county_estimates,
                  fill = FALSE,
                  weight = 2) %>%
      addPolygons(data = region_estimates,
                  fillColor = ~palette_regions(region_estimates$pop_per_atty_region),
                  weight = 2,
                  fillOpacity = .8,
                  smoothFactor = .8,
                  label = region_tooltip)
    
    
    maps <- list(county_map, region_map)
    
    return(maps)

}



# Produce maps with all civil attorneys

estimates <- get_estimates(poverty_data, attys_list, trla_counties)

county_estimates <- estimates[[1]]
region_estimates <- estimates[[2]]

maps <- build_maps(county_estimates, region_estimates)

county_map <- maps[[1]]
region_map <- maps[[2]]



# Produce maps without law grads

estimates_nlg <- get_estimates(poverty_data, attys_list_nlg, trla_counties)

county_estimates_nlg <- estimates_nlg[[1]]
region_estimates_nlg <- estimates_nlg[[2]]

maps_nlg <- build_maps(county_estimates_nlg, region_estimates_nlg)

county_map_nlg <- maps_nlg[[1]]
region_map_nlg <- maps_nlg[[2]]


saveWidget(county_map, file="./Output/2021_09_by_county.html")
saveWidget(region_map, file="./Output/2021_09_by_region.html")

saveWidget(county_map_nlg, file="./Output/2021_09_by_county_no_law_grads.html")
saveWidget(region_map_nlg, file="./Output/2021_09_by_region_no_law_grads.html")


# Next steps: would be great to have this as a single file with selectable layers & dynamic legend
# Reproduce the data layers excluding law grads
# Add two addPolygons to the map
# Add layerscontrol to allow toggle (example below)
# Make sure that the legend updates accordingly

# #  addLayersControl(
# baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
# overlayGroups = c("Quakes", "Outline"),
# options = layersControlOptions(collapsed = FALSE)
# )

