## Functions needed to produce the TRLA attorney : client population estimates ratio maps



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
    subset(GEOID %in% trla_counties$i_geoid) %>%
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

