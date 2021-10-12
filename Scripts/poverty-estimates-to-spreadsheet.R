# I am hopeful we can start looking at the new census data and analyze it for trends, etc. 
# One thing that needs to be updated is simply the raw and percentage shifts in the poverty population as 
# it relates to our 68 counties.  Can we start with a basic listing the overall population by county, then
# the number of people who are poor (using 200% of poverty or whatever the census uses now) by county? 
# I’d like the data in a spreadsheet for the whole state, and then another for just our 68 counties so
# I don’t have to weed them out.  I need these stats for various things that come up. 
# Also need the race/ethnicity breakdown by county of total population and the poverty population.
# We need trends comparing previous data, etc.  
# And anything other metric we should start looking at for further analysis. 
# I know this is easy stuff for data people, but it is a place for me to start.


# For all Texas counties, and for all TRLA service area counties:
# A workbook with 3 tabs
# Population under 200% of poverty level
# Population by race/ethnicity
# Population by under 200% of poverty level by race/ethnicity

library(tidyverse)
library(tidycensus)
library(sf)
library(data.table)

## ----- Load TRLA service area counties dataset

trla_counties <- read.csv("./Data/SACount.csv")


## ----- Get data from the Census API

# Add Census API key to your environment
key <- read.delim("../credentials/census_api_key.txt", header = FALSE)

census_api_key(key)


## ------ Adapting Lizzie's code from Poverty Estimates.R

# Get population estimates under 200% of poverty level by county
tx_poverty <- get_acs(geography = "county",
                      table = "C17002",
                      survey = "acs5",
                      year = 2019,
                      state = "TX",
                      output = "tidy",
                      geometry = FALSE
)


# All Texas counties
pop_under_200_tx <- tx_poverty %>%
  filter(variable == "C17002_002" | 
         variable == "C17002_003" | 
         variable == "C17002_004" | 
         variable == "C17002_005" | 
         variable == "C17002_006" | 
         variable == "C17002_007") %>%
  group_by(GEOID) %>%
  mutate(Under200 = sum(estimate),
         MOE = moe_sum(moe,estimate)) %>%
  filter(variable == "C17002_002") %>%
  select(-estimate, -moe)


# Filter to TRLA service area
pop_under_200_sa <- subset(pop_under_200_tx, GEOID %in% trla_counties$GEOID)






# write.xlsx(as.data.frame(pop_under_200), "Under200_2019_5y.xlsx")








#### ------Anna testing: 

# Save library of variables
v19 <- load_variables(2019, "acs5", cache = TRUE) 


# Obtain county -level demographic estimates with geometry from the 2019 ACS 5-Year Estimates
# Reformat & add fields for percentages
acs_19_tx <- get_acs(geography = "county",
                     variables = c(
                       total_pop_poverty = "C17002_001", 
                       p2 = "C17002_002", # Pop under 50% of poverty level
                       p3 = "C17002_003", # 50-99%
                       p4 = "C17002_004", # 100 - 1.24%
                       p5 = "C17002_005", # 1.25 - 1.5%
                       p6 = "C17002_006",
                       p7 = "C17002_007",
                       total_pop_race = "B03002_001",
                       white = "B03002_003", # white non-Hispanic
                       black = "B03002_004", # Black non-Hispanic
                       asian = "B03002_006", # Asian non-Hispanic
                       hispanic = "B03002_012", # Hispanic all races
                       american_indian = "B03002_007"), # American Indian, non-Hispanic
                     state = "TX",
                     geometry = FALSE) %>%
  dplyr::select("NAME", "GEOID", "variable", "estimate") %>%
  tidyr::spread(variable, estimate) %>%
  mutate(total_under_200_poverty = (p2 + p3 + p4 + p5 + p6 + p7),
         pct_under_200_poverty = (total_under_200_poverty/total_pop_poverty)*100, 
         pct_hispanic = (hispanic/total_pop_race)*100,
         pct_white = (white/total_pop_race)*100,
         pct_black = (black/total_pop_race)*100,
         pct_asian = (asian/total_pop_race)*100,
         pct_hispanic = (hispanic/total_pop_race)*100)




# Use county GEOID to subset the ACS data to counties in TRLA's service area
acs_19_trla <- subset(acs_19_tx, GEOID %in% trla_counties$ï..GEOID) 






