## ACS Data for 2022 Needs Assessment

library(readr)
library(readxl)
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus) 
library(janitor)




# Load office - county pairings
office_county_matches <- read_excel("./Data/TRLA_Counties_by_Office.xlsx") %>%
  clean_names() %>%
  mutate(office = str_to_upper(office))

# Sinton = Corpus Christi
office_county_matches$office[office_county_matches$office=="SINTON"] <- "CORPUS CHRISTI"

# Load list of counties in TRLA service area with GEOIDS
trla_counties <- read.csv("./Data/SACount.csv") %>% clean_names()





## ----------- Census API Key

# Register for your unique API key here: https://api.census.gov/data/key_signup.html
# Save your key outside of this project directory
key <- read.delim("../credentials/census_api_key.txt", header = FALSE)

# Add Census API key to your environment
census_api_key(key)


v20 <- load_variables(2020, "acs5", cache = TRUE)
View(v20)





## ---------- Load ACS 2020 5YR Poverty Population Estimates

# 2020 COUNTIES
acs_5yr_20 <- get_acs(geography = "county",
                            table = "C17002",
                            survey = "acs5",
                            year = 2020,
                            key = census_api_key,
                            state = "TX",
                            output = "tidy",
                            geometry = FALSE
)


acs_5yr_20_trla <- subset(acs_5yr_20, GEOID %in% trla_counties$i_geoid)


# Estimate for under 200% FPL

poverty_data_20 <- acs_5yr_20_trla %>%
  filter(variable == "C17002_002" | variable == "C17002_003" | variable == "C17002_004" | variable == "C17002_005"
         | variable == "C17002_006" | variable == "C17002_007") %>%
  group_by(GEOID) %>%
  mutate(Under200 = sum(estimate),
         Under200MOE = moe_sum(moe,estimate),
         Under200MOE_sq = (Under200MOE * Under200MOE),
         Perc_MOE = round(Under200MOE/Under200 * 100, 2)) %>%
  filter(variable == "C17002_002") %>%
  select(-estimate, -moe) %>%
  mutate(NAME = str_replace(NAME, " County, Texas", ""))


# Estimate - population under 200% FPL in TRLA service area, 2016-2020 
under_200_e_2020 <- sum(poverty_data_20$Under200) # 3060423

under_200_moe_2020 <- sum(poverty_data_20$Under200MOE_sq) %>% sqrt() # 30005.46


poverty_data_20_total <- acs_5yr_20_trla %>%
  filter(variable == "C17002_001") %>% mutate(moe_sq = moe * moe)

total_pop_e_2020 <- sum(poverty_data_20_total$estimate) # 8252710
total_pop_moe_2020 <- sum(poverty_data_20_total$moe_sq) %>% sqrt() # 4045.143






# Estimate for under 125% FPL

poverty_data_20 <- acs_5yr_20_trla %>%
  filter(variable == "C17002_002" | variable == "C17002_003" | variable == "C17002_004" | variable == "C17002_005"
         | variable == "C17002_006" | variable == "C17002_007") %>%
  group_by(GEOID) %>%
  mutate(Under200 = sum(estimate),
         Under200MOE = moe_sum(moe,estimate),
         Under200MOE_sq = (Under200MOE * Under200MOE),
         Perc_MOE = round(Under200MOE/Under200 * 100, 2)) %>%
  filter(variable == "C17002_002") %>%
  select(-estimate, -moe) %>%
  mutate(NAME = str_replace(NAME, " County, Texas", ""))


# Estimate - population under 200% FPL in TRLA service area, 2016-2020 
under_200_e_2020 <- sum(poverty_data_20$Under200) # 3060423

under_200_moe_2020 <- sum(poverty_data_20$Under200MOE_sq) %>% sqrt() # 30005.46


poverty_data_20_total <- acs_5yr_20_trla %>%
  filter(variable == "C17002_001") %>% mutate(moe_sq = moe * moe)

total_pop_e_2020 <- sum(poverty_data_20_total$estimate) # 8252710
total_pop_moe_2020 <- sum(poverty_data_20_total$moe_sq) %>% sqrt() # 4045.143








## ---------- Load ACS 2015 5YR Poverty Population Estimates

# 2015 COUNTIES
acs_5yr_15 <- get_acs(geography = "county",
                      table = "C17002",
                      survey = "acs5",
                      year = 2015,
                      key = census_api_key,
                      state = "TX",
                      output = "tidy",
                      geometry = FALSE
)



acs_5yr_15_trla <- subset(acs_5yr_15, GEOID %in% trla_counties$i_geoid)


poverty_data_15 <- acs_5yr_15_trla %>%
  filter(variable == "C17002_002" | variable == "C17002_003" | variable == "C17002_004" | variable == "C17002_005"
         | variable == "C17002_006" | variable == "C17002_007") %>%
  group_by(GEOID) %>%
  mutate(Under200 = sum(estimate),
         Under200MOE = moe_sum(moe,estimate),
         Under200MOE_sq = (Under200MOE * Under200MOE),
         Perc_MOE = round(Under200MOE/Under200 * 100, 2)) %>%
  filter(variable == "C17002_002") %>%
  select(-estimate, -moe) %>%
  mutate(NAME = str_replace(NAME, " County, Texas", ""))


# Estimate - population under 200% FPL in TRLA service area, 2011-2015 
under_200_e_2015 <- sum(poverty_data_15$Under200) # 

under_200_moe_2015 <- sum(poverty_data_15$Under200MOE_sq) %>% sqrt() #


poverty_data_15_total <- acs_5yr_15_trla %>%
  filter(variable == "C17002_001") %>% mutate(moe_sq = moe * moe)

total_pop_e_2015 <- sum(poverty_data_15_total$estimate) # 
total_pop_moe_2015 <- sum(poverty_data_15_total$moe_sq) %>% sqrt() #




## -------------- ATTYS Data


# Load latest civil attorney staff list
attys_list_raw <- read.csv("./Data/staff_lists/April 2022 Attorneys.csv") %>% 
  clean_names() %>% 
  rename(office = ofc, job_title = job, employee_name = empnam) %>%
  select(-c("i_empnum", "email"))







## -------------- Decennial Census Population Change










## -------------- ACS 1 YR Eligible Pop. Estimates for Major Counties (?)






