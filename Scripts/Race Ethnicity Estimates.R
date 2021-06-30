# Gather data on race/ethnicity, race/ethnicity and poverty, and educational attainment in TRLA service area

### Race/ethnicity of our service area ###

library(dplyr)
library(tidycensus)
library(tidyverse)
library(xlsx)

Counties <- read.csv("../SACount.csv")

TX_race_eth <- get_acs(geography = "county",
                      table = "B03002",
                      survey = "acs5",
                      year = 2018,
                      state = "TX",
                      output = "tidy",
                      geometry = FALSE,
                      cache_table = TRUE)

inSA2 <- subset(TX_race_eth, GEOID %in% Counties$ï..GEOID)

Service_area_summary <- inSA2 %>%
  group_by(variable) %>%
  mutate(TOTAL = sum(estimate), MoError = moe_sum(moe, estimate)) %>%
  filter(GEOID == 48029) %>%
  select(-estimate, -moe, -NAME)

total_pop_SA <- Service_area_summary$TOTAL[1]

Service_area_summary <- Service_area_summary %>%
  mutate(percent_total = TOTAL/total_pop_SA, moe_perc_total = moe_prop(TOTAL,total_pop_SA,MoError,0))

write.xlsx(as.data.frame(Service_area_summary), "./Output/Service_Area_Race_Ethnicity.xlsx")


### Race and ethnicity and income ###

TX_poverty_race_eth <- get_acs(geography = "county",
                                 table = "S1701",
                                 survey = "acs5",
                                 year = 2018,
                                 state = "TX",
                                 output = "tidy",
                                 geometry = FALSE,
                                 cache_table = TRUE)

inSA3 <- subset(TX_poverty_race_eth, GEOID %in% Counties$ï..GEOID)

list <- data.frame(a=c("S1701_C02_013", 
                       "S1701_C01_001" ,
                       "S1701_C02_001" ,
                       "S1701_C02_014", 
                       "S1701_C02_015", 
                       "S1701_C02_016", 
                       "S1701_C02_017", 
                       "S1701_C02_018", 
                       "S1701_C02_019", 
                       "S1701_C02_020", 
                       "S1701_C02_021"))

Service_area_PRE_summary <- inSA3 %>%
  filter(variable %in% list$a) %>%
  group_by(variable) %>%
  mutate(TOTAL = sum(estimate), MoError = moe_sum(moe, estimate)) %>%
  filter(GEOID == 48029) %>%
  select(-estimate, -moe, -NAME)

total_poverty_pop = Service_area_PRE_summary$TOTAL[1]
total_pop_under_100 = Service_area_PRE_summary$TOTAL[2]

Service_area_PRE_summary <- Service_area_PRE_summary %>%
  mutate(percent_total = TOTAL/total_poverty_pop, moe_perc_total = moe_prop(TOTAL,total_poverty_pop,MoError,0),
         percent_poverty = TOTAL/total_pop_under_100, more_perc_povpop_total= moe_prop(TOTAL, total_pop_under_100, MoError,0))

write.xlsx(as.data.frame(Service_area_PRE_summary), "./Output/Service_Area_Poverty_Race_Ethnicity.xlsx")


### Educational attainment ###

TX_edu_attain_race <- get_acs(geography = "county",
                               table = "S1501",
                               survey = "acs5",
                               year = 2018,
                               state = "TX",
                               output = "tidy",
                               geometry = FALSE,
                               cache_table = TRUE)

inSA4 <- subset(TX_edu_attain_race, GEOID %in% Counties$ï..GEOID)


list <- data.frame(a=c("S1501_C01_006",
                       "S1501_C01_015",
                       "S1501_C01_030",
                       "S1501_C01_033",
                       "S1501_C01_036",
                       "S1501_C01_039",
                       "S1501_C01_042",
                       "S1501_C01_045",
                       "S1501_C01_048",
                       "S1501_C01_051",
                       "S1501_C01_054"))

Service_area_EDU_summary <- inSA4 %>%
  filter(variable %in% list$a) %>%
  group_by(variable) %>%
  mutate(TOTAL = sum(estimate), MoError = moe_sum(moe, estimate)) %>%
  filter(GEOID == 48029) %>%
  select(-estimate, -moe, -NAME)

total_over_25 = 5174171
total_w_bachelors = 1465634

Service_area_EDU_summary <- Service_area_EDU_summary %>%
  mutate(percent_ppl_under_25 = TOTAL/total_over_25, moe_perc_total = moe_prop(TOTAL,total_over_25,MoError,0),
         percent_bach = TOTAL/total_w_bachelors, more_perc_bach_total= moe_prop(TOTAL, total_w_bachelors, MoError,0))

write.xlsx(as.data.frame(Service_area_EDU_summary), "./Output/Service_Area_Edu.xlsx")
