#Script initially written for Nuclear DEIS project with Maggie Barnes

library(dplyr)
library(tidycensus)
library(tidyverse)
library(xlsx)

lepVar <- c("PercLEP" = "S1602_C04_001")
raceVars <- c("TotalPop" = "B03002_001", "WhitePop" = "B03002_003")
incVars <- c("totalPov" = "C17002_001", "Under50" = "C17002_002", "50to99" = "C17002_003", "100to124" = "C17002_004",  
          "125to150" = "C17002_005", "150to184"="C17002_006","185to199" = "C17002_007")
allVars <- c(lepVar, raceVars, incVars)

TX_LEP <- get_acs(geography = "tract",
                       variables = lepVar,
                       survey = "acs5",
                       year = 2018,
                       state = "TX",
                       output = "wide",
                       geometry = FALSE,
                       cache_table = TRUE)

TX_Race <- get_acs(geography = "tract",
                  variables = raceVars,
                  survey = "acs5",
                  year = 2018,
                  state = "TX",
                  output = "wide",
                  geometry = FALSE,
                  cache_table = TRUE)


TX_Inc <- get_acs(geography = "tract",
                   variables = incVars,
                   survey = "acs5",
                   year = 2018,
                   state = "TX",
                   output = "wide",
                   geometry = FALSE,
                   cache_table = TRUE)

TX_Inc_tidy <- get_acs(geography = "tract",
                  variables = incVars,
                  survey = "acs5",
                  year = 2018,
                  state = "TX",
                  output = "tidy",
                  geometry = FALSE,
                  cache_table = TRUE)

TX_Inc_tidy_tbl <- TX_Inc_tidy %>%
  group_by(GEOID) %>%
  summarise(U200_moe_sum = moe_sum(moe, estimate))

TX_Inc2 <- right_join(TX_Inc, TX_Inc_tidy_tbl, by = "GEOID") %>%
  mutate(sum_under200 = (Under50E + `50to99E` + `100to124E` + `125to150E` + `150to184E` + `185to199E`),
         pct_under200 = 100*(sum_under200/totalPovE),
         U200_moe_prop = moe_prop(sum_under200, totalPovE, U200_moe_sum, totalPovM)) %>%
  select("GEOID","NAME", "sum_under200", "pct_under200", "U200_moe_sum", "U200_moe_prop")

TX_Race2 <- TX_Race %>%
  mutate(Pct_White = 100*(WhitePopE/TotalPopE),
         Pct_White_moe = 100*(moe_prop(WhitePopE, TotalPopE, WhitePopM, TotalPopM)))

Allstats <- right_join(TX_Race2, TX_Inc2, by = "GEOID") %>%
  right_join(TX_LEP, by = "GEOID") %>%
  filter(str_detect(GEOID,"^48201") | str_detect(GEOID, "^48141"))

AllstatsFINAL <- Allstats %>%
  select(-c("NAME.y","NAME"))

write.csv(AllstatsFINAL, "./Output/AllStatsFinal.csv")
