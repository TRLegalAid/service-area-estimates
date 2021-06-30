library(dplyr)
library(tidycensus)
library(tidyverse)
library(xlsx)


### Sum under 125 ###

Counties <- read.csv("../SACount.csv")

TXPoverty <- get_acs(geography = "county",
        table = "C17002",
        survey = "acs5",
        year = 2012,
        state = "TX",
        output = "tidy",
        geometry = FALSE
        )

inSA2 <- subset(TXPoverty, GEOID %in% Counties$ï..GEOID)

SumUnder125 <- inSA2 %>%
  filter(variable == "C17002_002" | variable == "C17002_003" | variable == "C17002_004") %>%
  group_by(GEOID) %>%
  mutate(Under125 = sum(estimate)) %>%
  filter(variable == "C17002_002") %>%
  select(-estimate, -moe)

write.xlsx(as.data.frame(SumUnder125), "./Output/SumUnder125_2012_5y.xlsx")


### Sum under 200 ###

TXPoverty2 <- get_acs(geography = "county",
                      table = "C17002",
                      survey = "acs5",
                      year = 2018,
                      state = "TX",
                      output = "tidy",
                      geometry = FALSE
)

inSA2 <- subset(TXPoverty2, GEOID %in% Counties$ï..GEOID)

SumUnder1252 <- inSA2 %>%
  filter(variable == "C17002_002" | variable == "C17002_003" | variable == "C17002_004" | variable == "C17002_005"
         | variable == "C17002_006" | variable == "C17002_007") %>%
  group_by(GEOID) %>%
  mutate(Under125 = sum(estimate)) %>%
  filter(variable == "C17002_002") %>%
  select(-estimate, -moe)

write.xlsx(as.data.frame(SumUnder1252), "SumUnder200_2018_5y.xlsx")

