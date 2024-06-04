# Import packages ----

library(tidyverse)
library(janitor)
library(lubridate)
library(zoo)
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))
source(file = "./functions.R")

# Set working directory 
setwd("~/../../Department of Health and Social Care/NW005 - DischargeAnalysisCenter/Analysis Projects/NCTR Published - Briefing Tool/Code/")

# Import data ----

icb_beds_bs <- read_csv(file = './data/beds/backseries/beds_icb_backseries.csv')
trust_beds_bs <- read_csv(file = './data/beds/backseries/beds_trusts_backseries.csv')
icb_trusts_mapping <- read_csv(file = './data/Trust_to_ICB_mapping_2023.csv')

# check for differences after August 2023

icb_test <- icb_beds_bs %>%
  rename(location_code = icb_code,
         location_name = icb_name) %>%
  select(-region) %>%
  group_by(floor_month) %>%
  summarise(beds = sum(beds))

trust_test <- trust_beds_bs %>%
  filter(floor_month > "2023-07-01") %>%
  rename(location_code = trust_code,
         location_name = trust_name) %>%
  select(-region) %>%
  group_by(floor_month) %>%
  summarise(beds = sum(beds))

test <- icb_test %>%
  left_join(trust_test, by = join_by(floor_month), suffix = c(".icb", ".trust"))
View(test)

#make beds for icbs from 01/04/2022 - 01/08/2023 by aggregating up trusts ----
distinct_icb_trust <- icb_trusts_mapping %>%
  select(ICB_Code, Trust_Code, ICB_Name) %>%
  distinct()

icb_beds_pre_aug_23 <- trust_beds_bs %>%
  filter(floor_month < "2023-08-01") %>%
  inner_join(distinct_icb_trust, 
             by = join_by(trust_code == Trust_Code)) %>%
  group_by(ICB_Code, floor_month) %>%
  summarise(region = first(region),
            ICB_Name = first(ICB_Name),
            beds = sum(beds)) %>%
  clean_names() %>%
  select(region, icb_code, icb_name, beds, floor_month) 

#write out old backseries, which collate_beds_published_icb.R can append too
write.csv(x = icb_beds_pre_aug_23, 
          file = paste0('data/beds/backseries/beds_icb_backseries_pre_aug_23.csv'), 
          row.names = FALSE)


