# Import packages ----

library(tidyverse)
library(janitor)
library(lubridate)
library(zoo)
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))
source(file = "./functions.R")

# Set working directory 
setwd("~/../../Department of Health and Social Care/NW005 - DischargeAnalysisCenter/Analysis Projects/20240129 - NCTR Published - Briefing Tool/Code/")

# Import data ----

icb_nctr <- read_csv(file = './data/NCTR/DAC_icb_nctr_final_mar.csv')
trust_nctr <- read_csv(file = './data/NCTR/DAC_trust_nctr_final_mar.csv')

nas_trusts <- check_proportion_nas(trust_nctr) #metric value 1.1% NAs
nas_icbs <- check_proportion_nas(icb_nctr) #metric value 0% NAs

# Clean data ----

clean_data <- function(data){
  data <- data %>%
    mutate(Date = as.Date(Date, format = '%d/%m/%Y'),
           MetricValue = as.numeric(MetricValue))
}

trust_nctr <- clean_data(trust_nctr)
icb_nctr <- clean_data(icb_nctr)

#bring in beds ----

icb_beds <- read_csv(file = './output/monthly_beds_icb_2024-04-19.csv') 
trusts_beds <- read_csv(file = './output/monthly_beds_trusts_2024-04-19.csv')
icb_trusts_map <- read_csv(file = './data/Trust_to_ICB_mapping_2023.csv')

#test for consistency between icb and trust beds after August 2023

icb_test <- icb_beds %>%
  rename(location_code = icb_code,
         location_name = icb_name) %>%
  select(-region) %>%
  group_by(floor_month) %>%
  summarise(beds = sum(beds))

trust_test <- trusts_beds %>%
  filter(floor_month > "2023-07-01") %>%
  rename(location_code = trust_code,
         location_name = trust_name) %>%
  select(-region) %>%
  group_by(floor_month) %>%
  summarise(beds = sum(beds))

View(setdiff(trust_test, icb_test))
View(setdiff(icb_test, trust_test))


#make beds for icbs from 01/04/2022 - 01/08/2023 by aggregating up trusts ----
by_trust <- join_by(trust_code == Trust_Code)
distinct_icb_trust <- icb_trusts_map %>%
  select(ICB_Code, Trust_Code, ICB_Name) %>%
  distinct() #so we don't get many to many relationship

icb_beds_temp <- trusts_beds %>%
  filter(floor_month < "2023-08-01") %>%
  inner_join(distinct_icb_trust, 
            by = by_trust) %>%
  group_by(ICB_Code, floor_month) %>%
  summarise(region = first(region),
            ICB_Name = first(ICB_Name),
            beds = sum(beds)) %>%
  clean_names() %>%
  select(region, icb_code, icb_name, beds, floor_month) %>%
  rbind(icb_beds)


#compare vs aggregated trust

icb_test_2 <- icb_beds_temp %>%
  rename(location_code = icb_code,
         location_name = icb_name) %>%
  select(-region) %>%
  group_by(floor_month) %>%
  summarise(beds = sum(beds))

trust_test_2 <- trusts_beds %>%
  rename(location_code = trust_code,
         location_name = trust_name) %>%
  select(-region) %>%
  group_by(floor_month) %>%
  summarise(beds = sum(beds))

View(setdiff(trust_test_2, icb_test_2))
View(setdiff(icb_test_2, trust_test_2))

# Add floor month in prep for join ----

add_floor_month <- function(data){
  data <- data %>%
    mutate(floor_month = floor_date(ymd(Date), "month"))
}

icb_nctr <- add_floor_month(icb_nctr)
trust_nctr <- add_floor_month(trust_nctr)

#join nctr and beds ----
by_trust <- join_by(TrustKey == trust_code, floor_month)
trust_final <- trust_nctr %>%
  left_join(trusts_beds,
            by = by_trust) %>%
  filter_nctr_and_date() %>% 
  rename(`Org Code` = TrustKey,
         `Org Name` = trust.Name,
         `NCTR Value` = MetricValue) %>%
  select(`Org Code`, `Org Name`, Date, `NCTR Value`, beds)

by_icb <- join_by(ICBKey == icb_code, floor_month)
icb_final <- icb_nctr %>%
  left_join(icb_beds_temp,
            by = by_icb) %>%
  filter_nctr_and_date() %>% 
  rename(`Org Code` = ICBKey,
         `Org Name` = icb.Name,
         `NCTR Value` = MetricValue) %>%
  select(`Org Code`, `Org Name`, Date, `NCTR Value`, beds)

#Check NAs in each column adn write data----

na_check_trust <- check_proportion_nas(trust_final)
na_check_icb <- check_proportion_nas(icb_final)

#ensure ordered by date
trust_final <- trust_final %>%
  arrange(Date)
icb_final <- icb_final %>%
  arrange(Date)

writexl::write_xlsx(x = trust_final, path = 'output/trust_nctr_beds.xlsx')
writexl::write_xlsx(x = icb_final, path = 'output/icb_nctr_beds.xlsx')

