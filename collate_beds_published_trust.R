# Import packages ---------------------------------------------------------

library(tidyverse)
library(janitor)
library(rstudioapi)
library(readxl)

setwd(dirname(getActiveDocumentContext()$path))
source(file = "./functions.R")

# Set working directory 
setwd("~/../../Department of Health and Social Care/NW005 - DischargeAnalysisCenter/Analysis Projects/NCTR Published - Briefing Tool/Code/")

# Create backseries for icb beds from create_backseries/ directory
CREATE_BACKSERIES <- TRUE

#If not creating backseries, then read in backseries
if(CREATE_BACKSERIES == FALSE){
  trust_backseries <- read_csv(file = './data/beds/backseries/beds_trusts_backseries.csv')
  print("read in backseries, ready to append new data to it")
} else {
  print("creating backseries")
}

# Create vectors for later use from file names ----------------------------
if(CREATE_BACKSERIES){
  beds_files <- list.files(path = 'data/beds/trusts/create_bs/', pattern = '.xlsx')
} else {
  beds_files <- list.files(path = 'data/beds/trusts/', pattern = '.xlsx')
}
year_month_vec <- c()
month_floor_vec <- c()
for(date in beds_files){
  year_ <- substr(x = date, start = 1, stop = 4)
  month_ <- substr(x = date, start = 5, stop = 6)
  year_month <- substr(x = date, start = 1, stop = 6)
  floor_month <- paste0(year_, "-", month_, "-01")
  year_month_vec <- c(year_month_vec, year_month)
  month_floor_vec <- c(month_floor_vec, floor_month)
}

# DF length needs to match number of files in dir we are reading from
# E.g. create_bs/ or trusts/
# USER SHOULD AMMEND AS APPROPRIATE ----------------
TRUST_CELL_REF_DF <- data.frame(month_year = year_month_vec,
                                cell_ref = c("B15:O150", #april 2022
                                             "B15:O150", #may 2022
                                             "B15:O150", #june 2022
                                             "B15:O150", #july 2022
                                             "B15:O150", #august 2022
                                             "B15:O150", #september 2022
                                             "B15:O150", #october 2022
                                             "B15:O150", #november 2022
                                             "B15:O150", #december 2022
                                             "B15:O150", #january 2023
                                             "B15:O150", #february 2023
                                             "B15:O150", #march 2023
                                             "B15:O150", #april 2023
                                             "B15:O150", #may 2023
                                             "B15:O150", #june 2023
                                             "B15:O150", #july 2023
                                             "B15:O192", #august 2023
                                             "B15:O191", #september 2023
                                             "B15:O190", #october 2023
                                             "B15:O191", #november 2023
                                             "B15:O191", #december 2023
                                             "B15:O191", #january 2024
                                             "B15:O191", #february 2024
                                             "B15:O191", #march 2024
                                             "B15:O191", #april 2024
                                             "B15:O191"), #may 2024
                                ignore_rows = c(12, #april 2022 
                                                12, #may 2022
                                                12, #june 2022
                                                12, #july 2022
                                                12, #august 2022
                                                12, #september 2022
                                                12, #october 2022
                                                12, #november 2022
                                                12, #december 2022
                                                12, #january 2023
                                                12, #february 2023
                                                12, #march 2023
                                                12, #april 2023
                                                12, #may 2023
                                                12, #june 2023
                                                12, #july 2023
                                                55, #august 2023
                                                55, #september 2023
                                                55, #october 2023
                                                55, #november 2023
                                                55, #december 2023
                                                55, #january 2024
                                                55,  #february 2024
                                                55, #march 2024
                                                55, #april 2024
                                                55)) #may 2024


# Read data function ------------------------------------------------------

read_data <- function(file_name, cell_ref, backseries){
  if(backseries == TRUE){
    path <- "data/beds/trusts/create_bs/"
  } else {
    path <- "data/beds/trusts/"
  }
  sheet <- grep(excel_sheets(path = paste0(path, file_name)), 
                pattern = 'type 1', 
                value = TRUE)
  df <- readxl::read_xlsx(path = paste0(path, file_name),
                          sheet = sheet,
                          range = cell_ref,
                          na = "-")}


# Create list of excels, one item per month -------------------------------

list_excels <- function(df, backseries){
  #create empty list
  temp_list <- list()
  #iterate through files in beds/ using index
  for(i in c(1:length(beds_files))){
    #obtain cell references from df
    print(beds_files[i])
    cell_ref <-  df$cell_ref[df$month_year==year_month_vec[i]]
    #get df with correct cell reference
    beds_df_temp <- read_data(beds_files[i], cell_ref, backseries)
    print(sprintf("sucessfully read in file %s", beds_files[i]))
    temp_list[[i]] <- beds_df_temp}
  return(temp_list)
}

#icb_excel_list <- list_excels(ICB_CELL_REF_DF)
trust_excel_list <- list_excels(TRUST_CELL_REF_DF, CREATE_BACKSERIES)


# Iterate through each dataframe in the list and wrangle to get
# beds data --------

wrangle_sheets <- function(excel_list, cell_ref_df){
  for(i in c(1: length(excel_list))){
    # iterate through each month
    df <- excel_list[[i]]
    # cell_ref_df$ignore_rows[i] gets ignore_rows in cell reference df
    # print(paste0("ignore rows: ",cell_ref_df$ignore_rows[i]))
    # print(paste0("cut up to row: ", dim(df)[1]))
    df <- df[c(cell_ref_df$ignore_rows[i]:dim(df)[1]),]
    print(dim(df))
    # rename columns
    if(i < 17){ #first 5 months have reversed column order for trust_code and trust_name
      names(df)[1:3] <- c('region', 'trust_name', 'trust_code')
    }
    else{
      names(df)[1:3] <- c('region', 'trust_code', 'trust_name')
    }
    df$`Adult G&A beds available` <- as.numeric(df$`Adult G&A beds available`)
    # get beds
    if("Adult G&A covid void beds" %in% names(df)){
      df$`Adult G&A covid void beds` <- as.numeric(df$`Adult G&A covid void beds`)
      df[['beds']] <- df[['Adult G&A beds available']] - df[['Adult G&A covid void beds']]
    }else{
      df[['beds']] <- df[['Adult G&A beds available']] 
    }
    # add floor date per month for join with NCTR
    df[['floor_month']] <- as.Date(month_floor_vec[i])
    print(as.Date(month_floor_vec[i]))
    # keep selected columns
    df <- df %>% select('region', 'trust_code', 'trust_name', 'beds', 'floor_month')
    # update / overwrite original list
    excel_list[[i]] <- df}
  return(excel_list)
}

trust_excel_list_formatted <- wrangle_sheets(trust_excel_list, TRUST_CELL_REF_DF)

#combine rows
trust_beds_long <- trust_excel_list_formatted %>%
  bind_rows() %>%
  mutate(trust_name = str_to_title(trust_name),
         region = str_to_title(region))

test_national_trust_source <- trust_beds_long %>%
  group_by(floor_month) %>%
  summarise(sum = sum(beds))

#check ~119 trusts inc. historical trusts
length(unique(trust_beds_long$trust_name))

#check those trusts with different count to number of months
count_occurences <- function(data){
  n_months <- data %>%
    distinct(`floor_month`) %>%
    pull() %>%
    length()
  missing_trusts <- data %>%
    group_by(trust_name) %>%
    summarise(count = n()) %>%
    filter(count != n_months) %>%
    return(missing_trusts)
}

check <- count_occurences(trust_beds_long)

#write csv ----
date_today <- Sys.Date()
if(CREATE_BACKSERIES == TRUE){
  # overwrite backseries
  write.csv(x = trust_beds_long, 
            file = paste0('data/beds/backseries/beds_trusts_backseries','.csv'), 
            row.names = FALSE)
} else {
  # append new data to backseries
  icb_backseries_and_new <- rbind(trust_backseries, trust_beds_long)
  write.csv(x = icb_backseries_and_new, 
            file = paste0('output/monthly_beds_trusts_', date_today,'.csv'), 
            row.names = FALSE)
}


