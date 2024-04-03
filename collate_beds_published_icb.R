# Import packages --------------------------------------------------------

library(tidyverse)
library(janitor)
library(rstudioapi)
library(readxl)


setwd(dirname(getActiveDocumentContext()$path))
source(file = "./functions.R")

# Set working directory 
setwd("~/../../Department of Health and Social Care/NW005 - DischargeAnalysisCenter/Analysis Projects/20240129 - NCTR Published - Briefing Tool/Code/")

# Create backseries for icb beds from create_backseries/ directory
CREATE_BACKSERIES <- FALSE

#If not creating backseries, then read in backseries
if(CREATE_BACKSERIES == FALSE){
  icb_backseries <- read_csv(file = './data/beds/backseries/beds_icb_backseries.csv')
  print("read in backseries, ready to append new data to it")
} else {
  print("creating backseries")
}

# Create vectors for later use from file names ----------------------------
# Only ICB level data in published files from Aug 2023 - current
if(CREATE_BACKSERIES){
  beds_files <- list.files(path = 'data/beds/icb/create_bs/', pattern = '.xlsx')
  } else {
  beds_files <- list.files(path = 'data/beds/icb/', pattern = '.xlsx')
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
# E.g. create_bs/ or icb/
# USER SHOULD AMMEND AS APPROPRIATE ----------------

ICB_CELL_REF_DF <- data.frame(month_year = year_month_vec,
                              cell_ref = c(#"B15:O67", #aug 23
                                           #"B15:O67", #sep 23
                                           #"B15:O67", #oct 23
                                           #"B15:O67", #nov 23
                                           #"B15:O67", #dec 23
                                           #"B15:O67", #jan 24
                                           "B15:O67"), #feb 24
                              ignore_rows = c(#11, 
                                              #11, 
                                              #11, 
                                              #11, 
                                              #11, 
                                              #11,
                                              11))

# Read data function ------------------------------------------------------

read_data <- function(file_name, cell_ref, backseries){
  if(backseries){
    path <- "data/beds/icb/create_bs/"
  } else {
    path <- "data/beds/icb/"
  }
  sheet <- grep(excel_sheets(path = paste0(path, file_name)), 
                pattern = 'type 1', 
                value = TRUE)
  df <- readxl::read_xlsx(path = paste0(path, file_name),
                          sheet = sheet,
                          range = cell_ref)}

# Create list of excels, one item per month -------------------------------

list_excels <- function(df, backseries){
  #create empty list
  temp_list <- list()
  #iterate through files in beds/ using index
  for(i in c(1:length(beds_files))){
    #obtain cell references from df
    cell_ref <-  df$cell_ref[df$month_year==year_month_vec[i]]
    #get df with correct cell reference
    print(beds_files[i])
    print(cell_ref)
    beds_df_temp <- read_data(beds_files[i], cell_ref, backseries)
    print(sprintf("sucessfully read in file %s", beds_files[i]))
    temp_list[[i]] <- beds_df_temp}
  return(temp_list)
}

icb_excel_list <- list_excels(ICB_CELL_REF_DF, CREATE_BACKSERIES)

# Iterate through each dataframe in the list and wrangle to get
# beds data --------

wrangle_sheets <- function(excel_list, cell_ref_df){
  for(i in c(1: length(excel_list))){
    # iterate through each month
    df <- excel_list[[i]]
    # cell_ref_df$ignore_rows[i] gets ignore_rows in cell reference df
    df <- df[c(cell_ref_df$ignore_rows[i]:dim(df)[1]),]
    # rename columns
    names(df)[1:3] <- c('region', 'icb_code', 'icb_name')
    # get beds
    df$`Adult G&A beds available` <- as.numeric(df$`Adult G&A beds available`)
    df[['beds']] <- df[['Adult G&A beds available']] - df[['Adult G&A covid void beds']]
    # add floor date per month for join with NCTR
    df[['floor_month']] <- as.Date(month_floor_vec[i])
    # keep selected columns
    df <- df %>% select('region', 'icb_code', 'icb_name', 'beds', 'floor_month')
    # update / overwrite original list
    excel_list[[i]] <- df}
  return(excel_list)
}

icb_excel_list_formatted <- wrangle_sheets(icb_excel_list, ICB_CELL_REF_DF)

#combine rows
icb_beds_long <- icb_excel_list_formatted %>%
  bind_rows() %>%
  mutate(icb_name = str_to_title(icb_name),
         region = str_to_title(region))

test_national_icb_source <- icb_beds_long %>%
  group_by(floor_month) %>%
  summarise(sum = sum(beds))

#write csv
date_today <- Sys.Date()
if(CREATE_BACKSERIES == TRUE){
  # overwrite backseries
  write.csv(x = icb_beds_long, 
            file = paste0('data/beds/backseries/beds_icb_backseries','.csv'), 
            row.names = FALSE)
} else {
  # append new data to backseries
  icb_backseries_and_new <- rbind(icb_backseries, icb_beds_long)
  write.csv(x = icb_backseries_and_new, 
            file = paste0('output/monthly_beds_icb_', date_today,'.csv'), 
            row.names = FALSE)
  }

#check 42 ICBs
length(unique(icb_beds_long$icb_name)) == 42

#check those ICBs with different count to number of months
count_occurences <- function(data){
  n_months <- data %>%
    distinct(floor_month) %>%
    pull() %>%
    length()
  missing_icbs <- data %>%
    group_by(icb_name) %>%
    summarise(count = n()) %>%
    filter(count != n_months) %>%
    return(missing_icbs)
}

check <- count_occurences(icb_beds_long)
check <- count_occurences(icb_backseries_and_new)
