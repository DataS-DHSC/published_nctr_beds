# Import packages ---------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)
library(zoo)
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
  national_backseries <- read_csv(file = './data/beds/backseries/beds_national_backseries.csv')
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
# Create vectors for later use from file names ----------------------------
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


# Create dataframe which USER SHOULD AMMEND AS APPROPRIATE ----------------

# ICB_CELL_REF_DF <- data.frame(month_year = year_month_vec,
#                               cell_ref = c("B15:O67", "B15:O67"),
#                               ignore_rows = c(12, 12))
NAT_CELL_REF_DF <- data.frame(month_year = year_month_vec,
                                cell_ref = c("B15:O16", #april 2022
                                             "B15:O16", #may 2022
                                             "B15:O16", #june 2022
                                             "B15:O16", #july 2022
                                             "B15:O16", #august 2022
                                             "B15:O16", #september 2022
                                             "B15:O16", #october 2022
                                             "B15:O16", #november 2022
                                             "B15:O16", #december 2022
                                             "B15:O16", #january 2023
                                             "B15:O16", #february 2023
                                             "B15:O16", #march 2023
                                             "B15:O16", #april 2023
                                             "B15:O16", #may 2023
                                             "B15:O16", #june 2023
                                             "B15:O16", #july 2023
                                             "B15:O16", #august 2023
                                             "B15:O16", #september 2023
                                             "B15:O16", #october 2023
                                             "B15:O16", #november 2023
                                             "B15:O16", #december 2023
                                             "B15:O16", #january 2024
                                             "B15:O16", #february 2024
                                             "B15:O16", #march 2024
                                             "B15:O16"), #april 2024
                                ignore_rows = c(0, #april 2022 
                                                0, #may 2022
                                                0, #june 2022
                                                0, #july 2022
                                                0, #august 2022
                                                0, #september 2022
                                                0, #october 2022
                                                0, #november 2022
                                                0, #december 2022
                                                0, #january 2023
                                                0, #february 2023
                                                0, #march 2023
                                                0, #april 2023
                                                0, #may 2023
                                                0, #june 2023
                                                0, #july 2023
                                                0, #august 2023
                                                0, #september 2023
                                                0, #october 2023
                                                0, #november 2023
                                                0, #december 2023
                                                0, #january 2024
                                                0, #february 2024
                                                0, #march 2024
                                                0)) #april 2024


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
    print(cell_ref)
    #get df with correct cell reference
    beds_df_temp <- read_data(beds_files[i], cell_ref, backseries)
    print(sprintf("sucessfully read in file %s", beds_files[i]))
    #View(beds_df_temp)
    #break
    temp_list[[i]] <- beds_df_temp}
  return(temp_list)
}

nat_excel_list <- list_excels(NAT_CELL_REF_DF, CREATE_BACKSERIES)

# Iterate through each dataframe in the list and wrangle to get
# beds data --------

wrangle_sheets <- function(excel_list, cell_ref_df){
  for(i in c(1: length(excel_list))){
    # iterate through each month
    df <- excel_list[[i]]
    # cell_ref_df$ignore_rows[i] gets ignore_rows in cell reference df
    print("-------")
    print(paste0("ignore rows: ",cell_ref_df$ignore_rows[i]))
    print(paste0("cut up to row: ", dim(df)[1]))
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
    #View(df)
    # update / overwrite original list
    excel_list[[i]] <- df}
  return(excel_list)
}

nat_excel_list_formatted <- wrangle_sheets(nat_excel_list, NAT_CELL_REF_DF)

#combine rows
nat_beds_long <- nat_excel_list_formatted %>%
  bind_rows() %>%
  mutate(trust_name = str_to_title(trust_name),
         region = str_to_title(region),
         trust_code = "ENG") %>%
  select(-region)

#Check correct number of rows
length(beds_files) == dim(nat_beds_long)[1]

if(CREATE_BACKSERIES == TRUE){
  # overwrite backseries
  write.csv(x = nat_beds_long, 
            file = paste0('data/beds/backseries/beds_national_backseries','.csv'), 
            row.names = FALSE)
} else{
  national_backseries_and_new <- rbind(national_backseries, nat_beds_long)
  #read in Carl's national nctr data csv file
  nat_nctr_daily <- readxl::read_xlsx(path = "./data/NCTR/20240411_March_2024_NCTR_briefing.xlsx",
                                      sheet = "Daily Series - March 2024",
                                      range = "B5:R1101") 
  
  nat_nctr_daily <- nat_nctr_daily %>%
    clean_names() %>%
    select(date, 
           number_of_patients_remaining_in_hospital_who_no_longer_meet_the_criteria_to_reside,
           seven_dra_of_number_of_patients_remaining_in_hospital_who_no_longer_meet_the_criteria_to_reside) #7DRA column
  
  # Add floor month in prep for join ----
  
  add_floor_month <- function(data){
    data <- data %>%
      mutate(floor_month = floor_date(ymd(date), "month"))
  }
  
  nat_nctr_daily <- add_floor_month(nat_nctr_daily)
  
  #join beds and alter column headers for write ----
  by_month <- join_by(floor_month)
  nat_nctr_beds_final <- nat_nctr_daily %>%
    left_join(national_backseries_and_new,
              by = by_month) %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= "2022-07-01") %>% 
    mutate(trust_name = "ENGLAND") %>%
    rename(`Org Code` = trust_code,
           `Org Name` = trust_name,
           `NCTR Value` = number_of_patients_remaining_in_hospital_who_no_longer_meet_the_criteria_to_reside,
           `NCTR 7-day RA` = seven_dra_of_number_of_patients_remaining_in_hospital_who_no_longer_meet_the_criteria_to_reside,
           `G&A beds` = beds,
           Date = date) %>%
    select(`Org Name`, Date, `NCTR Value`, `G&A beds`, `NCTR 7-day RA`)
  
  #write output
  writexl::write_xlsx(x = nat_nctr_beds_final, path = paste0('output/national_nctr_beds.xlsx'))
}


