# Import packages --------------------------------------------------------

library(tidyverse)
library(janitor)
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))
source(file = "./functions.R")

# Set working directory 
setwd("~/../../Department of Health and Social Care/NW005 - DischargeAnalysisCenter/Analysis Projects/20240129 - NCTR Published - Briefing Tool/Code/")

# Create vectors for later use from file names ----------------------------
# Only ICB level data in published files from Aug 2023 - current
beds_files <- list.files(path = 'data/beds/icb/')
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

ICB_CELL_REF_DF <- data.frame(month_year = year_month_vec,
                              cell_ref = c("B15:O67", #one per month
                                           "B15:O67",
                                           "B15:O67", 
                                           "B15:O67",
                                           "B15:O67", 
                                           "B15:O67",
                                           "B15:O67"),
                              ignore_rows = c(11, 
                                              11, 
                                              11, 
                                              11, 
                                              11, 
                                              11,
                                              11))

# Read data function ------------------------------------------------------

read_data <- function(file_name, cell_ref){
  df <- readxl::read_xlsx(path = paste0("./data/beds/icb/", file_name),
                          sheet = 1,
                          range = cell_ref,
                          na = "-")}


# Create list of excels, one item per month -------------------------------

list_excels <- function(df){
  #create empty list
  temp_list <- list()
  #iterate through files in beds/ using index
  for(i in c(1:length(beds_files))){
    #obtain cell references from df
    cell_ref <-  df$cell_ref[df$month_year==year_month_vec[i]]
    #get df with correct cell reference
    beds_df_temp <- read_data(beds_files[i], cell_ref)
    print(sprintf("sucessfully read in file %s", beds_files[i]))
    temp_list[[i]] <- beds_df_temp}
  return(temp_list)
}

icb_excel_list <- list_excels(ICB_CELL_REF_DF)

# Iterate through each dataframe in the list and wrangle to get
# beds data --------

wrangle_sheets <- function(excel_list, cell_ref_df, type){
  for(i in c(1: length(excel_list))){
    # iterate through each month
    df <- excel_list[[i]]
    # cell_ref_df$ignore_rows[i] gets ignore_rows in cell reference df
    df <- df[c(cell_ref_df$ignore_rows[i]:dim(df)[1]),]
    # rename columns
    if(type == "icb"){
      names(df)[1:3] <- c('region', 'icb_code', 'icb_name')
    }
    else{
      names(df)[1:3] <- c('region', 'trust_code', 'trust_name')
    }
    # get beds
    df$`Adult G&A beds available` <- as.numeric(df$`Adult G&A beds available`)
    df[['beds']] <- df[['Adult G&A beds available']] - df[['Adult G&A covid void beds']]
    # add floor date per month for join with NCTR
    df[['floor_month']] <- as.Date(month_floor_vec[i])
    # keep selected columns
    if(type == "icb"){
      df <- df %>% select('region', 'icb_code', 'icb_name', 'beds', 'floor_month')
    }
    else{
      df <- df %>% select('region', 'trust_code', 'trust_name', 'beds', 'floor_month')
    }
    # update / overwrite original list
    excel_list[[i]] <- df}
  return(excel_list)
}

icb_excel_list_formatted <- wrangle_sheets(icb_excel_list, ICB_CELL_REF_DF, "icb")

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
write.csv(x = icb_beds_long, file = paste0('output/monthly_beds_icb_', date_today,'.csv'), row.names = FALSE)

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
