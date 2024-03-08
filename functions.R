#Filter data for nctr metric and drop data prior to July as missing data
#in published data on the 19/06/2022 which causes issue in data from DAC
filter_nctr_and_date <- function(data){
  data <- data %>%
    filter(MetricName == "Number of patients remaining in hospital who no longer meet the criteria to reside",
           Date >= "2022-07-01")
}

#Check NAs in each column

check_proportion_nas <- function(data){
  nas_proprtion <- data %>%
    summarise_all(list(x = ~sum(is.na(.))/length(.)))
}