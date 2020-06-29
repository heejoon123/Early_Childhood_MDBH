#' Name: Heejoon Ahn
#' Date : June 26, 2020
#' Making functions for actigraph data adjusted for Emond Lab that is directly derived from the 
#' device

#' Load the necessary libraries for all functions that will be applied to the 
#' package that will ulimately be generated.
library(zoo)
library(RcppRoll)
library(dplyr)
library(tidyselect)
library(tidyr)
library(tidyverse)
library(devtools)
library(readr)
library(tools)
library(assertthat)
library(lubridate)
library(chron)

############## SET 4: Values of Interest ############## 
#' extract time points of interest function : put in row 1 and row 2 values
#' returns the counts for the time of interest 
#' df = dataframe with the days of interest
#' row1 and row2 values are the timepoints that are the "in bed" and "out of bed"
#' times recorded by the parent. 
#' 

bedtime_counts <- function(df,in_bed, out_bed){
  date_log <- df[in_bed:out_bed,]
  return(date_log$counts)
}


# testing
bedtime_counts(km5, 271, 931)
bedtime_counts(km13, 17641, 18286)


#' calculating total counts function
#' the value is the total actigraphy counts summed together for the entire sleep period
total_counts <- function(df, in_bed, out_bed){
  date_log <- df[in_bed:out_bed,]
  counts <- date_log$counts
  n <- length(counts)
  sum_count <- sum(counts[-n])
  return(sum_count)
}

# testing
total_counts(k01m004, 685, 1315)
total_counts(km5, 271, 931)
total_counts(km13, 17641, 18286)
total_counts(km13, 19111, 19771)
total_counts(km16, 7803, 8343)

#' Find onset time function from times of interest from in-bed to out-bed
#' SLEEP ONSET = The first minute that the algorithm scores "asleep"
#' 

onset_time <- function(df, in_bed, out_bed){
  onset.df <- df[in_bed:out_bed,]
  onset <- match("S", onset.df$sleep)
  ot <- onset.df$epochs[onset]
  return(ot)
}

onset_time(km5, 271, 931)
onset_time(km13, 17641, 18286)
onset_time(km13, 19111, 19771)
onset_time(km16, 7803, 8343)

#' LATENCY = function to calculate the latency
#' latency = onset time - in bed (by minutes)
#' 

latency <- function(df, in_bed, out_bed){
  onset <- onset_time(df, in_bed, out_bed)
  bedtime <- df$epoch[in_bed]
  value <- difftime(onset,bedtime, units="mins")
  return(as.numeric(value))
}

latency(km5, 271, 931)
latency(km13, 17641, 18286)
latency(km13, 19111, 19771)
latency(km16, 7803, 8343)

#' TOTAL MINUTES IN BED : Total number of minutes recorded to be in bed
#' calculated from subtracting the times from out_bed time and in_bed time
#' 
total_in_bed <- function(df, in_bed, out_bed){
  value <- difftime(df[out_bed,"epochs"], df[in_bed,"epochs"], units="mins")
  return(as.numeric(value))
}

total_in_bed(km5, 271, 931)
total_in_bed(km13, 17641, 18286)
total_in_bed(km13, 19111, 19771)
total_in_bed(km16, 7803, 8343)

#' TOTAL SLEEP TIME: Total number of minutes recorded as "asleep"
#' calculated from summing all the minutes indicated as "asleep" during in_bed 
#' and out_bed times
#' EDIT 4/27/20: added sleep onset time into consideration
total_sleep_time <- function(df, in_bed, out_bed){
  bed <- df[in_bed:out_bed,]
  sleeping <- bed[which(bed$sleep=="S"),]
  total_min <- nrow(sleeping)
  tib <- total_in_bed(df, in_bed, out_bed)
  if (total_min > tib){
    total_min <- tib
  } else {
    total_min
  }
  return(total_min)
}

# testing 
total_sleep_time(km5, 271, 931)
total_sleep_time(km13, 17641, 18286)
total_sleep_time(km13_2, 17641, 18286)
total_sleep_time(km13, 19111, 19771)
total_sleep_time(km16, 7803, 8343)


#' EFFICIENCY  = total number of sleep minutes / total minutes in bed
#' function generates the value of how efficient the sleep is from the two previous 
#' functions
#' 
efficiency <- function(df, in_bed, out_bed){
  tst <- total_sleep_time(df, in_bed, out_bed)
  tib <- as.numeric(total_in_bed(df, in_bed, out_bed))
  value <- round(100*(tst/tib),2)
  if (value >= 100){
    value <- 100
    return(value)
  }else{
    return(value)
  }
}
# test 
efficiency(km5, 271, 931)
efficiency(km13, 17641, 18286)
efficiency(km13, 19111, 19771)
efficiency(km16, 7803, 8343)

#' AWAKENINGS = number of different awakening episodes as scored by algorithm
#' considered as the total number of awakenings in the night
#' 

#' Num_awakenings 2 :numA function
num_awakenings <- function(df, in_bed, out_bed){
  sleeptime <- df[in_bed:out_bed,]
  onset <- onset_time(df, in_bed, out_bed)
  o_idx <- which(sleeptime$epochs==onset)
  awake <- 0
  n <- nrow(sleeptime)
  for(i in o_idx:(n)){
    if(i != n){
      if(sleeptime$sleep[i-1]=="S" && sleeptime$sleep[i] == "W" ){
        awake <- awake + 1
      }
    } else {
      awake <- awake
    }
  }
  return(awake)
}

num_awakenings(km5, 271, 931)
num_awakenings(km13, 17641, 18286)
num_awakenings(km13, 19111, 19771)
num_awakenings(km16, 7803, 8343)

#' WAKE AFTER SLEEP ONSET = The total number of minutes the subject was awake 
#' after sleep onset occurred
#' Calculated by = # awakening x average awakening length or 
#' total minutes in bed - total sleep time - latency
#' 

waso <- function(df, in_bed, out_bed){
  waso_val <- total_in_bed(df, in_bed, out_bed) - total_sleep_time(df, in_bed, out_bed) -
    latency(df, in_bed, out_bed)
  return(ceiling(waso_val))
}

waso(km5, 271, 931)
waso(km13, 17641, 18286)
waso(km13, 19111, 19771)
waso(km16, 7803, 8343)

#' AVERAGE AWAKENINGS = The average length, in minutes, of all awakening episodes.
#' total minutes in bed - total sleep time to get the total number of minutes 
#' the child is considered to be awake. 
#' 
avg_awake <- function(df, in_bed, out_bed){
  avg_min <- waso(df, in_bed, out_bed) / num_awakenings(df, in_bed, out_bed)
  return(round(avg_min,2))
}

avg_awake(km5, 271, 931)
avg_awake(km13, 17641, 18286)
avg_awake(km13, 19111, 19771)
avg_awake(km16, 7803, 8343)



















