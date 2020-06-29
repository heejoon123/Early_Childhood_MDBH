#' Name: Heejoon Ahn
#' Date : June 28, 2020
#' 
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

#' Most of this code was generated from referring back to https://github.com/dipetkov/actigraph.sleepr/blob/master/R/apply_sadeh.R
#' for more information, please refer the link provided.

############## SET 3: SADEH ALGORITHM ############## 

#' making a function for calculating the Mean-W-5-min value
#' Mean-W-5-min value : Average # of activity counts during scored epoch & window
#' of 5 epochs preceding and following it.
meanw5min <- function(x, half_window=5) {
  zeros <- rep(0, half_window)
  roll_mean(c(zeros, x, zeros), n=2*half_window+1, partial=FALSE)
}

#' making a function for calculating the SD-last 6 min value
#' SD-last 6 min value : Standard deviation of activity counts during the scored
#' epoch and five epochs preceding it.
sdlast6min <- function(x, half_window=5){
  zeros <- rep(0, half_window)
  roll_sd(c(zeros, x), n = half_window + 1, partial = FALSE, align = "right")
}

#' making a function for calculating the NAT value
#' NAT value : # of epochs with activity level equal to or higher than 50 but 
#' lower than 100 activity counts in a window of 11 minutes that includes 
#' the scored epoch and 5 epochs preceding and following it. 
nat <- function(x, half_window=5){
  zeros <- rep(0, half_window)
  rule <- ifelse(x >= 50 & x < 100, 1, 0)
  roll_sum(c(zeros, rule, zeros), n = 2 * half_window + 1, partial = FALSE)
}

#' Providing the sleep status of the subject in the dataframe. 
#' S = Sleep
#' W = Awake
#' Based on Sadeh paper, the condition for PS is that if PS >= -4, then "S"
#' If PS < -4, then "W"
#' code that fixes the total sleep time 
prob_sleep <- function(data, counts=data$counts){
  data %>% 
    mutate(count = pmin(.data$counts, 300),
           score = (7.601
                    - 0.065 * meanw5min(.data$count)
                    - 1.08 * nat(.data$count)
                    - 0.056 * sdlast6min(.data$count)
                    - 0.703 * log(.data$count + 1)),
           sleep = if_else(.data$score > -4, "S", "W"))
}
