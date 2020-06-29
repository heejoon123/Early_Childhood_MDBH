#' Name: Heejoon Ahn
#' Date : June 26, 2020
#' Making functions for actigraph data adjusted for Emond Lab

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

############## SET 2: RAW agd FILES READ-IN = pilot study ############## 

#' The following two lines are the typical ways to read in the files.
#' Because the agd files are already converted into 60-second epochs and in excel files,
#' the files were manually edited outside so that the copies of data files
#' removed the first row, which is the name of the files.
#' 

#' Setting the file path
file_path <-
  "~/Desktop/Research/Emond Lab/Copy of Data/raw_actigraph_epoch60/K01M005 AGD/K01M005.csv"

#' reading in the file
k01m005.df <- read.csv(file=file_path,header = TRUE)

#' checking the dates of the data file to double check we have all dates
#' of interest and to make sure what years the subject was observed
check_dates <- function(df){
  df_dates <- unique(df$date)
  return(df_dates)
}
# check_dates(k01m005.df)

#' function made to check that there is no missing data in the file
#' this should return a table of how many NA values there are in each column. 
check_no_NA <- function(df){
  numNA <- colSums(is.na(df))
  return(numNA)
}
#check_no_NA(k01m043)

#### NOTES SO FAR ####
#' The two sets of functions should ultimately return the same output of 
#' data frames so that the firt column is the timestamp called "epochs" and the 
#'  second column is the "counts".
#'  This second version sets a max to the counts for threshold purposes

extract_sadeh.df <- function(df, col1=1, col2=5, id_year=2019){
  sadeh.df <- df[,col1:col2]
  # add the counts column from the vector of counts based off of the values from axis1
  # these are the counts used to calculate the parameters for the PS (Probability of 
  # Sleep) value. 
  sadeh.df$counts <- sadeh.df$axis1
  sadeh.df <- sadeh.df[,c(1:2, 6)]
  # converting date and time column to one column called timestamp
  sadeh.df$date <- as.Date(sadeh.df$date, format = "%m/%d/%Y")
  year(sadeh.df$date) <- id_year
  sadeh.df$epoch <- as.POSIXct(sadeh.df$epoch, format = "%I:%M:%S %p", tz = "EST")
  sadeh.df$epoch <- times(strftime(sadeh.df$epoch, format = "%H:%M:%S", tz = "EST"))
  sadeh.df$timestamp <- as.POSIXct(paste(sadeh.df$date, sadeh.df$epoch), 
                                   format="%Y-%m-%d %H:%M:%S", tz = "EST")
  # re-ordering columns so it returns newly generated timestamp epochs column
  # and the counts column only.
  sadeh.df <- sadeh.df[,c(4,3)]
  colnames(sadeh.df)[1] <- "epochs"
  
  return(sadeh.df)
}

#' testing the function and seeing if it works
prep.df <- extract_sadeh.df(k01m005.df, id_year=2019)


