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

############## SET 1: RAW agd FILES READ-IN ############## 

#' This first set of functions is to read in the raw agd files set into 
#' 10-second epochs by the actigraph device and extract only the wanted data files
#' by the appropriate 60-second epoch interval..
#' The second set of functions is to read in the pilot study data if conversion has 
#' already been done into a different file format 
#' (mainly for Emond Lab work only)
#' The last set of the functions will be for calculations of certain values of interest

#' read_agd = This function is set to read raw agd files and is derived from
#' the source: https://github.com/dipetkov/actigraph.sleepr/blob/master/R/read_agd.R
#' The following function is actually the read_agd_raw function from the document
#' as it gives the most known efficient way to do so.
#' 

#' Read an *.agd file, with no post-processing
#'
#' Read ActiGraph sleep watch data from an SQLite database stored in an AGD file and return a list with (at least) five tables: data, sleep, filters, settings, awakenings. The tables have the schema described in the ActiLife 6 User manual and the timestamps are converted from Unix time format to human-readable POSIXct representation.
#' @param file Full path to an agd file to read.
#' @param tz Time zone to convert DateTime ticks to POSIX time.
#' @return A list of five tables: settings, data, filters, sleep, awakenings and, if available, capsense.
#' @details
#' Some Actigraph devices contain a capacitive sensor to detect monitor removal when worn against the skin. If that data is available, the return list includes a capsense table as well.
#' @references ActiLife 6 User's Manual by the ActiGraph Software Department. 04/03/2012.
#' @references \code{covertagd}: R package for converting agd files from ActiGraph into data.frames.
#' @seealso \code{\link{read_agd}}
#' @examples
#' file <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
#'                     package = "actigraph.sleepr")
#' str(read_agd_raw(file))
#' @export
#' 

read_agd_raw <- function(file, tz = "EST") {
  
  assert_that(file.exists(file))
  
  # Connect to the *.agd database
  db <- DBI::dbConnect(RSQLite::SQLite(), dbname = file)
  
  # Get the names of all tables in the database
  query <- "SELECT name FROM sqlite_master WHERE type = 'table'"
  tables_agd <- db %>% tbl(sql(query)) %>% collect()
  tables_agd <- tables_agd$name
  tables_required <- c("data", "sleep", "awakenings", "filters", "settings")
  assert_that(all(tables_required %in% tables_agd))
  
  # Cast Unix time ticks to POSIXct date/time
  # Timestamps are stored as ticks since 12:00:00 midnight, January 1, 0001.
  # Each tick is one ten-millionth of a second and so ticks are of type INTEGER
  # (long int). R does not have a 64 bit integer type and timestamps overflow.
  # So divide by 10,000,000 and convert to date/time with STRFTIME, before
  # selecting these columns from the database.
  cast_dttms <- function(x) {
    if (length(x)) ymd_hms(x, tz = tz) else as.POSIXct(x)
  }
  select_dttms <- function(table, cols) {
    query <- "SELECT"
    for (col in cols) {
      # Start counting seconds from January 1st, 1970;
      # 62135596800 is the number of seconds elapsed
      # from 01/01/0001 00:00:00 to 01/01/1970 00:00:00
      query <- paste0(query,
                      " STRFTIME('%Y-%m-%d %H:%M:%S', ",
                      col, "/", "10000000 - 62135596800, ",
                      "'unixepoch') AS ", col, "_ts, ")
    }
    query <- paste0(query, " * FROM ", table)
    db %>% tbl(sql(query)) %>%
      collect(n = Inf) %>%
      select(- one_of(cols)) %>%
      rename_all(str_replace, "_ts$", "") %>%
      mutate_at(vars(cols), cast_dttms)
  }
  
  settings <- db %>% tbl("settings") %>% collect()
  data <- select_dttms("data", "dataTimestamp")
  sleep <- select_dttms("sleep", c("inBedTimestamp",
                                   "outBedTimestamp"))
  awakenings <- select_dttms("awakenings", "timestamp")
  filters <- select_dttms("filters", c("filterStartTimestamp",
                                       "filterStopTimestamp"))
  
  # The capsense table stores data from an optional wear sensor,
  # so it might not be present in the database.
  # The capsense table stores data from an optional wear sensor,
  # so it might not be present in the database.
  tables <- list(data = data, sleep = sleep, filters = filters,
                 settings = settings, awakenings = awakenings)
  if ("capsense" %in% tables_agd)
    tables$capsense <- select_dttms("capsense", "timeStamp")
  
  DBI::dbDisconnect(db)
  
  tables
}

#' the following is a test data from the full study to observe if this read_agd function
#' works properly. The following line is just the file path object.

file_path <- "~/Desktop/Research/Emond Lab/Copy of Data/full_study_agd/K01M013 (2019-09-04)10sec.agd"
file_path <- "~/Desktop/Research/Emond Lab/Copy of Data/Full Study/Baseline visits/K01M016 (2020-01-10)10sec.agd"
fdf <- read_agd_raw(file_path, tz="EST")
agd_data <- fdf$data

#' Because the data has the raw formatted data, this requires the conversion to 
#' 60-second epochs. This requires that the timestamp column to be POSIXct type 
#' the format of reading the agd file should automatically have this timestamp format
#' so there should be no issue. 
#' RETURNS: timestamp column and axis 1 column, which is the counts value.
#' 

epoch_60s <- function(file){
  colnames(file)[2] <- "counts"
  colnames(file)[1] <- "epochs"
  file %>%
    group_by(epochs = cut(epochs, breaks="1 min")) %>%
    summarize(counts = sum(counts))
}

# testing
agd_60s <- epoch_60s(agd_data)

# convert the epochs to timestamp
epoch_timestamp.df <- function(df){
  sadeh.df <- df[,1]
  # add the counts column from the vector of counts based off of the values from axis1
  # these are the counts used to calculate the parameters for the PS (Probability of 
  # Sleep) value. 
  sadeh.df$counts <- df$counts
  # converting date and time column to one column called timestamp
  sadeh.df$epochs <- as.POSIXct(sadeh.df$epochs, format="%Y-%m-%d %H:%M:%S", 
                                   tz = "EST")
  # return as dataframe instead of tbl format in R
  sadeh.df <- data.frame(sadeh.df)
  return(sadeh.df)
}

prep_df <- epoch_timestamp.df(agd_60s)

