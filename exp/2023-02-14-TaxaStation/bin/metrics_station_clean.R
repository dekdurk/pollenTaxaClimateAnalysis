# The purpose of this experiment is to organize the data such that the pollen metrics of each taxa can be analyzed by station.

# Load the necessary packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# Read in your data and store it in a data frame called 'dailyPollen_df'
pollen.import <- function(filePath) {
  dailyPollen_df <- read.csv(filePath)
  dailyPollen_df <-
    subset(dailyPollen_df,
           select = -c(longitude, latitude, Total.Pollen.Count))
  
  names(dailyPollen_df) <- tolower(names(dailyPollen_df))
  names(dailyPollen_df)[names(dailyPollen_df) == "stationid"] <-
    "station"
  
  dailyPollen_df$date <- date(ymd_hms(dailyPollen_df$date))
  dailyPollen_df$year <- year(ymd(dailyPollen_df$date)) #add years
  dailyPollen_df$month <- month(ymd(dailyPollen_df$date))
  
  dailyPollen_df <-
    pivot_longer(
      dailyPollen_df,
      cols = 2:43,
      names_to = "taxa",
      values_to = "raw_daily"
    )
  dailyPollen_df <- dailyPollen_df %>% drop_na(raw_daily)
  
  return(dailyPollen_df)
}


#Removes rows with less than 10 daily pollen counts using the remove_short() function
#Calculates the mean and maximum pollen counts for each year and for each combination of taxa and station using the calculate_meanAndMax() function
#Writes the resulting data frame to a CSV file located at "exp/2023-02-14-TaxaStation/raw/taxaStation_data.csv" using the write.csv() function, with row names omitted.
pollen.formatAndSave <- function(df) {
  df <- remove_short(df)
  df <- calculate_pollen_stats(df)

  write.csv(df,
            "exp/2023-02-14-TaxaStation/raw/taxaStation_data.csv",
            row.names = FALSE)
}

#Functions for this script, not to be used by outside scripts ----

remove_short <- function(df) {
  # only includes Stations with more than 5 years of data
  YearCount_Station_LT5 <- df %>%
    group_by(station, year) %>%
    summarise(Avg.NOBS = mean(n())) %>%
    filter(n_distinct(year) <= 5) %>%
    group_by(station) %>%
    summarise(Nyears = n())
  
  # Removing stations <5yrs
  return(df[!df$station %in% YearCount_Station_LT5$station, ])
}

calculate_pollen_stats <- function(df) {
  # Extract taxa and station from input dataframe
  taxa <- unique(df$taxa)
  station <- unique(df$station)
  
  # Initialize an empty dataframe to store the results
  result_df <- data.frame()
  
  # split the input data frame by taxa and station
  df_list <- split(df, list(df$taxa, df$station))
  
  # define a function to calculate the mean and max pollen count for a single subset of the data frame
  # define a function to calculate the mean, max, and total pollen count for a single subset of the data frame
  calc_stats <- function(sub_df) {
    if (nrow(sub_df) > 0) {
      # calculate the mean, max, and total pollen count for each year
      stats_df <- aggregate(
        sub_df$raw_daily,
        by = list(year = format(sub_df$date, "%Y")),
        FUN = function(x)
          c(
            mean = mean(x),
            max = max(x),
            total = sum(x)
          )
      )
      
      # add columns for taxa and station to the stats_df
      stats_df$taxa <- sub_df$taxa[1]
      stats_df$station <- sub_df$station[1]
      
      return(stats_df)
    } else {
      return(NULL)
    }
  }
  
  # apply the calc_stats function to each subset of the data frame using lapply
  stats_list <- lapply(df_list, calc_stats)
  
  # filter out any NULL elements from the resulting list of data frames
  stats_list <-
    stats_list[sapply(stats_list, function(x)
      !is.null(x))]
  
  # combine the resulting list of data frames into a single data frame
  result_df <- do.call(rbind, stats_list)

 
  return(result_df)
}

