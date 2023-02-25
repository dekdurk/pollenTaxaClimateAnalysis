# Load the necessary packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)


#Temporal trends in 10 pollen season metrics
## Maximum monthly count
## Mean daily count
## Median daily count
## Season start date*
## Season end date
## Season length
## Spring total count
## Summer total count
## Fall total count
## Annual total count


# Importing and Cleaning TotalPollenData_Daily.csv ----------------------------------------------

# Read in your data and store it in a data frame called 'dailyPollen_df'
dailyPollen_df <- read.csv("raw/TotalPollenData_Daily.csv")
dailyPollen_df <- dailyPollen_df[,-c(2,45,46)]
# remove columns if their name is "longitude", "latitude", "Total.Pollen.Count" 

dailyPollen_df$Date <- date(ymd_hms(dailyPollen_df$Date))
dailyPollen_df$year <- year(ymd(dailyPollen_df$Date)) #add years
dailyPollen_df$month <- month(ymd(dailyPollen_df$Date))

dailyPollen_df <- pivot_longer(dailyPollen_df, cols = 2:43, names_to = "species", values_to = "raw_daily_concentration")
dailyPollen_df <- dailyPollen_df %>% drop_na(raw_daily_concentration)


# Removing stations with less than 5 years of data ------------------------

# First, it groups the data by StationID and year, 
# calculates the average number of observations (Avg.NOBS) 
# for each combination of StationID and year, and then groups 
# the resulting data by StationID again to calculate the number
# of years (Nyears) for each StationID. This result is saved as 
# a data frame called "YearCount_Station_All".
YearCount_Station_All <- dailyPollen_df %>%
  group_by(StationID, year) %>%
  summarise(Avg.NOBS = mean(n()))%>%
  group_by(StationID) %>%
  summarise(Nyears = n())

# Next, the code repeats the same process but adds a filter that
# only includes StationID with more than 5 years of data, 
# saving the result as "YearCount_Station_GT5". 
# The third part of the code does the same thing, but includes only StationID 
# with 5 or fewer years of data, and saves the result as "YearCount_Station_LT5".
YearCount_Station_GT5 <- dailyPollen_df %>%
  group_by(StationID, year) %>%
  summarise(Avg.NOBS = mean(n()))%>%
  group_by(StationID) %>%
  filter(n_distinct(year) > 5) %>%
  group_by(StationID) %>%
  summarise(Nyears = n())
YearCount_Station_LT5 <- dailyPollen_df %>%
  group_by(StationID, year) %>%
  summarise(Avg.NOBS = mean(n()))%>%
  filter(n_distinct(year) <= 5) %>%
  group_by(StationID) %>%
  summarise(Nyears = n())

# # Finally, the code saves each of these data frames as 
# # a CSV file in a specified location using the write.csv() function.
# write.csv(YearCount_Station_All, "Outputdata/YearCount_Station_All.csv", row.names=FALSE)
# write.csv(YearCount_Station_GT5, "Outputdata/YearCount_Station_GT5.csv", row.names=FALSE)
# write.csv(YearCount_Station_LT5, "Outputdata/YearCount_Station_LT5.csv", row.names=FALSE)

# Removing stations <5yrs
dailyPollen_df <- dailyPollen_df[!dailyPollen_df$StationID %in% YearCount_Station_LT5$StationID, ]


# Linear Interpolation ----------------------------------------------------

year_dates <- data.frame(Date = seq.Date(ymd("2000-01-01"), ymd("2018-12-30"), by = 1))

# Merge the year_dates data frame with the dailyPollen_df data frame to fill in any missing dates
pollen_data_all_dates <- dailyPollen_df %>% 
  group_by(StationID,species)%>%
  complete(Date = year_dates$Date) %>% 
  fill(raw_daily_concentration) 

# Interpolate missing values using linear interpolation
pollen_data_interpolated <- pollen_data_all_dates %>% 
  group_by(species, station, year) %>% 
  mutate_at(vars(raw_daily_concentration), ~ ifelse(is.na(.), NA, approx(seq_along(.), ., seq_along(.), method = "linear")$y))

# Remove any rows with missing data
pollen_data_interpolated <- pollen_data_interpolated[complete.cases(pollen_data_interpolated), ]
ApproxFun <- approxfun(x = ydate$Date, y = dailyPollen_df$raw_daily_concentration)

LinearFit <- ApproxFun(Dates)

ggplot() +
  geom_point(data = dailyPollen_df, aes(x = Date, y = raw_daily_concentration)) +
  geom_line(aes(x = Dates, y = LinearFit), color = "blue")




# Season Integrals by SPI>0.05 (Clot, 2003) -------------------------------

# Calculate the 30th percentile of daily concentration measurements for each species
p <- pollen_data_interpolated %>% summarise(threshold = quantile(raw_daily_concentration, 0.3))

# Define a function to determine the start and end dates of the pollen season for each species
get_pollen_season <- function(data) {
  start_date <- data$Date[min(which(data$raw_daily_concentration > data$threshold))]
  end_date <- data$Date[max(which(data$raw_daily_concentration > data$threshold)) + 1]
  return(c(start_date, end_date))
}

# Apply the get_pollen_season function to each species
pollen_seasons <- pollen_data_long %>% group_by(species,year) %>% summarise(start_date = get_pollen_season(cur_data)[1],
                                                                  end_date = get_pollen_season(cur_data)[2])


# Season Integrals by 4 consecutive days (Ziska et al 2019) ---------------

# (Ziska et al 2019) "we used a metric of 4 consecutive days of pollen collected (with the fourth day considered  the  start  of  the  pollen  season)  and  the  last  period of 4 consecutive days (with the last day of the 4-day period  considered  the  end  of  the  pollen  season),  as  suggested by Aerobiology Research Laboratories, Canada (FC,unpublished)."

save(dailyPollen_df, file = "dailyPollen_df.RData" )

load(file = "dailyPollen_df.RData")
# create an empty data frame to store the start dates
startDate <- data.frame(year = integer(), species = character(), StationID = character(), Date = character(), stringsAsFactors = FALSE)

# loop over unique combinations of year, species, and station ID
for (yr in unique(dailyPollen_df$year)) {
  for (sp in unique(dailyPollen_df$species)) {
    for (st in unique(dailyPollen_df$StationID)) {
      
      # subset the data for the current year, species, and station ID
      subset <- dailyPollen_df[dailyPollen_df$year == yr & dailyPollen_df$species == sp & dailyPollen_df$StationID == st, ]
      
      # if the subset is empty, skip the loop
      if (nrow(subset) == 0) {
        next
      }
      
      # initialize a counter for the number of consecutive days with raw_daily_concentration > 0
      count <- 0
      
      # loop over the rows in the subset
      for (i in 1:nrow(subset)) {
        
        # if raw_daily_concentration > 0, increment the counter
        if (subset[i, "raw_daily_concentration"] > 0) {
          count <- count + 1
          
          # if we have 4 consecutive days with raw_daily_concentration > 0, save the start date
          if (count == 4) {
            startDate <- rbind(startDate, data.frame(year = yr, species = sp, StationID = st, Date = subset[i - 3, "Date"], stringsAsFactors = FALSE))
            break
          }
        } else {
          # if raw_daily_concentration <= 0, reset the counter
          count <- 0
        }
      }
    }
  }
}

save(startDate, file = "seasonSD.RData" )

yr = 2001
sp = "Acer"
st = "1-Eugene"

subset <- dailyPollen_df[dailyPollen_df$year == yr & dailyPollen_df$species == sp & dailyPollen_df$StationID == st, ]

count <- 0

# loop over the rows in the subset
for (i in 1:nrow(subset)) {
  
  # if raw_daily_concentration > 0, increment the counter
  if (subset[i, "raw_daily_concentration"] > 0) {
    count <- count + 1
    
    # if we have 4 consecutive days with raw_daily_concentration > 0, save the start date
    if (count == 4) {
      startDate <- rbind(startDate, data.frame(year = yr, species = sp, StationID = st, Date = subset[i - 3, "Date"], stringsAsFactors = FALSE))
      break
    }
  } else {
    # if raw_daily_concentration <= 0, reset the counter
    count <- 0
  }
}

save(startDate, file = "seasonSD.RData" )


# Spring total count, Summer total count, Fall total count, Annual count--------

# Creating Season by Month Catagories
months_spring <-
  c("February", "March", "April", "May") # February 1–May 31 for spring integrals

months_summer <-
  c("June", "July", "August") # June 1–August 31 for summer integrals

months_fall <-
  c("September", "October", "November")# September 1–November 30 for fall integrals

# January 1–December 31 for annual integrals

#Separating pc_data in season data frames

## Spring
spring_data <- dailyPollen_df %>%
  filter(Month %in% months_spring)

## Summer
summer_data <- dailyPollen_df %>%
  filter(Month %in% months_summer)

# Fall
fall_data <- dailyPollen_df %>%
  filter(Month %in% months_fall)

df <- dailyPollen_df %>%
  filter_at(vars(logAvgPol), all_vars(!is.infinite(.)))

# Displayed is the slope of log-transformed
# pollen metrics against time, back-transformed change over 2000-2018 in % for concentrations
# and days for start date, end date, and season length metrics, and the p-value of the mixed effects model.

m3 <- lmList(mean_daily_count ~ year|species , data = daily_mean)
mm3 <- lmList(log10(mean_daily_count+0.01) ~ year|species , data = daily_mean)

plot(m3)
plot(mm3)
coef(mm3)

summary(mm3)



# Calculate linear regression for each taxa daily mean against time -------

daily_mean <- dailyPollen_df %>%
  group_by(year, species) %>%
  summarise(mean_daily_count = mean(raw_daily_concentration))

# Calculate linear regression for each taxa daily mean against time
regression_results <- daily_mean %>%
  group_by(species) %>%
  do(model = lm(log10(mean_daily_count + 0.01) ~ year, data = .))

# Extract p-values, slopes, and r-squared values for each species
daily_mean_results_summary <- regression_results %>%
  summarize(
    species = first(species),
    p_value = summary(model)$coefficients[2,4],
    slope = summary(model)$coefficients[2,1],
    r_squared = summary(model)$r.squared
  )
# Calculate percent change in mean daily count per year for each species
percent_change <- daily_mean %>%
  group_by(species) %>%
  summarize(
    percent_change = ((exp(mean_daily_count[year == max(year)]) / exp(mean_daily_count[year == min(year)]))^(1/(max(year)-min(year))) - 1) * 100
  )

# Merge percent change with regression results
results_summary <- left_join(daily_mean_results_summary, percent_change, by = "species")
write.csv(results_summary,"Data/model_daily_mean.csv",
)


# Calculate linear regression for each taxa daily max against time --------

daily_max <- dailyPollen_df %>%
  group_by(year, species) %>%
  summarise(max_daily_count = max(raw_daily_concentration))

regression_results <- daily_max %>%
  group_by(species) %>%
  do(model = lm(log10(max_daily_count + 0.01) ~ year, data = .))

# Extract p-values, slopes, and r-squared values for each species
daily_max_results_summary <- regression_results %>%
  summarize(
    species = first(species),
    p_value = summary(model)$coefficients[2,4],
    slope = summary(model)$coefficients[2,1],
    r_squared = summary(model)$r.squared
  )
# Calculate percent change in mean daily count per year for each species
percent_change <- daily_max %>%
  group_by(species) %>%
  summarize(
    percent_change = ((exp(max_daily_count[year == max(year)]) / exp(max_daily_count[year == min(year)]))^(1/(max(year)-min(year))) - 1) * 100
  )

# Merge percent change with regression results
results_summary <- left_join(daily_max_results_summary, percent_change, by = "species")
write.csv(results_summary,"Data/model_daily_max.csv",
)

library(ggpmisc) # version >= 0.3.4 !!

#Trends in Mean Daily Count by Year for Multiple Pollen Taxa
ggplot(daily_mean, aes(x = year, y = log10(mean_daily_count+0.01), group = species)) +
  geom_smooth(method="lm")+
  geom_point()+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,
               label.x.npc = "right",
               vstep = 0.05) +
  facet_wrap(~species)

# Removing stations with less than 5 years of data ------------------------

YearCount_Station_LT5 <- dailyPollen_df %>%
  group_by(StationID, year) %>%
  summarise(Avg.NOBS = mean(n()))%>%
  filter(n_distinct(year) <= 5) %>%
  group_by(StationID) %>%
  summarise(Nyears = n())

# Removing stations <5yrs
dailyPollen_df <- dailyPollen_df[!dailyPollen_df$StationID %in% YearCount_Station_LT5$StationID, ]

rm(YearCount_Station_LT5)



# Identifying the Start Dates of Pollen Seasons by Species and Year -------

# This code creates an empty data frame startDate to store the start dates of pollen seasons for each combination of year, species, and station ID in the dailyPollen_df data frame. The code loops over each unique combination of year and species and subsets the data for the current year and species. The subset is then aggregated by date and the sum of the raw_daily_concentration column is calculated for each date. The code then loops over the rows in the subset, counting the number of consecutive days with raw_daily_concentration > 0. If there are four consecutive days with raw_daily_concentration > 0, the start date is saved to the startDate data frame. The resulting startDate data frame will have one row for each pollen season, with the start date, year, and species for each season.

# create an empty data frame to store the start dates
startDate <- data.frame(year = integer(), species = character(), Date = character(), stringsAsFactors = FALSE)

# loop over unique combinations of year, species, and station ID
for (yr in unique(dailyPollen_df$year)) {
  for (sp in unique(dailyPollen_df$species)) {
    
    # subset the data for the current year, species
    subset <- dailyPollen_df[dailyPollen_df$year == yr & dailyPollen_df$species == sp, ]
    subset <- aggregate(raw_daily_concentration ~ Date, data = subset, FUN = sum)
    # if the subset is empty, skip the loop
    if (nrow(subset) == 0) {
      next
    }
    
    # initialize a counter for the number of consecutive days with raw_daily_concentration > 0
    count <- 0
    
    # loop over the rows in the subset
    for (i in 1:nrow(subset)) {
      
      # if raw_daily_concentration > 0, increment the counter
      if (subset[i, "raw_daily_concentration"] > 0) {
        count <- count + 1
        
        # if we have 4 consecutive days with raw_daily_concentration > 0, save the start date
        if (count == 4) {
          startDate <- rbind(startDate, data.frame(year = yr, species = sp, Date = subset[i - 3, "Date"], stringsAsFactors = FALSE))
          break
        }
      } else {
        # if raw_daily_concentration <= 0, reset the counter
        count <- 0
      }
    }
  }
}

# save(startDate, file = "seasonSD.RData" )
# load("seasonSD.RData
#      ")

# Identifying the End Dates of Pollen Seasons by Species and Year -------

# create an empty data frame to store the end dates
endDate <- data.frame(year = integer(), species = character(), Date = character(), stringsAsFactors = FALSE)

# loop over unique combinations of year and species
for (yr in unique(dailyPollen_df$year)) {
  for (sp in unique(dailyPollen_df$species)) {
    
    # subset the data for the current year and species
    subset <- dailyPollen_df[dailyPollen_df$year == yr & dailyPollen_df$species == sp, ]
    subset <- aggregate(raw_daily_concentration ~ Date, data = subset, FUN = sum)
    # if the subset is empty, skip the loop
    if (nrow(subset) == 0) {
      next
    }
    
    # initialize a counter for the number of consecutive days with raw_daily_concentration > 0
    count <- 0
    # initialize a variable to store the last date with raw_daily_concentration > 0
    last_date <- ""
    
    # loop over the rows in the subset in reverse order
    for (i in nrow(subset):1) {
      # if raw_daily_concentration > 0, increment the counter and update the last_date variable
      if (subset[i, "raw_daily_concentration"] > 0) {
        count <- count + 1
        if (count == 4) {
          endDate <- rbind(endDate, data.frame(year = yr, species = sp, Date = last_date, stringsAsFactors = FALSE))
          break
        }
        last_date <- subset[i, "Date"]
      } else {
        # if raw_daily_concentration <= 0, reset the counter and last_date variable
        count <- 0
        last_date <- ""
      }
    }
  }
}

# save(startDate, file = "seasonED.RData" )

# Difference Between Pollen Season Start and End Dates by Taxa and --------


# merge the startDate and endDate data frames by year and species
pollenSeasons <- merge(startDate, endDate, by = c("year", "species"))

# rename the Date column to start_date and end_date
colnames(pollenSeasons) <- c("year", "species", "start_date", "end_date")

# calculate the difference in days between start_date and end_date
pollenSeasons$duration <- as.numeric(as.Date(pollenSeasons$end_date) - as.Date(pollenSeasons$start_date))

# plot the duration of each pollen season for each species against year
ggplot(pollenSeasons, aes(x = year, y = log10(duration))) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~species) +
  labs(x = "Year", y = "Duration (days)") +
  theme_bw() +
  ggtitle("Difference Between Pollen Season Start and End Dates by Taxa and Year")

pollenSeasons_lm <- lm(log10(duration) ~ year + species, data = pollenSeasons)
pollenSeasons_lmsq <- lm(sqrt(duration) ~ year + species, data = pollenSeasons)
pollenSeasons_lmreq <- lm(duration ~ year + species, data = pollenSeasons)
AIC(pollenSeasons_lmlg,pollenSeasons_lmsq,pollenSeasons_lmreq)

pollenSeasons_summary <- summary(pollenSeasons_lm)$coefficients %>%
  as.data.frame() %>%
  rename(slope = Estimate, "Std. Error" = `Std. Error`, "t-value" = `t value`, "p-value" = `Pr(>|t|)`) %>%
  rownames_to_column(var = "Taxa")

pollenSeasons_summary$Taxa <- gsub("species", "", pollenSeasons_summary$Taxa)
pollenSeasons_summary$Taxa <- gsub("\\.", " ", pollenSeasons_summary$Taxa)

pollenSeasons_pval0.05 <- pollenStartDates_summary[pollenStartDates_summary$"p-value" < 0.05, ]
pollenSeasons_pval0.05 <- pollenStartDates_pval0.05[!(pollenStartDates_pval0.05$Taxa %in% c("(Intercept)", "year")), ]

write_csv(pollenSeasons, file = "Outputdata/pollenSeasons.csv")
write_csv(pollenSeasons_pval0.05, file = "Outputdata/pollenSeasons_pval0.05.csv")

spring_data_Pval <- pollenSeasons %>%
  filter(species %in% pollenStartDates_pval0.05$Taxa)

ggplot(pollenSeasons, aes(x = year, y = log10(duration), color = species)) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm",se=FALSE) + 
  labs(x = "Year", y = bquote(log[10]~(Duration))) +
  ggtitle("Pollen Season Duration (p<0.05)")

ggsave("Pollen Season Duration (p<0.05) Layered.jpeg",
       device = "jpeg", width=  13, height = 7.5, units="in")

ggplot(pollenSeasons, aes(x = year, y = log10(duration), color = species)) +
  geom_point(alpha = 0.8)+
  geom_smooth(method = "lm") + 
  facet_wrap(~species)+
  labs(x = "Year", y = bquote(log[10]~(Duration))) +
  theme(legend.position = "none")+
  ggtitle("Pollen Season Duration (p<0.05)")
ggsave("Pollen Season Duration (p<0.05) Facet.jpeg",
       device = "jpeg", width=  13, height = 7.5, units="in")







# pollenStartDates_lm -----------------------------------------------------


pollenStartDates_lmlg <- lm(log10(yday(Date)) ~ year + species, data = startDate)
pollenStartDates_lmsq <- lm(sqrt(yday(Date)) ~ year + species, data = startDate)
AIC(pollenStartDates_lmlg, pollenStartDates_lmsq)

pollenStartDates_summary <- summary(pollenStartDates_lmlg)$coefficients %>%
  as.data.frame() %>%
  rename(slope = Estimate, "Std. Error" = `Std. Error`, "t-value" = `t value`, "p-value" = `Pr(>|t|)`) %>%
  rownames_to_column(var = "Taxa")

pollenStartDates_summary$Taxa <- gsub("species", "", pollenStartDates_summary$Taxa)
pollenStartDates_summary$Taxa <- gsub("\\.", " ", pollenStartDates_summary$Taxa)


pollenStartDates_pval0.05 <- pollenStartDates_summary[pollenStartDates_summary$"p-value" < 0.05, ]
pollenStartDates_pval0.05 <- pollenStartDates_pval0.05[!(pollenStartDates_pval0.05$Taxa %in% c("(Intercept)", "year")), ]
write_csv(pollenSeasons_summary, file = "Outputdata/pollenStartDates_pval0.05.csv")

ggplot(startDate, aes(x = year, y = yday(Date), color = species)) +
  geom_smooth(method = "lm") + 
  labs(x = "Year", y = "Start Date") +
  scale_y_date(date_labels = "%b %d", date_breaks = "1 month") +
  theme_bw() +
  ggtitle("Pollen Season Start Dates by Taxa and Year")

ggplot(startDate, aes(x = year, y = sqrt(yday(Date)), color = species)) +
  geom_smooth(method = "lm") + 
  face
labs(x = "Year", y = "Start Date") +
  theme_bw() +
  ggtitle("Pollen Season Start Dates by Taxa and Year")

ggplot(startDate, aes(x = year, y = log10(yday(Date)))) +
  geom_smooth(method = "lm") + 
  facet_wrap(~species)+
  labs(x = "Year", y = "Start Date") +
  theme_bw() +
  ggtitle("Pollen Season Start Dates by Taxa and Year")




spring_data_Pval <- startDate %>%
  filter(species %in% pollenStartDates_pval0.05$Taxa)

ggplot(spring_data_Pval, aes(x = year, y = log10(yday(Date)), color = species)) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm",se=FALSE) + 
  labs(x = "Year", y = bquote(log[10]~(Start~Date))) +
  ggtitle("Pollen Season Start Date (p<0.05)")+
  theme_classic()

ggsave("Pollen Season Start Date (p<0.05) Layered.jpeg",
       device = "jpeg", width=  13, height = 7.5, units="in")

ggplot(spring_data_Pval, aes(x = year, y = log10(yday(Date)), color = species)) +
  geom_point(alpha = 0.8)+
  geom_smooth(method = "lm") + 
  facet_wrap(~species)+
  labs(x = "Year", y = bquote(log[10]~(Start~Date))) +
  theme(legend.position = "none")+
  ggtitle("Pollen Season Start Date (p<0.05)")
ggsave("Pollen Season Start Date (p<0.05) Facet.jpeg",
       device = "jpeg", width=  13, height = 7.5, units="in")



# pollenEndDates_lm -----------------------------------------------------

startDate <- endDate

pollenEndDates_lmlg <- lm(log10(yday(Date)) ~ year + species, data = startDate)
pollenStartDates_lmsq <- lm(sqrt(yday(Date)) ~ year + species, data = startDate)
AIC(pollenStartDates_lmlg, pollenStartDates_lmsq)

pollenStartDates_summary <- summary(pollenStartDates_lmlg)$coefficients %>%
  as.data.frame() %>%
  rename(slope = Estimate, "Std. Error" = `Std. Error`, "t-value" = `t value`, "p-value" = `Pr(>|t|)`) %>%
  rownames_to_column(var = "Taxa")

pollenStartDates_summary$Taxa <- gsub("species", "", pollenStartDates_summary$Taxa)
pollenStartDates_summary$Taxa <- gsub("\\.", " ", pollenStartDates_summary$Taxa)


pollenStartDates_pval0.05 <- pollenStartDates_summary[pollenStartDates_summary$"p-value" < 0.05, ]
pollenStartDates_pval0.05 <- pollenStartDates_pval0.05[!(pollenStartDates_pval0.05$Taxa %in% c("(Intercept)", "year")), ]
write_csv(pollenSeasons_summary, file = "Outputdata/pollenEndDates_pval0.05.csv")

ggplot(startDate, aes(x = year, y = yday(Date), color = species)) +
  geom_smooth(method = "lm") + 
  labs(x = "Year", y = "Start Date") +
  scale_y_date(date_labels = "%b %d", date_breaks = "1 month") +
  theme_bw() +
  ggtitle("Pollen Season Start Dates by Taxa and Year")

ggplot(startDate, aes(x = year, y = sqrt(yday(Date)), color = species)) +
  geom_smooth(method = "lm") + 
labs(x = "Year", y = "Start Date") +
  theme_bw() +
  ggtitle("Pollen Season Start Dates by Taxa and Year")

ggplot(startDate, aes(x = year, y = log10(yday(Date)))) +
  geom_smooth(method = "lm") + 
  facet_wrap(~species)+
  labs(x = "Year", y = "End Date") +
  theme_bw() +
  ggtitle("Pollen Season End Dates by Taxa and Year")






spring_data_Pval <- endDate %>%
  filter(species %in% pollenStartDates_pval0.05$Taxa)

ggplot(spring_data_Pval, aes(x = year, y = log10(yday(Date)), color = species)) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm",se=FALSE) + 
  labs(x = "Year", y = bquote(log[10]~(End~Date))) +
  ggtitle("Pollen Season End Date (p<0.05)")

ggsave("Pollen Season End Date (p<0.05) Layered.jpeg",
       device = "jpeg", width=  13, height = 7.5, units="in")

ggplot(spring_data_Pval, aes(x = year, y = log10(yday(Date)), color = species)) +
  geom_point(alpha = 0.8)+
  geom_smooth(method = "lm") + 
  facet_wrap(~species)+
  labs(x = "Year", y = bquote(log[10]~(End~Date))) +
  theme(legend.position = "none")+
  ggtitle("Pollen Season End Date (p<0.05)")
ggsave("Pollen Season End Date (p<0.05) Facet.jpeg",
       device = "jpeg", width=  13, height = 7.5, units="in")




