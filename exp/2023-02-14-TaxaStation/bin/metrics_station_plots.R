# The purpose of this experiment is to investigate the distribution of different pollen taxa at various station locations.
library(tidyverse)
library(lme4)

source("exp/2023-02-14-TaxaStation/bin/metrics_station_clean.R")
source("exp/2023-02-14-TaxaStation/bin/metrics_station_models.R")
ff <- regions(p_data)
lm_TaxaMaxCount_regions(r)
# define the file path
file_path <- "exp/2023-02-14-TaxaStation/raw/taxaStation_data.csv"

# check if the file exists
if (!file.exists(file_path)) {
  # call the function with the file path
  pollen.formatAndSave(pollen.import("raw/TotalPollenData_Daily.csv"))
}

p_data <- read.csv(file_path)

# Rename the columns in the p_data
names(p_data)[names(p_data) == "x.mean"] <- "mean"
names(p_data)[names(p_data) == "x.max"] <- "max"
names(p_data)[names(p_data) == "x.total"] <- "total"


#plots all stations over time (tp = time plot)
allStations.tp <- function(df) {
  ggplot(df, aes(
    x = year,
    y = log10(max + 0.01),
    color = taxa
  )) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap( ~ station)
}

allStations.tp(p_data)


mydata_max <- p_data %>%
  group_by(station, taxa)
model <-
  lmer(log10(max + 0.01) ~ year + (year |
                                     station), data = mydata_max)

# Predict the maximum of each taxa over time for each station using the fitted model
predictions <- predict(model, newdata = mydata_max)

# Add the predicted values to the original dataset
mydata_max$predicted <- predictions

# Create a line plot of the maximum of each taxa over time for each station, with the predicted values overlaid
ggplot(mydata_max, aes(
  x = year,
  y = log10(max + 0.01),
  color = taxa
)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "red", linetype = "dashed") +
  facet_wrap( ~ station)
labs(x = "Time", y = "Max Taxa") +
  theme_bw()


ggplot(p_data_filtered) +
  geom_smooth(method = "lm", aes(x = year, y = log_max, color = taxa)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(x = year, y = log_max, color = station),
    color = "black",
    linetype = "dashed"
  ) +
  facet_wrap( ~ station) +
  labs(x = "Time", y = "Max Taxa") +
  theme_bw()

#
ggplot(p_data_filtered) +
  geom_smooth(method = "lm", aes(x = year, y = log_max, color = station)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(x = year, y = log_max, color = taxa),
    color = "black",
    linetype = "dashed"
  ) +
  facet_wrap( ~ taxa) +
  labs(x = "Time", y = "Max Taxa") +
  theme_bw()

ggplot(r) +
  geom_smooth(method = "lm", aes(x = year, y = log10(mean_count+0.01), color = region)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(x = year, y = log10(mean_count+0.01), color = taxa),
    color = "black",
    linetype = "dashed"
  ) +
  facet_wrap( ~ taxa) +
  labs(x = "Time", y = "Max Taxa") +
  theme_bw()

r<- ff

ggplot(r) +
  geom_smooth(method = "lm", aes(x = year, y = log10(mean_count+0.01), color = taxa)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(x = year, y = log10(mean_count+0.01), color = region),
    color = "black",
    linetype = "dashed"
  ) +
  facet_wrap( ~ region) +
  labs(x = "Time", y = "Max Taxa") +
  theme_bw()

# Replace "asteraceae..excluding.ambrosia.and.artemisia." with "asteraceae (excluding ambrosia and artemisia)"
r$taxa <- gsub("(Excluding Ambrosia And Artemisia)
", "", r$taxa)

# Replace all "." in any taxa value with " "
r$taxa <- gsub("\\.\\.", " ", r$taxa)
r$taxa <- gsub("\\.", " ", r$taxa)

r$taxa <- str_to_title(r$taxa)

ggplot(r) +
  geom_smooth(method = "lm", aes(x = year, y = log10(mean_count+0.01), color = taxa),se = FALSE) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(x = year, y = log10(mean_count+0.01), color = region),
    color = "black",
    linetype = "dashed"
  ) +
  facet_wrap( ~ region) +
  labs(x = "Time", y = "log(avg.count/month)") +
  theme_classic()+ 
  theme(legend.position = "bottom", legend.title = element_blank()) 


# Select data for North Midwest region only
north_midwest_data <- subset(r, region == "North Midwest")
south_midwest_data <- subset(r, region == "South Midwest")
south_east_data <-subset(r, region == "Southeast")
west_data <- subset(r, region == "West")
north_east_data <- subset(r, region == "Northeast")

nmw <- ggplot(north_midwest_data ) +
  geom_smooth(method = "lm", aes(x = year, y = log_mean, color = taxa),se = FALSE) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(x = year, y = log_mean, color = region),
    color = "black",
    linetype = "dashed"
  ) +
  labs(x = "", y = "") +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank()) 
  

west <- ggplot(west_data) +
  geom_smooth(method = "lm", aes(x = year, y = log_mean, color = taxa),se = FALSE) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(x = year, y = log_mean, color = region),
    color = "black",
    linetype = "dashed"
  ) +
  labs(x = "", y = "") +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank()) 

north_east<- ggplot(north_east_data) +
  geom_smooth(method = "lm", aes(x = year, y = log_mean, color = taxa),se = FALSE) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(x = year, y = log_mean, color = region),
    color = "black",
    linetype = "dashed"
  ) +
  labs(x = "", y = "") +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank()) 

smidw<- ggplot(south_midwest_data) +
  geom_smooth(method = "lm", aes(x = year, y = log_mean, color = taxa),se = FALSE) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(x = year, y = log_mean, color = region),
    color = "black",
    linetype = "dashed"
  ) +
  labs(x = "", y = "") +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank())

south_east<- ggplot(south_east_data) +
  geom_smooth(method = "lm", aes(x = year, y = log_mean, color = taxa),se = FALSE) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(x = year, y = log_mean, color = region),
    color = "black",
    linetype = "dashed"
  ) +
  labs(x = "", y = "") +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank()) 

grid.arrange(west, nmw,smidw,north_east,south_east, nrow = 1)


ggplot(r) +
  geom_smooth(method = "lm", aes(x = year, y = log10(mean_count+0.01), color = region), se = FALSE) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(x = year, y = log10(mean_count+0.01), color = taxa),
    color = "black",
    linetype = "dashed"
  ) +
  facet_wrap( ~ taxa) +
  labs(x = "", y = "") +
  theme_bw()

# Group the data by station and taxa
p_data_groups <- p_data %>% group_by(station, taxa)

# Calculate the lengths of consecutive runs of log_max for each group
run_lengths <- p_data_groups %>%
  reframe(run_lengths = with(rle(log_max), lengths[values != 0]))

# Filter out taxa with fewer than 5 consecutive years of log_max
p_data_filtered <- p_data_groups %>%
  filter(taxa %in% run_lengths$taxa[run_lengths$run_lengths >= 5])

# Remove row names
rownames(p_data_filtered) <- NULL








####
####
####
# Create a logical vector indicating which rows to keep
keep_rows <- !grepl("_NA", pc_regions$region_taxa)

# Subset the data frame to keep only rows where keep_rows is TRUE
ff <- pc_regions[keep_rows, ]



ff <- ff[ff$mean_count != 0, ]

# Log-transform the max values and add 0.01
ff$log_mean <- log10(ff$mean_count + 0.01)

# Group the data by station and taxa
p_data_groups <- split(ff, list(ff$region_taxa))

# Define a function to calculate the p-value of a linear regression
# This function will be applied to each group using the 'lapply' function
# Define a function to calculate the p-value and slope of a linear regression
get_pvalue_slope <- function(group) {
  # Remove any rows with NA values
  group <- na.omit(group)
  
  # Return NA if there are less than 2 observations
  if (nrow(group) < 2) {
    return(c(NA, NA))
  }
  
  # Fit a linear model and extract the p-value and slope
  model <- lm(log_mean ~ mean_tMean, data = group)
  p_val <- summary(model)$coefficients[2,4]
  slope <- summary(model)$coefficients[2,1]
  
  return(c(p_val, slope))
}

# Apply the function to each group to obtain the p-values and slopes
p_values_slopes <- t(sapply(p_data_groups, get_pvalue_slope))

# Store the p-values and slopes in a data frame along with the corresponding region_taxa names
p_values_df <- data.frame(region_taxa = unique(ff$region_taxa),
                          p_value = p_values_slopes[, 1],
                          slope = p_values_slopes[, 2])
# Remove row names
rownames(p_values_df) <- NULL

# Remove rows with p-values > 0.05, NaN values, or negative slope
p_values_df <- p_values_df[!is.na(p_values_df$p_value) & p_values_df$p_value <= 0.05 & p_values_df$slope > 0, ]

# Filter the original data frame based on the selected region_taxa values
ff <- ff[ff$region_taxa %in% p_values_df$region_taxa, ]




# Replace "asteraceae..excluding.ambrosia.and.artemisia." with "asteraceae (excluding ambrosia and artemisia)"
ff$taxa <- gsub("asteraceae..excluding.ambrosia.and.artemisia.
", "asteraceae", ff$taxa)

# Replace all "." in any taxa value with " "
ff$taxa <- gsub("\\.\\.", " ", ff$taxa)
ff$taxa <- gsub("\\.", " ", ff$taxa)

ff$taxa <- str_to_title(ff$taxa)
ggplot(ff) +
  geom_smooth(method = "lm", aes(x = mean_tMean, y = mean_count, color = taxa), se = FALSE) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(x = mean_tMean, y = mean_count, color = region),
    color = "black",
    linetype = "dashed"
  ) +
  facet_wrap( ~ region) +
  labs(x = "", y = "") +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank()) 









# Create a logical vector indicating which rows to keep
keep_rows <- !grepl("_NA", r$region_taxa)
# Remove rows with "North Midwest" in region_taxa
ff <- ff[!grepl("North Midwest", ff$region_taxa), ]


# Subset the data frame to keep only rows where keep_rows is TRUE
ff <- r[keep_rows, ]



ff <- ff[ff$mean_count != 0, ]

# Log-transform the max values and add 0.01
ff$log_mean <- log10(ff$mean_count + 0.01)

# Group the data by station and taxa
p_data_groups <- split(ff, list(ff$region_taxa))

# Define a function to calculate the p-value of a linear regression
# This function will be applied to each group using the 'lapply' function
# Define a function to calculate the p-value and slope of a linear regression
get_pvalue_slope <- function(group) {
  # Remove any rows with NA values
  group <- na.omit(group)
  
  # Return NA if there are less than 2 observations
  if (nrow(group) < 2) {
    return(c(NA, NA))
  }
  
  # Fit a linear model and extract the p-value and slope
  model <- lm(log_mean ~ year, data = group)
  p_val <- summary(model)$coefficients[2,4]
  slope <- summary(model)$coefficients[2,1]
  
  return(c(p_val, slope))
}

# Apply the function to each group to obtain the p-values and slopes
p_values_slopes <- t(sapply(p_data_groups, get_pvalue_slope))

# Store the p-values and slopes in a data frame along with the corresponding region_taxa names
p_values_df <- data.frame(region_taxa = unique(ff$region_taxa),
                          p_value = p_values_slopes[, 1],
                          slope = p_values_slopes[, 2])
# Remove row names
rownames(p_values_df) <- NULL

# Remove rows with p-values > 0.05, NaN values, or negative slope
p_values_df <- p_values_df[!is.na(p_values_df$p_value) & p_values_df$p_value <= 0.05 & p_values_df$slope > 0, ]

# Filter the original data frame based on the selected region_taxa values
ff <- ff[ff$region_taxa %in% p_values_df$region_taxa, ]




# Replace "asteraceae..excluding.ambrosia.and.artemisia." with "asteraceae (excluding ambrosia and artemisia)"
ff$taxa <- gsub("asteraceae..excluding.ambrosia.and.artemisia.
", "asteraceae", ff$taxa)

# Replace all "." in any taxa value with " "
ff$taxa <- gsub("\\.\\.", " ", ff$taxa)
ff$taxa <- gsub("\\.", " ", ff$taxa)

ff$taxa <- str_to_title(ff$taxa)

ggplot(ff) +
  geom_smooth(method = "lm", aes(x = year, y = log_mean, color = taxa), se = FALSE) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(x = year, y = log_mean, color = region),
    color = "black",
    linetype = "dashed"
  ) +
  facet_wrap( ~ region) +
  labs(x = "Annual Temperature (°C)", y = bquote(log[10]~(avg. monthly pollen))) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank()) 

