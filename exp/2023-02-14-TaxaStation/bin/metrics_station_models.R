#determining best linear regression models to use




lm_compare <- function(df,variable){
  
  aicModels <- AIC(regModel(df,variable), sqrtModel(df,variable), logModel(df,variable))
  return(aicModels)
}

regModel <- function(df, variable){
  
  df_lm_reg <- lm(sqrt(df[[variable]]) ~ year + taxa, data = df)
  
  df_lm_reg_sum <- summary(df_lm_reg)$coefficients %>%
    as.data.frame() %>%
    rename(slope = Estimate, "std.error" = `Std. Error`, "t-value" = `t value`, "p-value" = `Pr(>|t|)`) %>%
    rownames_to_column(var = "taxa")
  
  df_lm_reg_sum$taxa <- gsub("taxa", "", df_lm_reg$taxa)
  df_lm_reg$taxa <- gsub("\\.", " ", df_lm_reg$taxa)
  
  df_lm_reg_sum <- df_lm_reg[df_lm_reg_sum$"p-value" < 0.05, ]
  df_lm_reg <- df_lm_log_summary[!(df_lm_reg_sum$taxa %in% c("(Intercept)", "year")), ]
  
  return(df_lm_reg_sum)
  
}

sqrtModel <- function(df,variable){
  
  df_lm_sqrt <- lm(sqrt(df[[variable]]) ~ year + taxa, data = df)
  
  df_lm_sqrt_sum <- summary(df_lm_sqrt)$coefficients %>%
    as.data.frame() %>%
    rename(slope = Estimate, "std.error" = `Std. Error`, "t-value" = `t value`, "p-value" = `Pr(>|t|)`) %>%
    rownames_to_column(var = "taxa")
  
  df_lm_sqrt_sum$taxa <- gsub("taxa", "", df_lm_sqrt_sum$taxa)
  df_lm_sqrt_sum$taxa <- gsub("\\.", " ", df_lm_sqrt_sum$taxa)
  
  df_lm_log <- df_lm_sqrt_sum[df_lm_sqrt_sum$"p-value" < 0.05, ]
  df_lm_log <- df_lm_log_summary[!(df_lm_sqrt_sum$taxa %in% c("(Intercept)", "year")), ]
  return(df_lm_log)
}

logModel <- function(df,variable){
  
  df_lm_log <- lm(log10(df[[variable]]+0.01) ~ year + taxa, data = df)
  
  df_lm_log_summary <- summary(df_lm_log)$coefficients %>%
    as.data.frame() %>%
    rename("slope" = Estimate, "std.error" = `Std. Error`, "t-value" = `t value`, "p-value" = `Pr(>|t|)`) %>%
    rownames_to_column(var = "taxa")
  
  df_lm_log_summary$taxa <- gsub("taxa", "", df_lm_log_summary$taxa)
  df_lm_log_summary$taxa <- gsub("\\.", " ", df_lm_log_summary$taxa)
  
  df_lm_log <- df_lm_log_summary[df_lm_log_summary$"p-value" < 0.05, ]
  df_lm_log_summary <- df_lm_log_summary[!(df_lm_log_summary$taxa %in% c("(Intercept)", "year")), ]
  return(df_lm_log_summary)
  
}



lm_TaxaMaxCount_stations <- function(p_data) {
  # Log-transform the max values and add 0.01
  p_data$log <- log10(p_data$max + 0.01)
  
  return(lm_pvals_stations(p_data))
  
}


lm_TaxaMeanCount_stations <- function(p_data) {
  
  # Log-transform the mean values and add 0.01
  p_data$log <- log10(p_data$mean + 0.01)
  
  return(lm_pvals_stations(p_data))

}


lm_TaxaTotalCount_stations <- function(p_data) {
  
  # Log-transform the total values and add 0.01
  p_data$log <- log10(p_data$total + 0.01)
  
  return(lm_pvals_stations(p_data))
  
}

get_pvalue <- function(group) {
  # Remove any rows with NA values
  group <- na.omit(group)
  
  # Fit a linear model and extract the p-value
  model <- lm(log ~ year, data = group)
  summary(model)$coefficients[, 4]
}

lm_pvals_stations <- function(p_data) {
  # Group the data by station and taxa
  p_data_groups <- split(p_data, list(p_data$station_taxa))
  
  # Apply the function to each group to obtain the p-values
  p_values <- unlist(lapply(p_data_groups, get_pvalue))
  
  # Store the p-values in a data frame along with the corresponding station_taxa names
  p_values_df <-
    data.frame(station_taxa = unique(p_data$station_taxa),
               p_value = p_values)
  # Remove row names
  rownames(p_values_df) <- NULL
  
  # Remove rows with p-values > 0.05 or NaN values
  p_values_df <-
    p_values_df[!is.na(p_values_df$p_value) &
                  p_values_df$p_value <= 0.05, ]
  
  # Merge the p_values_df and p_data data frames on 'station_taxa'
  p_data_filtered <- merge(p_data, p_values_df, by = "station_taxa")
  
  # Remove row names
  rownames(p_data_filtered) <- NULL
  
  return(p_data_filtered)
}

regions <- function(p_data){
  
  regionPollen <- read.csv("raw/station_regions.csv")
  
  # Get unique stations in regionPollen
  unique_stations <- unique(regionPollen$station)
  
  # Subset p_data to keep only rows with stations in unique_stations
  p_data <- subset(p_data, station %in% unique_stations)
  
  # Merge p_data with regionPollen by "station" column
  p_data <- merge(p_data, regionPollen, by = "station", all.x = TRUE)
  
  p_data$region_taxa <- paste(p_data$region, p_data$taxa, sep = "_")
  
  region_taxa_summary <- p_data %>%
    group_by(taxa,region,region_taxa,year,mean_tMean ) %>%
    summarise(
      total_count = sum(total),
      mean_count = mean(mean),
      max_count = max(max),
      mean_t = mean(mean_tMean)
    )
  
  
  return(region_taxa_summary)
  
}

lm_TaxaTotalCount_regions <- function(p_data) {
  
  # Log-transform the total values and add 0.01
  p_data <- p_data[p_data$total_count > 0, ]
  p_data$log <- log10(p_data$total_count + 0.01)
  return(lm_pvals_regions(p_data))
}

lm_TaxaMaxCount_regions <- function(p_data) {
  
  # Log-transform the total values and add 0.01
  p_data <- p_data[p_data$max_count > 0, ]
  p_data$log <- log10(p_data$max_count + 0.01)
  
  
  return(lm_pvals_regions(p_data))
}

lm_TaxaMeanCount_regions <- function(p_data) {
  
  # Log-transform the total values and add 0.01
  p_data <- p_data[p_data$mean_count > 0, ]
  p_data$log <- log10(p_data$mean_count + 0.01)
  
  return(lm_pvals_regions(p_data))
}

lm_pvals_regions <- function(df) {
  
  # Group the data by region and taxa
  p_data_groups <- split(df, list(df$region_taxa))
  get_pvalue <- function(group) {
    # Remove any rows with NA values
    group <- na.omit(group)
    
    # Fit a linear model and extract the p-value
    model <- lm(log ~ mean_tMean, data = group)
    summary(model)$coefficients[, 4]
  }
  # Apply the function to each group to obtain the p-values
  p_values <- lapply(p_data_groups, get_pvalue)
  
  # Convert p_values list to data frame with single column
  p_values_df <- data.frame(p_value = unlist(p_values))
  
  # Store the region_taxa names in a separate variable
  p_values_df$region_taxa <- unique(df$region_taxa)
  
  # Remove rows with p-values > 0.05 or NaN values
  p_values_df <- p_values_df[!is.na(p_values_df$p_value) & p_values_df$p_value <= 0.05, ]
  
  # Merge the p_values_df and p_data data frames on 'region_taxa'
  p_data_filtered <- merge(df, p_values_df, by = "region_taxa")
  
  # Remove row names
  rownames(p_data_filtered) <- NULL
  
  return(p_data_filtered)
}




