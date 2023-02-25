library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library("car")
library(nlme)
library(lme4)
library(tidyverse)
library(lubridate)
library(MuMIn)

rm(list = ls())  # clear variables from environment
graphics.off()  # close any existing plots


# Import and Clean Data ---------------------------------------------------

## Pollen Climate Data (pc_data)
pc_data_dirty <-
  read.csv(
      "Data/PollenAndClimateDataMerged.csv"
    )

colnames(pc_data_dirty)[6] <- "avgPollen_M"
colnames(pc_data_dirty)[8] <- "ppt" #precipitation mm
colnames(pc_data_dirty)[9] <- "tMin"
colnames(pc_data_dirty)[10] <- "tMean"
colnames(pc_data_dirty)[11] <- "tMax"

pc_data_dirty <- pc_data_dirty %>% drop_na(avgPollen_M)
pc_data_dirty <- pc_data_dirty %>% drop_na(tMin)
pc_data_dirty <-
  pc_data_dirty[!grepl("Latitude", pc_data_dirty$Species), ]
pc_data <- pc_data_dirty[!grepl("Total", pc_data_dirty$Species), ]

save(pc_data, file = "Data/pc_data.RData")

load(file = "Data/pc_data.RData")
# Spring -----------------------------------------------------------------

# Creating Season by Month Catagories
months_spring <-
  c("February", "March", "April", "May") # February 1–May 31 for spring integrals
## Spring
spring_data <- pc_data %>%
  filter(Month %in% months_spring)

spring_data <- spring_data %>%
  group_by(year, Species) %>%
  summarize(avg_pollen = mean(avgPollen_M, na.rm = TRUE)) %>%
  select(year, Species, avg_pollen)

DF <- spring_data


  pollenDFs_lmlg <- lm(log10(avg_pollen+0.01) ~ year + Species, data = DF)
  pollenDFs_lmsq <- lm(sqrt(avg_pollen) ~ year + Species, data = DF)
  pollenDFs_lmreg <- lm(avg_pollen ~ year + Species, data = DF)
  AIC(pollenDFs_lmlg, pollenDFs_lmsq,pollenDFs_lmreg)
  
  pollenDFs_summary <- summary(pollenDFs_lmlg)$coefficients %>%
    as.data.frame() %>%
    rename(slope = Estimate, "Std. Error" = `Std. Error`, "t-value" = `t value`, "p-value" = `Pr(>|t|)`) %>%
    rownames_to_column(var = "Taxa")
  
  pollenDFs_summary$Taxa <- gsub("Species", "", pollenDFs_summary$Taxa)
  pollenDFs_summary$Taxa <- gsub("\\.", " ", pollenDFs_summary$Taxa)
  
  pollenDFs_pval0.05 <- pollenDFs_summary[pollenDFs_summary$"p-value" < 0.05, ]
  spring_data_lmsq <- pollenDFs_pval0.05[!(pollenDFs_pval0.05$Taxa %in% c("(Intercept)", "year")), ]
  
write_csv(spring_data_lmsq, file = "Outputdata/springData_pval0.05.csv")

ggplot(spring_data, aes(x = year, y = log10(avg_pollen+0.01))) +
  geom_smooth(method = "lm") + 
  facet_wrap(~Species)+
  labs(x = "Year", y = "Avg.Pollen/Month") +
  theme_bw() +
  ggtitle("Average Spring Monthly Pollen by Taxa and Year")

# Summer Taxa p>0.05 plot

spring_data_Pval <- spring_data %>%
  filter(Species %in% spring_data_lmsq$Taxa)

ggplot(spring_data_Pval, aes(x = year, y = sqrt(avg_pollen), color = Species)) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm",se=FALSE) + 
  labs(x = "Year", y = bquote(log[10]~(Avg.Pollen~Count))) +
  ggtitle("Average Spring Pollen Count p<0.05")

ggsave("Spring Pollen Count p<0.05 Layered.jpeg",
       device = "jpeg", width=  13, height = 7.5, units="in")

ggplot(spring_data_Pval, aes(x = year, y = sqrt(avg_pollen), color = Species)) +
  geom_point(alpha = 0.8)+
  geom_smooth(method = "lm") + 
  facet_wrap(~Species)+
  labs(x = "Year", y = bquote(log[10]~(Avg.Pollen~Count))) +
  theme(legend.position = "none")+
  ggtitle("Average Spring Pollen Count p<0.05")
ggsave("Spring Pollen Count p<0.05 Facet.jpeg",
       device = "jpeg", width=  13, height = 7.5, units="in")

# Summer ------------------------------------------------------------------


months_summer <-
  c("June", "July", "August") # June 1–August 31 for summer integrals

## Summer
summer_data <- pc_data %>%
  filter(Month %in% months_summer)

summer_data <- summer_data %>%
  group_by(year, Species) %>%
  summarize(avg_pollen = mean(avgPollen_M, na.rm = TRUE)) %>%
  select(year, Species, avg_pollen)

DF <- summer_data

pollenDFs_lmlg <- lm(log10(avg_pollen+0.01) ~ year + Species, data = DF)
pollenDFs_lmsq <- lm(sqrt(avg_pollen) ~ year + Species, data = DF)
pollenDFs_lmreg <- lm(avg_pollen ~ year + Species, data = DF)
AIC(pollenDFs_lmlg, pollenDFs_lmsq,pollenDFs_lmreg)

pollenDFs_summary <- summary(pollenDFs_lmsq)$coefficients %>%
  as.data.frame() %>%
  rename(slope = Estimate, "Std. Error" = `Std. Error`, "t-value" = `t value`, "p-value" = `Pr(>|t|)`) %>%
  rownames_to_column(var = "Taxa")

pollenDFs_summary$Taxa <- gsub("Species", "", pollenDFs_summary$Taxa)
pollenDFs_summary$Taxa <- gsub("\\.", " ", pollenDFs_summary$Taxa)

pollenDFs_pval0.05 <- pollenDFs_summary[pollenDFs_summary$"p-value" < 0.05, ]
summer_data_lmsq <- pollenDFs_pval0.05[!(pollenDFs_pval0.05$Taxa %in% c("(Intercept)", "year")), ]

write_csv(summer_data_lmsq, file = "Outputdata/summerAvgMon_Sqrt.csv")

ggplot(summer_data, aes(x = year, y = sqrt(avg_pollen))) +
  geom_smooth(method = "lm") + 
  facet_wrap(~Species)+
  labs(x = "Year", y = "Avg.Pollen/Month") +
  theme_bw() +
  ggtitle("Average Summer Monthly Pollen by Taxa and Year (squareroot)")

# Summer Taxa p>0.05 plot
summer_data_lmsq <- lm(sqrt(avg_pollen) ~ year + Species, data = summer_data)
summer_data_Pval <- summer_data %>%
  filter(Species %in% summer_data_lmsq$Taxa)

ggplot(summer_data_Pval, aes(x = year, y = sqrt(avg_pollen), color = Species)) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm",se=FALSE) + 
  labs(x = "Year", y = "Square root (Avg.Pollen Count)") +
  theme(legend.position = "none")+
  ggtitle("Average Summer Pollen Count p<0.05")
ggsave("Summer Pollen Count p<0.05 Layered.jpeg",
       device = "jpeg", width=  13, height = 7.5, units="in")

ggplot(summer_data_Pval, aes(x = year, y = sqrt(avg_pollen), color = Species)) +
  geom_point(alpha = 0.8)+
  geom_smooth(method = "lm") + 
  facet_wrap(~Species)+
  labs(x = "Year", y = "Square root (Avg.Pollen Count)") +
  theme(legend.position = "none")+
  ggtitle("Average Summer Pollen Count p<0.05")
ggsave("Summer Pollen Count p<0.05 Facet.jpeg",
       device = "jpeg", width=  13, height = 7.5, units="in")



# Fall --------------------------------------------------------------------


months_fall <-
  c("September", "October", "November")# September 1–November 30 for fall integrals

# Fall
fall_data <- pc_data %>%
  filter(Month %in% months_fall)


fall_data <- fall_data %>%
  group_by(year, Species) %>%
  summarize(avg_pollen = mean(avgPollen_M, na.rm = TRUE)) %>%
  select(year, Species, avg_pollen)

DF <- fall_data

pollenDFs_lmlg <- lm(log10(avg_pollen+0.01) ~ year + Species, data = DF)
pollenDFs_lmsq <- lm(sqrt(avg_pollen) ~ year + Species, data = DF)
pollenDFs_lmreg <- lm(avg_pollen ~ year + Species, data = DF)
AIC(pollenDFs_lmlg, pollenDFs_lmsq,pollenDFs_lmreg)

pollenDFs_summary <- summary(pollenDFs_lmsq)$coefficients %>%
  as.data.frame() %>%
  rename(slope = Estimate, "Std. Error" = `Std. Error`, "t-value" = `t value`, "p-value" = `Pr(>|t|)`) %>%
  rownames_to_column(var = "Taxa")

pollenDFs_summary$Taxa <- gsub("Species", "", pollenDFs_summary$Taxa)
pollenDFs_summary$Taxa <- gsub("\\.", " ", pollenDFs_summary$Taxa)

pollenDFs_pval0.05 <- pollenDFs_summary[pollenDFs_summary$"p-value" < 0.05, ]
fall_data_lmsq <- pollenDFs_pval0.05[!(pollenDFs_pval0.05$Taxa %in% c("(Intercept)", "year")), ]

write_csv(pollenDFs_pval0.05, file = "Outputdata/fallAvgMon_Sqrt.csv")

ggplot(fall_data, aes(x = year, y = sqrt(avg_pollen))) +
  geom_smooth(method = "lm") + 
  facet_wrap(~Species)+
  labs(x = "Year", y = "Avg.Pollen/Month") +
  theme_bw() +
  ggtitle("Average Fall Monthly Pollen by Taxa and Year (squareroot)")


# Fall Taxa p>0.05 plot

fall_data_Pval <- fall_data %>%
  filter(Species %in% fall_data_lmsq$Taxa)

ggplot(fall_data_Pval, aes(x = year, y = sqrt(avg_pollen), color = Species)) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm",se=FALSE) + 
  labs(x = "Year", y = "Square root (Avg.Pollen Count)") +
  theme(legend.position = "none")+
  ggtitle("Average Fall Pollen Count p<0.05")
ggsave("Fall Pollen Count p<0.05 Layer.jpeg",
       device = "jpeg", width=  13, height = 7.5, units="in")

ggplot(fall_data_Pval, aes(x = year, y = sqrt(avg_pollen), color = Species)) +
  geom_point(alpha = 0.8)+
  geom_smooth(method = "lm") + 
  facet_wrap(~Species)+
  labs(x = "Year", y = "Square root (Avg.Pollen Count)") +
  theme(legend.position = "none")+
  ggtitle("Average Fall Pollen Count p<0.05")
ggsave("Fall Pollen Count p<0.05 Facet.jpeg",
       device = "jpeg", width=  13, height = 7.5, units="in")

df <- pc_data %>%
  filter_at(vars(logAvgPol), all_vars(!is.infinite(.)))

# Taxa --------------------------------------------------------------------


# Defines several vectors containing different taxa categories for use in subsequent analysis

taxa_all <- unique(pc_data$Species) #gets all taxa collected

taxa_tree <- c(
  "Pinus",
  "Quercus",
  "Picea",
  "Betula",
  "Alnus",
  "Tsuga",
  "Cupress",
  "Fagus",
  "Ulmus",
  "Abies",
  "Fraxinus",
  "Salix"
)
taxa_herb <- c("Poaceae", "Cyperaceae", "Artemisia", "ChenAm")
taxa_all <- c(taxa_tree, taxa_herb)
taxa_dis <- c(
  "ChenAm",
  "Cupress",
  "Tsuga",
  "Poaceae",
  "Cyperaceae",
  "Salix",
  "Artemisia",
  "Picea",
  "Alnus",
  "Betula",
  "Pinus",
  "Abies",
  "Ulmus",
  "Quercus",
  "Fagus",
  "Fraxinus"
)

# Linear Regression of Species --------------------------------------------

#All Models Possible
options(na.action = "na.fail") #  change the default "na.omit" to prevent models
#  from being fitted to different datasets in
#  case of missing values.

fm1 <-
  lm(log10(avgPollen_M + 0.01) ~ log10(tMean+0.01) + log10(tMax+ 0.01) + log10(tMin+ 0.01) + log10(ppt+ 0.01), data = pc_data)


fm2 <-
  lm(sqrt(avgPollen_M) ~ tMean + tMax + tMin + ppt, data = pc_data)
fm3 <-
  lm(polMaxMon ~ tMean + tMax + tMin + ppt, data = merged_df)
fm4 <-
  lm(log10(polMaxMon +0.01) ~ ., data = merged_df)

ms1 <- dredge(fm1)
ms2 <- dredge(fm2)
ms3 <- dredge(fm3)

ms1_summary <- summary(ms1)$coefficients %>%
  as.data.frame() %>%
  rename(slope = Estimate, "Std. Error" = `Std. Error`, "t-value" = `t value`, "p-value" = `Pr(>|t|)`) %>%
  rownames_to_column(var = "Taxa")


par(mar = c(3, 5, 6, 4))
plot(ms1, labAsExpr = TRUE)
model.avg(ms1, subset = delta < 4)
confset.95p <- get.models(ms1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)


#Climate Models
lm_all <-lmList(avgPollen_M ~ tMean + tMax + tMin + ppt | Species, data = pc_data)
model_Tmean <- lmList(avgPollen_M ~ tMean | Species, data = pc_data)
model_Tmax <- lmList(avgPollen_M ~ tMax | Species, data = pc_data)
model_Tmin <- lmList(avgPollen_M ~ tMin | Species, data = pc_data)
model_PPT <- lmList(avgPollen_M ~ ppt | Species, data = pc_data)
model_decDate <- lmList(avgPollen_M ~ decimal_date(dmy(pc_data$Date))| Species, data = pc_data)

ggplot(data = pc_data, aes(x = decimal_date(dmy(pc_data$Date)), y = log(avgPollen_M + 0.01))) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ Species) +
  labs(title = "Average Pollen Concentration by Species Over Time")


write.csv(
  data.frame(summary(model_Tmean)$coefficients),
  "/Users/kober/Documents/School/Utah/Honors Thesis/model_Tmean.csv",
  row.names = TRUE
)
write.csv(
  data.frame(summary(model_Tmax)$coefficients),
  "/Users/kober/Documents/School/Utah/Honors Thesis/model_Tmax.csv",
  row.names = TRUE
)
write.csv(
  data.frame(summary(model_Tmin)$coefficients),
  "/Users/kober/Documents/School/Utah/Honors Thesis/model_Tmin.csv",
  row.names = TRUE
)
write.csv(
  data.frame(summary(model_PPT)$coefficients),
  "/Users/kober/Documents/School/Utah/Honors Thesis/model_PPT.csv",
  row.names = TRUE
)

pc_data$year <- year(mdy(pc_data$Date)) #add years
model_ <-
  lmList(log10(avgPollen_M + 0.01) ~ year | Species, data = pc_data)
write.csv(
  data.frame(summary(model_)$coefficients),
  "/Users/kober/Documents/School/Utah/Honors Thesis/logPollenByYear.csv",
  row.names = TRUE
)
model_ <-
  lmList(log10(avgPollen_M + 0.01) ~ tMean | Species, data = pc_data)
write.csv(
  data.frame(summary(model_)$coefficients),
  "/Users/kober/Documents/School/Utah/Honors Thesis/logPollenByAvgTemp.csv",
  row.names = TRUE
)




### Mixed Effect Models ###

pc_data.lmer1 <- lmer(avgPollen_M ~ tMean + (1 | Species), pc_data)

summary(pc_data.lmer1)
plot(pc_data.lmer1)

# Total Pollen Count
# Acer
# Alnus
# Ambrosia
# Arecaceae
# Artemisia
# Asteraceae (Excluding Ambrosia and Artemisia)
# Betula
# Carpinus/Ostrya
# Carya
# Celtis
# Chenopodiaceae/Amaranthaceae
# Corylus
# Cupressaceae
# Cyperaceae
# Eupatorium
# Fagus
# Fraxinus
# Gramineae / Poaceae
# Juglans
# Latitude
# Ligustrum
# Liquidambar
# Morus
# Myrica
# Olea
# Other Grass Pollen
# Other Tree Pollen
# Other Weed Pollen
# Pinaceae
# Plantago
# Platanus
# Populus
# Prosopis
# Pseudotsuga
# Quercus
# Rumex
# Salix
# Tilia
# Tsuga
# Typha
# Ulmus
# Unidentified Pollen
# Urticaceae


#PLOTS

#AVG total pc_data from 2000-2018
yres <-
  aggregate(pc_data$avgPollen_M, list(pc_data$year), FUN = mean)
ggplot() +
  geom_line(data = yres, aes(x = Group.1, y = x)) +
  labs(title = "Average Pollen from 2000 - 2018", x = "Year", y = "Average Pollen Count")

# scatter plot with a linear regression line using the log-transformed average pc_data count
# (avgPollen_M) on the y-axis and year on the x-axis, with separate panels for each species
# in the data frame "pc_data." The addition of 0.01 to avgPollen_M before taking the logarithm
# is to avoid taking the logarithm of zero or negative values.
ggplot(pc_data, aes(x = year, y = log10(avgPollen_M + 0.01), color=Species)) +
  geom_smooth(method = "lm") +
  theme(legend.position = "none")



# Importing and Cleaning TotalPollenData_Daily.csv ----------------------------------------------

# Read in your data and store it in a data frame called 'dailyPollen_df'
dailyPollen_df <- read.csv("Data/TotalPollenData_Daily.csv")
dailyPollen_df <- dailyPollen_df[,-c(2,45,46)]

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




max <-dailyPollen_df %>%
  group_by(species, year, month) %>%
  summarise(max_raw_daily_concentration = max(raw_daily_concentration))

max_conc <- pc_data %>%
  mutate(year = lubridate::year(mdy(Date))) 

names(max_conc)[names(max_conc) == "Species"] <- "species"

pc_data_with_max <- max  %>%
  left_join(max_conc, by = c("species", "year", "month")) %>%
  select(-year, -month)

merged_df <- merge(max, max_conc, by = c("month", "year", "species"))

names(fdf)[names(fdf) == "Species"] <- "species"
names(merged_df)[names(merged_df) == "max_raw_daily_concentration"] <- "polMaxMon"


fdf <- aggregate(tMean ~ year + Species, data = max_conc, FUN = mean)
merged_df2 <- merge(fdf, pollenSeasons, by = c( "year", "species"))

lm_avg <- lm(duration~tMean+species, merged_df2)
lm_avg1 <- lm(sqrt(duration)~tMean+species, merged_df2)
lm_avg2 <- lm(log10(duration)~tMean+species, merged_df2) #best
AIC(lm_avg2,lm_avg1,lm_avg)

lm_avg2_summary <- summary(lm_avg2)$coefficients %>%
  as.data.frame() %>%
  rename(slope = Estimate, "Std. Error" = `Std. Error`, "t-value" = `t value`, "p-value" = `Pr(>|t|)`) %>%
  rownames_to_column(var = "Taxa")

lm_avg2_summary$Taxa <- gsub("species", "", lm_avg2_summary$Taxa)
lm_avg2_summary$Taxa <- gsub("\\.", " ", lm_avg2_summary$Taxa)


pollenStartDates_pval0.05 <- lm_avg2_summary[lm_avg2_summary$"p-value" < 0.05, ]
pollenStartDates_pval0.05 <- pollenStartDates_pval0.05[!(pollenStartDates_pval0.05$Taxa %in% c("(Intercept)", "year")), ]

merged_df2 <- fdf %>%
  filter(species %in% pollenStartDates_pval0.05$Taxa)

ggplot(merged_df2, aes(x = tMean, y = log10(duration), color = species)) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm",se=FALSE) + 
  labs(x = "Year", y = bquote(log[10]~(duration))) +
  ggtitle("Pollen duration")

ggsave("Pollen duration tMean Layered.jpeg",
       device = "jpeg", width=  13, height = 7.5, units="in")

ggplot(merged_df2, aes(x = tMean, y = log10(duration), color = species)) +
  geom_point(alpha = 0.8)+
  geom_smooth(method = "lm") + 
  facet_wrap(~species)+
  labs(x = "Annual Temperature (°C)", y = bquote(log[10]~(duration))) +
  theme(legend.position = "none")+
  ggtitle("Pollen duration (p<0.05)")
ggsave("Pollen Season duration with tMean Facet.jpeg",
       device = "jpeg", width=  13, height = 7.5, units="in")

