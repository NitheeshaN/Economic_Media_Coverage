###################################################################
#7) Econvote_MediaSubs/SentimentData_PW/code_nn/stimson_NYT_USA.R 
###################################################################
# Date Run:	February, 10 2023
# Date Run:	February, 13 2023
# Date Run:	February, 21 2023
# Date Run:	March, 27 2023
# Date Run:	March, 29 2023



#Purpose: Produced single NYT and single USAtoday time series using stimson algorithm by doing the following:
  # Calculated monthly weighted averages of positive probabilities in both Proquest (PA) and Lexis Nexis (LN) series for NYT and USAToday. 
    # I used the following calculation: sum(word_count*positive_probas)/sum(word_count))
  # Calculated monthly article frequency in both PA and LN in both NYT and USAToday. 
  # I then use the distinct() to create a clean dataset of monthly weighted probabilities and article frequency for all four series
  # I then use the stimson algorithm to combine and splice the PA and LN series for NYT and USAToday. I create two splices each (with smoothing and without)
      #This creates a smooth and nonsmooth series for NYT & smooth and nonsmooth series for USAToday. 4 series total and save them as .rds files. 
  # For NYT and USAToday I create a single dataframe of both Stimson series (with and without smoothing) w/ date variable called stim.NYT.csv and stim.USA.csv.
  # Finally I merge stim.NYT.csv and stim.USA.csv with the MonthlyEconoicData_Feb2023 dataset and create MonthlyEconomicData_Feb132023.csv.

#Data in:	MonthlyEconomicData_Feb2023.dta, MonthlyEconomicData_Feb2023v2.dta
#         full_predictions_nyt_proquest_PA_20230118.csv
#         full_predictions_nyt_lexisnexis_20230110.csv
#         full_predictions_usatoday_proquest_PA_20230123.csv 
#         full_predictions_usatoday_lexisnexis_20230117.csv

#Data Out:	NYT_pa.csv, NYT_pa_agg.csv, NYT_ln.csv, NYT_ln_agg.csv
#           NYT_stim_smth.rds, NYT_stim_nosmth.rds, stim.NYT.csv
#           USA_pa.csv, USA_pa_agg.csv, USA_ln.csv, USA_ln_agg.csv
#           USA_stim_smth.rds, USA_stim_nosmth.rds, stim.USA.csv
#           MonthlyEconomicData_Feb132023.csv
#           MonthlyEconomicData_Feb212023.csv
#           MonthlyEconomicData_Mar272023.csv
#           MonthlyEconomicData_Mar292023.csv

###################################################################

library(dplyr)
library(ggplot2)
library(data.table)
library(readr)
library(dplyr)
library(tidyr)
library(DyadRatios)
library(lubridate)
library(tseries)
library(astsa)
library(gridExtra)

###################################################################
# Stimson function to get single NYT time series
###################################################################
NYT_pa <- read_csv("full_predictions_nyt_pa.csv")
    #1947-01-01 to 2014-12-28
NYT_ln <- read_csv("full_predictions_nyt_ln.csv")
    #1980-06-01 to 2022-11-20

# add a PA and LN variable to each dataframe
NYT_pa$Variable <- rep("PA")
NYT_ln$Variable <- rep("LN")

# rename columns to match
NYT_pa <- NYT_pa %>% rename("content" = "text")
NYT_pa <- NYT_pa %>% rename("word_count" = "words")

# re-arrange data in descending order of date
NYT_pa <- NYT_pa %>% arrange(date)
NYT_ln <- NYT_ln %>% arrange(date)

#############################################
# Aggregate NYT PA Predictions
#############################################
# extract years and months
years <- format(as.Date(NYT_pa$date, format = "%y/%m/%d"), "%Y")
years <- as.vector(years)
months <- format(as.Date(NYT_pa$date, format = "%m/%d/%y"), "%m")
months <-as.vector(months)

# add to end of df
NYT_pa$year <- as.numeric(years)
NYT_pa$month <- as.numeric(months)

NYT_pa <- NYT_pa %>%
  group_by(month, year) %>%
  mutate(weighted.avg.pa = sum(word_count*positive_probas)/sum(word_count))

## Get article frequency per month
NYT_pa$count <- 1

NYT_pa <- NYT_pa %>%
  group_by(month, year) %>%
  mutate(article_freq = sum(count))

write.csv(NYT_pa, "NYT_pa.csv", row.names = FALSE)

## redo data frames to just distinct values
NYT_pa_agg <- NYT_pa %>%
  select(Variable, year, month, weighted.avg.pa, article_freq) %>%
  distinct()

# check series length
table(NYT_pa_agg$month)
  #sept and oct are missing 
  # find the year that's missing September and October and put in 0 
  missing <- subset(NYT_pa_agg, month == "9") #1978 does not have September
  missing <- subset(NYT_pa_agg, month == "10") #1978 does not have October

  #Impute 0's for those two months in 1978
  sept <- c(1978, 9, NA)
  sept<- as.data.frame(sept)
  sept<- t(sept)
  sept<- as.data.frame(sept)

  names(sept)[names(sept) == "V1"] <- "year"
  names(sept)[names(sept) == "V2"] <- "month"
  names(sept)[names(sept) == "V3"] <- "weighted.avg.pa"

  oct <- c(1978, 10, NA)
  oct<- as.data.frame(oct)
  oct<- t(oct)
  oct<- as.data.frame(oct)
  
  names(oct)[names(oct) == "V1"] <- "year"
  names(oct)[names(oct) == "V2"] <- "month"
  names(oct)[names(oct) == "V3"] <- "weighted.avg.pa"

  # Impute
  NYT_pa_agg <- bind_rows(NYT_pa_agg, sept)
  NYT_pa_agg <- bind_rows(NYT_pa_agg, oct)
  
  NYT_pa_agg$Variable <- rep("PA")
  
  # check series length
  table(NYT_pa_agg$month) 

  #clean
  NYT_pa_agg <- arrange(NYT_pa_agg, month)
  NYT_pa_agg <- arrange(NYT_pa_agg, year)

write.csv(NYT_pa_agg, "NYT_pa_agg.csv")

#############################################
# Aggregate NYT LN Predictions
#############################################
years <- format(as.Date(NYT_ln$date, format = "%y/%m/%d"), "%Y")
years <- as.vector(years)
months <- format(as.Date(NYT_ln$date, format = "%m/%d/%y"), "%m")
months <-as.vector(months)

# add to end of df
NYT_ln$year <- as.numeric(years)
NYT_ln$month <- as.numeric(months)

NYT_ln <- NYT_ln %>%
  group_by(month, year) %>%
  mutate(weighted.avg.ln = sum(word_count*positive_probas)/sum(word_count))

## Get article frequency per month
NYT_ln$count <- 1

NYT_ln <- NYT_ln %>%
  group_by(month, year) %>%
  mutate(article_freq = sum(count))

write.csv(NYT_ln, "NYT_ln.csv", row.names = FALSE)

## redo data frames to just distinct values
NYT_ln_agg <- NYT_ln %>%
  select(Variable, year, month, weighted.avg.ln, article_freq) %>%
  distinct()

## check series length
table(NYT_ln_agg$month)

write.csv(NYT_ln_agg, "NYT_ln_agg.csv")

#############################################
# Combine and Splice NYT Predictions
#############################################
NYT_pa_agg <- NYT_pa_agg %>% rename("weighted.avg" = "weighted.avg.pa")
NYT_ln_agg <- NYT_ln_agg %>% rename("weighted.avg" = "weighted.avg.ln")

# put one series on top of the other
NYT_stim <- bind_rows(NYT_pa_agg,NYT_ln_agg)

#recreate date variable to ensure that it is date class
NYT_stim$date <- as.Date(paste(NYT_stim$year, NYT_stim$month, 1, sep = "/"))

# run function WITH SMOOTHING
out1 <- extract(varname = NYT_stim$Variable, date = NYT_stim$date, index = NYT_stim$weighted.avg, 
                ncases = NYT_stim$article_freq, unit = "M", smoothing = TRUE)

saveRDS(out1, "NYT_stim_smth.rds")

plot(out1)
display(out1)

summary(out1)
#Variable Loadings and Descriptive Information: Dimension 1
#Variable Name Cases Loading  Mean    Std Dev 
#         LN    509   0.920 0.336990 0.0291493 
#         PA    814   0.888 0.372363 0.0605138 

# set smoothing = FALSE
out2 <- extract(varname = NYT_stim$Variable, date = NYT_stim$date, index = NYT_stim$weighted.avg, 
                ncases = NYT_stim$article_freq, unit = "M", smoothing = FALSE)

plot(out2)
display(out2)
summary(out2)
#Variable Loadings and Descriptive Information: Dimension 1
#Variable Name Cases Loading    Mean    Std Dev 
#         LN    509   0.928 0.336990 0.0291493 
#         PA    814   0.972 0.372363 0.0605138 

saveRDS(out2, "NYT_stim_nosmth.rds")

#############################################
# create a Dataframe of both Stimson series (with and without smoothing) w/ date variable
#############################################
##extract stimson latent values and put into single dataframe
# Latent Values WITH SMOOTH
latent.1 <- unlist(out1$latent1)
latent.1 <- as.data.frame(latent.1)

# Latent Values WITHOUT SMOOTH
latent.2 <- unlist(out2$latent1)
latent.2 <- as.data.frame(latent.2)

# combine latent values
stim.nyt.latent <- cbind(latent.1, latent.2)

# create date class variable
#note: the dates are the same for both Stimson out1 and out2, here I choose to work with out2 to get dates 
date <- unlist(out2$period)
date <- as.data.frame(date) #1x1 dataframe with numeric objects that look like this: 1947.01
class(date$date)

#create date variable
date$date <- as.character(sprintf("%.2f", date$date))
class(date$date)

#get year
date$year <- as.integer(date$date)
#get month
date$month <- sub('.*\\.', '', date$date)
#create date class variable
date$Date <- as.Date(paste(date$year, date$month, 1, sep = "/"))
str(date$Date)

stim.PA_LN <- cbind(date, stim.nyt.latent)
stim.PA_LN <- stim.PA_LN[, c(4:6)]
names(stim.PA_LN)[names(stim.PA_LN) == 'latent.1'] <- 'smooth'
names(stim.PA_LN)[names(stim.PA_LN) == 'latent.2'] <- 'nonsmooth'

write.csv(stim.PA_LN, "stim.NYT.csv")

###################################################################
# Stimson function to get single USAToday time series
###################################################################
USA_pa <- read_csv("full_predictions_usa_PA.csv")
  #1987-04-01 to 2014-12-31
USA_ln <- read_csv("full_predictions_usa_ln.csv")
  #1989-01-03 to 2022-11-28

# add a PA and LN variable to each dataframe
USA_pa$Variable <- rep("PA")
USA_ln$Variable <- rep("LN")

# rename columns to match
USA_pa <- USA_pa %>% rename("content" = "text")
USA_pa <- USA_pa %>% rename("word_count" = "words")

# re-arrange data in descending order of date
USA_pa <- USA_pa %>% arrange(date)
USA_ln <- USA_ln %>% arrange(date)


#############################################
# Aggregate USA PA Predictions
#############################################
# extract years and months
years <- format(as.Date(USA_pa$date, format = "%y/%m/%d"), "%Y")
years <- as.vector(years)
months <- format(as.Date(USA_pa$date, format = "%m/%d/%y"), "%m")
months <-as.vector(months)

# add to end of df
USA_pa$year <- as.numeric(years)
USA_pa$month <- as.numeric(months)

USA_pa <- USA_pa %>%
  group_by(month, year) %>%
  mutate(weighted.avg.pa = sum(word_count*positive_probas)/sum(word_count))

## Get article frequency per month
USA_pa$count <- 1

USA_pa <- USA_pa %>%
  group_by(month, year) %>%
  mutate(article_freq = sum(count))

write.csv(USA_pa, "USA_pa.csv", row.names = FALSE)

## redo data frames to just distinct values
USA_pa_agg <- USA_pa %>%
  select(Variable, year, month, weighted.avg.pa, article_freq) %>%
  distinct()

# check series length
table(USA_pa_agg$month)

write.csv(USA_pa_agg, "USA_pa_agg.csv")

#############################################
# Aggregate USA LN Predictions
#############################################
years <- format(as.Date(USA_ln$date, format = "%y/%m/%d"), "%Y")
years <- as.vector(years)
months <- format(as.Date(USA_ln$date, format = "%m/%d/%y"), "%m")
months <-as.vector(months)

# add to end of df
USA_ln$year <- as.numeric(years)
USA_ln$month <- as.numeric(months)

USA_ln <- USA_ln %>%
  group_by(month, year) %>%
  mutate(weighted.avg.ln = sum(word_count*positive_probas)/sum(word_count))

## Get article frequency per month
USA_ln$count <- 1

USA_ln <- USA_ln %>%
  group_by(month, year) %>%
  mutate(article_freq = sum(count))

write.csv(USA_ln, "USA_ln.csv", row.names = FALSE)

## redo data frames to just distinct values
USA_ln_agg <- USA_ln %>%
  select(Variable, year, month, weighted.avg.ln, article_freq) %>%
  distinct()

## check series length
table(USA_ln_agg$month)

write.csv(USA_ln_agg, "USA_ln_agg.csv")

#############################################
# Combine and Splice NYT Predictions
#############################################
USA_pa_agg <- USA_pa_agg %>% rename("weighted.avg" = "weighted.avg.pa")
USA_ln_agg <- USA_ln_agg %>% rename("weighted.avg" = "weighted.avg.ln")

# put one series on top of the other
USA_stim <- bind_rows(USA_pa_agg,USA_ln_agg)

#recreate date variable to ensure that it is date class
USA_stim$date <- as.Date(paste(USA_stim$year, USA_stim$month, 1, sep = "/"))

# run function WITH SMOOTHING
out1 <- extract(varname = USA_stim$Variable, date = USA_stim$date, index = USA_stim$weighted.avg, 
                ncases = USA_stim$article_freq, unit = "M", smoothing = TRUE)

saveRDS(out1, "USA_stim_smth.rds")

plot(out1)
display(out1)

summary(out1)
#Variable Loadings and Descriptive Information: Dimension 1
#Variable Name Cases Loading  Mean    Std Dev 
#         LN    406   0.925 0.344068 0.0399532 
#         PA    333   0.848 0.376255 0.0707575 

# set smoothing = FALSE
out2 <- extract(varname = USA_stim$Variable, date = USA_stim$date, index = USA_stim$weighted.avg, 
                ncases = USA_stim$article_freq, unit = "M", smoothing = FALSE)

plot(out2)
display(out2)
summary(out2)
#Variable Loadings and Descriptive Information: Dimension 1
#Variable Name Cases Loading    Mean    Std Dev 
#         LN    406   0.972 0.344068 0.0399532 
#         PA    333   0.933 0.376255 0.0707575  

saveRDS(out2, "USA_stim_nosmth.rds")

#############################################
# create a Dataframe of both Stimson series (with and without smoothing) w/ date variable
#############################################
##extract stimson latent values and put into single dataframe
# Latent Values WITH SMOOTH
latent.1 <- unlist(out1$latent1)
latent.1 <- as.data.frame(latent.1)

# Latent Values WITHOUT SMOOTH
latent.2 <- unlist(out2$latent1)
latent.2 <- as.data.frame(latent.2)

# combine latent values
stim.usa.latent <- cbind(latent.1, latent.2)

# create date class variable
#note: the dates are the same for both Stimson out1 and out2, here I choose to work with out2 to get dates 
date <- unlist(out2$period)
date <- as.data.frame(date) #1x1 dataframe with numeric objects that look like this: 1947.01
class(date$date)

#create date variable
date$date <- as.character(sprintf("%.2f", date$date))
class(date$date)

#get year
date$year <- as.integer(date$date)
#get month
date$month <- sub('.*\\.', '', date$date)
#create date class variable
date$Date <- as.Date(paste(date$year, date$month, 1, sep = "/"))
str(date$Date)

stim.PA_LN <- cbind(date, stim.usa.latent)
stim.PA_LN <- stim.PA_LN[, c(4:6)]
names(stim.PA_LN)[names(stim.PA_LN) == 'latent.1'] <- 'smooth'
names(stim.PA_LN)[names(stim.PA_LN) == 'latent.2'] <- 'nonsmooth'

write.csv(stim.PA_LN, "stim.USA.csv")

###################################################################
# Merge USA and NYT Stim series w/ Econmedia March 2023 data
###################################################################
econ <- read_dta("~MonthlyEconomicData_March2023.dta")
NYT <- read_csv("~/stim.NYT.csv")
USA <- read_csv("~/stim.USA.csv")

# create data variable in econ
econ$Date1 <- as.Date(paste(econ$year, econ$month, 1, sep = "/"))

#### USA
# specify column names
USA <- USA %>% rename("USA_smth" = "smooth")
USA <- USA %>% rename("USA_nosmth" = "nonsmooth")

# merge with econ data
econ2 <- left_join(econ, USA, by = c("Date1" = "Date"))

#### NYT
# specify column names
NYT <- NYT %>% rename("NYT_smth" = "smooth")
NYT <- NYT %>% rename("NYT_nosmth" = "nonsmooth")
NYT<- NYT[, 2:4]

# merge with econ data
econ2 <- left_join(econ2, NYT, by = c("Date1" = "Date"))

write.csv(econ2, "econ2.csv")



