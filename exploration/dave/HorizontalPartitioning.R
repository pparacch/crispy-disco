#===================================================================================================
#
# File:        HorizontalPartitioning.R
# Description: Split the raw training data horizontally to facilitate simple recommender training
#              and analysis scenarios:
#
#              1. Partition by May 2016.
#              2. Partition by April 2016.
#
#==================================================================================================
library(dplyr)
library(lubridate)



#
# Load data
#
train <- read.csv("train_ver2.csv", stringsAsFactors = FALSE)

# Use lubridate to work with date/time data
train$fecha_dato <- date(train$fecha_dato)



# Horizontally partition by May 2016 and save
df_may_2016 <- train %>%
  filter(fecha_dato == "2016-05-28")
save(df_may_2016, file = "df_May2016.RData")



# Horizontaly partition by April 2016
df_april_2016 <- train %>%
  filter(fecha_dato == "2016-04-28")
save(df_april_2016, file = "df_April2016.RData")

