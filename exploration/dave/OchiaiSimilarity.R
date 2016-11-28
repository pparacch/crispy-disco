#===================================================================================================
#
# File:        OchiaiSimilarity.R
# Description: Simple data exploration illustrating using Ochiai similarity to judge if features
#              help bucket customers into more similar neighborhoods.
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

# Filter down training data to most recent (i.e., May 2016)
train.may <- train %>%
  filter(fecha_dato == "2016-05-28")



#
# Calculate Ochiai Coefficient matrix for May 2016 products for all customers
#

# Utility function to calculate Ochiai Coefficient
avg.ochiai <- function(feature.matrix) {
  denominator <- ((nrow(feature.matrix) ^ 2) - nrow(feature.matrix)) / 2
  total <- 0.0

  for(i in 1:(nrow(feature.matrix) - 1)) {
     for(j in (i + 1):nrow(feature.matrix)) {
      a_int_b <- sum(intersect(feature.matrix[i,],
                               feature.matrix[j,]))
      
      if (a_int_b == 0) {
        ochiai <- 0
      } else {
        ochiai <- a_int_b / (sum(feature.matrix[i,]) + sum(feature.matrix[j,]))
      }
      
      total <- total + ochiai
    }
  }
  
  return(total / denominator)
}


# Can't calculate Ochiai for everything, use random sample of all May Customers
product.cols <- 25:48

set.seed(12345)
sample.may <- sample_n(train.may[, product.cols], size = 1000)

ochiai.may <- avg.ochiai(sample.may)
ochiai.may



#
# Calculate Ochiai Coefficient matrix for May 2016 products for "01 - TOP" customers
#

# OK, use segmento feature and see what's going on
train.top <- train.may %>%
  filter(segmento == "01 - TOP")

set.seed(54321)
sample.top <- sample_n(train.top, size = 33)

ochiai.top <- avg.ochiai(sample.top[, product.cols])
ochiai.top

#
# Results:
#
#     The Ochiai coefficient for sample of all customers:        0.2881937
#     The Ochiai coefficient for sample of "01 - TOP" customers: 0.4922935
#
#


