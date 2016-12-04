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
sample.may <- sample_n(train.may[, product.cols], size = 5000)

ochiai.may <- avg.ochiai(sample.may)
ochiai.may



#
# Calculate Ochiai Coefficient matrix for May 2016 products for "01 - TOP" customers
#

# OK, use segmento feature and see what's going on 
train.top <- train.may %>%
  filter(segmento == "01 - TOP")

set.seed(54321)
sample.top <- sample_n(train.top, size = 190)

ochiai.top <- avg.ochiai(sample.top[, product.cols])
ochiai.top


#
# Calculate Ochiai Coefficient matrix for May 2016 products for "02 - PARTICULARES" customers
#

# OK, use segmento feature and see what's going on 
train.particulares <- train.may %>%
  filter(segmento == "02 - PARTICULARES")

set.seed(984357)
sample.particulares <- sample_n(train.particulares, size = 2913)

ochiai.particulares <- avg.ochiai(sample.particulares[, product.cols])
ochiai.particulares


#
# Calculate Ochiai Coefficient matrix for May 2016 products for "03 - UNIVERSITARIO" customers
#

# OK, use segmento feature and see what's going on 
train.universitario <- train.may %>%
  filter(segmento == "03 - UNIVERSITARIO")

set.seed(74353)
sample.universitario <- sample_n(train.universitario, size = 1860)

ochiai.universitario <- avg.ochiai(sample.universitario[, product.cols])
ochiai.universitario


#
# Calculate Ochiai Coefficient matrix for May 2016 products for "" customers
#

# OK, use segmento feature and see what's going on 
train.blank <- train.may %>%
  filter(segmento == "")

set.seed(38566)
sample.blank <- sample_n(train.blank, size = 38)

ochiai.blank <- avg.ochiai(sample.blank[, product.cols])
ochiai.blank


#
# Results for May products owned/used:
#
#     The Ochiai coefficient for sample of all customers:                  0.2780641
#     The Ochiai coefficient for sample of "01 - TOP" customers:           0.5077601
#     The Ochiai coefficient for sample of "02 - PARTICULARES" customers:  0.241388
#     The Ochiai coefficient for sample of "03 - UNIVERSITARIO" customers: 0.3207485
#     The Ochiai coefficient for sample of "" customers:                   0.04800853
#
#


