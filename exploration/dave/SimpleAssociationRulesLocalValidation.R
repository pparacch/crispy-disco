#===================================================================================================
#
# File:        SimpleAssociationRules1.R
# Description: Evaluate the following using a simple Association Rules recommender:
#
#              1. Local validation score using April 2016 binary product data to train the 
#                 recommender and May 2016 binary product data for validation.
#              2. Kaggle submission score using May 2016 binary product data to train the
#                 recommender and predict for the Kaggle test data.
#
#==================================================================================================
library(dplyr)
library(lubridate)
library(Metrics)
library(tidyr)
library(arules)
library(recommenderlab)


#
# Load data
#
train <- read.csv("train_ver2.csv", stringsAsFactors = FALSE)

# Use lubridate to work with date/time data
train$fecha_dato <- date(train$fecha_dato)

# Filter down training data to most recent (i.e., May 2016) for validation set
train.may <- train %>%
  filter(fecha_dato == "2016-05-28")

# Filter down training data to April 2016
train.april <- train %>%
  filter(fecha_dato == "2016-04-28")

# Filter validation set down to customer present in April data and subset columns
customer.ids <- sort(base::intersect(train.may$ncodpers, train.april$ncodpers))
columns <- c(2, 25:48)
validation.may <- train.may %>%
  filter(ncodpers %in% customer.ids) %>%
  select(columns) %>%
  arrange(ncodpers)


# Leverage tidyr to move from wide to long data format
validation.may.long <- gather(validation.may, product, owned, -ncodpers)
train.april.long <- gather(train.april[, columns], product, owned, -ncodpers)


# Filter out products not owned
validation.may.long <- validation.may.long %>%
  filter(owned == 1) %>%
  arrange(ncodpers)

train.april.long <- train.april.long %>%
  filter(owned == 1) %>%
  arrange(ncodpers)


# Subset validation set to only products not owned in April
validation.may.long <- validation.may.long %>%
  anti_join(train.april.long, by = c("ncodpers", "product")) %>%
  arrange(ncodpers, product)

# User left outer join to create master validation set, including those May 
# customers that didn't add anything in the month of May
validation.may.long <- validation.may.customers %>%
  select(ncodpers) %>%
  left_join(validation.may.long, by = "ncodpers") %>%
  arrange(ncodpers, product)



# Transform validation into form that can be used by the apk() function
create.recommendations <- function(customer.id, products.long) {
  product.obs <- products.long %>%
    filter(ncodpers == customer.id)
  
  return(as.vector(product.obs$product))
  
}

# The apk() function, as well as the Kaggle submission requires a list
# of recommended product names for each customer, use lapply() to build
# the list
#
# NOTE - The following code takes a long time to run, load from disk!
#
lst_validation_may <- lapply(customer.ids, 
                             create.recommendations, 
                             validation.may.long)
save(validation.may.list, file = "lst_Validation_May2016.RData")

# Load validation list
load("lst_Validation_May2016.RData")



# Subset April training data and build Association Rules recommender using the
# arules and recommenderlab package

# Susbset
binary.columns <- 25:48
train.april.trans <- train.april %>%
  select(binary.columns)

# Fix up data to be R binary data type
for(i in 1:ncol(train.april.trans)) {
  train.april.trans[, i] <- as.logical(train.april.trans[, i])
}

# Convert to arules transaction object
transactions.april <- as(train.april.trans, "transactions")
transactions.april

# Convert arules transactions object to recommender lab object
matrix.april <- as(transactions.april, "binaryRatingMatrix")
matrix.april

# Set up parameters for Association Rules
params <- list(supp = 0.01, conf = 0.7)

# Train Association Rule recommender
recommender <- Recommender(matrix.april, method = "AR", parameter = params)


# Subset April for only the same customers in May
preds.april <- train.april %>%
  filter(ncodpers %in% customer.ids) %>%
  arrange(ncodpers) %>%
  select(binary.columns)

# Fix up data to be R binary data type
for(i in 1:ncol(preds.april)) {
  preds.april[, i] <- as.logical(preds.april[, i])
}

# Convert to arules transaction object
transactions.preds.april <- as(preds.april, "transactions")
transactions.preds.april

# Convert arules transactions object to recommender lab object
matrix.preds.april <- as(transactions.preds.april, "binaryRatingMatrix")
matrix.preds.april

# Make some predictions using April data for May
preds <- predict(recommender, matrix.preds.april, 7)
lst_preds <- as(preds, "list")

# Prediction appear to have extraneous NAs, remove them
preds.cleanup <- function(preds) {
  if (length(preds) > 1) {
    return(preds[!is.na(preds)])    
  } else {
    return(preds)
  }
}
lst_preds <- lapply(lst_preds, preds.cleanup)

# Evaluate with apk() function
results <- apk(7, lst_validation_may, lst_preds)

