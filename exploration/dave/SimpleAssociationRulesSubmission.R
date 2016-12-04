#===================================================================================================
#
# File:        SimpleAssociationRulesSubmission.R
# Description: Evaluate the following using a simple Association Rules recommender:
#
#              1. Kaggle submission score using May 2016 binary product data to train the
#                 recommender and predict for the Kaggle test data.
#
#==================================================================================================
library(dplyr)
library(arules)
library(recommenderlab)


# Load up data from May 2016
load("df_May2016.RData")

# Load up test data
test <- read.csv("test_ver2.csv", header = TRUE, stringsAsFactors = FALSE)


#
# Train Association Rules Recommender
#

# Subset May 2016 to only the binary product data
binary.columns <- 25:48
train.may.trans <- df_may_2016 %>%
  select(binary.columns)

# Fix up data to be R binary data type
for(i in 1:ncol(train.may.trans)) {
  train.may.trans[, i] <- as.logical(train.may.trans[, i])
}

# Convert to arules transaction object
transactions.may <- as(train.may.trans, "transactions")
transactions.may

# Convert arules transactions object to recommender lab object
matrix.may <- as(transactions.may, "binaryRatingMatrix")
matrix.may

# Set up parameters for Association Rules
params <- list(supp = 0.01, conf = 0.7)

# Train Association Rule recommender
recommender <- Recommender(matrix.may, method = "AR", parameter = params)



#
# Prep data for predictions
#

# Filter training set down to customers present in test data and subset columns
customer.ids <- sort(base::intersect(test$ncodpers, df_may_2016$ncodpers))
columns <- c(2, 25:48)
preds.may <- df_may_2016 %>%
  filter(ncodpers %in% customer.ids) %>%
  select(columns) %>%
  arrange(ncodpers)

# Fix up data to be R binary data type
for(i in 2:ncol(preds.may)) {
  preds.may[, i] <- as.logical(preds.may[, i])
}

# Convert preds to arules transaction object
transactions.preds.may <- as(preds.may[, 2:ncol(preds.may)], "transactions")
transactions.preds.may

# Convert arules transactions object to recommenderlab object
matrix.preds.may <- as(transactions.preds.may, "binaryRatingMatrix")
matrix.preds.may 



#
# Make predictions and save in Kaggle-compliant format
#

# Make some predictions for June using data from May
preds <- predict(recommender, matrix.preds.may, 7)
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


# Utility function - transform vector of product strings into a single string
format.preds <- function(preds) {
  return(paste(unlist(preds), collapse = " "))
}

# Create a data frame formatted to Kaggle's specifictions
submission <- data.frame(ncodpers = preds.may$ncodpers,
                         added_products = sapply(lst_preds, format.preds))

# Save data frame off as .CSV sutiable for submission to Kaggle
write.csv(submission, "SantanderSubmission.csv", quote = FALSE, row.names = FALSE)
