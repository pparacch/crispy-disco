#===================================================================================================
#
# File:        RentaMissingValues.R
# Description: Sample script showing missing value analysis over a vertical partition of the 
#              the training data - focusing on the 'renta' numeric variable.
#
#==================================================================================================



# Set your working directory to the proper location
load("df_renta.RData")

# Convert renta to numeric from factor
df_renta$renta <- as.numeric(as.character(df_renta$renta))

# Get the indexes for those records with missing renta values
renta.na.indexes <- which(is.na(df_renta$renta))

# Subset data
renta.missing <- df_renta[renta.na.indexes,]

# Use dplyr for some analysis
library(dplyr)

# Total up missing values by ncodpers
renta.missing.totals <- renta.missing %>%
  group_by(ncodpers) %>%
  summarize(total = n()) %>%
  arrange(ncodpers)

# Display some in RStudio
View(renta.missing.totals[1:1000,])

# Total up all values by ncodpers
renta.totals <- df_renta %>%
  group_by(ncodpers) %>%
  summarize(total = n()) %>%
  arrange(ncodpers)

# Display some in RStudio
View(renta.totals[1:1000,])

# Join the data up
renta.both.totals <- renta.missing.totals %>%
  inner_join(renta.totals, by = "ncodpers")

# Grab the rows where there are different totals (i.e., find customers that 
# have some renta values missing and some values present)
renta.different.totals <- renta.both.totals %>%
  filter(total.x != total.y)

# OK, grab all records for each ncodpers 
renta.different <- df_renta %>%
  filter(ncodpers %in% renta.different.totals$ncodpers) %>%
  arrange(ncodpers, desc(fecha_dato))

# Display in RStudio
View(renta.different)

