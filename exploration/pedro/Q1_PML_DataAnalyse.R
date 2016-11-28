## INSTALL AND LOAD PACKAGES 
install.packages("tidyverse")   #Using the tidyverse package for performing exploration and data analysis.
require(tidyverse)
require(data.table)
require(lubridate)

## GETTING THE DATA
setwd("C:/Users/Pedro/Desktop/Kaggle/Santander Product Recommendation/crispy-disco/data/processed")
load("C:/Users/Pedro/Desktop/Kaggle/Santander Product Recommendation/crispy-disco/data/processed/df_2015_Q1.RData")

## Separating the Trimestre by Month (for an easier visualization) 
M1_Q1a <- df_2015_Q1$fecha_dato_year == 2015 & (df_2015_Q1$fecha_dato_month == 1)
M2_Q1a <- df_2015_Q1$fecha_dato_year == 2015 & (df_2015_Q1$fecha_dato_month == 2)
M3_Q1a <- df_2015_Q1$fecha_dato_year == 2015 & (df_2015_Q1$fecha_dato_month == 3)

M1_Q1 <- df_2015_Q1[M1_Q1a,]
M2_Q1 <- df_2015_Q1[M2_Q1a,]
M3_Q1 <- df_2015_Q1[M3_Q1a,]

rm(list= c("M1_Q1a", "M2_Q1a", "M3_Q1a"))

## Checking for Missing Values
sapply(M1_Q1,function(x)any(is.na(x))) 
sapply(M2_Q1,function(x)any(is.na(x))) 
sapply(M3_Q1,function(x)any(is.na(x))) 

## Split columns that have missing values
## Splitting AGE
M1_Q1age <- data.frame(cbind(fecha_dato = M1_Q1$fecha_dato,
                           ncodpers = M1_Q1$ncodpers,
                           age = M1_Q1$age))

M2_Q1age <- data.frame(cbind(fecha_dato = M2_Q1$fecha_dato,
                           ncodpers = M2_Q1$ncodpers,
                           age = M2_Q1$age))

M3_Q1age <- data.frame(cbind(fecha_dato = M3_Q1$fecha_dato,
                           ncodpers = M3_Q1$ncodpers,
                           age = M3_Q1$age))

# Searching for rows with missing values by feature (so R wont omitte rows)
which(is.na(M1_Q1age), arr.ind=TRUE) 
which(is.na(M2_Q1age), arr.ind=TRUE)
which(is.na(M3_Q1age), arr.ind=TRUE)



