## INSTALL AND LOAD PACKAGES 
install.packages("tidyverse")   #Using the tidyverse package for performing exploration and data analysis.
require(data.table, warn.conflicts = FALSE)
require(tidyverse, warn.conflicts = FALSE)
require(dplyr, warn.conflicts = FALSE)
require(knitr, warn.conflicts = FALSE)
require(data.table, warn.conflicts = FALSE)
require(lubridate, warn.conflicts = FALSE)

## GETTING THE DATA
setwd("C:/Users/Pedro/Desktop/Kaggle/Santander Product Recommendation/crispy-disco/data/processed")
load("C:/Users/Pedro/Desktop/Kaggle/Santander Product Recommendation/crispy-disco/data/processed/df_2015_Q1.RData")

# Converting to factors

setDataTypes <- function(x){
  x$ncodpers <- as.factor(x$ncodpers)
  x$ind_empleado <- as.factor(x$ind_empleado)
  x$pais_residencia <- as.factor(x$pais_residencia)
  x$sexo <- as.factor(x$sexo)
  
  x$fecha_alta <- lubridate::ymd(x$fecha_alta)
  
  x$ind_nuevo <- as.factor(x$ind_nuevo)
  x$indrel <- as.factor(x$indrel)
  
  x$ult_fec_cli_1t <- lubridate::ymd(x$ult_fec_cli_1t)
  
  x$indrel_1mes <- as.factor(x$indrel_1mes)
  x$tiprel_1mes <- as.factor(x$tiprel_1mes)
  x$indresi <- as.factor(x$indresi)
  x$indext <- as.factor(x$indext)
  x$conyuemp <- as.factor(x$conyuemp)
  x$canal_entrada <- as.factor(x$canal_entrada)
  x$indfall <- as.factor(x$indfall)
  x$tipodom <- as.factor(x$tipodom)
  x$cod_prov <- as.factor(x$cod_prov)
  x$nomprov <- as.factor(x$nomprov)
  x$ind_actividad_cliente <- as.factor(x$ind_actividad_cliente)
  
  x$segmento <- as.factor(x$segmento)
  x
}

df_2015_Q1 <- setDataTypes(df_2015_Q1)
str(df_2015_Q1)

## Separating the Trimestre by Month (for an easier visualization) 
M1_Q1a <- df_2015_Q1$fecha_dato_year == 2015 & (df_2015_Q1$fecha_dato_month == 1)
M2_Q1a <- df_2015_Q1$fecha_dato_year == 2015 & (df_2015_Q1$fecha_dato_month == 2)
M3_Q1a <- df_2015_Q1$fecha_dato_year == 2015 & (df_2015_Q1$fecha_dato_month == 3)

M1_Q1 <- df_2015_Q1[M1_Q1a,]
M2_Q1 <- df_2015_Q1[M2_Q1a,]
M3_Q1 <- df_2015_Q1[M3_Q1a,]

write.csv(M1_Q1, file="M1_Q1df.csv", row.names=FALSE) # Example to write a csv and analyse in Trifacta

rm(list= c("M1_Q1a", "M2_Q1a", "M3_Q1a"))

## Checking for Missing Values
sapply(M1_Q1,function(x)any(is.na(x))) 
sapply(M2_Q1,function(x)any(is.na(x))) 
sapply(M3_Q1,function(x)any(is.na(x)))

## Split columns that have missing values
## Splitting AGE
M1_Q1age <- data.frame(cbind(fecha_dato = M1_Q1$fecha_dato, ncodpers = M1_Q1$ncodpers, age = M1_Q1$age))
M2_Q1age <- data.frame(cbind(fecha_dato = M2_Q1$fecha_dato, ncodpers = M2_Q1$ncodpers, age = M2_Q1$age))
M3_Q1age <- data.frame(cbind(fecha_dato = M3_Q1$fecha_dato, ncodpers = M3_Q1$ncodpers, age = M3_Q1$age))
Q1_2005 <- data.frame(cbind(fecha_dato = df_2015_Q1$fecha_dato, ncodpers = df_2015_Q1$ncodpers, age = df_2015_Q1$age))

# Searching for rows with missing values by feature (Null not included and R wont omitte rows)
which(is.na(M1_Q1age), arr.ind=TRUE) 
which(is.na(M2_Q1age), arr.ind=TRUE)
which(is.na(M3_Q1age), arr.ind=TRUE)

# Checking for duplicates in Months 1, 2 and 3 of Quarter 1
Q1_2005[duplicated(Q1_2005),]
Q1_2005[!duplicated(Q1_2005),]     # Checking the contradiction to see if its true

## Missing Values/ Empty Values Analysis

noOfMissingValues <- function(x){
  #all features
  sum(is.na(x))
}

noOfEmptyValues <- function(x){
  #For character vector and factors
  if(is.character(x) | is.factor(x)){
    sum(x == "")   
  }else{
    0
  }
}

## Checking NA @ 2015_Q1
tbl_missing_nas <- sort(sapply(as.list(df_2015_Q1), FUN = noOfMissingValues), decreasing = T)
tbl_missing_nas <- as.matrix(tbl_missing_nas[tbl_missing_nas > 0])
colnames(tbl_missing_nas) <- "NAs"
tbl_missing_nas       # Calling to check NA

## Checking NULL @ 2015_Q1
tbl_empty <- sort(sapply(as.list(df_2015_Q1), FUN = noOfEmptyValues), decreasing = T)
tbl_empty <- as.matrix(tbl_empty[tbl_empty > 0])
colnames(tbl_empty) <- "EMPTYs"
tbl_empty             # Calling to check NULL

## Analysing each feature @ age 
tmp <- select(df_2015_Q1, fecha_dato, ncodpers, age)
age_an <- group_by(tmp, ncodpers) %>% 
  summarise(min = min(age, na.rm = T), max = max(age, na.rm = T), mean = mean(age, na.rm = T), nas = sum(is.na(age)))
idx_age_ageAvailableSomeNAs <- age_an$nas > 0 & (!is.na(age_an$min) | !is.na(age_an$max) | !is.na(age_an$mean))
idx_age_ageAvailableAllNAs <- age_an$nas > 0 & (is.na(age_an$min) & is.na(age_an$max) & is.na(age_an$mean))
age_an_1 <- age_an[idx_age_ageAvailableSomeNAs,]
age_an_2 <- age_an[idx_age_ageAvailableAllNAs,]
age_an_1$sameValues <- (age_an_1$min == age_an_1$max) & (age_an_1$mean == age_an_1$max)




df_2015_Q1[df_2015_Q1$ncodpers == 183439, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 227035, c(1:48), with = F]  
df_2015_Q1[df_2015_Q1$ncodpers == 237553, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 314773, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 329698, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 407854, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 508391, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 511523, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 525527, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 541633, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 553523, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 602125, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 624973, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 650529, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == , c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == , c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == , c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == , c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == , c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == , c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 5, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 5, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 5, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 5, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 35, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == , c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 5, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 5, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 227035, c(1:48), with = F] 
df_2015_Q1[df_2015_Q1$ncodpers == 227035, c(1:48), with = F] 







tmp <- select(df_2015_Q1, fecha_dato, ncodpers, renta)
renta_an <- group_by(tmp, ncodpers) %>% 
  summarise(min = min(renta, na.rm = T), max = max(renta, na.rm = T), mean = mean(renta, na.rm = T), nas = sum(is.na(renta)))
idx_renta_rentaAvailableSomeNAs <- renta_an$nas > 0 & (!is.na(renta_an$min) | !is.na(renta_an$max) | !is.na(renta_an$mean))
idx_renta_rentaAvailableAllNAs <- renta_an$nas > 0 & (is.na(renta_an$min) & is.na(renta_an$max) & is.na(renta_an$mean))
renta_an_1 <- renta_an[idx_renta_rentaAvailableSomeNAs,]
renta_an_2 <- renta_an[idx_renta_rentaAvailableAllNAs,]
renta_an_1$sameValues <- (renta_an_1$min == renta_an_1$max) & (renta_an_1$mean == renta_an_1$max)









