#===================================================================================================
#
# File:        SimpleClusterAnalysis.R
# Description: Perform a simple cluster analysis on April training data with data segmented by the
#              most prevalent values of the segmento variable. The results may be useful for 
#              improving recommender performance by grouping association rules by found clusters
#              of 
#
#==================================================================================================
library(lubridate)
library(caret)
library(fpc)


#
# Load data
#
load("df_May2016.RData")
load("df_April2016.RData")

train.may <- df_may_2016
train.april <- df_april_2016

rm(df_may_2016)
rm(df_april_2016)
gc()


# Examine the ind_empleado variable and fix up data if needed
table(train.april$ind_empleado, useNA = "always")
train.april$ind_empleado <- as.factor(train.april$ind_empleado)

table(train.may$ind_empleado, useNA = "always")
train.may$ind_empleado <- as.factor(train.may$ind_empleado)


# Examine the pais_residencia variable and fix up data if needed
table(train.april$pais_residencia, useNA = "always")
train.april$pais_residencia <- as.factor(train.april$pais_residencia)

table(train.may$pais_residencia, useNA = "always")
train.may$pais_residencia <- as.factor(train.may$pais_residencia)


# Examine the sexo variable and fix up data if needed
table(train.april$sexo, useNA = "always")
train.april$sexo[train.april$sexo == ""] <- "U"
train.april$sexo <- as.factor(train.april$sexo)

table(train.may$sexo, useNA = "always")
train.may$sexo[train.may$sexo == ""] <- "U"
train.may$sexo <- as.factor(train.may$sexo)


# Examine the age variable and fix up data if needed
train.april$age <- as.numeric(train.april$age)
summary(train.april$age)

train.may$age <- as.numeric(train.may$age)
summary(train.may$age)


# Examine the fecha_alta variable and fix up data if needed
length(which(train.april$fecha_alta == ""))
length(which(is.na(train.april$fecha_alta)))
train.april$fecha_alta <- date(train.april$fecha_alta)

length(which(train.may$fecha_alta == ""))
length(which(is.na(train.may$fecha_alta)))
train.may$fecha_alta <- date(train.may$fecha_alta)


# Examine the ind_nuevo variable and fix up data if needed
summary(train.april$ind_nuevo)
table(train.april$ind_nuevo, useNA = "always")
train.april$ind_nuevo <- as.factor(train.april$ind_nuevo)

summary(train.may$ind_nuevo)
table(train.may$ind_nuevo, useNA = "always")
train.may$ind_nuevo <- as.factor(train.may$ind_nuevo)


# Examine the antiguedad variable and fix up data if needed
train.april$antiguedad <- as.numeric(train.april$antiguedad)
summary(train.april$antiguedad)

train.may$antiguedad <- as.numeric(train.may$antiguedad)
summary(train.may$antiguedad)


# Examine the indrel variable and fix up data if needed
summary(train.april$indrel)
table(train.april$indrel, useNA = "always")
train.april$indrel <- as.factor(train.april$indrel)

summary(train.may$indrel)
table(train.may$indrel, useNA = "always")
train.may$indrel <- as.factor(train.may$indrel)


# Examine the ult_fec_cli_1t variable and fix up data if needed
table(train.april$ult_fec_cli_1t, useNA = "always")
table(train.may$ult_fec_cli_1t, useNA = "always")


# Examine the indrel_1mes variable and fix up data if needed
table(train.april$indrel_1mes, useNA = "always")
train.april$indrel_1mes[train.april$indrel_1mes == "1.0"] <- "1"
train.april$indrel_1mes[train.april$indrel_1mes == "2.0"] <- "2"
train.april$indrel_1mes[train.april$indrel_1mes == "3.0"] <- "3"
train.april$indrel_1mes[train.april$indrel_1mes == "4.0"] <- "4"
train.april$indrel_1mes[train.april$indrel_1mes == ""] <- "U"
train.april$indrel_1mes <- as.factor(train.april$indrel_1mes)

table(train.may$indrel_1mes, useNA = "always")
train.may$indrel_1mes[train.may$indrel_1mes == "1.0"] <- "1"
train.may$indrel_1mes[train.may$indrel_1mes == "2.0"] <- "2"
train.may$indrel_1mes[train.may$indrel_1mes == "3.0"] <- "3"
train.may$indrel_1mes[train.may$indrel_1mes == "4.0"] <- "4"
train.may$indrel_1mes[train.may$indrel_1mes == ""] <- "U"
train.may$indrel_1mes <- as.factor(train.may$indrel_1mes)


# Examine the tiprel_1mes variable and fix up data if needed
table(train.april$tiprel_1mes, useNA = "always")
train.april$tiprel_1mes[train.april$tiprel_1mes == ""] <- "U"
train.april$tiprel_1mes <- as.factor(train.april$tiprel_1mes)

table(train.may$tiprel_1mes, useNA = "always")
train.may$tiprel_1mes[train.may$tiprel_1mes == ""] <- "U"
train.may$tiprel_1mes <- as.factor(train.may$tiprel_1mes)


# Examine the indresi variable and fix up data if needed
table(train.april$indresi, useNA = "always")
train.april$indresi <- as.factor(train.april$indresi)

table(train.may$indresi, useNA = "always")
train.may$indresi <- as.factor(train.may$indresi)


# Examine the indext variable and fix up data if needed
table(train.april$indext, useNA = "always")
train.april$indext <- as.factor(train.april$indext)

table(train.may$indext, useNA = "always")
train.may$indext <- as.factor(train.may$indext)


# Examine the conyuemp variable and fix up data if needed
table(train.april$conyuemp, useNA = "always")
train.april$conyuemp[train.april$conyuemp == ""] <- "U"
train.april$conyuemp <- as.factor(train.april$conyuemp)

table(train.may$conyuemp, useNA = "always")
train.may$conyuemp[train.may$conyuemp == ""] <- "U"
train.may$conyuemp <- as.factor(train.may$conyuemp)


# Examine the canal_entrada variable and fix up data if needed
table(train.april$canal_entrada, useNA = "always")
train.april$canal_entrada[train.april$canal_entrada == ""] <- "U"
train.april$canal_entrada <- as.factor(train.april$canal_entrada)

table(train.may$canal_entrada, useNA = "always")
train.may$canal_entrada[train.may$canal_entrada == ""] <- "U"
train.may$canal_entrada <- as.factor(train.may$canal_entrada)


# Examine the indfall variable and fix up data if needed
table(train.april$indfall, useNA = "always")
train.april$indfall <- as.factor(train.april$indfall)

table(train.may$indfall, useNA = "always")
train.may$indfall <- as.factor(train.may$indfall)


# Examine the tipodom variable and fix up data if needed
table(train.april$tipodom, useNA = "always")
table(train.may$tipodom, useNA = "always")


# Examine the cod_prov variable and fix up data if needed
table(train.april$cod_prov, useNA = "always")
train.april$cod_prov[is.na(train.april$cod_prov)] <- "U"
train.april$cod_prov <- as.factor(train.april$cod_prov)

table(train.may$cod_prov, useNA = "always")
train.may$cod_prov[is.na(train.may$cod_prov)] <- "U"
train.may$cod_prov <- as.factor(train.may$cod_prov)


# Examine the nomprov variable and fix up data if needed
table(train.april$nomprov, useNA = "always")
table(train.may$nomprov, useNA = "always")


# Examine the ind_actividad_cliente variable and fix up data if needed
table(train.april$ind_actividad_cliente, useNA = "always")
train.april$ind_actividad_cliente <- as.factor(train.april$ind_actividad_cliente)

table(train.may$ind_actividad_cliente, useNA = "always")
train.may$ind_actividad_cliente <- as.factor(train.may$ind_actividad_cliente)


# Examine the renta variable and fix up data if needed
summary(train.april$renta)
summary(train.may$renta)


# Examine the segmento variable and fix up data if needed
table(train.april$segmento, useNA = "always")
train.april$segmento[train.april$segmento == ""] <- "U"
train.april$segmento <- as.factor(train.april$segmento)

table(train.may$segmento, useNA = "always")
train.may$segmento[train.may$segmento == ""] <- "U"
train.may$segmento <- as.factor(train.may$segmento)


# Subset features to those that make sense for clustering
cluster.features <- c("ind_empleado", "pais_residencia", "sexo", "age", "ind_nuevo", "antiguedad", 
                      "indrel", "indrel_1mes", "tiprel_1mes", "indresi", "indext", "conyuemp", 
                      "canal_entrada", "indfall", "cod_prov", "ind_actividad_cliente", "segmento")

# Convert factor variable to binary dummy variables
#
# NOTE - This code takes a while to run! Load from disk instead!
#
# dummy.vars <- dummyVars(~ ., data = train.april[, cluster.features])
# train.april.cluster <- data.frame(predict(dummy.vars, train.april[, cluster.features]))
# save(train.april.cluster, file = "df_AprilClusterTrain.RData")
load("df_AprilClusterTrain.RData")


#
# Use the segmento feature as the first splitting mechanism.
#


# Utility function for helping to find the number of clusters
try.clusters <- function(seed, data) {
  set.seed(seed)
  
  wss <- (nrow(data)-1) * sum(apply(data, 2, var))
  for (i in 2:8) wss[i] <- sum(kmeans(data,
                                      centers = i,
                                      iter.max = 20,
                                      nstart = 3)$withinss)
  plot(1:8, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
}


# Start with segmento of "01 - TOP"
top.indexes <- which(train.april$segmento == "01 - TOP")
top.april.cluster <- train.april.cluster[top.indexes, 1:368]

# Scree plot "elbow" at 2 clusters.
try.clusters(349857, top.april.cluster)


# Now segmento of "02 - PARTICULARES"
particulares.indexes <- which(train.april$segmento == "02 - PARTICULARES")
particulares.april.cluster <- train.april.cluster[particulares.indexes, 1:368]

# Scree plot "elbow" at 4 clusters.
try.clusters(652223, particulares.april.cluster)


# segmento of "03 - UNIVERSITARIO"
universitario.indexes <- which(train.april$segmento == "03 - UNIVERSITARIO")
universitario.april.cluster <- train.april.cluster[universitario.indexes, 1:368]

# Scree plot "elbow" at 7 clusters.
try.clusters(56259, universitario.april.cluster)



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



#
# Evaluate clusters for "01 - TOP"
#

# Subset data
top.april <- train.april[top.indexes,]

# Train kmeans algorithm
set.seed(439587)
top.kmeans <- kmeans(top.april.cluster, centers = 2, iter.max = 20, nstart = 3)

top.april$cluster <- top.kmeans$cluster
table(top.april$cluster)

# NOTE - NO need to evaluate give that there's really only 1 effective cluster!



#
# Evaluate clusters for "02 - PARTICULARES"
#

# Subset data
particulares.april <- train.april[particulares.indexes,]

# Train kmeans algorithm
set.seed(96736)
particulares.kmeans <- kmeans(particulares.april.cluster, centers = 4, iter.max = 20, nstart = 3)

particulares.april$cluster <- particulares.kmeans$cluster
table(particulares.april$cluster)


# Calculate the Ochiai Coefficient for cluster 2
particulares.april.2 <- particulares.april %>%
  filter(cluster == 2)
set.seed(89432)
particulares.april.2.ochiai <- avg.ochiai(sample_n(particulares.april.2[,25:48],
                                                   size = 5000))

# Calculate the Ochiai Coefficient for cluster 3
particulares.april.3 <- particulares.april %>%
  filter(cluster == 3)
set.seed(543296)
particulares.april.3.ochiai <- avg.ochiai(sample_n(particulares.april.3[,25:48],
                                                   size = 5000))


# Calculate the Ochiai Coefficient for cluster 4
particulares.april.4 <- particulares.april %>%
  filter(cluster == 4)
set.seed(72449)
particulares.april.4.ochiai <- avg.ochiai(sample_n(particulares.april.4[,25:48],
                                                   size = 5000))



#
# Evaluate clusters for "03 - UNIVERSITARIO"
#

# Subset data
universitario.april <- train.april[universitario.indexes,]

# Train kmeans algorithm
set.seed(96736)
universitario.kmeans <- kmeans(universitario.april.cluster, centers = 7, iter.max = 20, nstart = 3)

universitario.april$cluster <- universitario.kmeans$cluster
table(universitario.april$cluster)








