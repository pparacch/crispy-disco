# Exploration & Data Analysis
Pier Lorenzo Paracchini  
11/27/2016  



Using the `tidyverse` package for performing exploration and data analysis.


```r
require(data.table)
require(lubridate)
require(dplyr)
require(knitr)
input_file <- "./../../data/raw/train_ver2.csv"

#Splitting the training data as one file till 2016.04 (included)
input_train <- "./../../data/processed/df_train.RData"
input_train_typesSet <- "./../../data/processed/df_train_typesSet.RData"

#Splitting up the data by periods to have smaller chunks
#2015 split in qurters
#2016 from Jan to April (included)
input_2015_Q1 <- "./../../data/processed/df_2015_Q1.RData"
input_2015_Q2 <- "./../../data/processed/df_2015_Q2.RData"
input_2015_Q3 <- "./../../data/processed/df_2015_Q3.RData"
input_2015_Q4 <- "./../../data/processed/df_2015_Q4.RData"
input_2016 <- "./../../data/processed/df_2016.RData"

#Splitting the test data 2016.05 (only)
input_test <- "./../../data/processed/df_test.RData"
```

## Getting the data


```r
df <- fread(input = input_file, header = T, stringsAsFactors = F)
df$fecha_dato <- lubridate::ymd(df$fecha_dato)
df$fecha_dato_year <- lubridate::year(df$fecha_dato)
df$fecha_dato_month <- lubridate::month(df$fecha_dato)

#Splitting in test & training
idx_train <- df$fecha_dato_year <= 2016 & df$fecha_dato_month <= 4
df_train <- df[idx_train,]
save(df_train, file = input_train)

#Splititng Data horizontally by time
idx_2015_Q1 <- df$fecha_dato_year == 2015 & df$fecha_dato_month <= 3
idx_2015_Q2 <- df$fecha_dato_year == 2015 & (df$fecha_dato_month > 3 & df$fecha_dato_month <= 6)
idx_2015_Q3 <- df$fecha_dato_year == 2015 & (df$fecha_dato_month > 6 & df$fecha_dato_month <= 9)
idx_2015_Q4 <- df$fecha_dato_year == 2015 & df$fecha_dato_month > 9

df_2015_Q1 <- df[idx_2015_Q1,]
df_2015_Q2 <- df[idx_2015_Q2,]
df_2015_Q3 <- df[idx_2015_Q3,]
df_2015_Q4 <- df[idx_2015_Q4,]

idx_2016_til_april <- df$fecha_dato_year == 2016 & df$fecha_dato_month <= 4
idx_test <- df$fecha_dato_year == 2016 & df$fecha_dato_month > 4
df_2016 <- df[idx_2016_til_april,]
df_test <- df[idx_test,]

save(df_2015_Q1, file = input_2015_Q1)
save(df_2015_Q2, file = input_2015_Q2)
save(df_2015_Q3, file = input_2015_Q3)
save(df_2015_Q4, file = input_2015_Q4)

save(df_2016, file = input_2016)
save(df_test, file = input_test)
rm(list = c("df", "df_2015_Q1", "df_2015_Q2", "df_2015_Q3", "df_2015_Q4", "df_2016", "df_test", "df_train"))
rm(list = ls(pattern = "idx_*"))
```

## Data Exploration


```r
load(input_train)
df_train[,fecha_dato_year:= NULL]
df_train[,fecha_dato_month := NULL]

#Training dataset
#Dimension

dim(df_train)

#Structure 
str(df_train)

#Summary
summary(df_train)
```



```r
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

#setting the proper datatypes for the features
df_train <- setDataTypes(df_train)
save(df_train, file = input_train_typesSet)
```


```r
load(input_train_typesSet)

#structure
str(df_train)
## Classes 'data.table' and 'data.frame':	6202950 obs. of  48 variables:
##  $ fecha_dato           : Date, format: "2015-01-28" "2015-01-28" ...
##  $ ncodpers             : Factor w/ 945336 levels "15889","15890",..: 803901 539076 539077 539078 539079 539080 539081 539082 539084 539085 ...
##  $ ind_empleado         : Factor w/ 6 levels "","A","B","F",..: 5 5 5 5 5 5 5 5 5 5 ...
##  $ pais_residencia      : Factor w/ 119 levels "","AD","AE","AL",..: 38 38 38 38 38 38 38 38 38 38 ...
##  $ sexo                 : Factor w/ 3 levels "","H","V": 2 3 3 2 3 2 2 2 2 2 ...
##  $ age                  : int  35 23 23 22 23 23 23 23 24 23 ...
##  $ fecha_alta           : Date, format: "2015-01-12" "2012-08-10" ...
##  $ ind_nuevo            : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ antiguedad           : int  6 35 35 35 35 35 35 35 35 35 ...
##  $ indrel               : Factor w/ 2 levels "1","99": 1 1 1 1 1 1 1 1 1 1 ...
##  $ ult_fec_cli_1t       : Date, format: NA NA ...
##  $ indrel_1mes          : Factor w/ 10 levels "","1","1.0","2",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ tiprel_1mes          : Factor w/ 6 levels "","A","I","N",..: 2 3 3 3 2 3 3 2 3 3 ...
##  $ indresi              : Factor w/ 3 levels "","N","S": 3 3 3 3 3 3 3 3 3 3 ...
##  $ indext               : Factor w/ 3 levels "","N","S": 2 3 2 2 2 2 2 2 2 2 ...
##  $ conyuemp             : Factor w/ 3 levels "","N","S": 1 1 1 1 1 1 1 1 1 1 ...
##  $ canal_entrada        : Factor w/ 161 levels "","004","007",..: 154 151 151 150 151 151 151 151 151 151 ...
##  $ indfall              : Factor w/ 3 levels "","N","S": 2 2 2 2 2 2 2 2 2 2 ...
##  $ tipodom              : Factor w/ 1 level "1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ cod_prov             : Factor w/ 52 levels "1","2","3","4",..: 29 13 13 50 50 45 24 50 20 10 ...
##  $ nomprov              : Factor w/ 53 levels "","ALAVA","ALBACETE",..: 33 18 18 53 53 49 29 53 22 13 ...
##  $ ind_actividad_cliente: Factor w/ 2 levels "0","1": 2 1 1 1 2 1 1 2 1 1 ...
##  $ renta                : num  87218 35549 122179 119776 NA ...
##  $ segmento             : Factor w/ 4 levels "","01 - TOP",..: 3 4 4 4 4 4 4 4 4 4 ...
##  $ ind_ahor_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_aval_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_cco_fin_ult1     : int  1 1 1 0 1 1 1 1 1 1 ...
##  $ ind_cder_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_cno_fin_ult1     : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_ctju_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_ctma_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_ctop_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_ctpp_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_deco_fin_ult1    : int  0 0 0 1 0 0 0 0 0 0 ...
##  $ ind_deme_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_dela_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_ecue_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_fond_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_hip_fin_ult1     : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_plan_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_pres_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_reca_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_tjcr_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_valo_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_viv_fin_ult1     : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_nomina_ult1      : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_nom_pens_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ind_recibo_ult1      : int  0 0 0 0 0 0 0 0 0 0 ...
##  - attr(*, ".internal.selfref")=<externalptr>

#summary
summary(df_train)
##    fecha_dato            ncodpers       ind_empleado pais_residencia  
##  Min.   :2015-01-28   15889  :      8    :  22276    ES     :6150014  
##  1st Qu.:2015-03-28   15890  :      8   A:   1168           :  22276  
##  Median :2016-01-28   15892  :      8   B:   1649    FR     :   2399  
##  Mean   :2015-10-17   15893  :      8   F:   1174    AR     :   2261  
##  3rd Qu.:2016-03-28   15894  :      8   N:6176675    GB     :   2150  
##  Max.   :2016-04-28   15895  :      8   S:      8    DE     :   2146  
##                       (Other):6202902                (Other):  21704  
##  sexo             age           fecha_alta         ind_nuevo     
##   :  22306   Min.   :  2.00   Min.   :1995-01-16   0   :5868252  
##  H:2812620   1st Qu.: 24.00   1st Qu.:2004-04-15   1   : 312422  
##  V:3368024   Median : 39.00   Median :2011-09-07   NA's:  22276  
##              Mean   : 40.17   Mean   :2009-02-09                 
##              3rd Qu.: 50.00   3rd Qu.:2013-10-17                 
##              Max.   :164.00   Max.   :2016-04-29                 
##              NA's   :22276    NA's   :22276                      
##    antiguedad         indrel        ult_fec_cli_1t        indrel_1mes     
##  Min.   :-999999.0   1   :6170707   Min.   :2015-07-01   1      :3712625  
##  1st Qu.:     23.0   99  :   9967   1st Qu.:2015-07-23   1.0    :2445187  
##  Median :     52.0   NA's:  22276   Median :2016-01-28          :  42498  
##  Mean   :     77.1                  Mean   :2015-12-17   3.0    :   1035  
##  3rd Qu.:    136.0                  3rd Qu.:2016-03-11   3      :    672  
##  Max.   :    255.0                  Max.   :2016-04-28   P      :    309  
##  NA's   :22276                      NA's   :6192983      (Other):    624  
##  tiprel_1mes indresi     indext      conyuemp    canal_entrada    
##   :  42498    :  22276    :  22276    :6202133   KHE    :1858449  
##  A:2869150   N:  30660   N:5890591   N:    809   KAT    :1487523  
##  I:3289158   S:6150014   S: 290083   S:      8   KFC    :1392553  
##  N:      3                                       KHQ    : 301260  
##  P:   1835                                       KFA    : 186969  
##  R:    306                                       KHK    : 100231  
##                                                  (Other): 875965  
##  indfall     tipodom           cod_prov            nomprov       
##   :  22276   1   :6180674   28     :1997050   MADRID   :1997050  
##  N:6165063   NA's:  22276   8      : 568651   BARCELONA: 568651  
##  S:  15611                  46     : 307934   VALENCIA : 307934  
##                             41     : 277016   SEVILLA  : 277016  
##                             15     : 196083   CORUÑA, A: 196083  
##                             (Other):2803288   MURCIA   : 182700  
##                             NA's   :  52928   (Other)  :2673516  
##  ind_actividad_cliente     renta                        segmento      
##  0   :3266834          Min.   :    1203                     :  55827  
##  1   :2913840          1st Qu.:   68783   01 - TOP          : 263234  
##  NA's:  22276          Median :  101951   02 - PARTICULARES :3586881  
##                        Mean   :  134251   03 - UNIVERSITARIO:2297008  
##                        3rd Qu.:  156093                               
##                        Max.   :28894396                               
##                        NA's   :1317552                                
##  ind_ahor_fin_ult1  ind_aval_fin_ult1  ind_cco_fin_ult1
##  Min.   :0.000000   Min.   :0.00e+00   Min.   :0.0000  
##  1st Qu.:0.000000   1st Qu.:0.00e+00   1st Qu.:0.0000  
##  Median :0.000000   Median :0.00e+00   Median :1.0000  
##  Mean   :0.000107   Mean   :2.37e-05   Mean   :0.6767  
##  3rd Qu.:0.000000   3rd Qu.:0.00e+00   3rd Qu.:1.0000  
##  Max.   :1.000000   Max.   :1.00e+00   Max.   :1.0000  
##                                                        
##  ind_cder_fin_ult1   ind_cno_fin_ult1  ind_ctju_fin_ult1 
##  Min.   :0.0000000   Min.   :0.00000   Min.   :0.000000  
##  1st Qu.:0.0000000   1st Qu.:0.00000   1st Qu.:0.000000  
##  Median :0.0000000   Median :0.00000   Median :0.000000  
##  Mean   :0.0004053   Mean   :0.08381   Mean   :0.009867  
##  3rd Qu.:0.0000000   3rd Qu.:0.00000   3rd Qu.:0.000000  
##  Max.   :1.0000000   Max.   :1.00000   Max.   :1.000000  
##                                                          
##  ind_ctma_fin_ult1 ind_ctop_fin_ult1 ind_ctpp_fin_ult1 ind_deco_fin_ult1 
##  Min.   :0.00000   Min.   :0.0000    Min.   :0.00000   Min.   :0.000000  
##  1st Qu.:0.00000   1st Qu.:0.0000    1st Qu.:0.00000   1st Qu.:0.000000  
##  Median :0.00000   Median :0.0000    Median :0.00000   Median :0.000000  
##  Mean   :0.01065   Mean   :0.1338    Mean   :0.04492   Mean   :0.001596  
##  3rd Qu.:0.00000   3rd Qu.:0.0000    3rd Qu.:0.00000   3rd Qu.:0.000000  
##  Max.   :1.00000   Max.   :1.0000    Max.   :1.00000   Max.   :1.000000  
##                                                                          
##  ind_deme_fin_ult1  ind_dela_fin_ult1 ind_ecue_fin_ult1 ind_fond_fin_ult1
##  Min.   :0.000000   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
##  1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
##  Median :0.000000   Median :0.00000   Median :0.00000   Median :0.00000  
##  Mean   :0.001728   Mean   :0.04434   Mean   :0.08575   Mean   :0.01888  
##  3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
##  Max.   :1.000000   Max.   :1.00000   Max.   :1.00000   Max.   :1.00000  
##                                                                          
##  ind_hip_fin_ult1   ind_plan_fin_ult1 ind_pres_fin_ult1 ind_reca_fin_ult1
##  Min.   :0.000000   Min.   :0.00000   Min.   :0.0000    Min.   :0.0000   
##  1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.0000    1st Qu.:0.0000   
##  Median :0.000000   Median :0.00000   Median :0.0000    Median :0.0000   
##  Mean   :0.006169   Mean   :0.00952   Mean   :0.0028    Mean   :0.0537   
##  3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:0.0000    3rd Qu.:0.0000   
##  Max.   :1.000000   Max.   :1.00000   Max.   :1.0000    Max.   :1.0000   
##                                                                          
##  ind_tjcr_fin_ult1 ind_valo_fin_ult1 ind_viv_fin_ult1   ind_nomina_ult1
##  Min.   :0.00000   Min.   :0.0000    Min.   :0.000000   Min.   :0.000  
##  1st Qu.:0.00000   1st Qu.:0.0000    1st Qu.:0.000000   1st Qu.:0.000  
##  Median :0.00000   Median :0.0000    Median :0.000000   Median :0.000  
##  Mean   :0.04511   Mean   :0.0268    Mean   :0.003994   Mean   :0.056  
##  3rd Qu.:0.00000   3rd Qu.:0.0000    3rd Qu.:0.000000   3rd Qu.:0.000  
##  Max.   :1.00000   Max.   :1.0000    Max.   :1.000000   Max.   :1.000  
##                                                         NA's   :11432  
##  ind_nom_pens_ult1 ind_recibo_ult1 
##  Min.   :0.00      Min.   :0.0000  
##  1st Qu.:0.00      1st Qu.:0.0000  
##  Median :0.00      Median :0.0000  
##  Mean   :0.06      Mean   :0.1328  
##  3rd Qu.:0.00      3rd Qu.:0.0000  
##  Max.   :1.00      Max.   :1.0000  
##  NA's   :11432
```

### Missing Values/ Empty Values Analysis

Working with all of the data from beginning til April 2016 (included) and trying to answer the following question __Which feature has missing values? Or Which feature (either character or factor) has empty values?__


```r
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

tbl_missing_nas <- sort(sapply(as.list(df_train), FUN = noOfMissingValues), decreasing = T)
tbl_missing_nas <- as.matrix(tbl_missing_nas[tbl_missing_nas > 0])
colnames(tbl_missing_nas) <- "NAs"

tbl_empty <- sort(sapply(as.list(df_train), FUN = noOfEmptyValues), decreasing = T)
tbl_empty <- as.matrix(tbl_empty[tbl_empty > 0])
colnames(tbl_empty) <- "EMPTYs"
```


Table: Summary OF MISSING VALUES (>0) BY FEATURE (ALL FEATURES)

                             NAs
----------------------  --------
ult_fec_cli_1t           6192983
renta                    1317552
cod_prov                   52928
age                        22276
fecha_alta                 22276
ind_nuevo                  22276
antiguedad                 22276
indrel                     22276
tipodom                    22276
ind_actividad_cliente      22276
ind_nomina_ult1            11432
ind_nom_pens_ult1          11432




Table: Summary OF EMPTY VALUES (>0) BY FEATURE (CHARACTER & FACTOR ONLY)

                    EMPTYs
----------------  --------
conyuemp           6202133
segmento             55827
canal_entrada        54569
nomprov              52928
indrel_1mes          42498
tiprel_1mes          42498
sexo                 22306
ind_empleado         22276
pais_residencia      22276
indresi              22276
indext               22276
indfall              22276



__ult_fec_cli_1t__, last date as primary customer (if he isn't at the end of the month)

__renta__, Gross income of the household


```r
tmp <- select(df_train, fecha_dato, ncodpers, renta)
renta_an <- group_by(tmp, ncodpers) %>% 
    summarise(min = min(renta, na.rm = T), max = max(renta, na.rm = T), mean = mean(renta, na.rm = T), nas = sum(is.na(renta)))

idx_renta_rentaAvailableSomeNAs <- renta_an$nas > 0 & (!is.na(renta_an$min) | !is.na(renta_an$max) | !is.na(renta_an$mean))
idx_renta_rentaAvailableAllNAs <- renta_an$nas > 0 & (is.na(renta_an$min) & is.na(renta_an$max) & is.na(renta_an$mean))

renta_an_1 <- renta_an[idx_renta_rentaAvailableSomeNAs,]
renta_an_2 <- renta_an[idx_renta_rentaAvailableAllNAs,]
renta_an_1$sameValues <- (renta_an_1$min == renta_an_1$max) & (renta_an_1$mean == renta_an_1$max)
```

From the data below we can see that there are 49 clients where it is posible to recove the missing renta just just using the available renta for the same client not NA. On teh other side there are 237690 clients where such information cannot be found.



Table: Summary OF CLIENTS WITH Missing renta for some entries

ncodpers          min         max        mean   nas  sameValues 
---------  ----------  ----------  ----------  ----  -----------
183439      134512.83   134512.83   134512.83     4  TRUE       
227035      691513.77   691513.77   691513.77     2  TRUE       
237553      367279.05   367279.05   367279.05     2  TRUE       
258075      161469.84   161469.84   161469.84     3  TRUE       
314773       93224.55    93224.55    93224.55     4  TRUE       
329698       80374.74    80374.74    80374.74     4  TRUE       
407854       49531.41    49531.41    49531.41     4  TRUE       
508391      111592.62   111592.62   111592.62     3  TRUE       
511523       48663.81    48663.81    48663.81     3  TRUE       
525527      216098.07   216098.07   216098.07     4  TRUE       
541633      154167.24   154167.24   154167.24     4  TRUE       
553523      169494.24   169494.24   169494.24     4  TRUE       
602125      301414.59   301414.59   301414.59     1  TRUE       
624973       71828.64    71828.64    71828.64     4  TRUE       
626915       65924.67    65924.67    65924.67     4  TRUE       
650529       82098.03    82098.03    82098.03     4  TRUE       
670125      189111.51   189111.51   189111.51     3  TRUE       
704123      303118.53   303118.53   303118.53     4  TRUE       
708861      103593.63   103593.63   103593.63     4  TRUE       
730041       48126.69    48126.69    48126.69     4  TRUE       
761015       75830.67    75830.67    75830.67     2  TRUE       
767789      138182.34   138182.34   138182.34     3  TRUE       
872308       87466.62    87466.62    87466.62     4  TRUE       
958957       78080.79    78080.79    78080.79     1  TRUE       
1023366      65037.84    65037.84    65037.84     3  TRUE       
1058086      94322.67    94322.67    94322.67     4  TRUE       
1135973     105021.39   105021.39   105021.39     2  TRUE       
1138614     556310.67   556310.67   556310.67     3  TRUE       
1224617      88265.25    88265.25    88265.25     1  TRUE       
1237901     112292.43   112292.43   112292.43     1  TRUE       
1238073     181864.83   181864.83   181864.83     2  TRUE       
1239302     277918.20   277918.20   277918.20     3  TRUE       
1246401     149977.17   149977.17   149977.17     2  TRUE       
1249566      84892.53    84892.53    84892.53     1  TRUE       
1249946      90196.29    90196.29    90196.29     4  TRUE       
1253914     156985.65   156985.65   156985.65     1  TRUE       
1265115     148928.13   148928.13   148928.13     1  TRUE       
1266330      65572.47    65572.47    65572.47     4  TRUE       
1319315     149537.88   149537.88   149537.88     4  TRUE       
1327313     128379.21   128379.21   128379.21     4  TRUE       
1363763     106737.54   106737.54   106737.54     3  TRUE       
1369095      61756.56    61756.56    61756.56     4  TRUE       
1376219     114618.48   114618.48   114618.48     4  TRUE       
1376650      39661.68    39661.68    39661.68     4  TRUE       
1376785     117029.82   117029.82   117029.82     3  TRUE       
1377797     103033.68   103033.68   103033.68     3  TRUE       
1383721     122384.40   122384.40   122384.40     2  TRUE       
1385426     317745.42   317745.42   317745.42     2  TRUE       
1394072      85008.78    85008.78    85008.78     1  TRUE       




Table: Summary OF CLIENTS (first 20s) WITH Missing renta all entries

ncodpers    min   max   mean   nas
---------  ----  ----  -----  ----
15917        NA    NA    NaN     8
15945        NA    NA    NaN     4
15980        NA    NA    NaN     4
15993        NA    NA    NaN     8
15997        NA    NA    NaN     8
16022        NA    NA    NaN     8
16031        NA    NA    NaN     8
16041        NA    NA    NaN     8
16048        NA    NA    NaN     8
16060        NA    NA    NaN     8
16063        NA    NA    NaN     4
16095        NA    NA    NaN     8
16098        NA    NA    NaN     8
16102        NA    NA    NaN     4
16114        NA    NA    NaN     8
16135        NA    NA    NaN     4
16138        NA    NA    NaN     8
16177        NA    NA    NaN     8
16179        NA    NA    NaN     8
16203        NA    NA    NaN     4


