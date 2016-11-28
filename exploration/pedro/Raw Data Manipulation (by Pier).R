## LOADING THE DATA
df <- fread(input = "train.csv", header = T, stringsAsFactors = F)
df$fecha_dato <- lubridate::ymd(df$fecha_dato)
df$fecha_dato_year <- lubridate::year(df$fecha_dato)
df$fecha_dato_month <- lubridate::month(df$fecha_dato)

idx_2015_Q1 <- df$fecha_dato_year == 2015 & df$fecha_dato_month <= 3
idx_2015_Q2 <- df$fecha_dato_year == 2015 & (df$fecha_dato_month > 3 & df$fecha_dato_month <= 6)
idx_2015_Q3 <- df$fecha_dato_year == 2015 & (df$fecha_dato_month > 6 & df$fecha_dato_month <= 9)
idx_2015_Q4 <- df$fecha_dato_year == 2015 & df$fecha_dato_month > 9

df_2015_Q1 <- df[idx_2015_Q1,]
df_2015_Q2 <- df[idx_2015_Q2,]
df_2015_Q3 <- df[idx_2015_Q3,]
df_2015_Q4 <- df[idx_2015_Q4,]

df_2016 <- df[df$fecha_dato_year == 2016,]

save(df_2015_Q1, file = input_2015_Q1)
save(df_2015_Q2, file = input_2015_Q2)
save(df_2015_Q3, file = input_2015_Q3)
save(df_2015_Q4, file = input_2015_Q4)

save(df_2016, file = input_2016)
rm(list = c("df", "df_2015_Q1", "df_2015_Q2", "df_2015_Q3", "df_2015_Q4", "df_2016"))

## DATA EXPLORATION

setDataTypes <- function(x){
  x <- df_2015_Q1
  x$ncodpers <- as.factor(x$ncodpers)
  x$ind_empleado <- as.factor(x$ind_empleado)
  x$pais_residencia <- as.factor(x$pais_residencia)
  x$sexo <- as.factor(x$sexo)
  x$fecha_alta <- lubridate::ymd(x$fecha_alta)
  x$ind_nuevo <- factor(x$ind_nuevo, levels = c(0,1), labels = c("NO", "YES"))
  x$indrel <- as.factor(x$indrel)
  
  idx_isEmpty <- x$ult_fec_cli_1t == ""
  x$ult_fec_cli_1t[idx_isEmpty] <- NA
  x$ult_fec_cli_1t[!idx_isEmpty] <-lubridate::ymd(x$ult_fec_cli_1t[!idx_isEmpty])
  
}
load(input_2015_Q1)
table(df_2015_Q1$fecha_dato_year, df_2015_Q1$fecha_dato_month)