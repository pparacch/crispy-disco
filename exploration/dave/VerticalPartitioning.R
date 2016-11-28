#===================================================================================================
#
# File:        VerticalPartitioning.R
# Description: Split the raw training data vertically for non-product variable to decrease data
#              file size. Each file contains the following:
#
#              1. The fecha_Data variable.
#              2. The ncodpers variables.
#              3. A non-product variable.
#
#==================================================================================================



#
# Load data
#
train <- read.csv("train_ver2.csv", stringsAsFactors = FALSE)


# Save off data frame for the ind_empleado variable
df_ind_empleado <- data.frame(cbind(fecha_dato = train$fecha_dato,
                                    ncodpers = train$ncodpers,
                                    ind_empleado = train$ind_empleado))
save(df_ind_empleado, file = "df_ind_empleado.RData")


# Save off data frame for the pais_residencia variable
df_pais_residencia <- data.frame(cbind(fecha_dato = train$fecha_dato,
                                       ncodpers = train$ncodpers,
                                       pais_residencia = train$pais_residencia))
save(df_pais_residencia, file = "df_pais_residencia.RData")


# Save off data frame for the sexo variable
df_sexo <- data.frame(cbind(fecha_dato = train$fecha_dato,
                            ncodpers = train$ncodpers,
                            sexo = train$sexo))
save(df_sexo, file = "df_sexo.RData")


# Save off data frame for the age variable
df_age <- data.frame(cbind(fecha_dato = train$fecha_dato,
                           ncodpers = train$ncodpers,
                           age = train$age))
save(df_age, file = "df_age.RData")


# Save off data frame for the fecha_alta variable
df_fecha_alta <- data.frame(cbind(fecha_dato = train$fecha_dato,
                                  ncodpers = train$ncodpers,
                                  fecha_alta = train$fecha_alta))
save(df_fecha_alta, file = "df_fecha_alta.RData")


# Save off data frame for the ind_nuevo variable
df_ind_nuevo <- data.frame(cbind(fecha_dato = train$fecha_dato,
                                 ncodpers = train$ncodpers,
                                 ind_nuevo = train$ind_nuevo))
save(df_ind_nuevo, file = "df_ind_nuevo.RData")


# Save off data frame for the antiguedad variable
df_antiguedad <- data.frame(cbind(fecha_dato = train$fecha_dato,
                                  ncodpers = train$ncodpers,
                                  antiguedad = train$antiguedad))
save(df_antiguedad, file = "df_antiguedad.RData")


# Save off data frame for the indrel variable
df_indrel <- data.frame(cbind(fecha_dato = train$fecha_dato,
                              ncodpers = train$ncodpers,
                              indrel = train$indrel))
save(df_indrel, file = "df_indrel.RData")


# Save off data frame for the ult_fec_cli_1t variable
df_ult_fec_cli_1t <- data.frame(cbind(fecha_dato = train$fecha_dato,
                                      ncodpers = train$ncodpers,
                                      ult_fec_cli_1t = train$ult_fec_cli_1t))
save(df_ult_fec_cli_1t, file = "df_ult_fec_cli_1t.RData")


# Save off data frame for the indrel_1mes variable
df_indrel_1mes <- data.frame(cbind(fecha_dato = train$fecha_dato,
                                   ncodpers = train$ncodpers,
                                   indrel_1mes = train$indrel_1mes))
save(df_indrel_1mes, file = "df_indrel_1mes.RData")


# Save off data frame for the tiprel_1mes variable
df_tiprel_1mes <- data.frame(cbind(fecha_dato = train$fecha_dato,
                                   ncodpers = train$ncodpers,
                                   tiprel_1mes = train$tiprel_1mes))
save(df_tiprel_1mes, file = "df_tiprel_1mes.RData")


# Save off data frame for the indresi variable
df_indresi <- data.frame(cbind(fecha_dato = train$fecha_dato,
                               ncodpers = train$ncodpers,
                               indresi = train$indresi))
save(df_indresi, file = "df_indresi.RData")


# Save off data frame for the indext variable
df_indext <- data.frame(cbind(fecha_dato = train$fecha_dato,
                              ncodpers = train$ncodpers,
                              indext = train$indext))
save(df_indext, file = "df_indext.RData")


# Save off data frame for the conyuemp variable
df_conyuemp <- data.frame(cbind(fecha_dato = train$fecha_dato,
                                ncodpers = train$ncodpers,
                                conyuemp = train$conyuemp))
save(df_conyuemp, file = "df_conyuemp.RData")


# Save off data frame for the canal_entrada variable
df_canal_entrada <- data.frame(cbind(fecha_dato = train$fecha_dato,
                                     ncodpers = train$ncodpers,
                                     canal_entrada = train$canal_entrada))
save(df_canal_entrada, file = "df_canal_entrada.RData")


# Save off data frame for the indfall variable
df_indfall <- data.frame(cbind(fecha_dato = train$fecha_dato,
                               ncodpers = train$ncodpers,
                               indfall = train$indfall))
save(df_indfall, file = "df_indfall.RData")


# Save off data frame for the tipodom variable
df_tipodom <- data.frame(cbind(fecha_dato = train$fecha_dato,
                               ncodpers = train$ncodpers,
                               tipodom = train$tipodom))
save(df_tipodom, file = "df_tipodom.RData")


# Save off data frame for the cod_prov variable
df_cod_prov <- data.frame(cbind(fecha_dato = train$fecha_dato,
                                ncodpers = train$ncodpers,
                                cod_prov = train$cod_prov))
save(df_cod_prov, file = "df_cod_prov.RData")


# Save off data frame for the nomprov variable
df_nomprov <- data.frame(cbind(fecha_dato = train$fecha_dato,
                               ncodpers = train$ncodpers,
                               nomprov = train$nomprov))
save(df_nomprov, file = "df_nomprov.RData")


# Save off data frame for the ind_actividad_cliente variable
df_ind_actividad_cliente <- data.frame(cbind(fecha_dato = train$fecha_dato,
                                             ncodpers = train$ncodpers,
                                             ind_actividad_cliente = train$ind_actividad_cliente))
save(df_ind_actividad_cliente, file = "df_ind_actividad_cliente.RData")


# Save off data frame for the renta variable
df_renta <- data.frame(cbind(fecha_dato = train$fecha_dato,
                             ncodpers = train$ncodpers,
                             renta = train$renta))
save(df_renta, file = "df_renta.RData")


# Save off data frame for the segmento variable
df_segmento <- data.frame(cbind(fecha_dato = train$fecha_dato,
                                ncodpers = train$ncodpers,
                                segmento = train$segmento))
save(df_segmento, file = "df_segmento.RData")



