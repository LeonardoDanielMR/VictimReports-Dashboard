#----------------------------------------------------#
#             PROCESAMIENTO DE DATOS                 #
#----------------------------------------------------#

library(foreign)     # Lectura SPPS
library(funModeling) # Valores perdidos 
library(haven)
library(dplyr)       # para usar pipes 
library(DMwR2)       # imputación

# Lectura de datos
caracteristicas<-read.spss("Capítulo_200_Denuncia_de_Delitos_2017.sav",
                           use.value.labels=FALSE,
                           to.data.frame=TRUE,reencode="latin1")

# Renombrando
caracteristicas <-rename(caracteristicas,DIA=IH203_DIA)
caracteristicas <-rename(caracteristicas,MES=IH203_MES)
caracteristicas <-rename(caracteristicas,ANIO=IH203_ANIO)
caracteristicas <-rename(caracteristicas,HOR=IH204_HOR)
caracteristicas <-rename(caracteristicas,UBIGEO=UBIGEO_HECHO)

caracteristicas <-rename(caracteristicas,DELITO_GENERICO=IH208_GENERICO)
caracteristicas <-rename(caracteristicas,DELITO_ESPECIFICO=IH208_ESPECIFICO)
caracteristicas <-rename(caracteristicas,DELITO_MODALIDAD=IH208_MODALIDAD)
caracteristicas <-rename(caracteristicas,VIA=H206)


# Eliminando variables que no interesan en el estudio y con 
# demasiados valores perdidos (porcentaje mayor al 10%)
caracteristicas$IH201<-NULL   
caracteristicas$IH201_O<-NULL
caracteristicas$IH201_A<-NULL
caracteristicas$IH201_A_O<-NULL
caracteristicas$UBIGEO_CIA<-NULL
caracteristicas$IH205_DD<-NULL
caracteristicas$IH205_PP<-NULL
caracteristicas$IH205_DI<-NULL
caracteristicas$IH206_O<-NULL
caracteristicas$IH207_A<-NULL
caracteristicas$IH207_B<-NULL
caracteristicas$IH209_O<-NULL
caracteristicas$IH210_A_O<-NULL
caracteristicas$IH210_B_O<-NULL
caracteristicas$IH211_1<-NULL
caracteristicas$IH211_2<-NULL
caracteristicas$IH211_3<-NULL
caracteristicas$IH211_4<-NULL
caracteristicas$IH211_5<-NULL
caracteristicas$IH211_6<-NULL
caracteristicas$IH211_7<-NULL
caracteristicas$IH211_8<-NULL
caracteristicas$IH211_9<-NULL
caracteristicas$IH211_10<-NULL
caracteristicas$IH211_11<-NULL
caracteristicas$IH211_12<-NULL
caracteristicas$IH211_13<-NULL
caracteristicas$IH211_14<-NULL
caracteristicas$IH211_15<-NULL
caracteristicas$IH211_16<-NULL
caracteristicas$IH211_17<-NULL
caracteristicas$IH211_18<-NULL
caracteristicas$IH211_19<-NULL
caracteristicas$IH211_20<-NULL
caracteristicas$IH211_21<-NULL
caracteristicas$IH211_22<-NULL
caracteristicas$IH211_23<-NULL
caracteristicas$IH211_24<-NULL
caracteristicas$IH211_24_O<-NULL
caracteristicas$FUENTE<-NULL
caracteristicas$IH204_MIN<-NULL 
caracteristicas$IH210_A<-NULL
caracteristicas$IH212<-NULL
caracteristicas$IH208_A<-NULL
caracteristicas$ID_N<-NULL
caracteristicas$IH210_B<-NULL
caracteristicas$IH209<-NULL

# Recodificando Via
caracteristicas$VIA[caracteristicas$VIA == 1] <- "Avenida"
caracteristicas$VIA[caracteristicas$VIA == 2] <- "Jiron"
caracteristicas$VIA[caracteristicas$VIA == 3] <- "Calle"
caracteristicas$VIA[caracteristicas$VIA == 4] <- "Pasaje"
caracteristicas$VIA[caracteristicas$VIA == 5] <- "Carretera"
caracteristicas$VIA[caracteristicas$VIA == 6] <- "Prologoncacion"
caracteristicas$VIA[caracteristicas$VIA == 7] <- "Otros"

# En la data 99 --> No especificó
# RECODIFICANDO 99 --> NA
# a) recodificando dia
caracteristicas[,5] = replace(caracteristicas[,c(5)],
                              caracteristicas[,c(5)]==99,NA)
# b) recodificando mes
caracteristicas[,6] = replace(caracteristicas[,c(6)],
                              caracteristicas[,c(6)]==99,NA)
# c) recodificando hora
caracteristicas[,8] = replace(caracteristicas[,c(8)],
                              caracteristicas[,c(8)]==99,NA)
# Recodificando 0 -> 24
caracteristicas[,8] = replace(caracteristicas[,c(8)],
                               caracteristicas[,c(8)]==0,24)
# Creando Intervalos para la Hora: 
# MADRUGADA 24 AM - 5 AM
# MAÑANA 6 AM - 12 AM
# TARDE 13 PM - 18 PM
# NOCHE 19 PM - 23 PM
puntos_corte <- c(0, 5, 12, 18, 24)
etiquetas <- c("Madrugada", "Mañana", "Tarde", "Noche")
# Recodificar las horas en intervalos
caracteristicas$HOR <- cut(caracteristicas$HOR, breaks = puntos_corte,
                           labels = etiquetas, right = FALSE)
# Valores perdidos
df_status(caracteristicas, print_results = F) 
# Imputando
# Variables categóricas
horas_completas <- caracteristicas$HOR[!is.na(caracteristicas$HOR)]
caracteristicas$HOR[is.na(caracteristicas$HOR)] <- sample(horas_completas,
                                                          sum(is.na(caracteristicas$HOR)),
                                                          replace = TRUE)
# Variables numéricas
caracteristicas<-centralImputation(caracteristicas)
# Creando columna Fecha:
# Combinar las columnas en una columna de fecha con formato año-mes-dia
caracteristicas$FECHA <- as.Date(with(caracteristicas,
                                      paste(ANIO, MES, DIA, sep = "-")),
                                 format = "%Y-%m-%d")

# Luego recodificamos el mes:
caracteristicas$MES[caracteristicas$MES == 1] <- "Enero"
caracteristicas$MES[caracteristicas$MES == 2] <- "Febrero"
caracteristicas$MES[caracteristicas$MES == 3] <- "Marzo"
caracteristicas$MES[caracteristicas$MES == 4] <- "Abril"
caracteristicas$MES[caracteristicas$MES == 5] <- "Mayo"
caracteristicas$MES[caracteristicas$MES == 6] <- "Junio"
caracteristicas$MES[caracteristicas$MES == 7] <- "Julio"
caracteristicas$MES[caracteristicas$MES == 8] <- "Agosto"
caracteristicas$MES[caracteristicas$MES == 9] <- "Septiembre"
caracteristicas$MES[caracteristicas$MES == 10] <- "Octubre"
caracteristicas$MES[caracteristicas$MES == 11] <- "Noviembre"
caracteristicas$MES[caracteristicas$MES == 12] <- "Diciembre"

# Exportamos data preprocesada
write.csv(caracteristicas,"caracteristicas_delito3.csv",
          fileEncoding="latin1",row.names = FALSE)


length(table(caracteristicas$NOMBREDD))
# Departamentos ID

# Amazonas: 01
# Ancash: 02
# Apurimac: 03
# Arequipa: 04
# Ayacucho:05
# Cajamarca: 06
# Provincia callao: 07
# Cusco: 08
# Huancavelica: 09
# Huanuco: 10
# Ica: 11
# Junín : 12
# La Libertad: 13
# Lambayeque: 14
# Lima: 15
# Loreto : 16
# Madre de dios: 17
# Moquegua: 18
# Pasco: 19
# Piura: 20
# Puno:21
# San Martín: 22
# Tacna: 23
# Tumbes: 24
# Ucayali: 25

caracteristicas$NOMBREDD<-trimws(caracteristicas$NOMBREDD)
caracteristicas$NOMBREDD <- gsub("Lima Provincias 2/|Provincia de Lima 1/", "Lima", caracteristicas$NOMBREDD)
table(caracteristicas$NOMBREDD)




# Crear la variable CODIGODD basada en la variable NOMBREDD
caracteristicas$CODIGODD <- ifelse(caracteristicas$NOMBREDD == "Amazonas", "01",
                                   ifelse(caracteristicas$NOMBREDD == "Áncash", "02",
                                          ifelse(caracteristicas$NOMBREDD == "Apurímac", "03",
                                                 ifelse(caracteristicas$NOMBREDD == "Arequipa", "04",
                                                        ifelse(caracteristicas$NOMBREDD == "Ayacucho", "05",
                                                               ifelse(caracteristicas$NOMBREDD == "Cajamarca", "06",
                                                                      ifelse(caracteristicas$NOMBREDD == "Provincia Constitucional del Callao", "07",
                                                                             ifelse(caracteristicas$NOMBREDD == "Cusco", "08",
                                                                                    ifelse(caracteristicas$NOMBREDD == "Huancavelica", "09",
                                                                                           ifelse(caracteristicas$NOMBREDD == "Huánuco", "10",
                                                                                                  ifelse(caracteristicas$NOMBREDD == "Ica", "11",
                                                                                                         ifelse(caracteristicas$NOMBREDD == "Junín", "12",
                                                                                                                ifelse(caracteristicas$NOMBREDD == "La Libertad", "13",
                                                                                                                       ifelse(caracteristicas$NOMBREDD == "Lambayeque", "14",
                                                                                                                              ifelse(caracteristicas$NOMBREDD == "Lima", "15",
                                                                                                                                     ifelse(caracteristicas$NOMBREDD == "Loreto", "16",
                                                                                                                                            ifelse(caracteristicas$NOMBREDD == "Madre de Dios", "17",
                                                                                                                                                   ifelse(caracteristicas$NOMBREDD == "Moquegua", "18",
                                                                                                                                                          ifelse(caracteristicas$NOMBREDD == "Pasco", "19",
                                                                                                                                                                 ifelse(caracteristicas$NOMBREDD == "Piura", "20",
                                                                                                                                                                        ifelse(caracteristicas$NOMBREDD == "Puno", "21",
                                                                                                                                                                               ifelse(caracteristicas$NOMBREDD == "San Martín", "22",
                                                                                                                                                                                      ifelse(caracteristicas$NOMBREDD == "Tacna", "23",
                                                                                                                                                                                             ifelse(caracteristicas$NOMBREDD == "Tumbes", "24",
                                                                                                                                                                                                    ifelse(caracteristicas$NOMBREDD == "Ucayali", "25", NA)))))))))))))))))))))))))
write.csv(caracteristicas,"caracteristicas_delito4.csv",
          fileEncoding="latin1",row.names = FALSE)
                                          

