#----------------------------------------------------------#
# PREPROCESAMIENTO DE DATOS PARA LOS DATOS DE LAS VICTIMAS #
#----------------------------------------------------------#

# Librerías a utilizar 
library(foreign)     # Lectura de SPSS 
library(DMwR2)       # Imputación de variables 
library(funModeling) # Ver cantidad de valores perdidos

# Leyendo datos
victimas<-read.spss("Capítulo_300_Denuncia_de_Delitos_2017.sav",
                    use.value.labels=FALSE, 
                    to.data.frame=TRUE,reencode="latin1")

# Viendo valores perdidos
df_status(victimas, print_results = F)

# Se elimina la variable IVH308, IVH308_0 (depende de IVH308_0), IVH309, IVH310,
# IVH310_0(depende de IVH310),
victimas$IVH308<-NULL
victimas$IVH308_O<-NULL
victimas$IVH310_O<-NULL
victimas$IVH310<-NULL

# Luego eliminamos las variables que no nos interesan en nuestro estudio
victimas$VIC_DOLOSA<-NULL   

# IVH302 (tipo documento)
table(victimas$IVH302)/length(victimas$IVH302)*100
victimas$IVH302<-NULL     #Tipo de documento (92.85% son DNI)

# ID_PERSONA
victimas$ID_PERSONA<-NULL #ID_Persona (nosotros le crearemos su propio ID)

# LA FUENTE (SIDPOL o Registro) 
# -> No es de interés en el estudio ya que analizaremos todas las denuncias
victimas$FUENTE<-NULL

# OCUPACIÓN
# Donde 8 -> No precisó
table(victimas$IVH307)/length(victimas$IVH307)*100
victimas$IVH307<-NULL     #Ocupación de la victima (89.6% no precisaron este dato)

# ESTADO CIVIL
# Donde 6 -> No precisó
table(victimas$IVH309)/length(victimas$IVH309)*100  
victimas$IVH309<-NULL     #Estado civil de la victima (70% no precisaron este dato)


# Renombrando
victimas <-rename(victimas,IVH305=EDAD)
victimas <-rename(victimas,IVH303=SEXO)

# Recodificando edad 
# 99 --> No especificó --> NA
victimas[,3] = replace(victimas[,c(3)],
                       victimas[,c(3)]==99,NA)

# Recodificando Sexo : No precisa -> NA 
victimas[,2] = replace(victimas[,c(2)],
                       victimas[,c(2)]=="No precisa     ",NA)

# Eliminando IDS duplicados
victimas <- subset(victimas, !duplicated(victimas$ID_DENUNCIA))

# Viendo Valores perdidos luego de haber realizado las recodificaciones
df_status(victimas, print_results = F) 

# Imputando variables:
# Las variables Sexo y Edad tienen valores perdidos pero menor al 5%, por lo que
# realizaremos imputación.

# Imputación por muestreo aleatorio (v.categórica)
# --> Sexo:
sexo_completo <- victimas$SEXO[!is.na(victimas$SEXO)]
victimas$SEXO[is.na(victimas$SEXO)] <- sample(sexo_completo, sum(is.na(victimas$SEXO)), 
                                              replace = TRUE)

# Imputando por la media (v.numérica)
victimas<-centralImputation(victimas)

# Por último exportamos la data preprocesada
write.csv(victimas,"victimas_delito.csv", fileEncoding="latin1",row.names = F)





