#------------------------------------------------------#
#-- Universidad del Valle: Escuela de Estadistica    --#
#-- Proyecto: Preprocesamiento de datos con R                                        --# 
#-- Asignatura: Probabilidad y Estadística           --#
#-- Profesores: Ivan Mauricio Bermudez               --#
#------------------------------------------------------#
#               ESTUDIANTES:CODIGO
#-- Javier Humberto Grijalba Camayo: 1743693         --# 
#-- :              --# 
#------------------------------------------------------#

library("easypackages")
lib_req<-c("lubridate","dplyr","visdat","missMDA","mice","DMwR2","editrules", "corrplot","readr","here")# Listado de librerias requeridas por el script
easypackages::packages(lib_req)         # Verificación, instalación y carga de librerias.

#---------------------------------------------------------------#
#### 1. leyendo la hoja de datos "BD_huella.txt"     ####
#---------------------------------------------------------------#
data.completo <- read.table(here("datos","BD_huella.txt"), header=T,sep="\t",stringsAsFactors = F, encoding ="UTF-8", na.strings="", quote="", fill=FALSE)
summary(data.completo)
str(data.completo)
head(data.completo)
names(data.completo)
data.completo$ID
#convertiremos todos los string de la lista a minusculas no tener que crear
#tantos casos

#data.completo <- as.data.frame(lapply(data.completo, function(x) if (is.character(x)) tolower(x) else x))

data <- dplyr::select(data.completo, everything())

#le daremos nombre a las columnas
colnames(data) <-c("id","edad","genero","zona","grado",
                   "HHD","HHI","comp_HHD","comp_HHI","per.hog")

table(data.completo$id)
table(data.completo$edad)
table(data.completo$genero)
table(data.completo$zona)
table(data.completo$grado)
table(data.completo$HHD)
table(data.completo$HHI)
table(data.completo$comp_HHD)
table(data.completo$comp_HHI)
table(data.completo$per.hog)


# Declaración de niveles correctos para las variables tipo Factor

level_genero <- c("1"="Femenino", femenino="Femenino", Femenino="Femenino", FEMENINO="Femenino", 
                  "2" ="Masculino",masculino="Masculino",Masculino="Masculino",MASCULINO="Masculino")
level_zona <- c("1"="Urbano",urbano="Urbano",Urbano="Urbano",URBANO="Urbano",
                "2"="Rural",rural="Rural",Rural="Rural",RURAL="Rural")
level_grado <- c("6"="Sexto",sexto="Sexto",SEXTO="Sexto",
                 "7"="Septimo",septimo="Septimo",SEPTIMO="Septimo",
                 "8"="Octavo",octavo="Octavo",OCTAVO="Octavo",
                 "9"="Noveno",noveno="Noveno",NOVENO="Noveno",
                 "10"="Decimo",decimo="Decimo",DECIMO="Decimo",
                 "11"="Once",once="Once",ONCE="Once")
level_comp_HHD <- c(Lavado.ropa="Lavado Ropa",
                    Riego.jardin="Riego Jardin",
                    Uso.baño="Uso baño",Uso_baño="Uso baño",USO.BAÑO="Uso baño",
                    Uso.cocina="Uso cocina")
level_comp_HHI <- c(Café="Café",CAFÉ="Café",
                    Carne="Carne",CARNE="Carne",
                    Fruta="Fruta")

## Modificación del formato y transformación de variables
data <- transform(data,
                    genero=factor(dplyr::recode(genero, !!!level_genero)),
                    zona=factor(dplyr::recode(zona, !!!level_zona)),
                    grado=factor(dplyr::recode(grado, !!!level_grado)),
                    comp_HHD=factor(dplyr::recode(comp_HHD, !!!level_comp_HHD)),
                    comp_HHI=factor(dplyr::recode(comp_HHI, !!!level_comp_HHI))
)

table(data$id)
table(data$edad)
table(data$genero)
table(data$zona)
table(data$grado)
table(data$HHD)
table(data$HHI)
table(data$comp_HHD)
table(data$comp_HHI)
table(data$per.hog)

#---------------------------------------------------------------#
#### 2. construyendo la hoja "consistencia.txt"     ####
#---------------------------------------------------------------#

#---------------------------------------------------------------#
#### 3. aplicar las reglas de "consistencia.txt" sobre las hojas de datos     ####
#---------------------------------------------------------------#

#---------------------------------------------------------------#
#### 4. visualizar y representar los registros que presentan datos faltantes     ####
#---------------------------------------------------------------#

#---------------------------------------------------------------#
#### 5. construyendo la hoja "consistencia.txt"     ####
#---------------------------------------------------------------#

#---------------------------------------------------------------#
#### 6. construyendo la hoja "consistencia.txt"     ####
#---------------------------------------------------------------#

#---------------------------------------------------------------#
#### 7. construyendo la hoja "consistencia.txt"     ####
#---------------------------------------------------------------#

