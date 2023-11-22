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

level_genero <- c("1"="1", femenino="1", Femenino="1", FEMENINO="1", 
                  "2" ="2",masculino="2",Masculino="2",MASCULINO="2")
level_zona <- c("1"="1",urbano="1",Urbano="1",URBANO="1",
                "2"="2",rural="2",Rural="2",RURAL="2")
level_grado <- c("6"="6",sexto="6",SEXTO="6",
                 "7"="7",septimo="7",SEPTIMO="7",
                 "8"="8",octavo="8",OCTAVO="8",
                 "9"="9",noveno="9",NOVENO="9",
                 "10"="10",decimo="10",DECIMO="10",
                 "11"="11",once="11",ONCE="11")
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

visdat::vis_miss(data)

# Verificación de las reglas sobres los datos
Rules <- editrules::editfile("consistencia.txt")
editrules::violatedEdits(Rules, data)

valid_Data = editrules::violatedEdits(Rules, data)
summary(Valid_Data)

which(Valid_Data)
#---------------------------------------------------------------#
#### 3. aplicar las reglas de "consistencia.txt" sobre las hojas de datos     ####
#---------------------------------------------------------------#

# Carga del archivo de reglas de validación
Rules <- editrules::editfile("consistencia.txt")

# Conexión entre las  reglas
windows()
plot(Rules)

# Verificación de las reglas sobres los datos
editrules::violatedEdits(Rules, data)
Valid_Data = editrules::violatedEdits(Rules, data)
summary(Valid_Data)

#Identificar que observaciones presentan violaciones a las reglas
which(Valid_Data)
matrix(data=1:55, 5, 11)


# Visualización del diagnóstico
windows()
plot(Valid_Data)



#---------------------------------------------------------------#
#### 4. visualizar y representar los registros que presentan datos faltantes     ####
#---------------------------------------------------------------#

mean(data$HDD)#esto fallara
sum(is.na(data$HHD), na.rm=TRUE)#el numero de datos faltantes de HHD
sum(is.na(data$HHI), na.rm=TRUE)#el numero de datos faltantes de HHI
sum(is.na(data$per.hog), na.rm=TRUE)#el numero de datos faltantes de per.hog
summary(data)##otra forma de revisar los datos NA
View(data)
is.na(data)                                    # para cada elemento de Datos verifica si es NA
x11()
##### 4.1 grafica importante #####
visdat::vis_miss(data)


# Función que evalua e identifica los datos faltantes por variable e individuo.

miss<-function(Datos,plot=T){  
  n=nrow(Datos);p=ncol(Datos)
  names.obs<-rownames(Datos)
  
  
  nobs.comp=sum(complete.cases(Datos))         # Cuenta los registros completos
  Obs.comp=which(complete.cases(Datos))        # Identifica los registros completos
  nobs.miss = sum(!complete.cases(Datos))      # Identifica los registros con datos faltantes.
  Obs.miss=which(!complete.cases(Datos))       # Identifica los registros con datos faltantes.
  
  Datos.NA<-is.na(Datos)
  Var_Num<- sort(colSums(Datos.NA),decreasing=T)
  Var_per<-round(Var_Num/n,3)
  Obs_Num<-rowSums(Datos.NA)
  names(Obs_Num)<-names.obs
  Obs_Num<-sort(Obs_Num,decreasing=T)
  Obs_per<-round(Obs_Num/p,3)
  lista<-list(n.row = n, n.col = p,n.comp = nobs.comp,Obs.comp = Obs.comp,n.miss = nobs.miss,Obs.miss = Obs.miss, Var.n = Var_Num , Var.p = Var_per, Obs.n= Obs_Num, Obs.per= Obs_per)
  
  if(plot){
    windows(height=10,width=15)
    par(mfrow=c(1,2))
    coord<-barplot(Var_per,plot=F)
    barplot(Var_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "% datos faltantes por variable")
    axis(2,at=coord,labels=names(Var_per), cex.axis=0.5,pos=0,las=2)
    axis(1,seq(0,1,0.2),seq(0,1,0.2),pos=0)
    
    coord<-barplot(Obs_per,plot=F)
    barplot(Obs_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "% datos faltantes por registro")
    axis(2,at=coord,labels=names(Obs_per),cex.axis=0.5,pos=0,las=2)
    axis(1,seq(0,1,0.2),seq(0,1,0.2))
  }
  return(invisible(lista))
}

##### 4.2 otra grafica importante ######
Summary.NA = miss(data)

#---------------------------------------------------------------#
#### 5. Sobre el conjunto de variables cuantitativas, realice un diagnóstico de datos atípicos.     ####
#---------------------------------------------------------------#

#---------------------------------------------------------------#
#### 6. Con los resultados de los puntos anteriores, usted dispone del listado con registros inconsistentes y con datos faltantes. Es necesario corregirlo.     ####
#---------------------------------------------------------------#

#imputar, quitar datos atipicos del original, imputar original, 


#---------------------------------------------------------------#
#### 7. Genere un resumen de los cambios realizados en la hoja de datos. ReporteCambios.txt     ####
#---------------------------------------------------------------#

