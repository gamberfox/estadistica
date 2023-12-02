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

level_genero <- c("1"=1, femenino=1, Femenino=1, FEMENINO=1, 
                  "2" =2,masculino=2,Masculino=2,MASCULINO=2)
level_zona <- c("1"=1,urbano=1,Urbano=1,URBANO=1,
                "2"=2,rural=2,Rural=2,RURAL=2)
level_grado <- c("6"=6,sexto=6,SEXTO=6,
                 "7"=7,septimo=7,SEPTIMO=7,
                 "8"=8,octavo=8,OCTAVO=8,
                 "9"=9,noveno=9,NOVENO=9,
                 "10"=10,decimo=10,DECIMO=10,
                 "11"=11,once=11,ONCE=11)
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
data$grado <- as.numeric(data$grado)

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
summary(valid_Data)

which(valid_Data)
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
##### 4.1 grafica importante #####
visdat::vis_miss(data)
Summary.NA = miss(data,T)



##### 4.2 Identificación y manejo de datos faltantes(imputar) #####
# Función que evalua e identifica los datos faltantes por variable e individuo.

# Imputación por regresion.copiar a vecinos que comparten la misma
#variable si el par de variables son proporcionales
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
imputR = mice::mice(data, maxit = 1, method = "norm.predict",seed = 2018,print=F)
data_ImputR = mice::complete(imputR)
windows(height=10,width=15); visdat::vis_miss(data_ImputR) 
#Visualizar.AQ(Datos_ImputR)

x11()
boxplot(data_ImputR$HHD); abline(h=0,col="red")

##### 4.2 otra grafica importante ######
Summary.NA = miss(data)

#---------------------------------------------------------------#
#### 5. Sobre el conjunto de variables cuantitativas, realice un diagnóstico de datos atípicos.     ####
#---------------------------------------------------------------#



#---------------------------------------------------------------#
#### 6. Con los resultados de los puntos anteriores, usted dispone del listado con registros inconsistentes y con datos faltantes. Es necesario corregirlo.     ####
#---------------------------------------------------------------#

# Transformación de variables 
#str(data)
#dataImput = transform(data,                                                                          
#                  Month=factor(Month,levels=1:12,labels=month.abb),        # Conversión a formato fecha
#                  Day_of_week =factor(Day_of_week,levels=1:5,labels=c("Lunes","Martes","Miercoles","Jueves","Viernes")))
#str(dataImput)
#summary(dataImput)


# Identificación y visualización de outliers Univariados.
# Iniciaremos con la Variable: HHI
x11()
par(mfrow=c(3,1))
with(data_ImputR,{
  hist(HHI,freq=F,col="blue",breaks=13)
  boxplot(HHI,horizontal=T,col="blue")
  hist(scale(HHI),freq=F,col="blue",breaks=13)
}
)


##Criterio Estandarización
id = which(abs(scale(data_ImputR$HHI))>3)
data_ImputR$HHI[id]

##Criterio Boxplot
boxplot.stats(data_ImputR$HHI)$out
which(data_ImputR$HHI%in%boxplot.stats(data_ImputR$HHI)$out)

qnorm(0.75)+1.5*(qnorm(0.75)-qnorm(0.25))

##Criterio Distancia Cooks

mean(data_ImputR$HHI)

model=lm(HHI~1,data=data_ImputR);CD=cooks.distance(model)
id_HHI=unname(which(CD>4*mean(CD)))


windows()
labels=1:nrow(data_ImputR);labels[-HHI]="."
plot(CD,pch=20);abline(h=4*mean(CD),col="red",ylab="Cooks_Distance")
text(HHI,CD[HHI],HHI, col="red",pos=3,cex=0.8)
#######################################



### Este procedimiento tendriamos que repetirlo para todas 
##las variables, un poco demandante, mejor lo ponemos en una función.
id.out.uni=function(x,method=c("Standarized","Tukey","Cook")){
  id.out=NULL
  if(method=="Standarized"){id.out=which(abs(scale(x))>3)}
  else if(method=="Turkey"){id.out=which(x%in%(boxplot.stats(x)$out))}
  else if(method=="Cook"){model=lm(x~1);CD=cooks.distance(model)
  id.out=unname(which(CD>4*mean(CD)))}
  return(id.out)
}
# Miremos como funciona la función
id.out.uni(data_ImputR$HHI,method="Standarized")
id.out.uni(data_ImputR$HHI,method="Turkey")
id.out.uni(data_ImputR$HHI,method="Cook")
### Ahora vamos a automatizar la inspección de las variables

#Visualizar edad, grado, HHD, HHI, y per.hog
windows()
par(mfrow=c(1,5))
#lapply(data_ImputR[,-(1,3,4)],boxplot,col="Blue")
lapply(data_ImputR[,-c(1,3,4,8,9)],boxplot,col="Blue")

### Identificar los Datos Atipicos
out_Stand = lapply(data_ImputR[,-c(1,3,4,8,9)],id.out.uni,method="Standarized")
out_Tukey = lapply(data_ImputR[,-c(1,3,4,8,9)],id.out.uni,method="Turkey")
out_Cook = lapply(data_ImputR[,-c(1,3,4,8,9)],id.out.uni,method="Cook")



## Visualización de outliers multivariados
################does this need fixing
out.mult=function(Datos){
  n= nrow(Datos); p= ncol(Datos)
  Distance= mahalanobis(Datos,center=colMeans(Datos),cov=cov(Datos))
  Limit= qchisq(0.01, lower.tail=F,df=p)
  id.dist= which(Distance>Limit)
  Score_LOF = DMwR2::lofactor(Datos[,-(1:3)], k=5)
  id.LOF <- order(Score_LOF, decreasing=T)[1:ceiling(0.01*n)]
  
  windows()
  par(mfrow=c(2,1))
  plot(Distance,pch=20,ylim=c(0,max(Distance)*1.2))
  text(id.dist,Distance[id.dist],id.dist, col="red",pos=3,cex=0.8)
  abline(h=Limit,col="red",lwd=2,lty=2)
  plot(Score_LOF,pch=20,ylim=c(0,max(Score_LOF)*1.2))
  text(id.LOF,Score_LOF[id.LOF],id.LOF, col="red",pos=3,cex=0.8)
  return(list(Out_dist=id.dist,Out_LOF=id.LOF))
}

#ignoramos las columnas con datos cualitativos
id_Out_mult=out.mult(data_ImputR[,-c(1,3,4,8,9)])
id_Out_mult=out.mult(data_ImputR[, sapply(data_ImputR, is.numeric)])

### Ahora vamos a automatizar la inspección de las variables

#Visualizar
windows()
par(mfrow=c(1,5))
lapply(data_ImputR[,-c(1,3,4,8,9)],boxplot,col="Blue")


### Identificar los Datos Atipicos
out_Stand = lapply(data_ImputR[,-c(1,3,4,8,9)],id.out.uni,method="Standarized")
out_Tukey = lapply(data_ImputR[,-c(1,3,4,8,9)],id.out.uni,method="Tukey")
out_Cook = lapply(data_ImputR[,-c(1,3,4,8,9)],id.out.uni,method="Cook")

####### Identificación  multivariada de outliers
Ozone.cor = cor(data_ImputR[,-c(1,3,4,8,9)],method="pearson")
windows(height=10,width=15)
x11()
corrplot::corrplot(Ozone.cor , method = "ellipse",addCoef.col = "black",type="upper")

windows(height=10,width=15)
pairs(data_ImputR[,-c(1,3,4,8,9)],lower.panel = panel.smooth, pch = 15)


## Visualización de outliers multivariados
out.mult=function(Datos){
  n= nrow(Datos); p= ncol(Datos)
  Distance= mahalanobis(Datos,center=colMeans(Datos),cov=cov(Datos))
  Limit= qchisq(0.01, lower.tail=F,df=p)
  id.dist= which(Distance>Limit)
  Score_LOF = DMwR2::lofactor(Datos[,-c(1,3,4,8,9)], k=5)
  id.LOF <- order(Score_LOF, decreasing=T)[1:ceiling(0.01*n)]
  
  windows()
  par(mfrow=c(2,1))
  plot(Distance,pch=20,ylim=c(0,max(Distance)*1.2))
  text(id.dist,Distance[id.dist],id.dist, col="red",pos=3,cex=0.8)
  abline(h=Limit,col="red",lwd=2,lty=2)
  plot(Score_LOF,pch=20,ylim=c(0,max(Score_LOF)*1.2))
  text(id.LOF,Score_LOF[id.LOF],id.LOF, col="red",pos=3,cex=0.8)
  return(list(Out_dist=id.dist,Out_LOF=id.LOF))
}

id_Out_mult=out.mult(data_ImputR[,-c(1,3,4,8,9)])
#---------------------------------------------------------------#
#### 7. Genere un resumen de los cambios realizados en la hoja de datos. ReporteCambios.txt     ####
#---------------------------------------------------------------#

