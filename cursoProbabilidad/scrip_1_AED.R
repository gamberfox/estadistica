#------------------------------------------------------#
#-- Universidad del Valle: Escuela de Estadistica    --#
#-- Asignatura: Probabilidad y Estadística           --#
#-- Profesores: Ivan Mauricio Bermudez               --#   
#------------------------------------------------------#
#y es la variable respuesta, y x es la variable explicativa
#relaciones lineales,
#### 0. Establecer el directorio de trabajo ####


#### 1. Lectura de datos ####

datos <- read.table("data_chocolates.txt", header=TRUE, dec=".")

names(datos) #nombres de las variables en la BD.
str(datos)  #Indica el tipo de variable
head(datos) #Muestra las primeras lineas de la BD.


#### 2.  Visualizacion de datos ####

#### 2.1. Diagnostico del peso de las barras de chocolate ####

# a. Crear histograma

hist(datos$peso,
     main=paste("histograma del peso"),
     xlab="peso(gr)",
     ylab="frecuencia",
     xlim=c(26,32),
     col="#7FFF00")

#esto sobrepone lineas
#esta funcion siempre va despues del grafico
abline(v=c(28.5,30,31.5),lty=c(1,2,3),lwd=c(1,2,3),col=c("red","blue","yellow"))

# b. Crear Boxplot
#boxplot(datos$peso) #generico
#no hay medidad de frecuencia asi que omitimos xlab
boxplot(datos$peso,
     main="histograma del peso",
     ylab="peso(gr)",
     ylim=c(26,32),
     col="#7FFF00")
abline(h=c(28.5,30,31.5),lty=2,lwd=2,col="red")

#particionar ventana
par(mfrow=c(1,2))

#aqui habra problemas con el largo de la ventana
par(mfrow=c(2,1))

#ventana flotante
x11()
par(mfrow=c(2,1))

# c. ¿Que porcentaje de los pesos estan por fuera de los limites de especificación ?

sum(datos$peso < 28.5)

length(which(datos$peso < 28.5))

sum(datos$peso < 28.5)/length(datos$peso)

PMenores.LEI=sum(datos$peso < 28.5)/length(datos$peso)

PMayores.LEI=sum(datos$peso > 31.5)/length(datos$peso)

# d. Indicadores descriptivos

media<-mean(datos$peso)
mediana<-median(datos$peso)
desviacion<-sd(datos$peso)
cv<-desviacion/mediana
resumen<-summary(datos$peso)
resumen[1]
resumen[c(1,3)]

percenti.95<-quantile(datos$peso,0.95)
quantile(datos$peso,c(0.25,0.50,0.75))
round(quantile(datos$peso,c(0.25,0.50,0.75)),2)




#### 2.2. Intervencion #### 

# ¿Existe relacion entre el peso y la densidad de los chocolates?

x11()
#'<-' fails inside functions when we use '='
plot(datos$densidad,datos$peso,
     pch=20,
     xlab="Densidad",
     ylab="Peso",
     ylim=c(26,32),
     main="Relacion entre peso y densidad")

cor(datos$peso,datos$densidad)
abline(h=c(28.5,30,31.5),lty=2,lwd=2,col="red")




#### 2.3. Validación de la intervencion #### 

datos$peso_pos
hist(datos$peso_pos,
     main=paste("histograma del peso_pos"),
     xlab="peso_pos(gr)",
     ylab="frecuencia",
     xlim=c(26,32),
     col="#7FFF00")
#esto sobrepone lineas
#esta funcion siempre va despues del grafico
abline(v=c(28.5,30,31.5),lty=c(1,2,3),lwd=c(1,2,3),col=c("red","blue","yellow"))

#boxplot(datos$peso) #generico
#no hay medidad de frecuencia asi que omitimos xlab
boxplot(datos$peso_pos,
        main="histograma del peso_pos",
        ylab="peso_pos(gr)",
        ylim=c(26,32),
        col="#7FFF00")
abline(h=c(28.5,30,31.5),lty=2,lwd=2,col="red")


# Crear ventana flotante
x11() 
# Particionar ventana
par(mfrow = c(1, 1))

#comparar peso incial vs final
#boxplots son los mas utiles para comparar
boxplot(datos$peso,datos$peso_pos,
        main="histograma del peso_pos",
        ylab="peso_pos(gr)",
        ylim=c(26,32),
        col="#7FFF00",
        names=c("antes","despues"))
abline(h=c(28.5,30,31.5),lty=2,lwd=2,col="red")

#table1, libreria table1
#library(table1)

#informe de maximo 3 paginas
#analisis general del peso
#peso vs operario
#3.peso vs maquina
#4. peso vs operario-maquina









