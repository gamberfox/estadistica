#------------------------------------------------------#
#-- Universidad del Valle: Escuela de Estadistica    --#
#-- Taller1                                          --# 
#-- Asignatura: Probabilidad y Estadística           --#
#-- Profesores: Ivan Mauricio Bermudez               --#
#------------------------------------------------------#
#               ESTUDIANTES:CODIGO
#-- Javier Humberto Grijalba Camayo: 1743693         --# 
#-- :              --# 
#------------------------------------------------------#
#y es la variable respuesta, y x es la variable explicativa
#relaciones lineales,
#### 0. Establecer el directorio de trabajo ####


#### 1. Lectura de datos ####

datos <- read.table("data_embutidos.txt", header=TRUE, dec=".")

names(datos) #nombres de las variables en la BD.
str(datos)  #Indica el tipo de variable
head(datos) #Muestra las primeras lineas de la BD.

#### 2. Crear histograma ####

hist(datos$peso,
     main=paste("histograma del peso"),
     xlab="peso(gr)",
     ylab="frecuencia",
     xlim=c(208,228),
     col="#7FFF00")
#esto sobrepone lineas
#esta funcion siempre va despues del grafico
abline(v=c(212,220,228),lty=c(3,3,3),lwd=c(3,3,3),col="red")


#### 2.b. Crear Boxplot ####
#boxplot(datos$peso) #generico
#no hay medidad de frecuencia asi que omitimos xlab
boxplot(datos$peso,
        main="histograma del peso",
        ylab="peso(gr)",
        ylim=c(208,228),
        col="#7FFF00")
abline(h=c(212,220,228),lty=2,lwd=2,col="red")
# Points
stripchart(datos$peso,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)        # Add it over

points(datos$peso, pch = 16, col = "blue", cex = 0.7)


#estaba tratando de agregar los puntos al box plot
#boxplot(c(1, 2, 3, 4, 5,5,5))
#x <- c(1, 2, 3)
#y <- c(3, 3, 1)
#points(x,y, pch = 16, col = "red")



#### 3. tratando de encontrar porque esta fallando esto ####
#trataremos las 4 combinaciones de de maquina con operador.
#operador 1 y maquina 1
#operador 1 y mauqina 2
#operador 2 y mauqina 1
#operador 2 y mauqina 2

##### 3.a. revisando si alguno de los operadores es responsable #####
#datosOperadorA<-which(datos$operario == 'A')
#datosOperadorB<-which(datos$operario == 'B')

datosOperadorA<-datos[datos$operario == 'A',]
datosOperadorB<-datos[datos$operario == 'B',]

boxplot(datosOperadorA$peso,datosOperadorB$peso,
        names=c("Operador A","Operador B"),
        main="histograma del peso",
        ylab="peso(gr)",
        ylim=c(208,228),
        col="#7FFF00")
abline(h=c(212,220,228),lty=2,lwd=2,col="red")



##### 3.b. revisando si alguno de las maquinas es responsable #####
datosMaquina1<-datos[datos$maquina == 1,]
datosMaquina2<-datos[datos$maquina == 2,]
par(mfrow = c(1, 2))
boxplot(datosMaquina1$peso,
        names=c("Maquina 1"),
        main="histograma del peso",
        ylab="peso(gr)",
        ylim=c(208,228),
        col="#7FFF00")
abline(h=c(212,220,228),lty=2,lwd=2,col="red")
# Points
stripchart(c(datosMaquina1$peso,datosMaquina2$peso),              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 3:4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)        # Add it over

boxplot(datosMaquina2$peso,
        names=c("Maquina 2"),
        main="histograma del peso",
        ylab="peso(gr)",
        ylim=c(208,228),
        col="#7FFF00")
abline(h=c(212,220,228),lty=2,lwd=2,col="red")
stripchart(datosMaquina2$peso,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           
           add = TRUE)

#basado en este boxplot voy a asumir que la maquina 1 es responsable
#por las dificultades


datosA1 <- datos[(datos$operario == 'A')& (datos$maquina == 1),]
datosA2 <- datos[(datos$operario == 'A')& (datos$maquina == 2),]
datosB1 <- datos[(datos$operario == 'B')& (datos$maquina == 1),]
datosB2 <- datos[(datos$operario == 'B')& (datos$maquina == 2),]
boxplot(datosA1$peso,datosA2$peso,datosB1$peso,datosB2$peso,
        names=c("A1","A2","B1","B2"),
        main="histograma del peso",
        ylab="peso(gr)",
        ylim=c(208,228),
        col="#7FFF00")
abline(h=c(212,220,228),lty=2,lwd=2,col="red")
#habiendo hecho las 4 combinaciones de maquinas y operadores, concluyo que
#los resultados permanecen mas centrados cuando usamos la maquina 2

