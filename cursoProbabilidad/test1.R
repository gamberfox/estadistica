x11()
par(mfrow=c(2,1))
hist(datos$peso,
     main=paste("histograma del peso"),
     xlab="peso(gr)",
     ylab="frecuencia",
     xlim=c(26,32),
     col="#7FFF00")
abline(v=c(28.5,30,31.5),lty=c(1,2,3),lwd=c(1,2,3),col=c("red","blue","yellow"))

boxplot(datos$peso,
        main="histograma del peso",
        ylab="peso(gr)",
        ylim=c(26,32),
        col="#7FFF00")
abline(h=c(28.5,30,31.5),lty=2,lwd=2,col="red")