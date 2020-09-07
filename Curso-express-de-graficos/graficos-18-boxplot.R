# Función boxplot()
######################
par(mfrow=c(1,1),bg='gray',mar=c(2,2,2,2))
a <- c(24,34,21,14,27,35,32,54,56,43,89,76,90,3,45,32)
boxplot(a,col="green",
        main="Diagrama de Caja",lwd=2)
boxplot(a,col="green",
        main="Diagrama de Caja",lwd=2,horizontal =T)
boxplot(a,col="green",
        main="Diagrama de Caja",lwd=2)
n <- length(a)
n
b <- sort(a)
mediana.a.mano <- (b[n/2]+b[n/2+1])/2
mediana.a.mano
median(a)
abline(h=mediana.a.mano,lty=2,col="blue",lwd=2)
Q1.a.mano <- (b[n/4]+b[n/4+1])/2 
Q1.a.mano
quantile(a,0.25,type = 2)
abline(h=Q1.a.mano,lty=2,col="red",lwd=2)
Q3.a.mano <- (b[3*n/4]+b[3*n/4+1])/2
Q3.a.mano
quantile(a,0.75,type = 2)
abline(h=Q3.a.mano,lty=2,col="red",lwd=2)
ri <- Q3.a.mano-Q1.a.mano
lim.sup <- Q3.a.mano+1.5*ri
lim.inf <- Q1.a.mano-1.5*ri
a<lim.inf
a>lim.sup
minimo <- min(a)   #sort(a)[1]
maximo <- max(a)   #sort(a)[n]
abline(h=c(minimo,maximo),lty=2,col="magenta",lwd=2)
text(1,50,"no hay outliers!")
text(1,40,"datos asimétricos!")
boxplot.stats(a)
###########################################################
datos <- c(85.3, 87.5, 87.8, 89.9, 90.4,91.8, 92.7, 86.7, 
           87.8, 88.2, 88.6, 90.3, 91, 91.8, 92.3, 93.3, 
           89.9, 90.1, 90.1, 90.8, 90.9,91.1, 92.7, 93.4, 
           91.2, 91.5, 92.6, 92.7, 93.3, 94.2, 94.7, 94.2, 
           95.6,  96.1)
##BOXPLOT
boxplot(datos,col="green",main="Diagrama de Caja",lwd=2)
#
boxplot.stats(datos)
#
n <- length(datos)
n/2
s <- sort(datos)
s
mediana <- (s[17]+s[18])/2
mediana
quantile(datos,0.5)
abline(h=mediana,col='red',lwd=2)
n/4
q1 <- s[9]
abline(h=q1,col='blue',lwd=2)
3*n/4
q3 <- s[26]        
abline(h=q3,col='blue',lwd=2)
RI <- q3-q1
lim.sup <- q3+1.5*RI
lim.inf <- q1-1.5*RI
datos<lim.inf
datos[1]
abline(h=s[2],col='magenta',lwd=2)
datos>lim.sup
abline(h=s[n],col='magenta',lwd=2)
###################################################
boxplot(rlnorm(50,0,1),col = 7,pch=16) #lognormal
###################################################
set.seed(321)  #semilla para reproducibilidad
m <- rnorm(50,0,1)
boxplot(m,col = 7,pch=16)
q25 <- quantile(m,0.25,type = 2)
q50 <- quantile(m,0.5,type = 2) #median(m)
q75 <- quantile(m,0.75,type = 2)
RI <- q75 - q25 
Lsup <- q75 + 1.5*RI
Linf <- q25 - 1.5*RI
s <- sort(m)
which(s<Linf)
Bigote_abajo <- s[3]
which(s>Lsup)
Bigote_arriba <- s[49]
e <- c(s[1],s[2],Bigote_abajo,q25,q50,q75,Bigote_arriba,s[50])
abline(h = e, lty = 2, col = 2)
#############################################
par(mfrow=c(1,1),bg='black',mar=c(3,2,0,2))
M <- data.frame(rgamma(100,20,3),
                rgamma(100,21,3))
boxplot(M,col = 2:3,
        pch=16,border = 'white',lwd=3,horizontal = F)
M <- data.frame(rgamma(100,20,3),
                rgamma(100,21,3),
                rgamma(100,18,3))
boxplot(M,col = 2:4,
        pch=16,border = 'white',lwd=3,horizontal = F)
boxplot(M,col = 2:4,pch=16,border = 'white',lwd=3,plot = F)
#
#

par(mfrow=c(1,1),bg='black',mar=c(3,1,0,0))
M <- data.frame(rgamma(100,20,3),
                rgamma(100,21,3),
                rgamma(100,18,3),
                rgamma(100,19,3))
boxplot(M,col = 2:5,cex=2.5,pch=16,
        border = '6',lwd=4,horizontal = T)
points(runif(200,min(M),max(M)),runif(200,-1,5),
       pch='*',cex=0.8,col='gray')
points(runif(200,min(M),max(M)),runif(200,0,5),
       pch='*',cex=1,col='white')
points(runif(200,min(M),max(M)),runif(200,0,5),
       pch='*',cex=1.2,col='gray')
boxplot(M,col = 2:5,cex=2.5,pch=16,
        border = '6',lwd=4,horizontal = T,add=T)
axis(1,min(M):max(M),col = 7,lwd=3,col.axis=7)
