### La función segments()
par(bg='lightyellow',mar=c(3,2,1,1))
f <- function(x){x^6*exp(-2*x)}
L <- 10
curve(f(x),0,L,col=2,lwd=7)
#x <- seq(0,L,0.01)
#plot(x,f(x),type = 'l',col=2,lwd=7)
abline(h=0,v=0)
grid(10,col = 'blue')
curve(f(x),0,L,col=2,lwd=7,add=T)

delta <- 1
w <- seq(0,L,delta)
z <- w + delta
segments(z,0,z,f(z),lwd = 2,col = 1/delta)
for(i in 1:10){
        segments(z-i*delta/10,0,z-i*delta/10,f(z),
                 lwd = 2,col = 1/delta)        
}
segments(w,f(z),z,f(z),lwd = 2,col = 1/delta)

delta <- 1/2
w <- seq(0,L,delta)
z <- w + delta
segments(z,0,z,f(z),lwd = 2,col = 1/delta)
for(i in 1:10){
        segments(z-i*delta/10,0,z-i*delta/10,f(z),
                 lwd = 2,col = 1/delta)        
}
segments(w,f(z),z,f(z),lwd = 2,col = 1/delta)

delta <- 1/3
w <- seq(0,L,delta)
z <- w + delta
segments(z,0,z,f(z),lwd = 2,col = 1/delta)
for(i in 1:10){
        segments(z-i*delta/10,0,z-i*delta/10,f(z),
                 lwd = 2,col = 1/delta)        
}
segments(w,f(z),z,f(z),lwd = 2,col = 1/delta)

delta <- 1/4
w <- seq(0,L,delta)
z <- w + delta
segments(z,0,z,f(z),lwd = 2,col = 1/delta)
for(i in 1:10){
        segments(z-i*delta/10,0,z-i*delta/10,f(z),
                 lwd = 2,col = 1/delta)        
}
segments(w,f(z),z,f(z),lwd = 2,col = 1/delta)

delta <- 1/5
w <- seq(0,L,delta)
z <- w + delta
segments(z,0,z,f(z),lwd = 2,col = 1/delta)
for(i in 1:10){
        segments(z-i*delta/10,0,z-i*delta/10,f(z),
                 lwd = 2,col = 1/delta)        
}
segments(w,f(z),z,f(z),lwd = 2,col = 1/delta)

delta <- 1/6
w <- seq(0,L,delta)
z <- w + delta
segments(z,0,z,f(z),lwd = 2,col = 1/delta)
for(i in 1:10){
        segments(z-i*delta/10,0,z-i*delta/10,f(z),
                 lwd = 2,col = 1/delta)        
}
segments(w,f(z),z,f(z),lwd = 2,col = 1/delta)

delta <- 1/7
w <- seq(0,L,delta)
z <- w + delta
segments(z,0,z,f(z),lwd = 2,col = 1/delta)
for(i in 1:10){
        segments(z-i*delta/10,0,z-i*delta/10,f(z),
                 lwd = 2,col = 1/delta)        
}
segments(w,f(z),z,f(z),lwd = 2,col = 1/delta)

curve(f(x),0,L,col=2,lwd=7,add=T)
#################################################





#################################################
n <- 10 #número de intervalos
m <- 200 #tamaño de cada muestra
confianza <- 0.95
alpha <- 1-confianza
x <- matrix(0,m,n) 
y1 <- 1
y2 <- 1 
z <- 1
media <- 1
########Cálculo intervalo#############
for(i in 1:n){
x[,i] <- rnorm(m)
media[i] <- mean(x[,i])
y1[i] <- media[i]-qnorm(1-alpha/2)/sqrt(m)
y2[i] <- media[i]+qnorm(1-alpha/2)/sqrt(m)}
########################################
#Graficamos los intervalos de confianza
par(bg='gray',mar=c(2,3,4,2))
plot(media,pch=16,ylim=c(min(y1)-0.1,max(y2)+0.1),
     col='green',cex=2)
segments(1:n,y1,1:n,y2,lwd = 2)
abline(h=c(-qnorm(1-alpha/2)/sqrt(m),
           qnorm(1-alpha/2)/sqrt(m)),
                col='magenta',lwd=2)
points(media,pch=16,col='green',cex=2)

#iluminamos de rojo los que quedan fuera y los contamos
afuera <- 0
for(i in 1:n){
        linf <- -qnorm(1-alpha/2)/sqrt(m)
        lsup <- qnorm(1-alpha/2)/sqrt(m)
        if( media[i]<linf || media[i]>lsup ){
                points(i,media[i],pch=16,col="red",cex=2)
                afuera <- afuera + 1
        }
}
########################################
#Presentamos resultados
title(paste("quedaron ", afuera,' de ', n,
            " puntos fuera del intervalo ",
      round(-qnorm(1-alpha/2)/sqrt(m),3),
      round(qnorm(1-alpha/2)/sqrt(m),3)))
##########

