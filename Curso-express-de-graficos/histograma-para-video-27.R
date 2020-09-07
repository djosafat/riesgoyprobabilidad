###########################################
#Este programa genera el histograma 
#que muestre el comportamiento de una 
#m.a. de tamaño n formada por la v.a. 
#      (x_barra-mu)/raíz(var/n).
#Las X_1,...X_n las tomamos Poisson 
#(podría ser cualquier distribución)
#Como lo dice el TLC, conforme
#más grande sea n, debemos obtener 
#histogramas cada vez más normalizados.
#Usamos una semilla para controlar la m.a.
###########################################
tcl <- function(n,lambda,m=1000){
#n <- 100; lambda <- 1; m=1000
    media <- lambda
    varianza <- lambda
    z <- numeric(m)
    for(i in 1:m) {
        muestra <- rpois(n,lambda)
        z[i] <- (mean(muestra)-media)/sqrt(varianza/n)
    }
    par(bg='black',mar <- c(2,3,4,3))
    aux <- 1
    alturas <- hist(z)$density
    L <- max(0.45,alturas) #1/sqrt(2*pi) \approx 0.4
    elcolor <- rainbow(length(alturas),alpha = 0.6)
    hist(z,freq = F ,col=elcolor,xlab = '',ylab = '',
         ylim=c(0,L),main='')
    arrows(-4,0,4,0,lwd=2,col='white')
    arrows(0,-0.1,0,L,lwd=2,col='white')
    grid(10,col = 'green')
    title(paste("Hist de nueva v.a. que aproxima una 
        N(0,1) con n =", n),col.main=7,cex.main=2)
    points(runif(200,-4,4),runif(200,0,0.45),cex=1.5,
            pch=c(letters,'+','-','%','*'),col='gray')
    hist(z,freq = F ,col=elcolor,add=T)
    curve(dnorm(x),lwd=8,col=2,add = T)
    axis(1,-4:4,col = 7,lwd=3,col.axis=7)
    par(bg='white')
}
#EJEMPLO:
tcl(1,1)
#
tcl(5,1)
#
tcl(10,1)
#
tcl(30,1)
#
tcl(100,1)
#
tcl(1000,1)
#
tcl(10000,1)
#
tcl(100000,1)

