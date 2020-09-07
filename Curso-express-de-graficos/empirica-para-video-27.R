tlc2 <- function(n){
    z <- numeric(n)
    for(i in 1:n){
        numerador <- mean(rbinom(n,1,0.4))-0.4
        denominador <- sqrt(0.4*0.6/n)
        z[i] <- numerador/denominador
    }
    plot(ecdf(z),lwd=5,xlim=c(-3,3),col='blue',
         main="Distribución empírica de una muestra de
    (Xbarra-media)/sqrt(Var(X)/n) donde X1,...,Xn es
    una m.a. Bernoulli con tño n, contra N(0,1)")
    curve(pnorm(x),add=T,col='red',lwd=5)
    abline(h=0,v=0)
}
#EJEMPLO:
tlc2(10)
tlc2(30)
tlc2(100)
tlc2(500)
tlc2(1000)
