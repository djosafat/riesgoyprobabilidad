tlc3 <- function(n){
    z <- numeric(100)
    for(i in 1:100){
        numerador <- mean(runif(n))-0.5
        denominador <- 1/sqrt(12*n)
        z[i] <- numerador/denominador
    }
    par(bg='gray')
    qqnorm(z,pch=16,col=heat.colors(100),cex=1.5)
    qqline(z,col='blue',lty=3,lwd=4)
    abline(h=0,v=0)
    par(bg='white')
}
#EJEMPLO:
tlc3(1)
tlc3(5)
tlc3(10)
tlc3(30)
tlc3(100)
tlc3(500)
tlc3(1000)
tlc3(10000)
