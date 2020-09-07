MB <- function(m,n,t){
        #m <- 5; n <- 20; t <- 1
        pasos <- 2*rbinom(n*m,1,0.5)-1
        r <- matrix(pasos, nrow=n, ncol=m)
        delta <- t/n
        x <- (0:n)*delta ##Valores del eje tiempo.
        B <- matrix(rep(0,(n+1)*m), nrow=n+1, ncol=m)
        for(k in 1:m){
                B[1,k] <- 0
                for(j in 1:n){
                        aux <- r[j,k]*sqrt(delta)
                        B[j+1,k] <- B[j,k]+aux
                }
        }
        par(bg='lightyellow',mar=c(2,4,2,1)) 
        matplot(x,B, type="l",lwd=2,lty = 1,
                main='Movimiento Browniano Estándar',
                ylab='Espacio de estados',xlab='')
        grid(5)
        abline(v=0,h=0)
}
##########################
#Ejemplo##################
##########################
par(mfrow=c(3,1),oma=c(1,0,1,0))
MB(20,10,5)
MB(20,100,5)
MB(20,1000,5)
par(mfrow=c(1,1))
MB(50,1000,5)
MB(100,1000,5)

set.seed(321)
MB(100,1000,6)
#################################
x <- seq(-6,6,0.01)
y <- dnorm(x)
lines(1-y,x,lwd=10,col='orange')
lim_sup <- qnorm(0.975)
lim_sup
abline(h=c(lim_sup,-lim_sup),lwd=2)
#################################
y <- dnorm(x,mean=0,sd=sqrt(1/2))
lines(1/2-y,x,lwd=10,col=2)
lim_sup <- qnorm(0.975,0,sqrt(1/2))
lim_sup
abline(h=c(lim_sup,-lim_sup),col=2,lwd=2)
#################################
y <- dnorm(x,mean=0,sd=sqrt(2))
lines(2-y,x,lwd=10,col=3)
lim_sup <- qnorm(0.975,0,sqrt(2))
lim_sup
abline(h=c(lim_sup,-lim_sup),col=3,lwd=2)
#################################
y <- dnorm(x,mean=0,sd=sqrt(3))
lines(3-y,x,lwd=10,col=4)
lim_sup <- qnorm(0.975,0,sqrt(3))
lim_sup
abline(h=c(lim_sup,-lim_sup),col=4,lwd=2)
#################################
y <- dnorm(x,mean=0,sd=sqrt(4))
lines(4-y,x,lwd=10,col=5)
lim_sup <- qnorm(0.975,0,sqrt(4))
lim_sup
abline(h=c(lim_sup,-lim_sup),col=5,lwd=2)
#################################
y <- dnorm(x,mean=0,sd=sqrt(5))
lines(5-y,x,lwd=10,col=6)
lim_sup <- qnorm(0.975,0,sqrt(5))
lim_sup
abline(h=c(lim_sup,-lim_sup),col=6,lwd=2)
#################################
y <- dnorm(x,mean=0,sd=sqrt(6))
lines(6-y,x,lwd=10,col=1)
lim_sup <- qnorm(0.975,0,sqrt(6))
lim_sup
abline(h=c(lim_sup,-lim_sup),col=1,lwd=2)
#########################################


#########################################
set.seed(321)
m <- 100
n <- 1000
t <- 6
pasos <- 2*rbinom(n*m,1,0.5)-1
r <- matrix(pasos, nrow=n, ncol=m)
delta <- t/n
x <- (0:n)*delta ##Valores del eje tiempo.
B <- matrix(rep(0,(n+1)*m), nrow=n+1, ncol=m)
for(k in 1:m){
        B[1,k] <- 0
        for(j in 1:n){
                aux <- r[j,k]*sqrt(delta)
                B[j+1,k] <- B[j,k]+aux
        }
}
par(bg='black',mar=c(2,4,2,1)) 
matplot(x,B, type="l",lwd=2,lty = 1,
        col=heat.colors(m),
        col.main=7,
        main='Movimiento Browniano Estándar',
        ylab='Espacio de estados',xlab='')
grid(6,col='green')
abline(v=0,h=0,col='white')

