############################
#  La función curve()      #
############################
par(mfrow=c(2,2),bg = 'lightyellow',
    oma=c(1,0,0,0),mar=c(2,2,1,0))

curve(x^2)

curve(x^2,-1,1)

curve(x^2,-1,1,axes=F,col=2,lwd=4)
arrows(-1,0,1,0,lwd=2)
arrows(0,0,0,1,lwd=2)

curve(x^2,-1,1,axes=F,col=2,lwd=4)
abline(h=1/2)
xx <- seq(-sqrt(1/2),sqrt(1/2),0.01)
yy <- xx^2
polygon(c(xx,sqrt(1/2),-sqrt(1/2)),
        c(yy,1/2,1/2),
        col='orange')
arrows(-1,0,1,0,lwd=2)
arrows(0,0,0,1,lwd=2)

curve(x^2*exp(-x),0,10,axes=F,col=2,lwd=4)
abline(v=5)
xx <- seq(5,10,0.01)
yy <- xx^2*exp(-xx)
polygon(c(xx,5,5),
        c(yy,0,0),
        col='gray',lwd=2)
curve(x^3*exp(-x)/2,0,10,col=4,lwd=4,add=T)
curve(x^2*exp(-x),col=2,lwd=4,add=T)
arrows(0,0,10.2,0,lwd=2,length = 0.1)
abline(v=0,lwd=2)
text(5,-.05,'x=5')

#
plot(c(0,10),c(-0.1,0.8),type='n',
     axes=F,xlab='',ylab='')
curve(x^3*exp(-x)/2,0,10,col=4,lwd=4,add=T)
curve(x^2*exp(-x),0,10,col=2,lwd=4,add=T)
abline(v=5)
xx <- seq(5,10,0.01)
yy <- xx^2*exp(-xx)
polygon(c(xx,5,5),
        c(yy,0,0),
        col='gray',lwd=2)
curve(x^2*exp(-x),col=2,lwd=4,add=T)
arrows(0,0,10.2,0,lwd=2,length = 0.1)
abline(v=0,lwd=2)
text(5,-.05,'x=5')

#
plot(c(0,11),c(-0.1,0.7),
     type='n',axes=F,xlab='',ylab='')
curve(x^2*exp(-x),0,10,col=2,lwd=4,add=T)
lines(c(5,5),c(0,5^2*exp(-5)),lwd=2)
xx <- seq(5,10,0.01)
yy <- xx^2*exp(-xx)
polygon(c(xx,5,5),
        c(yy,0,0),
        col='gray',lwd=2)
curve(x^2*exp(-x),0,10,col=2,lwd=4,add=T)
arrows(0,0,10.7,0,lwd=2,length = 0.1)
arrows(0,0,0,0.65,lwd=2,length = 0.1)
text(5,-.05,'x=5')
text(0,-.05,'0')

#
plot(c(0,11),c(-0.1,0.7),
     type='n',axes=F,xlab='',ylab='')
curve(x^2*exp(-x),0,10,col=2,lwd=4,add=T)
lines(c(5,5),c(0,5^2*exp(-5)),lwd=2)
xx <- seq(5,10,0.01)
yy <- xx^2*exp(-xx)
polygon(c(xx,5,5),
        c(yy,0,0),
        col='gray',lwd=2)
curve(x^2*exp(-x),0,10,col=2,lwd=4,add=T)
curve(x^3*exp(-x)/2,0,10,col=4,lwd=4,add=T)
arrows(0,0,10.7,0,lwd=2,length = 0.1)
arrows(0,0,0,0.65,lwd=2,length = 0.1)
text(5,-.05,'x=5')
text(0,-.05,'0')

#
par(mfrow=c(1,1),bg = 'gray',
    oma=c(0,0,0,0),mar=c(1,0,1,0))
plot(c(-0.1,1.1),c(-0.1,1.1),type='n',
     axes=F,xlab='',ylab='',asp=1)
curve(1*x,0,1,lwd=4,add=T)
m <- 100
for(i in 2:m){
        curve(x^i,0,1,col=i,lwd=3,add=T)
}

for(i in 2:m){
        curve(x^(1/i),0,1,col=i,lwd=3,add=T)
}


plot(c(-1.1,1.1),c(-1.1,1.1),type='n',
     axes=F,xlab='',ylab='',asp=1)
curve(1*x,-1,1,lwd=4,add=T)

m <- 100
colores <- topo.colors(m)
for(i in 2:m){
        curve(x*i,-1,1,col=colores[i],lwd=3,add=T)
}

colores <- cm.colors(m)
for(i in 2:m){
        curve(x/i,-1,1,col=colores[i],lwd=3,add=T)
}

curve(-1*x,-1,1,lwd=4,add=T)
colores <- terrain.colors(m)
for(i in 2:m){
        curve(-x*i,-1,1,col=colores[i],lwd=3,add=T)
}

colores <- heat.colors(m)
for(i in 2:m){
        curve(-x/i,-1,1,col=colores[i],lwd=3,add=T)
}



##################################################

plot(c(-1.1,1.1),c(-1.1,1.1),type='n',
     axes=F,xlab='',ylab='',asp=1)
curve(1*x,-1,1,lwd=4,add=T)

m <- 100
colores <- rainbow(m)
for(i in 2:m){
        curve(x*rnorm(1,0,1),
              -1,1,
              col=colores[i],
              lwd=5,add=T)
}

#

K <- 10
a <- runif(K,-1,1)
f <- function(x){
        y <- 0
        for(i in 1:K){
                y <- y + a[i]*x^i
        }
        y
}
curve(f,-1,1,lwd=10,col=sample(1:7,1),asp=1)
abline(h=0,v=0)
a
imagen <- as.numeric(lapply(seq(-1,1,0.1),f))
imagen
Maximo <- max(imagen)
Minimo <- min(imagen)
#

contador <- 0
repeat{
        a <- runif(K,-1,1)
        f <- function(x){
                y <- runif(1,Minimo,Maximo)
                for(i in 1:K){
                        y <- y + a[i]*x^i
                }
                y
        }
        curve(f,lwd=10,col=sample(1:7,1),add=T)
        contador <- contador + 1
        if(contador==50){break}
}

