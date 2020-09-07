# La función persp
par(bg='lightyellow')
x <- seq(-2,2,0.1)
y <- seq(-2,2,0.1)
n <- length(x)
m <- length(y)
z <- matrix(0,n,m)
for(i in 1:n){
        for(j in 1:m){
                z[i,j] <- x[i]**2+y[j]**2
        }
}
persp(x,y,z)
persp(x,y,z,col = 'green')
persp(x,y,z,col = 'green',axes = F)
persp(x,y,z,col = 'green',box = F)
superf <- persp(x,y,z,col = 'green',box = F)
for(i in 1:m){
        points(trans3d(x,y[i],0,pmat=superf))
}
superf <- persp(x,y,z,col = 'green',box = F)
for(i in 1:m){
        points(trans3d(x,y[i],0,pmat=superf),pch='V',col='green',cex=0.5)
}
superf <- persp(x,y,z,col = 'green',box = F)
for(i in 1:m){
        lines(trans3d(x,y[i],0,pmat=superf))
        lines(trans3d(x[i],y,0,pmat=superf))
}


persp(x,y,z,col = 'green',box = F,ltheta = 0, shade = 0)
persp(x,y,z,col = 'green',box = F,ltheta = 30, shade = 0)
persp(x,y,z,col = 'green',box = F,ltheta = 60, shade = 0)
persp(x,y,z,col = 'green',box = F,ltheta = 120, shade = 0)
persp(x,y,z,col = 'green',box = F,ltheta = 120, shade = 0.25)
persp(x,y,z,col = 'green',box = F,ltheta = 120, shade = 0.5)
persp(x,y,z,col = 'green',box = F,ltheta = 120, shade = 0.75)
persp(x,y,z,col = 'green',box = F,ltheta = 120, shade = 0.75,expand = 0.5)
persp(x,y,z,col = 'green',box = F,ltheta = 120, shade = 0.75,expand = 2)

par(bg='black')
persp(x,y,z,col = 'green',box = F,theta = 30)
persp(x,y,z,col = 'green',box = F,theta = 30,phi = 45)
persp(x,y,z,col = 'green',box = F,theta = 0,phi = 35)
persp(x,y,z,col = 'green',box = F,theta = 0,phi = 35,expand = 2)
persp(x,y,z,col = 'green',box = F,theta = 30,shade = 0.5)
persp(x,y,z,col = c('red','green'),box = F,theta = 30,phi = 30)
persp(x,y,z,col = c('red','green'),box = F,theta = 60,phi = 30)
persp(x,y,z,col = c(8,1),box = F,theta = 60,phi = 30)
persp(x,y,z,col = terrain.colors(n*m),box = F,theta = 60,phi = 30)
persp(x,y,z,col = c(rep(2,n*m/2),rep(7,n*m/2)),box = F,theta = 60,phi = 15)
persp(x,y,z,col = c(rep(2,n*m/2),rep(7,n*m/2)),box = F,theta = 0,phi = 15)
##############################################################
sabana <- persp(x,y,z,col = 0,box = F,theta = 0,phi = 15,
                main = '¡Viva México!',col.main='7',cex.main=2)
for(i in 1:m){
        points(trans3d(x,y[i],0,pmat=sabana),col=1:8,pch=16,cex=0.2)
}
for(i in 1:m){
        lines(trans3d(x,2,x[i]^2+y[i]^2,pmat=sabana),col=8)
}
for(i in 1:m){
        lines(trans3d(-2,y,x[i]^2+y[i]^2,pmat=sabana),col='green')
}
for(i in 1:m){
        lines(trans3d(2,y,x[i]^2+y[i]^2,pmat=sabana),col='red')
}
for(i in 1:n){
        fxy <- x[i]^2+y[i]^2
        lines(trans3d(x,sqrt(fxy-x^2),fxy,pmat=sabana),col='white',lwd=3)
        lines(trans3d(x,-sqrt(fxy-x^2),fxy,pmat=sabana),col='white',lwd=3)
}
################################################
################################################
w <- matrix(0,n,m)
for(i in 1:n){
        for(j in 1:m){
                w[i,j] <- x[i]**2-y[j]**2
        }
}
persp(x,y,w)
persp(x,y,w,col = 'magenta')
persp(x,y,w,col = 'magenta',theta = 5)
persp(x,y,w,col = 'magenta',theta = 10)
persp(x,y,w,col = 'magenta',theta = 15)
persp(x,y,w,col = 'magenta',theta = 20)
persp(x,y,w,col = 'magenta',theta = 30)
persp(x,y,w,col = 'magenta',theta = 60)
persp(x,y,w,col = 'magenta',theta = 90)
persp(x,y,w,col = 'magenta',theta = 90,phi = 90)
persp(x,y,w,col = 'magenta',theta = 90,phi = 75)
persp(x,y,w,col = 'magenta',theta = 90,phi = 60)
persp(x,y,w,col = 'magenta',theta = 90,phi = 45)
persp(x,y,w,col = 'magenta',theta = 90,phi = 15)
persp(x,y,w,col = 'magenta',theta = 90,phi = 0)

A <- persp(x,y,w+z,col = 'yellow')
points(trans3d(x[n]/2,y[m]/2,6,A),col=7,cex=8,pch=16)
points(trans3d(-x[n]/2,y[m]/2,6,A),col=7,cex=8,pch=16)
points(trans3d(x[n]/2,y[m]/2,6,A),col=2,cex=4,pch=17)
points(trans3d(-x[n]/2,y[m]/2,6,A),col=2,cex=4,pch=17)

persp(x,y,w,col = 'blue',theta = 30,phi = 0)
persp(x,y,w,col=terrain.colors(n*m),theta = 30,phi = 0)
persp(x,y,w,col=heat.colors(n*m),theta = 30,phi = 0)
persp(x,y,w,col=cm.colors(n*m),theta = 30,phi = 0)
persp(x,y,w,col=topo.colors(n*m),theta = 30,phi = 0)
persp(x,y,w,col=rainbow(n*m),theta = 30,phi = 0)
persp(x,y,w,col=colors(n*m),theta = 30,phi = 0)

#######################################################################
#######################################################################
x <- seq(-10, 10, length= 30)
y <- x
f <- function(x, y) { 
        aux <- sqrt(x^2+y^2)
        10 * sin(aux)/aux 
}
z <- outer(x, y, f)
par(bg = "lightyellow")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "pink")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "pink",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Z",axes = F)
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "magenta",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Z")
(persp(x, y, z, theta = 30, phi = 30, expand = 0.5))

######################################

## Campana
x <- seq(-2.5,2.5,l=100)
y <- seq(-2.5,2.5,l=100)
f <- function(t1,t2){exp(-t1^2-t2^2)}
z <- outer(x,y,f)
persp(x, y, z, theta = 30, phi = 15, expand = 0.7, 
      col = heat.colors(100),
      ltheta = 60, shade = 0.5,box = F,
      xlab = "X", ylab = "Y", zlab = "Z",
      main = 'Campana de Gauss',
      cex.main=3)

## Función de probabilidad
par(bg='white')
set.seed(123)
fxy <- matrix(runif(9),3,3)
fxy <- fxy/sum(fxy)
x <- 1:3
y <- 1:3
B <- persp(x, y, fxy, col = 0,  xlab = "X", ylab = "Y", zlab = "Z",
      main = 'f(x,y)', theta = 30,
      expand = 0.9,border = F,box = F,
      cex.main=2,shade = 0.01)
fxy
sum(fxy)
lines(trans3d(c(1,3.5),c(1,1),0,B))
lines(trans3d(c(1,3.5),c(2,2),0,B))
lines(trans3d(c(1,3.5),c(3,3),0,B))
lines(trans3d(c(1,1),c(1,3.5),0,B))
lines(trans3d(c(2,2),c(1,3.5),0,B))
lines(trans3d(c(3,3),c(1,3.5),0,B))
lines(trans3d(1,1,c(0,1),B))
points(trans3d(x,y[1],0,B),pch=16,cex=3,col='gray')
points(trans3d(x,y[2],0,B),pch=16,cex=2.5,col='gray')
points(trans3d(x,y[3],0,B),pch=16,cex=2,col='gray')
points(trans3d(x,y[1],fxy[,1],B),pch=16,cex=3,col=2)
lines(trans3d(1,1,c(0,fxy[1,1]),B),lwd=4,lty=2)
lines(trans3d(2,1,c(0,fxy[2,1]),B),lwd=4,lty=2)
lines(trans3d(3,1,c(0,fxy[3,1]),B),lwd=4,lty=2)
points(trans3d(x,y[2],fxy[,2],B),pch=16,cex=2.5,col=3)
lines(trans3d(1,2,c(0,fxy[1,2]),B),lwd=3,lty=2)
lines(trans3d(2,2,c(0,fxy[2,2]),B),lwd=3,lty=2)
lines(trans3d(3,2,c(0,fxy[3,2]),B),lwd=3,lty=2)
points(trans3d(x,y[3],fxy[,3],B),pch=16,cex=2,col=4)

lines(trans3d(1,3,c(0,fxy[1,3]),B),lwd=2,lty=2)
lines(trans3d(2,3,c(0,fxy[2,3]),B),lwd=2,lty=2)
lines(trans3d(3,3,c(0,fxy[3,3]),B),lwd=2,lty=2)

text(trans3d(1.2,1.2,0,B),'(1,1)',cex = 1.2)
text(trans3d(1.2,2.2,0,B),'(1,2)',cex = 1)
text(trans3d(1.2,3.2,0,B),'(1,3)',cex = 0.8)

text(trans3d(2.2,1.2,0,B),'(2,1)',cex = 1.2)
text(trans3d(2.2,2.2,0,B),'(2,2)',cex = 1)
text(trans3d(2.2,3.2,0,B),'(2,3)',cex = 0.8)

text(trans3d(3.2,1.2,0,B),'(3,1)',cex = 1.2)
text(trans3d(3.2,2.2,0,B),'(3,2)',cex = 1)
text(trans3d(3.2,3.2,0,B),'(3,3)',cex = 0.8)

text(trans3d(1.2,1.2,fxy[1,1],B),paste(round(fxy[1,1],3)),cex = 1.2,col = 2)
text(trans3d(1.2,2.2,fxy[1,2],B),paste(round(fxy[1,2],3)),cex = 1,col = 3)
text(trans3d(1.2,3.2,fxy[1,3],B),paste(round(fxy[1,3],3)),cex = 0.8,col = 4)

text(trans3d(2.2,1.2,fxy[2,1],B),paste(round(fxy[2,1],3)),cex = 1.2,col = 2)
text(trans3d(2.2,2.2,fxy[2,2],B),paste(round(fxy[2,2],3)),cex = 1,col = 3)
text(trans3d(2.2,3.2,fxy[2,3],B),paste(round(fxy[2,3],3)),cex = 0.8,col = 4)

text(trans3d(3.2,1.2,fxy[3,1],B),paste(round(fxy[3,1],3)),cex = 1.2,col = 2)
text(trans3d(3.2,2.2,fxy[3,2],B),paste(round(fxy[3,2],3)),cex = 1,col = 3)
text(trans3d(2.8,3.2,fxy[3,3],B),paste(round(fxy[3,3],3)),cex = 0.8,col = 4)

