############################
#  La función polygon()    #
############################
par(mfrow=c(2,2),bg = 'lightyellow',mar=c(1,0,1,0))
plot(0:10,0:10,type = 'n',axes = F,ylab = '',xlab = '')
xx <- c(0,10,5,0)
yy <- c(0,0,10,0)
lines(xx,yy,col=2)

plot(0:10,0:10,type = 'n',axes = F,ylab = '',xlab = '')
xx <- c(0,10,5,0)
yy <- c(0,0,10,0)
points(xx,yy,col = 4,pch=16,cex=2)

plot(0:10,0:10,type = 'n',axes = F,ylab = '',xlab = '')
xx <- c(0,10,5,0)
yy <- c(0,0,10,0)
arrows(xx[1:3],yy[1:3],xx[2:4],yy[2:4],col = 6)

plot(0:10,0:10,type = 'n',axes = F,ylab = '',xlab = '')
xx <- c(0,10,5,0)
yy <- c(0,0,10,0)
polygon(xx,yy,col = 'yellow')

par(mfrow=c(1,1),bg = 'lightyellow',mar=c(1,0,1,0))
plot(0:10,0:10,type = 'n',axes = F,ylab = '',xlab = '')
xx <- c(0,10,5,0)
yy <- c(0,0,10,0)

polygon(xx,yy,col = 'yellow')

polygon(xx,-yy+10,col = 'green')

polygon(0.5*xx,0.5*yy,col = 'red')

polygon(0.5*xx+5,0.5*yy,col = 'blue')

polygon(0.5*xx+2.5,0.5*yy+5,col = 'pink')

#################################################
L <- 5
altura <- sqrt(3)*L/2
plot(c(0,L),c(0,altura),type = 'n',axes = F,
     ylab = '',xlab = '',asp=1)
xx <- c(0,L,L/2,0)
yy <- c(0,0,altura,0)

newxx <- xx
newyy <- yy
polygon(newxx,newyy,col = 1)

newxx <- newxx/2+L/4
newyy <- -newyy/2+altura/2
polygon(newxx,newyy,col = 2)

newxx <- newxx/2+L/4
newyy <- -newyy/2+altura/2
polygon(newxx,newyy,col = 3)

newxx <- newxx/2+L/4
newyy <- -newyy/2+altura/2
polygon(newxx,newyy,col = 4)
####################################
####################################
par(mfrow=c(1,1),bg = 'black',mar=c(1,0,1,0))
L <- 100
altura <- sqrt(3)*L/2
plot(c(0,L),c(0,altura),type = 'n',axes = F,
     ylab = '',xlab = '',asp=1)
xx <- c(0,L,L/2,0)
yy <- c(0,0,altura,0)

newxx <- xx
newyy <- yy
polygon(newxx,newyy,col = 'pink')
####################################################
####################################################

colores <- rainbow(40)

for(i in 2:10){
        newxx <- newxx/2+L/4
        newyy <- -newyy/2+altura/2
        polygon(newxx,newyy,col = colores[i])
}

L <- L/2
altura <- sqrt(3)*L/2
xx <- c(0,L,L/2,0)
yy <- c(0,0,altura,0)

newxx <- xx
newyy <- yy
for(i in 11:20){
        newxx <- newxx/2+L/4
        newyy <- -newyy/2+altura/2
        polygon(newxx,newyy,col = colores[i])
}

newxx <- xx 
newyy <- yy
for(i in 21:30){
        newxx <- (newxx)/2+L/4 
        newyy <- -newyy/2+altura/2
        polygon(newxx+L,newyy,col = colores[i])
}

newxx <- xx 
newyy <- yy
for(i in 31:40){
        newxx <- (newxx)/2+L/4 
        newyy <- -newyy/2+altura/2
        polygon(newxx+L/2,newyy+altura,col = colores[i])
}

colores2 <- heat.colors(30)

L <- L/2
altura <- sqrt(3)*L/2
xx <- c(0,L,L/2,0)
yy <- c(0,0,altura,0)

newxx <- xx
newyy <- yy
for(i in 1:10){
        newxx <- newxx/2+L/4
        newyy <- -newyy/2+altura/2
        polygon(newxx,newyy,col = colores2[i])
}

newxx <- xx 
newyy <- yy
for(i in 11:20){
        newxx <- (newxx)/2+L/4 
        newyy <- -newyy/2+altura/2
        polygon(newxx+L,newyy,col = colores2[i])
}

newxx <- xx 
newyy <- yy
for(i in 21:30){
        newxx <- (newxx)/2+L/4 
        newyy <- -newyy/2+altura/2
        polygon(newxx+L/2,newyy+altura,col = colores2[i])
}

colores3 <- terrain.colors(30)

newxx <- xx
newyy <- yy
for(i in 1:10){
        newxx <- newxx/2+L/4
        newyy <- -newyy/2+altura/2
        polygon(newxx+2*L,newyy,col = colores3[i])
}

newxx <- xx 
newyy <- yy
for(i in 11:20){
        newxx <- (newxx)/2+L/4 
        newyy <- -newyy/2+altura/2
        polygon(newxx+3*L,newyy,col = colores3[i])
}

newxx <- xx 
newyy <- yy
for(i in 21:30){
        newxx <- (newxx)/2+L/4 
        newyy <- -newyy/2+altura/2
        polygon(newxx+5*L/2,newyy+altura,col = colores3[i])
}

colores4 <- cm.colors(30)
newxx <- xx
newyy <- yy
for(i in 1:10){
        newxx <- newxx/2+L/4
        newyy <- -newyy/2+altura/2
        polygon(newxx+L,newyy+2*altura,col = colores4[i])
}

newxx <- xx 
newyy <- yy
for(i in 11:20){
        newxx <- (newxx)/2+L/4 
        newyy <- -newyy/2+altura/2
        polygon(newxx+2*L,newyy+2*altura,col = colores4[i])
}

newxx <- xx 
newyy <- yy
for(i in 21:30){
        newxx <- (newxx)/2+L/4 
        newyy <- -newyy/2+altura/2
        polygon(newxx+3*L/2,newyy+3*altura,col = colores4[i])
}

##################################################
par(bg='gray')
plot(c(0,10),c(0,10),type = 'n',asp=1,axes = F)
rect(0,0,10,10,col='red',lwd=5)
x <- 0:10
y <- sort(10*runif(11))
polygon(c(0,x,10,0),c(0,y,0,0),col='blue')
lines(c(0,x,10,0),c(0,y,0,0),col=1,lwd=3)

color <- topo.colors(10)
for(i in 1:10){
        polygon(c(0,x,10,0),c(0,y*(1-i/10),0,0),col=color[i])
        lines(c(0,x,10,0),c(0,y,0,0),col=1,lwd=3)
}

color <- terrain.colors(10)
for(i in 1:10){
        polygon(c(0,x,10,0),c(0,y*(1-i/10),0,0),col=color[i])
        lines(c(0,x,10,0),c(0,y,0,0),col=1,lwd=3)
}

color <- heat.colors(10)
for(i in 1:10){
        polygon(c(0,x,10,0),c(0,y*(1-i/10),0,0),col=color[i])
        lines(c(0,x,10,0),c(0,y,0,0),col=1,lwd=3)
}

color <- rainbow(10)
for(i in 1:10){
        polygon(c(0,x,10,0),c(0,y*(1-i/10),0,0),col=color[i])
        lines(c(0,x,10,0),c(0,y,0,0),col=1,lwd=3)
}
