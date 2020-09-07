par(mfrow=c(1,1),
    mar=c(1,1,1,1), 
    bg='black')
########################
# La función arrows()  #
########################
plot(-10:10,-10:10,   #lienzo
     type = 'n',
     axes = F,
     xlab = '',
     ylab = '',
     asp = 1) #x/y
arrows(-10,0,10,0,col = 'white',lwd=5)
arrows(0,-10,0,10,col = 'white',lwd=5)

arrows(0,0,3,4,col = 'yellow',lwd=5)
arrows(0,0,8,1,col = 'orange',lwd=5)
arrows(3,4,8,1,col = 'green',lwd=5)

arrows(-9,-2,-2,-2,col = 'pink',lwd=5,length = 0.5)
arrows(-9,-4,-2,-4,col = 'magenta',lwd=5,length = 0.5,angle = 45)
arrows(-9,-6,-2,-6,col = 'brown',lwd=5,length = 0.5,angle = 45,code = 1)
arrows(-9,-8,-2,-8,col = 'gray',lwd=5,length = 0.5,angle = 45,code = 1,lty = 2)

arrows(-9,2,-2,2,col = 'blue',lwd=5,length = 0.75)
arrows(-9,4,-2,4,col = 'green',lwd=5,length = 0.5,angle = 60)
arrows(-9,6,-2,6,col = 'lightgreen',lwd=5,length = 0.5,angle = 25,code = 3)
arrows(-9,8,-2,8,col = 'lightblue',lwd=5,length = 0.5,angle = 25,code = 3,lty = 3)

x <- 5
y <- -5
arrows(x,y,x+2,y-1,col = 'pink',lwd=5,length = 0.75)
arrows(x,y,x+3,y-3,col = 'yellow',lwd=5,length = 0.5,angle = 60)
arrows(x,y,x-2,y-3,col = 'orange',lwd=5,length = 0.5,angle = 25,code = 3)
arrows(x,y,x-2,y+4,col = 'white',lwd=5,length = 0.5,angle = 25,code = 3,lty = 4)



par(mfrow=c(1,1),
    mar=c(1,1,1,1), 
    bg='lightgray')

plot(-10:10,-10:10,   #lienzo
     type = 'n',
     axes = F,
     xlab = '',
     ylab = '',
     asp = 1)
arrows(-10,0,10,0,lwd=5)
arrows(0,-10,0,10,lwd=5)

m <- 1000
particion <- 20/m
for(i in 1:(m+1)){
        xx <- -10+(i-1)*particion
        yy <- sqrt(100-xx^2)*(-1)^i
        arrows(0,0,xx,yy,
               col = rainbow(m)[i],
               lwd=sample(1:10,1),
               length = runif(1,0),
               angle = sample(1:60,1),
               lty = sample(1:6,1))
}

