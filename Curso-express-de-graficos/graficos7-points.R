par(mfrow=c(1,2),
    mar=c(5,4,2,0.5), 
    bg='lightyellow')
########################
# La función points()  #
########################
plot(0:10,sample(0:10),type = 'l',asp = 1)
abline(h=0,v=0)
points(0:10,0:10,pch=16)
plot(0:10,sample(0:10),
     type = 'n',
     axes = F,
     xlab = '',
     ylab = '',
     asp = 1)
points(0:10,0:10,pch=16)
plot(0:10,sample(0:10),
     type = 'n',
     axes = F,
     xlab = '',
     ylab = '',
     asp = 1)
points(0:10,0:10,pch=16,cex=seq(1,3,0.2))
points(0:10,0:10,pch=16,cex=seq(1,3,0.2),
       col=4)
points(0:10,0:10,pch=16,cex=seq(1,3,0.2),
       col=1:11)

plot(0:10,sample(0:10),type = 'n',
     axes = F,xlab = '',ylab = '',asp = 1)
points(0:10,rep(2,11),
       pch=16,
       cex=seq(1,4,0.3),
       col=rainbow(11))
text(5,3,'rainbow()',col='blue',cex = 2)
points(0:10,rep(4,11),
       pch=16,
       cex=seq(1,4,0.3),
       col=terrain.colors(11))
text(5,5,'terrain.colors()',col='blue',cex = 2)
points(0:10,rep(6,11),
       pch=16,
       cex=seq(1,4,0.3),
       col=heat.colors(11))
text(5,7,'heat.colors()',col='blue',cex = 2)
points(0:10,rep(8,11),
       pch=16,
       cex=seq(1,4,0.3),
       col=topo.colors(11))
text(5,9,'topo.colors()',col='blue',cex = 2)
box()

plot(0:10,sample(0:10),type = 'n',
     axes = F,xlab = '',ylab = '',asp = 1)
points(0:10,0:10,
       pch=1:11,
       cex=seq(1,4,0.3),
       col=rainbow(11))
points(1:10,9:0,
       pch=12:21,
       cex=seq(1,4,0.3),
       col=rainbow(11))
box()

plot(0:10,sample(0:10),type = 'n',
     axes = F,xlab = '',ylab = '',asp = 1)
points(0:10,0:10,
       pch=letters[1:11],
       cex=seq(1,4,0.3),
       col=rainbow(11))
points(0:9,10:1,
       pch=c('1','2','d','e','m','a','y','o','2','0'),
       cex=seq(1,4,0.3),
       col=rainbow(10))
points(1:10,rep(5,10),
       pch=sample(1:25,10),
       cex=3,
       col=sample(1:8,10,replace = T))
points(rep(5,10),1:10,
       pch=sample(32:127,10),
       cex=3,
       col=c(1,2))
box()

plot(0:100,0:100,type = 'n',axes = F,
     xlab = '',ylab = '',asp = 1)
points(sample(1:100),sample(1:100),
       pch=sample(c(1:25)),
       cex=sample(1:100)/20,
       col=sample(rainbow(100)))
points(sample(1:100),sample(1:100),
       pch=sample(c(letters[1:26]),25),
       cex=sample(1:100)/20,
       col=sample(rainbow(100)))

plot(0:1000,0:1000,type = 'n',axes = F,
     xlab = '',ylab = '',asp = 1)
points(sample(1:1000),sample(1:1000),
       pch=sample(c(1:25),1000,replace = T),
       cex=sample(1:1000)/200,
       col=sample(rainbow(1000)))

#








############################################
#       Una aplicación interesante         #
############################################
m <- 10000
x <- runif(m,-1,1) #números aleatorios entre -1 y 1
y <- runif(m,-1,1) #números aleatorios entre -1 y 1
plot(x,y,asp = 1)
abline(h=0,v=0)
verdes <- numeric(m)
for(i in 1:m){
        if(x[i]^2+y[i]^2<=1){verdes[i] <- 1}
}
verdes
mean(verdes) #aproxima a pi/4
which(verdes>0)
exitosos <- which(verdes>0)
points(x[exitosos],y[exitosos],pch=16,col='green')
points(x[-exitosos],y[-exitosos],pch=16,col='red')
title(paste('Hubo ',length(exitosos),' casos de éxito'))
m*pi/4 # casos esperados teóricamente

#
#



par(mfrow=c(1,1),
    mar=c(5,4,2,0.5), 
    bg='lightgray')
plot(1:10,type = 'n',
     axes = F,
     xlab = '',
     ylab = '')
points(seq(2,9,1.5),c(3,4,6,7,8),
       pch=c('E','l','F','i','n'),
       cex=10,
       col=sample(rainbow(100),6))
box()

