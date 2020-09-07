par(bg = 'gray',oma=c(0,0,0,0),mar=c(0,0,0,0))
x <- 1:8 #coordenada x
x
y <- 10*runif(8) #coordenada y
y
z <- runif(8) #proporciones
z
z/2
symbols(x, y, thermometers = cbind(1, 1, z))
symbols(x, y, thermometers = cbind(0.5, 1, z))
symbols(x, y, thermometers = cbind(1, 0.5, z))
symbols(x, y, thermometers = cbind(0.5, 1, z), inches = .5)
symbols(x, y, thermometers = cbind(0.5, 1, z), inches = .8, fg = 1:8)
symbols(x, y, t = cbind(0.35, 1, z), inches = .8, fg = 2, bg=3)
symbols(x, y, t = cbind(0.35, 1, z,z/2), inches = .8, fg = 2, bg=3)

for(i in -20:20){curve(-0.9*x+i,col='white',add = T)}
symbols(x, y, t = cbind(0.35, 1, z,z/2), 
        inches = .8, fg = 2, bg=3,add=T)

##################################################3

x <- 1:8 #coordenada x
x
y <- 10*runif(8) #coordenada y
y
radios <- runif(8,0.2,0.8)
symbols(x, y, circles = radios)
symbols(x, y, circles = radios, inches = .7)
symbols(x, y, circles = radios, inches = .5, fg = 1:8)
symbols(x, y, circles = radios, inches = .5, fg = 1:8, bg=1:8)
symbols(x, y, circles = radios, inches = .5, fg = 1, bg=1:8)

par(bg = 'black',oma=c(0,0,0,0),mar=c(0,0,0,0))
plot(c(0,8),c(0,8), axes = F)
planetas <- 25
points(runif(100,0,8),runif(100,0,8),pch='*',col='white',cex=2)
points(runif(100,0,8),runif(100,0,8),pch='*',col='white',cex=1.5)
points(runif(100,0,8),runif(100,0,8),pch='*',col='white',cex=1)
points(runif(100,0,8),runif(100,0,8),pch='*',col='white',cex=.5)
x <- runif(planetas,0,8) #coordenada x
y <- runif(planetas,0,8) #coordenada y
radios <- runif(planetas,0.2,1.5)
symbols(x,y,circles=radios,inches=.5,bg=terrain.colors(planetas),add=T)
x <- runif(planetas,0,8) #coordenada x
y <- runif(planetas,0,8) #coordenada y
radios <- runif(planetas,0.2,2)
symbols(x,y,circles=radios,inches=.5,bg=rainbow(planetas),add=T)

par(bg = 'darkgreen',oma=c(0,0,0,0),mar=c(0,0,0,0))
plot(c(0,8),c(0,8), axes = F)
abline(h=seq(0,8,0.5),col='gray',lwd=seq(1,3,0.125))
abline(v=seq(0,8,0.5),col='white',lwd=seq(1,3,0.125))
estrellas <- 15
x <- runif(estrellas,0,8) #coordenada x
y <- runif(estrellas,0,8) #coordenada y
r1 <- runif(estrellas,0,2)
r2 <- runif(estrellas,0,2)
r3 <- runif(estrellas,0,2)
r4 <- runif(estrellas,0,2)
r5 <- runif(estrellas,0,2)
r6 <- runif(estrellas,0,2)
symbols(x,y,stars=cbind(r1,r2,r3,r4,r5,r6),
        bg=terrain.colors(estrellas),add = T)
x <- runif(estrellas,0,8) #coordenada x
y <- runif(estrellas,0,8) #coordenada y
symbols(x,y,stars=cbind(r1,r2,r3,r4,r5),
        bg=cm.colors(estrellas),add = T)
x <- runif(estrellas,0,8) #coordenada x
y <- runif(estrellas,0,8) #coordenada y
symbols(x,y,stars=cbind(r1,r2,r3,r4),
        bg=topo.colors(estrellas),add = T)
x <- runif(estrellas,0,8) #coordenada x
y <- runif(estrellas,0,8) #coordenada y
symbols(x,y,stars=cbind(r1,r2,r3),
        bg=heat.colors(estrellas),add = T)
x <- runif(estrellas,0,8) #coordenada x
y <- runif(estrellas,0,8) #coordenada y
symbols(x,y,stars=cbind(r1,r3,r6),
        bg=rainbow(estrellas),add = T)
