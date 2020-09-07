# Diagramas de pie
todos <- colors(T)
par(mfrow=c(2,2),oma=rep(0,4),mar=rep(0,4))
pie(rep(1,10),col = todos)
pie(rep(1,20),col = todos)
pie(rep(1,50),col = todos)
pie(rep(1,100),col = todos)
########################################


par(mfrow=c(1,1),oma=rep(0,4),mar=rep(0,4))
pie(rep(1,100))
pie(rep(1,100),col=1:100)
pie(rep(1,100),col=todos)
text(1,1,'¿Cuáles son las coordenadas?')
abline(h=c(-1,0,1),v=c(-1,0,1))
pie(rep(1,100),col=todos,radius = 0.5)
pie(rep(1,100),col=todos,radius = 0.8)
pie(rep(1,100),col=todos,radius = 1)
pie(rep(1,100),col=todos,radius = 1,
    labels = '')

points(runif(1000,-2,2),runif(1000,-2,2),
       pch=1:26,col =todos)

pie(rep(1,100),col=todos,radius = 1,
    labels = '',add=T)

x <- runif(10000,-2,2)
y <- runif(10000,-2,2)
yes <- which(x^2+y^2>1)

points(x[yes],y[yes],
       pch=1:26,col =todos)



pie(sample(5:10,12,replace = T),
    labels = LETTERS[1:12])
pie(sample(5:10,12,replace = T),
    labels = LETTERS[1:12],
    col=rainbow(12),radius = 0.9)
pie(sample(5:10,12,replace = T),
    labels = LETTERS[1:12],
    col=rainbow(12),radius = 0.9,
    density = 10)
pie(sample(5:10,12,replace = T),
    labels = LETTERS[1:12],
    col=rainbow(12),radius = 0.9,
    density = 20)
pie(sample(5:10,12,replace = T),
    labels = LETTERS[1:12],
    col=rainbow(12),radius = 0.9,
    density = 50)
pie(sample(5:10,12,replace = T),
    labels = LETTERS[1:12],
    col=rainbow(12),radius = 0.9,
    density = 100)
pie(sample(5:10,12,replace = T),
    labels = LETTERS[1:12],
    col=rainbow(12),radius = 0.9,
    density = 200)
pie(sample(5:10,12,replace = T),
    labels = LETTERS[1:12],
    col=rainbow(12),radius = 0.9,
    density = 200,border = 'black')
pie(sample(5:10,12,replace = T),
    labels = LETTERS[1:12],
    col=rainbow(12),radius = 0.9,
    density = 200,border = 'black',lty = 2)
pie(sample(5:10,12,replace = T),
    labels = LETTERS[1:12],
    col=rainbow(12),radius = 0.9,
    density = 200,border = 'black',lty = 3)
pie(sample(5:10,12,replace = T),
    labels = LETTERS[1:12],
    col=rainbow(12),radius = 0.9,
    density = 200,border = 'black',lty = 4)

#############################################
etiquetas <- c('SMarino','Belgium','Andorra','Spain',
               'UK','Italy','France','Sweden',
               'Sint Maarten','Netherlands')
datos <- c(1238,814,660,580,562,550,440,431,350,346)
proporciones <- datos/sum(datos)
proporciones
prop_acum <- cumsum(proporciones)
prop_acum
proporciones <- round(proporciones*100,2)
proporciones
n <- length(proporciones)
info <- paste(etiquetas,proporciones,'%')
info

par(mfrow=c(1,1),mar=c(0,0,2,0),bg='black')
pie(datos,col=rainbow(n),radius = 1,
    labels = '',cex=1.2)

aux <- (c(0,prop_acum[1:(n-1)])+c(prop_acum[1:n]))/2
x <- 0.75*cos(aux*2*pi)#polares a euclidianas
y <- 0.75*sin(aux*2*pi)
points(x,y,pch=16,cex=2)

pie(datos,col=rainbow(n),radius = 1,labels = '')
title('Top ten de países con más muertes por millón de hab.',
       col.main='yellow',cex.main=2)
text(x,y,info,col = c(7,1,2,4,6,1,2,7,7,7),cex=1.5)
text(0.6,-0.8,
     'Fuente:',col='white')
text(0.6,-0.9,
     'https://www.worldometers.info/coronavirus/',col='white')
text(0.6,-1,
     'mayo 29 2pm',col='white')
