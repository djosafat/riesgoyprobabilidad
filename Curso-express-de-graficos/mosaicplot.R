# La función mosaicplot()
#tabla de frecuencias (tabla de contingencia)
mariscos <- c(6,8,8,12,9)
pollo <- c(4,8,7,6,12)
cerdo <- c(5,3,2,7,9)
res <- c(3,5,6,10,15)
pasta <- c(5,7,8,7,10)         
muestra <- cbind(mariscos,pollo,cerdo,res,pasta)
muestra
rownames(muestra) <- c('lunes','martes',
                       'miércoles','jueves','viernes')
muestra
########################################################
par(bg='lightyellow',mar=c(0.5,1,1,1))
mosaicplot(muestra,color = 2:6
           ,main = 'Elección de alimentos',cex=1.2)
par(bg='lightyellow',mfrow=c(2,2),mar=c(0.5,1,1,1))
mosaicplot(muestra,color = 2:6
           ,main = 'Elección de alimentos',cex=1.2,las = 0)
mosaicplot(muestra,color = 2:6
           ,main = 'Elección de alimentos',cex=1.2,las = 1)
mosaicplot(muestra,color = 2:6
           ,main = 'Elección de alimentos',cex=1.2,las = 2)
mosaicplot(muestra,color = 2:6
           ,main = 'Elección de alimentos',cex=1.2,las = 3)
########################################################
?mosaicplot
########################################################

par(bg='lightyellow',mfrow=c(2,2),mar=rep(0,4),oma=rep(0,4))
a <- rnbinom(100,10,1/2)
b <- matrix(a,20,5)
mosaicplot(b,main = '',col = cm.colors(7),cex.axis = 0.01)
a <- rnbinom(50,10,1/2)
b <- matrix(a,10,5)
mosaicplot(b,main = '',col = cm.colors(7),cex.axis = 0.01)
a <- rnbinom(20,10,1/2)
b <- matrix(a,4,5)
mosaicplot(b,main = '',col = cm.colors(7),cex.axis = 0.01)
a <- rnbinom(10,10,1/2)
b <- matrix(a,2,5)
mosaicplot(b,main = '',col = cm.colors(7),cex.axis = 0.01)

########################################################
par(bg='lightyellow',mfrow=c(1,1),mar=rep(2,4))
Baja <- c(0.7,0.25,0.05)
Media <- c(0.3,0.55,0.15)
Alta <- c(0.1,0.4,0.5)
P <- rbind(Baja,Media,Alta)
colnames(P) <- c('Baja','Media','Alta')
P
mosaicplot(P,main = 'Transición de clase a clase en una generación',
           col = c(3,7,2),cex.axis = 1.2,off = 2)
centros_x <- c(.18,.5,.82)
leyendas <- P[3,]
text(centros_x,c(0,0.07,0.25),leyendas,cex=1.2)
leyendas <- P[2,]
text(centros_x,c(0.15,0.4,0.7),leyendas,cex=1.2)
leyendas <- P[1,]
text(centros_x,c(0.65,.85,0.93),leyendas,cex=1.2)
