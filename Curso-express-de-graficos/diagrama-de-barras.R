# barplot
?barplot
par(mfrow=c(1,1),bg ='lightyellow',oma=c(0,3,0,0),mar=c(2,1,2,1))
zacarias <- c(10,8,9,10)
zoe <- c(5,6,7,8)
zoraida <- c(8,8,9,7)
zulema <- c(7,8,7,9)
alumnos.con.z <- cbind(zacarias,zoe,zoraida,zulema)
alumnos.con.z

rownames(alumnos.con.z) <- c("español","física","biología","historia")

barplot(alumnos.con.z, beside=T,
        col=c("blue","red","yellow","green"),
        main="Diagrama de barras")

legend(5.45,10,row.names(alumnos.con.z),
       fill = c("blue","red","yellow","green"),bg="lightgray")

legend(5.45,10,row.names(alumnos.con.z),cex=0.5,
       fill = c("blue","red","yellow","green"),bg="lightgray")
## 
barplot(zoe,beside=TRUE,col=c("blue","red","yellow","green"),
        main="Calificaciones de Zoé")
legend(.3, 8,row.names(alumnos.con.z),
       fill = c("blue","red","yellow","green"),bg="lightgray")


barplot(alumnos.con.z,beside=T,horiz=T,
                 legend=F,col=c("blue","red","yellow","green"),las=1)
for(j in 1:4){for(i in 0:3){
        text(4,0.5+j+i*5,row.names(alumnos.con.z)[j],col = 1)}}

par(mfrow=c(2,2))
barplot(alumnos.con.z,beside=F,
        col=c("blue","red","yellow","green"),
        main="Diagrama de barras")
barplot(alumnos.con.z,beside=F,
        legend=c("esp","fís","biol","hist"),
        col=terrain.colors(4),
        main="Diagrama de barras")
barplot(alumnos.con.z,beside=F,
        col=cm.colors(4),
        horiz=T,main="Diagrama de barras")
barplot(alumnos.con.z,beside=T,
        legend=c("español","física","biología","historia"),
        col=heat.colors(4),
        horiz=T,main="Diagrama de barras")

