x <- seq(-5,5,0.01)
y <- exp(-x^2)
w <- 0.8*exp(-x^2/3) #igual a 0.8*z
par(mfrow=c(1,2),mar=c(5,4,2,0.5), bg='lightyellow')
##############################################
plot(x,x,
     col = 'magenta',
     type = 'l',
     lwd = 7,
     lty = 6,
     xlim = c(-4,4),
     ylim = c(-1,1),
     xlab = 'eje horizontal',
     ylab = 'eje vertical',
     main = 'Gráfico interesante')
abline(h=0,v=0)
############################################
lines(x,2*x*y,col='green',type = 'l',lwd=6,lty=1)
lines(x,y,col='blue',type = 'l',lwd=6,lty=2)
lines(x,w,col='red',lwd=7,lty=3)
legend(1, -0.5, 
       c('la uno','la dos','la tres','la cuatro'),  ##OJO
       lty = c(6,1,2,3),       ##OJO
       lwd = 6,
       col = c('magenta','green','blue','red'),
       cex = 1.2)

################################################
#                                              #
#           La función matplot()               #
#                                              #
################################################
X <- matrix(c(x,2*x*y,y,w),length(x),4)
matplot(x,X,
        col = c('magenta','green','blue','red'), ## OJO
        type = 'l',
        lwd = 7,
        lty = c(6,1,2,3),       ##OJO
        xlim = c(-4,4),
        ylim = c(-1,1),
        xlab = 'eje horizontal',
        ylab = 'eje vertical',
        main = 'Gráfico interesante')
abline(h=0,v=0)        
legend(1, -0.5, 
       c('la uno','la dos','la tres','la cuatro'),    ##OJO
       lty = c(6,1,2,3),       ##OJO
       lwd = 6,
       col = c('magenta','green','blue','red'),
       cex = 1.2)
