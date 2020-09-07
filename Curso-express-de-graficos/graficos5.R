# lwd lty  #lines()
x <- seq(-5,5,0.01)
y <- exp(-x^2)
z <- exp(-x^2/3)
w <- 0.8*exp(-x^2/3) #igual a 0.8*z
plot(x,y,col='red',type = 'l')
plot(x,y,col='red',type = 'l',lwd=6)
plot(x,y,col='red',type = 'l',lwd=6,lty=1)
plot(x,z,col='red',type = 'l',lwd=6,lty=2)
plot(x,w,col='red',type = 'l',lwd=6,lty=3)
plot(x,x+y,col='red',type = 'l',lwd=6,lty=4)
plot(x,x+z,col='red',type = 'l',lwd=6,lty=5)
plot(x,z+y,col='red',type = 'l',lwd=6,lty=6)
plot(x,x*z,col='magenta',type = 'l',lwd=7,lty=3)
abline(h=0,v=0)
############################################
############################################
plot(x,y,col='red',type = 'l',lwd=6,lty=1)
lines(x,z,col='magenta',lwd=7,lty=3)
lines(x,w,col='blue',lwd=7,lty=3)
##############################################