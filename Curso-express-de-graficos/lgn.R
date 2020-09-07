lgnzoom <- function(n){
    k <- floor(n/4)
    z <- k:n
    x <- runif(n)
    y <- cumsum(x)/1:n
    par(mfrow=c(2,1),oma=c(0,0,0,0),mar=c(3,3,1,2),
        bg='lightyellow',fg='blue')
    plot(1:n,y,type="n",lwd=7,xlab = '',ylab = '',
         axes = F)
    grid(5,lwd=2)
    abline(h=1/2,lty = 1, lwd = 5, col = "red",cex=2)
    abline(h=c(1/2+1.96/sqrt(12*n),1/2-1.96/sqrt(12*n)),
           lty = 2, lwd = 3, col = 'magenta')
    lines(1:n,y,lwd=7)
    axis(1,round(seq(1,n,length=10),3),
         col = 1,col.axis=1,lwd=3)
    axis(2,round(seq(min(y),max(y),length=5),3),
         col = 1,col.axis=1,lwd=3)
    ####################################
    plot(z,y[k:n],type="n",lwd=7,xlab = '',ylab = '',
         axes = F)
    grid(5,col = 'gray',lwd=2)
    abline(h=1/2,lty = 1, lwd = 5, col = "red",cex=2)
    abline(h=c(1/2+1.96/sqrt(12*n),1/2-1.96/sqrt(12*n)),
           lty = 2, lwd = 3, col = 'magenta')
    lines(z,y[k:n],lwd=7)
    axis(1,seq(k,n,by=floor((n-k)/5)),
         col = 1,col.axis=1,lwd=3)
    axis(2,round(seq(min(y),max(y),length=5),3),
         col = 1,col.axis=1,lwd=3)
    par(mfrow=c(1,1),fg='white')
}
lgnzoom(10)
lgnzoom(50)
lgnzoom(100)
lgnzoom(1000)
lgnzoom(10000)
