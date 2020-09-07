# Estrategia martingala
# para ganarle a un casino.
#############################
#n = número de apuestas
#u = capital inicial
#p = probabilidad de ganar
#############################
apuestas <- function(n,u,p){
        resultados <- numeric(n)
        d <- 1 #d es el monto que deciden apostar
        aux <- u #capital inicial
        x <- 2*rbinom(n,1,p)-1 # resultado de las apuestas
        for(i in 1:n){
                aux <- aux + d*x[i]
                d <- (2*d-1)*(x[i]==-1)+1 #cantidad que se apostará
                resultados[i] <- aux
        }
        par(bg = 'black')
        plot(0:n,c(u,resultados),type = 'l', lwd=5, col='yellow')
        title(paste('Se ganó ',resultados[n]-u,
                    ' después de ', n, 'apuestas'),
              col.main='white',cex.main=2)
        grid(10,lwd = 2,col = 'green')
        lines(0:n,c(u,resultados), lwd=5, col='yellow')
        abline(h=0,v=0,col=7)
        resultados
}
#######
#######EJEMPLOS
#######
apuestas(10,0,0.5)

apuestas(20,0,0.5)

apuestas(30,0,0.5)

apuestas(40,0,0.5)

apuestas(50,0,0.5)

apuestas(60,0,0.5)

apuestas(70,0,0.5)

apuestas(80,0,0.5)

apuestas(90,0,0.5)

apuestas(1000,0,0.5)

apuestas(10,10,0.5)
apuestas(20,10,0.5)
apuestas(30,10,0.5)
apuestas(40,10,0.5)
apuestas(50,10,0.5)
apuestas(60,10,0.5)
apuestas(70,10,0.5)
apuestas(80,10,0.5)
apuestas(90,10,0.5)
set.seed(123)
apuestas(100,10,0.5)

