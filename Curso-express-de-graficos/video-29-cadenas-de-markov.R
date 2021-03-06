#Cadenas de Markov 
#Simulaci�n de trayectoria de una 
#cadena de Markov cuando conocemos 
#su matriz de probabilidades de transici�n.
#Caso cuando el espacio de estados es finito.
## Los argumentos ser�n
## n <- n�mero de pasos
## X0 <- estado inicial
## P <- matriz de probabilidades de transici�n
##      de dimensi�n finita (matriz estoc�stica finita)
## Eligiremos a qu� estado salta la cadena 
## con la funci�n sample, 
## m�s info: consultar '?sample'

simulaCadena <- function(n,X0,P){
    n <- 30; X0 <- 2
    dim <- length(P[1,]) # length = dimensi�n del vector
    Xn <- numeric((n+1)) # declaro un vector en ceros
    Xn[1] <- X0
    for(i in 2:(n+1)){
        aux <- Xn[i-1]
        Xn[i] <- sample(1:dim,1,T,P[aux,])
    }
    plot((0:n),Xn,type = "l",
         pch = 16,col = "red",lwd=5)
    abline(h = 0, v=0)
    grid(10)
    Xn
}

# Ejemplo 1.

p1 <- c(1/2,1/2,0)
p2 <- c(1/2,1/3,1/6)
p3 <- c(0,1,0)
P <- rbind(p1,p2,p3)
P
par(mfrow=c(3,3),bg='lightyellow',
    oma=c(0,0,0,0),mar=c(2,2,1,1))
simulaCadena(10,1,P)
simulaCadena(10,2,P)
simulaCadena(10,3,P)
simulaCadena(50,1,P)
simulaCadena(50,2,P)
simulaCadena(50,3,P)
simulaCadena(100,1,P)
simulaCadena(100,2,P)
simulaCadena(100,3,P)
par(mfrow=c(3,1))
X1 <-simulaCadena(1000,1,P)
X2 <-simulaCadena(1000,2,P)
X3 <-simulaCadena(1000,3,P)

c(mean(X1==1),mean(X1==2),mean(X1==3))
c(mean(X2==1),mean(X2==2),mean(X2==3))
c(mean(X3==1),mean(X3==2),mean(X3==3))

P_10 <- P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P
P_10
P_10[1,]


#Ejemplo 2
par(mfrow=c(1,1))
P <- matrix(0,6,6)
for(i in 1:6){
    p <- rnbinom(6,2,0.5)
    P[i,] <- p/sum(p)
}
P
round(P,3)
simulaCadena(20,4,P)
simulaCadena(200,4,P)
simulaCadena(2000,4,P)
X <- simulaCadena(10000,4,P)
estac <- c(mean(X==1),mean(X==2),mean(X==3),
           mean(X==4),mean(X==5),mean(X==6))
round(estac,3)
####################
P_10 <- P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P
round(P_10,3)
#####################

