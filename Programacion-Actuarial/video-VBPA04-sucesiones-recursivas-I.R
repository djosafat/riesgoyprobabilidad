###########################
# Sucesiones recursivas I #
###########################
coeficientes <- c(1,1)
X0 <- c(1,1)
k <- length(X0)
n <- 10

# Método 1
x <- numeric(n+1)
x[1:k] <- X0 
for(i in (k+1):(n+1)){ # n debe ser mayor a k
    aux <- x[(i-1):(i-k)]*coeficientes # sumandos
    x[i] <- sum(aux) #suma final
}
#x
x[n+1]
plot(0:n,x,pch=16)
#



# Método 2
x <- X0
for(j in k:n){
    sumandos <- rev(x)*coeficientes
    x <- c(x[-1],sum(sumandos))
}
x[k]
#



# Método 3
k_ant <- rev(X0)
x <- sum(k_ant*coeficientes)
for(j in (k+1):n){   #n>=k
    k_ant <- c(x,k_ant[-k])
    x <- sum(k_ant*coeficientes)
}
x
