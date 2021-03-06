##############################################
# Sumas finitas y su c�lculo usando vectores #
##############################################

########################
# Ejemplo 1 (divergente)
# 1 + 2 + 3 + ... + n

#M�todo 1
n <- 100
sumandos <- numeric(100)
for(i in 1:n){sumandos[i] <- i}
sum(sumandos)

#M�todo 2
n <- 100
k <- 0
for(i in 1:n){k <- k + i}
k

#M�todo 3
n <- 100
sum(1:n)

#
plot(sumandos,type = 'l',col=2,lwd=5)
#


##############################
# Ejemplo 2 (convergente)
# 1 + 1/4 + 1/9 + ... + 1/n^2

#M�todo 1
n <- 100
sumandos <- numeric(100)
for(i in 1:n){sumandos[i] <- 1/i^2}
sum(sumandos)

#M�todo 2
n <- 100
k <- 0
for(i in 1:n){k <- k + 1/i^2}
k

#M�todo 3
n <- 100
sum(1/(1:n)^2)
#
pi^2/6
#

#
plot(sumandos,type = 'l',col=2,lwd=5)
#

########################################
# Ejemplo 3 (convergente)
# 1 + x + x^2/2! + x^3/3! + ... + x^n/n!

#M�todo 1
n <- 100
x <- 2
sumandos <- numeric(100)
for(i in 1:n){sumandos[i] <- x^i/factorial(i)}
1 + sum(sumandos)

#M�todo 2
n <- 100
k <- 0
for(i in 0:n){k <- k + x^i/factorial(i)}
k

#M�todo 3
n <- 100
sum(x^(0:n)/factorial(0:n))
#
exp(x)
#

#
plot(sumandos,type = 'l',col=2,lwd=5)
#


######################
# Ejemplo 4 (converge o diverge)
# 1 + p + p^2 + p^3 + ... + p^n

#M�todo 1
n <- 100
p <- 0.5
sumandos <- numeric(100)
for(i in 1:n){sumandos[i] <- p^i}
1 + sum(sumandos)

#M�todo 2
n <- 100
k <- 0
for(i in 0:n){k <- k + p^i}
k

#M�todo 3
n <- 100
sum(p^(0:n))
#
1/(1-p) # si |p|<1 y n tiende a infinito
#
(1-p^(n+1))/(1-p)

#
plot(sumandos,type = 'l',col=2,lwd=5)
#
