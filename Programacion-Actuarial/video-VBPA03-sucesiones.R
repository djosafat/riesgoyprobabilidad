##############
# Sucesiones #
##############

# Ejemplo 1
a_n <- function(n){n}
a_n(4)
a_n(14)
m <- 10
plot(1:m,a_n(1:m),pch=16,cex=2)
grid()
#


# Ejemplo 2
a_n <- function(n){1/n}
a_n(4)
a_n(14)
plot(1:10,a_n(1:10),pch=16,cex=2)
grid()
a_n(1:30)
a_n(1000)
plot(1:1000,a_n(1:1000),pch=16,cex=2)
grid()
#


# Ejemplo 3
a_n <- function(n){cos(n*pi)}
a_n(4)
a_n(5)
a_n(14)
a_n(15)
plot(1:10,a_n(1:10),pch=16,cex=2)
grid()
a_n(1:30)
a_n(1000)
plot(1:1000,a_n(1:1000),pch=16,cex=2)
grid()
#


# Ejemplo 4
a_n <- function(n){sin(n*pi/2)/n}
a_n(4)
a_n(5)
a_n(14)
a_n(15)
plot(1:10,a_n(1:10),pch=16,cex=2)
grid()
a_n(1:10)
a_n(10000)
plot(1:40,a_n(1:40),pch=16,cex=2)
plot(1:40,a_n(1:40),type = 'l',lwd=5)
grid()
#


# Ejemplo 5
a_n <- function(n){(2/3)^n}
aa_n <- function(n){
    aux <- 2/3
    for(i in 1:(n-1)){aux <- aux*(2/3)}
    aux
}
a_n(4)
aa_n(4)
a_n(5)
aa_n(5)
a_n(14)
aa_n(14)
plot(1:10,a_n(1:10),pch=16,cex=2)
grid()
a_n(1:10)
a_n(1000)
plot(1:40,a_n(1:40),pch=16,cex=2)
plot(1:40,a_n(1:40),type = 'l',lwd=5)
grid()
