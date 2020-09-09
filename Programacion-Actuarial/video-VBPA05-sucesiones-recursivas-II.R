fibo <- function(n){ #n>2
    x <- c(1,1)
    j <- 2
    while(j<n){
        j <- j + 1
        x[j] <- x[j-1] + x[j-2]
    }
    x
}
#EJEMPLO
fibo(19)
#####################################
Recursiva <- function(n,a,x0){# n>k
    k <- length(x0)
    x <- numeric(n)
    x[1:k] <- x0
    j <- k
    while(j<n){
        j <- j + 1
        x[j] <- sum(x[(j-1):(j-k)]*a)
    }
    x
}
#EJEMPLOS
Recursiva(15,c(1,1),c(1,1))
Recursiva(15,c(1,2,3),c(3,7,9))
