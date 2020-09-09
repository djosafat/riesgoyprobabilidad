fibo <- function(n){
  if(n>2){
    return(fibo(n-1) + fibo(n-2))
  }else{
    return(1)
  }
}
#Ejemplos
fibo(1)
fibo(2)
fibo(3)
fibo(4)
fibo(5)
#Sucesión entera
as.numeric(lapply(1:10,fibo))
plot(as.numeric(lapply(1:10,fibo)),pch=16,cex=3,col=2)
## Tiempo de ejecución
inicio <- proc.time()
as.numeric(lapply(1:30,fibo))
proc.time()-inicio 
########################################

fibo_turbo <- function(n){ #n>2
  x <- c(1,1)
  j <- 2
  while(j<n){
    j <- j + 1
    x[j] <- x[j-1] + x[j-2]
  }
  x
}
#EJEMPLO
inicio <- proc.time()
fibo_turbo(30)
proc.time()-inicio 
############################

En <- function(n){ #n>3
        x <- c(1,1)
        j <- 2
        while(j<n){
                j <- j + 1
                x[j] <- x[j-1] + x[j-2] + 1
        }
        x
}
En(1)
En(6)
En(30)
En(100)[100]
fibo_turbo(100)
