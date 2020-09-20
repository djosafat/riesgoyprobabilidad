#Calcular la potencia de una matriz

#n es el valor de la potencia
#A debe ser una matriz cuadrada
powMat <- function(A,n){
    if(dim(A)[1]==dim(A)[2] && n>1){
        aux <- A
        j <- 1
        repeat{
            A <- A%*%aux
            j <- j + 1
            if(j==n){break}
        }
        A
    }else if(dim(A)[1]==dim(A)[2] && n==1){
        A  #si n == 1 este caso se aplica
    }else{"La matriz no es cuadrada!"}
}

#Ejemplo
C1 <- sample(0:9,4,replace = T)
C2 <- sample(0:9,4,replace = T)
C3 <- sample(0:9,4,replace = T)
C4 <- sample(0:9,4,replace = T)

M <- matrix(c(C1,C2,C3,C4),4,4)
M

powMat(M,7)
M%*%M%*%M%*%M%*%M%*%M%*%M #lo mismo
