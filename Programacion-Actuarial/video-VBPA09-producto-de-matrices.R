# Uso de if else y for para programar
# una multiplicación de matrices

x <- matrix(c(5,4,3,2),nrow = 2,ncol =  2)

y <- matrix(c(1,2,3,4),nrow = 2,ncol =  2)

dim(x)
dim(y)

x%m%y # será nuestra nueva función

x%*%y # función ya programada en R

#producto de matrices
"%m%" <- function(x,y){
        n <- dim(x)[1]
        if(dim(x)[2]==n && dim(y)[1]==n && dim(y)[2]==n){
                res <- matrix(0,n,n)
                for(i in 1:n){
                        for(j in 1:n){
                                res[i,j] <- sum(x[i,]*y[,j])
                        }
                }
                res
        }else{"las dimensiones de x y y son distintas"}
}
##
x <- matrix(c(5,3,2,5,4,3,2,4,5,2,1,2,3,4,3,2),4,4)
y <- matrix(sample(0:10,16,replace = T),4,4)
x
y
x%m%y
#########
x%*%y
