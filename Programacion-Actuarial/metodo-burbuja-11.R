# Algoritmo de ordenación 1
# Método de Burbuja
orden1 <- function(vector){
    n <- length(vector)
    algo <- 0 #arranque de while
    while(algo!=1){
        vector_old <- vector
        for(i in 1:(n-1)){
            if(vector[i]>vector[i+1]){
                vector[i:(i+1)] <- vector[(i+1):i]
            }
        }
        algo <- prod(vector_old==vector) #producto de indicadoras
    }
    vector
}
##################################################################
orden1(c(2,7,2,3,2,5,4,5,3,4,8,5,3,6,8,7))
sort(c(2,7,2,3,2,5,4,5,3,4,8,5,3,6,8,7))
x <- sample(1:100,10,replace = T)
x
orden1(x)
############################################
############################################


####### Cuerpo de la función ###############
vector <- sample(1:100,10,replace = T)
vector
#############################################
#orden1 <- function(vector){
    n <- length(vector)
#    algo <- 0 #arranque de while
#    while(algo!=1){
        vector_old <- vector
        for(i in 1:(n-1)){
            if(vector[i]>vector[i+1]){
                vector[i:(i+1)] <- vector[(i+1):i]
            }
        }
        algo <- prod(vector_old==vector) #producto de indicadoras
#    }
    vector
#}

###########################################
#####################################
require(animation)
ani.options(interval = 0.05)
#####################################
vector <- sample(1:100,50,replace = T)
vector
n <- length(vector)
###########################################
plot(vector,type = 'p',lwd=6,cex=1.5)
points(1:n,vector,pch=16,cex=1.5,col=2)
###########################################
algo <- 0 #arranque de while
while(algo!=1){
    vector_old <- vector
    for(i in 1:(n-1)){
        if(vector[i]>vector[i+1]){
            vector[i:(i+1)] <- vector[(i+1):i]
        }
    }
    algo <- prod(vector_old==vector) #producto de indicadoras
    ###########################################
    plot(vector,type = 'p',lwd=6,cex=1.5)
    points(1:n,vector,pch=16,cex=1.5,col=2)
    ###########################################
    ani.pause()
    ###########################################
}
vector
    
