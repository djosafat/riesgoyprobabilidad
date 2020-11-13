# Algoritmo de ordenación 3
# Insertion sort (idea: ordenar naipes)
orden3 <- function(vector){
    n <- length(vector)
    for(i in 2:n){
        while(i>1){
            if(vector[i] < vector[i-1]){
                vector[(i-1):i] <- vector[i:(i-1)]
                i <- i - 1
            }else{
                i <- 1
            }
        }
    }
    vector
}
##################################################################

orden3(c(2,7,2,3,2,5,4,5,3,4,8,5,3,6,8,7))
sort(c(2,7,2,3,2,5,4,5,3,4,8,5,3,6,8,7))
x <- sample(1:100,10,replace = T)
x
orden3(x)
############################################
############################################


####### Cuerpo de la función ###############
vector <- sample(1:100,10,replace = T)
vector
xx <- sort(vector)
#############################################
#orden3 <- function(vector){
n <- length(vector)
for(i in 2:n){
    #i <- 2
    #i <- i + 1
    while(i>1){
        if(vector[i] < vector[i-1]){
            vector[(i-1):i] <- vector[i:(i-1)]
            i <- i - 1
        }else{
            i <- 1
        }
    }
}
vector
#}

###########################################
#####################################
require(animation)
ani.options(interval = 0.05)
#####################################
vector <- sample(1:100,200,replace = T)
vector
n <- length(vector)
###########################################
par(bg='black')
plot(vector,type = 'h',lwd=1.5,col=rainbow(n))
points(1:n,vector,pch=16,cex=1.5,col=rainbow(n))
###########################################
k <- 0 #contador de arreglos necesarios
for(i in 2:n){
    while(i>1){
        if(vector[i] < vector[i-1]){
            vector[(i-1):i] <- vector[i:(i-1)]
            i <- i - 1
            k <- k + 1
        }else{
            i <- 1
        }
    }
    ###########################################
    plot(c(vector),
         type = 'h',lwd=1.5,col=rainbow(n))
    points(1:n,vector,pch=16,cex=1.5,col=rainbow(n))
    text(n/4,80,paste(k,'arreglos'),
         col = 'yellow',cex=2)
    ###########################################
    ani.pause()
    ###########################################
}
n^2/4
k
vector

