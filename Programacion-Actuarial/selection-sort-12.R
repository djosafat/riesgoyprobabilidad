# Algoritmo de ordenación 2
# Selection sort
orden2 <- function(vector){
    n <- length(vector)
    vector_ordenado <- numeric(n-1)
    for(i in 1:(n-1)){
        aux <- vector[1]
        k <- 1
        # Búsqueda del mínimo
        for(j in 2:(n-i+1)){
            if(aux>vector[j]){
                aux <- vector[j] #nuevo mínimo
                k <- j #índice del nuevo mínimo
            }
        }
        #######################
        vector <- vector[-k] #se quita el mínimo
        vector_ordenado[i] <-  aux 
        #se almacena aux como el elemento i ordenado
    }
    c(vector_ordenado,vector)
}
##################################################################
orden2(c(2,7,2,3,2,5,4,5,3,4,8,5,3,6,8,7))
sort(c(2,7,2,3,2,5,4,5,3,4,8,5,3,6,8,7))
x <- sample(1:100,10,replace = T)
x
orden2(x)
############################################
############################################


####### Cuerpo de la función ###############
vector <- sample(1:100,10,replace = T)
vector
xx <- sort(vector)
#############################################
#orden2 <- function(vector){
    n <- length(vector)
    vector_ordenado <- numeric(n-1)
    for(i in 1:(n-1)){
        #
        i <- 1        # no incluir en la función
        i <- i + 1    # no incluir en la función
        #
        aux <- vector[1] #mínimo inicial
        k <- 1 #índice del mínimo
        # Búsqueda del mínimo
        for(j in 2:(n-i+1)){
            if(aux>vector[j]){
                aux <- vector[j] #nuevo mínimo
                k <- j #índice del nuevo mínimo
            }
        }
        #######################
        vector <- vector[-k] #se quita el mínimo
        vector_ordenado[i] <-  aux 
        #se almacena aux como el elemento i ordenado
    }
    c(vector_ordenado,vector)
#}

###########################################
#####################################
require(animation)
ani.options(interval = 0.05)
#####################################
vector <- sample(1:100,500,replace = T)
vector
n <- length(vector)
###########################################
plot(vector,type = 'p',cex=1.5)
points(1:n,vector,pch=16,cex=1.5,col=2)
###########################################
vector_ordenado <- numeric(n-1)
for(i in 1:(n-1)){
    aux <- vector[1] #mínimo inicial
    k <- 1 #índice del mínimo
    # Búsqueda del mínimo
    for(j in 2:(n-i+1)){
        if(aux>vector[j]){
            aux <- vector[j] #nuevo mínimo
            k <- j #índice del nuevo mínimo
        }
    }
    #######################
    vector <- vector[-k] #se quita el mínimo
    vector_ordenado[i] <-  aux 
    #se almacena aux como el elemento i ordenado
    ###########################################
    plot(c(vector_ordenado[1:i],vector),
         type = 'p',cex=1.5)
    points(1:n,c(vector_ordenado[1:i],vector)
           ,pch=16,cex=1.5,col=2)
    ###########################################
    ani.pause()
    ###########################################
}
c(vector_ordenado,vector)


