# Contando multiplicidades usando un "while"

##############################
# RESUMEN DEL VIDEO ANTERIOR #
##############################
muestra <- sample(1:5,10,replace = T)
muestra
m <- min(muestra)
M <- max(muestra)
H <- hist(muestra,
          breaks = seq(m-0.5,M+0.5,1))
#
paste('elemento =',(m:M)[H$counts>0],
      'multiplicidad=',H$counts[H$counts>0])
#################################
#################################

muestra <- sample(1:5,10,replace = T)
x <- muestra
x
i <- 0
elementos <- 0
multiplicidad <- 0
while(length(x)>0){
        i <- i + 1
        elementos[i] <- x[1]
        aux <- which(x==x[1])
        multiplicidad[i] <- length(aux)
        x <- x[-aux]
}
###
sort(paste('elemento =',elementos,
      'multiplicidad=',multiplicidad))
cbind(elementos,multiplicidad)
matrix(c(elementos, multiplicidad),
       length(elementos), ncol = 2)
#############################################
#############################################
#############################################

Contadora <- function(x){
        i <- 0
        elementos <- 0
        multiplicidad <- 0
        while(length(x)>0){
                i <- i + 1
                elementos[i] <- x[1]
                aux <- which(x==x[1])
                multiplicidad[i] <- length(aux)
                x <- x[-aux]
        }
        #paste('elemento =',elementos,
        #      'multiplicidad=',multiplicidad)
        cbind(elementos,multiplicidad)
        #matrix(c(elementos, multiplicidad),length(elementos),1,2)
}
#Ejemplo
muestra <- rbinom(5,5,0.5)/13
muestra
Contadora(muestra)
#

#Ejemplo
muestra <- rbinom(10,5,0.5)/13
muestra
Contadora(muestra)
#

#Ejemplo
muestra <- rbinom(20,5,0.5)/13
muestra
Contadora(muestra)
#