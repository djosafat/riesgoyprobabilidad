muestra <- c(1,3,5,2,3,4,2,3,4,3,2,1,23,43,3,32,2,2)
muestra <- rpois(15,10)
muestra
summary(muestra)
m <- min(muestra)
M <- max(muestra)
sort(muestra)
H <- hist(muestra,
     breaks = seq(m-0.5,M+0.5,1))
#
H[1] # divisiones (con estructura de lista)
H$breaks # estructura de vector
H[2] # frecuencia (con estructura de lista)
H$counts # estructura de vector
H[3]
H[4]
H[5]
H[6]
#################################
H$counts>0
H$counts[H$counts>0]
#
m:M
(m:M)[H$counts>0]
paste('elemento =',(m:M)[H$counts>0],
      'multiplicidad=',H$counts[H$counts>0])
sort(muestra)

#################################
## RESUMEN  #####################
#################################
muestra <- sample(0:10,100,replace = T)
muestra
m <- min(muestra)
M <- max(muestra)
H <- hist(muestra,
          breaks = seq(m-0.5,M+0.5,1))
#
paste('elemento =',(m:M)[H$counts>0],
      'multiplicidad=',H$counts[H$counts>0])
sort(muestra)
#################################

