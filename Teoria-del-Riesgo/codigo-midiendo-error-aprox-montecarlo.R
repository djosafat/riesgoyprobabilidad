#Código de video 22 b
m <- 10000
set.seed(123)
S <- numeric(m)
N <- rgeom(m,1/3)
for(i in 1:m){
    if(N[i]>0){
        S[i] <- sum(rexp(N[i],1/100))
    }else{
        S[i] <- 0
    }
}
FSestimado <- mean(S<=300)  
FSestimado
var(S<=300)   #varianza estimada
#####################
# exacto:
FSexacto <- 1-(1-1/3)*exp(-1)
FSexacto
#####################
epsilon <- 0.01
Diferencia_abs <- abs(FSestimado-FSexacto)
Diferencia_abs < epsilon

######################################################
######################################################
m <- 7200 # En cada experimento m simulaciones
num_exper <- 100 # Número de experimentos
FSestimado <- numeric(num_exper)
exacto <- 1-(1-1/3)*exp(-1)
epsilon <- 0.01
exito <- 0
contador <- 0
repeat{
    contador <- contador + 1
    ###
    S <- numeric(m)
    N <- rgeom(m,1/3)
    for(i in 1:m){
        if(N[i]>0){
            S[i] <- sum(rexp(N[i],1/100))
        }else{
            S[i] <- 0
        }
    }
    FSestimado[contador] <- mean(S<=300)
    Diferencia_abs <- abs(FSestimado[contador] - exacto)
    exito <- exito + 1*(Diferencia_abs < epsilon)
    ###
    if(contador == num_exper){break}
}
FSestimado
