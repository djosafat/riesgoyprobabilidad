trayectoria_rachas <- function(n,X0,p = 1/2){
        sigue_o_regresa <- rbinom(n,1,p) #1-sigue la racha, 0-vuelve
        trayectoria <- X0
        for(i in 1:n){
                trayectoria[i+1] <- (trayectoria[i]+1)*(sigue_o_regresa[i] == 1)
        }
        trayectoria
}

## Ejemplo

trayectoria_rachas(50,0)

## Ejemplo 2
path <- trayectoria_rachas(50,0,0.65)
plot(0:50,path,type = 'o',main = 'Trayectoria de la cadena de Rachas de éxitos',
     col = 2, pch = 16, las = 2)
abline(h = 0, v = 0)
abline(h = seq_along(path), col = 'lightgray', lty = 2)

## Ejemplo 3

path2 <- trayectoria_rachas(500,0,0.75)
plot(0:500,path2,type = 'l',main = 'Trayectoria de la cadena de Rachas de éxitos',
     col = 2, pch = 16, las = 2)
abline(h = 0, v = 0)
abline(h = seq_along(path2), col = 'lightgray', lty = 2)

## Función para calcular p_{ii}(n)

piin <- function(i,n,p, sim = 1000){
        pii_n <- numeric(sim)
        for(k in 1:sim){
                sigue_o_regresa <- rbinom(n,1,p)
                aux <- i
                for(j in 1:n){
                        aux <- (aux+1)*(sigue_o_regresa[j] == 1)
                }
        pii_n[k] <- 1*(aux == i)
        }
        mean(pii_n)
}

piin(0,7,0.6) # P(X_7 = 0 / X_0 = 0) = p_{0,0}(7) = 1 - p

piin(1,7,0.6) # P(X_7 = 1 / X_0 = 1) = p_{1,1}(7)

piin(2,7,0.6) # P(X_7 = 2 / X_0 = 2) = p_{2,2}(7)

## Función para calcular f_{ii}(n)

fiin <- function(i,n,p, sim = 20000){
        fii_n <- numeric(sim)
        for(k in 1:sim){
                sigue_o_regresa <- rbinom(n,1,p)
                aux <- (i+1)*(sigue_o_regresa[1] == 1)
                j <- 1
                while(aux!=i && j<n){
                        aux <- (aux+1)*(sigue_o_regresa[j+1] == 1)
                        j <- j+1
                }
                if(j < n){exito <- 0}else{exito <- 1*(aux == i)}
                fii_n[k] <- exito
        }
        mean(fii_n)
}

fiin(0,7,0.6) # P(X_7 = 0, X_j != 0; j=1,...,6 / X_0 = 0) = f_{0,0}(7) = 0.0187

fiin(1,7,0.6) # P(X_7 = 1, X_j != 1; j=1,...,6 / X_0 = 1) = f_{1,1}(7)

fiin(2,7,0.6) # P(X_7 = 2, X_j != 2; j=1,...,6 / X_0 = 2) = f_{2,2}(7)

