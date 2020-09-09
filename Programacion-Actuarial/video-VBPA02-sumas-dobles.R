################
# SUMAS DOBLES #
################

# Usando doble for
x <- 0
for(i in 1:3){
    for(j in 1:4){
        x <- x + i*j
    }
}
x
#
# Usando un solo for
x <- 0
for(i in 1:3){
    x <- x + sum(i*1:4)
}
x

# Usando doble for
x <- 0
for(i in 1:4){
    for(j in 1:i){
        x <- x + i*j
    }
}
x

#
# Usando un solo for
x <- 0
for(i in 1:4){
    x <- x + sum(i*1:i)
}
x

#############################
#Doble for
x <- 0
for(i in 1:4){
    for(j in 1:i){
        x <- x + sqrt(i+j)
    }
}
x
#

#Un solo for
x <- 0
for(i in 1:4){
    x <- x + sum(sqrt(i+1:i))
}
x

