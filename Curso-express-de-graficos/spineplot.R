#La función spineplot()
#Relación entre dos variables ordinales
set.seed(2020) #semilla para reproducibilidad
nivel_de_ingresos <- sample(1:5,300,
                            replace = T,prob = c(0.5,0.3,0.1,0.07,0.03)) 
nivel_de_ingresos
###############################################
nivel_de_estudios <- sample(1:6,300,
                            r = T,p = c(0.02,0.15,0.43,0.33,0.05,.02))
nivel_de_estudios
###############################################
spineplot(factor(nivel_de_ingresos)~factor(nivel_de_estudios))
spineplot(factor(nivel_de_ingresos)~factor(nivel_de_estudios),col = 2:6)
(spineplot(factor(nivel_de_ingresos)~factor(nivel_de_estudios),col = 2:6))
###############################################
###############################################
###############################################

set.seed(2021)
nivel_de_ingresos <- sample(1:5,300,
                            replace = T,prob = c(0.5,0.3,0.1,0.07,0.03)) 
nivel_de_ingresos <- factor(nivel_de_ingresos,
                labels=c('0-1sm','1-5sm','5-10sm','10-20sm','>20sm'))
###############################################
nivel_de_estudios <- sample(1:6,300,
                            r = T,p = c(0.02,0.15,0.43,0.33,0.05,.02))
nivel_de_estudios <- factor(nivel_de_estudios,
                            labels = c('Pri','Sec','Bach','Lic','Mas','Doc'))
###############################################
spineplot(nivel_de_ingresos~nivel_de_estudios)
spineplot(nivel_de_ingresos~nivel_de_estudios,col = 2:6)
(spineplot(nivel_de_ingresos~nivel_de_estudios,col = 2:6))


