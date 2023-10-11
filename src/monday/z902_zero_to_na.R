# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


require("data.table")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

# copio si hace falta el dataset

setwd("~/buckets/b1/")

#import dataset
dataset <- fread("./datasets/competencia_02.csv.gz")

#frecuencia de 0's por variables/foto_mes
tmp = dataset[,lapply(.SD, function(x){ ( sum( x == 0 ) / length(x) ) }),by=foto_mes]

#tabla con los casos con frecuencia = 1
allzero <- which(tmp == 1, arr.ind = T)
allzero <- data.frame(mes = tmp[allzero[,1],'foto_mes'], 
                    variable = colnames(tmp)[allzero[,2]])

# 0 -> NA
for (i in 1:nrow(allzero)){
    dataset[foto_mes == allzero[i, 1], allzero[i, 2]] <- NA
}

#export dataset
fwrite(dataset, "./datasets/competencia_02_c.csv.gz")
