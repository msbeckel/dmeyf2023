# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
library(caret)
require("httpgd")
hgd()

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/home/maxibeckel/maestria_datos/dmeyf/dmeyf2023") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./data/competencia_01.csv")

dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo

# exploratory analysis

# preprocess ----
znzv = TRUE
trans = TRUE
# Zero and Near-Zero-Variance Predictors
if(znzv){
        nzv <- nearZeroVar(dtrain, saveMetrics= TRUE)
        nzv[nrow(nzv),'nzv'] = FALSE #fuerzo a que la variable a predecir no se nzv
        dtrain <- dtrain[, !nzv$nzv, with=FALSE] 
        dapply <- dapply[, !nzv$nzv, with=FALSE]
}

#Data imputation/transformacion
if(trans){
        tData <- preProcess(dtrain[, -82, with=FALSE], c("BoxCox", "center", "scale", "knnImpute"))
        dtrain[, -82, with=FALSE] = predict(tData, dtrain[, -82, with=FALSE])
        dapply[, -82, with=FALSE] = predict(tData, dapply[, -82, with=FALSE])
}

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.3, # esto significa no limitar la complejidad de los splits
        minsplit = 0, # minima cantidad de registros para que se haga el split
        minbucket = 1, # tamaño minimo de una hoja
        maxdepth = 3
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)

# feature importance
importance <- modelo$variable.importance
importance <- round(100 * importance / sum(importance), 1)
importance[importance >= 1]

# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2002")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2002/K101_001.csv",
        sep = ","
)
