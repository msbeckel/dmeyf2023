# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot
rm(list = ls())
gc()

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
library(caret)
options(bitmapType = "cairo")
require("httpgd")
hgd()

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/home/maxibeckel/maestria_datos/dmeyf/dmeyf2023") # Establezco el Working Directory


# cargo el dataset
dataset <- fread("./data/competencia_01.csv")

# Feature Engineering
dataset[, antiguedad_edad := (cliente_antiguedad / 12) / cliente_edad]
dataset[, nivel_mrentabilidad := ifelse(cliente_antiguedad > 11, as.numeric(mrentabilidad > (mrentabilidad_annual / 12)), as.numeric(mrentabilidad > (mrentabilidad_annual / cliente_antiguedad)))]
dataset[, monto_transaccion := mautoservicio / ctarjeta_debito_transacciones]
dataset[, monto_pp := ifelse(cprestamos_personales > 0, mprestamos_personales / cprestamos_personales, 0)]
dataset[, monto_ppr := ifelse(cprestamos_prendarios > 0, mprestamos_prendarios / cprestamos_prendarios, 0)]
dataset[, monto_ph := ifelse(cprestamos_hipotecarios > 0, mprestamos_hipotecarios / cprestamos_hipotecarios, 0)]
dataset[, monto_pf := ifelse(cplazo_fijo > 0, (mplazo_fijo_dolares + mplazo_fijo_pesos) / cplazo_fijo, 0)]
dataset[, monto_i1 := ifelse(cinversion1 > 0, (minversion1_pesos + minversion1_dolares) / cinversion1, 0)]
dataset[, monto_i2 := ifelse(cinversion2 > 0, (minversion2) / cinversion1, 0)]
dataset[, cseguro := cseguro_vida + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales]
dataset[, ifpayroll := ifelse(cpayroll_trx > 0, 1, 0)]
dataset[, monto_da := ifelse(ccuenta_debitos_automaticos > 0, mcuenta_debitos_automaticos / ccuenta_debitos_automaticos, 0)]
dataset[, descuentos_comisiones := (mcajeros_propios_descuentos + mtarjeta_visa_descuentos + mtarjeta_master_descuentos) / (mcomisiones_mantenimiento + mcomisiones_otras)]
dataset[, bin_Master_mlimitecompra := as.numeric(arules::discretize(dataset$Master_mlimitecompra, method = "cluster", breaks = 5, labels = c(1, 2, 3, 4, 5)))]
dataset[, bin_Visa_mlimitecompra := as.numeric(arules::discretize(dataset$Visa_mlimitecompra, method = "cluster", breaks = 5, labels = c(1, 2, 3, 4, 5)))]


cols <- colnames(dataset)[grepl(pattern = "^m|^(Visa|Master)_m", colnames(dataset))]
dataset[, (cols) := lapply(.SD, function(x){frank(x, na.last= "keep", ties.method = "dense")}), .SDcols = cols, by = foto_mes]

# Slice data
dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo

if (FALSE) {
        # Near zero variance variables
        target <- which(colnames(dtrain) == "clase_ternaria")
        nzv <- nearZeroVar(dtrain, saveMetrics = TRUE)
        nzv[target, "nzv"] <- FALSE # fuerzo a que la variable a predecir no se nzv
        dtrain <- dtrain[, !nzv$nzv, with = FALSE]
        dapply <- dapply[, !nzv$nzv, with = FALSE]

        # Transform variables
        target <- which(colnames(dtrain) == "clase_ternaria")
        tmp <- as.data.frame(dtrain)
        tData <- preProcess(tmp[, c(-1, -target)], c("medianImpute"))
        # dtrain
        tmp[, c(-1, -target)] <- predict(tData, tmp[, c(-1, -target)])
        dtrain <- setDT(tmp)
        # dtest
        tmp <- as.data.frame(dapply)
        tmp[, c(-1, -target)] <- predict(tData, tmp[, c(-1, -target)])
        dapply <- setDT(tmp)
}
# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables

modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -1, # esto significa no limitar la complejidad de los splits
        minsplit = 400, # minima cantidad de registros para que se haga el split
        minbucket = 1, # tamaño minimo de una hoja
        maxdepth = 12
) # profundidad maxima del arbol

modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.8055, # esto significa no limitar la complejidad de los splits
        minsplit = 1407, # minima cantidad de registros para que se haga el split
        minbucket = 421, # tamaño minimo de una hoja
        maxdepth = 7
)

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
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_011.csv",
        sep = ","
)


#---------------------------------------------------------------#
#Entrenamiento a partir de clase binaria

# cargo el dataset
dataset <- fread("./data/competencia_01.csv")

dataset[, clase_virtual := ifelse(clase_ternaria == "CONTINUA", "NEG", "POS")]
dataset[,clase_ternaria := NULL]

# Slice data
dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo


modelo <- rpart(
        formula = "clase_virtual ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.4264, # esto significa no limitar la complejidad de los splits
        minsplit = 1966, # minima cantidad de registros para que se haga el split
        minbucket = 380, # tamaño minimo de una hoja
        maxdepth = 7
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
dapply[, prob_baja2 := prediccion[, "POS"]]

# solo le envio estimulo a los registros
#  que se encuentren por arriba de corte
corte = 8789
estimulo = order(dapply$prob_baja2, decreasing=T)[1:corte]
dapply$Predicted = 0
dapply[estimulo, 'Predicted'] = 1

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_013.csv",
        sep = ","
)
