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
fe=FALSE

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/home/maxibeckel/maestria_datos/dmeyf/dmeyf2023") # Establezco el Working Directory

##
selected_model = function(BO_log, iter){
        param = list()
        param$cp <- -1
        param$minsplit <- BO_log[iter, minsplit]
        param$minbucket <- BO_log[iter, minbucket]
        param$maxdepth <- BO_log[iter, maxdepth]

        return(param)
}

##

# cargo el dataset
dataset <- fread("./data/competencia_01.csv")
dataset[foto_mes == 202105, clase_ternaria := NA]

# defino la clase_binaria2
dataset[ , clase_binaria := ifelse( clase_ternaria=="CONTINUA", "NEG", "POS" ) ]

# Feature Engineering
if(fe){
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
}

# Slice data
dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo

#Subsampleo
#set.seed(594697)
#dtrain <- dtrain[, .SD[sample(.N, nrow(dtrain)/2)]]

#Oversampling
pesos <- copy(dtrain[, ifelse(clase_ternaria == "CONTINUA", 1.0, 100.0)])

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_binaria a partir de el resto de las variables

if(FALSE){
        res   <- fread("/home/maxibeckel/maestria_datos/dmeyf/dmeyf2023/exp/HT4010/BO_log.txt")
        param <- selected_model(BO_log = res, iter = 7)
} else {
        param <- list("cp" = -1, "minsplit" = 934, "minbucket" = 209, = "maxdepth" = 12)
}

modelo <- rpart(
        formula = "clase_binaria ~ . - clase_ternaria",
        data = dtrain,
        xval = 0,
        control = param,
        weights = pesos # aqui se hace oversampling
) # profundidad maxima del arbol

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


# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "POS"]]

# solo le envio estimulo a los registros
#  que se encuentren por arriba de corte
corte = 9500
estimulo = order(dapply$prob_baja2, decreasing=T)[1:corte]
dapply$Predicted = 0
dapply[estimulo, 'Predicted'] = 1

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2002/K201_003.csv",
        sep = ","
)


