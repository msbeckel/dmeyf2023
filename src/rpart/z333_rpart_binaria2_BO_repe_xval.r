# Optimizacion Bayesiana de hiperparametros de  rpart
# 10-repeated 5-fold Cross Validation
# trabaja con la clase_binaria2
#   POS={BAJA+1, BAJA+2}   NEG={CONTINUA}

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection
fe = TRUE
require("data.table")
require("rlist")

require("rpart")
require("parallel")

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


# Defino la  Optimizacion Bayesiana
PARAM <- list()

PARAM$home <- "/home/maxibeckel/maestria_datos/dmeyf/dmeyf2023/"

PARAM$experimento <- "HT3330"

# Aqui van las 10 semillas que hacen el 10-repeated
#  si se pone una sola semilla, se esta haciendo solo 5-fold xval
PARAM$semilla_azar <- c(
  594697, 594709, 594721, 594739, 594749,
  753587, 247759, 253369, 955127, 800519
)

# folds del cross validation
PARAM$xval_folds <- 5

# cantidad de iteraciones de la Optimizacion Bayesiana
PARAM$BO_iter <- 300

# la letra L al final de 1L significa ENTERO
PARAM$hs <- makeParamSet(
  makeNumericParam("cp", lower = -1, upper = 0.1),
  makeIntegerParam("minsplit", lower = 1L, upper = 8000L),
  makeIntegerParam("minbucket", lower = 1L, upper = 4000L),
  makeIntegerParam("maxdepth", lower = 3L, upper = 20L),
  makeIntegerParam("corte", lower = 5000L, upper = 15000L),
  forbidden = quote(minbucket > 0.5 * minsplit)
)
# minbuket NO PUEDE ser mayor que la mitad de minsplit


# este valor debe ser 1 si se utiliza Windows
PARAM$cores <- 5

#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(reg, arch = NA, folder = "./work/", ext = ".txt",
                    verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)

  # Escribo los titulos
  if (!file.exists(archivo)) {
    linea <- paste0(
      "fecha\t",
      paste(list.names(reg), collapse = "\t"), "\n"
    )

    cat(linea, file = archivo)
  }

  # la fecha y hora
  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
    gsub(", ", "\t", toString(reg)), "\n"
  )

  # grabo al archivo
  cat(linea, file = archivo, append = TRUE)

  # imprimo por pantalla
  if (verbose) cat(linea)
}
#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#   que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30),
#  agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30
# particionar( data=dataset, division=c(1,1,1,1,1),
#  agrupa=clase_ternaria, seed=semilla)   divide el dataset en 5 particiones

particionar <- function(data, division, agrupa = "", campo = "fold",
                        start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(
    function(x, y) {
      rep(y, x)
    }, division,
    seq(from = start, length.out = length(division))
  ))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------
# fold_test  tiene el numero de fold que voy a usar para testear,
#  entreno en el resto de los folds
# param tiene los hiperparametros del arbol

ArbolSimple <- function(fold_test, data, param, qfolds) {
  # genero el modelo
  # entreno en todo MENOS el fold_test que uso para testing
  modelo <- rpart("clase_virtual ~ . - clase_ternaria",
    data = data[fold != fold_test, ],
    xval = 0,
    control = param
  )

  # aplico el modelo a los datos de testing
  # aplico el modelo sobre los datos de testing
  # quiero que me devuelva probabilidades
  prediccion <- predict(modelo,
    data[fold == fold_test, ],
    type = "prob"
  )

  # esta es la probabilidad de baja
  prob_pos <- prediccion[, "POS"]

  tb_final <- copy(data[fold == fold_test, list(clase_ternaria)])
  tb_final[, prob := prob_pos]
  setorder(tb_final, -prob)

  # calculo la ganancia, los PARAM$corte de mayor probabilidad
  ganancia_testing <- tb_final[
    1:round(param$corte / qfolds),
    sum(ifelse(clase_ternaria == "BAJA+2",
      273000, -7000
    ))
  ]

  rm(tb_final)
  # esta es la ganancia sobre el fold de testing, NO esta normalizada
  return(ganancia_testing)
}
#------------------------------------------------------------------------------

ArbolesCrossValidation <- function(data, param, qfolds, pagrupa, semilla) {
  # generalmente  c(1, 1, 1, 1, 1 )  cinco unos
  divi <- rep(1, qfolds)

  # particiono en dataset en folds
  particionar(data, divi, seed = semilla, agrupa = pagrupa)

  ganancias <- mcmapply(ArbolSimple,
    seq(qfolds), # 1 2 3 4 5
    MoreArgs = list(data, param, qfolds),
    SIMPLIFY = FALSE,
    mc.cores = PARAM$cores
  )

  data[, fold := NULL]

  ganancia_xval <- sum(unlist(ganancias))

  return(ganancia_xval)
}
#------------------------------------------------------------------------------
# esta funcion solo puede recibir los parametros que se estan optimizando
# el resto de los parametros, lamentablemente se pasan como variables globales

EstimarGanancia <- function(x) {
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1

  # param= x los hiperparametros del arbol
  # qfolds= PARAM$xval_folds  la cantidad de folds
  gans <- c()
  for (semilla in PARAM$semilla_azar)
  {
    ganancia <- ArbolesCrossValidation(dataset,
      param = x,
      qfolds = PARAM$xval_folds,
      pagrupa = "clase_ternaria",
      semilla = semilla
    )

    gans <- c(gans, ganancia)
  }

  cat(gans, "\n")
  # logueo
  xx <- x

  xx$xval_repeats <- length(PARAM$semilla_azar)
  xx$xval_folds <- PARAM$xval_folds

  xx$ganancia <- mean(gans)
  xx$iteracion <- GLOBAL_iteracion
  loguear(xx, arch = archivo_log)

  return(mean(gans))
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Establezco el Working Directory inicial
setwd(PARAM$home)

# cargo los datos
dataset <- fread("./data/competencia_01.csv")

#feature engineering
if(fe){
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

}

# entreno en 202103
dataset <- dataset[foto_mes == 202103]

dataset[, clase_virtual := ifelse(clase_ternaria == "CONTINUA", "NEG", "POS")]

# creo la carpeta donde va el experimento
#  HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)

dir.create(paste0("./exp/", PARAM$experimento, "/"),
  showWarnings = FALSE
)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


archivo_log <- "BO_log.txt"
archivo_BO <- "bayesiana.RDATA"

# leo si ya existe el log
#  para retomar en caso que se se corte el programa
GLOBAL_iteracion <- 0

if (file.exists(archivo_log)) {
  tabla_log <- fread(archivo_log)
  GLOBAL_iteracion <- nrow(tabla_log)
}



# Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar <- EstimarGanancia

configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,
#  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
# minimize= FALSE estoy Maximizando la ganancia
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar,
  minimize = FALSE,
  noisy = TRUE,
  par.set = PARAM$hs,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl(
  save.on.disk.at.time = 600,
  save.file.path = archivo_BO
)

ctrl <- setMBOControlTermination(ctrl, iters = PARAM$BO_iter)
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

surr.km <- makeLearner("regr.km",
  predict.type = "se",
  covtype = "matern3_2", control = list(trace = TRUE)
)

# inicio la optimizacion bayesiana
if (!file.exists(archivo_BO)) {
  run <- mbo(
    fun = obj.fun,
    learner = surr.km,
    control = ctrl
  )
} else {
  run <- mboContinue(archivo_BO)
}
# retomo en caso que ya exista

##
#res = read.table("/home/maxibeckel/maestria_datos/dmeyf/dmeyf2023/exp/HT3330/BO_log.txt",  row.names=NULL)
