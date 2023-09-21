# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")
options(bitmapType = "cairo")
require(caret)
PARAM <- list()
# reemplazar por las propias semillas
semillas <- c(594697, 594709, 594721, 594739, 594749)

PARAM$semillas <- semillas

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos, preprocess = TRUE) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_ternaria", seed = semilla)

  if (preprocess) {
    dtrain <- dataset[fold == 1]
    dtest <- dataset[fold == 2]

    # Near zero variance variables
    target <- which(colnames(dtrain) == "clase_ternaria")
    nzv <- nearZeroVar(dtrain, saveMetrics = TRUE)
    nzv[target, "nzv"] <- FALSE # fuerzo a que la variable a predecir no se nzv
    dtrain <- dtrain[, !nzv$nzv, with = FALSE]
    dtest <- dtest[, !nzv$nzv, with = FALSE]

    # Transform variables
    target <- which(colnames(dtrain) == "clase_ternaria")
    tmp <- as.data.frame(dtrain)
    tData <- preProcess(tmp[, c(-1, -target)], c("center", "scale", "medianImpute", "pca"))
    # dtrain
    tmp[, c(-1, -target)] <- predict(tData, tmp[, c(-1, -target)])
    dtrain <- setDT(tmp)
    # dtest
    tmp <- as.data.frame(dtest)
    tmp[, c(-1, -target)] <- predict(tData, tmp[, c(-1, -target)])
    dtest <- setDT(tmp)
    rm(tmp)
    gc()

    # Final data.table
    dtrain[, fold := 1]
    dtest[, fold := 2]
    dataset <- rbindlist(list(dtrain, dtest))
    rm(dtrain, dtest)
    gc()
  }

  # genero el modelo
  # quiero predecir clase_ternaria a partir del resto
  modelo <- rpart("clase_ternaria ~ .",
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con TRES columnas,
  #  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
      ifelse(clase_ternaria == "BAJA+2", 273000, -7000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3

  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  ksemillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
    semillas, # paso el vector de semillas
    MoreArgs = list(param_basicos), # aqui paso el segundo parametro
    SIMPLIFY = FALSE,
    mc.cores = 5
  ) # se puede subir a 5 si posee Linux o Mac OS

  ganancia_promedio <- mean(unlist(ganancias))

  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("/home/maxibeckel/maestria_datos/dmeyf/dmeyf2023/")
# cargo los datos

# cargo los datos
dataset <- fread("./data/competencia_01.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]


# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch3.txt"

# Escribo los titulos al archivo donde van a quedar los resultados
# atencion que si ya existe el archivo, esta instruccion LO SOBREESCRIBE,
#  y lo que estaba antes se pierde
# la forma que no suceda lo anterior es con append=TRUE
cat(
  file = archivo_salida,
  sep = "",
  "cp", "\t",
  "max_depth", "\t",
  "mb", "\t",
  "min_split", "\t",
  "ganancia_promedio", "\n"
)


# itero por los loops anidados para cada hiperparametro
t0 <- Sys.time()
for (cp in c(-1)) {
  for (vmax_depth in c(4, 6, 8, 10, 12, 14)) {
    for (vmin_split in c(1000, 800, 600, 400, 200, 100, 50, 20, 10)) {
      for (mb in c(c(1, as.integer(vmin_split / 4), as.integer(vmin_split / 2)))) {
        # notar como se agrega

        # vminsplit  minima cantidad de registros en un nodo para hacer el split
        param_basicos <- list(
          "cp" = cp, # complejidad minima
          "minsplit" = vmin_split,
          "minbucket" = mb, # minima cantidad de registros en una hoja
          "maxdepth" = vmax_depth
        ) # profundidad máxima del arbol

        # Un solo llamado, con la semilla 17
        ganancia_promedio <- ArbolesMontecarlo(semillas, param_basicos)

        # escribo los resultados al archivo de salida
        cat(
          file = archivo_salida,
          append = TRUE,
          sep = "",
          cp, "\t",
          vmax_depth, "\t",
          mb, "\t",
          vmin_split, "\t",
          ganancia_promedio, "\n"
        )
      }
    }
  }
}
tiempo <- as.numeric(Sys.time() - t0)

# Leo los resultados
res <- fread("/home/maxibeckel/maestria_datos/dmeyf/dmeyf2023/exp/HT2020/gridsearch.txt")
View(res[order(-ganancia_promedio)])
