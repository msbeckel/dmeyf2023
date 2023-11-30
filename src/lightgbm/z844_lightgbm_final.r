#Indicaciones para correr este script:
#1) El dataset utilizado puede producirse con /monday/zero_to_na.R (para tratar los errores en la toma de los datos) 
#y /monday/z901_fe_sql.ipynb par el feature engineering.
#2) Este R presenta los hiperparámetros seleccionados mediante BO para correr el modelo final.
#3) criterio de corte tomado: 10500 envios.

# para correr el Google Cloud
#   8 vCPU
#  128 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
mis_semillas <- c(594697, 594709, 594721, 594739, 594749)

PARAM <- list()
PARAM$experimento <- "KU8842"

#FE generado a partir de src/z901_fe_sql.ipynb
PARAM$input$dataset <- "./datasets/competencia_03_c_fe_2.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(201901, 201902, 201903, 201904, 201905, 201906, 
                          201907, 201908, 201909, 201910, 201911, 201912,
                          202010, 202011, 202012, 202101, 202102, 202103, 
                          202104, 202105, , 202106, 202107)

PARAM$input$future <- c(202109) # meses donde se aplica el modelo

PARAM$finalmodel$semilla <- mis_semillas[1]


#Se construirá un modelo a partir de un ensamble de los modelos ganadores de la competencia 2.
# Se busco en el github de los mejores puestos el modelo final declarado y se construye una tabla
# con los hiperparámetros obtenidos en cada caso.

#En el caso de Juan Raman se obtuvo el archivo BO_log.txt, en el resto se extrajo los hiperparámetros
#a partir de los scripts.
raman <- fread("/home/ms_beckel/dmeyf2023/src/ensamble/BO_log.txt")
setorder(raman, -ganancia)

#En el caso de Raman (ganador de la competencia) me quedo con los primeros 3 modelos
models <- raman[1:3,.(num_iterations, learning_rate, feature_fraction, min_data_in_leaf, num_leaves)]
serpa <- list("num_iterations" = 2360, 
              "learning_rate" = 0.055101382, 
              "feature_fraction" = 0.153838929, 
              "min_data_in_leaf" = 14319, 
              "num_leaves" = 728)
flores <- list("num_iterations" = 6875, 
              "learning_rate" = 0.0133326486877864, 
              "feature_fraction" = 0.216652156817687, 
              "min_data_in_leaf" = 516, 
              "num_leaves" = 685)
casali1 <- list("num_iterations" = 829, 
              "learning_rate" = 0.0314795407476228, 
              "feature_fraction" = 0.575871678909783, 
              "min_data_in_leaf" = 5336, 
              "num_leaves" = 478)
casali2 <- list("num_iterations" = 411, 
              "learning_rate" = 0.090567777671882, 
              "feature_fraction" = 0.870907307849918, 
              "min_data_in_leaf" = 9496, 
              "num_leaves" = 1024)
marchesini <- list("num_iterations" = 253, 
              "learning_rate" = 0.060332943, 
              "feature_fraction" = 0.550117901, 
              "min_data_in_leaf" = 6648, 
              "num_leaves" = 263)
telsesser <- list("num_iterations" = 1859, 
              "learning_rate" = 0.0221787360433459, 
              "feature_fraction" = 0.999869931759323, 
              "min_data_in_leaf" = 15159, 
              "num_leaves" = 700)

#
models <- rbind(models, serpa, flores, casali1, casali2, marchesini, telsesser)

# Hiperparametros FIJOS de  lightgbm
PARAM$finalmodel$lgb_basicos <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO

  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0

  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

  extra_trees = TRUE, # Magic Sauce

  seed = PARAM$finalmodel$semilla
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)


## pred_res guarda las prob obtenidas en cada modelo.
pred_res = dataset[foto_mes == PARAM$input$future, .(numero_de_cliente,foto_mes)]

for (i in 1:nrow(models)){
  
  #parametros
  PARAM$finalmodel$optim$num_iterations   <- models[i, num_iterations]
  PARAM$finalmodel$optim$learning_rate    <- models[i,learning_rate]
  PARAM$finalmodel$optim$feature_fraction <- models[i,feature_fraction]
  PARAM$finalmodel$optim$min_data_in_leaf <- models[i,min_data_in_leaf]
  PARAM$finalmodel$optim$num_leaves       <- models[i, num_leaves]

  # genero el modelo
  param_completo <- c(PARAM$finalmodel$lgb_basicos,
    PARAM$finalmodel$optim)
  
  #modelo
  modelo <- lgb.train(
    data = dtrain,
    param = param_completo,
  )

  # aplico el modelo a los datos sin clase
  dapply <- dataset[foto_mes == PARAM$input$future]

  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )

  # genero la tabla de entrega
  tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
  tb_entrega[, prob := prediccion]

  #agrego la prob del modelo a la tabla
  setDT(pred_res)[tb_entrega, paste0("model", i) := i.prob, on = .(numero_de_cliente, foto_mes)]

  cat(paste0("\nFinished:\t", i))
}

#Calculo la prob para el ensamble de los modelos
pred_res = cbind(pred_res, pred_res[, .(ensamble = rowSums(.SD)), .SDcols = names(pred_res) %like% "model"])

#Exporto resultados
fwrite(pred_res,
  file = paste0("ensamble.csv"),
  sep = ","
)

#Ordeno por el ensamble
setorder(pred_res, -ensamble)

#Entregas Kaggle
cortes <- seq(10000, 13000, by = 500)
for (envios in cortes) {
  pred_res[, Predicted := 0L]
  pred_res[1:envios, Predicted := 1L]
  fwrite(pred_res[, list(numero_de_cliente, Predicted)],
    file = paste0(PARAM$experimento, "_", envios, ".csv"),
    sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")