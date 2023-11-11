#Indicaciones para correr este script:
#1) El dataset utilizado puede producirse con /monday/zero_to_na.R (para tratar los errores en la toma de los datos) 
#y /monday/z901_fe_sql.ipynb par el feature engineering.
#2) Este R presenta los hiperparámetros seleccionados mediante BO para correr el modelo final.
#3) criterio de corte tomado: 10500 envios.

# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")

meses <- c(201912, 202001, 202002, 202003, 202004, 202005, 202006, 202007,
           202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103, 202104, 202105)

# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
mis_semillas <- c(594697, 594709, 594721, 594739, 594749,
                  100103, 100109, 100129, 100151, 100153,
                  100019,100043,100049,100057,100069,
                  161729, 221729, 202789, 700241, 991107)

PARAM <- list()
PARAM$experimento <- "EC9009"

PARAM$input$dataset <- "./datasets/competencia_03_fe_ec.csv.gz"

# meses donde se entrena el modelo
#PARAM$input$training <- c(202012, 202101, 202102, 202103, 202104, 202105)
PARAM$input$training <- c(202011,202012,202101,202102,202103,202104)
PARAM$input$future <- c(202106) # meses donde se aplica el modelo

#PARAM$finalmodel$semilla <- mis_semillas[1]

# hiperparametros optimizados BO
bo <- fread("~/buckets/b1/exp/EC8232.0.9/BO_log.txt")
setorder(bo, -ganancia)
#bo[1,] 


PARAM$finalmodel$optim$num_iterations <- bo[1, num_iterations]
PARAM$finalmodel$optim$learning_rate <- bo[1,learning_rate]
PARAM$finalmodel$optim$feature_fraction <-  bo[1,feature_fraction]
PARAM$finalmodel$optim$min_data_in_leaf <-  bo[1,min_data_in_leaf]
PARAM$finalmodel$optim$num_leaves <- bo[1, num_leaves]


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

  extra_trees = FALSE# Magic Sauce
  #saque el seed
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


# Catastrophe Analysis  -------------------------------------------------------
# deben ir cosas de este estilo
#   dataset[foto_mes == 202006, active_quarter := NA]

# Data Drifting
# por ahora, no hago nada


# Feature Engineering Historico  ----------------------------------------------
#   aqui deben calcularse los  lags y  lag_delta
#   Sin lags no hay paraiso ! corta la bocha
#   https://rdrr.io/cran/data.table/man/shift.html


#--------------------------------------

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


## Experimentos colaborativos: BO con LR altos (>0.9)
pred_res = dataset[foto_mes == PARAM$input$future, .(numero_de_cliente,foto_mes)]
for (i in seq_along(mis_semillas)){
  
  #parametros
  seed <- list(seed = mis_semillas[i])
  param_completo <- c(PARAM$finalmodel$lgb_basicos, PARAM$finalmodel$optim, seed)

  #modelo
  modelo <- lgb.train(
    data = dtrain,
    param = param_completo,
  )

  #--------------------------------------
  if(FALSE){
    # ahora imprimo la importancia de variables
    tb_importancia <- as.data.table(lgb.importance(modelo))
    archivo_importancia <- "impo.txt"

    fwrite(tb_importancia,
      file = archivo_importancia,
      sep = "\t"
    )
  }
  #--------------------------------------


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

  #add prob by seed
  setDT(pred_res)[tb_entrega, paste0("seed", mis_semillas[i]) := i.prob, on = .(numero_de_cliente, foto_mes)]

  cat(paste0("\nFinished:\t", i))
}


#Add ensamble result
pred_res = cbind(pred_res, pred_res[, .(ensamble = rowSums(.SD)), .SDcols = names(pred_res) %like% "seed"])

#Export results
fwrite(pred_res,
  file = paste0("ensamble.csv"),
  sep = ","
)

cat("\n\nScript finished\n")