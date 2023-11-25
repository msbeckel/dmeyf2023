# leitmotiv  "Don't believe me, just watch"
# Este script dibuba los gráficos de la Clase plenaria-01
#   en donde se expuso que es posible construir Semillerios superadores
#   con learning_rate  absurdamente altos
#   y QUE FUNCIONAN MEJOR que el mejor LightGBM 
#   producto de una Bayesian Optimization

# les recuerdo las ganancias del podio de la Segunda Competencia
#   163.3  159.3  158.8

#limpio la memoria
rm( list= ls(all.names= TRUE) )  #remove all objects
gc( full= TRUE )                 #garbage collection

require("data.table")
require("primes")
require("lightgbm")
require("ggplot2")


#------------------------------------------------------------------------------
options(error = function() {
  traceback(20);
  options(error = NULL);
  stop("exiting after script error")
})
#------------------------------------------------------------------------------

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "plenaria-01"

# Dataset que tiene completa clase_ternaria para 202107
#   asi puedo calcular las ganancias
PARAM$dataset_input <-  "~/buckets/b1/datasets/competencia_02.csv.gz"

# grid inicial, se puede cambiar por los casos que se desee probar
# aqui estan los CIENTOS de casos con LR alto
PARAM$grid_nube  <- "https://storage.googleapis.com/open-courses/dmeyf2023-8a1e/grid_plenaria.txt"

# para generar vector de numeros primos
PARAM$semilla_primos <- 102191   # primer semilla

# observar hueco de la pandemia
PARAM$trainingstrategy$clases_minoritarias <- c("BAJA+1","BAJA+2")
PARAM$trainingstrategy$undersampling <- 0.1
PARAM$trainingstrategy$semilla <- 200177  # segunda semilla

PARAM$trainingstrategy$finaltrain <- c( 201901, 201902, 201903, 201904, 201905,
  202906, 201907, 201908, 201909, 201910, 201911, 201912, 202001,
  202002, 202010, 202011, 202012, 202101, 202102, 202103, 202104)

PARAM$trainingstrategy$future <- c(202107)


# Hiperparametros FIJOS de  lightgbm
PARAM$lgb_basicos <- list(
   boosting= "gbdt",               # puede ir  dart  , ni pruebe random_forest
   objective= "binary",
   metric= "custom",
   first_metric_only= TRUE,
   boost_from_average= TRUE,
   feature_pre_filter= FALSE,
   force_row_wise= TRUE,           # para que los alumnos no se atemoricen con tantos warning
   verbosity= -100,
   max_depth=  -1L,                # -1 significa no limitar,  por ahora lo dejo fijo
   min_gain_to_split= 0.0,         # min_gain_to_split >= 0.0
   min_sum_hessian_in_leaf= 0.001, # min_sum_hessian_in_leaf >= 0.0
   lambda_l1= 0.0,                 # lambda_l1 >= 0.0
   lambda_l2= 0.0,                 # lambda_l2 >= 0.0
   max_bin= 31L,                   # lo debo dejar fijo, no participa de la BO

   bagging_fraction= 1.0,          # 0.0 < bagging_fraction <= 1.0
   pos_bagging_fraction= 1.0,      # 0.0 < pos_bagging_fraction <= 1.0
   neg_bagging_fraction= 1.0,      # 0.0 < neg_bagging_fraction <= 1.0
   is_unbalance=  FALSE,           #
   scale_pos_weight= 1.0,          # scale_pos_weight > 0.0

   drop_rate=  0.1,                # 0.0 < neg_bagging_fraction <= 1.0
   max_drop= 50,                   # <=0 means no limit
   skip_drop= 0.5,                 # 0.0 <= skip_drop <= 1.0

   extra_trees= FALSE,              # IMPORTANCE que este en FALSE
   early_stopping= 0
   )


PARAM$graficar$x_min <- 0
PARAM$graficar$x_max <- 20000
PARAM$graficar$y_min <- 100
PARAM$graficar$y_max <- 180
PARAM$graficar$escalar  <- 1e-06

PARAM$home  <- "~/buckets/b1/"
PARAM$exp_directory <- "exp"

# FIN Parametros del script

#------------------------------------------------------------------------------
# Adaptado del ganador de la Segunda Competencia
# Asigno NA  a las variabliables rotas

CatastropheAnalysis <- function( dataset ) {
  # Catastrophe Analysis
  # deben ir cosas de este estilo
  #   dataset[foto_mes == 202006, active_quarter := NA]
  dataset[foto_mes == 201901, ctransferencias_recibidas := NA]
  dataset[foto_mes == 201901, mtransferencias_recibidas := NA]

  dataset[foto_mes == 201902, ctransferencias_recibidas := NA]
  dataset[foto_mes == 201902, mtransferencias_recibidas := NA]

  dataset[foto_mes == 201903, ctransferencias_recibidas := NA]
  dataset[foto_mes == 201903, mtransferencias_recibidas := NA]

  dataset[foto_mes == 201904, ctarjeta_visa_debitos_automaticos := NA]
  dataset[foto_mes == 201904, ctransferencias_recibidas := NA]
  dataset[foto_mes == 201904, mtransferencias_recibidas := NA]
  dataset[foto_mes == 201904, mttarjeta_visa_debitos_automaticos := NA]
  dataset[foto_mes == 201904, Visa_mfinanciacion_limite := NA]

  dataset[foto_mes == 201905, ccomisiones_otras := NA]
  dataset[foto_mes == 201905, ctarjeta_visa_debitos_automaticos := NA]
  dataset[foto_mes == 201905, ctransferencias_recibidas := NA]
  dataset[foto_mes == 201905, mactivos_margen := NA]
  dataset[foto_mes == 201905, mcomisiones := NA]
  dataset[foto_mes == 201905, mcomisiones_otras := NA]
  dataset[foto_mes == 201905, mpasivos_margen := NA]
  dataset[foto_mes == 201905, mrentabilidad_annual := NA]
  dataset[foto_mes == 201905, mrentabilidad := NA]
  dataset[foto_mes == 201905, mtransferencias_recibidas := NA]

  dataset[foto_mes == 201910, ccajeros_propios_descuentos := NA]
  dataset[foto_mes == 201910, ccomisiones_otras := NA]
  dataset[foto_mes == 201910, chomebanking_transacciones := NA]
  dataset[foto_mes == 201910, ctarjeta_master_descuentos := NA]
  dataset[foto_mes == 201910, ctarjeta_visa_descuentos := NA]
  dataset[foto_mes == 201910, mactivos_margen := NA]
  dataset[foto_mes == 201910, mcajeros_propios_descuentos := NA]
  dataset[foto_mes == 201910, mcomisiones := NA]
  dataset[foto_mes == 201910, mcomisiones_otras := NA]
  dataset[foto_mes == 201910, mpasivos_margen := NA]
  dataset[foto_mes == 201910, mrentabilidad_annual := NA]
  dataset[foto_mes == 201910, mrentabilidad := NA]
  dataset[foto_mes == 201910, mtarjeta_master_descuentos := NA]
  dataset[foto_mes == 201910, mtarjeta_visa_descuentos := NA]

  dataset[foto_mes == 202001, cliente_vip := NA]

  dataset[foto_mes == 202006, active_quarter := NA]
  dataset[foto_mes == 202006, catm_trx := NA]
  dataset[foto_mes == 202006, catm_trx_other := NA]
  dataset[foto_mes == 202006, ccajas_consultas := NA]
  dataset[foto_mes == 202006, ccajas_depositos := NA]
  dataset[foto_mes == 202006, ccajas_extracciones := NA]
  dataset[foto_mes == 202006, ccajas_otras := NA]
  dataset[foto_mes == 202006, ccajas_transacciones := NA]
  dataset[foto_mes == 202006, ccallcenter_transacciones := NA]
  dataset[foto_mes == 202006, ccheques_depositados := NA]
  dataset[foto_mes == 202006, ccheques_depositados_rechazados := NA]
  dataset[foto_mes == 202006, ccheques_emitidos := NA]
  dataset[foto_mes == 202006, ccheques_emitidos_rechazados := NA]
  dataset[foto_mes == 202006, ccomisiones_otras := NA]
  dataset[foto_mes == 202006, cextraccion_autoservicio := NA]
  dataset[foto_mes == 202006, chomebanking_transacciones := NA]
  dataset[foto_mes == 202006, cmobile_app_trx := NA]
  dataset[foto_mes == 202006, ctarjeta_debito_transacciones := NA]
  dataset[foto_mes == 202006, ctarjeta_master_transacciones := NA]
  dataset[foto_mes == 202006, ctarjeta_visa_transacciones := NA]
  dataset[foto_mes == 202006, ctrx_quarter := NA]
  dataset[foto_mes == 202006, mactivos_margen := NA]
  dataset[foto_mes == 202006, matm := NA]
  dataset[foto_mes == 202006, matm_other := NA]
  dataset[foto_mes == 202006, mautoservicio := NA]
  dataset[foto_mes == 202006, mcheques_depositados := NA]
  dataset[foto_mes == 202006, mcheques_depositados_rechazados := NA]
  dataset[foto_mes == 202006, mcheques_emitidos := NA]
  dataset[foto_mes == 202006, mcheques_emitidos_rechazados := NA]
  dataset[foto_mes == 202006, mcomisiones := NA]
  dataset[foto_mes == 202006, mcomisiones_otras := NA]
  dataset[foto_mes == 202006, mcuentas_saldo := NA]
  dataset[foto_mes == 202006, mextraccion_autoservicio := NA]
  dataset[foto_mes == 202006, mpasivos_margen := NA]
  dataset[foto_mes == 202006, mrentabilidad_annual := NA]
  dataset[foto_mes == 202006, mrentabilidad := NA]
  dataset[foto_mes == 202006, mtarjeta_master_consumo := NA]
  dataset[foto_mes == 202006, mtarjeta_visa_consumo := NA]
  dataset[foto_mes == 202006, tcallcenter := NA]
  dataset[foto_mes == 202006, thomebanking := NA]
}

#------------------------------------------------------------------------------
# Adaptivado del ganador de la Segunda Competencia
#  Lags y Delta Lags de orden 1, 2 y 6

FeatureEngineeringHistorico <- function( dataset ) {

  # Feature Engineering Historico
  #   aqui deben calcularse los  lags y  lag_delta
  #   Sin lags no hay paraiso !  corta la bocha
  # defino las columnas a las que les puedo calcular el lag
  cols_lagueables <- copy(setdiff( colnames(dataset),
    c("numero_de_cliente", "foto_mes", "clase_ternaria") ))

  # FUNDAMENTAL  ordenar el dataset anes de los lags
  setorder( dataset, numero_de_cliente, foto_mes )

  # lags de orden 1
  dataset[, paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"),
    by = numero_de_cliente,
    .SDcols = cols_lagueables]

  # agrego los delta lags de orden 1
  for (vcol in cols_lagueables) 
    dataset[, paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]


  # lags de orden 2
  dataset[, paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"),
    by = numero_de_cliente,
    .SDcols = cols_lagueables]

  # agrego los delta lags de orden 2
  for (vcol in cols_lagueables) 
    dataset[, paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]


  # lags de orden 6
  dataset[, paste0(cols_lagueables, "_lag6") := shift(.SD, 6, NA, "lag"),
    by = numero_de_cliente,
    .SDcols = cols_lagueables]

  # agrego los delta lags de orden 6
  for (vcol in cols_lagueables) 
    dataset[, paste0(vcol, "_delta6") := get(vcol) - get(paste0(vcol, "_lag6"))]

  # reordeno original
  setorder( dataset, foto_mes, numero_de_cliente )
}
#------------------------------------------------------------------------------
# solamente undersampling al 10% de la clase CONTINUA

TrainingStrategy <- function( dataset ) {

  dataset[, fold_future := 0L ]
  dataset[ foto_mes %in% PARAM$trainingstrategy$future,
           fold_future := 1L ]


  dataset[, fold_finaltrain := 0L ]
  set.seed( PARAM$trainingstrategy$semilla, kind= "L'Ecuyer-CMRG")
  dataset[ foto_mes %in% PARAM$trainingstrategy$finaltrain,
    azar := runif( nrow(dataset[foto_mes %in% PARAM$trainingstrategy$finaltrain ]) )]

  dataset[ foto_mes %in% PARAM$trainingstrategy$finaltrain &
           (azar <= PARAM$trainingstrategy$undersampling | 
             clase_ternaria %in% PARAM$trainingstrategy$clases_minoritarias ),
           fold_finaltrain := 1L]

  dataset[ , azar:= NULL ]  # elimino el campo auxiliar
}
#------------------------------------------------------------------------------
# calculo la curva de ganancia
#   ganancia vs envios

curva_ganancia  <- function( vganancias, vprobs, puntos ) {

  gans_acum <- cumsum( vganancias[ order(-vprobs) ] )

  max_suavizada <- max( frollmean( gans_acum, 
     n = 2001,
     align = "center", 
     na.rm = TRUE, 
     hasNA = TRUE ), 
     na.rm=TRUE )


  return( list( "suavizada" = max_suavizada,
                "curva" = gans_acum[1:puntos] ))
}

#------------------------------------------------------------------------------
# genero el grafico
#  tiene en color negro el semillerio
#   y luego cada uno de los modelitos

generar_grafico <- function( reg, tb_modelitos ) {

  ganancia <- curva_ganancia( tb_modelitos$ganancia, tb_modelitos$semillerio, PARAM$graficar$x_max )
  ganancia$curva <- ganancia$curva * PARAM$graficar$escalar
  semillerio_ganancia_suavizada <- ganancia$suavizada * PARAM$graficar$escalar

  tablita <- as.data.table( list( "envios"=1:PARAM$graficar$x_max ))
  tablita$ganancia <- ganancia$curva

  gra <- ggplot( tablita ) +
    geom_line( aes(x=envios, y=ganancia, color="black"), show.legend= FALSE, na.rm=TRUE ) +
    scale_y_continuous(limits = c(PARAM$graficar$y_min, PARAM$graficar$y_max) ) +
    labs(title = paste( reg$id, "sem",reg$semillerio, "lr", reg$learning_rate,
             "iter", reg$num_iterations, "leaves", reg$num_leaves,
             "min_data", reg$min_data_in_leaf, "ff", reg$feature_fraction ),
         subtitle= paste( "max gan suavizada", round(semillerio_ganancia_suavizada,1)),
         x= 'envios', 
         y='ganancia  ($ M)' ) 

  for( i in 1:reg$semillerio ) {
    ganancia <- curva_ganancia( tb_modelitos$ganancia, tb_modelitos[ , get(paste0("s",i))], PARAM$graficar$x_max )
    ganancia$curva <- ganancia$curva * PARAM$graficar$escalar

    tablita <- as.data.table( list( "envios"=1:PARAM$graficar$x_max ))
    tablita$ganancia <- ganancia$curva

    gra <- gra + geom_line( data=tablita, aes(x=envios, y=ganancia, color="grey"), show.legend= FALSE, na.rm=TRUE  )
  }


  ggsave( gra, file=paste0( reg$id, ".pdf" ) )

  return( semillerio_ganancia_suavizada ) 
}

#------------------------------------------------------------------------------
# genero las salida para kaggle
#  para que se puedan corroborar las ganancias en la Segunda Competencia
#  y tambien que se pueda generar una salida para la Ultima Competencia
#   y proba rque tan bueno es

generar_kaggle <- function( vnumero_de_cliente, vprobs, reg ){

  tablita <- as.data.table( 
    list( "numero_de_cliente" = vnumero_de_cliente[ order(-vprobs) ] ) )

  dir.create( "./kaggle", showWarnings = FALSE )

  # genero los archivos para cada corte
  for( corte in  seq( 10000, 13000, 500) ){
    tablita[ , Predicted := 0L ]
    tablita[ 1:corte, Predicted := 1L ]

    fwrite( tablita,
            file= paste0( "./kaggle/", reg$id, "_", sprintf( "%.5d", corte), ".csv" ),
            sep= "," )
  }
}
#------------------------------------------------------------------------------
# aqui realizo el trabajo pesado

procesar_y_graficar <- function( reg ) {

  cat( reg$id, " ")
  # creo la tabla donde voy a guardar la probabilidades
  #  del semillerio
  #  y de cada modelito
  tb_modelitos <- dataset_future[, list(numero_de_cliente, ganancia) ]
  tb_modelitos[, semillerio := 0 ]

  futuro_matrix <- data.matrix(dataset_future[, campos_buenos, with = FALSE])
  param_completo <- copy(PARAM$lgb_basicos) 

  # establezco los hiperparametros, excepto la semilla
  param_completo$num_iterations <- reg$num_iterations
  param_completo$learning_rate <- reg$learning_rate
  param_completo$num_leaves <- reg$num_leaves
  param_completo$min_data_in_leaf <- reg$min_data_in_leaf
  param_completo$feature_fraction <- reg$feature_fraction
 
  # itero por las semillas
  for( isem  in 1:reg$semillerio ) {

    cat( isem, " " ) 
    # aqui cambio la semilla de cada corrida
    param_completo$seed <- ksemillas[ isem ]

    # genero el modelo para ESA semilla
    modelito <- lgb.train( data= dfinaltrain,
                           param= param_completo,
                           verbose= -100 )

    # prediccion sobre los datos del futuro
    prediccion <- predict( modelito, futuro_matrix )

    # acumulo en semillerio las probabilidades
    #  directamente sumo, no hace falta promediar
    tb_modelitos[, semillerio := semillerio + prediccion ]
    tb_modelitos[, paste0("s", isem) := prediccion ]
  }

  # Si viene la clase_ternaria, entonces hay ganancia
  #   y puedo hacer los graficos
  if( tb_modelitos[ is.na(ganancia), .N ] == 0 )
    gan_suavizada <- generar_grafico( reg, tb_modelitos)

  # siempre puedo generar para Kaggle, aun sin clase_ternaria en future
  generar_kaggle( tb_modelitos$numero_de_cliente,
    tb_modelitos$semillerio,
    reg )

  rm( futuro_matrix )
  rm( tb_modelitos )
  gc()

  cat("\n")
  return( gan_suavizada )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

setwd( PARAM$home )

# creo la carpeta donde va el experimento
dir.create( paste0( "./", PARAM$exp_directory ), showWarnings = FALSE )
dir.create( paste0( "./", PARAM$exp_directory, "/", PARAM$experimento, "/"), showWarnings = FALSE )
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0( "./", PARAM$exp_directory, "/", PARAM$experimento, "/"))


# genero un vector de semillas
ksemillas <- generate_primes(min=100000, max=1000000)  # genero TODOS los numeros primos entre 100k y 1M
set.seed( PARAM$semilla_primos, kind= "L'Ecuyer-CMRG" ) # seteo la semilla que controla al sample de los primos
ksemillas  <- sample(ksemillas)

# cargo el dataset
# esta en la carpeta del exp_input y siempre se llama  dataset_training.csv.gz
dataset <- fread( PARAM$dataset_input )

#tmobile_app se daño a partir de 202010
dataset[ , tmobile_app := NULL ]

# Corrijo las variables rotas (estan en cero)
CatastropheAnalysis( dataset )

# No hago data drifting

# Feature Engeneering Historico, agrego columnas
FeatureEngineeringHistorico( dataset )


# Training Strategy
#  Defino donde voy a entrenar
TrainingStrategy( dataset )


# Preparo donde voy a entrenar
# paso la clase a binaria que tome valores {0,1}  enteros
dataset[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]

# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "clase01", "fold_future", "fold_finaltrain"))

# los datos del futuro, donde voy a evaluar el modelo
dataset_future <- copy( dataset[ fold_future ==1L, ] )
dataset_future[ , ganancia := ifelse( clase_ternaria=="BAJA+2", 273000, -7000) ]

# dejo los datos en el formato que necesita LightGBM
dfinaltrain <- lgb.Dataset(
  data = data.matrix(dataset[fold_finaltrain == 1L, campos_buenos, with = FALSE]),
  label = dataset[fold_finaltrain == 1L, clase01],
  free_raw_data = FALSE )


# hago espacio borrando lo que ya no necesito
rm(dataset)
gc()


# el dataset que tiene los parametros que voy a probar
if( file.exists( "grid_plenaria.txt" ) ) {
  tb_grid <- fread( "grid_plenaria.txt" )
} else {
  # leao el inicial de la nube
  tb_grid <- fread(PARAM$grid_nube) 
}


if( tb_grid[ , length(unique(id)) != .N ] ) {
 cat( "Hay  id's repetidos en tb_grid. No quiero seguir.\n" )
 stop()
}

# recorro la grid generando los graficos
for( i in 1:nrow(tb_grid) )
{
  if( tb_grid[ i, procesado] == 0L )
  {
    reg <- tb_grid[ i, ]

    # aqui hago el trabajo pesado
    gan_suavizada <- procesar_y_graficar( reg )

    # marco que ya procese
    tb_grid[ i, procesado := 1L ]
    tb_grid[ i, suavizada := gan_suavizada ]
    fwrite( tb_grid,
            file = "grid_plenaria.txt",
            sep ="\t" )
  }
}
