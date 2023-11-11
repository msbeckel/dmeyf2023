# Este script esta pensado para correr en Google Cloud
#   8 vCPU
# 128 GB memoria RAM

# se entrena con clase_binaria2  POS =  { BAJA+1, BAJA+2 }
# Optimizacion Bayesiana de hiperparametros de  lightgbm,

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")

require("lightgbm")

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

# para que se detenga ante el primer error
# y muestre el stack de funciones invocadas
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})


PARAM <- list()

PARAM$experimento <- "EC9010"

PARAM$input$dataset <- "./datasets/competencia_03_fe_ec.csv.gz"
PARAM$input$testing <- c(202106)

PARAM$hyperparametertuning$POS_ganancia <- 273000
PARAM$hyperparametertuning$NEG_ganancia <- -7000

# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

prob = fread('~/buckets/b1/exp/EC9010/EC9010_ensamble.csv')

head(dataset[foto_mes == '202106',.(numero_de_cliente, foto_mes, clase_ternaria)])
head(prob)
mes = '202106'

setDT(prob)[dataset, clase_ternaria := i.clase_ternaria, on = .(numero_de_cliente, foto_mes)]

#Func
calc_ganancia <- function(df, th, pos, neg){
    setorder(df, -prob)
    df <- df[1:th]
    df <- df[,ganancia := ifelse(clase_ternaria == 'BAJA+2', pos, neg)]

    ganancia_total = sum(df[,ganancia])

    return(ganancia_total)
}


trend_ganancia <- sapply(1000:21000,function(i){calc_ganancia(prob, th = i, pos = 273000, neg = -7000)})

calc_ganancia(prob, th = 20000, pos = 273000, neg = -7000)

plot(x=1000:21000, y=trend_ganancia)
max(trend_ganancia)
