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
require("ggplot2")
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

PARAM$input$dataset <- "~/buckets/b1/datasets/competencia_03_fe_ec.csv.gz"
PARAM$input$testing <- c(202107)

PARAM$hyperparametertuning$POS_ganancia <- 273000
PARAM$hyperparametertuning$NEG_ganancia <- -7000

# Aqui empieza el programa
setwd(paste0("~/buckets/b1/exp/", PARAM$experimento))

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

prob = fread('~/buckets/b1/exp/EC9010/ensamble.csv')

setDT(prob)[dataset, clase_ternaria := i.clase_ternaria, on = .(numero_de_cliente, foto_mes)]

#Func
calc_ganancia <- function(df, th, pos, neg){
  df <- df[1:th]
  df <- df[,ganancia := ifelse(clase_ternaria == 'BAJA+2', pos, neg)]
  
  ganancia_total = sum(df[,ganancia])
  
  return(ganancia_total)
}

cortes = 1000:18000
gan_ensamble <- sapply(colnames(prob)[3:23],function(x) NULL)

gan_ensamble <- as.data.table(cortes)


for(i in 3:23){
  tmp = prob[,c(colnames(prob)[i], "clase_ternaria"), with=FALSE]  
  colnames(tmp)[1] = "prob"
  setorder(tmp, -prob)
  gan_i <- sapply(cortes, function(ii){calc_ganancia(tmp, th = ii, pos = 273000, neg = -7000)})  
  gan_i <- as.data.table(gan_i)
  
  gan_ensamble <- cbind(gan_ensamble, gan_i)
  colnames(gan_ensamble)[ncol(gan_ensamble)] = colnames(prob)[i]
  
  print(paste0("Iter: ", i))
}


#Export results
fwrite(gan_ensamble,
       file = paste0("ganancia_ensamble.csv"),
       sep = ","
)

#Plot
#melt data frame into long format
df <- melt(gan_ensamble ,  id.vars = 'cortes', variable.name = 'series')

colv        <- c(scales::hue_pal()(20), "#000000")
names(colv) <- colnames(gan_ensamble)[2:22]

#create line plot for each column in data frame
p <- ggplot(data = df, aes(cortes, value, color = series)) +
       geom_line()+
       scale_color_manual(values = colv, guide="none") +
       theme_light()

png(filename="ensamble.png")
plot(p)
dev.off()