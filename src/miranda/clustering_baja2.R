#Script para hacer clustering sobre los BAJA+2

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

#paquetes
library(data.table)
library(randomForest)
library(ggplot2)
library(rstatix)

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory

#Importo datos
dataset <- "./datasets/competencia_02_c.csv.gz"

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(dataset)

#cantidad de BAJA+2 totales
dataset[clase_ternaria == 'BAJA+2',.N]

#cantidad de BAJA+2 por foto_mes
dataset[clase_ternaria == 'BAJA+2', .(n_baja2 = .N), by = .(clase_ternaria,foto_mes)]

#Subset BAJA+2 
baja2 = dataset[clase_ternaria == 'BAJA+2',]

#NA imputation
for(k in names(baja2)){
    med <- median(baja2[[k]],na.rm = T)
    set(x = baja2, which(is.na(baja2[[k]])), k, med)
}

notcols = c("numero_de_cliente", "clase_ternaria")
tmp = baja2[, .SD, .SDcols=-notcols]

#PCA
if(FALSE){
    dataset.pc <- prcomp(tmp, center = FALSE, scale. = FALSE)
    x <- as.data.frame(dataset.pc$x)
    ggplot(x, aes(x=PC1, y=PC2)) + geom_point()
}

#RandomForest

rf.fit <- randomForest(x = tmp, 
                        y = NULL, 
                        ntree = 1000, 
                        proximity = TRUE, 
                        oob.prox = TRUE, 
                        na.action=na.roughfix)
hclust.rf <- hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")
rf.cluster = cutree(hclust.rf, k=5)

baja2$rf.clusters <- as.factor(rf.cluster)

#Export data with clusters
fwrite(baja2, "./datasets/competencia_02_clusters.csv.gz")

#Importo baja+2
baja2 = fread("./datasets/competencia_02_clusters.csv.gz")

#PLOTS
my.summary = function(x) list(mean = mean(x), se = sd(x)  / sqrt(length(x)))
#Pdf
pdf('features_by_clusters.pdf',width = 14)
#Loop
for (i in 3:154){
  #Data
  f =names(baja2)[i]
  df<-baja2[, as.list(unlist(lapply(.SD, my.summary))), by=rf.clusters, .SDcols = f]
  df[,rf.clusters:=as.factor(rf.clusters)]
  names(df)[2:3] = c('mean', 'se')
  #Plot
  p1 <- ggplot(df) +
    geom_bar( aes(x=rf.clusters, y=mean, fill=rf.clusters), stat="identity", alpha=0.7)+
    geom_errorbar( aes(x=rf.clusters, ymin=mean-se, ymax=mean+se), width=0.4, colour="black", alpha=0.9, size=1.3)+
    ggtitle(f)
  #Print
  plot(p1)
}
dev.off()