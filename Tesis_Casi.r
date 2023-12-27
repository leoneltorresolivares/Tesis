setwd("C:/Users/DELL/Desktop/Tesis/Tesis_parte_final")
library(mclust)
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(cluster)
library(readxl)
library(openxlsx)

data_2014 <- read_excel("datos_2014_cluster.xlsx")
data_2015 <- read_excel("datos_2015_cluster.xlsx")
#dim(data_2015)
data_2016 <- read_excel("datos_2016_cluster.xlsx")
nrow(data_2014)+nrow(data_2015)+nrow(data_2016)
#dim(data_2016)
data <- data_2016
names(data)[1] <- "Cluster"
# View(data)
# dim(data)
colSums(is.na(data))
sum(colSums(is.na(data)))
# Analisis de los clusters ahora

a <- round(table(data$ingreso)/nrow(data)*100,2)
sum(a)
str(data)
data$genero <- factor(data$genero, order = F)
data$dependencia <- factor(data$dependencia, order = F)
data$rural <- factor(data$rural, order = F)
data$est_indigena <- factor(data$est_indigena, order = F)
data$clima <- factor(data$clima, order = F)
data$ingreso <- factor(data$ingreso, order = F)
data$Cluster <- factor(data$Cluster, order = F)
str(data)

variables <- c("clima", "genero", "ingreso", "dependencia", "rural", "est_indigena")
grupos_c <- NULL
# Tomar los valores maximo para identificar
for(i in 1:4){
  data_grupo <- subset(data, data$Cluster == i)
  max_valor <- c()
  categoria_max <- c()
  for (variable in variables) {
    tabla_frecuencias <- round(table(data_grupo[[variable]]) / nrow(data_grupo) * 100, 2)
    max_valor <- c(max_valor, max(tabla_frecuencias))
    categoria_max <- c(categoria_max, names(tabla_frecuencias)[tabla_frecuencias == max(tabla_frecuencias)])
  }
  resultados <- data.frame(Variable = variables, Max_Valor = max_valor, Categoria_Max = categoria_max)
  grupos_c <- rbind(grupos_c, resultados)

}
print(grupos_c)

grupos_n <- NULL
variables2 <- c("matematica","lectura","educ_padre","educ_madre","mat_total","horas_dc_aula","n_benef_sep")
for(i in 1:4){
  data_grupo <- subset(data, data$Cluster == i)
  prom <- c()
  dsd <- c()
  for(variable in variables2){
    va <- round(mean(data_grupo[[variable]]),2)
    prom <- c(prom, va)
    va <- round(sd(data_grupo[[variable]]),2)
    dsd <- c(dsd, va)
  }
  resultado <- data.frame(Variable = variables2, Promedio = prom, sd = dsd)
  resultado$cluster <- rep(i, length(variables2))
  grupos_n <- rbind(grupos_n, resultado)
}
print(grupos_n)



