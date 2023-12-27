setwd("C:/Users/DELL/Desktop/Tesis")
library(mclust)
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(cluster)
library(readxl)

datos_2014_limpios <- read_csv("datos_2014_limpios.csv")
datos_2015_limpios <- read_csv("datos_2015_limpios.csv")
datos_2016_limpios <- read_csv("datos_2016_limpios.csv")
data <- datos_2014_limpios

data <- data %>% filter(est_migrante == 0, pre_escolar == 1)

borrar <- c("agno", "rbd", "pre_escolar", "nombre_estab", "region", "cod_comuna", "mat_basica",
            "nro_dc_aula", "latitud", "longitud", "est_migrante")

data <- data[, !(names(data) %in% borrar)]

colSums(is.na(data))
sum(colSums(is.na(data)))

data$genero <- factor(data$genero, order = F)
data$clima <- factor(data$clima, order = F)
data$dependencia <- factor(data$dependencia, order = F)
data$rural <- factor(data$rural, order = F)
data$est_indigena <- factor(data$est_indigena, order = F)

# Educ_padre
data$educ_padre[data$educ_padre == 14] <- 13 
data$educ_padre[data$educ_padre == 15] <- 13 
data$educ_padre[data$educ_padre == 17] <- 13 
data$educ_padre[data$educ_padre >= 1 & data$educ_padre <= 13] <- data$educ_padre[data$educ_padre >= 1 & data$educ_padre <= 13] - 1
data$educ_padre[data$educ_padre == 16] <- 14
data$educ_padre[data$educ_padre == 18] <- 17
data$educ_padre[data$educ_padre == 19] <- 19
data$educ_padre[data$educ_padre == 20] <- 23

# Educ_madre
data$educ_madre[data$educ_madre == 14] <- 13 
data$educ_madre[data$educ_madre == 15] <- 13 
data$educ_madre[data$educ_madre == 17] <- 13 
data$educ_madre[data$educ_madre >= 1 & data$educ_madre <= 13] <- data$educ_madre[data$educ_madre >= 1 & data$educ_madre <= 13] - 1
data$educ_madre[data$educ_madre == 16] <- 14
data$educ_madre[data$educ_madre == 18] <- 17
data$educ_madre[data$educ_madre == 19] <- 19
data$educ_madre[data$educ_madre == 20] <- 23

# Ingreso
data$ingreso <- ifelse(data$ingreso >= 1 & data$ingreso <= 2, "E", data$ingreso)
data$ingreso <- ifelse(data$ingreso >= 3 & data$ingreso <= 4, "D", data$ingreso)
data$ingreso <- ifelse(data$ingreso >= 5 & data$ingreso <= 6, "C3", data$ingreso)
data$ingreso <- ifelse(data$ingreso >= 7 & data$ingreso <= 9, "C2", data$ingreso)
data$ingreso <- ifelse(data$ingreso >= 10 & data$ingreso <= 15, "ABC1", data$ingreso)

data$ingreso <- factor(data$ingreso, order = T, levels = c("E","D","C3","C2","ABC1"))
colSums(is.na(data))
sum(colSums(is.na(data)))
#write.csv(data, "datos_2016.csv")

































