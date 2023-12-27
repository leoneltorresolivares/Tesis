setwd("C:/Users/DELL/Desktop/Tesis")

library(readr)
datos_2014_limpios <- read_csv("datos_2014_limpios.csv")

datos_2015_limpios <- read_csv("datos_2015_limpios.csv")

datos_2016_limpios <- read_csv("datos_2016_limpios.csv")

#datos_2017_limpios <- read_csv("datos_2017_limpios.csv")
#datos_2017_limpios <- datos_2017_limpios[,-1]

data <- datos_2016_limpios

#Solo para asegurarse
colSums(is.na(data))
sum(colSums(is.na(data)))

#Matematica
summary(data$matematica)
round(sd(data$matematica),2)

#lectura
summary(data$lectura)
round(sd(data$lectura),2)

#clima
barplot(table(data$clima))
round(table(data$clima)/nrow(data)*100,2)

#dependencia
barplot(table(data$dependencia))
round(table(data$dependencia)/nrow(data)*100,2)

#educ_padre
data$educ_padre[data$educ_padre == 1] <- "No estudio"
data$educ_padre <- ifelse(data$educ_padre >= 2 & data$educ_padre <= 9, "B", data$educ_padre)
data$educ_padre <- ifelse(data$educ_padre >= 10 & data$educ_padre <= 15, "M", data$educ_padre)
data$educ_padre <- ifelse(data$educ_padre >= 16 & data$educ_padre <= 20, "S", data$educ_padre)
barplot(table(data$educ_padre))
round(table(data$educ_padre)/nrow(data)*100,2)

#educ_madre
data$educ_madre[data$educ_madre == 1] <- "No estudio"
data$educ_madre <- ifelse(data$educ_madre >= 2 & data$educ_madre <= 9, "B", data$educ_madre)
data$educ_madre <- ifelse(data$educ_madre >= 10 & data$educ_madre <= 15, "M", data$educ_madre)
data$educ_madre <- ifelse(data$educ_madre >= 16 & data$educ_madre <= 20, "S", data$educ_madre)
barplot(table(data$educ_madre))
round(table(data$educ_madre)/nrow(data)*100,2)

#ingreso
data$ingreso <- ifelse(data$ingreso >= 1 & data$ingreso <= 2, "E", data$ingreso)
data$ingreso[data$ingreso == 3] <- "D"
data$ingreso <- ifelse(data$ingreso >= 4 & data$ingreso <= 5, "C3", data$ingreso)
data$ingreso <- ifelse(data$ingreso >= 6 & data$ingreso <= 9, "C2", data$ingreso)
data$ingreso <- ifelse(data$ingreso >= 10 & data$ingreso <= 15, "ABC1", data$ingreso)
barplot(table(data$ingreso))
round(table(data$ingreso)/nrow(data)*100,2)

#mat_total
min <- min(data$mat_total[data$mat_total != 0])
data$mat_total[data$mat_total == 0] <- min
hist(data$mat_total)
boxplot(data$mat_total)
summary(data$mat_total)
round(sd(data$mat_total),1)

#horas_dc_aula
min <- min(data$horas_dc_aula[data$horas_dc_aula != 0])
data$horas_dc_aula[data$horas_dc_aula == 0] <- min
hist(data$horas_dc_aula)
boxplot(data$horas_dc_aula)
summary(data$horas_dc_aula)
round(sd(data$horas_dc_aula),1)

#n_benef_sep
hist(data$n_benef_sep)
boxplot(data$n_benef_sep)
summary(data$n_benef_sep)
round(sd(data$n_benef_sep),1)

#est_indigena
barplot(table(data$est_indigena))
round(table(data$est_indigena)/nrow(data)*100,2)

#rural
barplot(table(data$rural))
round(table(data$rural)/nrow(data)*100,2)

#genero
barplot(table(data$genero))
round(table(data$genero)/nrow(data)*100,2)








































