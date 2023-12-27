library(dplyr)
library(readr)
library(rio) #Para exportar los datos a excel
library(modeest) #moda
setwd("C:/Users/DELL/Desktop/Tesis")

datos_2014 <- read_csv("datos 2014.csv")
datos_2014 <- datos_2014[,-1]

datos_2015 <- read.csv("./datos 2015.csv") 
datos_2015 <- datos_2015[, -1]

datos_2016 <- read.csv("./datos 2016.csv") 
datos_2016 <- datos_2016[, -1]


data <- datos_2016 # cambio aca para trabajar general

data$edad <- NULL

colSums(is.na(data))
sum(colSums(is.na(data)))

data <- data %>%
  filter(!is.na(region) & grepl("^\\d+$", as.character(region))) %>%
  mutate(region = as.numeric(as.character(region)))

for (i in 1:15) {
  va <- filter(data, region == i)
  
  # Calcular la moda
  moda <- as.numeric(names(sort(table(va$genero), decreasing = TRUE)[1]))
  
  # Reemplazar NA con la moda
  data <- data %>% 
    mutate(genero = ifelse(is.na(genero) & region == i, moda, genero))
}


table(addNA(data$genero))

# clima

for (i in 1:15) {
  va <- filter(data, region == i)
  
  # Calcular la moda
  moda <- as.numeric(names(sort(table(va$clima), decreasing = TRUE)[1]))
  
  # Reemplazar NA con la moda
  data <- data %>% 
    mutate(clima = ifelse(is.na(clima) | clima == 0 | clima == 99 & region == i, moda, clima))
}


table(addNA(data$clima))

# educ_padre

for (i in 1:15) {
  va <- filter(data, region == i)
  
  # Calcular la moda
  moda <- as.numeric(names(sort(table(va$educ_padre), decreasing = TRUE)[1]))
  
  # Reemplazar NA con la moda
  data <- data %>% 
    mutate(educ_padre = ifelse(is.na(educ_padre) | educ_padre == 0 | educ_padre > 20 & 
                                 region == i, moda, educ_padre))
}

table(addNA(data$educ_padre))

# educ_madre

for (i in 1:15) {
  va <- filter(data, region == i)
  
  # Calcular la moda
  moda <- as.numeric(names(sort(table(va$educ_madre), decreasing = TRUE)[1]))
  
  # Reemplazar NA con la moda
  data <- data %>% 
    mutate(educ_madre = ifelse(is.na(educ_madre) | educ_madre == 0 | educ_madre > 20 & 
                                 region == i, moda, educ_madre))
}

table(addNA(data$educ_madre))

# ingreso 

for (i in 1:15) {
  va <- filter(data, region == i)
  
  # Calcular la moda
  moda <- as.numeric(names(sort(table(va$ingreso), decreasing = TRUE)[1]))
  
  # Reemplazar NA con la moda
  data <- data %>% 
    mutate(ingreso = ifelse(is.na(ingreso) & region == i, moda, ingreso))
}

table(addNA(data$ingreso))

# dependencia

for (i in 1:15) {
  va <- filter(data, region == i)
  
  # Calcular la moda
  moda <- as.numeric(names(sort(table(va$dependencia), decreasing = TRUE)[1]))
  
  # Reemplazar NA con la moda
  data <- data %>% 
    mutate(dependencia = ifelse(is.na(dependencia) & region == i, moda, dependencia))
}

table(addNA(data$dependencia))

# rural

for (i in 1:15) {
  va <- filter(data, region == i)
  
  # Calcular la moda
  moda <- as.numeric(names(sort(table(va$rural), decreasing = TRUE)[1]))
  
  # Reemplazar NA con la moda
  data <- data %>% 
    mutate(rural = ifelse(is.na(rural) & region == i, moda, rural))
}

table(addNA(data$rural))

# est_indigena

table(addNA(data$est_indigena))

# matematica

colegios <- as.data.frame(unique(data$nombre_estab))

data$matematica <- as.numeric(data$matematica)

sum(is.na(data$matematica))

data <- data %>%
  group_by(nombre_estab) %>%
  mutate(matematica = if_else(is.finite(matematica), matematica, median(matematica, na.rm = TRUE)))

sum(is.na(data$matematica))

colegios_problemas <- data %>%
  group_by(nombre_estab) %>%
  summarize(na_count = sum(is.na(matematica))) %>%
  filter(na_count > 0)

mediana_global <- median(data$matematica, na.rm = TRUE)

data <- data %>%
  mutate(matematica = ifelse(is.na(matematica), mediana_global, matematica))

summary(data$matematica)
round(sd(data$matematica),2)

# lectura

data$lectura <- as.numeric(data$lectura)

sum(is.na(data$lectura))

data <- data %>%
  group_by(nombre_estab) %>%
  mutate(lectura = if_else(is.finite(lectura), lectura, median(lectura, na.rm = TRUE)))

sum(is.na(data$lectura)) 

mediana_global <- median(data$lectura, na.rm = TRUE)

data <- data %>%
  mutate(lectura = ifelse(is.na(lectura), mediana_global, lectura))

summary(data$lectura)
round(sd(data$lectura),2)

# mat_total

data$mat_total <- as.numeric(data$mat_total)

sum(is.na(data$mat_total))

data <- data %>%
  group_by(nombre_estab) %>%
  mutate(mat_total = if_else(is.finite(mat_total), mat_total, median(mat_total, na.rm = TRUE)))

mediana_global <- median(data$mat_total, na.rm = T)

data <- data %>%
  mutate(mat_total = ifelse(is.na(mat_total), mediana_global, mat_total))

summary(data$mat_total)
round(sd(data$mat_total),2)

# horas_dc_aula

data$horas_dc_aula <- as.numeric(data$horas_dc_aula)

sum(is.na(data$horas_dc_aula))

data <- data %>%
  group_by(nombre_estab) %>%
  mutate(horas_dc_aula = if_else(is.finite(horas_dc_aula), horas_dc_aula, 
                                 median(horas_dc_aula, na.rm = TRUE)))

mediana_global <- median(data$horas_dc_aula, na.rm = T)

data <- data %>%
  mutate(horas_dc_aula = ifelse(is.na(horas_dc_aula), mediana_global, horas_dc_aula))

summary(data$horas_dc_aula)
round(sd(data$horas_dc_aula),2)

# n_benef_sep

data$n_benef_sep <- as.numeric(data$n_benef_sep)

sum(is.na(data$n_benef_sep))

data <- data %>%
  group_by(nombre_estab) %>%
  mutate(n_benef_sep = if_else(is.finite(n_benef_sep), n_benef_sep, 
                                 median(n_benef_sep, na.rm = TRUE)))

mediana_global <- median(data$n_benef_sep, na.rm = T)

data <- data %>%
  mutate(n_benef_sep = ifelse(is.na(n_benef_sep), mediana_global, n_benef_sep))

summary(data$n_benef_sep)
round(sd(data$n_benef_sep),2)


#Solo para 2014
#valores_numericos <- numeric()
#valores_no_numericos <- 0 #1052

# Iterar a través de los valores de la columna, se demora 5 min
for (valor in data$n_benef_sep) {
  # Intentar convertir el valor a numérico
  valor_numeric <- as.numeric(valor)
  
  # Verificar si la conversión fue exitosa
  if (!is.na(valor_numeric)) {
    # Si es numérico, agregarlo al vector de valores numéricos
    valores_numericos <- c(valores_numericos, valor_numeric)
  } else {
    # Si no es numérico, agregarlo al vector de valores no numéricos
    valor <- 216
    valores_no_numericos <- valores_no_numericos + 1
    valores_numericos <- c(valores_numericos, valor)
  }
}

# Calcular la mediana de los valores numéricos
mediana_esp <- median(valores_numericos, na.rm = TRUE) #216

# Reemplazar los valores NA y no numéricos en la columna n_benef_sep
data$n_benef_sep <- valores_numericos

summary(data$n_benef_sep)
round(sd(data$n_benef_sep),2)

#export(data, "datos_2016_limpios.csv")

colSums(is.na(data))
