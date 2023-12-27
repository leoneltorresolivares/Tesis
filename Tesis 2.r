setwd("C:/Users/DELL/Desktop/Tesis")
library(clusterCrit)
library(mclust)
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(cluster)
library(readxl)
library(openxlsx)

datos_2014 <- read_csv("datos_2014.csv")
datos_2015 <- read_csv("datos_2015.csv")
datos_2016 <- read_csv("datos_2016.csv")
data <- datos_2014
# View(data)
# Se crea la columna con ID
data <- data[,-1]

# Cambiar los NA a 0
colSums(is.na(data))
sum(colSums(is.na(data)))

normalizar <- c("matematica","lectura","educ_padre","educ_madre","mat_total","horas_dc_aula",
                          "n_benef_sep")
                          
data_numerica <- data[, normalizar]
# View(data_numerica)
medias <- c()
sds <- c()
for (i in normalizar) {
  columna_numerica <- as.numeric(data_numerica[[i]])
  media <- mean(columna_numerica)
  desviacion_estandar <- sd(columna_numerica)
  data_numerica[[i]] <- (columna_numerica - media) / desviacion_estandar
  medias <- c(medias, media)
  sds <- c(sds, desviacion_estandar)
}
head(data_numerica)
data_categorica <- data[, c("genero", "clima","ingreso","dependencia","rural","est_indigena")]


set.seed(19990772) # Mi rut

# Pongo 4 cluster debido a que el recorrido del clima escolar es de 4
k <- 4
clusters <- kmeans(data_numerica, centers = k)



vector <- c()
for(i in 1:50){
  va <- kmeans(data_numerica, centers = i)
  va2 <- va$tot.withinss
  vector <- c(vector, va2)
}
plot(vector, type = "o", main = "Suma de cuadrados", 
xlab = "Numero de cluster", ylab = "Valor", pch = 19, col = "blue")
df <- cbind(seq(from = 1, to = 50, by = 1), vector)
names(df)[1:2] <- c("cluster","valor")
df <- as.data.frame(df)
names(df)
imagen <- ggplot(data = df, aes(x = V1, y = vector)) +
   ggtitle("Suma de los errores modelo k-means 2014") +
   xlab("Número de clusters") +
   ylab("Valor de los errores") + 
  geom_point(pch = 10, col = "blue") + 
  theme(plot.background = element_rect(color = "black", # Color del borde
                                       size = 2))
png("C:/Users/DELL/Desktop/Tesis/imagen_clusters.png", width = 800, height = 600)
print(imagen)  # Imprime el gráfico en el dispositivo PNG
dev.off()

# gausian <- Mclust(data_numerica, G = k)

pca_resultado <- prcomp(data_numerica, scale. = F)
componentes_principales <- pca_resultado$x[, 1:2]

# Crear un data frame con las componentes principales y la información de cluster
datos_pca <- data.frame(
  PC1 = componentes_principales[, 1],
  PC2 = componentes_principales[, 2],
  Cluster = factor(clusters$cluster)
)

mi_grafico <- ggplot(datos_pca, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() +
  labs(title = "PCA de los Resultados k-Means") +
  xlab("Componente 1")
  ylab("Componente 2")
# summary(pca_resultado)

# png("C:/Users/DELL/Desktop/Tesis/grafico_kmeans2016.png", width = 800, height = 600)
# print(mi_grafico)  # Imprime el gráfico en el dispositivo PNG
# dev.off()

# Ahora hagamos los cruces con las categoricas

datos_means <- as.data.frame(clusters$cluster) # El cluster de cada observaciones
datos_means <- cbind(datos_means, data_categorica) # Union
head(datos_means)
# Desnormalizar
head(data_numerica)
for (i in 1:length(medias)) {
  media <- medias[i]
  desviacion_estandar <- sds[i]
  data_numerica[i] <- (data_numerica[i] * desviacion_estandar) + media
}
head(data_numerica)
datos_means <- cbind(datos_means, data_numerica)
head(datos_means)

# Exporta los datos
write.xlsx(datos_means, "C:/Users/DELL/Desktop/Tesis/datos_2016_cluster.xlsx")


DF <- as.data.frame(read_excel("DF_gower.xlsx"))
head(DF)
summary(datos_means)
summary(DF)
names(datos_means)[1] <- "cluster"

DF_numericas <- DF[,c("matematica","lectura","mat_total",
"horas_dc_aula","n_benef_sep")]
head(DF_numericas)
str(DF_numericas)
pca_resultado <- prcomp(DF_numericas, scale. = T)
componentes_principales <- pca_resultado$x[, 1:2]
# Crear un data frame con las componentes principales y la información de cluster
datos_pca <- data.frame(
  PC1 = componentes_principales[, 1],
  PC2 = componentes_principales[, 2],
  Cluster = factor(DF$labels_SC)
)
# View(datos_pca)
mi_grafico <- ggplot(datos_pca, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() +
  labs(title = "PCA de los Resultados Clustering Spectral")
  xlab("Componente 1")
  ylab("Componente 2")

png("C:/Users/DELL/Desktop/Tesis/grafico_CS.png", width = 800, height = 600)
print(mi_grafico)  # Imprime el gráfico en el dispositivo PNG
dev.off()
