setwd("C:/Users/DELL/Desktop/Tesis")
library(dplyr)
library(rio) #Para exportar los datos a excel
library(readr)
Data_Simce <- read.csv("C:/Users/DELL/Desktop/Tesis/Data_Simce.csv", stringsAsFactors=F)
BD <- Data_Simce
sum(is.na(BD)) #4.732.168
#Cuantos na hay en cada columna, MARGIN = 1 para filas, MARING = 2 columnas
a <- apply(X = is.na(BD), MARGIN = 1, FUN = sum)
sum(c(a) == 0) #179965 filas sin NA
a <- apply(X = is.na(BD), MARGIN = 2, FUN = sum)
sum(c(a) == 0) #4 columnas sin valores NA
rm(a)


#### PROBLEMAS CON VARIABLE AGNO ####

table(as.integer(Data_Simce$agno))

x_nonum <- which(is.na(as.numeric(BD$agno))) 
BD$agno[x_nonum] 
BD2 <- BD %>% slice(x_nonum) # BD2 filas con agno incorrecto

BD3 <- BD2 %>% filter(agno == "") # BD3 filas con agno vacio
table(BD3$agno)

BD4 <- BD2 %>% filter(agno != "") # BD4 filas con agno mal separado
library(stringr)
BD4[as.character(seq(1,36))] <- str_split_fixed(BD4$agno, ',', 36)
BD4 <- BD4[as.character(seq(1,36))] # BD4 filas agno bien separadas

BD5 <- BD %>%  filter(!row_number() %in% x_nonum) # BD5 filas con agno correcto

colnames(BD4) <- colnames(BD5)

BD6 <- rbind(BD5, BD4) # agno completamente corregido

BD <- BD6

rm(x_nonum)
rm(BD2)
rm(BD3)
rm(BD4)
rm(BD5)
rm(BD6)
######### Separar por año #####
repitencia <- subset(BD, !is.na(BD$repitencia))
sum(repitencia$repitencia == 1) #334
sum(repitencia$repitencia == 0) #8880
borrar <- c("grado","mrun","idalumno","comuna", "migrante", "n_aprob_sing", 
            "convenio_pie","pago_matricula","pago_mensual","n_prior_sep","repitencia")
BD2 <- BD[,!(names(BD) %in% borrar)]

dos_mil_diecisiete <- BD2[BD2$agno=="2017",]
dos_mil_dieciseis <- BD2[BD2$agno=="2016",]
dos_mil_quince <- BD2[BD2$agno=="2015",]
dos_mil_catorce <- BD2[BD2$agno=="2014",]
### WIII
nrow(dos_mil_catorce)+nrow(dos_mil_quince)+nrow(dos_mil_dieciseis)+nrow(dos_mil_diecisiete)-nrow(BD)

#Para exportar
write.csv(dos_mil_catorce, "datos 2014.csv")
write.csv(dos_mil_quince, "datos 2015.csv")
write.csv(dos_mil_dieciseis, "datos 2016.csv")
#write.csv(dos_mil_diecisiete, "datos 2017.csv")
rm(repitencia, borrar)

datos_2014 <- read_csv("datos 2014.csv")
datos_2014 <- datos_2014[,-1]






sum(is.na(datos_2014$matematica))

datos_2014 <- datos_2014 %>% # REEMPLAZO POR LA MEDIANA
  mutate(matematica = ifelse(is.na(matematica), median(matematica, na.rm = TRUE), matematica))

sum(is.na(datos_2014$lectura)) #31896

datos_2014 <- datos_2014 %>% # REEMPLAZO POR LA MEDIANA
  mutate(lectura = ifelse(is.na(lectura), median(lectura, na.rm = TRUE), lectura))


#Genero
table(addNA(datos_2014$genero))

sum(is.na(datos_2014$genero)) 

moda_genero <- as.numeric(names(sort(table(datos_2014$genero), decreasing = TRUE)[1]))

datos_2014 <- datos_2014 %>%
  mutate(genero = ifelse(is.na(genero), moda_genero, genero))

#CLIMA
table(addNA(datos_2014$clima))
sum(is.na(datos_2014$clima) | datos_2014$clima == 0 | datos_2014$clima == 99)
clima_sin_na <- subset(datos_2014, !is.na(datos_2014$clima))
vector <- c(sum(clima_sin_na$clima == 1),sum(clima_sin_na$clima == 2),sum(clima_sin_na$clima == 3),
            sum(clima_sin_na$clima == 4)) 
datos_2014$clima[is.na(datos_2014$clima)] <- 2
datos_2014$clima[datos_2014$clima == 99] <- 2
datos_2014$clima[datos_2014$clima == 0] <- 2
rm(clima_sin_na,vector)

#EDUC PADRE
table(addNA(datos_2014$educ_padre))

sum(is.na(datos_2014$educ_padre) | datos_2014$educ_padre == 0 | datos_2014$educ_padre > 20) 
moda_educ_padre <- as.numeric(names(sort(table(datos_2014$educ_padre), decreasing = TRUE)[1]))

datos_2014 <- datos_2014 %>%
  mutate(educ_padre = ifelse(is.na(
    educ_padre) | educ_padre == 0 | educ_padre > 20, moda_educ_padre, educ_padre))

#Educ madre
table(addNA(datos_2014$educ_madre))

sum(is.na(datos_2014$educ_madre) | datos_2014$educ_madre == 0 | datos_2014$educ_madre > 20)

moda_educ_madre <- as.numeric(names(sort(table(datos_2014$educ_madre), decreasing = TRUE)[1]))

datos_2014 <- datos_2014 %>%
  mutate(educ_madre = ifelse(is.na(educ_madre) | educ_madre == 0 | educ_madre > 20, 
                             moda_educ_madre, educ_madre))

#Ingreso
table(addNA(datos_2014$ingreso))

sum(is.na(datos_2014$ingreso))

moda_ingreso <- as.numeric(names(sort(table(datos_2014$ingreso), decreasing = TRUE)[1]))

datos_2014 <- datos_2014 %>%
  mutate(ingreso = ifelse(is.na(ingreso), moda_ingreso, ingreso))

#Dependencia
va <- c("1","2","3","4")
sum(datos_2014$dependencia %in% va)
nrow(datos_2014) - sum(datos_2014$dependencia %in% va)
depe <- subset(datos_2014, !is.na(datos_2014$dependencia))
nrow(datos_2014)-nrow(depe)
vector <- c()
for(i in 1:4){
  vector <- c(vector, sum(depe$dependencia == as.character(i)))
}
indice <- which.max(vector)
datos_2014$dependencia[is.na(datos_2014$dependencia)] <- indice
condicion <- !(datos_2014$dependencia %in% c("1", "2", "3", "4"))
valor_reemplazo <- as.character(indice)
datos_2014$dependencia <- ifelse(condicion, valor_reemplazo, datos_2014$dependencia)
datos_2014$dependencia <- as.numeric(datos_2014$dependencia)
rm(depe,vector,indice,condicion,valor_reemplazo,va)

#Rural
table(addNA(datos_2014$rural)) 

moda_rural <- as.numeric(names(sort(table(datos_2014$rural), decreasing = TRUE)[1]))

datos_2014 <- datos_2014 %>%
  mutate(rural = ifelse(is.na(rural) | rural > 1, moda_rural, rural))

#Estado indigena
indigena <- subset(datos_2014, !is.na(datos_2014$est_indigena))
sum(is.na(datos_2014$est_indigena))+sum(indigena$est_indigena < 0)
vector <- c(sum(indigena$est_indigena == 0), sum(indigena$est_indigena == 1)) #0
datos_2014$est_indigena[is.na(datos_2014$est_indigena)] <- 0
datos_2014$est_indigena[datos_2014$est_indigena < 0] <- 0
rm(indigena)

#Matricula total

sum(is.na(datos_2014$mat_total)) #100
mediana <- as.numeric(median(datos_2014$mat_total, na.rm = T))
datos_2014$mat_total[is.na(datos_2014$mat_total)] <- mediana

#Horas aula

sum(is.na(datos_2014$horas_dc_aula)) #108
mediana <- as.numeric(median(datos_2014$horas_dc_aula, na.rm = T))
datos_2014$horas_dc_aula[is.na(datos_2014$horas_dc_aula)] <- mediana

#Benefi sep

#mediana <- as.numeric(median(datos_2014$n_benef_sep, na.rm = T))
#datos_2014$n_benef_sep[is.na(datos_2014$n_benef_sep)] <- mediana
#No me funciono, hay palabras dentro de los valores que no pueden pasarse a numero

# Crear vectores para almacenar los valores numéricos y no numéricos
valores_numericos <- numeric()
valores_no_numericos <- 0 #1052

# Iterar a través de los valores de la columna, se demora 5 min
for (valor in datos_2014$n_benef_sep) {
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
datos_2014$n_benef_sep <- valores_numericos

colSums(is.na(datos_2014))
#export(datos_2014, "datos_2014_limpios.csv")














