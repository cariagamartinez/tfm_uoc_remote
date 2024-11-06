

#Cargamos librerías

library(tidyverse)
library(mice)
library(naniar)
library(missForest)


#Carga del dataset global
datos <- read.csv("BBDD/bbddAECM_07_10_24.csv")

md.pattern(datos, rotate.names = T)


# Convertir valores vacíos a NA en todo el dataset usando dplyr
datos <- datos %>%
  mutate(across(everything(), ~ ifelse(. == "", NA, .)))


glimpse(datos)

# Realizar el test MCAR
#mcar_test_result <- mcar_test(datos)

# Mostrar el resultado
#print(mcar_test_result)

#Generación de subsets de interés

# datos_dem <- datos %>% 
#   select(sex, ag, el)

#Ver la lista de valores únicos
#valores_unicos <- lapply(datos, unique)


# # Crear una tabla de codificación de niveles y códigos únicos para cada columna de texto
# codificacion <- lapply(datos, function(x) {
#   if (is.character(x)) {
#     factor_x <- factor(x)
#     data.frame(
#       Nivel = levels(factor_x),                    # Niveles únicos
#       Codigo = seq_along(levels(factor_x))         # Códigos numéricos únicos
#     )
#   } else {
#     NULL
#   }
# })
# 
# # Imprimir la tabla de codificación de cada columna
# codificacion

# Convertir valores vacíos a NA y luego convertir columnas character a factores numéricos, manteniendo los NA originales
datos_numericos <- datos %>%
  mutate(across(everything(), ~ ifelse(. == "", NA, .))) %>%  # Paso 1: reemplazar "" con NA
  mutate(across(where(is.character), ~ ifelse(is.na(.), NA, as.numeric(factor(.)))))

md.pattern(datos_numericos, rotate.names = T)

# Total de observaciones no nulas en todo el dataset
total_observaciones_no_na <- sum(!is.na(datos))
print(total_observaciones_no_na)

# Total de observaciones (incluyendo NA) en todo el dataset
total_observaciones <- nrow(datos) * ncol(datos)
print(total_observaciones)

#Contar el total de NAs (852)
total_na <- sum(is.na(datos))
print(total_na)

total_na <- sum(is.na(datos_numericos))
print(total_na)

#Ver posibles desbalances
# Filtrar solo las columnas que son character
datos_char <- datos %>%
  select(where(is.character))

# Convertir el data.frame filtrado a formato largo y contar frecuencias
frecuencias_df <- datos_char %>%
  pivot_longer(cols = everything(), names_to = "columna", values_to = "valor") %>%
  count(columna, valor, name = "frecuencia")

# Ver frecuencias por columna
print(frecuencias_df)


# Aplicar el método missForest a tu dataset
imputacion <- missForest(datos_numericos, ntree = 200, maxiter = 5)
datos_imputados <- imputacion$ximp
# Ver el dataset imputado
head(datos_imputados)

# Ver la tasa de error de la imputación
imputacion$OOBerror