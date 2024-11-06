# Cargar los paquetes necesarios
library(readr)
library(readxl)
library(dplyr)

# Leer la base de datos principal y el diccionario de variables
bbddAECM <- read_csv("BBDD/bbddAECM_07_10_24.csv")
variables <- read_excel("BBDD/Variables BBDD_Ariel.xlsx")

# Crear listas de columnas agrupadas por 'Grup variables'
listas_agrupadas <- split(variables$`Cod. variables`, variables$`Grup variables`)

# Generar un data.frame separado para cada grupo en 'Grup variables'
for (nombre_grupo in names(listas_agrupadas)) {
  columnas_grupo <- listas_agrupadas[[nombre_grupo]]
  
  # Filtrar las columnas que existen en bbddAECM
  columnas_validas <- columnas_grupo[columnas_grupo %in% colnames(bbddAECM)]
  
  # Solo crear el data.frame si hay columnas válidas en el grupo
  if (length(columnas_validas) > 0) {
    datos_grupo <- bbddAECM %>% select(any_of(columnas_validas))
    
    # Asignar el data.frame como un objeto independiente en el entorno global con el nombre del grupo
    assign(nombre_grupo, datos_grupo)
  } else {
    message(paste("No se encontraron columnas válidas para el grupo:", nombre_grupo))
  }
}

# Los data.frames estarán disponibles como objetos independientes según el nombre de cada grupo en 'Grup variables'


# Extraer los nombres de las columnas en bbddAECM
columnas_bbdd <- colnames(bbddAECM)

# Filtrar el diccionario de variables para encontrar coincidencias
coincidencias <- variables %>%
  filter(`Cod. variables` %in% columnas_bbdd) %>%
  select(`Grup variables`, `Cod. variables`) %>%
  rename(Descripcion = `Grup variables`, Columna = `Cod. variables`)


# ---------------------- PRIMER MODELO NAIVE ----------------------------------
# Cargar los paquetes necesarios
library(dplyr)
library(xgboost)

# Supongamos que tienes varios data.frames separados (ej., `df1`, `df2`, `df3`) que quieres unir
# Primero, une los data.frames que seleccionaste
df_combined <- bind_cols(Comorbilitats, Sociodemographic, `Neuropsychological test`)  # Ajusta los nombres de los data.frames según corresponda

# Asegúrate de que el data.frame combinado no tenga columnas duplicadas
df_combined <- df_combined %>% 
  select(!duplicated(names(df_combined)))

str(df_combined)
# Definir la columna objetivo (target) para clasificación multinomial
# Supongamos que la columna objetivo tiene valores de clase como 0, 1, 2, ...
target <- as.numeric(df_combined$`N) - 1  # Ajusta para que esté en el rango 0, 1, 2 para XGBoost
df_combined <- df_combined %>% select(-target)  # Remover la columna objetivo del conjunto de características

# Convertir el conjunto de datos a una matriz para XGBoost
data_matrix <- as.matrix(df_combined)

# Crear la estructura DMatrix para XGBoost con la columna objetivo
dtrain <- xgb.DMatrix(data = data_matrix, label = target)

# Ajustar un modelo de clasificación multinomial usando XGBoost
params <- list(
  objective = "multi:softmax",  # Para predicciones de clase directa, o "multi:softprob" para probabilidades
  num_class = length(unique(target)),  # Especifica el número de clases en la columna objetivo
  eval_metric = "mlogloss"  # Métrica de evaluación para clasificación multinomial
)

# Entrenar el modelo
modelo_xgb <- xgboost(data = dtrain, params = params, nrounds = 100)

# Imprimir un resumen del modelo
print(modelo_xgb)