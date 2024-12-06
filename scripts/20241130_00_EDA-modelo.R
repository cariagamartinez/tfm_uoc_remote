# Librerías
library(readxl)
library(fastDummies)
library(psych)
library(xgboost)
library(tidyverse)
library(DataExplorer)
library(naniar)
library(corrplot)
library(caret)
library(GGally)
library(DMwR2)
library(ROSE)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(ggfortify)
library(ggrepel)
library(MASS) #LDA


# Leer la base de datos principal y el diccionario de variables
bbddAECM <- read_csv("BBDD/bbddAECM_07_10_24.csv")
variables <- read_excel("BBDD/Variables BBDD_Ariel.xlsx")

# Características del dataset
str(bbddAECM)
summary(bbddAECM)

# Crear listas de columnas agrupadas por 'Group variables'
listas_agrupadas <- split(variables$`Cod. variables`, variables$`Grup variables`)

# Generar un dataframe separado para cada grupo en 'Group variables' y crear versiones dummyficadas
for (nombre_grupo in names(listas_agrupadas)) {
  columnas_grupo <- listas_agrupadas[[nombre_grupo]]
  
  # Filtrar las columnas que existen en bbddAECM
  columnas_validas <- columnas_grupo[columnas_grupo %in% colnames(bbddAECM)]
  
  # Solo crear el dataframe si hay columnas válidas en el grupo
  if (length(columnas_validas) > 0) {
    # Crear el data.frame sin dummyficar
    datos_grupo <- bbddAECM %>% dplyr::select(any_of(columnas_validas))
    
    # Asignar el dataframe sin dummyficar como un objeto independiente en el entorno global
    assign(nombre_grupo, datos_grupo)
    
    # Dummyficar el dataframe creado
    datos_grupo_dummy <- dummy_cols(datos_grupo, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
    
    # Asignar el dataframe dummyficado como un objeto independiente en el entorno global con el sufijo "_dummy"
    assign(paste0(nombre_grupo, "_dummy"), datos_grupo_dummy)
  } else {
    message(paste("No se encontraron columnas válidas para el grupo:", nombre_grupo))
  }
}

# Resultado: para cada grupo, habrá dos dataframes en el entorno global:
# - Uno sin dummyficar, con el nombre original del grupo
# - Uno dummyficado, con el nombre del grupo y el sufijo "_dummy"

# Los dataframes estarán disponibles como objetos independientes según el nombre de cada grupo en 'Group variables'

# Extraer los nombres de las columnas en bbddAECM
columnas_bbdd <- colnames(bbddAECM)

# Filtrar el diccionario de variables para encontrar coincidencias
coincidencias <- variables %>%
  filter(`Cod. variables` %in% columnas_bbdd) %>%
  dplyr::select(`Grup variables`, `Cod. variables`) %>%
  rename(Descripcion = `Grup variables`, Columna = `Cod. variables`)


# ----------------------
#GENERAR EL DATASET DE INTERÉS 
# ----------------------

#La idea de este apartado es ver qué tipos de dataset de análisis queremos generar y utilizar
#la variable target de interés.

# Primero, unir los dataframes seleccionados, por ejemplo, en este caso, usaremos 
# los dummys de comorbilidad, sociodemográficos y tests neuropsicológicos.
df_combined <- bind_cols(Comorbilitats_dummy, Sociodemographic_dummy, `Neuropsychological test_dummy`)

# Luego crearemos la variable target
Inclusion$cluster <- factor(Inclusion$cluster, levels = c("No COVID", "No LongCOVID", "LongCOVID NoCog", "LongCOVID Cog"))

# Convertir el factor a numérico comenzando desde 0 (así codificamos como 0=No COVID, etc)
Inclusion$cluster_cod <- as.numeric(Inclusion$cluster) - 1

# Convertir la columna objetivo en valores numéricos y agregarla como Target
df_combined <- bind_cols(df_combined, Target = Inclusion$cluster_cod)

#Seleccionamos NoCOVID vs LongCovid Cog (ASÍ ME DAN POCOS CEROS)
df_combined <- df_combined %>% filter(df_combined$Target %in% c(0, 3))
df_combined <- df_combined %>%
  mutate(Target = recode(Target, `3` = 1))  # Cambiar 3 a 1, dejando 0 como está: ahora 1 será "LongCOVID Cog"

# ------------------------------------------------------------------------------

# ----------------------
# Análisis general del dataset generado
# ----------------------

# Ver tipos de variables y estructura
str(df_combined)

# Resumen estadístico
summary(df_combined)

# Ver las dimensiones y primeras filas
cat("Dimensiones del dataset:", dim(df_combined), "\n")

# ----------------------
# Valores faltantes
# ----------------------

# Resumen de valores faltantes
gg_miss_var(df_combined, show_pct = TRUE)
#vis_miss(df_combined)  # Visualización de valores faltantes
#aggr(df_combined, col = c("navyblue", "red"), numbers = TRUE, sortVars = TRUE)

# Vamos a hacer una imputación con kNN
# Excluir columnas específicas
excluded_cols <- c("Target")

# Separar las columnas excluidas
excluded_data <- df_combined %>% 
  dplyr::select(all_of(excluded_cols))

# Convertir todas las columnas numéricas a tipo double
data_for_imputation <- df_combined %>% dplyr::select(-all_of(excluded_cols))
data_for_imputation <- data_for_imputation %>%
  mutate(across(where(is.numeric), as.numeric))

# Aplicar imputación
data_imputed <- knnImputation(data_for_imputation, k = 10)

# Combinar las columnas imputadas con las excluidas
df_combined_clean <- bind_cols(data_imputed, excluded_data)

# Gráfico tras imputación
gg_miss_var(df_combined_clean, show_pct = TRUE)

# ----------------------
# Balancear el dataset
# ----------------------

# Mantenemos el dataset original sin NAs que se llama df_combined_clean
# y aquí simplemente hacemos una primera prueba de balanceo manual.
# Me parece que el balanceo está siendo un poco excesivo así que 
# si es necesario se puede cambiar el nombre del dataset en el modelo por este
# dataset balanceo

#Aquí remodificamos los nombres para evitar problemas
df_combined_clean <- df_combined_clean %>% rename_with(make.names)

#Aplicamos ROSE: OJO: DATOS SINTÉTICOS
#df_combined_clean_balanced <- ROSE(Target ~ ., data = df_combined_clean, seed = 123, method = "under")$data

# Oversampling manual
minority_class <- df_combined_clean %>% filter(Target == "0")
oversampled <- minority_class %>% sample_n(size = nrow(df_combined_clean[df_combined_clean$Target == "1", ]), replace = TRUE)

# Combinar con la clase mayoritaria
df_combined_clean_balanced <- bind_rows(
  df_combined_clean %>% filter(Target == "1"),
  oversampled
)

table(df_combined_clean_balanced$Target)

# ----------------------
# Análisis univariado
# ----------------------

# a) Seleccionar variables numéricas
numeric_vars <- df_combined_clean %>% dplyr::select(where(is.numeric))

# Histogramas para variables numéricas
numeric_vars %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribuciones de variables numéricas")

# b) Variables categóricas
# Seleccionar columnas específicas y transformarlas a factor: mantenemos el nombre cambiado
# para evitar afectar el dataset generado con columnas numéricas que usaremos en los modelos
df_combined_clean_factors <- df_combined_clean %>%
  mutate(across(
    where(~ is.numeric(.) && all(. %in% c(0, 1)) && n_distinct(.) == 2),
    as.factor
  ))

# b.1) Selección de variables categóricas
categorical_vars <- df_combined_clean_factors %>% dplyr::select(where(is.factor))

# Gráficos de barras para variables categóricas
categorical_vars %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value, fill = Variable)) +
  geom_bar() +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribuciones de variables categóricas")

# ----------------------
# Correlación y relaciones multivariadas
# ----------------------

# a) Correlación entre variables numéricas
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "full", tl.cex = 0.7, tl.srt = 45)

# Otras formas de visualización más adecuadas ----------------------------------

# Convertir la matriz de correlación en formato largo
cor_long <- melt(cor_matrix)

# Graficar con ggplot2
ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8)
  ) +
  labs(title = "Matriz de correlación completa", x = "", y = "")


# Dividir las variables en grupos para ver las correlaciones --> ESTO FUNCIONA BIEN
num_vars <- ncol(cor_matrix)
block_size <- 20  # Tamaño del bloque
for (i in seq(1, num_vars, by = block_size)) {
  cor_block <- cor_matrix[i:min(i + block_size - 1, num_vars), i:min(i + block_size - 1, num_vars)]
  corrplot(cor_block, method = "color", type = "full", tl.cex = 0.7, title = paste("Variables", i, "a", min(i + block_size - 1, num_vars)))
}


# b) Relaciones numéricas-categóricas
# Ajustando "Target" como variable objetivo
if ("Target" %in% colnames(df_combined_clean)) {
  df_combined_clean %>%
    mutate(Target = as.factor(Target)) %>% #Considerando que Target es binaria
    gather(key = "Variable", value = "Value", -Target) %>%
    filter(!is.na(Value)) %>%
    ggplot(aes(x = Target, y = Value)) +
    geom_boxplot() +
    facet_wrap(~ Variable, scales = "free") +
    theme_minimal() +
    labs(title = "Relaciones entre variables y Target")
}

# ----------------------
# Reducción de dimensionalidad
# ----------------------

# a) Eliminar variables con baja variabilidad: esto es "automático" con el paquete caret
nzv <- nearZeroVar(df_combined_clean, saveMetrics = TRUE)
low_variability_vars <- rownames(nzv[nzv$nzv == TRUE, ])
cat("Variables con baja variabilidad eliminadas:", low_variability_vars, "\n")
df_combined_clean_low_var <- df_combined_clean %>% dplyr::select(-all_of(low_variability_vars))

# b) Análisis de componentes principales (PCA)

pca_data_scaled <- scale(df_combined_clean)

pca <- prcomp(pca_data_scaled, center = TRUE)
summary(pca)  # Proporción de la varianza explicada

# CONCLUSIÓN: NECESITAMOS UNAS 30 COMPONENTES PARA ALCANZAR EL 85% DE VARIANZA EXPLICADA

# Visualización compleja
biplot(pca, scale = 0)

# Visualización con ggplot2 (biplot)
autoplot(pca, data = df_combined_clean, 
         loadings = TRUE, 
         loadings.label = TRUE, 
         loadings.label.size = 3) +
  theme_minimal() +
  labs(title = "PCA (Biplot) con ggplot2",
       x = "PC1",
       y = "PC2")

# Obtener las cargas
loadings <- as.data.frame(pca$rotation[, 1:2])  # Cargas de PC1 y PC2
loadings$Variable <- rownames(loadings)

# Para construir las ecuaciones usando los componentes
rotation_matrix <- pca$rotation
components <- as.data.frame(rotation_matrix)

# Filtrar las variables con las mayores contribuciones
top_variables <- loadings %>%
  mutate(Contribution = sqrt(PC1^2 + PC2^2)) %>%
  arrange(desc(Contribution)) %>%
  dplyr::slice(1:30)  # Seleccionar las variables con mayor contribución

# Crear un biplot con solo las variables seleccionadas
autoplot(pca, 
         data = df_combined_clean, 
         loadings = TRUE, 
         loadings.label = TRUE, 
         loadings.label.size = 3) +
  geom_segment(data = top_variables,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text(data = top_variables,
            aes(x = PC1, y = PC2, label = Variable),
            hjust = -0.2, vjust = -0.2, color = "blue") +
  theme_minimal()

# Visualzación más sencilla
fviz_pca_ind(pca, 
             habillage = df_combined_clean$Target,
             label = "none", 
             repel = TRUE, 
             title = "Gráfico de PCA (Target como variable suplementaria)")

# Otra visualización de PCA más sencilla agregando elipses de confianza alrededor 
#de cada grupo para visualizar mejor las diferencias y la superposición entre ellos.
fviz_pca_ind(pca, 
             habillage = df_combined_clean$Target, 
             label = "none", 
             addEllipses = TRUE,  # Agrega elipses para resaltar los grupos
             ellipse.type = "confidence",  # Tipo de elipse (ej, "t", "norm")
             geom.ind = "point",  # Solo puntos para los individuos
             palette = "jco",     # Paleta de colores
             repel = TRUE,
             title = "Gráfico PCA con elipses por grupos")
# No parece mejorar gran cosa...

# Otra opción es, tras seleccionar las variables que explican el 80% de la varianza, generar un nuevo PCA
selected_vars <- top_variables$Variable  # Asumiendo que "Variable" contiene los nombres de las columnas

# Crear un nuevo dataset con solo las variables seleccionadas
df_reduced <- df_combined_clean %>% 
  dplyr::select(all_of(selected_vars))

# Añadir la variable Target para la visualización
df_reduced <- df_reduced %>%
  mutate(Target = df_combined_clean$Target)

# Recalcular el PCA con las variables filtradas
pca_reduced <- prcomp(dplyr::select(df_reduced, -Target), center = TRUE, scale. = TRUE)

biplot(pca_reduced)

# Visualizar el PCA con las variables seleccionadas
fviz_pca_ind(pca_reduced, 
             habillage = df_reduced$Target,  # Grupos según Target
             label = "none", 
             repel = TRUE, 
             title = "Gráfico de PCA con las variables que contribuyen al 80% de la varianza")

# Biplot con repel (ggrepel)

# Crear un biplot con etiquetas que no se solapen
autoplot(pca_reduced, data = df_combined_clean, loadings = TRUE, loadings.label = FALSE) +
  geom_segment(data = top_variables,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(data = top_variables,
                  aes(x = PC1, y = PC2, label = Variable),
                  size = 3, color = "blue") +
  theme_minimal() +
  labs(title = "Biplot sin solape de etiquetas (ggrepel)",
       x = "PC1",
       y = "PC2")

# ------------------------------------------------------------------------------
#ESTE GRÁFICO CON CLÚSTERES KNN ES PROMETEDOR

# Realizar clustering k-means en las observaciones
set.seed(123)
clusters <- kmeans(pca$x[, 1:2], centers = 3)  # Cambiar centers para ir variando el número de centros detectados

# Crear un dataframe con los clústeres y PCs
pca_clustered <- as.data.frame(pca$x[, 1:2])
pca_clustered$Cluster <- as.factor(clusters$cluster)

# Graficar con los centros de los clusters
ggplot(pca_clustered, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.7) +
  geom_point(data = as.data.frame(clusters$centers), aes(x = PC1, y = PC2), 
             color = "black", size = 4, shape = 8) +
  theme_minimal() +
  labs(title = "PCA con clústeres",
       x = "PC1",
       y = "PC2")

# ----------------------
# Clustering para patrones
# ----------------------
# Matriz de distancias y dendrograma
dist_matrix <- dist(scale(numeric_vars))
hclust_model <- hclust(dist_matrix, method = "ward.D2")
plot(hclust_model, main = "Dendrograma de clustering Jerárquico")

# Cortar el dendrograma en 2 clústeres
clusters <- cutree(hclust_model, k = 2)  # Cambiar "k" al número de clústeres sobre el que se cortará

# Ver asignaciones de clústeres para las observaciones
print(clusters)

# Asociar observaciones con cada clúster
cluster_observations <- split(row.names(numeric_vars), clusters)

# Imprimir observaciones de cada clúster
print("Observaciones en el clúster 1:")
print(cluster_observations[[1]])
print("Observaciones en el clúster 2:")
print(cluster_observations[[2]])

# Filtrar datos por clústeres si se desea trabajar con subconjuntos
cluster_1 <- numeric_vars[clusters == 1, ]
cluster_2 <- numeric_vars[clusters == 2, ]

# Mostrar los subconjuntos
print("Datos del clúster 1:")
print(cluster_1)
print("Datos del clúster 2:")
print(cluster_2)

# Agregar la asignación de clústeres como columna en los datos originales
numeric_vars$cluster <- clusters

# Visualizar los datos con la columna de clúster
print("Datos originales con asignación de clúster:")
print(head(numeric_vars))

# Opcional: Resumen por clúster
cluster_summary <- aggregate(. ~ cluster, data = numeric_vars, mean)
print("Resumen por clúster:")
print(cluster_summary)

# Opcional: Agregar rectángulos al dendrograma para visualizar los clústeres
plot(hclust_model, main = "Dendrograma con clústeres")
rect.hclust(hclust_model, k = 2, border = "red")  # Cambia "k" si es necesario

# ------------------------------------------------------------------------------

# ANOVA para cada variable numérica: WORK IN PROGRESS

# # Renombrar las columnas del dataframe para que sean válidas en fórmulas
# # colnames(df_combined_clean) <- make.names(colnames(df_combined_clean))
# # # 
# # # Seleccionar variables numéricas
# # df_combined_clean_anova <- df_combined_clean %>% select_if(is.numeric)
# # df_combined_clean_anova$Target <- as.factor(df_combined_clean_anova$Target)
# numeric_vars_anova <- df_combined_clean_anova %>% select(where(is.numeric))
# 
# # Seleccionar únicamente las variables numéricas
# numeric_vars_anova <- df_combined_clean %>%
#   select(where(is.numeric))  # Solo las columnas numéricas
# 
# # Convertir Target a factor en el dataframe original
# df_combined_clean$Target <- as.factor(df_combined_clean$Target)
# 
# # Realizar ANOVA para cada variable numérica contra Target
# anova_results <- sapply(colnames(numeric_vars_anova), function(var) {
#   formula <- as.formula(paste(var, "~ Target"))
#   summary(aov(formula, data = df_combined_clean_anova))[[1]][["Pr(>F)"]][1]
# })
# 
# # Convertir resultados a un data.frame para inspección
# anova_results_df <- data.frame(
#   Variable = colnames(numeric_vars),
#   p_value = anova_results
# )
# 
# # Aplicar corrección de Bonferroni
# bonferroni_corrected <- p.adjust(anova_results, method = "bonferroni")
# 
# # Ver variables con diferencias significativas después de Bonferroni
# significant_vars_bonferroni <- names(bonferroni_corrected[bonferroni_corrected < 0.05])
# print("Variables con diferencias significativas (ANOVA + Bonferroni):")
# print(significant_vars_bonferroni)

# ----------------------
# LDA
# ----------------------

lda_model <- lda(Target ~ ., data = df_combined_clean)

# Resumen del modelo
print(lda_model)

plot(lda_model)

# Predecir en los datos
lda_pred <- predict(lda_model)

# Crear matriz de confusión con caret
confusionMatrix(factor(lda_pred$class), factor(df_combined_clean$Target))

# # Comprobación de supuestos LDA
# # Dividir los datos por clases de la variable Target
# classes <- split(df_combined_clean[, -which(names(df_combined_clean) == "Target")], df_combined_clean$Target)
# 
# # Test de Shapiro-Wilk para cada variable en cada clase
# normality_tests <- lapply(classes, function(df) apply(df, 2, shapiro.test))
# 
# # Mostrar los resultados
# normality_tests