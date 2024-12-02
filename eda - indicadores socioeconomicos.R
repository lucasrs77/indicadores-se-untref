
load("backup_data.RData")

# Carga de librerías necesarias
library(tidyverse)
library(haven)
library(eph)

##### 1. Revisar la estructura general de los datos #####

#Esto incluye nombres de columnas, tipos de datos y un vistazo rápido a los valores.

# Estructura básica de los datos
str(eph_03_19)

# Resumen estadístico de todas las columnas
summary(eph_03_19)

# Visión rápida de los primeros registros
head(eph_03_19)

# Número de filas y columnas
dim(eph_03_19)


##### 2. Detectar valores faltantes #####

# Identifica qué columnas tienen valores NA y su proporción.

# Proporción de valores faltantes por columna
missing_values <- eph_03_19 %>%
  summarise(across(everything(), ~ sum(is.na(.)) / n())) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Proporcion_NA") %>%
  arrange(desc(Proporcion_NA))

print(missing_values)

##### 3. Identificar columnas únicas o con pocos valores distintos #####

# Esto ayuda a detectar columnas que no aportan información o que tienen valores atípicos.

# Número de valores únicos por columna
unique_values <- eph_03_19 %>%
  summarise(across(everything(), ~ n_distinct(.))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valores_Unicos") %>%
  arrange(Valores_Unicos)

print(unique_values)

##### 4. Análisis de distribución de variables clave #####

# Por ejemplo, tasas o variables categóricas como region, trimestre, ano4, etc.

# Distribución de observaciones por año y trimestre
eph_03_19 %>%
  count(ano4, trimestre) %>%
  arrange(ano4, trimestre) %>%
  print(n = Inf)

# Frecuencia de categorías en variables clave
eph_03_19 %>%
  count(region, sort = TRUE)

# Resumen estadístico de variables numéricas (e.g., pondera)
eph_03_19 %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), list(
    Min = ~ min(., na.rm = TRUE),
    Q1 = ~ quantile(., 0.25, na.rm = TRUE),
    Median = ~ median(., na.rm = TRUE),
    Q3 = ~ quantile(., 0.75, na.rm = TRUE),
    Max = ~ max(., na.rm = TRUE),
    Mean = ~ mean(., na.rm = TRUE),
    NA_Count = ~ sum(is.na(.))
  )))

# aglomerados 32 = Ciudad Autónoma de Buenos Aires y 33 = Partidos del GBA

eph_03_19 %>%
 # filter(aglomerado == 32 | aglomerado == 33) %>%
  group_by(region, aglomerado) %>%
  summarise(n = n()) %>% 
  print(n = 100)

##### 5. Distribuciones gráficas #####

# Usa gráficos para observar tendencias en variables clave

# Distribución por año
eph_03_19 %>%
  ggplot(aes(x = ano4)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribución de observaciones por año", x = "Año", y = "Número de observaciones")

# Distribución por región
eph_03_19 %>%
  ggplot(aes(x = region, fill = region)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribución de observaciones por región", x = "Región", y = "Número de observaciones") +
  theme(legend.position = "none")

# Distribución por aglomerado
eph_03_19 %>%
  ggplot(aes(x = aglomerado, fill = aglomerado)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribución de observaciones por región", x = "Región", y = "Número de observaciones") +
  theme(legend.position = "none")

# Histograma de una variable numérica, como pondera
eph_03_19 %>%
  ggplot(aes(x = pondera)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribución de la ponderación", x = "pondera", y = "Frecuencia")


##### 6. Correlación entre variables numéricas #####

# Si hay muchas variables numéricas, un mapa de calor de la correlación puede ser útil.


# Matriz de correlación (solo para variables numéricas)
num_vars <- eph_03_19 %>% select(where(is.numeric))
cor_matrix <- cor(num_vars, use = "pairwise.complete.obs")

# Graficar la matriz de correlación
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", 
         col = RColorBrewer::brewer.pal(n = 8, name = "RdYlBu"))

##### 7. Outliers y datos extremos ######

# Identifica valores extremos en variables numéricas clave.

# Detectar posibles valores atípicos en variables numéricas
eph_03_19 %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), list(
    Min = ~ min(., na.rm = TRUE),
    Max = ~ max(., na.rm = TRUE)
  )))

##### 8. Guardar resultados de análisis #####

# Guarda las tablas generadas en un archivo para inspección posterior.

# Guardar valores únicos y NA en un Excel
library(openxlsx)
write.xlsx(list(Missing_Values = missing_values, Unique_Values = unique_values),
           file = "analisis_calidad_datos.xlsx")




