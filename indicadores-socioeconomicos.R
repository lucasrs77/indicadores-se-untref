# Carga de librerías necesarias
library(tidyverse)
library(haven)
library(eph)


##### Datos faltantes #####

#La información correspondiente al tercer trimestre 2007 no está disponible ya que los aglomerados Mar del Plata-Batán, Bahía Blanca-Cerri y Gran La Plata no fueron relevados por causas de orden administrativo, mientras que los datos correspondientes al Aglomerado Gran Buenos Aires no fueron relevados por paro del personal de la EPH.

## 03.17 - No disponible
## 01.14 - Error 
## 03.15 - No disponible
## 04.15 - No disponible
## 01.16 - No disponible

##### carga de datos #####

data_0419 <- get_microdata(year = 2004:2019, trimester = 1:4, 
                      type = "individual",
                      vars = variables)

# Cambiar los nombres de las columnas a minúsculas usando colnames
colnames(data_0419) <- tolower(colnames(data_0419))


setdiff(colnames(data_0419_2), colnames(final_data_0715)) # Columnas que están en data_0419_2 pero no en final_data_0715
setdiff(colnames(final_data_0715), colnames(data_0419_2)) # Columnas que están en final_data_0715 pero no en data_0419_2

# Agregar la columna "pondih" vacía a final_data_0715
final_data_0715$pondih <- NA


eph_03_19 <- rbind(data_0419_2, final_data_0715)

save(eph_03_19, file = "backup_data.RData")
load("backup_data.RData")

a <- eph_03_19 %>% group_by(ano4, trimestre) %>% summarise(n())


##### opcional: descarga por partes #####

data_2003_2010 <- get_microdata(year = 2003:2010, trimester = 1:4, 
                                type = "individual", vars = variables)
data_2011_2015 <- get_microdata(year = 2011:2015, trimester = 1:4, 
                                type = "individual", vars = variables)
data_2016_2019 <- get_microdata(year = 2016:2019, trimester = 1:4, 
                                type = "individual", vars = variables)





##### Link #####

https://citra.org.ar/publicaciones/coyuntura-laboral/coyuntura-laboral-6-principales-indicadores-julio-2024/
  
https://revistas.ucatolicaluisamigo.edu.co/index.php/RCCS/article/view/2343/pdf

https://www.aacademica.org/agustin.salvia/285.pdf

https://aset.org.ar/congresos-anteriores/10/ponencias/p15_Benigni.pdf


##### Guardar y recuperar data #####
# Guardar los objetos en archivos .rds
saveRDS(data_list, "data_list_backup.rds")
saveRDS(final_data_0715, "final_data_0715_backup.rds")

# Eliminar objetos del entorno de trabajo
rm(data_list, final_data_0715)

# Recuperar los objetos
data_list <- readRDS("data_list_backup.rds")
final_data_0715 <- readRDS("final_data_0715_backup.rds")

# Guardar los objetos en un solo archivo .RData
save(data_list, final_data_0715, file = "backup_data.RData")

# Recuperar los objetos desde el archivo .RData
load("backup_data.RData")


##### Leer datos faltantes descargados directamente del indec ######



# Definir path y las variables de interés
path <- "C:/Users/lucas/Documents/UNTREF/Indicadores socioeconomicos"
variables <- c('CODUSU','NRO_HOGAR', 'REGION', 'AGLOMERADO', 'PONDERA',
               'ANO4', 'TRIMESTRE', 'CH04', 'CH06', 'NIVEL_ED', 
               'CAT_OCUP', 'ESTADO', 'INTENSI', 'ITF', 'PONDIH',
               'PP04A', 'PP04B_COD', 'PP04C99', 'PP04D_COD', 
               'PP07C', 'PP07G1', 'PP07G2', 'PP07G3', 'PP07G4', 
               'PP07H', 'PP07I', 'PP07K', 'P21', 'RDECOCUR')

# Convertir variables de interés a minúsculas para normalizar
variables <- tolower(variables)

# Detectar archivos que comiencen con "Individual_" o "individual_"
files <- list.files(
  path, 
  pattern = "(?i)^individual_.*\\.(sav|dta)$", # (?i) hace que sea insensible a mayúsculas/minúsculas
  full.names = TRUE
)

# Verificar la cantidad de archivos encontrados
length(files)


# Leer todos los archivos en una lista
data_list <- files %>%
  map(~ {
    # Detectar el formato y leer el archivo
    data <- if (str_detect(.x, "\\.sav$")) {
      read_sav(.x)
    } else {
      read_dta(.x)
    }
    
    # Normalizar nombres de columnas a minúsculas
    data <- data %>% rename_with(tolower)
    
    # Seleccionar variables de interés
    select(data, any_of(variables))
  })

# Unir todos los datos en un único data frame
final_data_0715 <- bind_rows(data_list)

# Ver resumen del resultado
glimpse(final_data)





##### Inconsistencias #####

# Definir función para verificar columnas faltantes en un data frame
check_missing_columns_from_list <- function(data, variables, file_name) {
  # Nombres de las columnas en el data frame (normalizados a minúsculas)
  cols_in_file <- colnames(data) %>% tolower()
  
  # Identificar las variables que faltan
  missing_vars <- setdiff(variables, cols_in_file)
  
  # Retornar un tibble con los resultados
  tibble(file = file_name, missing_columns = list(missing_vars))
}

# Generar reporte de columnas faltantes en cada data frame dentro de data_list
missing_columns_report <- map2_dfr(
  data_list, 
  basename(files), # Asocia cada data frame con su archivo correspondiente
  ~ check_missing_columns_from_list(.x, variables, .y)
)

# Visualizar el reporte
print(missing_columns_report)



