
load("backup_data.RData")

# Carga de librerías necesarias
library(eph)
library(tidyverse) 
library(corrplot)
library(RColorBrewer)
library(scales)
library(openxlsx)

lineas <- get_poverty_lines(regional = TRUE)

head(adulto_equivalente)

colnames(eph_03_19) <- toupper(colnames(eph_03_19))

##### calcular pobreza e indigencia #####

eph_03_19 <- calculate_poverty(eph_03_19, basket = lineas)


# Agrupar por año y trimestre, luego calcular el conteo y porcentaje dentro de cada grupo
poverty_summary <- eph_03_19 %>%
  
  # Agregar la columna 'situacion' según la categorización de pobreza
  
  mutate(
    situacion = case_when(
      situacion == "pobre" ~ "Pobre",
      situacion == "no_pobre" ~ "No pobre",
      TRUE ~ "No clasificado"  # Para valores que no sean clasificados
    )
  ) %>% 
  
  group_by(ANO4, TRIMESTRE) %>%
  summarise(
    total = n(),
    count_pobre = sum(situacion == "Pobre", na.rm = TRUE),
    count_no_pobre = sum(situacion == "No pobre", na.rm = TRUE),
    count_no_clasificado = sum(situacion == "No clasificado", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje_pobre = (count_pobre / total) * 100,
    porcentaje_no_pobre = (count_no_pobre / total) * 100,
    porcentaje_no_clasificado = (count_no_clasificado / total) * 100
  )

## opcional filtro jovenes ##
eph_03_19 %>% filter(CH06 <= 34) %>% 
  calculate_poverty(basket = lineas)

eph_data <- eph_03_19

##### recategorizaciones #####

eph_03_19 <- eph_03_19 %>%  mutate(EDAD_CAT = case_when(# Edad categórica
  CH06 <= 15 ~ "15 años y menos",
  CH06 >= 16  & CH06 <= 24 ~ "16 a 24 años",
  CH06 >= 25  & CH06 <= 34 ~ "25 a 34 años",
  CH06 >= 35 & CH06 <= 45 ~ "35 a 45 años", 
  CH06 >= 46 & CH06 <= 60 ~ "46 a 60 años", 
  CH06 >= 61 ~ "Más de 60 años",
  TRUE ~ NA_character_   # Valor NA para otros casos de edad
),

Joven_cat = case_when(
  CH06 <= 15 ~ "Menor de 15",  # Asignar una categoría explícita
  CH06 >= 16  & CH06 <= 34 ~ "Joven",
  CH06 >= 35 & CH06 <= 60 ~ "No joven",
  CH06 >= 61 ~ "Adulto mayor"),

asalariado = case_when(
  P21 > 0 & ESTADO == 1 & CAT_OCUP == 3 ~ 1,  # Obreros o empleados con ingresos mayores a 0
  ESTADO == 1 & CAT_OCUP == 3 ~ 0,           # Obreros o empleados sin remuneración
  TRUE ~ NA_integer_                        # No son tenidos en cuenta, valor NA
)) %>% 
  organize_cno() %>% organize_caes()


##### tasas básicas #####


tasas <- eph_03_19 %>%
  group_by(REGION, CH04, ANO4, TRIMESTRE) %>% 
  summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]),
            Desocupados       = sum(PONDERA[ESTADO == 2]),
            PEA               = Ocupados + Desocupados,
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==2]),
            Subocupados       = Suboc_demandante + Suboc_no_demand ,
            Asalariados       = sum(PONDERA[asalariado == 1], na.rm = T),
            # También podemos llamar a las variables entre comillas, incluyendo nombres compuestos
            # A su vez, podemos utilizar la variable recién creada en la definción de otra varible
            'Tasa Actividad'                  = PEA/Poblacion,
            'Tasa Empleo'                     = Ocupados/Poblacion,
            'Tasa Desocupacion'               = Desocupados/PEA,
            'Tasa Subocupación'               = Subocupados/PEA,
            'Tasa Subocupación demandante'    = Suboc_demandante/PEA,
            'Tasa Subocupación no demandante' = Suboc_no_demand/PEA,
            'Tasa de Asalarizacion'           = Asalariados/Ocupados) %>% 
  select(-c(5:12))

tasas <- tasas %>% 
  mutate(
    REGION = as.factor(REGION),
    CH04 = as.factor(CH04)) %>% 
  organize_labels()


##### prueba labels #####

eph_sample <- eph_sample %>%
  mutate(
    REGION = as_factor(REGION),
    ESTADO = as_factor(ESTADO))

# Cargar dplyr si no está cargado
library(dplyr)

# Tomar una muestra aleatoria del 10% de los datos
eph_sample <- eph_03_19 %>%
  sample_frac(0.1)  # 0.1 indica que se tomará el 10%

# Ver las primeras filas de la muestra
head(eph_sample)

# Probar la función organize_labels() en la muestra
eph_sample <- eph_sample %>%
  organize_labels()

# Ver la muestra después de organizar las etiquetas
head(eph_sample)

# Ver el código de la función organize_labels
getAnywhere(organize_labels)


##### Pruebas de categoria ocupacional #####

test <- eph_03_19 %>% select(CAT_OCUP, JERARQUIA) %>% 
  filter(CAT_OCUP == 2, JERARQUIA != "Cuenta propia")

test <- eph_03_19 %>% select(CAT_OCUP, ESTADO, P21, JERARQUIA, asalariado) %>% 
  filter(JERARQUIA == "Trabajadores asalariados", asalariado != 1)


##### Informalidad #####

## A
## cuenta propia en ocupaciones de calificaciÛn 
## tÈcnica, operativa o no calificada
## pobre o indigente

## B
## patrones/as en ocupaciones que no sean
## de calificaciÛn profesional
## pobre o indigente

## C
## Trabajadores/as familiares
## de calificaciÛn tÈcnica, operativa o no calificada, 
## que se desempeÒan en establecimientos de hasta cinco ocupados

## D
## Asalariados/as que se desempeÒan en establecimientos 
## de hasta cinco ocupados, y que no se les efect˙a descuento jubilatorio

## E
## Asalariados/as que se desempeÒan en establecimientos 
## de m·s de cinco ocupados, y que no se les efect˙a descuento jubilatorio

## F
## Asalariados/as a quienes se les efect˙a descuento jubilatorio 
## y tienen un acuerdo laboral no permanente

## G
## Menores de 16 años ocupados (con o sin remuneracion)


eph_03_19 <- eph_03_19 %>% mutate(CAT_INFORMALIDAD = case_when(
  (CAT_OCUP == 2) & (CALIFICACION == "Operativos" | 
                       CALIFICACION == "No calificados" | CALIFICACION == "Técnicos") & 
    (situacion == "indigente" | situacion == "pobre") ~ "A",
  
  (CAT_OCUP == 1) & (CALIFICACION != "Profesionales") 
  & (situacion == "indigente" | situacion == "pobre") ~ "B",
  
  (CAT_OCUP == 4) & (CALIFICACION != "Profesionales") 
  & (PP04C99 == 1) ~ "C",
  
  (asalariado == 1) & (PP07H == 2) 
  & (PP04C99 == 1) ~ "D",
  
  (asalariado == 1) & (PP07I == 2) 
  & (PP04C99 == 2 | PP04C99 == 3) ~ "E",
  
  (asalariado == 1) & (PP07C == 1) 
  & (PP07H == 1 | PP07I == 1) ~ "F",
  
  (Joven_cat == "Menor de 15" & ESTADO == 1) ~ "G",
  
  (ESTADO == 1) ~ "Formal"),
  
  Informal = case_when((CAT_INFORMALIDAD == "A" | CAT_INFORMALIDAD == "B" |
                          CAT_INFORMALIDAD == "C" | CAT_INFORMALIDAD == "D" |
                          CAT_INFORMALIDAD == "E" | CAT_INFORMALIDAD == "F" |
                          CAT_INFORMALIDAD == "G"  )
                       & (CAT_INFORMALIDAD != "Formal" ) ~ 1,
                       (ESTADO == 1) ~ 0),
  
  Precariedad = case_when(
    (ESTADO  == 1) & (PP07H == 2 | PP07I == 2) | 
      (PP07C == 1) ~ 1,
    (ESTADO == 1) ~ 0))


# Agrupar por año (ANO4) y calcular registros válidos y NAs para las columnas Informal y Precariedad
grouped_summary <- eph_03_19 %>% 
  filter(ESTADO == 1) %>% 
  group_by(ANO4) %>%
  summarise(
    total_validos_informal = sum(!is.na(Informal)),
    total_na_informal = sum(is.na(Informal)),
    total_validos_precariedad = sum(!is.na(Precariedad)),
    total_na_precariedad = sum(is.na(Precariedad)),
    .groups = "drop"
  )

# checkear informalidad g

informalidad_g <- eph_03_19 %>% 
  filter(CAT_INFORMALIDAD == "G")

informalidad_g %>% group_by(NIVEL_ED) %>% summarise(n())




##### Tabla de contingencia #####

contingency_df <- eph_03_19 %>% 
  select(ESTADO, INTENSI, PP07C, PP07H, PP07G1, PP07G2, PP07G3, PP07G4) %>% 
  
  # Usar pivot_longer para convertir las columnas en un formato largo
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Categoria") %>% 
  
  # Usar pivot_wider para obtener la tabla de contingencia
  group_by(Categoria, Variable) %>%  # Invertir el orden aquí
  summarise(Count = n()) %>% 
  pivot_wider(names_from = Variable, 
              values_from = Count, 
              values_fill = 0) %>% 
  organize_labels()

##### Correlation plot #####
M <- cor(contingency_df[2:9])
M

corrplot::corrplot(M, type="upper", order="original", 
                   addCoef.col = "black", method = "color",
                   col=brewer.pal(n=8, name="RdYlBu"))


##### Tasas #####


base_tasas <- eph_03_19 %>%
  group_by(ANO4, CH04) %>%
  summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]),
            Desocupados       = sum(PONDERA[ESTADO == 2]),
            PEA               = Ocupados + Desocupados,
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==2]),
            Subocupados       = Suboc_demandante + Suboc_no_demand ,
            Asalariados       = sum(PONDERA[asalariado == 1], na.rm = T),
            Informales        = sum(PONDERA[Informal == 1], na.rm = T),
            Precarizados      = sum(PONDERA[Precariedad == 1], na.rm = T))


tasas <- base_tasas %>% 
  group_by(ANO4, CH04) %>%
  summarise(
    # También podemos llamar a las variables entre comillas, incluyendo nombres compuestos
    # A su vez, podemos utilizar la variable recién creada en la definción de otra varible
    'Tasa Actividad'                  = PEA/Poblacion,
    'Tasa Empleo'                     = Ocupados/Poblacion,
    'Tasa Desocupacion'               = Desocupados/PEA,
    'Tasa Subocupación'               = Subocupados/PEA,
    'Tasa Subocupación demandante'    = Suboc_demandante/PEA,
    'Tasa Subocupación no demandante' = Suboc_no_demand/PEA,
    'Tasa de Asalarizacion'           = Asalariados/Ocupados,
    'Tasa de Informalidad'            = Informales/Ocupados,
    'Tasa de Precarización'           = Precarizados/Ocupados)

write.xlsx(tasas, file = "tasas.xlsx", rowNames = FALSE)

##### Tasas jovenes #####


tasas_edad <- eph_03_19 %>%
  group_by(ANO4, EDAD_CAT, TRIMESTRE) %>% 
 # filter(EDAD_CAT == "15 años y menos") %>% 
  summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]),
            Informales        = sum(PONDERA[Informal == 1], na.rm = T),
            Precarizados      = sum(PONDERA[Precariedad == 1], na.rm = T),
            'Tasa de Informalidad'            = Informales/Ocupados,
            'Tasa de Precarización'           = Precarizados/Ocupados) %>% 
  select(-c("Poblacion", "Ocupados", "Informales", "Precarizados")) %>% 
  pivot_longer(-c(1:3), names_to = "Tasas", values_to = "Porcentaje")


write.xlsx(tasas_edad, file = "tasas_edad.xlsx", rowNames = FALSE)


####### Tasas para gráfico ######
tasas_viz <- eph_03_19 %>%
  group_by(ANO4, TRIMESTRE, CH04) %>%
  summarise(Ocupados          = sum(PONDERA[ESTADO == 1]),
            Informales        = sum(PONDERA[Informal == 1], na.rm = T),
            Precarizados      = sum(PONDERA[Precariedad == 1], na.rm = T),
            'Tasa de Informalidad'            = Informales/Ocupados,
            'Tasa de Precarización'           = Precarizados/Ocupados) %>% 
  select(-c("Ocupados", "Informales", "Precarizados"))

## valor trimestral de las tasas sin controlar por sexo ni edad
tasas_viz_gral <- eph_03_19 %>%
  group_by(ANO4, TRIMESTRE) %>%
  summarise(Ocupados          = sum(PONDERA[ESTADO == 1]),
            Informales        = sum(PONDERA[Informal == 1], na.rm = T),
            Precarizados      = sum(PONDERA[Precariedad == 1], na.rm = T),
            'Tasa de Informalidad'            = Informales/Ocupados,
            'Tasa de Precarización'           = Precarizados/Ocupados) %>% 
  select(-c("Ocupados", "Informales", "Precarizados"))





##### Viz #####
###### Tasas a traves del tiempo ######

graf_tasas <- tasas_viz

graf_tasas$Periodo <- as.Date(paste(graf_tasas$ANO4, 
                                    graf_tasas$TRIMESTRE*3, "01", 
                                    sep = "-"))

graf_tasas <- graf_tasas %>% pivot_longer(-c(1:3, 6), 
                                          names_to = "Tasas",
                                          values_to = "Porcentaje")

tasas_grales <- tasas_viz_gral %>% 
  pivot_longer(-c(1:2), 
               names_to = "Tasas",
               values_to = "Porcentaje_Gral") 

graf_tasas <- left_join(graf_tasas, tasas_grales, 
                        by = c("ANO4", "TRIMESTRE", 
                               "Tasas")) %>% 
  organize_labels()

graf_tasas %>% #filter(EDAD_CAT != "15 años y menos") %>% 
  # Gráfico de puntos con líneas conectadas
  ggplot(aes(x = Periodo, 
             y = Porcentaje, group = as.factor(CH04), 
             color = as.factor(CH04))) +
  geom_point() +
  geom_line() +
  
  # la linea puede comentarse para hacer el gráfico sin la tasa general
  geom_line(aes(y = Porcentaje_Gral, color = "Z-Tasa General"), 
            linetype = "dashed") +  # Agregar la tercera línea
  
  facet_wrap(~ Tasas, scales = "free")+
  
  labs(color = "Sexo",
       caption = "Fuente: Elaboración propia en base a datos de la EPH") +
  
  ggtitle("Gráfico 1. Tasas por Sexo a través del Tiempo") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_brewer(palette = "Set1") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months")  # Etiquetas y separación de meses


###### Tasas por grupo etareo ######

tasas_edad$Periodo <- as.Date(paste(tasas_edad$ANO4, 
                                    tasas_edad$TRIMESTRE*3, "01", 
                                    sep = "-"))

tasas_edad %>%  organize_labels() %>% 
  
  filter(EDAD_CAT != "15 años y menos") %>% 
  
  # Gráfico de puntos con líneas conectadas
  ggplot(aes(x = Periodo, 
             y = Porcentaje, group = as.factor(EDAD_CAT), 
             color = as.factor(EDAD_CAT))) +
  
  geom_point() +
  geom_line() +
  
  geom_line(data = subset(tasas_edad, 
                          EDAD_CAT %in% c("16 a 24 años", "25 a 34 años")), 
            aes(color = as.factor(EDAD_CAT)), linewidth = 1.5)+
  
  facet_wrap(~ Tasas, scales = "free")+
  
  labs(color = "Grupo Etario",
       caption = "Fuente: Elaboración propia en base a datos de la EPH") +
  
  ggtitle("Gráfico 2. Tasas por Grupo etario a través del Tiempo") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = label_percent())+
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months")  # Etiquetas y separación de meses


###### Tabla precariedad #####

tabla_precariedad <- eph_03_19 %>%
  group_by(ANO4, TRIMESTRE, EDAD_CAT) %>% 
  filter((EDAD_CAT != "15 años y menos") & (ESTADO == 1)) %>% 
  summarise(Ocupados          = sum(PONDERA[ESTADO == 1]),
            Informales        = sum(PONDERA[Informal == 1], na.rm = T),
            Precarizados      = sum(PONDERA[Precariedad == 1], na.rm = T),
            Intensidad        = sum(PONDERA[INTENSI == 1], na.rm = T)/Ocupados,
            No_vacaciones     = sum(PONDERA[PP07G1 == 2], na.rm = T)/Ocupados,
            no_aguinaldo      = sum(PONDERA[PP07G2 == 2], na.rm = T)/Ocupados,
            no_enfermedad     = sum(PONDERA[PP07G3 == 2], na.rm = T)/Ocupados,
            no_obra_social    = sum(PONDERA[PP07G4 == 2], na.rm = T)/Ocupados,
            'Tasa de Informalidad'            = Informales/Ocupados,
            'Tasa de Precarización'           = Precarizados/Ocupados)

M <- cor(tabla_precariedad[-c(2:6)])
M

corrplot::corrplot(M, type="upper", order="original", 
                   addCoef.col = "black", method = "color",
                   col=brewer.pal(n=8, name="RdYlBu"))

###### Grafico precariedad ######

graf_precariedad <- eph_03_19 %>%
  group_by(ANO4, EDAD_CAT) %>% 
  filter((EDAD_CAT != "15 años y menos") & (ESTADO == 1)) %>% 
  summarise(Ocupados          = sum(PONDERA[ESTADO == 1]),
            Informales        = sum(PONDERA[Informal == 1], na.rm = T),
            Precarizados      = sum(PONDERA[Precariedad == 1], na.rm = T),
            Intensidad        = sum(PONDERA[INTENSI == 1], na.rm = T)/Ocupados,
            No_vacaciones     = sum(PONDERA[PP07G1 == 2], na.rm = T)/Ocupados,
            no_aguinaldo      = sum(PONDERA[PP07G2 == 2], na.rm = T)/Ocupados,
            no_enfermedad     = sum(PONDERA[PP07G3 == 2], na.rm = T)/Ocupados,
            no_obra_social    = sum(PONDERA[PP07G4 == 2], na.rm = T)/Ocupados,
            'Tasa de Informalidad'            = Informales/Ocupados,
            'Tasa de Precarización'           = Precarizados/Ocupados) %>% 
  select(-c("Ocupados", "Informales", "Precarizados")) %>% 
  pivot_longer(-c(1:2), names_to = "Tasas", values_to = "Porcentaje")


graf_precariedad %>% filter((Tasas != "Tasa de Informalidad") 
                            #& (Tasas != "Tasa de Precarización")
) %>% 
  ggplot(aes(x = ANO4, 
             y = Porcentaje, group = as.factor(Tasas), 
             color = as.factor(Tasas)))+
  geom_point() +
  geom_line() +
  
  #facet_wrap(~ EDAD_CAT, scales = "free")+
  facet_grid(~EDAD_CAT , scales = "free")+
  
  labs(color = "Tasas", caption = "Fuente: Elaboración propia en base a datos de la EPH") +
  xlab("Año")+
  
  ggtitle("Gráfico 3. Precarización de derechos laborales según edad, 2018-2022") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = "Dark2") 
scale_x_date(date_labels = "%Y", date_breaks = "1 year")  # Etiquetas y separación de meses


###### Gráfico Calificación Ocupacional ######

eph_03_19 %>% group_by(ANO4, CALIFICACION) %>% 
  summarise(n())

# Filtrar las categorías deseadas en CALIFICACION
calificaciones_deseadas <- c("Técnicos", "Profesionales", "Operativos", "No calificados")
graf_calificacion <- eph_03_19 %>% 
  filter(CALIFICACION %in% calificaciones_deseadas) 

# Calcular el porcentaje de formalidad e informalidad en cada categoría de calificación ocupacional

graf_calificacion2 <-  graf_calificacion %>%
  group_by(ANO4, CALIFICACION) %>%
  summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]),
            Informales        = sum(PONDERA[Informal == 1], na.rm = T),
            Formales          = sum(PONDERA[Informal == 0], na.rm = T),
            No_Precarizados   = sum(PONDERA[Precariedad == 0], na.rm = T),
            Precarizados      = sum(PONDERA[Precariedad == 1], na.rm = T),
            'Precarizado'           = Precarizados/Ocupados,
            'No Precarizado'        = No_Precarizados/Ocupados,
            'Informal'              = Informales/Ocupados,
            'Formal'                = Formales/Ocupados
  ) %>% 
  select(-c('Poblacion', 'Ocupados', 'Informales', 'Formales',
            'No_Precarizados', 'Precarizados')) %>% 
  pivot_longer(-c(1:2), names_to = "Tasa", values_to = "Porcentaje") %>% 
  mutate(Tipo = case_when(
    Tasa == 'Precarizado' | Tasa == 'No Precarizado' ~ "Precarizacion",
    Tasa == 'Informal'| Tasa == 'Formal' ~ "Informalidad"))



graf_calificacion2 %>% 
  filter(ANO4 == 2004 | ANO4 == 2008 |ANO4 == 2015 |ANO4 == 2019) %>% 
  # Gráfico de columnas apiladas para Calificación Ocupacional con porcentajes
  ggplot(aes(x = CALIFICACION, y = Porcentaje, fill = factor(Tasa))) +
  geom_col(position = "stack") +  # Columnas apiladas
  labs(x = "Calificación Ocupacional", y = "Porcentaje", fill = "Tipo de Empleo",
       caption = "Fuente: Elaboración propia en base a datos de la EPH") +
  ggtitle("Gráfico 4. Incidencia de Informalidad y Precariedad por Calificación Ocupacional") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Spectral") +
  scale_y_continuous(labels = label_percent())+
  facet_grid(Tipo~ANO4)



###### Gráfico  Tamaño del Establecimiento y Sexo #####

# Filtrar las categorías deseadas en PP04C99 (Tamaño del Establecimiento)
tamanios_deseados <- c(1, 2, 3)

graf_tamanio <- eph_03_19 %>% 
  filter(PP04C99 %in% tamanios_deseados) #%>%
  na.omit()  # Eliminar filas con valores NA en la variable informalidad

# Calcular la incidencia de formalidad e informalidad en cada segmento de Tamaño del Establecimiento por sexo y año
graf_tamanio <- graf_tamanio %>%
  group_by(ANO4, PP04C99, CH04, Joven_cat, Precariedad) %>%
  summarise(count = n()) %>%
  group_by(ANO4, PP04C99, CH04, Joven_cat) %>%
  mutate(percentage = count/sum(count)) %>% organize_labels()

# Ajustar el valor de ANO4 según el filtro seleccionado
filtro_ANO4 <- c(2004, 2008, 2015, 2019)  # Puedes cambiar el año aquí

graf_tamanio %>% 
  
  filter(ANO4 %in% filtro_ANO4) %>% 
  filter(Joven_cat != "Menor de 15") %>% 
  
  # Gráfico de barras agrupadas por Tamaño del Establecimiento y Sexo con filtro seleccionable para ANO4
  ggplot(aes(x = factor(PP04C99), y = percentage, fill = factor(Precariedad))) +
  geom_col(position = "stack") +  # Barras agrupadas
  labs(x = "Tamaño del Establecimiento", y = "Porcentaje", fill = "Tipo de Empleo",
       caption = "Fuente: Elaboración propia en base a datos de la EPH") +
  ggtitle(paste("Gráfico 5. Incidencia del Empleo Precario según Tamaño del Establecimiento, Edad y Sexo - Año", filtro_ANO4)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Spectral",
                    labels = c("0" = "No Precario", "1" = "Precario")) +
  scale_y_continuous(labels = label_percent())+
  facet_grid(Joven_cat~CH04 + ANO4)  # Generar un gráfico por cada categoría de sexo


###### Gráfico de barras para Rama de Actividad ######

# Filtrar las filas con valores NA en la variable "informalidad"
# Calcular el porcentaje en cada Rama de Actividad
graf_rama <- eph_03_19 %>% 
  na.omit(Informal) %>%
    filter(ANO4 == 2019) %>%
  group_by(caes_eph_label, CH04) %>%
  summarise(perc_inf = mean(Informal, na.rm = TRUE),
            perc_prec = mean(Precariedad, na.rm = TRUE)) %>%
  # Reordenar las filas por el porcentaje de forma descendente
  arrange(desc(perc_inf))

# Convertir caes_eph_label en factor para establecer el orden en el gráfico
graf_rama$caes_eph_label <- factor(graf_rama$caes_eph_label
                                   #, levels = graf_rama$caes_eph_label
                                   )


# Combinar las paletas para tener 14 colores en total
colores <- c(brewer.pal(n = 12, name = "Set3"), 
             brewer.pal(n = 8, name = "Dark2"))

graf_rama %>% organize_labels() %>% 
  # Gráfico de barras para Rama de Actividad
  ggplot(aes(x = perc_inf, y = caes_eph_label, fill = caes_eph_label)) +
  geom_col() +  # Geometría de barras
  geom_vline(xintercept = mean(graf_rama$perc_inf))+
  labs(x = "Porcentaje", y = "Rama de Actividad", fill = "Rama de Actividad") +
  ggtitle("Gráfico 6.1. Porcentaje de Informalidad por Rama de Actividad. Año 2022") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  scale_fill_manual(values = colores)+ # Utilizar la paleta de colores personalizada
  scale_x_continuous(labels = label_percent())+
  theme(legend.position = "none") + # Eliminar la leyenda
  facet_wrap(~CH04)


graf_rama %>%  organize_labels() %>% 
  # Gráfico de barras para Rama de Actividad
  ggplot(aes(x = perc_prec, y = reorder(caes_eph_label, -perc_prec), fill = caes_eph_label)) +
  geom_col() +  # Geometría de barras
  geom_vline(xintercept = mean(graf_rama$perc_prec))+
  labs(x = "Porcentaje", y = "Rama de Actividad", fill = "Rama de Actividad") +
  ggtitle("Gráfico 6.2. Porcentaje de Precariedad por Rama de Actividad. Año 2022") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  scale_fill_manual(values = colores)+ # Utilizar la paleta de colores personalizada
  scale_x_continuous(labels = label_percent())+
  theme(legend.position = "none")+ # Eliminar la leyenda
  facet_wrap(~CH04)


###### Gráfico de barras para Nivel Educativo ######

# Calcular el porcentaje de informalidad por sexo, nivel educativo y año

graf_nived <- eph_03_19 %>% 
  #na.omit(Informal) %>% # Filtrar las filas con valores NA en la variable "informalidad"
  filter(!is.na(Informal)) %>% 
  filter(NIVEL_ED != 9) %>% # Filtrar nivel educativo diferente a categoría 9
  group_by(ANO4, NIVEL_ED, CH04, Joven_cat) %>%
  summarise(percentage = mean(Informal, na.rm = TRUE)) %>% 
  organize_labels()

graf_nived %>% filter(ANO4 %in% c(2014, 2015, 2016, 2019)) %>% 
  # Gráfico de barras para Nivel Educativo con barras agrupadas por sexo y espacio entre categorías, facet por ANO4
  ggplot(aes(x = factor(NIVEL_ED), y = percentage, fill = factor(CH04))) +
  geom_col(position = position_dodge(width = 0.7)) +  # Ajuste del ancho y espacio entre barras
  labs(x = "Nivel Educativo", y = "Porcentaje", fill = "Sexo",
       caption = "Fuente: Elaboración propia en base a datos de la EPH") +
  ggtitle("Gráfico 7. Tasa de Precarización por Nivel Educativo, Sexo y Edad.") +
  theme(plot.title = element_text(hjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = label_percent())+
  facet_wrap(~Joven_cat + ANO4, ncol = 4)
