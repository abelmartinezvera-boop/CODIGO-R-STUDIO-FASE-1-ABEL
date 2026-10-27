# 1. Dimensiones y estructura
cat("Número de filas y columnas:\n")
dim(journalist)
cat("\nTipos de variables:\n")
glimpse(journalist)

# 2. Análisis Univariante en bruto: Distribución del valor
# Verás que hay valores extremos (hasta 18.000) que rompen la escala
ggplot(journalist, aes(x = value)) +
  geom_histogram(fill = "red", color = "white", bins = 30) +
  labs(title = "GRÁFICO 1: Distribución del valor (SIN LIMPIAR)",
       subtitle = "Se observa una fuerte asimetría y valores extremos",
       x = "Número de viviendas", y = "Frecuencia") +
  theme_minimal()

# 3. Análisis Bivariante en bruto: Plan vs Tenencia
# Verás que hay muchas categorías duplicadas (ej. mayúsculas, nulos)
ggplot(journalist, aes(x = plan, fill = tenencia)) +
  geom_bar(position = "dodge") +
  labs(title = "GRÁFICO 2: Plan vs Tenencia (SIN LIMPIAR)",
       subtitle = "Observa la cantidad de categorías 'NA' o vacías",
       x = "Plan", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 1. Copia y limpieza de nombres
df_limpio <- journalist %>% clean_names()

# 2. Normalización de texto (quitar tildes, minúsculas, etc.)
limpiar_texto <- function(x) {
  x <- as.character(x)
  x <- str_to_lower(x)
  x <- str_trim(x)
  x <- str_replace_all(x, c("á"="a", "é"="e", "í"="i", "ó"="o", "ú"="u", "ü"="u", "ñ"="n"))
  x <- str_replace_all(x, "\\s+", "_")
  x <- str_replace_all(x, "[^a-z0-9_]", "")
  return(x)
}

df_limpio <- df_limpio %>%
  mutate(
    ccaa = limpiar_texto(ccaa),
    provincia = limpiar_texto(provincia),
    proteccion = limpiar_texto(proteccion),
    promotor = limpiar_texto(promotor),
    tenencia = limpiar_texto(tenencia)
  )

# 3. Tratamiento de fechas (Extraer años numéricos)
# La columna original mezcla "Permanente" con "2035-06-30". 
df_limpio <- df_limpio %>%
  mutate(
    min_year = as.numeric(str_extract(min_end_of_protection, "\\d{4}")),
    max_year = as.numeric(str_extract(max_end_of_protection, "\\d{4}")),
    es_permanente = str_detect(str_to_lower(min_end_of_protection), "permanente")
  )

# 4. Tratamiento de nulos (Convertir vacíos "" en NA)
df_limpio <- df_limpio %>%
  mutate(across(where(is.character), ~na_if(., "")))

# 5. Tratamiento de Atípicos en 'value'
# Calculamos el límite superior del Rango Intercuartílico
Q1 <- quantile(df_limpio$value, 0.25, na.rm = TRUE)
Q3 <- quantile(df_limpio$value, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
limite_sup <- Q3 + (1.5 * IQR_val)

df_limpio <- df_limpio %>%
  mutate(es_atipico = value > limite_sup)

# 6. Quitar duplicados exactos
df_limpio <- df_limpio %>% distinct()

# Resumen de lo que hemos limpiado (Para poner en el informe)
cat("Registros iniciales:", nrow(journalist), "\n")
cat("Registros tras limpiar duplicados:", nrow(df_limpio), "\n")
cat("Número de valores atípicos detectados (pero mantenidos):", sum(df_limpio$es_atipico, na.rm = TRUE), "\n")
 

# 1. Univariante: Distribución corregida (usando logaritmo para ver bien)
ggplot(df_limpio, aes(x = value)) +
  geom_histogram(fill = "#2c7fb8", color = "white", bins = 50) +
  scale_x_log10(labels = comma) +
  labs(title = "GRÁFICO 3: Distribución del valor (LIMPIO)",
       subtitle = "Escala logarítmica aplicada para corregir la asimetría",
       x = "Número de viviendas (Log10)", y = "Frecuencia") +
  theme_minimal()

# 2. Univariante: Top 10 CCAA con nombres estandarizados
df_limpio %>%
  count(ccaa, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(ccaa, n), y = n, fill = ccaa)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "GRÁFICO 4: Top 10 CCAA (LIMPIO)",
       x = "Comunidad Autónoma", y = "Registros") +
  theme_minimal()

# 3. Bivariante: Plan vs Tenencia (Gráfico de barras al 100%)
ggplot(df_limpio, aes(x = plan, fill = tenencia)) +
  geom_bar(position = "fill", color = "white") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "GRÁFICO 5: Tenencia según Plan (LIMPIO)",
       x = "Plan", y = "Porcentaje", fill = "Tenencia") +
  theme_minimal()

# ==============================================================================
# CÓDIGO DE LIMPIEZA Y CALIDAD DE DATOS (FASE 3)
# ==============================================================================
library(tidyverse)
library(janitor)
library(stringr)

# 1. Limpieza de nombres de columnas
df <- journalist %>% clean_names()

# 2. Normalización de texto
limpiar_texto <- function(x) {
  x <- as.character(x)
  x <- str_to_lower(x)
  x <- str_trim(x)
  x <- str_replace_all(x, c("á"="a", "é"="e", "í"="i", "ó"="o", "ú"="u", "ü"="u", "ñ"="n")) 
  x <- str_replace_all(x, "\\s+", "_")
  x <- str_replace_all(x, "[^a-z0-9_]", "")
  return(x)
}

df <- df %>%
  mutate(across(c(ccaa, provincia, proteccion, promotor, tenencia), limpiar_texto)) %>%
  mutate(provincia = case_when(provincia == "guadajalara" ~ "guadalajara", TRUE ~ provincia))

# 3. Tratamiento de fechas y textos
df <- df %>%
  mutate(
    year = as.numeric(year),
    min_ano_fin = as.numeric(str_extract(min_end_of_protection, "\\d{4}")),
    max_ano_fin = as.numeric(str_extract(max_end_of_protection, "\\d{4}")),
    es_permanente = str_detect(str_to_lower(min_end_of_protection), "permanente")
  )

# 4. Nulos
df <- df %>% mutate(across(where(is.character), ~na_if(., "")))

# 5. Atípicos (IQR)
Q1 <- quantile(df$value, 0.25, na.rm = TRUE)
Q3 <- quantile(df$value, 0.75, na.rm = TRUE)
limite_superior <- Q3 + (1.5 * (Q3 - Q1))
df <- df %>% mutate(es_atipico = value > limite_superior)

# 6. Duplicados
df_limpio <- df %>% distinct()

# (El código de generación de gráficos con ggplot2 se omite por brevedad, 
# pero incluye geom_histogram, geom_col y geom_bar con scale_x_log10).

# ==============================================================================
# CÓDIGO DE LIMPIEZA Y CALIDAD DE DATOS (FASE 3)
# ==============================================================================
library(tidyverse)
library(janitor)
library(stringr)

# 1. Limpieza de nombres de columnas
df <- journalist %>% clean_names()

# 2. Normalización de texto
limpiar_texto <- function(x) {
  x <- as.character(x)
  x <- str_to_lower(x)
  x <- str_trim(x)
  x <- str_replace_all(x, c("á"="a", "é"="e", "í"="i", "ó"="o", "ú"="u", "ü"="u", "ñ"="n")) 
  x <- str_replace_all(x, "\\s+", "_")
  x <- str_replace_all(x, "[^a-z0-9_]", "")
  return(x)
}

df <- df %>%
  mutate(across(c(ccaa, provincia, proteccion, promotor, tenencia), limpiar_texto)) %>%
  mutate(provincia = case_when(provincia == "guadajalara" ~ "guadalajara", TRUE ~ provincia))

# 3. Tratamiento de fechas y textos
df <- df %>%
  mutate(
    year = as.numeric(year),
    min_ano_fin = as.numeric(str_extract(min_end_of_protection, "\\d{4}")),
    max_ano_fin = as.numeric(str_extract(max_end_of_protection, "\\d{4}")),
    es_permanente = str_detect(str_to_lower(min_end_of_protection), "permanente")
  )

# 4. Nulos
df <- df %>% mutate(across(where(is.character), ~na_if(., "")))

# 5. Atípicos (IQR)
Q1 <- quantile(df$value, 0.25, na.rm = TRUE)
Q3 <- quantile(df$value, 0.75, na.rm = TRUE)
limite_superior <- Q3 + (1.5 * (Q3 - Q1))
df <- df %>% mutate(es_atipico = value > limite_superior)

# 6. Duplicados
df_limpio <- df %>% distinct()

# (El código de generación de gráficos con ggplot2 se omite por brevedad, 
# pero incluye geom_histogram, geom_col y geom_bar con scale_x_log10).
# ==============================================================================
# INSTALACIÓN Y CARGA DE LIBRERÍAS
# ==============================================================================
# Instalamos automáticamente GGally si no lo tienes
if (!require("GGally")) {
  install.packages("GGally")
  library(GGally)
}

library(tidyverse)
library(scales)

# Filtramos datos para evitar años irreales o nulos que rompan los gráficos
df_graf <- df_limpio %>% 
  filter(year >= 1970, year <= 2025) %>% 
  drop_na(year, value)

# ==============================================================================
# FIGURA 6: Dispersión y Regresión Lineal (Año vs Nº Viviendas)
# ==============================================================================
fig6 <- ggplot(df_graf, aes(x = year, y = value)) +
  geom_point(alpha = 0.3, color = "#2c7fb8") + 
  geom_smooth(method = "lm", color = "#c0392b", fill = "pink", alpha = 0.3) + 
  scale_y_log10(labels = comma) + 
  labs(
    title = "Figura 6: Tendencia y Regresión Lineal del Volumen de Vivienda",
    subtitle = "Relación entre el año de promoción y el número de viviendas (Log10)",
    x = "Año de la Promoción",
    y = "Número de Viviendas (Log10)"
  ) +
  theme_minimal()
print(fig6)

# ==============================================================================
# FIGURA 7: Matriz de Dispersión (Scatterplot Matrix)
# ==============================================================================
df_matrix <- df_limpio %>%
  # CORRECCIÓN AQUÍ: Usamos los nombres reales de tus columnas
  select(value, year, min_ano_fin, max_ano_fin) %>%
  drop_na() %>%
  sample_n(min(2000, nrow(.))) 

fig7 <- ggpairs(df_matrix, 
                title = "Figura 7: Matriz de dispersión multivariante",
                lower = list(continuous = wrap("points", alpha = 0.3, size = 0.5, color = "#2c7fb8")),
                upper = list(continuous = wrap("cor", size = 3))) +
  theme_minimal()
print(fig7)

# ==============================================================================
# FIGURA 8: Serie Temporal (Evolución total de viviendas por año)
# ==============================================================================
fig8 <- df_limpio %>%
  filter(year >= 1970 & year <= 2025) %>%
  group_by(year) %>%
  summarise(total_viviendas = sum(value, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_viviendas)) +
  geom_line(color = "#2c7fb8", size = 1) +
  geom_point(color = "#2c7fb8", size = 1.5) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Figura 8: Evolución Histórica del Total de Viviendas Promocionadas",
    x = "Año",
    y = "Total de Viviendas Registradas"
  ) +
  theme_minimal()
print(fig8)

# ==============================================================================
# FIGURA 9: Evolución del Alquiler vs Propiedad por Década
# ==============================================================================
fig9 <- df_limpio %>%
  filter(year >= 1980, tenencia %in% c("propiedad", "alquiler_sin_opcion", "alquiler_con_opcion")) %>%
  mutate(decada = floor(year / 10) * 10) %>% 
  count(decada, tenencia) %>%
  ggplot(aes(x = factor(decada), y = n, fill = tenencia)) +
  geom_col(position = "fill", color = "white") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Figura 9: Evolución del Régimen de Tenencia por Décadas",
    subtitle = "Proporción de Propiedad vs Alquiler desde 1980",
    x = "Década",
    y = "Porcentaje",
    fill = "Tenencia"
  ) +
  theme_minimal()
print(fig9)
# ==============================================================================
# FIGURA 10: Tamaño de la promoción según Promotor (MEJORADO)
# ==============================================================================
fig10 <- df_limpio %>%
  filter(promotor %in% c("publico", "privado"), !is.na(promotor)) %>%
  # FILTRO CLAVE: Limitamos a 200 viviendas para quitar los "outliers" gigantes 
  # que aplastaban el gráfico y así poder ver bien las cajas.
  filter(value <= 200) %>% 
  ggplot(aes(x = promotor, y = value, fill = promotor)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  # Quitamos scale_y_log10
  scale_fill_manual(values = c("publico" = "#e67e22", "privado" = "#8e44ad")) +
  labs(
    title = "Figura 10: Distribución del Tamaño de Promoción según el Promotor",
    subtitle = "Análisis enfocado en promociones de hasta 200 viviendas para apreciar la mediana",
    x = "Tipo de Promotor",
    y = "Número de Viviendas"
  ) +
  theme_minimal()
print(fig10)
 
