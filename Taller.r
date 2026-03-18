# ============================================================
# TALLER – ANÁLISIS DE DATOS CON R
# Dataset: Ventas de Videojuegos
# ============================================================

# ────────────────────────────────────────────
# SECCIÓN 1: CARGA Y EXPLORACIÓN DEL DATASET
# ────────────────────────────────────────────

# Cargar el archivo CSV
mi_data <- read.csv("dataset_videojuegos_500.csv")

# Visualizar las primeras filas del dataset
print(head(mi_data))

# Identificar número de registros y variables
print(dim(mi_data))                          # Filas x Columnas
n_registros <- nrow(mi_data)                 # Número de filas
n_variables <- ncol(mi_data)                 # Número de columnas
print(paste("Registros:", n_registros))
print(paste("Variables:", n_variables))

# Analizar el tipo de dato de cada columna
str(mi_data)                                 # Estructura general
print(sapply(mi_data, class))                # Clase de cada variable

# Convertir la columna 'fecha' de texto a tipo Date
mi_data$fecha <- as.Date(mi_data$fecha)

# ────────────────────────────────────────────
# SECCIÓN 2: VECTOR – VARIABLE NUMÉRICA PRINCIPAL
# ────────────────────────────────────────────

# Almacenar la columna 'precio' en un vector
precios_vector <- mi_data$precio

# Calcular estadísticas básicas del precio
promedio_precio <- mean(precios_vector, na.rm = TRUE)   # Promedio
max_precio      <- max(precios_vector, na.rm = TRUE)    # Máximo
min_precio      <- min(precios_vector, na.rm = TRUE)    # Mínimo

print(paste("Promedio del precio:", promedio_precio))
print(paste("Precio máximo:", max_precio))
print(paste("Precio mínimo:", min_precio))

# Resumen estadístico completo del vector
summary(precios_vector)

# ────────────────────────────────────────────
# SECCIÓN 3: LISTA – INFORMACIÓN GENERAL DEL ANÁLISIS
# ────────────────────────────────────────────

# Se usa una lista porque permite almacenar distintos tipos
# de datos juntos: texto, números y fechas en un solo objeto
info_analisis <- list(
  nombre          = "Análisis de ventas de videojuegos",
  categoria_principal = "Videojuegos",
  ciudades        = unique(mi_data$ciudad),       # Vector con las ciudades del dataset
  n_registros     = nrow(mi_data),                # Número total de filas
  n_variables     = ncol(mi_data),                # Número total de columnas
  precio_promedio = mean(mi_data$precio),         # Promedio de precio (numérico)
  fecha_inicio    = min(mi_data$fecha),           # Fecha de venta más antigua (Date)
  fecha_fin       = max(mi_data$fecha)            # Fecha de venta más reciente (Date)
)

# Mostrar la lista completa
print(info_analisis)

# También se puede acceder a cada elemento individualmente
print(paste("Análisis:", info_analisis$nombre))
print(paste("Registros:", info_analisis$n_registros))
print(paste("Período:", info_analisis$fecha_inicio, "al", info_analisis$fecha_fin))


#Porque usamos una lista porque mezcla character, integer, Date y hasta 
#vectores dentro de un mismo objeto

# ────────────────────────────────────────────
# SECCIÓN 4: MATRIZ – VARIABLES NUMÉRICAS E INDICADOR
# ────────────────────────────────────────────

# (por desarrollar)

# ────────────────────────────────────────────
# SECCIÓN 5: FILTROS Y ESTADÍSTICAS BÁSICAS
# ────────────────────────────────────────────

# (por desarrollar)

# ────────────────────────────────────────────
# SECCIÓN 6: FACTOR – VARIABLE CATEGÓRICA
# ────────────────────────────────────────────

# (por desarrollar)

# ────────────────────────────────────────────
# SECCIÓN 7: FRECUENCIAS Y AGRUPACIÓN
# ────────────────────────────────────────────

# (por desarrollar)

# ────────────────────────────────────────────
# SECCIÓN 8: ANÁLISIS TEMPORAL Y GRÁFICO
# ────────────────────────────────────────────

# (por desarrollar)