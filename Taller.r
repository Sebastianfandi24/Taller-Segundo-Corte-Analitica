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

# Se usa una matriz porque permite organizar datos numéricos
# en formato fila-columna y operar sobre ellos fácilmente

matriz_ventas <- matrix(
  c(mi_data$precio, mi_data$cantidad),  # Datos: precio y cantidad
  ncol = 2,                              # Dos columnas
  dimnames = list(NULL, c("precio", "cantidad"))  # Nombres de columnas
)

# Ver las primeras filas de la matriz
print(head(matriz_ventas))

# Calcular un nuevo indicador: ingreso total por venta (precio x cantidad)
ingreso_total <- matriz_ventas[, "precio"] * matriz_ventas[, "cantidad"]

# Agregar el indicador como nueva columna al data frame original
mi_data$ingreso_total <- ingreso_total

# Verificar que se agregó correctamente
print(head(mi_data[, c("precio", "cantidad", "ingreso_total")]))

# Estadísticas básicas del nuevo indicador
print(paste("Ingreso total promedio:", mean(ingreso_total)))
print(paste("Ingreso total máximo:",  max(ingreso_total)))
print(paste("Ingreso total mínimo:",  min(ingreso_total)))

#la matriz solo acepta un tipo de dato (en este caso numérico), y eso la hace 
#ideal para operaciones matemáticas entre columnas, 
#como multiplicar precio × cantidad para obtener el ingreso


# ────────────────────────────────────────────
# SECCIÓN 5: FILTROS Y ESTADÍSTICAS BÁSICAS
# ────────────────────────────────────────────

# Los filtros permiten explorar subconjuntos del dataset
# según condiciones específicas de las variables

# --- Filtro 1: Ventas con precio mayor a 150000 ---
ventas_caras <- mi_data[mi_data$precio > 150000, ]
print(paste("Ventas con precio mayor a 150.000:", nrow(ventas_caras)))

# --- Filtro 2: Ventas por ciudad (Bogotá) ---
ventas_bogota <- mi_data[mi_data$ciudad == "Bogota", ]
print(paste("Ventas en Bogotá:", nrow(ventas_bogota)))

# --- Filtro 3: Ventas de una plataforma específica (PlayStation) ---
ventas_ps <- mi_data[mi_data$plataforma == "PlayStation", ]
print(paste("Ventas en PlayStation:", nrow(ventas_ps)))

# --- Filtro 4: Ventas con cantidad mayor a 3 unidades ---
ventas_cantidad_alta <- mi_data[mi_data$cantidad > 3, ]
print(paste("Ventas con más de 3 unidades:", nrow(ventas_cantidad_alta)))

# --- Filtro combinado: PlayStation con precio > 150000 ---
ventas_ps_caras <- mi_data[mi_data$plataforma == "PlayStation" & mi_data$precio > 150000, ]
print(paste("PlayStation con precio > 150.000:", nrow(ventas_ps_caras)))

# --- Estadísticas básicas generales del dataset ---
print(summary(mi_data$precio))
print(summary(mi_data$cantidad))
print(summary(mi_data$ingreso_total))

# ────────────────────────────────────────────
# SECCIÓN 6: FACTOR – VARIABLE CATEGÓRICA
# ────────────────────────────────────────────

# Un factor es la estructura adecuada para variables categóricas
# porque organiza los datos en niveles (categorías únicas)
# y permite analizar su frecuencia fácilmente

# Convertir la columna 'genero' a factor
genero_factor <- factor(mi_data$genero)

# Ver los niveles (categorías únicas) del factor
print("Categorías de género:")
print(levels(genero_factor))

# Ver cuántos niveles tiene
print(paste("Número de géneros distintos:", nlevels(genero_factor)))

# Frecuencia de cada categoría
print("Frecuencia por género:")
print(table(genero_factor))

# También convertir 'plataforma' a factor y analizar su frecuencia
plataforma_factor <- factor(mi_data$plataforma)

print("Categorías de plataforma:")
print(levels(plataforma_factor))

print("Frecuencia por plataforma:")
print(table(plataforma_factor))

# ────────────────────────────────────────────
# SECCIÓN 7: FRECUENCIAS Y AGRUPACIÓN
# ────────────────────────────────────────────

# Se calculan conteos y agrupaciones para identificar
# qué categorías concentran más ventas en el dataset

# --- Frecuencia por ciudad ---
freq_ciudad <- table(mi_data$ciudad)
print("Frecuencia de ventas por ciudad:")
print(freq_ciudad)

# Ciudad con más ventas
print(paste("Ciudad con más ventas:", names(which.max(freq_ciudad))))

# --- Frecuencia por plataforma ---
freq_plataforma <- table(mi_data$plataforma)
print("Frecuencia de ventas por plataforma:")
print(freq_plataforma)

# Plataforma con más ventas
print(paste("Plataforma con más ventas:", names(which.max(freq_plataforma))))

# --- Frecuencia cruzada: ciudad x plataforma ---
# Permite ver qué plataforma domina en cada ciudad
freq_ciudad_plataforma <- table(mi_data$ciudad, mi_data$plataforma)
print("Frecuencia cruzada ciudad x plataforma:")
print(freq_ciudad_plataforma)

# --- Agrupación: ingreso total por género ---
# Se usa aggregate para sumar ingresos agrupando por género
ingresos_por_genero <- aggregate(
  ingreso_total ~ genero,
  data = mi_data,
  FUN  = sum
)

# Ordenar de mayor a menor ingreso
ingresos_por_genero <- ingresos_por_genero[order(-ingresos_por_genero$ingreso_total), ]

print("Ingresos totales por género:")
print(ingresos_por_genero)

# Género con mayores ingresos
print(paste("Género con mayores ingresos:", ingresos_por_genero$genero[1]))

# 1. table(): Se utiliza para el CONTEO de registros. Solo dice cuántas veces 
#    aparece cada categoría (frecuencia absoluta).
# 2. aggregate(): Se utiliza para AGRUPAR datos y aplicar una función (como sum 
#    o mean) sobre una variable numérica (ej. sumar ingresos por fecha).
# 3. Frecuencia Cruzada: Al usar table(var1, var2), obtenemos el comportamiento 
#    combinado (ej. qué plataforma se vende más en cada ciudad), lo cual 
#    aporta un análisis más profundo que un conteo simple.

# ────────────────────────────────────────────
# SECCIÓN 8: ANÁLISIS TEMPORAL Y GRÁFICO
# ────────────────────────────────────────────

# Se analiza cómo evolucionan los ingresos a lo largo del tiempo
# agrupando las ventas por mes y generando un gráfico de tendencia

# --- Agrupar ingresos totales por mes ---
# Se extrae el mes y año de la columna fecha para agrupar
mi_data$mes <- format(mi_data$fecha, "%Y-%m")  # Crear columna mes (ej: "2024-03")

ingresos_por_mes <- aggregate(
  ingreso_total ~ mes,
  data = mi_data,
  FUN  = sum
)

# Ordenar cronológicamente
ingresos_por_mes <- ingresos_por_mes[order(ingresos_por_mes$mes), ]

print("Ingresos totales por mes:")
print(ingresos_por_mes)

# Mes con mayores ingresos
print(paste("Mes con mayores ingresos:", ingresos_por_mes$mes[which.max(ingresos_por_mes$ingreso_total)]))

# --- Gráfico de tendencia de ingresos por mes ---
plot(
  x    = 1:nrow(ingresos_por_mes),          # Posición en el eje X
  y    = ingresos_por_mes$ingreso_total,     # Ingresos en el eje Y
  type = "b",                                # Línea + puntos
  col  = "steelblue",
  lwd  = 2,
  pch  = 16,                                 # Forma del punto (círculo relleno)
  main = "Tendencia de ingresos por mes",    # Título del gráfico
  xlab = "Mes",                              # Etiqueta eje X
  ylab = "Ingresos totales (COP)",           # Etiqueta eje Y
  xaxt = "n"                                 # Desactivar eje X automático
)

# Agregar etiquetas de mes en el eje X
axis(
  side   = 1,
  at     = 1:nrow(ingresos_por_mes),
  labels = ingresos_por_mes$mes,
  las    = 2                                 # Etiquetas verticales
)

# Agregar línea de tendencia (regresión lineal)
x_seq <- 1:nrow(ingresos_por_mes)  # Crear la secuencia por separado

abline(
  lm(ingresos_por_mes$ingreso_total ~ x_seq),
  col = "red",
  lwd = 1.5,
  lty = 2                                    # Línea punteada
)

# Agregar leyenda
legend(
  "topleft",
  legend = c("Ingresos por mes", "Tendencia"),
  col    = c("steelblue", "red"),
  lwd    = c(2, 1.5),
  lty    = c(1, 2),
  pch    = c(16, NA)
)


# ────────────────────────────────────────────
# Conculusión del análisis temporal
# ────────────────────────────────────────────

#Se observa una tendencia general creciente durante el primer semestre, 
#con una caída en el bimestre enero–febrero atribuible al efecto post-navidad, 
#seguida de una recuperación sostenida hacia mayo, que coincide con la temporada 
#previa a vacaciones de mitad de año en Colombia.