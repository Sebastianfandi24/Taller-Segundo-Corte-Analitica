# ============================================================
#  TALLER DE GRÁFICAS - SEMANA SANTA
# ============================================================

# ------------------------------------------------------------
# CARGA DEL DATASET
# ------------------------------------------------------------
ventas <- read.csv("dataset_videojuegos_500.csv", stringsAsFactors = FALSE)

ventas$fecha <- as.Date(ventas$fecha)
ventas$ingresos <- ventas$precio * ventas$cantidad

ventas$mes_num  <- as.integer(format(ventas$fecha, "%m"))
ventas$mes_nombre <- format(ventas$fecha, "%b")

meses_etiq <- c("Ene","Feb","Mar","Abr","May","Jun")

# ============================================================
# PARTE 1: GRÁFICAS
# ============================================================

# 1. HISTOGRAMA
hist(ventas$precio, col="lightblue",
     main="Distribución del Precio",
     xlab="Precio", ylab="Frecuencia")
# Interpretación:
# Los precios se concentran en un rango medio.

# 2. BARRAS
ventas_plat <- sort(table(ventas$plataforma), decreasing = TRUE)
barplot(ventas_plat, col="orange",
        main="Ventas por Plataforma", las=1)
# Interpretación:
# Identifica plataformas más vendidas.

# 3. LÍNEAS
tx_mes <- aggregate(id_venta ~ mes_num, ventas, length)
tx_mes <- tx_mes[order(tx_mes$mes_num), ]

plot(tx_mes$mes_num, tx_mes$id_venta, type="o",
     col="blue", xaxt="n",
     main="Transacciones por Mes")
axis(1, at=1:6, labels=meses_etiq)
# Interpretación:
# Muestra evolución de ventas.

# 4. DISPERSIÓN
plot(ventas$precio, ventas$ingresos,
     col="red", pch=16,
     main="Precio vs Ingresos")
# Interpretación:
# Relación positiva entre variables.

# 5. BOXPLOT CIUDAD
boxplot(ingresos ~ ciudad, data=ventas,
        col=rainbow(length(unique(ventas$ciudad))),
        las=2)
# Interpretación:
# Comparación de ingresos por ciudad.

# 6. DENSIDAD
plot(density(ventas$cantidad),
     col="purple", lwd=2,
     main="Densidad de Cantidad")
# Interpretación:
# Distribución suavizada.

# 7. HEATMAP
mat_hm <- tapply(ventas$ingresos,
                 list(ventas$genero, ventas$plataforma),
                 mean)
mat_hm[is.na(mat_hm)] <- 0

heatmap(mat_hm, col=heat.colors(20))
# Interpretación:
# Relación género-plataforma.

# 8. BOXPLOT GENERO
boxplot(ingresos ~ genero, data=ventas,
        col=rainbow(length(unique(ventas$genero))))
# Interpretación:
# Variación por género.

# 9. ÁREA
ing_mes <- aggregate(ingresos ~ mes_num, ventas, sum)
plot(ing_mes$mes_num, ing_mes$ingresos, type="n")
polygon(c(ing_mes$mes_num, rev(ing_mes$mes_num)),
        c(ing_mes$ingresos, rep(0, nrow(ing_mes))),
        col="lightblue")
# Interpretación:
# Tendencia de ingresos.

# 10. PIE
pie(table(ventas$genero),
    col=rainbow(5))
# Interpretación:
# Proporción por género.

# 11. PAIRS
pairs(ventas[,c("precio","cantidad","ingresos")])
# Interpretación:
# Relación entre variables.

# 12. CORRELACIÓN
heatmap(cor(ventas[,c("precio","cantidad","ingresos")]))
# Interpretación:
# Variables relacionadas.

# 13. REGRESIÓN
modelo <- lm(ingresos ~ precio, data=ventas)
plot(ventas$precio, ventas$ingresos)
abline(modelo, col="blue")
# Interpretación:
# Tendencia lineal.

# 14. SERIE TIEMPO
ts_data <- ts(aggregate(ingresos ~ fecha, ventas, sum)$ingresos)
plot(ts_data)
# Interpretación:
# Comportamiento temporal.

# 15. BURBUJAS
symbols(ventas$precio, ventas$ingresos,
        circles=ventas$cantidad,
        inches=0.1)
# Interpretación:
# 3 variables combinadas.

# ============================================================
# PARTE 2: ANÁLISIS
# ============================================================

# PROBLEMA:
# ¿Qué factores influyen en los ingresos de videojuegos?

# 1. BARRAS GENERO
ing_genero <- aggregate(ingresos ~ genero, ventas, sum)
barplot(ing_genero$ingresos,
        names.arg=ing_genero$genero)
# Justificación: comparar categorías
# Interpretación: géneros más rentables

# 2. BOXPLOT
boxplot(precio ~ plataforma, data=ventas)
# Justificación: ver dispersión
# Interpretación: variabilidad de precios

# 3. LÍNEAS TOP
top3 <- names(sort(tapply(ventas$ingresos,
                          ventas$genero, sum),
                   decreasing=TRUE))[1:3]

ventas_top3 <- ventas[ventas$genero %in% top3, ]
ing_mes_gen <- aggregate(ingresos ~ mes_num + genero,
                         ventas_top3, sum)

plot(1, type="n", xlim=c(1,6),
     ylim=range(ing_mes_gen$ingresos))
for(g in unique(ing_mes_gen$genero)){
  sub <- ing_mes_gen[ing_mes_gen$genero==g,]
  lines(sub$mes_num, sub$ingresos)
}
# Justificación: evolución temporal
# Interpretación: comportamiento en el tiempo

# 4. PIE CIUDAD
pie(tapply(ventas$ingresos, ventas$ciudad, sum))
# Justificación: proporciones
# Interpretación: ciudades con más ingresos

# 5. BARRAS AGRUPADAS
mat <- xtabs(ingresos ~ ciudad + plataforma, ventas)
barplot(mat, beside=TRUE)
# Justificación: comparación múltiple
# Interpretación: combinaciones más rentables

# ============================================================
# CONCLUSIÓN
# ============================================================

# Los ingresos dependen principalmente del género y la plataforma.
# Existe relación positiva entre precio e ingresos.
# Algunas ciudades concentran mayor volumen de ventas.