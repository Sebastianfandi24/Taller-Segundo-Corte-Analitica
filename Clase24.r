# ============================================================
#  TALLER DE GRÁFICAS - SEMANA SANTA
#  Dataset: Ventas de Videojuegos (500 registros, Ene-Jun 2024)
# ============================================================

# Evitar notación científica (e+05, e+06, etc.) en todos los ejes
options(scipen = 999)

# Librerías necesarias
# install.packages("ggplot2")   # solo la primera vez
# install.packages("corrplot")  # solo la primera vez
library(ggplot2)
library(corrplot)

# ------------------------------------------------------------
# CARGA Y PREPARACIÓN DEL DATASET
# ------------------------------------------------------------
ventas <- read.csv(
  "C:\\Users\\migue\\OneDrive\\Documents\\DEVs\\Taller-Segundo-Corte-Analitica\\dataset_videojuegos_500.csv",
  stringsAsFactors = FALSE
)

ventas$fecha    <- as.Date(ventas$fecha)
ventas$ingresos <- ventas$precio * ventas$cantidad
ventas$mes_num  <- as.integer(format(ventas$fecha, "%m"))

meses_etiq <- c("Ene","Feb","Mar","Abr","May","Jun")

# Paletas de colores reutilizables
pal4 <- c("#4E79A7","#F28E2B","#E15759","#59A14F")          # 4 plataformas
pal6 <- c("#4E79A7","#F28E2B","#E15759","#76B7B2","#59A14F","#EDC948") # 6 géneros/ciudades

# Asignación fija de colores por género y por plataforma
generos    <- c("Accion","Aventura","Deportes","Estrategia","RPG","Shooter")
plataformas <- c("Nintendo","PC","PlayStation","Xbox")
col_gen  <- setNames(pal6, generos)
col_plat <- setNames(pal4, plataformas)

# ============================================================
# PARTE 1: RÉPLICA DE LAS 15 GRÁFICAS
# ============================================================

# ── 1. HISTOGRAMA ────────────────────────────────────────────
# Propósito: visualizar la distribución de los precios de venta.
hist(
  ventas$precio / 1000,
  col    = colorRampPalette(c("#AED6F1","#1A5276"))(8),
  border = "white",
  main   = "Distribución de Precios de Videojuegos",
  xlab   = "Precio (miles de COP)",
  ylab   = "Frecuencia",
  labels = TRUE,
  breaks = 8,
  xaxt   = "n"
)
axis(1, at = pretty(ventas$precio / 1000),
     labels = paste0(pretty(ventas$precio / 1000), " K"))
# Interpretación: Los precios se concentran entre $90.000 y $180.000 COP.
# El gradiente azul permite apreciar visualmente la densidad de cada bin.
# No hay valores extremos ni distribución claramente sesgada.

# ── 2. DIAGRAMA DE BARRAS ────────────────────────────────────
# Propósito: comparar el número de transacciones por plataforma.
ventas_plat <- sort(table(ventas$plataforma), decreasing = TRUE)
barplot(
  ventas_plat,
  col    = col_plat[names(ventas_plat)],
  border = "white",
  main   = "Número de Ventas por Plataforma",
  xlab   = "Plataforma",
  ylab   = "Número de Transacciones",
  las    = 1,
  ylim   = c(0, max(ventas_plat) * 1.15)
)
# Interpretación: Cada barra representa una plataforma diferente con su
# color distintivo. La plataforma más alta es la que concentra mayor
# número de transacciones en el período analizado.

# ── 3. GRÁFICA DE LÍNEAS ─────────────────────────────────────
# Propósito: mostrar la evolución mensual del número de transacciones.
tx_mes <- aggregate(id_venta ~ mes_num, ventas, length)
tx_mes <- tx_mes[order(tx_mes$mes_num), ]

plot(
  tx_mes$mes_num, tx_mes$id_venta,
  type = "o", pch = 16, lwd = 2,
  col  = "#2471A3",
  xaxt = "n",
  main = "Número de Transacciones por Mes",
  xlab = "Mes (2024)",
  ylab = "Número de Transacciones",
  ylim = c(0, max(tx_mes$id_venta) * 1.15)
)
axis(1, at = 1:6, labels = meses_etiq)
grid(nx = NA, ny = NULL, col = "grey85", lty = "dashed")
points(tx_mes$mes_num, tx_mes$id_venta, pch = 16, col = "#2471A3", cex = 1.5)
# Interpretación: La línea conecta los puntos mensuales y permite ver si
# las ventas aumentan, disminuyen o se mantienen estables en el semestre.
# Los círculos resaltan el valor exacto de cada mes.

# ── 4. DIAGRAMA DE DISPERSIÓN ────────────────────────────────
# Propósito: analizar la relación entre precio e ingresos, coloreando
# los puntos por género de videojuego.
plot(
  ventas$precio / 1000, ventas$ingresos / 1e6,
  col  = adjustcolor(col_gen[ventas$genero], alpha.f = 0.6),
  pch  = 16, cex = 0.9,
  main = "Precio vs Ingresos por Género",
  xlab = "Precio (miles de COP)",
  ylab = "Ingresos (millones de COP)",
  xaxt = "n", yaxt = "n"
)
axis(1, at = pretty(ventas$precio / 1000),
     labels = paste0(pretty(ventas$precio / 1000), " K"))
axis(2, at = pretty(ventas$ingresos / 1e6),
     labels = paste0(pretty(ventas$ingresos / 1e6), " M"), las = 1)
legend(
  "topleft",
  legend = generos,
  fill   = col_gen,
  cex    = 0.75,
  bty    = "n",
  title  = "Género"
)
# Interpretación: Se observa una relación lineal positiva: a mayor precio,
# mayores ingresos por transacción. El color por género permite detectar
# si algún género tiende a tener precios consistentemente más altos.

# ── 5. BOXPLOT ───────────────────────────────────────────────
# Propósito: comparar la distribución de ingresos entre ciudades.
ciudades_ord <- names(sort(tapply(ventas$ingresos, ventas$ciudad, median),
                           decreasing = TRUE))
ventas$ingresos_M <- ventas$ingresos / 1e6
boxplot(
  ingresos_M ~ factor(ciudad, levels = ciudades_ord),
  data   = ventas,
  col    = pal6,
  border = "grey30",
  main   = "Distribución de Ingresos por Ciudad",
  xlab   = "Ciudad",
  ylab   = "Ingresos (millones de COP)",
  las    = 1,
  yaxt   = "n"
)
axis(2, at = pretty(ventas$ingresos_M),
     labels = paste0(pretty(ventas$ingresos_M), " M"), las = 1)
# Interpretación: Cada caja muestra mediana (línea central), Q1-Q3 (caja)
# y los bigotes (rango estadístico). Las ciudades están ordenadas de mayor
# a menor mediana para facilitar la comparación.

# ── 6. GRÁFICA DE DENSIDAD ───────────────────────────────────
# Propósito: visualizar la distribución suavizada de la cantidad de
# unidades vendidas por transacción.
dens_cant <- density(ventas$cantidad)
plot(
  dens_cant,
  col  = "#8E44AD", lwd = 2,
  main = "Densidad de la Cantidad de Unidades Vendidas",
  xlab = "Cantidad de Unidades por Transacción",
  ylab = "Densidad"
)
polygon(dens_cant, col = adjustcolor("#8E44AD", 0.25), border = NA)
# Interpretación: La curva suavizada muestra los valores de cantidad más
# frecuentes. Si hay varios picos (multimodal), podría indicar patrones
# de compra diferenciados (ej. compras individuales vs compras múltiples).

# ── 7. MAPA DE CALOR (HEATMAP) ───────────────────────────────
# Propósito: identificar qué combinaciones género-plataforma generan
# mayores ingresos promedio.
mat_hm <- tapply(ventas$ingresos,
                 list(ventas$genero, ventas$plataforma),
                 mean)
mat_hm[is.na(mat_hm)] <- 0

heatmap(
  mat_hm,
  col     = colorRampPalette(c("#D6EAF8","#1F618D","#1A2343"))(25),
  Rowv    = NA,
  Colv    = NA,
  main    = "Ingreso Promedio por Género y Plataforma",
  xlab    = "Plataforma",
  ylab    = "Género",
  margins = c(8, 8),
  scale   = "none"
)
# Interpretación: Las celdas más oscuras representan las combinaciones
# género-plataforma con mayor ingreso promedio. Permite detectar qué
# plataforma es más rentable para cada género de videojuego.

# ── 8. DIAGRAMA DE VIOLÍN ────────────────────────────────────
# Propósito: comparar la forma completa de la distribución de ingresos
# por género, combinando densidad y boxplot.
ggplot(ventas, aes(x = genero, y = ingresos / 1e6, fill = genero)) +
  geom_violin(trim = FALSE, alpha = 0.85) +
  geom_boxplot(width = 0.08, fill = "white",
               outlier.size = 0.8, outlier.alpha = 0.5) +
  scale_fill_manual(values = col_gen) +
  scale_y_continuous(labels = function(x) paste0(x, " M")) +
  labs(
    title = "Distribución de Ingresos por Género (Diagrama de Violín)",
    x     = "Género de Videojuego",
    y     = "Ingresos (millones de COP)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 30, hjust = 1),
    plot.title      = element_text(face = "bold", hjust = 0.5)
  )
# Interpretación: La anchura del violín indica la densidad de datos en ese
# rango de ingresos. Un violín ancho en la parte media indica que la mayoría
# de transacciones de ese género tienen ingresos similares.

# ── 9. GRÁFICA DE ÁREA ───────────────────────────────────────
# Propósito: mostrar la evolución y acumulación de ingresos mes a mes.
ing_mes <- aggregate(ingresos ~ mes_num, ventas, sum)
ing_mes <- ing_mes[order(ing_mes$mes_num), ]

# Convertir a millones para que el eje Y sea legible
ing_mes$ingresos_M <- ing_mes$ingresos / 1e6
ylim_max <- max(ing_mes$ingresos_M) * 1.15

plot(
  ing_mes$mes_num, ing_mes$ingresos_M,
  type = "n",
  xaxt = "n", yaxt = "n",
  main = "Ingresos Totales por Mes",
  xlab = "Mes (2024)",
  ylab = "Ingresos Totales (millones de COP)",
  ylim = c(0, ylim_max)
)
polygon(
  c(ing_mes$mes_num[1], ing_mes$mes_num, tail(ing_mes$mes_num, 1)),
  c(0, ing_mes$ingresos_M, 0),
  col    = adjustcolor("#2471A3", 0.35),
  border = NA
)
lines(ing_mes$mes_num,  ing_mes$ingresos_M, col = "#2471A3", lwd = 2)
points(ing_mes$mes_num, ing_mes$ingresos_M, pch = 16, col = "#2471A3", cex = 1.5)

# Eje X con nombres de meses
axis(1, at = 1:6, labels = meses_etiq)

# Eje Y con formato "XX.X M" para evitar números largos
y_ticks <- pretty(c(0, ylim_max))
axis(2, at = y_ticks, labels = paste0(y_ticks, " M"), las = 1)

grid(nx = NA, ny = NULL, col = "grey85", lty = "dashed")
# Interpretación: El área sombreada facilita la percepción del volumen total
# de ingresos por mes. El eje Y muestra valores en millones de COP para
# mayor legibilidad. Los puntos marcan el valor exacto de cada período
# y la línea superior muestra la tendencia general.

# ── 10. GRÁFICA DE PASTEL ────────────────────────────────────
# Propósito: mostrar la participación porcentual de cada género en
# el total de transacciones.
tbl_gen <- sort(table(ventas$genero), decreasing = TRUE)
pct     <- round(100 * tbl_gen / sum(tbl_gen), 1)
etiq    <- paste0(names(tbl_gen), "\n", pct, "%")

pie(
  tbl_gen,
  labels = etiq,
  col    = col_gen[names(tbl_gen)],
  main   = "Distribución de Ventas por Género de Videojuego",
  cex    = 0.9
)
# Interpretación: Cada porción representa la proporción de transacciones
# de ese género sobre el total. Los géneros con mayor porcentaje son los
# más populares en volumen de ventas durante el período analizado.

# ── 11. PAIR PLOT (MATRIZ DE DISPERSIÓN) ─────────────────────
# Propósito: analizar simultáneamente las relaciones entre todas las
# variables numéricas del dataset.
pairs(
  data.frame(
    "Precio\n(miles COP)"    = ventas$precio   / 1000,
    "Cantidad\n(Unidades)"   = ventas$cantidad,
    "Ingresos\n(millones COP)" = ventas$ingresos / 1e6
  ),
  col    = adjustcolor(col_gen[ventas$genero], 0.5),
  pch    = 16, cex = 0.6,
  main   = "Matriz de Dispersión: Variables Numéricas",
  labels = c("Precio\n(miles COP)", "Cantidad\n(Unidades)", "Ingresos\n(mill. COP)")
)
# Interpretación: La diagonal indica el nombre de cada variable. Cada celda
# fuera de la diagonal es un scatter de dos variables. La fuerte correlación
# precio-ingresos es esperable (ingresos = precio × cantidad).

# ── 12. GRÁFICA DE CORRELACIÓN ───────────────────────────────
# Propósito: cuantificar y visualizar las correlaciones entre variables
# numéricas mediante una matriz de calor con coeficientes.
mat_cor <- cor(ventas[, c("precio","cantidad","ingresos")])
corrplot(
  mat_cor,
  method      = "color",
  type        = "upper",
  tl.col      = "black",
  tl.srt      = 45,
  addCoef.col = "black",
  number.cex  = 0.9,
  col         = colorRampPalette(c("#1A5276","white","#922B21"))(200),
  title       = "Matriz de Correlación: Variables Numéricas",
  mar         = c(0, 0, 2, 0)
)
# Interpretación: Los valores van de -1 (correlación negativa perfecta) a
# +1 (correlación positiva perfecta). Una alta correlación precio-ingresos
# es esperable ya que ingresos = precio × cantidad.

# ── 13. GRÁFICA DE REGRESIÓN ─────────────────────────────────
# Propósito: mostrar la tendencia lineal entre precio e ingresos y
# evaluar la bondad del ajuste con el R².
ventas$precio_K   <- ventas$precio   / 1000
ventas$ingresos_M <- ventas$ingresos / 1e6
modelo <- lm(ingresos_M ~ precio_K, data = ventas)
r2     <- round(summary(modelo)$r.squared, 3)

plot(
  ventas$precio_K, ventas$ingresos_M,
  col  = adjustcolor(col_gen[ventas$genero], 0.5),
  pch  = 16, cex = 0.8,
  main = "Regresión Lineal: Precio vs Ingresos",
  xlab = "Precio (miles de COP)",
  ylab = "Ingresos (millones de COP)",
  xaxt = "n", yaxt = "n"
)
axis(1, at = pretty(ventas$precio_K),
     labels = paste0(pretty(ventas$precio_K), " K"))
axis(2, at = pretty(ventas$ingresos_M),
     labels = paste0(pretty(ventas$ingresos_M), " M"), las = 1)
abline(modelo, col = "#1A2343", lwd = 2, lty = 1)
legend(
  "topleft",
  legend = c(paste("R² =", r2), "Línea de regresión"),
  col    = c(NA, "#1A2343"),
  lty    = c(NA, 1), lwd = c(NA, 2),
  pch    = c(NA, NA),
  bty    = "n", cex = 0.85
)
# Interpretación: La línea azul representa el modelo lineal ajustado.
# El R² indica qué porcentaje de la variación en ingresos es explicado
# por el precio. Puntos alejados de la línea son observaciones atípicas.

# ── 14. SERIE DE TIEMPO ──────────────────────────────────────
# Propósito: observar el comportamiento cronológico de los ingresos
# diarios a lo largo del semestre.
ing_fecha <- aggregate(ingresos ~ fecha, ventas, sum)
ing_fecha <- ing_fecha[order(ing_fecha$fecha), ]
ing_fecha$ingresos_M <- ing_fecha$ingresos / 1e6

plot(
  ing_fecha$fecha, ing_fecha$ingresos_M,
  type = "l", col = "#27AE60", lwd = 1.5,
  main = "Serie de Tiempo: Ingresos Diarios Totales",
  xlab = "Fecha (2024)",
  ylab = "Ingresos Diarios (millones de COP)",
  yaxt = "n"
)
axis(2, at = pretty(ing_fecha$ingresos_M),
     labels = paste0(pretty(ing_fecha$ingresos_M), " M"), las = 1)
lines(
  lowess(as.numeric(ing_fecha$fecha), ing_fecha$ingresos_M, f = 0.15),
  col = "#1A2343", lwd = 2, lty = 2
)
legend(
  "topleft",
  legend = c("Ingresos diarios","Tendencia (lowess)"),
  col    = c("#27AE60","#1A2343"),
  lty    = c(1, 2), lwd = c(1.5, 2),
  bty    = "n", cex = 0.85
)
grid(col = "grey85")
# Interpretación: La línea verde muestra la variación diaria; la línea
# punteada oscura suaviza el ruido y revela la tendencia real del período.
# Picos en la serie pueden corresponder a fechas de promociones o lanzamientos.

# ── 15. GRÁFICA DE BURBUJAS ──────────────────────────────────
# Propósito: relacionar precio (X), ingresos (Y) y cantidad vendida
# (tamaño de burbuja) en una sola visualización.
symbols(
  ventas$precio / 1000, ventas$ingresos / 1e6,
  circles = ventas$cantidad,
  inches  = 0.18,
  bg      = adjustcolor(col_gen[ventas$genero], 0.55),
  fg      = "white",
  main    = "Precio vs Ingresos (tamaño = Cantidad vendida)",
  xlab    = "Precio (miles de COP)",
  ylab    = "Ingresos (millones de COP)",
  xaxt    = "n", yaxt = "n"
)
axis(1, at = pretty(ventas$precio / 1000),
     labels = paste0(pretty(ventas$precio / 1000), " K"))
axis(2, at = pretty(ventas$ingresos / 1e6),
     labels = paste0(pretty(ventas$ingresos / 1e6), " M"), las = 1)
legend(
  "topleft",
  legend = generos,
  fill   = adjustcolor(col_gen, 0.75),
  cex    = 0.75, bty = "n",
  title  = "Género"
)
# Interpretación: Burbujas más grandes indican mayor cantidad de unidades
# vendidas en esa transacción. Se pueden detectar simultáneamente qué
# géneros tienen alto precio, altos ingresos y alto volumen de unidades.


# ============================================================
# PARTE 2: ANÁLISIS
# ¿Qué factores influyen en los ingresos de videojuegos?
# ============================================================

# ── A. BARRAS: INGRESOS TOTALES POR GÉNERO ───────────────────
ing_genero <- aggregate(ingresos ~ genero, ventas, sum)
ing_genero <- ing_genero[order(ing_genero$ingresos, decreasing = TRUE), ]

# Convertir a millones para eje Y legible
ing_genero$ingresos_M <- ing_genero$ingresos / 1e6
ylim_max_A <- max(ing_genero$ingresos_M) * 1.15

bp <- barplot(
  ing_genero$ingresos_M,
  names.arg = ing_genero$genero,
  col       = col_gen[ing_genero$genero],
  border    = "white",
  main      = "Ingresos Totales por Género de Videojuego",
  xlab      = "Género",
  ylab      = "Ingresos Totales (millones de COP)",
  las       = 1,
  yaxt      = "n",
  ylim      = c(0, ylim_max_A)
)
# Eje Y con sufijo "M"
y_ticks_A <- pretty(c(0, ylim_max_A))
axis(2, at = y_ticks_A, labels = paste0(y_ticks_A, " M"), las = 1)
# Justificación: el diagrama de barras ordenado permite comparar de forma
# inmediata cuáles géneros son más rentables en términos de ingresos totales.
# Interpretación: El género con la barra más alta es el que más factura en
# el período. Las diferencias entre géneros revelan oportunidades de inversión.

# ── B. BOXPLOT: PRECIO POR PLATAFORMA ────────────────────────
plat_ord <- names(sort(tapply(ventas$precio, ventas$plataforma, median),
                       decreasing = TRUE))
boxplot(
  I(precio / 1000) ~ factor(plataforma, levels = plat_ord),
  data   = ventas,
  col    = col_plat[plat_ord],
  border = "grey30",
  main   = "Variabilidad de Precios por Plataforma",
  xlab   = "Plataforma",
  ylab   = "Precio (miles de COP)",
  las    = 1,
  yaxt   = "n"
)
axis(2, at = pretty(ventas$precio / 1000),
     labels = paste0(pretty(ventas$precio / 1000), " K"), las = 1)
# Justificación: el boxplot revela no solo el precio promedio por plataforma,
# sino también su dispersión, lo cual es clave para la estrategia de precios.
# Interpretación: Plataformas con cajas más altas tienen juegos de mayor
# precio mediano; bigotes largos indican alta variabilidad de precios.

# ── C. LÍNEAS: TOP 3 GÉNEROS POR MES ─────────────────────────
top3 <- names(sort(tapply(ventas$ingresos, ventas$genero, sum),
                   decreasing = TRUE))[1:3]
ventas_top3 <- ventas[ventas$genero %in% top3, ]
ing_mes_gen <- aggregate(ingresos ~ mes_num + genero, ventas_top3, sum)
cols_top3   <- setNames(pal6[1:3], top3)

ing_mes_gen$ingresos_M <- ing_mes_gen$ingresos / 1e6

plot(
  1, type = "n",
  xlim = c(1, 6), ylim = c(0, max(ing_mes_gen$ingresos_M) * 1.15),
  main = "Evolución Mensual de Ingresos - Top 3 Géneros",
  xlab = "Mes (2024)",
  ylab = "Ingresos (millones de COP)",
  xaxt = "n", yaxt = "n"
)
axis(1, at = 1:6, labels = meses_etiq)
axis(2, at = pretty(c(0, max(ing_mes_gen$ingresos_M))),
     labels = paste0(pretty(c(0, max(ing_mes_gen$ingresos_M))), " M"), las = 1)
grid(nx = NA, ny = NULL, col = "grey85", lty = "dashed")
for (g in top3) {
  sub <- ing_mes_gen[ing_mes_gen$genero == g, ]
  sub <- sub[order(sub$mes_num), ]
  lines(sub$mes_num, sub$ingresos_M,
        col = cols_top3[g], lwd = 2, type = "o", pch = 16)
}
legend("topright", legend = top3, col = cols_top3,
       lwd = 2, pch = 16, bty = "n", cex = 0.85, title = "Género")
# Justificación: las líneas permiten comparar tendencias temporales de los
# géneros más rentables en el mismo gráfico y la misma escala.
# Interpretación: Si una línea sube consistentemente, ese género está
# ganando tracción. Cruces entre líneas indican cambios de liderazgo.

# ── D. PIE: PARTICIPACIÓN DE INGRESOS POR CIUDAD ─────────────
ing_ciudad <- sort(tapply(ventas$ingresos, ventas$ciudad, sum),
                   decreasing = TRUE)
pct_c  <- round(100 * ing_ciudad / sum(ing_ciudad), 1)
etiq_c <- paste0(names(ing_ciudad), "\n", pct_c, "%")

pie(
  ing_ciudad,
  labels = etiq_c,
  col    = pal6[1:length(ing_ciudad)],
  main   = "Participación de Ingresos por Ciudad",
  cex    = 0.9
)
# Justificación: la gráfica circular muestra de forma intuitiva qué ciudades
# aportan mayor proporción al total de ingresos del período.
# Interpretación: Las ciudades con porciones más grandes concentran el mercado
# y deben priorizarse en campañas y distribución de inventario.

# ── E. BARRAS AGRUPADAS: CIUDAD × PLATAFORMA ─────────────────
mat <- xtabs(ingresos ~ ciudad + plataforma, ventas)
mat_M <- mat / 1e6

barplot(
  t(mat_M),
  beside      = TRUE,
  col         = col_plat[colnames(mat_M)],
  border      = "white",
  main        = "Ingresos por Ciudad y Plataforma",
  xlab        = "Ciudad",
  ylab        = "Ingresos (millones de COP)",
  las         = 1,
  yaxt        = "n",
  legend.text = colnames(mat_M),
  args.legend = list(x = "topright", bty = "n", cex = 0.75, title = "Plataforma")
)
axis(2, at = pretty(c(0, max(mat_M))),
     labels = paste0(pretty(c(0, max(mat_M))), " M"), las = 1)
# Justificación: las barras agrupadas permiten una doble comparación:
# entre ciudades y entre plataformas dentro de cada ciudad.
# Interpretación: Se identifica qué plataforma lidera en cada ciudad y qué
# combinación ciudad-plataforma genera los mayores ingresos totales.


# ============================================================
# CONCLUSIÓN FINAL
# ============================================================
# 1. El género de videojuego es el principal factor de diferenciación en
#    ingresos; los tres géneros más rentables concentran la mayoría de la
#    facturación total del semestre.
# 2. Existe una correlación positiva fuerte entre precio e ingresos,
#    confirmada tanto por el scatter como por la regresión lineal (R²).
# 3. La variabilidad de precios por plataforma es relativamente uniforme,
#    lo que sugiere una estrategia de precios homogénea entre plataformas.
# 4. Algunas ciudades concentran una proporción desproporcionada de ingresos,
#    siendo candidatas prioritarias para estrategias de marketing localizado.
# 5. La serie de tiempo no muestra una tendencia creciente clara, pero sí
#    variaciones diarias que podrían responder a eventos puntuales del sector.