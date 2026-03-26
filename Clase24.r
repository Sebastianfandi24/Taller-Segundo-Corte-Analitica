# ============================================================
#  TALLER DE ANÁLISIS DE DATOS CON R
#  Tema: Análisis de Ventas de Videojuegos en Colombia
#  Dataset: 500 registros de ventas (Enero-Junio 2024)
# ============================================================

# Evitar notación científica
options(scipen = 999)

# Librerías necesarias
library(ggplot2)
library(corrplot)

# ------------------------------------------------------------
# CARGA Y PREPARACIÓN DEL DATASET
# ------------------------------------------------------------
ventas <- read.csv("dataset_videojuegos_500.csv", stringsAsFactors = FALSE)
ventas$fecha    <- as.Date(ventas$fecha)
ventas$ingresos <- ventas$precio * ventas$cantidad
ventas$mes_num  <- as.integer(format(ventas$fecha, "%m"))
ventas$precio_K <- ventas$precio / 1000
ventas$ingresos_M <- ventas$ingresos / 1e6

meses_etiq <- c("Ene","Feb","Mar","Abr","May","Jun")
pal4 <- c("#4E79A7","#F28E2B","#E15759","#59A14F")
pal6 <- c("#4E79A7","#F28E2B","#E15759","#76B7B2","#59A14F","#EDC948")

generos <- c("Accion","Aventura","Deportes","Estrategia","RPG","Shooter")
plataformas <- c("Nintendo","PC","PlayStation","Xbox")
col_gen  <- setNames(pal6, generos)
col_plat <- setNames(pal4, plataformas)

# ============================================================
# FUNCIÓN PARA GUARDAR GRÁFICAS AUTOMÁTICAMENTE
# ============================================================
guardar_grafica <- function(nombre_archivo, ancho=12, alto=8) {
  png(nombre_archivo, width=ancho, height=alto, units="in", res=300)
}

# ============================================================
# PARTE 1: LAS 15 GRÁFICAS 
# ============================================================
# ── 1. HISTOGRAMA ────────────────────────────────────────────
#  Observamos que la mayoría de videojuegos cuestan 
#  alrededor de 100K COP, con una fuerte concentración
#  en ese rango. Hay menos juegos en los rangos de 
#  precios más altos (170-180K), indicando que los 
#  juegos más caros son menos frecuentes.

guardar_grafica("01_histograma_precios.png")
hist(ventas$precio / 1000, col=colorRampPalette(c("#AED6F1","#1A5276"))(8),
     border="white", main="Distribución de Precios de Videojuegos",
     xlab="Precio (miles de COP)", ylab="Frecuencia", labels=TRUE, breaks=8, xaxt="n")
axis(1, at=pretty(ventas$precio/1000), labels=paste0(pretty(ventas$precio/1000)," K"))
dev.off()

# ── 2. DIAGRAMA DE BARRAS ────────────────────────────────────

# PlayStation y Xbox son las plataformas más populares
# con 135 transacciones cada una, seguidas por PC(140)
# y Nintendo (100).

guardar_grafica("02_barras_plataformas.png")
barplot(ventas_plat, col=col_plat[names(ventas_plat)], border="white",
        main="Número de Ventas por Plataforma", xlab="Plataforma",
        ylab="Número de Transacciones", las=1, ylim=c(0,max(ventas_plat)*1.15))
dev.off()

# ── 3. GRÁFICA DE LÍNEAS ─────────────────────────────────────

#  El número de transacciones mensuales muestra una
#  tendencia positiva desde enero hasta marzo (70-93),
#  con un pico de 93 en marzo. Luego desciende en
#  abril, repunta en mayo y cae nuevamente en junio.
#  Esto sugiere variabilidad estacional en las ventas.

tx_mes <- aggregate(id_venta ~ mes_num, ventas, length)
tx_mes <- tx_mes[order(tx_mes$mes_num),]
guardar_grafica("03_lineas_transacciones_mes.png")
plot(tx_mes$mes_num, tx_mes$id_venta, type="o", pch=16, lwd=2, col="#2471A3",
     xaxt="n", main="Número de Transacciones por Mes", xlab="Mes (2024)",
     ylab="Número de Transacciones", ylim=c(0,max(tx_mes$id_venta)*1.15))
axis(1, at=1:6, labels=meses_etiq)
grid(nx=NA, ny=NULL, col="grey85", lty="dashed")
dev.off()

# ── 4. DIAGRAMA DE DISPERSIÓN ────────────────────────────────
#  Mostrar relación entre dos variables continuas
#  Se observa correlación positiva débil entre precio
#  e ingresos (R²≈0.16). Productos con precios similares
#  generan ingresos muy variables, indicando que factores
#  como cantidad vendida y género son también importantes.
guardar_grafica("04_scatter_precio_ingresos.png")
plot(ventas$precio_K, ventas$ingresos_M, col=adjustcolor(col_gen[ventas$genero], 0.6),
     pch=16, cex=0.9, main="Precio vs Ingresos por Género",
     xlab="Precio (miles de COP)", ylab="Ingresos (millones de COP)", xaxt="n", yaxt="n")
axis(1, at=pretty(ventas$precio_K), labels=paste0(pretty(ventas$precio_K)," K"))
axis(2, at=pretty(ventas$ingresos_M), labels=paste0(pretty(ventas$ingresos_M)," M"), las=1)
legend("topleft", legend=generos, fill=col_gen, cex=0.75, bty="n", title="Género")
dev.off()

# ── 5. BOXPLOT ───────────────────────────────────────────────
#  Comparar distribuciones entre grupos y detectar outliers
#  Bogotá y Cartagena tienen medianas similares
#  (~370K COP), mientras Cali tiene la menor mediana
#  (~330K COP). Las distribuciones son anchuras
#  comparables, pero con algunos outliers en ciudades
#  como Bogotá (ingresos mayores a 700K).
ciudades_ord <- names(sort(tapply(ventas$ingresos, ventas$ciudad, median), decreasing=TRUE))
guardar_grafica("05_boxplot_ciudades.png")
boxplot(ingresos_M ~ factor(ciudad, levels=ciudades_ord), data=ventas, col=pal6,
        border="grey30", main="Distribución de Ingresos por Ciudad",
        xlab="Ciudad", ylab="Ingresos (millones de COP)", las=1, yaxt="n")
axis(2, at=pretty(ventas$ingresos_M), labels=paste0(pretty(ventas$ingresos_M)," M"), las=1)
dev.off()

# ── 6. GRÁFICA DE DENSIDAD ───────────────────────────────────
#  Mostrar la distribución suave de una variable continua
#  La cantidad de unidades vendidas muestra una
#  distribución multimodal con picos alrededor de
#  1, 2, 3 y 4 unidades. La mayoría de transacciones
#  involucran 1-3 unidades, siendo raros los casos
#  de 4+ unidades por transacción.
dens_cant <- density(ventas$cantidad)
guardar_grafica("06_densidad_cantidad.png")
plot(dens_cant, col="#8E44AD", lwd=2, main="Densidad de la Cantidad de Unidades Vendidas",
     xlab="Cantidad de Unidades por Transacción", ylab="Densidad")
polygon(dens_cant, col=adjustcolor("#8E44AD", 0.25), border=NA)
dev.off()

# ── 7. MAPA DE CALOR ────────────────────────────────────────
#  Mostrar patrones en datos bidimensionales con color
#  El heatmap revela que la combinación de Shooter en
#  PlayStation y Estrategia en PC generan los ingresos
#  promedio más altos. Acción en PC tiene menores
#  ingresos promedio, sugiriendo diferencias en
#  preferencias de géneros por plataforma.
mat_hm <- tapply(ventas$ingresos, list(ventas$genero, ventas$plataforma), mean)
mat_hm[is.na(mat_hm)] <- 0
guardar_grafica("07_heatmap_genero_plataforma.png")
heatmap(mat_hm, col=colorRampPalette(c("#D6EAF8","#1F618D","#1A2343"))(25),
        Rowv=NA, Colv=NA, main="Ingreso Promedio por Género y Plataforma",
        xlab="Plataforma", ylab="Género", margins=c(8,8), scale="none")
dev.off()

# ── 8. DIAGRAMA DE VIOLÍN (ggplot2) ─────────────────────────
#  Comparar distribuciones completas entre grupos
#  El género Estrategia muestra la distribución más
#  concentrada de ingresos, mientras Shooter presenta
#  mayor dispersión. RPG y Acción tienen distribuciones
#  más simétricas. Esto sugiere que no todos los géneros
#  tienen comportamientos de ingreso similares.
guardar_grafica("08_violin_generos.png", ancho=14, alto=8)
print(ggplot(ventas, aes(x=genero, y=ingresos/1e6, fill=genero)) +
  geom_violin(trim=FALSE, alpha=0.85) +
  geom_boxplot(width=0.08, fill="white", outlier.size=0.8, outlier.alpha=0.5) +
  scale_fill_manual(values=col_gen) +
  scale_y_continuous(labels=function(x) paste0(x," M")) +
  labs(title="Distribución de Ingresos por Género (Diagrama de Violín)",
       x="Género de Videojuego", y="Ingresos (millones de COP)") +
  theme_minimal(base_size=12) +
  theme(legend.position="none", axis.text.x=element_text(angle=30,hjust=1),
        plot.title=element_text(face="bold", hjust=0.5)))
dev.off()

# ── 9. GRÁFICA DE ÁREA ──────────────────────────────────────
#  Mostrar magnitud y tendencia de una variable en el tiempo
#  Los ingresos totales mensuales muestran estacionalidad
#  fuerte. Marzo es el mes de máximos ingresos (~32M COP),
#  seguido por mayo (~33M). Febrero y abril son más débiles.
#  La pendiente positiva en junio sugiere recuperación.
ing_mes <- aggregate(ingresos ~ mes_num, ventas, sum)
ing_mes <- ing_mes[order(ing_mes$mes_num),]
ing_mes$ingresos_M <- ing_mes$ingresos / 1e6
ylim_max <- max(ing_mes$ingresos_M) * 1.15
guardar_grafica("09_area_ingresos_mes.png")
plot(ing_mes$mes_num, ing_mes$ingresos_M, type="n", xaxt="n", yaxt="n",
     main="Ingresos Totales por Mes", xlab="Mes (2024)",
     ylab="Ingresos Totales (millones de COP)", ylim=c(0,ylim_max))
polygon(c(ing_mes$mes_num[1], ing_mes$mes_num, tail(ing_mes$mes_num,1)),
       c(0, ing_mes$ingresos_M, 0), col=adjustcolor("#2471A3",0.35), border=NA)
lines(ing_mes$mes_num, ing_mes$ingresos_M, col="#2471A3", lwd=2)
points(ing_mes$mes_num, ing_mes$ingresos_M, pch=16, col="#2471A3", cex=1.5)
axis(1, at=1:6, labels=meses_etiq)
axis(2, at=pretty(c(0,ylim_max)), labels=paste0(pretty(c(0,ylim_max))," M"), las=1)
grid(nx=NA, ny=NULL, col="grey85", lty="dashed")
dev.off()

# ── 10. GRÁFICA DE PASTEL ───────────────────────────────────
#  Mostrar proporciones o participaciones de un total
#  La distribución de ventas por género es relativamente
#  balanceada, con Estrategia (19.4%) y Shooter (18.4%)
#  siendo ligeramente más populares. Acción es el género
#  menos vendido (14.2%), sugiriendo demanda diversificada.
tbl_gen <- sort(table(ventas$genero), decreasing=TRUE)
pct <- round(100*tbl_gen/sum(tbl_gen),1)
etiq <- paste0(names(tbl_gen),"\n",pct,"%")
guardar_grafica("10_pie_generos.png")
pie(tbl_gen, labels=etiq, col=col_gen[names(tbl_gen)],
    main="Distribución de Ventas por Género de Videojuego", cex=0.9)
dev.off()

# ── 11. PAIR PLOT ───────────────────────────────────────────
#  Explorar todas las relaciones entre variables numéricas
#  La matriz de dispersión múltiple reveal que cantidad
#  e ingresos están fuertemente correlacionados, mientras
#  precio muestra relación más débil con ambos. La
#  visualización simultánea ayuda a distinguir patrones
#  multidimensionales.
guardar_grafica("11_pairs_numericas.png", ancho=12, alto=10)
pairs(data.frame("Precio\n(miles COP)"=ventas$precio_K,
                 "Cantidad\n(Unidades)"=ventas$cantidad,
                 "Ingresos\n(millones COP)"=ventas$ingresos_M),
      col=adjustcolor(col_gen[ventas$genero],0.5), pch=16, cex=0.6,
      main="Matriz de Dispersión: Variables Numéricas")
dev.off()

# ── 12. MATRIZ DE CORRELACIÓN ───────────────────────────────
#  Cuantificar relaciones lineales entre variables
#  Cantidad e ingresos muestran correlación muy fuerte
#  (0.88), lo que es lógico. Precio correlaciona
#  débilmente con ingresos (0.40), indicando que el
#  precio NO es el factor determinante principal de
#  ingresos. La gestión de volumen es más crítica.
mat_cor <- cor(ventas[,c("precio","cantidad","ingresos")])
guardar_grafica("12_correlacion.png")
corrplot(mat_cor, method="color", type="upper", tl.col="black", tl.srt=45,
         addCoef.col="black", number.cex=0.9,
         col=colorRampPalette(c("#1A5276","white","#922B21"))(200),
         title="Matriz de Correlación: Variables Numéricas", mar=c(0,0,2,0))
dev.off()

# ── 13. REGRESIÓN LINEAL ────────────────────────────────────
#  Modelar relación lineal e identificar tendencia general
#  El modelo tiene R²=0.159, indicando que el precio
#  explica solo el 15.9% de la variabilidad en ingresos.
#  Existe relación positiva pero débil. Los residuales
#  ampliamente dispersos indican que otros factores
#  (cantidad, género, ciudad) son más influyentes.
modelo <- lm(ingresos_M ~ precio_K, data=ventas)
r2 <- round(summary(modelo)$r.squared,3)
guardar_grafica("13_regresion_lineal.png")
plot(ventas$precio_K, ventas$ingresos_M, col=adjustcolor(col_gen[ventas$genero],0.5),
     pch=16, cex=0.8, main="Regresión Lineal: Precio vs Ingresos",
     xlab="Precio (miles de COP)", ylab="Ingresos (millones de COP)", xaxt="n", yaxt="n")
axis(1, at=pretty(ventas$precio_K), labels=paste0(pretty(ventas$precio_K)," K"))
axis(2, at=pretty(ventas$ingresos_M), labels=paste0(pretty(ventas$ingresos_M)," M"), las=1)
abline(modelo, col="#1A2343", lwd=2)
legend("topleft", legend=c(paste("R² =",r2),"Línea de regresión"),
       col=c(NA,"#1A2343"), lty=c(NA,1), lwd=c(NA,2), pch=c(NA,NA), bty="n", cex=0.85)
dev.off()

# ── 14. SERIE DE TIEMPO ─────────────────────────────────────
#  Analizar patrones en datos a nivel diario/granular
#  Los ingresos diarios muestran alta volatilidad con
#  picos esporádicos (máximo ~2.9M COP). La tendencia
#  (línea discontinua) revela aumento general hasta
#  marzo-abril, estabilización en mayo, y recuperación
#  en junio. Patrón consistente con análisis mensual.
ing_fecha <- aggregate(ingresos ~ fecha, ventas, sum)
ing_fecha <- ing_fecha[order(ing_fecha$fecha),]
ing_fecha$ingresos_M <- ing_fecha$ingresos / 1e6
guardar_grafica("14_serie_tiempo.png")
plot(ing_fecha$fecha, ing_fecha$ingresos_M, type="l", col="#27AE60", lwd=1.5,
     main="Serie de Tiempo: Ingresos Diarios Totales", xlab="Fecha (2024)",
     ylab="Ingresos Diarios (millones de COP)", yaxt="n")
axis(2, at=pretty(ing_fecha$ingresos_M), labels=paste0(pretty(ing_fecha$ingresos_M)," M"), las=1)
lines(lowess(as.numeric(ing_fecha$fecha), ing_fecha$ingresos_M, f=0.15),
      col="#1A2343", lwd=2, lty=2)
legend("topleft", legend=c("Ingresos diarios","Tendencia (lowess)"),
       col=c("#27AE60","#1A2343"), lty=c(1,2), lwd=c(1.5,2), bty="n", cex=0.85)
grid(col="grey85")
dev.off()

# ── 15. GRÁFICA DE BURBUJAS ─────────────────────────────────
#  Visualizar 3+ variables simultáneamente (precio, ingresos, cantidad)
#  El tamaño de burbuja representa cantidad vendida.
#  Se observa que mayores cantidades generalmente producen
#  mayores ingresos (correlación visible). La dispersión
#  en cantidad es evidente. Algunos juegos de alto precio
#  (180K) vendidos en pequeñas cantidades generan
#  ingresos menores que juegos más económicos con
#  mejores volúmenes.
guardar_grafica("15_burbujas.png")
symbols(ventas$precio/1000, ventas$ingresos/1e6, circles=ventas$cantidad,
        inches=0.18, bg=adjustcolor(col_gen[ventas$genero],0.55), fg="white",
        main="Precio vs Ingresos (tamaño = Cantidad vendida)",
        xlab="Precio (miles de COP)", ylab="Ingresos (millones de COP)", xaxt="n", yaxt="n")
axis(1, at=pretty(ventas$precio/1000), labels=paste0(pretty(ventas$precio/1000)," K"))
axis(2, at=pretty(ventas$ingresos/1e6), labels=paste0(pretty(ventas$ingresos/1e6)," M"), las=1)
legend("topleft", legend=generos, fill=adjustcolor(col_gen,0.75), cex=0.75, bty="n", title="Género")
dev.off()


# ============================================================
#           PLANTEAMIENTO DEL PROBLEMA DE ANÁLISIS
# ============================================================
#  Una tienda colombiana de videojuegos registró 500 transacciones
#  en 6 ciudades (Barranquilla, Bogotá, Bucaramanga, Cali,
#  Cartagena, Medellín) durante enero–junio 2024.
#  La gerencia desea tomar decisiones estratégicas basadas en datos.
#
#  PROBLEMA CENTRAL:
#  ¿Cuáles son los factores que más impactan los ingresos de la
#  tienda y cómo varía su comportamiento según plataforma,
#  género, ciudad y período del año?
#
#  PREGUNTAS ESPECÍFICAS:
#  1. ¿La cantidad vendida o el precio es el factor más relevantepara explicar los ingresos?
#  2. ¿Existen patrones estacionales en los ingresos mensuales?
#  3. ¿Qué combinaciones de ciudad y plataforma generan
#     mejores resultados?
#  4. ¿Qué géneros rinden mejor según la plataforma?
#  5. ¿Tiene el precio un impacto real sobre los ingresos totales?
#

# ============================================================

# ------------------------------------------------------------
#  1. ¿La cantidad vendida o el precio es el factor más relevantepara explicar los ingresos?
# ------------------------------------------------------------
# (replica de la gráfica #12)
# La cantidad vendida es claramente el factor más relevante. La matriz de correlación 
# muestra un coeficiente de r = 0.88 entre cantidad e ingresos, lo que indica una 
# relación lineal muy fuerte: a mayor número de unidades vendidas, los ingresos 
# suben de forma casi proporcional. El precio, en cambio, solo alcanza r = 0.40, 
# una relación moderada-baja. Esto significa que dos transacciones con el mismo 
# precio pueden generar ingresos muy distintos dependiendo de cuántas unidades se 
# vendan, pero dos transacciones con la misma cantidad vendida tienden a producir 
# ingresos similares independientemente del precio. La conclusión es directa: 
# el volumen de ventas mueve los ingresos, el precio los ajusta marginalmente.

mat_cor <- cor(ventas[, c("precio", "cantidad", "ingresos")])

guardar_grafica("P1_correlacion.png")
corrplot(mat_cor,
         method    = "color",
         type      = "upper",
         tl.col    = "black",
         tl.srt    = 45,
         addCoef.col = "black",
         number.cex  = 1.1,
         col = colorRampPalette(c("#1A5276", "white", "#922B21"))(200),
         title = "P1 · Matriz de Correlación entre Variables Numéricas",
         mar   = c(0, 0, 2, 0))
dev.off()

# ------------------------------------------------------------
#  2. ¿Existen patrones estacionales en los ingresos mensuales?
# ------------------------------------------------------------
# (replica de la gráfica #9)
# Marzo y mayo son los meses pico con aproximadamente 32–33 millones de COP cada uno, 
# mientras febrero y abril presentan caídas notables respecto a los meses anteriores. 
# Enero parte en un nivel intermedio y junio muestra señales de recuperación. 
# Este comportamiento bimodal sugiere que factores externos como vacaciones 
# escolares, lanzamientos de títulos o fechas especiales de consumo están influenciando 
# la demanda.

ing_mes   <- aggregate(ingresos ~ mes_num, ventas, sum)
ing_mes   <- ing_mes[order(ing_mes$mes_num), ]
ing_mes$ingresos_M <- ing_mes$ingresos / 1e6
ylim_p2   <- max(ing_mes$ingresos_M) * 1.18

guardar_grafica("P2_area_ingresos_mes.png")
plot(ing_mes$mes_num, ing_mes$ingresos_M,
     type = "n", xaxt = "n", yaxt = "n",
     main = "P2 · Ingresos Totales por Mes (Ene–Jun 2024)",
     xlab = "Mes (2024)",
     ylab = "Ingresos Totales (millones de COP)",
     ylim = c(0, ylim_p2))
polygon(c(ing_mes$mes_num[1], ing_mes$mes_num, tail(ing_mes$mes_num, 1)),
        c(0, ing_mes$ingresos_M, 0),
        col = adjustcolor("#2471A3", 0.30), border = NA)
lines(ing_mes$mes_num, ing_mes$ingresos_M, col = "#2471A3", lwd = 2.5)
points(ing_mes$mes_num, ing_mes$ingresos_M, pch = 16, col = "#2471A3", cex = 1.8)
text(ing_mes$mes_num, ing_mes$ingresos_M,
     labels = paste0(round(ing_mes$ingresos_M, 1), " M"),
     pos = 3, cex = 0.8, col = "#1A2343")
axis(1, at = 1:6, labels = meses_etiq)
axis(2, at = pretty(c(0, ylim_p2)),
     labels = paste0(pretty(c(0, ylim_p2)), " M"), las = 1)
grid(nx = NA, ny = NULL, col = "grey85", lty = "dashed")
dev.off()

# ------------------------------------------------------------
#  3. ¿Qué combinaciones de ciudad y plataforma generan mejores resultados?
# ------------------------------------------------------------
#Bogotá con PC y Bogotá con PlayStation son las combinaciones de mayor ingreso 
# en el semestre analizado. En general, PC y PlayStation superan a Nintendo y Xbox 
# en prácticamente todas las ciudades, lo que indica que esta ventaja no es exclusiva 
# de Bogotá sino una tendencia de mercado. Medellín y Cali ocupan el segundo y 
# tercer lugar respectivamente, con un comportamiento similar en cuanto a la 
# jerarquía de plataformas. Nintendo es consistentemente la plataforma de menor 
# ingreso en todas las ciudades. Esto sugiere que concentrar el inventario y las 
# promociones en PC y PlayStation en las ciudades con mayor tráfico (Bogotá, 
# Medellín, Cali) representa la combinación de mayor retorno para la tienda.
mat_cp  <- xtabs(ingresos ~ ciudad + plataforma, ventas)
mat_cpM <- mat_cp / 1e6

guardar_grafica("P3_barras_ciudad_plataforma.png", ancho = 14, alto = 8)
barplot(t(mat_cpM),
        beside      = TRUE,
        col         = col_plat[colnames(mat_cpM)],
        border      = "white",
        main        = "P3 · Ingresos por Ciudad y Plataforma",
        xlab        = "Ciudad",
        ylab        = "Ingresos (millones de COP)",
        las         = 1,
        yaxt        = "n",
        legend.text = colnames(mat_cpM),
        args.legend = list(x = "topright", bty = "n",
                           cex = 0.85, title = "Plataforma"))
axis(2, at     = pretty(c(0, max(mat_cpM))),
        labels = paste0(pretty(c(0, max(mat_cpM))), " M"), las = 1)
dev.off()

# ------------------------------------------------------------
#  4. ¿Qué géneros rinden mejor según la plataforma?
# ------------------------------------------------------------
# (replica de la gráfica #7)
# El heatmap revela que el rendimiento por género varía de forma importante según 
# la plataforma. Shooter en PlayStation y Estrategia en PC son las combinaciones 
# con mayor ingreso promedio por transacción, representadas por las celdas de mayor 
# intensidad de color. RPG muestra un rendimiento alto en PlayStation y moderado 
# en PC. En el extremo opuesto, Acción en Nintendo y Aventura en Xbox producen 
# los ingresos promedio más bajos. Esto tiene una implicación práctica concreta: 
# no existe un género universalmente rentable, sino que cada plataforma tiene su 
# propio segmento dominante. Un catálogo que alinee Shooter y RPG hacia PlayStation, 
# y Estrategia hacia PC, estará mejor posicionado que uno que distribuya los 
# títulos sin distinción de plataforma.

mat_hm <- tapply(ventas$ingresos,
                 list(ventas$genero, ventas$plataforma), mean)
mat_hm[is.na(mat_hm)] <- 0

guardar_grafica("P4_heatmap_genero_plataforma.png")
heatmap(mat_hm,
        col     = colorRampPalette(c("#D6EAF8", "#1F618D", "#1A2343"))(25),
        Rowv    = NA, Colv = NA,
        main    = "P4 · Ingreso Promedio por Género y Plataforma",
        xlab    = "Plataforma", ylab = "Género",
        margins = c(8, 8), scale = "none")
dev.off()

# ------------------------------------------------------------
#  5. ¿Tiene el precio un impacto real sobre los ingresos totales?
# ------------------------------------------------------------
# (replica de la gráfica #4)
#  JUSTIFICACIÓN:
#El impacto del precio sobre los ingresos es real pero muy limitado. El diagrama 
# de dispersión con regresión lineal muestra un R² = 0.159, lo que significa que 
# el precio unitario explica apenas el 15.9% de la variabilidad en los ingresos 
# totales. El 84.1% restante está determinado por otros factores, siendo el más 
# importante la cantidad vendida (como ya confirmó la gráfica P1). Además, la 
# dispersión de los puntos alrededor de la línea de regresión es amplia en todos 
# los géneros, lo que descarta que algún segmento particular rompa esta tendencia. 
# En términos prácticos: subir el precio de un juego no garantiza mayores ingresos, 
# y bajarlo tampoco los reduce necesariamente de forma proporcional. Las decisiones 
# de pricing tienen poco poder predictivo sobre el resultado financiero final.

modelo_p5 <- lm(ingresos_M ~ precio_K, data = ventas)
r2_p5     <- round(summary(modelo_p5)$r.squared, 3)

guardar_grafica("P5_scatter_precio_ingresos.png")
plot(ventas$precio_K, ventas$ingresos_M,
     col  = adjustcolor(col_gen[ventas$genero], 0.55),
     pch  = 16, cex = 0.9,
     main = "P5 · Precio vs. Ingresos por Género",
     xlab = "Precio (miles de COP)",
     ylab = "Ingresos (millones de COP)",
     xaxt = "n", yaxt = "n")
axis(1, at     = pretty(ventas$precio_K),
        labels = paste0(pretty(ventas$precio_K), " K"))
axis(2, at     = pretty(ventas$ingresos_M),
        labels = paste0(pretty(ventas$ingresos_M), " M"), las = 1)
abline(modelo_p5, col = "#1A2343", lwd = 2)
legend("topleft",
       legend = c(generos, paste0("R² = ", r2_p5), "Regresión"),
       fill   = c(col_gen, NA, NA),
       col    = c(rep(NA, length(generos)), NA, "#1A2343"),
       lty    = c(rep(NA, length(generos)), NA, 1),
       lwd    = c(rep(NA, length(generos)), NA, 2),
       border = c(rep("grey60", length(generos)), NA, NA),
       bty    = "n", cex = 0.75, title = "Género")
dev.off()






















# ------------------------------------------------------------
#                 - VALOR AGREGADO -
# ------------------------------------------------------------

# ============================================================
#  6.   ¿En qué mes, plataforma y con qué género
#       debería lanzar un nuevo título para maximizar rentabilidad esperada
#       por transacción?"
# ============================================================
#  JUSTIFICACIÓN Y ANÁLISIS:

# Crear tabla de ingresos promedio por mes-plataforma-género
analisis_lanzamiento <- aggregate(ingresos_M ~ mes_num + plataforma + genero,
                                  data = ventas, FUN = mean)
analisis_lanzamiento <- analisis_lanzamiento[order(analisis_lanzamiento$ingresos_M, 
                                                     decreasing = TRUE), ]

# Obtener top 12 combinaciones
top12_lanzamiento <- head(analisis_lanzamiento, 12)

# Crear etiquetas legibles
top12_lanzamiento$etiqueta <- paste0(
  meses_etiq[top12_lanzamiento$mes_num], " · ",
  substr(top12_lanzamiento$plataforma, 1, 3), " · ",
  substr(top12_lanzamiento$genero, 1, 4)
)

# Crear secuencia de colores por plataforma
colores_top12 <- col_plat[top12_lanzamiento$plataforma]

guardar_grafica("P6_estrategia_lanzamiento.png", ancho = 13, alto = 8)
par(mar = c(6, 5, 4, 2))
bp <- barplot(top12_lanzamiento$ingresos_M,
              names.arg = top12_lanzamiento$etiqueta,
              col = colores_top12,
              border = "white",
              main = "P6 · Top 12 Estrategias de Lanzamiento: Mes × Plataforma × Género",
              xlab = "",
              ylab = "Ingreso Promedio por Transacción (millones de COP)",
              las = 2,
              ylim = c(0, max(top12_lanzamiento$ingresos_M) * 1.2),
              cex.names = 0.9)

# Agregar valores sobre las barras
text(bp, top12_lanzamiento$ingresos_M + 0.01,
     labels = round(top12_lanzamiento$ingresos_M, 3),
     pos = 3, cex = 0.8, col = "#1A2343", font = 2)

# Eje Y mejorado
axis(2, at = pretty(c(0, max(top12_lanzamiento$ingresos_M))),
     labels = paste0(pretty(c(0, max(top12_lanzamiento$ingresos_M))), " M"),
     las = 1)

# Leyenda de plataformas
legend("topright", legend = names(col_plat), fill = col_plat,
       bty = "n", cex = 0.9, title = "Plataforma")

# Resaltar la combinación ganadora
rect(bp[1] - 0.5, -0.05, bp[1] + 0.5, 
     max(top12_lanzamiento$ingresos_M) * 1.2,
     border = "#E74C3C", lwd = 3, lty = 2)
text(bp[1], max(top12_lanzamiento$ingresos_M) * 1.15,
     "★ RECOMENDACIÓN", cex = 0.9, col = "#E74C3C", font = 2, pos = 1)

par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()


mejor_combo <- top12_lanzamiento[1, ]
cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║          RECOMENDACIÓN ESTRATÉGICA DE LANZAMIENTO              ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")
cat("🎯 MEJOR COMBINACIÓN IDENTIFICADA:\n")
cat("   Mes:        ", meses_etiq[mejor_combo$mes_num], "\n")
cat("   Plataforma: ", mejor_combo$plataforma, "\n")
cat("   Género:     ", mejor_combo$genero, "\n")
cat("   Ingreso Promedio por Transacción: ", 
    round(mejor_combo$ingresos_M, 3), " millones de COP\n\n")

cat("📊 ALTERNATIVAS (Top 5 combinaciones):\n")
for(i in 1:5) {
  cat(sprintf("   %d. %s · %s · %s → %.3f M COP\n",
              i, meses_etiq[top12_lanzamiento$mes_num[i]],
              top12_lanzamiento$plataforma[i],
              top12_lanzamiento$genero[i],
              top12_lanzamiento$ingresos_M[i]))
}


cat("🚀 ACCIONES RECOMENDADAS:\n")
cat("   1. Coordinar lanzamiento para ", meses_etiq[mejor_combo$mes_num], "\n")
cat("   2. Priorizar inventario en ", mejor_combo$plataforma, "\n")
cat("   3. Género enfoque: ", mejor_combo$genero, "\n")