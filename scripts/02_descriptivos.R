# ---------------------------------------------------------------------------
# 02_descriptivos.R
# Estadísticas descriptivas del panel y Figura 1.
#
# La figura se dibuja en blanco y negro, con un tipo de línea distinto por
# región, para que se entienda impresa sin color.
#
# Salidas: 03_resultados/tabla_descriptivos.csv (y .html)
#          03_resultados/figura_1_precio_por_hectarea.png
# ---------------------------------------------------------------------------

if (!exists("cfg")) source("scripts/00_config.R")
source("scripts/utilidades.R")

mensaje("== 02_descriptivos: resumen y Figura 1 ==")

panel <- utils::read.csv(file.path(cfg$dir_curados, "panel.csv"),
                         stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# 1. Tabla de descriptivos
# ---------------------------------------------------------------------------
# Se informa n (años con dato) junto a cada estadístico: con series que tienen
# huecos, un promedio sin su n engaña.
variables <- setdiff(names(panel), c("pais", "anio"))
filas <- list()

for (p in cfg$paises) {
  datos <- panel[panel$pais == p, , drop = FALSE]
  for (v in variables) {
    x <- datos[[v]]
    x <- x[!is.na(x)]
    filas[[length(filas) + 1]] <- data.frame(
      pais        = p,
      variable    = v,
      categoria   = categoria_de(v),
      n           = length(x),
      anios_sin_dato = length(cfg$anio_min:cfg$anio_max) - length(x),
      media       = if (length(x) > 0) mean(x) else NA_real_,
      desvio      = if (length(x) > 1) stats::sd(x) else NA_real_,
      minimo      = if (length(x) > 0) min(x) else NA_real_,
      maximo      = if (length(x) > 0) max(x) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
}
descriptivos <- do.call(rbind, filas)

guardar_tabla(
  descriptivos, "tabla_descriptivos",
  "Estadísticas descriptivas por país y variable, 2005-2025",
  nota = paste0("n es la cantidad de años con dato dentro del período. Las ",
                "variables con n = 0 no existen en data/ y están explicadas ",
                "en 02_curados/diccionario.csv.")
)

# ---------------------------------------------------------------------------
# 2. Figura 1: precio por hectárea de las tres regiones
# ---------------------------------------------------------------------------
# El trabajo pide la figura en dólares constantes de 2025. Mientras no esté el
# CPI-U, se dibuja en dólares corrientes y el título lo dice, para que nadie
# lea la figura como si estuviera deflactada.
hay_precio_real <- any(!is.na(panel$precio_ha_usd_2025))

if (hay_precio_real) {
  columna_precio <- "precio_ha_usd_2025"
  titulo <- paste0("Figura 1. Precio de la hectárea agrícola en USD constantes de ",
                   cfg$anio_base_precios, ", ", cfg$anio_min, "-", cfg$anio_max)
  etiqueta_y <- paste0("USD de ", cfg$anio_base_precios, " por hectárea")
  archivo <- "figura_1_precio_por_hectarea.png"
} else {
  columna_precio <- "precio_ha_usd_corriente"
  titulo <- paste0("Figura 1. Precio de la hectárea agrícola en USD CORRIENTES, ",
                   cfg$anio_min, "-", cfg$anio_max)
  etiqueta_y <- "USD corrientes por hectárea"
  archivo <- "figura_1_precio_por_hectarea_usd_corrientes.png"
}

etiquetas <- c("Argentina" = "Argentina (zona núcleo)",
               "Brasil" = "Brasil (Mato Grosso)",
               "Estados Unidos" = "EE. UU. (Iowa)")
tipos_linea <- c("Argentina" = 1, "Brasil" = 2, "Estados Unidos" = 3)
simbolos    <- c("Argentina" = 16, "Brasil" = 17, "Estados Unidos" = 15)

grDevices::png(file.path(cfg$dir_resultados, archivo),
               width = 2000, height = 1300, res = 240)
graphics::par(mar = c(5.5, 6.5, 4, 1), family = "serif")

rango_y <- range(panel[[columna_precio]], na.rm = TRUE)
# El eje y se rotula aparte: con miles separados por punto, las etiquetas son
# anchas y el título del eje por defecto les queda encima.
graphics::plot(NA, xlim = c(cfg$anio_min, cfg$anio_max), ylim = rango_y,
               xlab = "", ylab = "", main = titulo,
               cex.main = 0.95, las = 1, bty = "n", xaxt = "n", yaxt = "n")
graphics::mtext(etiqueta_y, side = 2, line = 4.6)
graphics::axis(1, at = seq(cfg$anio_min, cfg$anio_max, by = 2))
graphics::mtext("Año", side = 1, line = 2.4)
graphics::axis(2, at = pretty(rango_y),
               labels = format(pretty(rango_y), big.mark = ".",
                               decimal.mark = ",", scientific = FALSE),
               las = 1)
graphics::grid(nx = NA, ny = NULL, col = "grey80", lty = "dotted")

for (p in cfg$paises) {
  datos <- panel[panel$pais == p, ]
  datos <- datos[order(datos$anio), ]
  graphics::lines(datos$anio, datos[[columna_precio]],
                  lty = tipos_linea[[p]], lwd = 2, col = "black")
  graphics::points(datos$anio, datos[[columna_precio]],
                   pch = simbolos[[p]], cex = 0.7, col = "black")
}

graphics::legend("topleft", legend = etiquetas[cfg$paises],
                 lty = tipos_linea[cfg$paises], pch = simbolos[cfg$paises],
                 lwd = 2, bty = "n", col = "black", cex = 0.85)

pie_figura <- if (hay_precio_real) {
  "Fuentes: ver 02_curados/diccionario.csv. Deflactado por el CPI-U de EE. UU."
} else {
  paste0("Fuentes: ver 02_curados/diccionario.csv. Serie SIN deflactar: ",
         "falta el CPI-U de EE. UU.")
}
graphics::mtext(pie_figura, side = 1, line = 4.3, cex = 0.65, adj = 0)
grDevices::dev.off()

mensaje("Tabla de descriptivos escrita (", nrow(descriptivos), " filas).")
mensaje("Figura 1 escrita: ", file.path(cfg$dir_resultados, archivo))
if (!hay_precio_real) {
  mensaje("ATENCIÓN: la Figura 1 está en dólares corrientes porque falta el ",
          "CPI-U. La figura en dólares de ", cfg$anio_base_precios,
          " NO se produjo.")
}
