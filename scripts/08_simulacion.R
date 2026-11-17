# ---------------------------------------------------------------------------
# 08_simulacion.R
# Simulación de precios: qué precio por hectárea habría dado cada ecuación en
# cada año, usando los regresores observados de ese año, y cuánto se aparta
# del precio que de verdad se pagó.
#
# Se calculan DOS simulaciones, y la diferencia entre las dos es el punto del
# ejercicio:
#
#   1. CON EL AÑO ADENTRO. Se evalúa la ecuación de los 21 años en cada año
#      del panel. Muestra la forma del ajuste, pero NO mide capacidad de
#      predecir: la ecuación ya vio ese año cuando se estimaron los
#      coeficientes, así que rinde examen con el examen que estudió.
#
#   2. CON EL AÑO AFUERA. Para cada año se vuelve a estimar la ecuación SIN
#      ese año y recién entonces se predice. El año predicho no participó de
#      su propia predicción, así que el error es el que la ecuación cometería
#      frente a un dato nuevo. Es el número honesto y siempre es peor.
#
# Un error del X % no se puede juzgar solo, así que se compara contra dos
# referencias que no necesitan ningún modelo: predecir el promedio del país
# (calculado también sin el año que se predice, para que la comparación sea
# pareja) y predecir el precio del año anterior. La segunda usa información
# que la ecuación no tiene: el precio pasado no es un regresor.
#
# Al pasar de logaritmos a dólares se aplica la corrección de Duan, que es el
# promedio de exp(residuos): exp(media del logaritmo) subestima la media del
# nivel. En la versión que deja el año afuera, el factor se recalcula con los
# residuos de esa estimación y no con los de la completa.
#
# Esto NO es el contrafáctico de 06_convergencia.R: acá no se cambia ningún
# regresor. Se usan los valores observados de cada año y lo que se mide es
# cuánto del precio la ecuación consigue reproducir.
#
# Salidas: 03_resultados/tabla_simulacion.csv
#          03_resultados/tabla_simulacion_resumen.csv
#          03_resultados/figura_2_simulacion.png
# ---------------------------------------------------------------------------

if (!exists("cfg")) source("scripts/00_config.R")
source("scripts/utilidades.R")

mensaje("== 08_simulacion: precio simulado año por año ==")

ruta_modelos <- file.path(cfg$dir_resultados, "modelos.rds")

if (!file.exists(ruta_modelos)) {
  problemas <- nuevo_registro_faltantes()
  problemas <- agregar_faltante(
    problemas, "08_simulacion", "precio_simulado",
    paste0("No hay ecuaciones estimadas (04_estimacion.R no produjo ",
           "modelos.rds). Sin coeficientes no se puede simular ningún ",
           "precio. Ver no_estimado.csv.")
  )
  escribir_no_estimado(problemas, "08_simulacion")
  guardar_tabla(problemas, "tabla_simulacion",
                "Precio simulado: NO CALCULADO")
  mensaje("NO SE SIMULÓ NINGÚN PRECIO: faltan las ecuaciones.")
} else {

  ajustados <- readRDS(ruta_modelos)
  panel <- ajustados$panel
  OBSERVADO <- "precio_ha_usd_2025"

  detalle <- list()
  resumen <- list()

  for (pais in cfg$paises) {
    regs <- cfg$regresores[[pais]]
    datos <- panel[panel$pais == pais,
                   c("anio", cfg$dependiente, OBSERVADO, regs)]
    datos <- datos[order(datos$anio), ]
    datos <- datos[stats::complete.cases(datos), ]

    modelo <- ajustados$modelos_niveles[[pais]]
    # Si la muestra no coincide con la que usó 04_estimacion.R, los años
    # quedarían desalineados y la tabla compararía peras con manzanas. Mejor
    # frenar que publicar un error de alineación.
    if (nrow(datos) != stats::nobs(modelo)) {
      stop("La muestra de ", pais, " no coincide con la del modelo estimado: ",
           nrow(datos), " filas contra ", stats::nobs(modelo), ".",
           call. = FALSE)
    }

    observado <- datos[[OBSERVADO]]
    n <- nrow(datos)

    # Simulación con el año adentro.
    adentro <- exp(unname(stats::predict(modelo, newdata = datos))) *
      mean(exp(stats::residuals(modelo)))

    # Simulación con el año afuera: una estimación nueva por cada año.
    formula <- stats::reformulate(regs, cfg$dependiente)
    afuera <- vapply(seq_len(n), function(i) {
      m <- stats::lm(formula, data = datos[-i, ])
      prediccion <- unname(stats::predict(m, newdata = datos[i, , drop = FALSE]))
      exp(prediccion) * mean(exp(stats::residuals(m)))
    }, numeric(1))

    # Referencias sin modelo. El promedio también se calcula sin el año que
    # se predice; si no, la referencia jugaría con una ventaja que la
    # ecuación no tiene.
    promedio_sin_el_anio <- (sum(observado) - observado) / (n - 1)
    # Repetir el precio del año anterior: el primer año no tiene anterior.
    anio_anterior <- c(NA_real_, utils::head(observado, -1))

    error_pct <- function(prediccion) (prediccion / observado - 1) * 100
    error_medio <- function(prediccion) {
      mean(abs(error_pct(prediccion)), na.rm = TRUE)
    }

    detalle[[pais]] <- data.frame(
      pais = pais, anio = datos$anio,
      precio_observado = observado,
      precio_simulado_adentro = adentro,
      error_adentro_usd = adentro - observado,
      error_adentro_pct = error_pct(adentro),
      precio_simulado_afuera = afuera,
      error_afuera_usd = afuera - observado,
      error_afuera_pct = error_pct(afuera),
      stringsAsFactors = FALSE
    )

    peor_adentro <- which.max(abs(error_pct(adentro)))
    peor <- which.max(abs(error_pct(afuera)))
    e_afuera    <- error_medio(afuera)
    e_promedio  <- error_medio(promedio_sin_el_anio)
    e_anterior  <- error_medio(anio_anterior)

    # El error en la ventana donde se responden las hipótesis se informa
    # aparte: si ahí la ecuación anda peor que en el resto del período, la
    # descomposición se está apoyando justo en los años que peor reproduce.
    en_ventana <- datos$anio >= cfg$ventana_medias[1] &
      datos$anio <= cfg$ventana_medias[2]

    resumen[[pais]] <- data.frame(
      pais = pais, n = n,
      error_medio_adentro_pct = error_medio(adentro),
      error_medio_afuera_pct = e_afuera,
      ventana = sprintf("%d-%d", cfg$ventana_medias[1], cfg$ventana_medias[2]),
      error_ventana_adentro_pct = mean(abs(error_pct(adentro)[en_ventana])),
      error_ventana_afuera_pct = mean(abs(error_pct(afuera)[en_ventana])),
      peor_anio_adentro = datos$anio[peor_adentro],
      peor_error_adentro_pct = error_pct(adentro)[peor_adentro],
      error_medio_promedio_pct = e_promedio,
      error_medio_anio_anterior_pct = e_anterior,
      peor_anio = datos$anio[peor],
      peor_error_pct = error_pct(afuera)[peor],
      veredicto = if (e_afuera < min(e_promedio, e_anterior)) {
        "Con un año nuevo la ecuación le acierta mejor que las dos referencias sin modelo."
      } else if (e_afuera < e_promedio) {
        paste0("Con un año nuevo la ecuación le gana a predecir el promedio, ",
               "pero no a repetir el precio del año anterior.")
      } else {
        paste0("Con un año nuevo la ecuación no le gana a predecir el ",
               "promedio del período: lo que explica adentro de la muestra ",
               "no se sostiene afuera.")
      },
      stringsAsFactors = FALSE
    )
  }

  detalle <- do.call(rbind, detalle)
  resumen <- do.call(rbind, resumen)

  guardar_tabla(
    detalle, "tabla_simulacion",
    "Precio simulado año por año contra el precio observado",
    nota = paste0("Los precios están en dólares de ", cfg$anio_base_precios,
                  " por hectárea. 'Adentro' evalúa la ecuación de los 21 ",
                  "años en un año que esa misma ecuación usó para estimarse, ",
                  "así que subestima el error. 'Afuera' vuelve a estimar la ",
                  "ecuación sin ese año y recién después lo predice: es la ",
                  "columna que mide capacidad de predecir. El paso de ",
                  "logaritmos a dólares usa la corrección de Duan.")
  )

  guardar_tabla(
    resumen, "tabla_simulacion_resumen",
    "Error medio de la simulación y referencias sin modelo",
    nota = paste0("El error medio es el promedio del error absoluto en ",
                  "porcentaje. Las dos referencias no usan ninguna ",
                  "ecuación: predecir el promedio del país (calculado sin el ",
                  "año que se predice) y repetir el precio del año anterior. ",
                  "La segunda usa el precio pasado, que no es un regresor de ",
                  "ninguna ecuación, así que juega con información que el ",
                  "modelo no tiene.")
  )

  # -------------------------------------------------------------------------
  # Figura: observado contra simulado, un panel por país.
  # Los tres paneles comparten el eje de años (2005-2025) y la misma escala
  # en dólares, para que un país no se vea más alto solo porque su eje
  # arranca más arriba. El precio pagado se dibuja todos los años en los que
  # existe, aunque la ecuación no se haya podido evaluar: Iowa 2005 y 2006
  # no tienen ventas de maquinaria, y Brasil 2020 no tiene diésel. Esos años
  # no se rellenan: la línea simulada se corta y la observada sigue.
  # -------------------------------------------------------------------------
  etiquetas <- c("Argentina" = "Argentina (zona núcleo)",
                 "Brasil" = "Brasil (Mato Grosso)",
                 "Estados Unidos" = "EE. UU. (Iowa)")
  anios_fig <- cfg$anio_min:cfg$anio_max
  precios_obs <- panel[[OBSERVADO]]
  cortes_y <- pretty(range(c(precios_obs, detalle$precio_simulado_adentro),
                           na.rm = TRUE))
  ticks_x <- seq(cfg$anio_min, cfg$anio_max, by = 5)

  archivo_fig <- "figura_2_simulacion.png"
  grDevices::png(file.path(cfg$dir_resultados, archivo_fig),
                 width = 2400, height = 950, res = 240)
  graphics::par(mfrow = c(1, length(cfg$paises)),
                mar = c(3.4, 5.2, 2.6, 0.9), oma = c(5.2, 0, 2.2, 0),
                family = "serif")

  for (pais in cfg$paises) {
    d <- detalle[detalle$pais == pais, ]
    obs <- panel[panel$pais == pais, c("anio", OBSERVADO)]
    serie_obs <- obs[[OBSERVADO]][match(anios_fig, obs$anio)]
    serie_sim <- d$precio_simulado_adentro[match(anios_fig, d$anio)]
    graphics::plot(NA, xlim = c(cfg$anio_min, cfg$anio_max),
                   ylim = range(cortes_y),
                   xlab = "", ylab = "", main = etiquetas[[pais]],
                   cex.main = 1, las = 1, bty = "n", xaxt = "n", yaxt = "n")
    graphics::axis(1, at = ticks_x)
    graphics::axis(2, at = cortes_y,
                   labels = format(cortes_y, big.mark = ".",
                                   decimal.mark = ",", scientific = FALSE),
                   las = 1)
    graphics::mtext(paste0("USD de ", cfg$anio_base_precios, " por hectárea"),
                    side = 2, line = 3.9, cex = 0.62)
    graphics::grid(nx = NA, ny = NULL, col = "grey80", lty = "dotted")
    graphics::lines(anios_fig, serie_obs, lwd = 2.4, col = "black")
    graphics::lines(anios_fig, serie_sim, lwd = 1.6, lty = 2, col = "grey35")
  }

  graphics::mtext(paste0("Figura 2. Precio observado y precio simulado por la ",
                         "ecuación de cada país, ", cfg$anio_min, "-",
                         cfg$anio_max),
                  side = 3, outer = TRUE, line = 0.4, cex = 0.78)
  graphics::mtext("Año", side = 1, outer = TRUE, line = 0.6, cex = 0.7)

  # La leyenda va debajo de los tres paneles: adentro de cualquiera de ellos
  # se le monta encima a las líneas. Se dibuja sobre un lienzo vacío que
  # ocupa toda la figura.
  graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
                new = TRUE)
  graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",
                 xlab = "", ylab = "")
  graphics::legend("bottom", horiz = TRUE, bty = "n", cex = 0.78,
                   lwd = c(2.4, 1.6), lty = c(1, 2),
                   col = c("black", "grey35"),
                   legend = c("precio observado", "precio simulado"))
  grDevices::dev.off()

  mensaje("Simulación calculada para: ", paste(cfg$paises, collapse = ", "))
  for (i in seq_len(nrow(resumen))) {
    mensaje("  ", resumen$pais[i],
            ": error medio con el año adentro = ",
            round(resumen$error_medio_adentro_pct[i], 1), " %",
            ", con el año afuera = ",
            round(resumen$error_medio_afuera_pct[i], 1), " %")
  }
  mensaje("Figura 2 escrita: ", file.path(cfg$dir_resultados, archivo_fig))
}
