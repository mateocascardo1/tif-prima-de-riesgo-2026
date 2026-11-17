# ---------------------------------------------------------------------------
# 10_riesgo_brasil.R
# Precio de la hectárea argentina, año por año, si el riesgo país hubiera
# sido el de Brasil.
#
# Tres series, desde el primer año del panel hasta el último. No se agrega
# ningún año posterior: esto no es un pronóstico.
#
#   1. El precio que de verdad se pagó.
#   2. El precio que devuelve la ecuación argentina alimentada con los datos
#      argentinos de ese año. Es la misma simulación "con el año adentro"
#      de 08_simulacion.R.
#   3. El mismo cálculo, reemplazando únicamente el riesgo país argentino de
#      ese año por el de Brasil. Rendimiento, soja, tipo de cambio real,
#      tasas, ventas de maquinaria por hectárea, diésel, lluvia, la
#      alícuota de retenciones y la brecha cambiaria quedan en su valor
#      argentino. Brasil no tiene alícuota ni brecha: en esta serie no se
#      las pone en cero. El crédito no entra.
#   4. Una serie aparte, en la figura 4: lo mismo que la 3, y además la
#      alícuota argentina en cero y la brecha cambiaria en cero. Esos ceros
#      no son series brasileñas medidas. Son el contrafáctico de que
#      Argentina cobra lo que Brasil cobra por exportar soja, que es nada,
#      y de que no hay diferencia entre el dólar paralelo y el mayorista.
#      La ecuación no se reestima.
#
# Los coeficientes salen de la ecuación argentina ya estimada, en niveles.
# No se reestima. Al pasar de logaritmos a dólares se aplica la corrección
# de Duan de la ecuación completa, y ese factor multiplica por igual a las
# dos simulaciones. El porcentaje entre ellas es, por eso, solo el cambio
# que produce el coeficiente del riesgo país.
#
# Si Brasil no tiene riesgo país en un año, ese año queda vacío. Si el
# valor brasileño cae fuera del rango que Argentina tuvo en todo el período,
# el año se marca y no se borra.
#
# Salidas: 03_resultados/tabla_riesgo_brasil.csv
#          03_resultados/figura_3_riesgo_brasil.png
#          03_resultados/figura_4_institucional_brasil.png
# ---------------------------------------------------------------------------

if (!exists("cfg")) source("scripts/00_config.R")
source("scripts/utilidades.R")

mensaje("== 10_riesgo_brasil: precio argentino con el riesgo país de Brasil ==")

archivo_fig <- "figura_3_riesgo_brasil.png"
ruta_fig <- file.path(cfg$dir_resultados, archivo_fig)
OBSERVADO <- "precio_ha_usd_2025"
pais <- cfg$pais_precio
origen <- cfg$pais_riesgo_reemplazo
var_riesgo <- cfg$variable_riesgo_pais

# Sin coeficientes, o sin riesgo país en la ecuación, no hay resultado.
# Se deja el mismo aviso que 06 y 08, y no un gráfico vacío.
avisar <- function(motivo) {
  problemas <- nuevo_registro_faltantes()
  problemas <- agregar_faltante(
    problemas, "10_riesgo_brasil", "precio_riesgo_brasil", motivo
  )
  escribir_no_estimado(problemas, "10_riesgo_brasil")
  guardar_tabla(problemas, "tabla_riesgo_brasil",
                "Precio con riesgo de Brasil: NO CALCULADO")
  if (file.exists(ruta_fig)) file.remove(ruta_fig)
  mensaje("NO SE CALCULÓ EL PRECIO CON RIESGO DE BRASIL.")
  mensaje(motivo)
}

ruta_modelos <- file.path(cfg$dir_resultados, "modelos.rds")

if (!file.exists(ruta_modelos)) {
  avisar(paste0(
    "No hay ecuaciones estimadas (04_estimacion.R no produjo modelos.rds). ",
    "Sin coeficientes no se puede simular el precio con el riesgo país de ",
    origen, ". Ver no_estimado.csv."
  ))
} else {

  ajustados <- readRDS(ruta_modelos)
  panel <- ajustados$panel
  modelo <- ajustados$modelos_niveles[[pais]]
  regs <- cfg$regresores[[pais]]
  tiene_riesgo <- !is.null(modelo) && var_riesgo %in% names(stats::coef(modelo))

  if (!tiene_riesgo) {
    avisar(paste0(
      "La ecuación en niveles de ", pais, " no incluye el riesgo país (",
      var_riesgo, "). Sin ese coeficiente no hay nada que reemplazar, y no ",
      "se publica un gráfico vacío. Ver no_estimado.csv."
    ))
  } else {

    datos <- panel[panel$pais == pais,
                   c("anio", cfg$dependiente, OBSERVADO, regs)]
    datos <- datos[order(datos$anio), ]
    datos <- datos[stats::complete.cases(datos), ]
    datos <- datos[datos$anio >= cfg$anio_min & datos$anio <= cfg$anio_max, ]

    if (nrow(datos) != stats::nobs(modelo)) {
      stop("La muestra de ", pais, " no coincide con la del modelo estimado: ",
           nrow(datos), " filas contra ", stats::nobs(modelo), ".",
           call. = FALSE)
    }
    if (any(datos$anio > cfg$anio_max)) {
      stop("Hay años posteriores a ", cfg$anio_max, ".", call. = FALSE)
    }

    # Misma cuenta que 08_simulacion.R: exp(predicción) por el factor de
    # Duan de la ecuación completa.
    factor_duan <- mean(exp(stats::residuals(modelo)))
    simulado_ar <- exp(unname(stats::predict(modelo, newdata = datos))) *
      factor_duan

    # Riesgo país del otro país, alineado por año. Un año sin dato queda NA.
    # No se interpola ni se completa con cero.
    br <- panel[panel$pais == origen &
                  panel$anio >= cfg$anio_min & panel$anio <= cfg$anio_max, ]
    riesgo_br <- if (var_riesgo %in% names(br)) {
      br[[var_riesgo]][match(datos$anio, br$anio)]
    } else {
      rep(NA_real_, nrow(datos))
    }

    datos_cf <- datos
    datos_cf[[var_riesgo]] <- riesgo_br

    # Lo único que puede cambiar es el riesgo país. La alícuota, en
    # particular, tiene que seguir siendo la argentina.
    for (k in setdiff(names(datos), var_riesgo)) {
      if (!identical(datos_cf[[k]], datos[[k]])) {
        stop("Se modificó ", k, " y solo debía cambiar el riesgo país.",
             call. = FALSE)
      }
    }
    if ("alicuota_export_soja_pct" %in% names(datos_cf) &&
        !identical(datos_cf$alicuota_export_soja_pct,
                   datos$alicuota_export_soja_pct)) {
      stop("La alícuota argentina fue modificada.", call. = FALSE)
    }

    # na.pass: un año sin riesgo brasileño devuelve NA y no se cae de la
    # fila. El default de predict.lm ya es na.pass; se deja escrito.
    pred_cf <- unname(stats::predict(modelo, newdata = datos_cf,
                                     na.action = stats::na.pass))
    if (length(pred_cf) != nrow(datos)) {
      stop("La predicción con riesgo de ", origen,
           " no tiene una fila por año.", call. = FALSE)
    }
    simulado_br <- exp(pred_cf) * factor_duan

    # --- Controles, antes de publicar ------------------------------------
    ruta_sim <- file.path(cfg$dir_resultados, "tabla_simulacion.csv")
    if (!file.exists(ruta_sim)) {
      stop("No está tabla_simulacion.csv: no se puede controlar la ",
           "alineación con 08_simulacion.R.", call. = FALSE)
    }
    sim <- utils::read.csv(ruta_sim, stringsAsFactors = FALSE)
    if (!"precio_simulado_adentro" %in% names(sim)) {
      stop("tabla_simulacion.csv no tiene la simulación con el año adentro.",
           call. = FALSE)
    }
    sim_ar <- sim[sim$pais == pais, ]
    sim_ar <- sim_ar[order(sim_ar$anio), ]
    if (!isTRUE(all.equal(as.numeric(sim_ar$anio), as.numeric(datos$anio)))) {
      stop("Los años no coinciden con la simulación ya publicada.",
           call. = FALSE)
    }
    desvio_sim <- abs(simulado_ar - sim_ar$precio_simulado_adentro)
    if (any(desvio_sim > 1e-6)) {
      i <- which.max(desvio_sim)
      stop("El precio simulado con datos argentinos no coincide con la ",
           "simulación con el año adentro en ", datos$anio[i], ": ",
           simulado_ar[i], " contra ", sim_ar$precio_simulado_adentro[i],
           ". Hay un error de alineación.", call. = FALSE)
    }

    beta <- unname(stats::coef(modelo)[[var_riesgo]])
    esperado <- exp(beta * (riesgo_br - datos[[var_riesgo]]))
    cociente <- simulado_br / simulado_ar
    con_dato <- !is.na(cociente) & !is.na(esperado)
    desvio_cociente <- abs(cociente[con_dato] - esperado[con_dato])
    if (any(desvio_cociente > 1e-8)) {
      stop("El cociente entre el precio con riesgo de ", origen,
           " y el simulado argentino no es el cambio que produce solo el ",
           "coeficiente del riesgo país.", call. = FALSE)
    }

    # Rango que Argentina tuvo en todo el período. Un valor brasileño
    # fuera de ese rango se marca y se conserva.
    ar_periodo <- panel[panel$pais == pais &
                          panel$anio >= cfg$anio_min &
                          panel$anio <= cfg$anio_max, ]
    rango <- range(ar_periodo[[var_riesgo]], na.rm = TRUE)
    if (!all(is.finite(rango))) {
      stop(pais, " no tiene riesgo país en el período: no hay rango contra ",
           "el cual marcar extrapolación.", call. = FALSE)
    }
    fuera <- !is.na(riesgo_br) & (riesgo_br < rango[1] | riesgo_br > rango[2])
    marca <- ifelse(is.na(riesgo_br), "", ifelse(fuera, "sí", "no"))

    # Bloque institucional entero: el riesgo de Brasil, las retenciones
    # afuera y la brecha cambiaria en cero. El coeficiente de la alícuota
    # no alcanza para valuar la quita del impuesto: en la muestra solo se
    # movió unos puntos alrededor del 30 %. Sacar el impuesto se valúa
    # además con el coeficiente de la soja, que sí está medido: el productor
    # pasa a quedarse con el precio entero, y la hectárea reacciona como
    # ya reacciona a la soja. La brecha sí se valúa con su propio
    # coeficiente, llevada a cero. La ecuación no se reestima.
    var_fiscal <- "alicuota_export_soja_pct"
    var_soja <- "ln_precio_soja_cbot"
    var_brecha <- "brecha_cambiaria_pct"
    tiene_brecha <- var_brecha %in% names(stats::coef(modelo))
    inst_en_eq <- intersect(
      names(cfg$categorias)[cfg$categorias == "institucional"], regs
    )
    inst_resto <- setdiff(inst_en_eq, var_riesgo)
    esperadas <- c(var_fiscal, if (tiene_brecha) var_brecha)
    if (!setequal(inst_resto, esperadas)) {
      stop("La ecuación tiene variables institucionales que este escenario ",
           "no reemplaza: ", paste(inst_resto, collapse = ", "), ".",
           call. = FALSE)
    }
    if (!all(c(var_fiscal, var_soja) %in% names(stats::coef(modelo)))) {
      stop("La ecuación no tiene coeficiente de alícuota o de soja.",
           call. = FALSE)
    }
    tasa <- datos[[var_fiscal]] / 100
    if (any(!is.na(tasa) & tasa >= 1)) {
      stop("Hay una alícuota de 100 % o más: no queda precio para el productor.",
           call. = FALSE)
    }
    datos_inst <- datos_cf
    datos_inst[[var_fiscal]] <- 0
    # ln(precio / (1 - alícuota)) - ln(precio) = -ln(1 - alícuota).
    datos_inst[[var_soja]] <- datos[[var_soja]] - log(1 - tasa)
    if (tiene_brecha) datos_inst[[var_brecha]] <- 0
    pred_inst <- unname(stats::predict(modelo, newdata = datos_inst,
                                       na.action = stats::na.pass))
    if (length(pred_inst) != nrow(datos)) {
      stop("La predicción institucional no tiene una fila por año.",
           call. = FALSE)
    }
    simulado_inst <- exp(pred_inst) * factor_duan
    beta_fiscal <- unname(stats::coef(modelo)[[var_fiscal]])
    beta_soja <- unname(stats::coef(modelo)[[var_soja]])
    delta_brecha <- if (tiene_brecha) {
      unname(stats::coef(modelo)[[var_brecha]]) *
        (0 - datos[[var_brecha]])
    } else {
      0
    }
    esperado_inst <- exp(
      beta * (riesgo_br - datos[[var_riesgo]]) +
        beta_fiscal * (0 - datos[[var_fiscal]]) +
        beta_soja * (datos_inst[[var_soja]] - datos[[var_soja]]) +
        delta_brecha
    )
    cociente_inst <- simulado_inst / simulado_ar
    con_inst <- !is.na(cociente_inst) & !is.na(esperado_inst)
    if (any(abs(cociente_inst[con_inst] - esperado_inst[con_inst]) > 1e-8)) {
      stop("El precio con el bloque institucional no coincide con el ",
           "cambio de los coeficientes institucionales y de la soja.",
           call. = FALSE)
    }
    rango_fiscal <- range(ar_periodo[[var_fiscal]], na.rm = TRUE)
    if (!all(is.finite(rango_fiscal))) {
      stop(pais, " no tiene alícuota en el período.", call. = FALSE)
    }
    cero_fuera <- 0 < rango_fiscal[1] || 0 > rango_fiscal[2]

    diferencia_usd <- simulado_br - simulado_ar
    diferencia_pct <- (simulado_br / simulado_ar - 1) * 100
    diferencia_inst_usd <- simulado_inst - simulado_ar
    diferencia_inst_pct <- (simulado_inst / simulado_ar - 1) * 100

    tabla <- data.frame(
      pais = pais,
      anio = datos$anio,
      precio_observado = datos[[OBSERVADO]],
      precio_simulado_argentino = simulado_ar,
      precio_simulado_riesgo_brasil = simulado_br,
      diferencia_usd = diferencia_usd,
      diferencia_pct = diferencia_pct,
      riesgo_pais_argentina = datos[[var_riesgo]],
      riesgo_pais_brasil = riesgo_br,
      extrapolacion = marca,
      precio_simulado_institucional_brasil = simulado_inst,
      diferencia_institucional_usd = diferencia_inst_usd,
      diferencia_institucional_pct = diferencia_inst_pct,
      alicuota_contrafactual_pct = 0,
      extrapolacion_retenciones = ifelse(cero_fuera, "sí", "no"),
      alicuota_argentina_pct = datos[[var_fiscal]],
      brecha_argentina_pct = if (tiene_brecha) datos[[var_brecha]] else NA_real_,
      brecha_contrafactual_pct = if (tiene_brecha) 0 else NA_real_,
      stringsAsFactors = FALSE
    )

    n_vacio <- sum(is.na(riesgo_br))
    n_fuera <- sum(marca == "sí")

    guardar_tabla(
      tabla, "tabla_riesgo_brasil",
      paste0("Precio argentino simulado reemplazando solo el riesgo país ",
             "por el de ", origen),
      nota = paste0(
        "Precios en dólares de ", cfg$anio_base_precios, " por hectárea. ",
        "El precio simulado argentino es la ecuación de ", pais, " evaluada ",
        "con los datos argentinos de ese año: la misma simulación con el ",
        "año adentro de tabla_simulacion.csv. El precio con riesgo de ",
        origen, " cambia solo ", var_riesgo, " por el valor de ", origen,
        " de ese año y deja el resto en el valor argentino, incluida la ",
        "alícuota de retenciones. Al pasar de logaritmos a dólares se ",
        "multiplica por el factor de Duan de la ecuación completa (",
        round(factor_duan, 4), "), el mismo para las dos simulaciones, así ",
        "que el porcentaje entre ellas es solo el efecto del coeficiente ",
        "del riesgo país. Si ", origen, " no tiene riesgo país en un año, ",
        "el precio queda vacío: no se interpola ni se pone cero. 'sí' en ",
        "extrapolación quiere decir que ese riesgo cae fuera del rango que ",
        pais, " tuvo entre ", cfg$anio_min, " y ", cfg$anio_max,
        " (de ", round(rango[1], 1), " a ", round(rango[2], 1),
        " puntos básicos); el año se muestra igual. La serie termina en ",
        cfg$anio_max, " y no proyecta los años siguientes. ",
        "precio_simulado_institucional_brasil repite ese reemplazo y además ",
        "saca las retenciones y pone la brecha cambiaria en cero. Sacar el ",
        "impuesto se valúa como una suba del precio de la soja que le queda ",
        "al productor, en la proporción de la alícuota, con el coeficiente ",
        "de la soja ya estimado; la alícuota también pasa a cero, así que su ",
        "coeficiente entra al número. La brecha se valúa con su propio ",
        "coeficiente. La ecuación no se reestima. Esos ceros no son series ",
        "de ", origen, ". La alícuota observada se movió entre ",
        round(rango_fiscal[1], 1), " % y ", round(rango_fiscal[2], 1), " %. ",
        if (cero_fuera) {
          paste0("Cero está fuera de las alícuotas que ", pais, " tuvo (",
                 round(rango_fiscal[1], 1), " a ", round(rango_fiscal[2], 1),
                 " %), así que esa serie entera es una extrapolación.")
        } else {
          "Cero cae dentro de las alícuotas que el país tuvo."
        }
      )
    )

    # -----------------------------------------------------------------------
    # Figura: un solo panel. Misma tipografía, grilla y ejes que la figura 2.
    # La leyenda va debajo para no tapar las líneas.
    # -----------------------------------------------------------------------
    etiquetas <- c("Argentina" = "Argentina (zona núcleo)",
                   "Brasil" = "Brasil (Mato Grosso)",
                   "Estados Unidos" = "EE. UU. (Iowa)")

    grDevices::png(ruta_fig, width = 2000, height = 1120, res = 240)
    graphics::par(mfrow = c(1, 1),
                  mar = c(3.4, 5.2, 2.6, 0.9), oma = c(5.2, 0, 2.4, 0),
                  family = "serif")

    # La figura 3 y la 4 comparten la escala, para que se puedan mirar juntas.
    cortes_y <- pretty(range(c(tabla$precio_observado,
                               tabla$precio_simulado_argentino,
                               tabla$precio_simulado_riesgo_brasil,
                               tabla$precio_simulado_institucional_brasil),
                             na.rm = TRUE))
    rango_y <- range(cortes_y)
    graphics::plot(NA, xlim = c(cfg$anio_min, cfg$anio_max), ylim = rango_y,
                   xlab = "", ylab = "",
                   main = etiquetas[[pais]],
                   cex.main = 1, las = 1, bty = "n", xaxt = "n", yaxt = "n")
    graphics::axis(1, at = seq(cfg$anio_min, cfg$anio_max, by = 5))
    graphics::axis(2, at = cortes_y,
                   labels = format(cortes_y, big.mark = ".",
                                   decimal.mark = ",", scientific = FALSE),
                   las = 1)
    graphics::mtext(paste0("dólares de ", cfg$anio_base_precios,
                           " por hectárea"),
                    side = 2, line = 3.9, cex = 0.62)
    graphics::grid(nx = NA, ny = NULL, col = "grey80", lty = "dotted")
    graphics::lines(tabla$anio, tabla$precio_observado, lwd = 2.4,
                    col = "black")
    graphics::lines(tabla$anio, tabla$precio_simulado_argentino, lwd = 1.6,
                    lty = 2, col = "grey35")
    graphics::lines(tabla$anio, tabla$precio_simulado_riesgo_brasil, lwd = 2,
                    col = "#b3261e")

    graphics::mtext(paste0(
      "Figura 3. Precio argentino simulado reemplazando solo el riesgo ",
      "país por el de ", origen, ", ", cfg$anio_min, "-", cfg$anio_max
    ), side = 3, outer = TRUE, line = 0.4, cex = 0.72)
    graphics::mtext("Año", side = 1, outer = TRUE, line = 0.6, cex = 0.7)

    graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
                  new = TRUE)
    graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",
                   xlab = "", ylab = "")
    graphics::legend(
      "bottom", horiz = TRUE, bty = "n", cex = 0.68,
      lwd = c(2.4, 1.6, 2), lty = c(1, 2, 1),
      col = c("black", "grey35", "#b3261e"),
      legend = c("precio observado",
                 "precio simulado con datos argentinos",
                 paste0("precio simulado con riesgo país de ", origen))
    )
    grDevices::dev.off()

    mensaje("Control contra la simulación con el año adentro: coinciden.")
    mensaje("Control del cociente: es solo el coeficiente del riesgo país.")
    mensaje("Años sin riesgo país de ", origen, ": ", n_vacio, ".")
    mensaje("Años marcados como extrapolación: ", n_fuera, ".")
    fila_fin <- tabla[tabla$anio == cfg$anio_max, ]
    if (nrow(fila_fin) == 1) {
      mensaje("  ", cfg$anio_max, ": simulado argentino = ",
              round(fila_fin$precio_simulado_argentino, 1),
              " dólares; con riesgo de ", origen, " = ",
              round(fila_fin$precio_simulado_riesgo_brasil, 1),
              " dólares.")
    }
    mensaje("Figura 3 escrita: ", ruta_fig)

    # Figura 4: el bloque institucional entero. La roja es la figura 3,
    # para ver cuánto agrega sacar las retenciones.
    ruta_fig4 <- file.path(cfg$dir_resultados, "figura_4_institucional_brasil.png")
    grDevices::png(ruta_fig4, width = 2000, height = 1180, res = 240)
    graphics::par(mfrow = c(1, 1),
                  mar = c(3.4, 5.2, 2.6, 0.9), oma = c(6.2, 0, 2.4, 0),
                  family = "serif")
    graphics::plot(NA, xlim = c(cfg$anio_min, cfg$anio_max), ylim = rango_y,
                   xlab = "", ylab = "",
                   main = etiquetas[[pais]],
                   cex.main = 1, las = 1, bty = "n", xaxt = "n", yaxt = "n")
    graphics::axis(1, at = seq(cfg$anio_min, cfg$anio_max, by = 5))
    graphics::axis(2, at = cortes_y,
                   labels = format(cortes_y, big.mark = ".",
                                   decimal.mark = ",", scientific = FALSE),
                   las = 1)
    graphics::mtext(paste0("dólares de ", cfg$anio_base_precios,
                           " por hectárea"),
                    side = 2, line = 3.9, cex = 0.62)
    graphics::grid(nx = NA, ny = NULL, col = "grey80", lty = "dotted")
    graphics::lines(tabla$anio, tabla$precio_observado, lwd = 2.4,
                    col = "black")
    graphics::lines(tabla$anio, tabla$precio_simulado_argentino, lwd = 1.6,
                    lty = 2, col = "grey35")
    graphics::lines(tabla$anio, tabla$precio_simulado_riesgo_brasil, lwd = 2,
                    col = "#b3261e")
    graphics::lines(tabla$anio, tabla$precio_simulado_institucional_brasil,
                    lwd = 2, col = "#0f7b6c")
    graphics::mtext(paste0(
      "Figura 4. Precio argentino si el bloque institucional fuera el de ",
      origen, ", ", cfg$anio_min, "-", cfg$anio_max
    ), side = 3, outer = TRUE, line = 0.4, cex = 0.72)
    graphics::mtext("Año", side = 1, outer = TRUE, line = 0.6, cex = 0.7)
    graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
                  new = TRUE)
    graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",
                   xlab = "", ylab = "")
    graphics::legend(
      "bottom", horiz = FALSE, bty = "n", cex = 0.68, ncol = 2,
      lwd = c(2.4, 1.6, 2, 2), lty = c(1, 2, 1, 1),
      col = c("black", "grey35", "#b3261e", "#0f7b6c"),
      legend = c("precio observado",
                 "simulado con datos argentinos",
                 paste0("solo el riesgo país de ", origen),
                 paste0("riesgo de ", origen, ", sin retenciones y sin brecha"))
    )
    grDevices::dev.off()

    if (nrow(fila_fin) == 1) {
      mensaje("  ", cfg$anio_max, ": con el bloque institucional de ", origen,
              " = ", round(fila_fin$precio_simulado_institucional_brasil, 1),
              " dólares.")
    }
    mensaje("Figura 4 escrita: ", ruta_fig4)
  }
}
