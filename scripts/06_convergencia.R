# ---------------------------------------------------------------------------
# 06_convergencia.R
# Precio potencial de la hectárea argentina bajo convergencia institucional
# hacia Brasil.
#
# El ejercicio: tomar la ecuación argentina, evaluarla en las medias de la
# ventana y reemplazar SOLO las variables institucionales por los niveles de
# Brasil, en convergencia parcial (25 % y 50 %) y total (100 %). Todo lo
# demás queda en su valor argentino observado.
#
# Las medias salen de cfg$ventana_medias y los coeficientes, siempre del
# período completo. Las dos ventanas conviven en las tablas bajo la columna
# 'ventana': la de rol 'principal' responde H2 y la de 'anexo' repite el
# cálculo sobre los veintiún años.
#
# Tres cuidados, que son los que hacen que el número signifique algo:
#   1. La comparación es contra el precio PREDICHO por la ecuación, no contra
#      el observado. Comparar un contrafáctico predicho contra un dato
#      observado mezcla el efecto del escenario con el error del modelo.
#   2. Al pasar de logaritmos a dólares se aplica la corrección de Duan
#      (factor de "smearing"): exp(media del logaritmo) subestima la media del
#      nivel. El factor es el promedio de exp(residuos) y, como multiplica por
#      igual al predicho y al contrafáctico, no altera la variación relativa.
#   3. Si el nivel brasileño cae fuera del rango que Argentina tuvo en el
#      período, el escenario es una extrapolación y se marca como tal.
#
# Salidas: 03_resultados/tabla_precio_potencial.csv
#          03_resultados/tabla_h2.csv
# ---------------------------------------------------------------------------

if (!exists("cfg")) source("scripts/00_config.R")
source("scripts/utilidades.R")

mensaje("== 06_convergencia: precio potencial ==")

ruta_modelos <- file.path(cfg$dir_resultados, "modelos.rds")

if (!file.exists(ruta_modelos)) {
  problemas <- nuevo_registro_faltantes()
  problemas <- agregar_faltante(
    problemas, "06_convergencia", "precio_potencial",
    paste0("No hay ecuación argentina estimada (04_estimacion.R no produjo ",
           "modelos.rds). Sin coeficientes no se puede predecir ningún ",
           "escenario. Ver no_estimado.csv.")
  )
  escribir_no_estimado(problemas, "06_convergencia")
  guardar_tabla(problemas, "tabla_precio_potencial",
                "Precio potencial: NO CALCULADO")
  mensaje("NO SE CALCULÓ EL PRECIO POTENCIAL: falta la ecuación argentina.")
} else {

  ajustados <- readRDS(ruta_modelos)
  panel <- ajustados$panel
  modelo_ar <- ajustados$modelos_niveles[["Argentina"]]
  beta <- stats::coef(modelo_ar)

  datos_ar <- panel[panel$pais == "Argentina", ]
  datos_br <- panel[panel$pais == "Brasil", ]

  # -------------------------------------------------------------------------
  # Qué variables se reemplazan
  # -------------------------------------------------------------------------
  # Solo las institucionales de la ecuación argentina, y solo si Brasil tiene
  # esa serie. Donde Brasil no la tiene NO se pone cero: un cero sería afirmar
  # que en Brasil esa variable vale cero, y lo único que sabemos es que no se
  # midió. Esas variables quedan en su valor argentino y se informan aparte.
  institucionales_ar <- cfg$regresores[["Argentina"]][
    categoria_de(cfg$regresores[["Argentina"]]) == "institucional"
  ]
  reemplazables <- institucionales_ar[
    vapply(institucionales_ar, function(v) {
      v %in% names(datos_br) && any(!is.na(datos_br[[v]]))
    }, logical(1))
  ]
  no_reemplazables <- setdiff(institucionales_ar, reemplazables)

  # Si ninguna variable institucional de la ecuación argentina existe también
  # en Brasil, no hay nada que converger: el escenario daría exactamente el
  # precio predicho actual. Eso no es un resultado, es la ausencia del
  # ejercicio, y se informa como tal en vez de publicar un aumento de cero.
  escenario_informativo <- length(reemplazables) > 0

  # -------------------------------------------------------------------------
  # Medias y escenarios
  # -------------------------------------------------------------------------
  # Las MEDIAS salen de cada ventana; los COEFICIENTES, siempre de la ecuación
  # estimada sobre el período completo (el porqué está en 00_config.R). El
  # rango con el que se detecta extrapolación también es el del período
  # completo, porque es el recorrido que la ecuación efectivamente vio.
  regresores_ar <- cfg$regresores[["Argentina"]]

  predecir <- function(x) {
    unname(beta[["(Intercept)"]] + sum(beta[regresores_ar] * x[regresores_ar]))
  }

  # Factor de smearing de Duan: promedio de exp(residuos) de la ecuación.
  factor_duan <- mean(exp(stats::residuals(modelo_ar)))

  rango_ventana <- cfg$ventana_medias
  ventanas <- list(
    list(clave = sprintf("%d-%d", rango_ventana[1], rango_ventana[2]),
         rol = "principal",
         datos = panel[panel$anio >= rango_ventana[1] &
                         panel$anio <= rango_ventana[2], ]),
    list(clave = sprintf("%d-%d", cfg$anio_min, cfg$anio_max),
         rol = "anexo", datos = panel)
  )

  escenarios_en <- function(v) {
    ar <- v$datos[v$datos$pais == "Argentina", ]
    br <- v$datos[v$datos$pais == "Brasil", ]
    medias_ar <- vapply(regresores_ar,
                        function(k) mean(ar[[k]], na.rm = TRUE), numeric(1))
    medias_br <- vapply(regresores_ar, function(k) {
      if (k %in% reemplazables) mean(br[[k]], na.rm = TRUE) else NA_real_
    }, numeric(1))

    ln_base <- predecir(medias_ar)
    precio_base <- exp(ln_base) * factor_duan
    precio_observado <- mean(ar$precio_ha_usd_2025, na.rm = TRUE)

    filas <- lapply(cfg$grados_convergencia, function(g) {
      x <- medias_ar
      fuera_de_rango <- character(0)
      for (k in reemplazables) {
        nuevo <- medias_ar[[k]] + g * (medias_br[[k]] - medias_ar[[k]])
        x[[k]] <- nuevo
        rango <- range(datos_ar[[k]], na.rm = TRUE)
        if (nuevo < rango[1] || nuevo > rango[2]) {
          fuera_de_rango <- c(fuera_de_rango, k)
        }
      }
      ln_escenario <- predecir(x)
      delta <- ln_escenario - ln_base

      data.frame(
        ventana = v$clave,
        rol = v$rol,
        grado_convergencia = paste0(round(g * 100), " %"),
        variables_reemplazadas = paste(reemplazables, collapse = "; "),
        ln_precio_predicho_base = ln_base,
        ln_precio_predicho_escenario = ln_escenario,
        aumento_log = delta,
        aumento_porcentual = log_a_porcentaje(delta),
        precio_predicho_base_usd2025 = precio_base,
        precio_escenario_usd2025 = exp(ln_escenario) * factor_duan,
        precio_observado_usd2025 = precio_observado,
        factor_smearing_duan = factor_duan,
        advertencia_extrapolacion = if (length(fuera_de_rango) > 0) {
          paste0("EXTRAPOLACIÓN: el nivel brasileño de ",
                 paste(fuera_de_rango, collapse = ", "),
                 " cae fuera del rango que Argentina tuvo entre ",
                 cfg$anio_min, " y ", cfg$anio_max, ".")
        } else "",
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, filas)
  }

  tabla_potencial <- do.call(rbind, lapply(ventanas, escenarios_en))
  base_principal <- tabla_potencial[tabla_potencial$rol == "principal", ][1, ]

  nota_potencial <- paste0(
    "La comparación es contra el precio PREDICHO por la ecuación argentina ",
    "evaluada en las medias de cada ventana (",
    round(base_principal$precio_predicho_base_usd2025), " USD/ha de ",
    cfg$anio_base_precios, " en la ventana principal ", base_principal$ventana,
    "), no contra el precio observado promedio (",
    round(base_principal$precio_observado_usd2025), " USD/ha). Los ",
    "coeficientes salen siempre de la ecuación estimada sobre ", cfg$anio_min,
    "-", cfg$anio_max, ". Se aplica la corrección de ",
    "retransformación de Duan con factor ", round(factor_duan, 4),
    ": al pasar de logaritmos a dólares, exp() del valor predicho subestima ",
    "el promedio del nivel. Como el factor multiplica por igual al base y al ",
    "escenario, el aumento porcentual no depende de él.",
    if (length(no_reemplazables) > 0) {
      paste0(" NO se reemplazaron ", paste(no_reemplazables, collapse = ", "),
             ": Brasil no tiene esa serie y poner cero sería inventar una ",
             "medición.")
    } else ""
  )

  if (!escenario_informativo) {
    tabla_potencial$advertencia_extrapolacion <- paste0(
      "ESCENARIO VACÍO: no se reemplazó ninguna variable. Las ",
      "institucionales de la ecuación argentina (",
      paste(institucionales_ar, collapse = ", "),
      ") no existen en la ecuación de Brasil, así que el contrafáctico ",
      "coincide con el precio predicho actual. El aumento de 0 % no es un ",
      "resultado: es la ausencia del ejercicio."
    )
    nota_potencial <- paste0(
      nota_potencial,
      " ATENCIÓN: este escenario no se pudo construir. Para que tenga ",
      "contenido hace falta al menos una variable institucional medida en ",
      "los dos países (el PSE de la OCDE, o el riesgo soberano con ",
      "usar_embi = TRUE, o una serie de derechos de exportación de Brasil)."
    )
    problemas <- nuevo_registro_faltantes()
    problemas <- agregar_faltante(
      problemas, "06_convergencia", "precio_potencial",
      paste0("El escenario de convergencia no reemplaza ninguna variable: ",
             "las institucionales de la ecuación argentina no existen en la ",
             "de Brasil. Hace falta una variable institucional medida en los ",
             "dos países.")
    )
    escribir_no_estimado(problemas, "06_convergencia")
  }

  guardar_tabla(tabla_potencial, "tabla_precio_potencial",
                "Precio potencial de la hectárea argentina por grado de convergencia institucional hacia Brasil",
                nota = nota_potencial)

  # -------------------------------------------------------------------------
  # Control numérico contra la descomposición
  # -------------------------------------------------------------------------
  # Con convergencia total, el aumento logarítmico tiene que coincidir con la
  # contribución institucional al componente de niveles de Argentina-Brasil,
  # restringida a las variables que efectivamente se reemplazaron.
  ruta_detalle <- file.path(cfg$dir_resultados,
                            "tabla_descomposicion_por_variable.csv")
  detalle <- if (file.exists(ruta_detalle)) {
    utils::read.csv(ruta_detalle, stringsAsFactors = FALSE)
  } else NULL

  # El control se hace ventana por ventana: cada escenario de convergencia
  # total tiene que coincidir con la contribución institucional de niveles
  # calculada sobre las mismas medias.
  delta_de <- function(clave) {
    tabla_potencial$aumento_log[tabla_potencial$ventana == clave &
                                  tabla_potencial$grado_convergencia == "100 %"]
  }
  control <- do.call(rbind, lapply(ventanas, function(v) {
    delta_v <- delta_de(v$clave)
    fila <- data.frame(
      ventana = v$clave,
      rol = v$rol,
      verificacion = "Convergencia total contra contribución institucional de niveles (Argentina-Brasil)",
      aumento_log_convergencia_total = if (length(delta_v) == 1) delta_v else NA_real_,
      contribucion_institucional_niveles = NA_real_,
      diferencia = NA_real_,
      resultado = "No verificado: falta tabla_descomposicion_por_variable.csv.",
      stringsAsFactors = FALSE
    )
    if (!is.null(detalle) && length(delta_v) == 1) {
      filas_br <- detalle[detalle$comparable == "Brasil" &
                            detalle$ventana == v$clave &
                            detalle$variable %in% reemplazables, ]
      contribucion <- sum(filas_br$contribucion_niveles_log)
      fila$contribucion_institucional_niveles <- contribucion
      fila$diferencia <- delta_v - contribucion
      fila$resultado <- if (abs(delta_v - contribucion) < 1e-8) {
        "Verificado: los dos cálculos coinciden."
      } else {
        paste0("NO coinciden: diferencia de ",
               signif(delta_v - contribucion, 4),
               " puntos logarítmicos. Revisar antes de usar el resultado.")
      }
    }
    fila
  }))
  guardar_tabla(control, "tabla_verificacion_convergencia",
                "Control numérico: convergencia total contra la descomposición")

  # -------------------------------------------------------------------------
  # H2: ¿el aumento supera la mitad de la brecha con Mato Grosso?
  # -------------------------------------------------------------------------
  # H2 solo se puede evaluar si el escenario reemplazó algo y si existe una
  # brecha positiva que cerrar. Si Argentina está por encima de Mato Grosso en
  # la ventana, "cubrir el 50 % de la brecha" no quiere decir nada, y con un
  # escenario vacío cualquier veredicto sería un artefacto.
  h2 <- do.call(rbind, lapply(ventanas, function(v) {
    ar <- v$datos[v$datos$pais == "Argentina", ]
    br <- v$datos[v$datos$pais == "Brasil", ]
    brecha_br <- mean(br[[cfg$dependiente]], na.rm = TRUE) -
      mean(ar[[cfg$dependiente]], na.rm = TRUE)
    delta_v <- delta_de(v$clave)

    motivo <- if (!escenario_informativo) {
      paste0("El escenario de convergencia no reemplazó ninguna variable: no ",
             "hay variables institucionales medidas en Argentina y en Brasil ",
             "a la vez.")
    } else if (is.na(brecha_br)) {
      "No se puede calcular la brecha entre Argentina y Mato Grosso."
    } else if (brecha_br <= 0) {
      paste0("En ", v$clave, " Argentina no está por debajo de Mato Grosso ",
             "(brecha de ", round(brecha_br, 4), " puntos logarítmicos), así ",
             "que no hay una brecha positiva que cerrar.")
    } else ""

    evaluable <- !nzchar(motivo) && length(delta_v) == 1
    supera <- evaluable && delta_v > 0.5 * brecha_br

    data.frame(
      ventana = v$clave,
      rol = v$rol,
      hipotesis = "H2: con convergencia institucional total hacia Brasil, el aumento del precio argentino supera el 50 % de la brecha con Mato Grosso.",
      aumento_log_convergencia_total = if (length(delta_v) == 1) delta_v else NA_real_,
      brecha_total_log_argentina_brasil = brecha_br,
      umbral_50_por_ciento = if (evaluable) 0.5 * brecha_br else NA_real_,
      proporcion_de_la_brecha_cubierta = if (evaluable) delta_v / brecha_br else NA_real_,
      veredicto = if (!evaluable) "NO EVALUABLE" else if (supera) "CONFIRMADA" else "RECHAZADA",
      motivo = motivo,
      stringsAsFactors = FALSE
    )
  }))

  guardar_tabla(h2, "tabla_h2",
                "H2: aumento por convergencia institucional frente a la brecha con Mato Grosso",
                nota = paste0("La hipótesis se evalúa solo si el escenario ",
                              "reemplazó al menos una variable institucional ",
                              "y si la brecha con Mato Grosso es positiva. En ",
                              "cualquier otro caso el veredicto es NO ",
                              "EVALUABLE y la columna 'motivo' explica por ",
                              "qué. La hipótesis se responde con la fila de ",
                              "rol 'principal' (medias de ",
                              sprintf("%d-%d", rango_ventana[1],
                                      rango_ventana[2]),
                              "); la de rol 'anexo' repite el cálculo sobre ",
                              "el período completo."))

  h2_principal <- h2[h2$rol == "principal", ][1, ]
  h2_anexo <- h2[h2$rol == "anexo", ][1, ]

  mensaje("Precio potencial calculado. Variables reemplazadas: ",
          if (length(reemplazables) > 0) paste(reemplazables, collapse = ", ")
          else "ninguna")
  if (length(no_reemplazables) > 0) {
    mensaje("No reemplazadas (Brasil no tiene la serie): ",
            paste(no_reemplazables, collapse = ", "))
  }
  mensaje("Control de la identidad: ",
          paste(control$ventana, control$resultado, sep = ": ",
                collapse = " | "))
  mensaje("H2 (", h2_principal$ventana, "): ", h2_principal$veredicto,
          if (nzchar(h2_principal$motivo)) paste0(" — ", h2_principal$motivo)
          else "")
  mensaje("H2 (anexo ", h2_anexo$ventana, "): ", h2_anexo$veredicto,
          if (nzchar(h2_anexo$motivo)) paste0(" — ", h2_anexo$motivo) else "")
  if (!escenario_informativo) {
    mensaje("ATENCIÓN: el escenario quedó vacío. Hace falta una variable ",
            "institucional medida en Argentina y en Brasil (PSE, EMBI con ",
            "usar_embi = TRUE, o derechos de exportación de Brasil).")
  }
}
