# ---------------------------------------------------------------------------
# 05_descomposicion.R
# Descomposición de Oaxaca-Blinder, calculada a mano.
#
# Por qué a mano y no con el paquete 'oaxaca': ese paquete exige la MISMA
# fórmula en las dos ecuaciones, y acá hay variables que existen en un país y
# no en otro (la alícuota de retenciones solo existe en Argentina).
#
# La identidad que se calcula, para cada comparable C contra Argentina (A):
#
#   media(lnP)_C - media(lnP)_A = (X̄_C - X̄_A)' β̂_A  +  X̄_C' (β̂_C - β̂_A)  +  u
#                                 \_____________/        \_______________/     ^
#                                  componente de          componente de        |
#                                  NIVELES (dotación)     VALORACIÓN (precios) RESIDUO
#
# El término u es la diferencia entre los residuos promedio de las dos
# ecuaciones en la ventana donde se toman las medias. Sobre el período
# completo vale CERO por construcción, porque la recta de mínimos cuadrados
# pasa por las medias de su propia muestra. Sobre una ventana más corta no
# tiene por qué anularse, y lo que mide es la parte de la brecha de esa
# ventana que la ecuación no explica: cuánto se desvía el modelo justo en los
# años que se están mirando. Se informa, no se esconde adentro de otro
# componente.
#
# Las medias salen de cfg$ventana_medias y los coeficientes siempre del
# período completo (el porqué está en 00_config.R). Las dos ventanas se
# calculan y conviven en las mismas tablas bajo la columna 'ventana'.
#
# Convenciones, todas deliberadas:
#   - X̄ incluye la constante, que vale 1 en los dos países. Por eso su
#     término de niveles es cero y la diferencia de ordenadas al origen cae
#     entera en el componente de valoración.
#   - Una variable que NO está en la ecuación del comparable vale 0 en X̄_C.
#     Su término de valoración es entonces 0 por construcción, y toda su
#     contribución aparece en el componente de niveles. Esto es una decisión
#     de medición, no un resultado: está advertido en la tabla y en el informe.
#   - El componente de valoración se informa SOLO agregado, como residual.
#     El detalle por variable no está identificado: depende de qué categoría
#     se tome como base omitida (Oaxaca y Ransom, 1999).
#
# Salidas (cuando hay modelos):
#   03_resultados/tabla_descomposicion_por_variable.csv
#   03_resultados/tabla_descomposicion_por_categoria.csv
#   03_resultados/tabla_h1.csv
# ---------------------------------------------------------------------------

if (!exists("cfg")) source("scripts/00_config.R")
source("scripts/utilidades.R")

mensaje("== 05_descomposicion: Oaxaca-Blinder ==")

ruta_modelos <- file.path(cfg$dir_resultados, "modelos.rds")

if (!file.exists(ruta_modelos)) {
  problemas <- nuevo_registro_faltantes()
  problemas <- agregar_faltante(
    problemas, "05_descomposicion", "descomposicion_oaxaca_blinder",
    paste0("No hay ecuaciones estimadas (04_estimacion.R no produjo ",
           "modelos.rds). Sin coeficientes por país no hay nada que ",
           "descomponer. Ver no_estimado.csv para saber qué series faltan.")
  )
  escribir_no_estimado(problemas, "05_descomposicion")
  guardar_tabla(problemas, "tabla_descomposicion_por_categoria",
                "Descomposición de Oaxaca-Blinder: NO CALCULADA")
  mensaje("NO SE CALCULÓ LA DESCOMPOSICIÓN: faltan las ecuaciones por país.")
} else {

  ajustados <- readRDS(ruta_modelos)
  panel <- ajustados$panel
  PAIS_REFERENCIA <- "Argentina"
  comparables <- setdiff(cfg$paises, PAIS_REFERENCIA)

  # Las dos ventanas en las que se toman las medias. La primera es la que
  # responde las hipótesis; la segunda queda como anexo. Los coeficientes
  # salen del panel completo en los dos casos.
  rango_ventana <- cfg$ventana_medias
  panel_ventana <- panel[panel$anio >= rango_ventana[1] &
                           panel$anio <= rango_ventana[2], ]
  if (nrow(panel_ventana) == 0) {
    stop("La ventana ", rango_ventana[1], "-", rango_ventana[2],
         " no tiene ningún año en el panel.")
  }
  ventanas <- list(
    list(clave = sprintf("%d-%d", rango_ventana[1], rango_ventana[2]),
         rol = "principal", datos = panel_ventana),
    list(clave = sprintf("%d-%d", cfg$anio_min, cfg$anio_max),
         rol = "anexo", datos = panel)
  )

  # -------------------------------------------------------------------------
  # Funciones de cálculo
  # -------------------------------------------------------------------------

  # Estima la ecuación en niveles de un país sobre los datos que se le pasen.
  # Se usa tanto con el panel original como con cada remuestra del bootstrap.
  estimar <- function(datos, pais) {
    sub <- datos[datos$pais == pais, ]
    formula <- stats::as.formula(paste(
      cfg$dependiente, "~", paste(cfg$regresores[[pais]], collapse = " + ")
    ))
    stats::lm(formula, data = sub)
  }

  # Vector de medias de un país sobre el conjunto de variables 'vars'.
  # La constante vale 1. Una variable que no pertenece a la ecuación de ese
  # país vale 0 (ver la nota del encabezado).
  # Las medias se toman sobre los mismos años que usa la ecuación. Si un
  # regresor tiene un año vacío, ese año no entra ni a la media del precio ni
  # a la media del regresor: mezclarlas compararía un promedio de 21 años
  # contra un promedio de 20.
  filas_completas <- function(datos, pais) {
    sub <- datos[datos$pais == pais, , drop = FALSE]
    cols <- intersect(c(cfg$dependiente, cfg$regresores[[pais]]), names(sub))
    sub[stats::complete.cases(sub[, cols, drop = FALSE]), , drop = FALSE]
  }

  medias_de <- function(datos, pais, vars) {
    sub <- filas_completas(datos, pais)
    propias <- cfg$regresores[[pais]]
    vapply(vars, function(v) {
      if (v == "(Intercept)") return(1)
      if (!v %in% propias) return(0)
      mean(sub[[v]], na.rm = TRUE)
    }, numeric(1))
  }

  # Vector de coeficientes extendido al conjunto 'vars', con cero donde la
  # variable no está en esa ecuación.
  coeficientes_de <- function(modelo, vars) {
    b <- stats::coef(modelo)
    vapply(vars, function(v) if (v %in% names(b)) unname(b[v]) else 0, numeric(1))
  }

  # Núcleo de la descomposición. Devuelve el detalle por variable, el
  # agregado por categoría y los totales.
  # 'datos' es de donde salen los COEFICIENTES y 'datos_medias' de donde salen
  # las MEDIAS. Por omisión son el mismo conjunto, que es el caso clásico.
  descomponer <- function(datos, comparable, datos_medias = datos) {
    modelo_a <- estimar(datos, PAIS_REFERENCIA)
    modelo_c <- estimar(datos, comparable)

    vars <- union(names(stats::coef(modelo_a)), names(stats::coef(modelo_c)))
    vars <- c("(Intercept)", setdiff(vars, "(Intercept)"))

    xa <- medias_de(datos_medias, PAIS_REFERENCIA, vars)
    xc <- medias_de(datos_medias, comparable, vars)
    ba_propio <- coeficientes_de(modelo_a, vars)
    ba <- ba_propio
    bc <- coeficientes_de(modelo_c, vars)

    # Referencia de los coeficientes. La principal es Argentina; "pooled"
    # queda como ejercicio de robustez y usa la muestra de los dos países
    # comparados con la especificación argentina.
    if (identical(cfg$referencia_oaxaca, "pooled")) {
      datos_pool <- datos[datos$pais %in% c(PAIS_REFERENCIA, comparable), ]
      formula_pool <- stats::as.formula(paste(
        cfg$dependiente, "~",
        paste(cfg$regresores[[PAIS_REFERENCIA]], collapse = " + ")
      ))
      modelo_pool <- stats::lm(formula_pool, data = datos_pool)
      ba <- coeficientes_de(modelo_pool, vars)
    }

    niveles_por_variable <- (xc - xa) * ba
    valoracion_total <- sum(xc * (bc - ba))

    dep <- cfg$dependiente
    media_a <- mean(filas_completas(datos_medias, PAIS_REFERENCIA)[[dep]])
    media_c <- mean(filas_completas(datos_medias, comparable)[[dep]])
    brecha <- media_c - media_a

    # Residuo promedio de cada ecuación en la ventana: lo observado menos lo
    # que la ecuación del período completo predice en esas medias. Sobre el
    # período completo da cero; sobre una ventana corta, no.
    residuo_a <- media_a - sum(xa * ba_propio)
    residuo_c <- media_c - sum(xc * bc)
    residuo_total <- residuo_c - residuo_a

    categorias <- categoria_de(vars)
    categorias[vars == "(Intercept)"] <- "constante"
    niveles_por_categoria <- tapply(niveles_por_variable, categorias, sum)

    list(
      variables  = vars,
      categorias = categorias,
      x_a = xa, x_c = xc, beta_a = ba, beta_c = bc,
      niveles_por_variable  = niveles_por_variable,
      niveles_por_categoria = niveles_por_categoria,
      niveles_total    = sum(niveles_por_variable),
      valoracion_total = valoracion_total,
      residuo_total    = residuo_total,
      residuo_a = residuo_a, residuo_c = residuo_c,
      brecha_total     = brecha,
      media_a = media_a, media_c = media_c
    )
  }

  # Resumen compacto que usa el bootstrap: una contribución por categoría más
  # la diferencia institucional menos productiva, que es la que evalúa H1.
  resumen_para_bootstrap <- function(datos, comparable, datos_medias = datos) {
    d <- descomponer(datos, comparable, datos_medias)
    obtener <- function(cat) {
      valor <- d$niveles_por_categoria[cat]
      if (is.na(valor) || is.null(valor)) 0 else unname(valor)
    }
    c(
      productivo      = obtener("productivo"),
      economico       = obtener("economico"),
      institucional   = obtener("institucional"),
      niveles_total   = d$niveles_total,
      valoracion      = d$valoracion_total,
      residuo         = d$residuo_total,
      brecha_total    = d$brecha_total,
      inst_menos_prod = obtener("institucional") - obtener("productivo"),
      inst_menos_econ = obtener("institucional") - obtener("economico")
    )
  }

  # -------------------------------------------------------------------------
  # Bootstrap en bloques
  # -------------------------------------------------------------------------
  # Se remuestrean AÑOS en bloques contiguos, no observaciones sueltas: las
  # series son anuales y están autocorrelacionadas, y además los tres países
  # tienen que moverse juntos para que el panel siga alineado.
  # Largo del bloque: round(n^(1/3)), que con 21 años son bloques de 3.
  #
  # Qué se remuestrea y qué no: se remuestrean los años que estiman los
  # COEFICIENTES, porque ahí está la incertidumbre que interesa. Las medias de
  # la ventana se dejan fijas en su valor observado: los niveles de 2021-2025
  # son un dato, no una muestra de la que haya que inferir nada. El intervalo
  # que sale refleja entonces la incertidumbre de las sensibilidades, y así
  # queda declarado en la nota de la tabla.
  intervalos_bootstrap <- function(comparable, datos_medias) {
    if (!requireNamespace("boot", quietly = TRUE)) {
      return(NULL)
    }
    anios <- sort(unique(panel$anio))
    n <- length(anios)
    largo <- cfg$largo_bloque(n)
    nombres <- names(resumen_para_bootstrap(panel, comparable, datos_medias))

    estadistico <- function(indices) {
      anios_remuestra <- anios[indices]
      datos <- do.call(rbind, lapply(seq_along(anios_remuestra), function(k) {
        fila <- panel[panel$anio == anios_remuestra[k], ]
        fila$anio <- k   # se reindexa para no repetir años dentro del lm
        fila
      }))
      salida <- try(resumen_para_bootstrap(datos, comparable, datos_medias),
                    silent = TRUE)
      if (inherits(salida, "try-error")) {
        return(rep(NA_real_, length(nombres)))
      }
      salida
    }

    set.seed(cfg$semilla)
    resultado <- boot::tsboot(
      tseries   = stats::ts(seq_len(n)),
      statistic = estadistico,
      R         = cfg$bootstrap_B,
      l         = largo,
      sim       = "fixed"
    )
    list(replicas = resultado$t, largo_bloque = largo, nombres = nombres)
  }

  # -------------------------------------------------------------------------
  # Cálculo y salidas
  # -------------------------------------------------------------------------
  detalle_variables <- list()
  detalle_categorias <- list()
  filas_h1 <- list()

  for (v in ventanas) {
   for (comparable in comparables) {
    d <- descomponer(panel, comparable, v$datos)

    # Control de la identidad: niveles + valoración + residuo da la brecha.
    error <- abs(d$niveles_total + d$valoracion_total + d$residuo_total -
                   d$brecha_total)
    if (error > 1e-8) {
      warning("La identidad de la descomposición no cierra para ", comparable,
              " en la ventana ", v$clave,
              " (diferencia de ", signif(error, 3), ").")
    }

    # --- Detalle por variable (solo componente de niveles) -----------------
    detalle_variables[[paste(v$clave, comparable)]] <- data.frame(
      ventana = v$clave,
      rol = v$rol,
      comparable = comparable,
      variable   = d$variables,
      categoria  = d$categorias,
      media_argentina = d$x_a,
      media_comparable = d$x_c,
      coeficiente_argentina = d$beta_a,
      contribucion_niveles_log = d$niveles_por_variable,
      porcentaje_de_la_brecha = d$niveles_por_variable / d$brecha_total * 100,
      diferencia_porcentual = log_a_porcentaje(d$niveles_por_variable),
      observacion = ifelse(
        !d$variables %in% cfg$regresores[[comparable]] &
          d$variables != "(Intercept)",
        paste0("La variable no está en la ecuación de ", comparable,
               ": su media vale 0 por construcción y toda su contribución ",
               "cae en el componente de niveles."),
        ""
      ),
      stringsAsFactors = FALSE, row.names = NULL
    )

    # --- Agregado por categoría, con intervalos de confianza ---------------
    boot_salida <- intervalos_bootstrap(comparable, v$datos)
    punto <- resumen_para_bootstrap(panel, comparable, v$datos)

    intervalo <- function(nombre) {
      if (is.null(boot_salida)) return(c(NA_real_, NA_real_))
      columna <- match(nombre, boot_salida$nombres)
      valores <- boot_salida$replicas[, columna]
      valores <- valores[is.finite(valores)]
      if (length(valores) < 100) return(c(NA_real_, NA_real_))
      alfa <- (1 - cfg$nivel_confianza) / 2
      unname(stats::quantile(valores, c(alfa, 1 - alfa)))
    }

    componentes <- c("productivo", "economico", "institucional",
                     "niveles_total", "valoracion", "residuo",
                     "inst_menos_prod", "inst_menos_econ")
    etiquetas <- c(
      productivo      = "Niveles: factores productivos",
      economico       = "Niveles: factores económicos",
      institucional   = "Niveles: factores institucionales",
      niveles_total   = "Componente de niveles (total)",
      valoracion      = "Componente de valoración (residual, solo agregado)",
      residuo         = "Residuo: parte de la brecha que la ecuación no explica en la ventana",
      inst_menos_prod = "Institucional menos productivo",
      inst_menos_econ = "Institucional menos económico"
    )

    # Cuando la brecha total es casi cero, expresar cada contribución como
    # porcentaje de ella produce números enormes que no significan nada
    # (dividir por un denominador minúsculo). En ese caso se avisa.
    brecha_chica <- abs(punto[["brecha_total"]]) < 0.10
    aviso_porcentaje <- if (brecha_chica) {
      paste0("La brecha total es de apenas ",
             round(punto[["brecha_total"]], 4), " puntos logarítmicos: el ",
             "porcentaje sobre la brecha no es interpretable. Usar la ",
             "contribución en puntos logarítmicos.")
    } else ""

    filas_categoria <- lapply(componentes, function(k) {
      ic <- intervalo(k)
      data.frame(
        ventana = v$clave,
        rol = v$rol,
        comparable = comparable,
        componente = etiquetas[[k]],
        contribucion_log = unname(punto[k]),
        ic_inferior = ic[1], ic_superior = ic[2],
        porcentaje_de_la_brecha = unname(punto[k]) / punto[["brecha_total"]] * 100,
        diferencia_porcentual = log_a_porcentaje(unname(punto[k])),
        advertencia = aviso_porcentaje,
        stringsAsFactors = FALSE
      )
    })
    filas_categoria[[length(filas_categoria) + 1]] <- data.frame(
      ventana = v$clave,
      rol = v$rol,
      comparable = comparable,
      componente = "Brecha total (media de ln P del comparable menos Argentina)",
      contribucion_log = unname(punto[["brecha_total"]]),
      ic_inferior = NA_real_, ic_superior = NA_real_,
      porcentaje_de_la_brecha = 100,
      diferencia_porcentual = log_a_porcentaje(unname(punto[["brecha_total"]])),
      advertencia = aviso_porcentaje,
      stringsAsFactors = FALSE
    )
    detalle_categorias[[paste(v$clave, comparable)]] <-
      do.call(rbind, filas_categoria)

    # --- Evaluación de H1 ---------------------------------------------------
    inst <- unname(punto[["institucional"]])
    prod <- unname(punto[["productivo"]])
    econ <- unname(punto[["economico"]])
    cumple <- inst > prod && inst > econ
    filas_h1[[paste(v$clave, comparable)]] <- data.frame(
      ventana = v$clave,
      rol = v$rol,
      comparable = comparable,
      institucional = inst, productivo = prod, economico = econ,
      institucional_mayor_que_productivo = inst > prod,
      institucional_mayor_que_economico  = inst > econ,
      cumple_en_este_comparable = cumple,
      stringsAsFactors = FALSE
    )
   }
  }

  guardar_tabla(
    do.call(rbind, detalle_variables), "tabla_descomposicion_por_variable",
    "Componente de niveles, detallado por variable",
    nota = paste0("Solo se detalla el componente de NIVELES. El de valoración ",
                  "se informa agregado en la tabla por categoría, porque su ",
                  "reparto por variable no está identificado (Oaxaca y ",
                  "Ransom, 1999). Las contribuciones negativas reducen la ",
                  "brecha y se informan con su signo. El precio de la soja ",
                  "en CBOT y la tasa TIPS toman el mismo valor en los tres ",
                  "países, y el tipo de cambio real está normalizado a ",
                  "promedio 100 en cada país: esas tres aportan cero al ",
                  "componente de niveles por construcción. La inflación de ",
                  "cierre y la tasa nominal local difieren entre países y ",
                  "sí pueden aportar a los niveles. Los coeficientes son ",
                  "siempre los del período completo; lo que cambia entre ",
                  "ventanas son las medias.")
  )

  guardar_tabla(
    do.call(rbind, detalle_categorias), "tabla_descomposicion_por_categoria",
    "Descomposición de Oaxaca-Blinder por categoría, con intervalos bootstrap",
    nota = paste0("Unidades: puntos logarítmicos, porcentaje de la brecha ",
                  "total y diferencia porcentual equivalente (exp(delta)-1)*100. ",
                  "Intervalos al ", round(cfg$nivel_confianza * 100),
                  "% por bootstrap en bloques de años (B = ", cfg$bootstrap_B,
                  "). Coeficientes de referencia: ", cfg$referencia_oaxaca,
                  ". Los coeficientes se estiman siempre sobre ", cfg$anio_min,
                  "-", cfg$anio_max, "; la columna 'ventana' indica sobre qué ",
                  "años se tomaron las MEDIAS. El bootstrap remuestrea los ",
                  "años que estiman los coeficientes y deja las medias de la ",
                  "ventana en su valor observado, así que el intervalo mide ",
                  "incertidumbre de las sensibilidades, no de los niveles.")
  )

  # --- Veredicto de H1 -------------------------------------------------------
  # Una regla por ventana: la principal responde la hipótesis y la de anexo
  # queda para comparar.
  h1 <- do.call(rbind, filas_h1)
  veredicto_de <- function(cumplidos) {
    if (cumplidos == length(comparables)) {
      "CONFIRMADA: la contribución institucional supera a la productiva y a la económica frente a los dos comparables."
    } else if (cumplidos == 1) {
      "CONFIRMADA PARCIALMENTE: se cumple frente a un comparable y no frente al otro."
    } else {
      "RECHAZADA: no se cumple frente a ningún comparable."
    }
  }
  h1$veredicto_global <- vapply(seq_len(nrow(h1)), function(i) {
    misma <- h1$ventana == h1$ventana[i]
    veredicto_de(sum(h1$cumple_en_este_comparable[misma]))
  }, character(1))

  guardar_tabla(
    h1, "tabla_h1",
    "H1: ¿la contribución institucional al componente de niveles es la mayor?",
    nota = paste0("Regla fijada antes de estimar: se confirma si la ",
                  "contribución institucional supera a la productiva y a la ",
                  "económica frente a los dos comparables; se confirma ",
                  "parcialmente si se cumple frente a uno; se rechaza en los ",
                  "demás casos. La hipótesis se responde con la fila de rol ",
                  "'principal' (medias de ",
                  sprintf("%d-%d", rango_ventana[1], rango_ventana[2]),
                  "); la de rol 'anexo' repite el cálculo sobre el período ",
                  "completo.")
  )

  principal <- h1[h1$rol == "principal", ]
  mensaje("Descomposición calculada para: ", paste(comparables, collapse = ", "))
  mensaje("Medias de la ventana principal: ", principal$ventana[1],
          " — anexo: ", h1$ventana[h1$rol == "anexo"][1])
  mensaje("H1 (", principal$ventana[1], "): ", principal$veredicto_global[1])
  mensaje("H1 (anexo ", h1$ventana[h1$rol == "anexo"][1], "): ",
          h1$veredicto_global[h1$rol == "anexo"][1])
}
