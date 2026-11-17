# ---------------------------------------------------------------------------
# 04_estimacion.R
# Regresión por MCO del logaritmo del precio por hectárea, una ecuación por
# país, con errores estándar Newey-West (HAC).
#
# Antes de estimar cualquier cosa, el script pasa por un control (el "gate"):
# si alguna variable de la especificación no existe en el panel o tiene años
# vacíos, NO se estima nada. En su lugar se escribe 03_resultados/no_estimado.csv
# con el detalle de qué falta. Preferimos un resultado ausente a un resultado
# construido sobre datos inventados.
#
# Salidas (cuando el gate abre):
#   03_resultados/tabla_coeficientes.csv       coeficientes con errores HAC
#   03_resultados/tabla_ajuste.csv             R2 ajustado, gl y n
#   03_resultados/tabla_diagnosticos.csv       BG, BP, RESET y VIF
#   03_resultados/tabla_pruebas_bloques.csv    significatividad por bloque
#   03_resultados/tabla_capitalizacion.csv     H0: la tierra sigue a la soja 1 a 1
#   03_resultados/tabla_igualdad_coeficientes.csv
#   03_resultados/tabla_endogeneidad_fiscal.csv
#   03_resultados/modelos.rds                  insumo de 05 y 06
# ---------------------------------------------------------------------------

if (!exists("cfg")) source("scripts/00_config.R")
source("scripts/utilidades.R")

mensaje("== 04_estimacion: regresiones por país ==")

for (paquete in c("sandwich", "lmtest")) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    stop("Falta el paquete '", paquete, "'. Instalarlo con renv::install(\"",
         paquete, "\").", call. = FALSE)
  }
}

panel <- utils::read.csv(file.path(cfg$dir_curados, "panel.csv"),
                         stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# Gate: ¿están todos los datos que la especificación necesita?
# ---------------------------------------------------------------------------
control <- verificar_gate(panel, cfg)

if (!control$ok) {
  escribir_no_estimado(control$problemas, "04_estimacion")
  guardar_tabla(
    control$problemas, "tabla_coeficientes",
    "Coeficientes por país: NO ESTIMADOS",
    nota = paste0("Las regresiones no se corrieron. Cada fila dice qué ",
                  "variable falta y por qué. Cuando esas series estén en ",
                  "data/ y el panel se vuelva a armar, este archivo pasa a ",
                  "tener los coeficientes.")
  )
  mensaje("\nNO SE ESTIMÓ NINGUNA ECUACIÓN. Faltan ", nrow(control$problemas),
          " series o años:")
  for (i in seq_len(nrow(control$problemas))) {
    mensaje("  - ", control$problemas$elemento[i], ": ",
            control$problemas$motivo[i])
  }
  mensaje("\nDetalle completo en ", file.path(cfg$dir_resultados, "no_estimado.csv"))
  # Se corta acá sin error: run_all.R sigue con los demás scripts, que también
  # van a informar que no hay nada que descomponer.
} else {

  # -------------------------------------------------------------------------
  # Estimación
  # -------------------------------------------------------------------------
  # Siempre se estima la ecuación en NIVELES, porque es la que necesita la
  # descomposición de Oaxaca-Blinder (que compara medias de la dependiente).
  # Si 00_config.R pide otra especificación, se estima también esa y las dos
  # quedan informadas.
  estimar_niveles <- function(pais) {
    datos <- panel[panel$pais == pais, ]
    datos <- datos[order(datos$anio), ]
    formula <- stats::as.formula(paste(
      cfg$dependiente, "~", paste(cfg$regresores[[pais]], collapse = " + ")
    ))
    stats::lm(formula, data = datos)
  }

  estimar_diferencias <- function(pais) {
    datos <- panel[panel$pais == pais, ]
    datos <- datos[order(datos$anio), ]
    columnas <- c(cfg$dependiente, cfg$regresores[[pais]])
    diferenciado <- as.data.frame(lapply(datos[, columnas, drop = FALSE], diff))
    formula <- stats::as.formula(paste(
      cfg$dependiente, "~", paste(cfg$regresores[[pais]], collapse = " + ")
    ))
    stats::lm(formula, data = diferenciado)
  }

  modelos_niveles <- stats::setNames(lapply(cfg$paises, estimar_niveles),
                                     cfg$paises)

  modelos_elegidos <- switch(
    cfg$especificacion,
    "niveles_MCO" = modelos_niveles,
    "diferencias" = stats::setNames(lapply(cfg$paises, estimar_diferencias),
                                    cfg$paises),
    "ARDL" = {
      if (!requireNamespace("ARDL", quietly = TRUE)) {
        stop("La especificación 'ARDL' necesita el paquete 'ARDL'. ",
             "Instalarlo con renv::install(\"ARDL\") y volver a correr.",
             call. = FALSE)
      }
      stop("La especificación 'ARDL' requiere elegir el orden de rezagos y ",
           "correr el bounds test con datos completos. Con 21 observaciones ",
           "y 5 regresores los grados de libertad no alcanzan para rezagos ",
           "largos: definir el orden antes de habilitarla.", call. = FALSE)
    },
    stop("especificacion desconocida en 00_config.R: ", cfg$especificacion,
         call. = FALSE)
  )

  # -------------------------------------------------------------------------
  # Coeficientes con errores Newey-West y medidas de ajuste
  # -------------------------------------------------------------------------
  coeficientes <- list()
  ajuste <- list()
  diagnosticos <- list()

  for (pais in cfg$paises) {
    modelo <- modelos_elegidos[[pais]]
    n  <- stats::nobs(modelo)
    gl <- modelo$df.residual
    rezago <- cfg$rezago_hac(n)

    tabla_pais <- coeficientes_hac(modelo, rezago)
    tabla_pais$pais <- pais
    tabla_pais$categoria <- categoria_de(tabla_pais$variable)
    tabla_pais$rezago_hac <- rezago
    coeficientes[[pais]] <- tabla_pais[, c("pais", "variable", "categoria",
                                           "coeficiente", "ee_hac",
                                           "estadistico_t", "p_valor",
                                           "rezago_hac")]

    resumen <- summary(modelo)
    ajuste[[pais]] <- data.frame(
      pais = pais, n = n, regresores = length(stats::coef(modelo)) - 1,
      grados_libertad = gl,
      r2 = resumen$r.squared, r2_ajustado = resumen$adj.r.squared,
      rezago_hac = rezago,
      advertencia = if (gl < 10) {
        paste0("Quedan ", gl, " grados de libertad: la inferencia es frágil.")
      } else "",
      stringsAsFactors = FALSE
    )

    # Diagnósticos. Cada uno puede fallar por falta de grados de libertad;
    # cuando pasa, se informa en lugar de dejar la celda vacía.
    correr <- function(etiqueta, expresion) {
      resultado <- try(expresion, silent = TRUE)
      if (inherits(resultado, "try-error")) {
        data.frame(pais = pais, prueba = etiqueta, estadistico = NA_real_,
                   p_valor = NA_real_,
                   observacion = paste0("No se pudo calcular: ",
                                        trimws(as.character(resultado))),
                   stringsAsFactors = FALSE)
      } else {
        data.frame(pais = pais, prueba = etiqueta,
                   estadistico = unname(resultado$statistic),
                   p_valor = unname(resultado$p.value),
                   observacion = "", stringsAsFactors = FALSE)
      }
    }

    pruebas_pais <- rbind(
      correr("Breusch-Godfrey (autocorrelación, orden 1)",
             lmtest::bgtest(modelo, order = 1)),
      correr("Breusch-Godfrey (autocorrelación, orden 2)",
             lmtest::bgtest(modelo, order = 2)),
      correr("Breusch-Pagan (heterocedasticidad)", lmtest::bptest(modelo)),
      correr("RESET (forma funcional, potencias 2 y 3)",
             lmtest::resettest(modelo, power = 2:3, type = "fitted"))
    )

    vif <- vif_manual(modelo)
    pruebas_vif <- data.frame(
      pais = pais,
      prueba = paste0("VIF de ", vif$variable),
      estadistico = vif$vif,
      p_valor = NA_real_,
      observacion = ifelse(is.na(vif$vif), "",
                           ifelse(vif$vif > 10,
                                  "VIF mayor a 10: colinealidad alta.", "")),
      stringsAsFactors = FALSE
    )
    diagnosticos[[pais]] <- rbind(pruebas_pais, pruebas_vif)
  }

  guardar_tabla(do.call(rbind, coeficientes), "tabla_coeficientes",
                paste0("Coeficientes por país (", cfg$especificacion,
                       ") con errores estándar Newey-West"),
                nota = paste0("El rezago HAC sigue la regla floor(4*(n/100)^(2/9)). ",
                              "La variable dependiente es ", cfg$dependiente, "."))
  guardar_tabla(do.call(rbind, ajuste), "tabla_ajuste",
                "Ajuste de las ecuaciones: R2, grados de libertad y n")
  guardar_tabla(do.call(rbind, diagnosticos), "tabla_diagnosticos",
                "Diagnósticos de las ecuaciones por país")

  # -------------------------------------------------------------------------
  # Significatividad por bloque de factores
  # -------------------------------------------------------------------------
  # La tabla de coeficientes mira una variable por vez, y con 21 años eso
  # subestima lo que el bloque hace en conjunto: las variables de un mismo
  # bloque se mueven juntas, así que la ecuación no puede repartir el mérito
  # entre ellas y ninguna sobresale por separado aunque el grupo sí importe.
  #
  # Acá se prueba el bloque entero de una vez. H0: TODOS los coeficientes del
  # bloque son cero a la vez. Es la pregunta que corresponde, porque H1 está
  # escrita sobre un grupo de factores y no sobre una variable suelta.
  #
  # La prueba no cambia ningún coeficiente ni ninguna ecuación: cambia qué se
  # le pregunta a la ecuación ya estimada. Y no arregla la endogeneidad de la
  # alícuota: que el bloque institucional resulte significativo dice que esas
  # variables están asociadas al precio de la tierra, no que lo causen.
  filas_bloques <- list()
  for (pais in cfg$paises) {
    modelo <- modelos_elegidos[[pais]]
    V <- sandwich::NeweyWest(modelo,
                             lag = cfg$rezago_hac(stats::nobs(modelo)),
                             prewhite = FALSE, adjust = TRUE)
    regs <- cfg$regresores[[pais]]
    cats <- categoria_de(regs)
    orden <- intersect(unique(unname(cfg$categorias)), cats)
    bloques <- stats::setNames(lapply(orden, function(ct) regs[cats == ct]),
                               orden)
    bloques[["la ecuación entera"]] <- regs

    for (nombre in names(bloques)) {
      vs <- bloques[[nombre]]
      w <- prueba_wald(modelo, V, vs)
      p <- w$p_valor_f
      filas_bloques[[paste(pais, nombre)]] <- data.frame(
        pais = pais, bloque = nombre, n_variables = length(vs),
        variables = paste(vs, collapse = ", "),
        estadistico_f = w$estadistico_f,
        gl_numerador = w$gl, gl_denominador = w$gl_denominador,
        p_valor = p, p_valor_chi2 = w$p_valor,
        conclusion = if (is.na(p)) {
          "Sin veredicto: la prueba no se pudo calcular."
        } else if (p < cfg$nivel_bloques) {
          paste0("El bloque mueve el precio de la tierra en ", pais,
                 ": se rechaza que sus coeficientes sean todos cero.")
        } else {
          paste0("El bloque no llega: no se puede descartar que sus ",
                 "coeficientes sean todos cero en ", pais, ".")
        },
        stringsAsFactors = FALSE
      )
    }
  }

  guardar_tabla(
    do.call(rbind, filas_bloques), "tabla_pruebas_bloques",
    "Significatividad conjunta de cada bloque de factores, por país",
    nota = paste0("H0: todos los coeficientes del bloque son cero a la vez. ",
                  "Prueba de Wald con matriz de covarianzas Newey-West. El ",
                  "p-valor informado es el de la versión F, que compara el ",
                  "estadístico contra los grados de libertad que quedan; la ",
                  "columna p_valor_chi2 trae la versión asintótica, que con ",
                  "21 observaciones rechaza de más y no es la que se ",
                  "reporta. La fila 'la ecuación entera' prueba todos los ",
                  "regresores a la vez. La prueba no corrige la endogeneidad ",
                  "de la variable fiscal: mide asociación conjunta, no ",
                  "efecto causal.")
  )

  # -------------------------------------------------------------------------
  # Capitalización del precio internacional de la soja
  # -------------------------------------------------------------------------
  # Se prueba H0: el coeficiente del precio de la soja vale 1, que es la
  # capitalización plena (el campo sube lo mismo que el precio internacional).
  # El porqué del 1 y el porqué de la fila extra de Argentina están en
  # 00_config.R. La prueba no usa el coeficiente de la alícuota, así que el
  # resultado no hereda su problema de endogeneidad.
  soja <- cfg$variable_capitalizacion

  medir_capitalizacion <- function(etiqueta, pais, regs) {
    if (!soja %in% regs) return(NULL)
    datos <- panel[panel$pais == pais, c(cfg$dependiente, regs)]
    datos <- datos[stats::complete.cases(datos), ]
    if (nrow(datos) <= length(regs) + 1) return(NULL)

    modelo <- stats::lm(stats::reformulate(regs, cfg$dependiente), data = datos)
    gl <- stats::df.residual(modelo)
    V <- sandwich::NeweyWest(modelo, lag = cfg$rezago_hac(stats::nobs(modelo)),
                             prewhite = FALSE, adjust = TRUE)
    b <- unname(stats::coef(modelo)[soja])
    ee <- sqrt(V[soja, soja])
    critico <- stats::qt(0.975, gl)
    t_cero <- b / ee
    t_uno  <- (b - 1) / ee
    p_uno  <- 2 * stats::pt(abs(t_uno), gl, lower.tail = FALSE)

    # De logaritmos a precios: cuánto sube la hectárea si la soja sube el
    # porcentaje de referencia.
    en_precio <- function(coeficiente) {
      (exp(coeficiente * log(1 + cfg$suba_referencia_pct / 100)) - 1) * 100
    }

    data.frame(
      pais = pais, especificacion = etiqueta,
      n = stats::nobs(modelo), grados_libertad = gl,
      coeficiente = b, ee_hac = ee,
      ic95_inferior = b - critico * ee, ic95_superior = b + critico * ee,
      suba_soja_pct = cfg$suba_referencia_pct,
      suba_tierra_pct = en_precio(b),
      suba_tierra_pct_ic_inferior = en_precio(b - critico * ee),
      suba_tierra_pct_ic_superior = en_precio(b + critico * ee),
      t_igual_cero = t_cero,
      p_igual_cero = 2 * stats::pt(abs(t_cero), gl, lower.tail = FALSE),
      t_igual_uno = t_uno, p_igual_uno = p_uno,
      conclusion = if (p_uno < 0.05) {
        paste0("Se rechaza la capitalización plena: la hectárea no sigue al ",
               "precio internacional uno a uno.")
      } else {
        paste0("No se rechaza la capitalización plena: no se puede ",
               "distinguir de que la hectárea siga al precio internacional ",
               "uno a uno.")
      },
      stringsAsFactors = FALSE
    )
  }

  filas_capitalizacion <- list()
  if (grepl("^ln_", cfg$dependiente)) {
    for (pais in cfg$paises) {
      filas_capitalizacion[[pais]] <- medir_capitalizacion(
        "ecuación del trabajo", pais, cfg$regresores[[pais]])

      # Argentina va dos veces, y la segunda queda pegada a la primera para
      # que se lean juntas: con la lista de variables de Brasil, que es la
      # comparación pareja (el porqué está en 00_config.R).
      if (identical(pais, "Argentina")) {
        quitadas <- setdiff(cfg$regresores[["Argentina"]],
                            cfg$regresores[["Brasil"]])
        if (length(quitadas) > 0) {
          filas_capitalizacion[["Argentina comparable"]] <- medir_capitalizacion(
            paste0("sin ", paste(quitadas, collapse = ", "),
                   ": misma lista que Brasil"),
            "Argentina", intersect(cfg$regresores[["Argentina"]],
                                   cfg$regresores[["Brasil"]]))
        }
      }
    }
  }

  capitalizacion <- if (length(filas_capitalizacion) == 0) {
    data.frame(
      pais = "ninguno", especificacion = "no corrida",
      conclusion = paste0("La prueba necesita la dependiente y el precio de ",
                          "la soja en logaritmo, y que el precio de la soja ",
                          "sea regresor de alguna ecuación."),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, filas_capitalizacion)
  }

  guardar_tabla(
    capitalizacion, "tabla_capitalizacion",
    paste0("Capitalización del precio internacional de la soja: cuánto sube ",
           "la hectárea si la soja sube ", cfg$suba_referencia_pct, " %"),
    nota = paste0("H0: el coeficiente de ", soja, " vale 1, es decir que la ",
                  "hectárea sigue al precio internacional uno a uno. Prueba ",
                  "t con errores Newey-West. La columna suba_tierra_pct ",
                  "traduce el coeficiente a precios: cuánto sube la ",
                  "hectárea si la soja sube ", cfg$suba_referencia_pct,
                  " %. La fila comparable de Argentina repite la estimación ",
                  "con la lista de variables de Brasil, porque con la ",
                  "alícuota adentro el coeficiente mide el efecto a ",
                  "retención fija y en Brasil mide el efecto total.")
  )

  # -------------------------------------------------------------------------
  # Igualdad de coeficientes económicos entre países
  # -------------------------------------------------------------------------
  # La pregunta sobre los factores económicos es de SENSIBILIDAD, no de
  # niveles: no interesa si el precio de la soja es más alto en un país
  # (es el mismo precio internacional), sino si el precio de la tierra
  # reacciona distinto. Por eso se estima un modelo agrupado con una
  # interacción país x variable y se prueba si esas interacciones son cero.
  #
  # El modelo agrupado solo puede incluir las variables que existen en las
  # tres ecuaciones. Las variables exclusivas de un país (la alícuota, por
  # ejemplo) quedan afuera, y eso se informa en la nota de la tabla.
  comunes <- Reduce(intersect, cfg$regresores)
  economicas <- comunes[categoria_de(comunes) == "economico"]

  if (length(economicas) == 0) {
    igualdad <- data.frame(
      variable = "ninguna", estadistico_f = NA_real_, grados_libertad = 0L,
      grados_libertad_denominador = NA_integer_, p_valor = NA_real_,
      conclusion = paste0("No hay variables económicas comunes a las tres ",
                          "ecuaciones; la prueba no se corrió."),
      stringsAsFactors = FALSE
    )
  } else {
    datos_pool <- panel[, c("pais", "anio", cfg$dependiente, comunes)]
    datos_pool <- datos_pool[stats::complete.cases(datos_pool), ]
    datos_pool$pais <- factor(datos_pool$pais, levels = cfg$paises)

    formula_pool <- stats::as.formula(paste(
      cfg$dependiente, "~ pais * (", paste(comunes, collapse = " + "), ")"
    ))
    modelo_pool <- stats::lm(formula_pool, data = datos_pool)
    V_pool <- sandwich::NeweyWest(modelo_pool,
                                  lag = cfg$rezago_hac(stats::nobs(modelo_pool)),
                                  prewhite = FALSE, adjust = TRUE)

    filas_igualdad <- list()
    for (v in economicas) {
      interacciones <- grep(paste0("^pais.*:", v, "$"), names(stats::coef(modelo_pool)),
                            value = TRUE)
      w <- prueba_wald(modelo_pool, V_pool, interacciones)
      filas_igualdad[[v]] <- data.frame(
        variable = v,
        estadistico_f = w$estadistico_f,
        grados_libertad = w$gl,
        grados_libertad_denominador = w$gl_denominador,
        p_valor = w$p_valor_f,
        conclusion = if (is.na(w$p_valor_f)) {
          "Sin veredicto: la prueba no se pudo calcular."
        } else if (w$p_valor_f < 0.05) {
          "Se rechaza la igualdad: la sensibilidad difiere entre países."
        } else {
          "No se rechaza la igualdad de coeficientes entre países."
        },
        stringsAsFactors = FALSE
      )
    }
    # Prueba conjunta de todas las variables económicas a la vez.
    todas <- unlist(lapply(economicas, function(v) {
      grep(paste0("^pais.*:", v, "$"), names(stats::coef(modelo_pool)), value = TRUE)
    }))
    w_total <- prueba_wald(modelo_pool, V_pool, todas)
    filas_igualdad[["conjunta"]] <- data.frame(
      variable = "todas las económicas en conjunto",
      estadistico_f = w_total$estadistico_f,
      grados_libertad = w_total$gl,
      grados_libertad_denominador = w_total$gl_denominador,
      p_valor = w_total$p_valor_f,
      conclusion = if (is.na(w_total$p_valor_f)) {
        "Sin veredicto."
      } else if (w_total$p_valor_f < 0.05) {
        "Se rechaza que las tres ecuaciones compartan las sensibilidades económicas."
      } else {
        "No se rechaza que las tres ecuaciones compartan las sensibilidades económicas."
      },
      stringsAsFactors = FALSE
    )
    igualdad <- do.call(rbind, filas_igualdad)
  }

  guardar_tabla(
    igualdad, "tabla_igualdad_coeficientes",
    "Igualdad de coeficientes económicos entre países (modelo agrupado con interacciones)",
    nota = paste0("H0: el coeficiente de la variable es el mismo en los tres ",
                  "países. Prueba de Wald con matriz de covarianzas ",
                  "Newey-West, informada en su versión F, que es la que no ",
                  "rechaza de más con muestras cortas. El modelo agrupado ",
                  "incluye solo las variables ",
                  "presentes en las tres ecuaciones (",
                  paste(comunes, collapse = ", "),
                  "); las variables exclusivas de un país no participan.")
  )

  # -------------------------------------------------------------------------
  # Endogeneidad de la variable fiscal: función de reacción
  # -------------------------------------------------------------------------
  # El coeficiente de la alícuota sale con signo positivo, al revés de lo que
  # predice el trabajo. Antes de leerlo como un efecto, hay que mostrar que
  # la alícuota no es independiente de lo que determina el precio de la
  # tierra: el Estado la sube cuando el precio internacional de la soja está
  # alto, que es cuando el campo vale más. Eso se documenta regresando la
  # alícuota contra el precio de la soja, contemporáneo y rezagado un año.
  # Si el precio del año anterior también la explica, el precio PRECEDE a la
  # alícuota y la simultaneidad queda a la vista.
  # Esta tabla NO corrige nada: deja escrito que el coeficiente de la
  # variable fiscal no admite lectura causal.
  fiscal <- if (identical(cfg$variable_fiscal, "pse")) {
    "pse_ocde_pct"
  } else {
    "alicuota_export_soja_pct"
  }
  precio_ref <- "ln_precio_soja_cbot"

  filas_reaccion <- list()
  for (pais in cfg$paises) {
    if (!fiscal %in% cfg$regresores[[pais]]) next
    datos <- panel[panel$pais == pais, ]
    datos <- datos[order(datos$anio), ]
    if (!precio_ref %in% names(datos)) next
    datos$precio_rezagado <- c(NA, utils::head(datos[[precio_ref]], -1))

    for (etiqueta in c("precio del mismo año", "precio del año anterior")) {
      explicativa <- if (etiqueta == "precio del mismo año") {
        precio_ref
      } else {
        "precio_rezagado"
      }
      usables <- datos[stats::complete.cases(datos[, c(fiscal, explicativa)]), ]
      if (nrow(usables) < 10) next
      m <- stats::lm(stats::reformulate(explicativa, fiscal), data = usables)
      tabla <- coeficientes_hac(m, cfg$rezago_hac(stats::nobs(m)))
      tabla <- tabla[tabla$variable == explicativa, ]
      filas_reaccion[[paste(pais, etiqueta)]] <- data.frame(
        pais = pais, variable_fiscal = fiscal, explicativa = etiqueta,
        n = stats::nobs(m), coeficiente = tabla$coeficiente,
        ee_hac = tabla$ee_hac, estadistico_t = tabla$estadistico_t,
        p_valor = tabla$p_valor, r2 = summary(m)$r.squared,
        conclusion = if (tabla$p_valor < 0.05) {
          paste0("El precio de la soja explica la variable fiscal: no es ",
                 "independiente de lo que determina el precio de la tierra.")
        } else {
          "No se detecta respuesta al precio de la soja."
        },
        stringsAsFactors = FALSE
      )
    }
  }

  reaccion <- if (length(filas_reaccion) == 0) {
    data.frame(
      pais = "ninguno", variable_fiscal = fiscal, explicativa = "no corrida",
      n = 0L, coeficiente = NA_real_, ee_hac = NA_real_,
      estadistico_t = NA_real_, p_valor = NA_real_, r2 = NA_real_,
      conclusion = paste0("Ningún país lleva la variable fiscal como ",
                          "regresor, o no hay años suficientes."),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, filas_reaccion)
  }

  guardar_tabla(
    reaccion, "tabla_endogeneidad_fiscal",
    "Función de reacción de la variable fiscal al precio de la soja",
    nota = paste0("Regresión auxiliar de ", fiscal, " contra ", precio_ref,
                  ", con errores Newey-West. No corrige la ecuación ",
                  "principal: documenta que la variable fiscal responde al ",
                  "mismo auge que empuja el precio de la tierra, así que su ",
                  "coeficiente en la ecuación por país mide asociación y no ",
                  "efecto causal.")
  )

  # -------------------------------------------------------------------------
  # Guardado para 05 y 06
  # -------------------------------------------------------------------------
  saveRDS(
    list(
      especificacion   = cfg$especificacion,
      modelos_niveles  = modelos_niveles,
      modelos_elegidos = modelos_elegidos,
      panel            = panel
    ),
    file.path(cfg$dir_resultados, "modelos.rds")
  )

  mensaje("Ecuaciones estimadas: ", paste(cfg$paises, collapse = ", "))
  for (pais in cfg$paises) {
    a <- ajuste[[pais]]
    mensaje("  ", pais, ": n = ", a$n, ", gl = ", a$grados_libertad,
            ", R2 ajustado = ", round(a$r2_ajustado, 3),
            if (nzchar(a$advertencia)) paste0(" | ", a$advertencia) else "")
  }
  if (!identical(cfg$especificacion, "niveles_MCO")) {
    mensaje("ATENCIÓN: la especificación elegida es '", cfg$especificacion,
            "'. La descomposición de Oaxaca usa igualmente la ecuación en ",
            "niveles, porque compara medias de la dependiente.")
  }
}
