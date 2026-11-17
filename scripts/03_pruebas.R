# ---------------------------------------------------------------------------
# 03_pruebas.R
# Pruebas de raíz unitaria (ADF y KPSS) por serie y por país, y resumen del
# orden de integración.
#
# Las dos pruebas se usan juntas porque se preguntan cosas opuestas:
#   ADF  -> la hipótesis nula es que la serie TIENE raíz unitaria.
#           Un p-valor chico (< 0,05) dice que la serie es estacionaria.
#   KPSS -> la hipótesis nula es que la serie ES estacionaria.
#           Un p-valor grande (> 0,05) deja esa hipótesis en pie.
# Cuando las dos coinciden, la conclusión es firme. Cuando no, se informa
# "ambiguo" en lugar de elegir la que convenga.
#
# Con 21 observaciones estas pruebas tienen poca potencia. Eso no se oculta:
# va como advertencia en la tabla y en el informe.
#
# Salidas: 03_resultados/tabla_raices_unitarias.csv (y .html)
#          03_resultados/especificacion_sugerida.csv (y .html)
# ---------------------------------------------------------------------------

if (!exists("cfg")) source("scripts/00_config.R")
source("scripts/utilidades.R")

mensaje("== 03_pruebas: raíces unitarias ==")

if (!requireNamespace("tseries", quietly = TRUE)) {
  stop("Falta el paquete 'tseries'. Instalarlo con renv::install(\"tseries\").",
       call. = FALSE)
}

panel <- utils::read.csv(file.path(cfg$dir_curados, "panel.csv"),
                         stringsAsFactors = FALSE)
diccionario <- utils::read.csv(file.path(cfg$dir_curados, "diccionario.csv"),
                               stringsAsFactors = FALSE)

# Series que interesan: la dependiente, todos los regresores de las tres
# ecuaciones y las series descriptivas que igual conviene caracterizar.
series <- unique(c(
  cfg$dependiente,
  unlist(cfg$regresores, use.names = FALSE),
  "precio_ha_usd_corriente", "rendimiento_soja_t_ha",
  "tcr_bis_2005_2025_100", "embi_pb", "alicuota_export_soja_pct",
  "precipitacion_desvio_mm", "maquinaria_vendida_unidades",
  "credito_agro_musd", "inflacion_dic_pct", "tasa_nominal_local_pct"
))

# Cantidad mínima de años para que la prueba tenga algún sentido.
MINIMO_OBS <- 10

# Corre ADF y KPSS sobre un vector, devolviendo también si el p-valor quedó
# fuera de la tabla de valores críticos (eso pasa seguido con n chico).
probar <- function(x) {
  resultado <- list(adf_stat = NA_real_, adf_p = NA_real_,
                    kpss_stat = NA_real_, kpss_p = NA_real_,
                    aviso = "")
  avisos <- character(0)

  adf <- withCallingHandlers(
    try(tseries::adf.test(x), silent = TRUE),
    warning = function(w) {
      avisos <<- c(avisos, "p-valor del ADF fuera de la tabla")
      invokeRestart("muffleWarning")
    }
  )
  if (!inherits(adf, "try-error")) {
    resultado$adf_stat <- unname(adf$statistic)
    resultado$adf_p    <- adf$p.value
  }

  kpss <- withCallingHandlers(
    try(tseries::kpss.test(x, null = "Level"), silent = TRUE),
    warning = function(w) {
      avisos <<- c(avisos, "p-valor del KPSS fuera de la tabla")
      invokeRestart("muffleWarning")
    }
  )
  if (!inherits(kpss, "try-error")) {
    resultado$kpss_stat <- unname(kpss$statistic)
    resultado$kpss_p    <- kpss$p.value
  }

  resultado$aviso <- paste(unique(avisos), collapse = "; ")
  resultado
}

# Combina las dos pruebas en un veredicto.
veredicto <- function(adf_p, kpss_p) {
  if (is.na(adf_p) || is.na(kpss_p)) return("sin veredicto")
  estacionaria_adf  <- adf_p < 0.05    # se rechaza la raíz unitaria
  estacionaria_kpss <- kpss_p > 0.05   # no se rechaza la estacionariedad
  if (estacionaria_adf && estacionaria_kpss) return("estacionaria")
  if (!estacionaria_adf && !estacionaria_kpss) return("no estacionaria")
  "ambiguo"
}

filas <- list()
for (p in cfg$paises) {
  datos <- panel[panel$pais == p, ]
  datos <- datos[order(datos$anio), ]

  for (v in series) {
    x <- datos[[v]]
    completos <- sum(!is.na(x))

    # Casos en los que no se puede probar nada. Se informa el motivo en vez
    # de dejar la fila en blanco.
    if (completos == 0) {
      motivo <- diccionario$estado[match(v, diccionario$variable)]
      filas[[length(filas) + 1]] <- data.frame(
        pais = p, variable = v, n = 0L,
        adf_estadistico = NA_real_, adf_p = NA_real_,
        kpss_estadistico = NA_real_, kpss_p = NA_real_,
        nivel = "no estimada", primera_diferencia = "no estimada",
        orden_integracion = "no estimada",
        observacion = paste0("La serie no existe en data/. ",
                             ifelse(is.na(motivo), "", motivo)),
        stringsAsFactors = FALSE
      )
      next
    }
    if (anyNA(x) || completos < MINIMO_OBS) {
      filas[[length(filas) + 1]] <- data.frame(
        pais = p, variable = v, n = completos,
        adf_estadistico = NA_real_, adf_p = NA_real_,
        kpss_estadistico = NA_real_, kpss_p = NA_real_,
        nivel = "no estimada", primera_diferencia = "no estimada",
        orden_integracion = "no estimada",
        observacion = paste0("La serie tiene huecos dentro del período (",
                             length(cfg$anio_min:cfg$anio_max) - completos,
                             " año(s) sin dato). Las pruebas necesitan una ",
                             "serie continua; no se completan los huecos."),
        stringsAsFactors = FALSE
      )
      next
    }

    en_nivel <- probar(x)
    en_dif   <- probar(diff(x))
    v_nivel  <- veredicto(en_nivel$adf_p, en_nivel$kpss_p)
    v_dif    <- veredicto(en_dif$adf_p, en_dif$kpss_p)

    orden <- if (v_nivel == "estacionaria") {
      "I(0)"
    } else if (v_nivel == "no estacionaria" && v_dif == "estacionaria") {
      "I(1)"
    } else if (v_nivel == "no estacionaria" && v_dif == "no estacionaria") {
      "I(2) o superior: revisar"
    } else {
      "ambiguo"
    }

    avisos <- paste(c(en_nivel$aviso, en_dif$aviso)[nzchar(c(en_nivel$aviso, en_dif$aviso))],
                    collapse = "; ")
    filas[[length(filas) + 1]] <- data.frame(
      pais = p, variable = v, n = completos,
      adf_estadistico = en_nivel$adf_stat, adf_p = en_nivel$adf_p,
      kpss_estadistico = en_nivel$kpss_stat, kpss_p = en_nivel$kpss_p,
      nivel = v_nivel, primera_diferencia = v_dif,
      orden_integracion = orden,
      observacion = avisos,
      stringsAsFactors = FALSE
    )
  }
}

tabla <- do.call(rbind, filas)
guardar_tabla(
  tabla, "tabla_raices_unitarias",
  "Pruebas de raíz unitaria (ADF y KPSS) por país y serie",
  nota = paste0("ADF: la nula es que hay raíz unitaria; KPSS: la nula es que ",
                "la serie es estacionaria. Las columnas ADF y KPSS son en ",
                "nivel. Con 21 observaciones las dos pruebas tienen poca ",
                "potencia: el veredicto es orientativo. Las filas 'no ",
                "estimada' corresponden a series ausentes o con huecos.")
)

# ---------------------------------------------------------------------------
# Especificación sugerida
# ---------------------------------------------------------------------------
# La regla, escrita antes de ver los resultados:
#   series I(0)                      -> "niveles_MCO"
#   series I(1) con cointegración    -> "ARDL"
#   series I(1) sin cointegración    -> "diferencias"
#
# La prueba de cointegración (bounds test del ARDL) solo tiene sentido si
# existe la variable dependiente. Hoy no existe, así que no se corre ni se
# afirma nada sobre cointegración.
hay_dependiente <- any(!is.na(panel[[cfg$dependiente]]))
probadas <- tabla[tabla$orden_integracion %in% c("I(0)", "I(1)", "ambiguo"), ]

if (!hay_dependiente) {
  sugerida <- "sin determinar"
  motivo <- paste0(
    "No se determina la especificación: la variable dependiente (",
    cfg$dependiente, ") está vacía porque falta el CPI-U de EE. UU. ",
    "Tampoco se corre el bounds test de cointegración, porque no hay ",
    "ecuación que cointegrar. Lo que sí se probó son los regresores ",
    "disponibles, y sus resultados están en tabla_raices_unitarias."
  )
} else {
  proporcion_i1 <- mean(probadas$orden_integracion == "I(1)")
  if (proporcion_i1 >= 0.5) {
    sugerida <- "ARDL si el bounds test da cointegración; diferencias si no"
    motivo <- paste0("La mayoría de las series probadas resultó I(1) (",
                     round(proporcion_i1 * 100), "%).")
  } else {
    sugerida <- "niveles_MCO"
    motivo <- paste0("La mayoría de las series probadas resultó I(0) o ",
                     "ambigua; no hay evidencia suficiente para abandonar los ",
                     "niveles.")
  }
}

especificacion <- data.frame(
  especificacion_en_config = cfg$especificacion,
  especificacion_sugerida  = sugerida,
  series_probadas          = nrow(probadas),
  series_no_estimadas      = sum(tabla$orden_integracion == "no estimada"),
  motivo                   = motivo,
  stringsAsFactors = FALSE
)
guardar_tabla(especificacion, "especificacion_sugerida",
              "Especificación sugerida por las pruebas de raíz unitaria")

mensaje("Tabla de raíces unitarias escrita (", nrow(tabla), " filas; ",
        sum(tabla$orden_integracion == "no estimada"), " no estimadas).")
mensaje("Especificación en 00_config.R: ", cfg$especificacion)
mensaje("Especificación sugerida: ", sugerida)
if (!hay_dependiente) {
  mensaje("ATENCIÓN: no se corrió ninguna prueba de cointegración. ", motivo)
}
