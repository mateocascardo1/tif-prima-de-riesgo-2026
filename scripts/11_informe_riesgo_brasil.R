# ---------------------------------------------------------------------------
# 11_informe_riesgo_brasil.R
# Informe HTML del precio argentino año por año, reemplazando solo el
# riesgo país por el de Brasil.
#
# No calcula nada: lee lo que dejó 10_riesgo_brasil.R y lo arma para leer.
# Los párrafos salen de los números de la corrida.
#
# Salida:
#   03_resultados/informe_riesgo_brasil.html
# ---------------------------------------------------------------------------

if (!exists("cfg")) source("scripts/00_config.R")
source("scripts/utilidades.R")
source("scripts/html_comun.R")

mensaje("== 11_informe_riesgo_brasil: informe del riesgo país de Brasil ==")

detalle <- leer("tabla_riesgo_brasil")
hay <- !is.null(detalle) &&
  "precio_simulado_riesgo_brasil" %in% names(detalle)
hoy <- format(Sys.Date(), "%d/%m/%Y")
fig <- "figura_3_riesgo_brasil.png"
hay_fig <- hay && file.exists(file.path(cfg$dir_resultados, fig))
ventana <- sprintf("%d-%d", cfg$ventana_medias[1], cfg$ventana_medias[2])
anio_fin <- cfg$anio_max

# --- Tabla año por año ----------------------------------------------------

filas_anuales <- character(0)
if (hay) {
  d <- detalle[order(detalle$anio), ]
  filas_anuales <- vapply(seq_len(nrow(d)), function(i) {
    dentro <- d$anio[i] >= cfg$ventana_medias[1] &&
      d$anio[i] <= cfg$ventana_medias[2]
    resaltar <- function(x) if (dentro) paste0("<b>", x, "</b>") else x
    marca <- if (!nzchar(d$extrapolacion[i])) {
      "—"
    } else if (d$extrapolacion[i] == "sí") {
      "<span class='chip ambar'>sí</span>"
    } else {
      e(d$extrapolacion[i])
    }
    fila(
      resaltar(d$anio[i]),
      resaltar(miles(d$precio_observado[i])),
      miles(d$precio_simulado_argentino[i]),
      resaltar(miles(d$precio_simulado_riesgo_brasil[i])),
      usd_con_signo(d$diferencia_usd[i]),
      resaltar(pct(d$diferencia_pct[i], signo = TRUE)),
      num(d$riesgo_pais_argentina[i], 1),
      num(d$riesgo_pais_brasil[i], 1),
      marca
    )
  }, character(1))
}

# --- Lectura derivada de los números --------------------------------------

texto_cierre <- ""
texto_extrapolacion <- ""
texto_vacios <- ""
if (hay) {
  d <- detalle[order(detalle$anio), ]
  fin <- d[d$anio == anio_fin, ]
  if (nrow(fin) == 1 && !is.na(fin$precio_simulado_riesgo_brasil)) {
    sentido <- if (fin$diferencia_usd >= 0) "por encima" else "por debajo"
    texto_cierre <- paste0(
      "En ", anio_fin, " la ecuación, alimentada con los datos argentinos, ",
      "devuelve <b>", miles(fin$precio_simulado_argentino),
      "</b> dólares de ", cfg$anio_base_precios, " por hectárea. Con el ",
      "riesgo país de Brasil de ese año devuelve <b>",
      miles(fin$precio_simulado_riesgo_brasil), "</b>: ",
      miles(abs(fin$diferencia_usd)), " dólares ", sentido,
      " del simulado argentino, un ", pct(fin$diferencia_pct, signo = TRUE),
      ". Esa diferencia se mide contra el precio de la ecuación, no contra ",
      "los ", miles(fin$precio_observado),
      " dólares que se pagaron: si se la midiera contra el precio pagado, ",
      "se mezclaría el cambio de riesgo con lo que la ecuación no llega a ",
      "reproducir."
    )
  }

  fuera <- d$anio[d$extrapolacion == "sí"]
  piso <- min(d$riesgo_pais_argentina, na.rm = TRUE)
  techo <- max(d$riesgo_pais_argentina, na.rm = TRUE)
  texto_extrapolacion <- if (length(fuera) == 0) {
    paste0("Ningún año quedó fuera del rango de riesgo país que Argentina ",
           "tuvo entre ", cfg$anio_min, " y ", cfg$anio_max,
           " (", num(piso, 0), " a ", num(techo, 0), " puntos básicos).")
  } else {
    paste0(
      "Entre ", cfg$anio_min, " y ", cfg$anio_max, " el riesgo país ",
      "argentino fue de ", num(piso, 0), " a ", num(techo, 0),
      " puntos básicos. En ", unir(fuera), " el de Brasil queda fuera de ",
      "ese rango, así que la ecuación está aplicada a un riesgo que ",
      "Argentina no tuvo. ",
      if (length(fuera) == 1) {
        "Ese año sigue en la tabla y está marcado."
      } else {
        "Esos años siguen en la tabla y están marcados."
      }
    )
  }

  vacios <- d$anio[is.na(d$riesgo_pais_brasil)]
  if (length(vacios) > 0) {
    texto_vacios <- paste0(
      "En ", unir(vacios), " Brasil no tiene riesgo país. Ese precio queda ",
      "vacío: no se completó el hueco."
    )
  }
}

fig4 <- "figura_4_institucional_brasil.png"
hay_fig4 <- hay &&
  "precio_simulado_institucional_brasil" %in% names(detalle) &&
  file.exists(file.path(cfg$dir_resultados, fig4))
texto_inst <- ""
if (hay_fig4) {
  d <- detalle[order(detalle$anio), ]
  fin <- d[d$anio == anio_fin, ]
  if (nrow(fin) == 1 && !is.na(fin$precio_simulado_institucional_brasil)) {
    brecha_lineas <- fin$precio_simulado_institucional_brasil -
      fin$precio_simulado_riesgo_brasil
    sentido <- if (brecha_lineas >= 0) "por encima" else "por debajo"
    suba <- d$precio_simulado_institucional_brasil - d$precio_simulado_riesgo_brasil
    i_max <- which.max(suba)
    texto_inst <- paste0(
      "La línea verde junta tres cambios. El riesgo país pasa al de Brasil. ",
      "Las retenciones salen, y esa quita se valúa con lo que la hectárea ya ",
      "responde a la soja, porque el coeficiente de la alícuota no se ",
      "distingue de cero: el productor pasa a cobrar el precio entero. La ",
      "brecha cambiaria se pone en cero. Es un factor institucional ",
      "argentino, y Brasil no tiene esa serie.",
      "<br><br>En ", anio_fin, " eso deja la hectárea en <b>",
      miles(fin$precio_simulado_institucional_brasil),
      "</b> dólares, ", miles(abs(brecha_lineas)), " dólares ", sentido,
      " de los ", miles(fin$precio_simulado_riesgo_brasil),
      " que salen de cambiar solo el riesgo, un ",
      pct(100 * brecha_lineas / fin$precio_simulado_riesgo_brasil, signo = TRUE),
      ". El año en que más se aleja de cambiar solo el riesgo es ",
      d$anio[i_max], ": ", miles(suba[i_max]),
      " dólares. Ese año la alícuota argentina era ",
      num(d$alicuota_argentina_pct[i_max], 1), " %",
      if ("brecha_argentina_pct" %in% names(d)) {
        paste0(" y la brecha ", num(d$brecha_argentina_pct[i_max], 1),
               " puntos")
      } else "",
      ".",
      "<br><br>No es una reestimación. El cero de las retenciones no se vio ",
      "en estos 21 años, y el cero de la brecha tampoco. El rendimiento, el ",
      "tipo de cambio, las tasas, las ventas de maquinaria, el diésel, la ",
      "urea y la lluvia siguen en el valor argentino."
    )
  }
}

recuadro <- paste0(
  "Solo se cambió el riesgo país: en cada año, el de Argentina se reemplazó ",
  "por el de Brasil. Quedaron en el valor argentino de ese año el ",
  "rendimiento, el precio de la soja, el tipo de cambio real, la tasa real ",
  "internacional, la tasa real local, las ventas de maquinaria por hectárea, ",
  "el diésel, la urea, la lluvia y la brecha cambiaria.",
  "<br><br>Las retenciones siguen siendo las argentinas. En esta base Brasil ",
  "no tiene alícuota de exportación de soja, y un cero habría sido un dato ",
  "inventado. La brecha cambiaria también queda en el valor argentino: en ",
  "la figura siguiente se la lleva a cero, junto con las retenciones. El ",
  "crédito no entra.",
  "<br><br>La serie termina en ", anio_fin,
  ", el último año del panel, y no dice nada de los años siguientes."
)

# --- Armado del documento -------------------------------------------------

doc <- c(
encabezado_html(
  "Riesgo país de Brasil — precio de la hectárea argentina",
  "La hectárea argentina con el riesgo país de Brasil",
  paste0("Qué precio por hectárea habría devuelto la ecuación argentina en ",
         "cada año si solo el riesgo país hubiera sido el de Brasil. ",
         "Corrida del ", hoy, ".")),

if (!hay) {
  c('<p class="vacio">Este cálculo no se hizo en esta corrida.</p>',
    if (!is.null(detalle) && "motivo" %in% names(detalle) && nrow(detalle) > 0) {
      paste0("<p>", e(detalle$motivo[1]), "</p>")
    } else NULL)
} else c(
  callout("cinfo", "Qué se cambió y qué no", recuadro),

  if (hay_fig) paste0('<img src="', fig,
                      '" alt="Precio argentino simulado reemplazando solo el riesgo país por el de Brasil">')
  else NULL,

  if (nzchar(texto_cierre)) callout("cnota", "Cómo se lee", texto_cierre)
  else NULL,

  '<h2>Año por año</h2>',
  paste0('<p>Precios en dólares de ', cfg$anio_base_precios,
         ' por hectárea. El riesgo país está en puntos básicos: es el premio ',
         'que el mercado pide por prestarle a ese país. La diferencia es el ',
         'precio con el riesgo de Brasil menos el precio simulado con los ',
         'datos argentinos. En negrita, los años de la ventana ',
         e(ventana), '.</p>'),
  paste0('<p>Extrapolación quiere decir que el riesgo de Brasil de ese año ',
         'está por fuera de los valores que Argentina tuvo entre ',
         cfg$anio_min, ' y ', cfg$anio_max,
         '. El año no se borra. El paso de la ecuación, que está en ',
         'logaritmos, a dólares usa la misma corrección que la simulación ',
         'ya publicada: un factor que multiplica por igual a las dos series ',
         'y no cambia el porcentaje entre ellas.</p>'),
  if (nzchar(texto_extrapolacion)) paste0("<p>", texto_extrapolacion, "</p>")
  else NULL,
  if (nzchar(texto_vacios)) paste0("<p>", texto_vacios, "</p>") else NULL,
  tabla(c("año", "precio observado", "simulado argentino",
          "con riesgo de Brasil", "diferencia en USD", "diferencia en %",
          "riesgo argentino", "riesgo de Brasil", "extrapolación"),
        filas_anuales, num_desde = 2),

  if (hay_fig4) c(
    '<h2>El bloque institucional entero</h2>',
    paste0('<img src="', fig4,
           '" alt="Precio argentino si el bloque institucional fuera el de Brasil">'),
    if (nzchar(texto_inst)) callout("cnota", "Qué agrega el escenario institucional",
                                    texto_inst) else NULL
  ) else NULL
),

pie_html("11_informe_riesgo_brasil.R",
  'Los números completos, con todos los decimales, están en <code>03_resultados/tabla_riesgo_brasil.csv</code>.')
)

ruta_html <- file.path(cfg$dir_resultados, "informe_riesgo_brasil.html")
writeLines(unlist(doc), ruta_html, useBytes = TRUE)
mensaje("Informe de riesgo país escrito: ", ruta_html)
