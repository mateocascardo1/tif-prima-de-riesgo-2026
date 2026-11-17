# ---------------------------------------------------------------------------
# 14_anexo_datos.R
# Cuadros del anexo: un país por cuadro, un año por fila, y solo las
# variables que entran en la ecuación de ese país.
#
# El número es el que usa la estimación (sale de 02_curados/panel.csv).
# Donde no hay dato la celda queda vacía: ese año no entra y no se rellena.
#
# Salida:
#   03_resultados/anexo_datos_regresion.html
#   03_resultados/anexo_datos_regresion.csv
# ---------------------------------------------------------------------------

if (!exists("cfg")) source("scripts/00_config.R")
source("scripts/utilidades.R")
source("scripts/html_comun.R")

mensaje("== 14_anexo_datos: cuadros del anexo ==")

ruta_panel <- file.path(cfg$dir_curados, "panel.csv")
if (!file.exists(ruta_panel)) {
  stop("No está el panel. Corré 01_curado.R desde la raíz.")
}
panel <- utils::read.csv(ruta_panel, stringsAsFactors = FALSE)

# Orden de lectura del cuadro: primero lo que se explica, después lo
# productivo, lo económico y lo institucional. Si 00_config suma un
# regresor y acá no hay rótulo, el script se detiene en vez de omitirlo.
rotulos <- list(
  ln_precio_ha_usd_2025 = list(
    nombre = "Precio de la tierra", unidad = "logaritmo, USD de 2025", dec = 3,
    que = "Lo que la ecuación explica: el logaritmo del precio por hectárea en dólares de 2025."),
  rendimiento_soja_t_ha = list(
    nombre = "Rendimiento de soja", unidad = "t/ha", dec = 3,
    que = "Toneladas cosechadas por hectárea en la región de referencia."),
  ln_ventas_maquinaria_por_ha = list(
    nombre = "Ventas de maquinaria", unidad = "logaritmo, ventas por ha", dec = 3,
    que = "Logaritmo de las ventas de tractores y cosechadoras por hectárea sembrada. No es el stock de máquinas."),
  diesel_usd_l = list(
    nombre = "Diésel", unidad = "USD por litro", dec = 3,
    que = "Precio minorista del gasoil en la región, con impuestos, en dólares por litro."),
  ln_urea = list(
    nombre = "Urea", unidad = "logaritmo, USD por t", dec = 3,
    que = "Logaritmo del precio internacional de la urea. Es el mismo número en los tres países."),
  precipitacion_desvio_mm = list(
    nombre = "Precipitación", unidad = "mm", dec = 1,
    serie = "precipitacion_mm",
    que = "Milímetros caídos en el año. En la ecuación entra la diferencia de este número contra el promedio 2005-2025 del mismo lugar. Esa diferencia se llama desvío de la precipitación."),
  ln_precio_soja_cbot = list(
    nombre = "Precio de la soja", unidad = "logaritmo, USD por t", dec = 3,
    que = "Logaritmo del precio de la soja en Chicago. Es el mismo número en los tres países."),
  tcr_bis_2005_2025_100 = list(
    nombre = "Tipo de cambio real", unidad = "índice, 2005-2025 = 100", dec = 2,
    que = "Índice del BIS. Un número más alto quiere decir que la moneda local está más cara en términos reales."),
  tips_10a_pct = list(
    nombre = "TIPS a 10 años", unidad = "% anual", dec = 2,
    que = "Tasa real de los bonos del Tesoro de Estados Unidos a 10 años. Es la misma en los tres países."),
  tasa_real_local_pct = list(
    nombre = "Tasa real local", unidad = "% anual", dec = 2,
    que = "Costo real del dinero en el país. Sale de la tasa nominal y de la inflación de cierre."),
  alicuota_export_soja_pct = list(
    nombre = "Retenciones", unidad = "%", dec = 2,
    que = "Alícuota de derechos de exportación de la soja."),
  brecha_cambiaria_pct = list(
    nombre = "Brecha cambiaria", unidad = "%", dec = 2,
    que = "Diferencia entre el dólar paralelo y el mayorista."),
  embi_pb = list(
    nombre = "EMBI", unidad = "puntos básicos", dec = 1,
    que = "Premio de riesgo soberano.")
)

orden <- names(rotulos)

# La ecuación usa el desvío. En el anexo se leen los milímetros caídos,
# que es el dato del que sale ese desvío.
serie_de <- function(v) {
  serie <- rotulos[[v]]$serie
  if (is.null(serie)) v else serie
}

region <- c(
  "Argentina" = "Zona núcleo",
  "Brasil" = "Mato Grosso",
  "Estados Unidos" = "Iowa"
)

columnas_de <- function(pais) {
  usadas <- c(cfg$dependiente, cfg$regresores[[pais]])
  sin_rotulo <- setdiff(usadas, orden)
  if (length(sin_rotulo) > 0) {
    stop("Falta el rótulo de: ", paste(sin_rotulo, collapse = ", "),
         ". Agregalo en 14_anexo_datos.R.")
  }
  faltan <- setdiff(c(usadas, vapply(usadas, serie_de, character(1))), names(panel))
  if (length(faltan) > 0) {
    stop("El panel no tiene: ", paste(faltan, collapse = ", "))
  }
  orden[orden %in% usadas]
}

juntar <- function(xs) {
  xs <- as.character(xs)
  n <- length(xs)
  if (n <= 1) return(xs)
  if (n == 2) return(paste(xs, collapse = " y "))
  paste0(paste(xs[-n], collapse = ", "), " y ", xs[n])
}

# Años que se caen de la ecuación, agrupados por el dato que les falta.
texto_huecos <- function(sub, columnas) {
  grupos <- list()
  for (i in seq_len(nrow(sub))) {
    vacias <- columnas[is.na(unlist(sub[i, columnas, drop = FALSE]))]
    if (length(vacias) == 0) next
    clave <- paste(vacias, collapse = "|")
    grupos[[clave]] <- c(grupos[[clave]], sub$anio[i])
  }
  if (length(grupos) == 0) return(NULL)
  frases <- vapply(names(grupos), function(clave) {
    anios <- grupos[[clave]]
    vars <- strsplit(clave, "|", fixed = TRUE)[[1]]
    nombres <- vapply(vars, function(v) rotulos[[v]]$nombre, character(1))
    verbo <- if (length(anios) == 1) "no entra" else "no entran"
    if (length(nombres) == 1) {
      falta <- paste0("no hay dato de ", nombres)
    } else if (length(nombres) == 2) {
      falta <- paste0("no hay dato de ", nombres[1], " ni de ", nombres[2])
    } else {
      falta <- paste0("no hay dato de ",
                      paste(nombres[-length(nombres)], collapse = ", de "),
                      " ni de ", nombres[length(nombres)])
    }
    paste0(juntar(anios), " ", verbo, ": ", falta, ".")
  }, character(1))
  paste(frases, collapse = " ")
}

donde_entra <- function(variable) {
  if (identical(variable, cfg$dependiente)) {
    return("En las tres ecuaciones.")
  }
  paises <- cfg$paises[vapply(cfg$paises, function(p) {
    variable %in% cfg$regresores[[p]]
  }, logical(1))]
  if (length(paises) == length(cfg$paises)) return("En las tres ecuaciones.")
  paste0("Solo en ", juntar(paises), ".")
}

ancla <- function(pais) chartr(" ", "-", tolower(pais))

cuadro_pais <- function(pais) {
  columnas <- columnas_de(pais)
  sub <- panel[panel$pais == pais, , drop = FALSE]
  sub <- sub[order(sub$anio), , drop = FALSE]
  esperados <- cfg$anio_min:cfg$anio_max
  if (!setequal(sub$anio, esperados)) {
    stop(pais, " no tiene los años ", cfg$anio_min, "-", cfg$anio_max, ".")
  }

  entra <- apply(sub[, columnas, drop = FALSE], 1, function(fila) !anyNA(fila))
  n_entra <- sum(entra)

  encabezados <- c(
    "<th>Año</th>",
    vapply(columnas, function(v) {
      rot <- rotulos[[v]]
      paste0("<th>", e(rot$nombre),
             "<span class='unidad'>", e(rot$unidad), "</span></th>")
    }, character(1))
  )

  filas <- lapply(seq_len(nrow(sub)), function(i) {
    celdas <- vapply(columnas, function(v) {
      valor <- sub[[serie_de(v)]][i]
      if (is.na(valor)) return("<td class='vacio'>—</td>")
      paste0("<td>", num(valor, rotulos[[v]]$dec), "</td>")
    }, character(1))
    clase <- if (entra[i]) "" else " class='fuera'"
    paste0("<tr", clase, "><td>", sub$anio[i], "</td>",
           paste(celdas, collapse = ""), "</tr>")
  })

  huecos <- texto_huecos(sub, columnas)
  nota_huecos <- if (is.null(huecos)) NULL else {
    paste0("<p>", e(huecos), " El hueco no se rellena.</p>")
  }
  marca <- if (any(!entra)) {
    paste0("<p class='meta'>La fila marcada es un año que no entra, ",
           "porque le falta un dato.</p>")
  } else NULL

  c(
    paste0("<h2 id='", ancla(pais), "'>", e(pais), "</h2>"),
    paste0("<p class='meta'>", e(region[[pais]]), " · entran ",
           n_entra, " de ", nrow(sub), " años.</p>"),
    marca,
    nota_huecos,
    "<div class='tabla-anexo'>",
    "<table>",
    paste0("<thead><tr>", paste(encabezados, collapse = ""), "</tr></thead>"),
    "<tbody>",
    filas,
    "</tbody></table></div>"
  )
}

# El csv guarda el valor completo, sin redondear. El html redondea para leer.
partes <- lapply(cfg$paises, function(pais) {
  columnas <- columnas_de(pais)
  series <- vapply(columnas, serie_de, character(1))
  sub <- panel[panel$pais == pais, c("pais", "anio", unique(series)), drop = FALSE]
  filas <- lapply(columnas, function(v) {
    data.frame(
      pais = sub$pais,
      anio = sub$anio,
      variable = serie_de(v),
      rotulo = rotulos[[v]]$nombre,
      valor = sub[[serie_de(v)]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
})
largo <- do.call(rbind, partes)
rownames(largo) <- NULL
ruta_csv <- file.path(cfg$dir_resultados, "anexo_datos_regresion.csv")
utils::write.csv(largo, ruta_csv, row.names = FALSE, fileEncoding = "UTF-8")

usadas_todas <- unique(c(cfg$dependiente, unlist(cfg$regresores, use.names = FALSE)))
glosario <- lapply(orden[orden %in% usadas_todas], function(v) {
  rot <- rotulos[[v]]
  paste0("<p><strong>", e(rot$nombre), ".</strong> ", e(rot$que), " ",
         e(donde_entra(v)), "</p>")
})

enlaces <- paste0(
  "<a href='#", ancla(cfg$paises), "'>", e(cfg$paises), "</a>",
  collapse = " · "
)

extra_css <- c(
".tabla-anexo{overflow-x:auto;margin:.2rem 0 1.4rem}",
".tabla-anexo table{margin:0;font-size:.8rem;width:max-content;min-width:100%}",
".tabla-anexo th,.tabla-anexo td{padding:.45rem .6rem}",
".tabla-anexo th{text-transform:none;letter-spacing:0;font-size:.74rem;",
"  color:var(--tinta);font-weight:600;white-space:normal;min-width:6.4rem;",
"  line-height:1.25;background:#fff}",
".tabla-anexo th:first-child,.tabla-anexo td:first-child{position:sticky;",
"  left:0;min-width:3.8rem;background:#fff;border-right:1px solid var(--linea)}",
".tabla-anexo th:first-child{z-index:2}",
".tabla-anexo td:first-child{z-index:1}",
".tabla-anexo td:nth-child(n+2){text-align:right;",
"  font-variant-numeric:tabular-nums;white-space:nowrap}",
".tabla-anexo th span.unidad{display:block;font-weight:400;color:var(--suave);",
"  font-size:.68rem;margin-top:.2rem}",
".tabla-anexo tr.fuera td{background:var(--ambarbg)}",
".tabla-anexo tbody tr:hover td{background:#fafafa}",
".tabla-anexo tr.fuera:hover td{background:#f4e7c3}",
"body{max-width:1440px}",
"a{color:inherit}"
)

doc <- c(
  encabezado_html(
    "Anexo. Datos de las ecuaciones",
    "Anexo. Datos de las ecuaciones",
    paste0("Un cuadro por país. Cada fila es un año, de ", cfg$anio_min,
           " a ", cfg$anio_max, ". Cada columna es una variable de la ",
           "ecuación de ese país, con el número que usa la estimación.")
  ),
  "<style>", extra_css, "</style>",
  paste0("<p class='meta'>", enlaces, "</p>"),
  callout(
    "cinfo",
    "Cómo se lee",
    paste0(
      "El precio de la tierra, el de la soja, la urea y las ventas de ",
      "maquinaria están en logaritmo, porque así entran en la ecuación. ",
      "La precipitación está en milímetros caídos. En la ecuación entra ",
      "la diferencia de ese número contra el promedio del lugar. ",
      "La soja, la urea y los TIPS se repiten igual en los tres cuadros: ",
      "son precios internacionales. Una celda vacía es un dato que no ",
      "existe. Ese año sale de la ecuación de ese país y no se completa. ",
      "El cuadro redondea para poder leerlo. El valor que entra en la ",
      "ecuación es el completo."
    )
  ),
  lapply(cfg$paises, cuadro_pais),
  "<h2>Qué mide cada columna</h2>",
  glosario,
  pie_html(
    "14_anexo_datos.R",
    paste0("Los números salen de <code>02_curados/panel.csv</code>. ",
           "Qué columna entra lo decide <code>scripts/00_config.R</code>. ",
           "El valor sin redondear está en ",
           "<code>03_resultados/anexo_datos_regresion.csv</code>.")
  )
)

ruta_html <- file.path(cfg$dir_resultados, "anexo_datos_regresion.html")
writeLines(unlist(doc), ruta_html, useBytes = TRUE)
mensaje("Anexo HTML escrito: ", ruta_html)
mensaje("Valores completos: ", ruta_csv)
