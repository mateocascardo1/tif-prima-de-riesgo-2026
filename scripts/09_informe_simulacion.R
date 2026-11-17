# ---------------------------------------------------------------------------
# 09_informe_simulacion.R
# Informe HTML de la simulación de precios: qué precio habría dado cada
# ecuación en cada año y cuánto se aparta del precio observado.
#
# El informe muestra SOLO la simulación con el año adentro, que es la que
# evalúa la ecuación de los veintiún años en cada uno de esos años. La
# versión que deja el año afuera se sigue calculando en 08_simulacion.R y
# queda en tabla_simulacion.csv: el informe la menciona en una línea, porque
# el error que se muestra acá es el optimista y callarlo sería presentar
# como ajuste algo que en parte es memoria.
#
# No calcula nada: lee lo que dejó 08_simulacion.R y lo arma para leer. Los
# párrafos de lectura se construyen con los números de la corrida, así que
# si cambia un resultado cambia el texto.
#
# Salida:
#   03_resultados/informe_simulacion.html
# ---------------------------------------------------------------------------

if (!exists("cfg")) source("scripts/00_config.R")
source("scripts/utilidades.R")
source("scripts/html_comun.R")

mensaje("== 09_informe_simulacion: informe de la simulación ==")

detalle <- leer("tabla_simulacion")
resumen <- leer("tabla_simulacion_resumen")
hay_sim <- !is.null(resumen) && "error_medio_adentro_pct" %in% names(resumen)

hoy <- format(Sys.Date(), "%d/%m/%Y")
fig <- "figura_2_simulacion.png"
hay_fig <- file.exists(file.path(cfg$dir_resultados, fig))
ventana <- if (hay_sim) resumen$ventana[1] else
  sprintf("%d-%d", cfg$ventana_medias[1], cfg$ventana_medias[2])

# --- Tablas ---------------------------------------------------------------

filas_resumen <- character(0)
if (hay_sim) {
  filas_resumen <- vapply(seq_len(nrow(resumen)), function(i) fila(
    paste0("<b>", e(resumen$pais[i]), "</b>"),
    paste0("<b>", pct(resumen$error_medio_adentro_pct[i]), "</b>"),
    pct(resumen$error_ventana_adentro_pct[i]),
    resumen$peor_anio_adentro[i],
    pct(resumen$peor_error_adentro_pct[i], signo = TRUE)
  ), character(1))
}

# Tabla año por año, una por país. Los años de la ventana van en negrita,
# igual que en el informe principal.
bloques_anuales <- character(0)
if (!is.null(detalle) && "precio_simulado_adentro" %in% names(detalle)) {
  for (pais in cfg$paises) {
    d <- detalle[detalle$pais == pais, ]
    if (nrow(d) == 0) next
    d <- d[order(d$anio), ]
    filas_d <- vapply(seq_len(nrow(d)), function(i) {
      dentro_ventana <- d$anio[i] >= cfg$ventana_medias[1] &&
        d$anio[i] <= cfg$ventana_medias[2]
      resaltar <- function(x) if (dentro_ventana) paste0("<b>", x, "</b>") else x
      fila(
        resaltar(d$anio[i]),
        resaltar(miles(d$precio_observado[i])),
        miles(d$precio_simulado_adentro[i]),
        usd_con_signo(d$error_adentro_usd[i]),
        resaltar(pct(d$error_adentro_pct[i], signo = TRUE))
      )
    }, character(1))
    bloques_anuales <- c(bloques_anuales,
      paste0("<h3>", e(pais), "</h3>"),
      tabla(c("año", "precio observado", "precio simulado",
              "diferencia en USD", "diferencia en %"),
            filas_d, num_desde = 2))
  }
}

# --- Párrafos derivados de los números ------------------------------------

texto_lectura <- ""
texto_ventana <- ""
if (hay_sim) {
  por_pais <- function(f) unir(vapply(seq_len(nrow(resumen)), f, character(1)))

  texto_lectura <- paste0(
    "En un año típico la simulación se aparta del precio real ",
    por_pais(function(i) paste0("<b>", pct(resumen$error_medio_adentro_pct[i]),
                                "</b> en ", e(resumen$pais[i]))),
    ". La ecuación brasileña es la que peor sigue a su propia serie, y se ",
    "ve en la figura: el precio de Mato Grosso pega saltos que los ",
    "regresores no acompañan.")

  peor_ventana <- resumen$error_ventana_adentro_pct >
    resumen$error_medio_adentro_pct
  texto_ventana <- paste0(
    "En la ventana ", e(ventana), ", que es donde se responden las ",
    "hipótesis, la simulación se aparta ",
    por_pais(function(i) paste0(pct(resumen$error_ventana_adentro_pct[i]),
                                " en ", e(resumen$pais[i]))),
    ". ",
    if (all(peor_ventana)) {
      paste0("En los tres países el ajuste ahí es <b>peor</b> que en el ",
             "promedio de los años que entran en la ecuación, así que la ",
             "descomposición se apoya justo en los años que la ecuación peor ",
             "reproduce.")
    } else if (any(peor_ventana)) {
      paste0("El ajuste ahí es peor que en el promedio de los años que ",
             "entran en la ecuación en ", unir(e(resumen$pais[peor_ventana])),
             ", y mejor en ", unir(e(resumen$pais[!peor_ventana])), ".")
    } else {
      paste0("En los tres países el ajuste ahí es mejor que en el promedio ",
             "de los años que entran en la ecuación.")
    },
    " Eso no invalida la descomposición, pero es la razón por la que su ",
    "término residual da cero sobre el período completo y no da cero sobre ",
    "la ventana.")
}

# --- Armado del documento -------------------------------------------------

doc <- c(
encabezado_html(
  "Simulación de precios — brecha de la tierra agrícola",
  "Simulación de precios",
  paste0("Qué precio por hectárea habría dado cada ecuación en cada año, y ",
         "cuánto se aparta del precio que de verdad se pagó. Corrida del ",
         hoy, ".")),

callout("cinfo", "Qué es este ejercicio",
  paste0("Se toma la ecuación de cada país, se la alimenta con los ",
         "regresores observados de un año y se mira qué precio devuelve. No ",
         "se cambia ningún dato: es el mismo año real, con sus valores ",
         "reales. Lo que se mide es cuánto del precio la ecuación consigue ",
         "reproducir. <b>No es el ejercicio de convergencia</b> del informe ",
         "principal, donde sí se reemplazan las variables institucionales ",
         "argentinas por las brasileñas.")),
callout("cnota", "Qué variables entran",
  paste0("La ecuación que se simula es la del trabajo. En los tres países ",
         "entran el rendimiento, el precio de la soja, el tipo de cambio ",
         "real, la tasa real internacional, la tasa real local, las ventas ",
         "de maquinaria por hectárea, el diésel, la urea y la lluvia. ",
         "Argentina suma las retenciones, el riesgo país y la brecha ",
         "cambiaria. Brasil suma el riesgo país. El ",
         "crédito no entra.",
         "<br><br>En la figura los tres paneles usan la misma escala, de ",
         "2005 a 2025. El precio pagado de Iowa en 2005 y 2006 se dibuja. ",
         "La línea simulada no: no hay ventas de maquinaria y ese número no ",
         "se inventa. Lo mismo en Brasil 2020, donde falta el diésel de ",
         "septiembre: el precio pagado está, el simulado se corta.")),

callout("calerta", "Este error es el optimista, y conviene saberlo",
  paste0("Cada año que se simula participó antes del cálculo de los ",
         "coeficientes, así que la ecuación está describiendo datos que ya ",
         "vio. Eso mide <b>qué tan bien la ecuación resume el período</b>, ",
         "que es lo que el trabajo necesita, pero no mide qué tan bien le ",
         "pegaría a un año nuevo: ahí el error sería mayor. La versión que ",
         "estima la ecuación sin el año que después predice está calculada ",
         "en las columnas <code>_afuera</code> de ",
         "<code>tabla_simulacion.csv</code>.")),

if (!hay_sim) '<p class="vacio">La simulación no se calculó en esta corrida.</p>'
  else NULL,

if (hay_fig) paste0('<img src="', fig, '" alt="Precio observado y simulado">')
  else NULL,

'<h2>1. Cuánto se aparta la simulación del precio real</h2>',
'<p>La diferencia se mide en porcentaje del precio y sin importar el signo.',
' Un 7 % quiere decir que en un año típico la ecuación le pega a la hectárea',
' con una diferencia de siete centavos por cada dólar que vale.</p>',
tabla(c("país", "diferencia en un año típico",
        paste0("diferencia en ", e(ventana)), "peor año",
        "cuánto se apartó"),
      filas_resumen, num_desde = 2),
if (nzchar(texto_lectura)) callout("cnota", "Cómo se lee", texto_lectura)
  else NULL,

'<h2>2. El ajuste en la ventana de las hipótesis</h2>',
if (nzchar(texto_ventana)) paste0("<p>", texto_ventana, "</p>")
  else '<p class="vacio">Sin datos.</p>',

'<h2>3. Año por año</h2>',
paste0('<p>Precios en dólares de ', cfg$anio_base_precios, ' por hectárea. ',
       'La diferencia positiva quiere decir que la ecuación pide más de lo ',
       'que se pagó. En negrita, los años de la ventana ', e(ventana),
       '.</p>'),
bloques_anuales,

pie_html("09_informe_simulacion.R", c(
  'Los números completos, con todos los decimales, están en',
  ' <code>03_resultados/tabla_simulacion.csv</code> y',
  ' <code>tabla_simulacion_resumen.csv</code>.'))
)

ruta_html <- file.path(cfg$dir_resultados, "informe_simulacion.html")
writeLines(unlist(doc), ruta_html, useBytes = TRUE)
mensaje("Informe de simulación escrito: ", ruta_html)
