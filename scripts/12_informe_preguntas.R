# ---------------------------------------------------------------------------
# 12_informe_preguntas.R
# Un informe por las cuatro preguntas del trabajo. No reestima la ecuación
# principal: lee los coeficientes, la descomposición y la simulación
# institucional, y arma el texto con esos números.
#
# El atraso sí se calcula acá, como una mirada aparte: la misma ecuación más
# el precio del año anterior. No reemplaza a la ecuación del trabajo.
#
# Salida:
#   03_resultados/informe_preguntas.html
# ---------------------------------------------------------------------------

if (!exists("cfg")) source("scripts/00_config.R")
source("scripts/utilidades.R")
source("scripts/html_comun.R")

mensaje("== 12_informe_preguntas: las cuatro preguntas ==")

panel <- utils::read.csv(file.path(cfg$dir_curados, "panel.csv"),
                         stringsAsFactors = FALSE)
coeficientes <- leer("tabla_coeficientes")
capital      <- leer("tabla_capitalizacion")
igualdad     <- leer("tabla_igualdad_coeficientes")
riesgo       <- leer("tabla_riesgo_brasil")
h2           <- leer("tabla_h2")

if (is.null(coeficientes)) {
  stop("No está tabla_coeficientes.csv. Correr antes 04_estimacion.R.",
       call. = FALSE)
}

ventana <- paste(cfg$ventana_medias, collapse = "-")

usd <- function(x) paste0(miles(x), " dólares")

# Un resultado es representativo cuando se distingue del azar al 5 %.
representa <- function(p) !is.na(p) && p < 0.05

chip_repr <- function(p) {
  if (representa(p)) "<span class='chip verde'>sí</span>"
  else "<span class='chip gris'>no</span>"
}

efecto_pct <- function(beta, delta) (exp(beta * delta) - 1) * 100

# "un 3 % más cara" o "un 3 % más barata". El signo ya está en las palabras.
en_palabras <- function(pct, dec = 1) {
  if (length(pct) != 1 || is.na(pct)) return("sin un número usable")
  if (abs(pct) < 0.05) return("casi quieta")
  lado <- if (pct > 0) "más cara" else "más barata"
  paste0("un ", num(abs(pct), dec), " % ", lado)
}

coef_de <- function(pais, variable) {
  f <- coeficientes[coeficientes$pais == pais &
                      coeficientes$variable == variable, ]
  if (nrow(f) == 0) NULL else f[1, ]
}

# --- Precios de la ventana ------------------------------------------------

media_precio <- function(pais) {
  d <- panel[panel$pais == pais & en_ventana(panel$anio), "precio_ha_usd_2025"]
  mean(d, na.rm = TRUE)
}
precio_ar <- media_precio("Argentina")
precio_br <- media_precio("Brasil")
precio_us <- media_precio("Estados Unidos")

# --- 1. La brecha tomada como 100 % ---------------------------------------
# Cada línea es un factor. Entra en la suma del grupo solo cuando explica
# por qué la otra hectárea es más cara. Un factor con el mismo valor en los
# dos países no explica el nivel. Una línea cuyo coeficiente sale al revés
# no se presenta como parte de la brecha.
#
# Las retenciones son la excepción. El coeficiente de la alícuota no se
# distingue de cero, y Brasil e Iowa no tienen la serie. La comparación los
# pone en 0 y valúa el salto con el coeficiente de la soja: el productor
# pasa a quedarse con el precio entero. Es el mismo supuesto del escenario
# institucional, no el componente de Oaxaca de la alícuota.

detalle_brecha <- leer("tabla_descomposicion_por_variable")

NOMBRE_FACTOR <- c(
  embi_pb                     = "Riesgo país",
  brecha_cambiaria_pct        = "Brecha cambiaria",
  alicuota_export_soja_pct    = "Retenciones",
  tasa_real_local_pct         = "Tasa real local",
  tcr_bis_2005_2025_100       = "Tipo de cambio real",
  ln_precio_soja_cbot         = "Precio de la soja",
  tips_10a_pct                = "Tasa real internacional",
  rendimiento_soja_t_ha       = "Rendimiento",
  diesel_usd_l                = "Diésel",
  precipitacion_desvio_mm     = "Lluvia",
  ln_urea                     = "Urea",
  ln_ventas_maquinaria_por_ha = "Ventas de maquinaria"
)

ORDEN_GRUPO <- list(
  institucional = c("embi_pb", "brecha_cambiaria_pct",
                    "alicuota_export_soja_pct"),
  economico     = c("tasa_real_local_pct", "tcr_bis_2005_2025_100",
                    "ln_precio_soja_cbot", "tips_10a_pct"),
  productivo    = c("rendimiento_soja_t_ha", "diesel_usd_l",
                    "precipitacion_desvio_mm", "ln_urea",
                    "ln_ventas_maquinaria_por_ha")
)

TITULO_GRUPO <- c(
  institucional = "Institucionales",
  economico     = "Económicos",
  productivo    = "Productivos"
)

tomar_factor <- function(comparable, variable) {
  if (is.null(detalle_brecha)) return(NULL)
  f <- detalle_brecha[detalle_brecha$rol == "principal" &
                        detalle_brecha$comparable == comparable &
                        detalle_brecha$variable == variable, ]
  if (nrow(f) == 0) NULL else f[1, ]
}

explica_brecha <- function(f) {
  !is.null(f) && is.finite(f$porcentaje_de_la_brecha) &&
    f$porcentaje_de_la_brecha > 0 &&
    abs(f$contribucion_niveles_log) >= 1e-6
}

# Parte de la brecha que son las retenciones, con el otro país en 0.
# La brecha en logaritmos sale de la fila de Oaxaca (contribución / parte).
# El aporte es el coeficiente argentino de la soja por la fracción del
# precio que la alícuota le saca al productor.
parte_retenciones <- function(comparable) {
  f <- tomar_factor(comparable, "alicuota_export_soja_pct")
  soja <- coef_de("Argentina", "ln_precio_soja_cbot")
  if (is.null(f) || is.null(soja)) return(NULL)
  if (!is.finite(f$porcentaje_de_la_brecha) || f$porcentaje_de_la_brecha == 0) {
    return(NULL)
  }
  brecha <- f$contribucion_niveles_log / (f$porcentaje_de_la_brecha / 100)
  tau <- f$media_argentina / 100
  if (!is.finite(tau) || tau <= 0 || tau >= 1) return(NULL)
  if (!is.finite(brecha) || brecha <= 0) return(NULL)
  if (!is.finite(soja$coeficiente)) return(NULL)
  list(
    tau = tau,
    share = soja$coeficiente * (-log(1 - tau)) / brecha * 100,
    p_soja = soja$p_valor
  )
}

mismo_nivel <- function(f) abs(f$contribucion_niveles_log) < 1e-6

parte_factor <- function(f, variable, comparable) {
  if (variable == "alicuota_export_soja_pct") {
    r <- parte_retenciones(comparable)
    if (is.null(r)) return("no se usa")
    return(pc(r$share, 1))
  }
  if (is.null(f) || mismo_nivel(f)) return("no explica el nivel")
  if (f$porcentaje_de_la_brecha >= 1) return(pc(f$porcentaje_de_la_brecha, 1))
  if (f$porcentaje_de_la_brecha > 0) return("menos del 1 %")
  "no se usa"
}

nivel_factor <- function(variable, f, lugar) {
  a <- f$media_argentina
  b <- f$media_comparable
  if (mismo_nivel(f)) return("el mismo valor en los dos")
  if (variable == "embi_pb") {
    if (b == 0) {
      return(paste0(miles(a), " puntos en Argentina. ", lugar,
                    " no tiene la serie y la cuenta pone 0"))
    }
    return(paste0(miles(a), " puntos contra ", miles(b)))
  }
  if (variable == "alicuota_export_soja_pct") {
    return(paste0(num(a, 0), " % contra 0 %"))
  }
  if (variable == "brecha_cambiaria_pct") {
    return(paste0(num(a, 0), " puntos en Argentina. ", lugar,
                  " no tiene la serie y la cuenta pone 0"))
  }
  if (variable == "tasa_real_local_pct") {
    return(paste0(pc(a, 0), " contra ", pc(b, 0)))
  }
  if (variable == "tcr_bis_2005_2025_100") {
    return(paste0("índice ", num(a, 0), " contra ", num(b, 0)))
  }
  if (variable == "rendimiento_soja_t_ha") {
    return(paste0(num(a, 1), " t/ha contra ", num(b, 1)))
  }
  if (variable == "diesel_usd_l") {
    return(paste0(num(a, 2), " USD/l contra ", num(b, 2)))
  }
  if (variable == "precipitacion_desvio_mm") {
    return(paste0("desvío de ", num(a, 0), " mm contra ", num(b, 0), " mm"))
  }
  if (variable == "ln_ventas_maquinaria_por_ha") {
    quien <- if (b > a) lugar else "La zona núcleo"
    return(paste0(quien,
                  " vende más por hectárea. El coeficiente sale al revés, ",
                  "así que esta línea no se lee como parte de la brecha"))
  }
  ""
}

suma_grupo <- function(comparable, variables) {
  s <- 0
  for (v in variables) {
    if (v == "alicuota_export_soja_pct") {
      r <- parte_retenciones(comparable)
      if (!is.null(r)) s <- s + r$share
      next
    }
    f <- tomar_factor(comparable, v)
    if (explica_brecha(f)) s <- s + f$porcentaje_de_la_brecha
  }
  s
}

bloque_brecha <- function(comparable) {
  lugar <- if (comparable == "Brasil") "Mato Grosso" else "Iowa"
  precio <- if (comparable == "Brasil") precio_br else precio_us
  dolares <- precio - precio_ar
  mas <- 100 * (precio / precio_ar - 1)
  sumas <- vapply(names(ORDEN_GRUPO), function(g) {
    suma_grupo(comparable, ORDEN_GRUPO[[g]])
  }, numeric(1))
  total <- sum(sumas)

  filas <- character(0)
  for (g in names(ORDEN_GRUPO)) {
    filas <- c(filas, fila(
      paste0("<b>", TITULO_GRUPO[[g]], "</b>"),
      "",
      paste0("<b>", pc(sumas[[g]], 1), "</b>")
    ))
    for (v in ORDEN_GRUPO[[g]]) {
      f <- tomar_factor(comparable, v)
      if (is.null(f)) next
      filas <- c(filas, fila(
        paste0("&nbsp;&nbsp;", e(NOMBRE_FACTOR[[v]])),
        e(nivel_factor(v, f, lugar)),
        parte_factor(f, v, comparable)
      ))
    }
  }
  filas <- c(filas, fila(
    "<b>Juntos</b>",
    "de la diferencia, tomada como 100 %",
    paste0("<b>", pc(total, 1), "</b>")
  ))

  texto <- paste0(
    "La hectárea de ", lugar, " sale <b>", usd(dolares),
    "</b> más cara que la de la zona núcleo, un ", pc(mas, 0),
    ". Esa diferencia es el 100 %.",
    "<br><br>De ese 100 %, los institucionales explican el ",
    pc(sumas[["institucional"]], 1),
    ", los económicos el ", pc(sumas[["economico"]], 1),
    " y los productivos el ", pc(sumas[["productivo"]], 1),
    ". Juntos, el ", pc(total, 1), "."
  )
  list(lugar = lugar, texto = texto, filas = filas, total = total)
}

brecha_br <- if (!is.null(detalle_brecha)) bloque_brecha("Brasil") else NULL
brecha_us <- if (!is.null(detalle_brecha)) bloque_brecha("Estados Unidos") else NULL

# --- 2. Rendimiento y maquinaria ------------------------------------------

MEDIA_TONELADA <- 0.5
SUBA_MAQ <- log(1.10)

filas_rend <- character(0)
filas_maq <- character(0)
for (pais in cfg$paises) {
  ry <- coef_de(pais, "rendimiento_soja_t_ha")
  mq <- coef_de(pais, "ln_ventas_maquinaria_por_ha")
  filas_rend <- c(filas_rend, fila(
    paste0("<b>", e(pais), "</b>"),
    pc(efecto_pct(ry$coeficiente, MEDIA_TONELADA), signo = TRUE),
    chip_repr(ry$p_valor)
  ))
  filas_maq <- c(filas_maq, fila(
    paste0("<b>", e(pais), "</b>"),
    pc(efecto_pct(mq$coeficiente, SUBA_MAQ), signo = TRUE),
    chip_repr(mq$p_valor)
  ))
}

# --- 3. Soja, tipo de cambio y atraso -------------------------------------

DEL_TRABAJO <- "ecuación del trabajo"
SUBA <- cfg$suba_referencia_pct
filas_soja <- character(0)
if (!is.null(capital)) {
  cap <- capital[capital$especificacion == DEL_TRABAJO, ]
  for (i in seq_len(nrow(cap))) {
    filas_soja <- c(filas_soja, fila(
      paste0("<b>", e(cap$pais[i]), "</b>"),
      paste0("<b>", pc(cap$suba_tierra_pct[i]), "</b>"),
      paste0(pc(cap$suba_tierra_pct_ic_inferior[i], signo = TRUE), " a ",
             pc(cap$suba_tierra_pct_ic_superior[i], signo = TRUE)),
      chip_repr(cap$p_igual_cero[i]),
      chip_repr(cap$p_igual_uno[i])
    ))
  }
}

# Diez puntos del índice. El promedio 2005-2025 vale 100, y un aumento es
# una moneda local más fuerte en términos reales.
PUNTOS_TCR <- 10
filas_tcr <- character(0)
for (pais in cfg$paises) {
  tc <- coef_de(pais, "tcr_bis_2005_2025_100")
  filas_tcr <- c(filas_tcr, fila(
    paste0("<b>", e(pais), "</b>"),
    pc(efecto_pct(tc$coeficiente, PUNTOS_TCR), signo = TRUE),
    chip_repr(tc$p_valor)
  ))
}

p_igualdad <- function(variable) {
  if (is.null(igualdad)) return(NA_real_)
  f <- igualdad[igualdad$variable == variable, ]
  if (nrow(f) == 0) NA_real_ else f$p_valor[1]
}

# La misma ecuación, más el precio del año anterior. Mide cuánto de un
# movimiento queda para el año siguiente. No es la ecuación del trabajo.
atraso_pais <- function(pais) {
  regs <- cfg$regresores[[pais]]
  d <- panel[panel$pais == pais, c("anio", cfg$dependiente, regs), drop = FALSE]
  d <- d[order(d$anio), ]
  d$L <- c(NA, utils::head(d[[cfg$dependiente]], -1))
  salto <- c(FALSE, diff(d$anio) != 1)
  d$L[salto] <- NA
  d <- d[stats::complete.cases(d), ]
  modelo <- stats::lm(stats::reformulate(c("L", regs), cfg$dependiente), data = d)
  tab <- coeficientes_hac(modelo, cfg$rezago_hac(stats::nobs(modelo)))
  rho <- tab$coeficiente[tab$variable == "L"]
  list(
    n = nrow(d),
    rho = rho,
    p = tab$p_valor[tab$variable == "L"],
    vida = if (is.finite(rho) && rho > 0 && rho < 1) log(0.5) / log(rho) else NA_real_,
    soja = tab[tab$variable == "ln_precio_soja_cbot", ],
    tcr = tab[tab$variable == "tcr_bis_2005_2025_100", ]
  )
}

atrasos <- lapply(stats::setNames(cfg$paises, cfg$paises), atraso_pais)

# Acompañamiento de los cambios, el mismo año y dos años después.
# 1: se mueven igual. 0: no se acompañan.
acompanan <- function(pais, variable, rezago) {
  d <- panel[panel$pais == pais, c("anio", cfg$dependiente, variable)]
  d <- d[order(d$anio), ]
  d <- d[stats::complete.cases(d), ]
  dy <- diff(d[[cfg$dependiente]])
  dx <- diff(d[[variable]])
  if (rezago == 0) {
    stats::cor(dy, dx)
  } else {
    n <- length(dy)
    stats::cor(dy[-(seq_len(rezago))], dx[seq_len(n - rezago)])
  }
}

# --- 4. Instituciones de Brasil en la ecuación argentina ------------------

filas_inst <- character(0)
if (!is.null(riesgo) && nrow(riesgo) > 0) {
  for (i in seq_len(nrow(riesgo))) {
    anio <- riesgo$anio[i]
    marca <- if (en_ventana(anio)) paste0("<b>", anio, "</b>") else anio
    filas_inst <- c(filas_inst, fila(
      marca,
      miles(riesgo$precio_observado[i]),
      miles(riesgo$precio_simulado_argentino[i]),
      miles(riesgo$precio_simulado_riesgo_brasil[i]),
      miles(riesgo$precio_simulado_institucional_brasil[i]),
      pc(riesgo$diferencia_institucional_pct[i], signo = TRUE)
    ))
  }
}

h2_pr <- if (!is.null(h2)) h2[h2$rol == "principal", ] else NULL

# --- Textos ---------------------------------------------------------------

ar_at <- atrasos[["Argentina"]]
br_at <- atrasos[["Brasil"]]
us_at <- atrasos[["Estados Unidos"]]

# Fracción de una suba de la soja que se queda. El año 0 es el de la suba.
# La mitad y el 90 % salen de la memoria del precio: cuánto de un movimiento
# sigue presente al año siguiente. No es la ecuación del trabajo.
plazo_soja <- function(at) {
  vacio <- list(mismo = "no se puede afirmar", mitad = "—", casi = "—")
  if (is.null(at) || !representa(at$p)) return(vacio)
  if (!is.finite(at$rho) || at$rho <= 0 || at$rho >= 1) return(vacio)
  en_anios <- function(k) {
    if (k <= 0) return("el mismo año")
    if (k < 1.5) return("al año siguiente")
    paste0("a los ", num(round(k), 0), " años")
  }
  list(
    mismo = paste0(pc((1 - at$rho) * 100, 0), " de la suba"),
    mitad = en_anios(log(0.5) / log(at$rho) - 1),
    casi = en_anios(log(0.1) / log(at$rho) - 1)
  )
}

filas_plazo <- character(0)
for (pais in cfg$paises) {
  pl <- plazo_soja(atrasos[[pais]])
  filas_plazo <- c(filas_plazo, fila(
    paste0("<b>", e(pais), "</b>"), pl$mismo, pl$mitad, pl$casi
  ))
}

texto_atraso <- paste0(
  "Esto no reemplaza la ecuación del trabajo. Se le agrega el precio del ",
  "año anterior para ver cuánto de un movimiento queda para el año ",
  "siguiente. En Brasil queda el ", pc(br_at$rho * 100, 0),
  " y eso es representativo: un golpe tarda ",
  num(br_at$vida, 1), " años en perder la mitad de su fuerza. En Iowa queda ",
  "el ", pc(us_at$rho * 100, 0), ", también representativo, y la mitad se ",
  "pierde en ", num(us_at$vida, 1), " años. En Argentina queda el ",
  pc(ar_at$rho * 100, 0), " y no es representativo: puede ser memoria o ",
  "puede ser ruido. El orden es Brasil el más lento, Iowa el más rápido, ",
  "y Argentina en el medio, sin que esa memoria se pueda afirmar."
)

texto_soja_cambio <- paste0(
  "Cuando la soja sube de un año al otro, la tierra de Mato Grosso la ",
  "acompaña en el mismo año (", num(acompanan("Brasil", "ln_precio_soja_cbot", 0), 2),
  ", donde 1 es moverse igual y 0 es no acompañarse). Iowa también, en el ",
  "mismo año (", num(acompanan("Estados Unidos", "ln_precio_soja_cbot", 0), 2),
  "). En la zona núcleo el mismo año el acompañamiento es ",
  num(acompanan("Argentina", "ln_precio_soja_cbot", 0), 2),
  ", y dos años después es ",
  num(acompanan("Argentina", "ln_precio_soja_cbot", 2), 2),
  ". El rastro argentino es débil en los dos momentos."
)

texto_futuro <- NULL
if (!is.null(riesgo) && nrow(riesgo) > 0) {
  w <- en_ventana(riesgo$anio)
  texto_futuro <- paste0(
    "Hacia adelante la cuenta no dibuja una tendencia. Dibuja un piso. Si ",
    "las instituciones argentinas se quedaran en el nivel brasileño, la ",
    "hectárea partiría de cerca de ",
    usd(mean(riesgo$precio_simulado_institucional_brasil[w])),
    " en un año parecido a ", e(ventana), ", y de ahí se movería con la ",
    "soja, el rendimiento y las tasas, como ya se mueve. Ese piso sigue ",
    "debajo de lo que se pagó en Mato Grosso en la misma ventana (",
    usd(precio_br), ") y debajo de Iowa (", usd(precio_us),
    "). El cambio institucional, solo, no alcanza para cerrar la brecha."
  )
}

# --- Documento ------------------------------------------------------------

hoy <- format(Sys.Date(), "%d/%m/%Y")

doc <- c(
encabezado_html(
  "Las cuatro preguntas — brecha de la tierra agrícola",
  "Las cuatro preguntas",
  paste0("Zona núcleo, Mato Grosso e Iowa, ", cfg$anio_min, "-", cfg$anio_max,
         ". Los coeficientes salen de todos los años. La brecha se mira en ",
         e(ventana), ". Corrida del ", hoy, ".")),

callout("cinfo", "Cómo está armado",
  paste0("Cada respuesta usa la ecuación de ese país, la misma del informe ",
         "principal. Un número es <b>representativo</b> cuando se distingue ",
         "del azar: con estos veintiún años, el vínculo es más grande que el ",
         "ruido. Si no es representativo, el número está, y puede ser ",
         "casualidad de la muestra.")),

'<h2>1. Qué parte de la brecha explica cada grupo</h2>',
paste0('<p class="meta">La diferencia de ', e(ventana),
       ' tomada como 100 %. Zona núcleo: ', usd(precio_ar),
       ' por hectárea.</p>'),
if (!is.null(brecha_br)) c(
  '<h3>Frente a Brasil</h3>',
  callout("cnota", "La brecha", brecha_br$texto),
  tabla(c("grupo", paste0("en ", e(ventana)), "parte del 100 %"),
        brecha_br$filas, num_desde = 3)
) else NULL,
if (!is.null(brecha_us)) c(
  '<h3>Frente a Estados Unidos</h3>',
  callout("cnota", "La brecha", brecha_us$texto),
  tabla(c("grupo", paste0("en ", e(ventana)), "parte del 100 %"),
        brecha_us$filas, num_desde = 3)
) else NULL,
callout("cinfo", "Cómo entran las retenciones",
  paste0("Mato Grosso e Iowa se ponen en 0 %. No tienen la retención ",
         "argentina, así que el productor de allá se queda con el precio ",
         "entero de la soja. El coeficiente de la alícuota no se usa: en la ",
         "muestra casi no se movió y no se distingue de cero. La parte se ",
         "valúa con el coeficiente de la soja, el mismo supuesto del ",
         "escenario institucional. Con el ",
         num(parte_retenciones("Brasil")$tau * 100, 0),
         " % de retención el productor argentino se queda con el ",
         num((1 - parte_retenciones("Brasil")$tau) * 100, 0),
         " % del precio. Ese coeficiente de la soja ",
         if (representa(coef_de("Argentina", "ln_precio_soja_cbot")$p_valor)) {
           "se distingue de cero."
         } else {
           "tampoco se distingue de cero."
         },
         "<br><br>La soja de Chicago, la tasa real internacional y la urea ",
         "valen lo mismo en los dos países, así que no explican el nivel. ",
         "Las ventas de maquinaria no entran, ni frente a Mato Grosso ni ",
         "frente a Iowa: el coeficiente sale al revés. Lo que falta para el ",
         "100 % es que el mismo factor se paga distinto en cada país.")),

'<h2>2. Cuánto se capitalizan el rendimiento y la tecnología</h2>',
'<p class="meta">Rendimiento de la soja y ventas de maquinaria por hectárea.</p>',
callout("cnota", "La respuesta",
  paste0("Representativo quiere decir que el resultado se distingue del azar. ",
         "El rendimiento se capitaliza en Iowa, y ahí es representativo. ",
         "Media tonelada más por hectárea va con una tierra ",
         en_palabras(efecto_pct(
           coef_de("Estados Unidos", "rendimiento_soja_t_ha")$coeficiente,
           MEDIA_TONELADA)),
         ". En Argentina el mismo salto la deja ",
         en_palabras(efecto_pct(
           coef_de("Argentina", "rendimiento_soja_t_ha")$coeficiente,
           MEDIA_TONELADA)),
         ", y no es representativo. En Brasil la deja ",
         en_palabras(efecto_pct(
           coef_de("Brasil", "rendimiento_soja_t_ha")$coeficiente,
           MEDIA_TONELADA)),
         ", y tampoco lo es.",
         "<br><br>Las ventas de maquinaria son lo que hay de adopción ",
         "tecnológica. Se esperaba que más tecnología subiera los rindes de ",
         "los años que vienen y, con eso, el precio de la hectárea. Salen al ",
         "revés, y en los ",
         "tres países eso es representativo. Una suba del 10 % en las ventas ",
         "por hectárea deja la tierra ",
         en_palabras(efecto_pct(
           coef_de("Argentina", "ln_ventas_maquinaria_por_ha")$coeficiente,
           SUBA_MAQ)),
         " en Argentina, ",
         en_palabras(efecto_pct(
           coef_de("Brasil", "ln_ventas_maquinaria_por_ha")$coeficiente,
           SUBA_MAQ)),
         " en Brasil y ",
         en_palabras(efecto_pct(
           coef_de("Estados Unidos", "ln_ventas_maquinaria_por_ha")$coeficiente,
           SUBA_MAQ)),
         " en Iowa. La serie es las unidades vendidas ese año, tractores ",
         "más cosechadoras, divididas por las hectáreas de soja sembradas. ",
         "No es el parque de máquinas.")),
'<h3>Rendimiento</h3>',
'<p>Si el rinde sube media tonelada por hectárea, cuánto se mueve la tierra.</p>',
tabla(c("país", "la tierra se mueve", "representativo"),
      filas_rend, num_desde = 2),
'<h3>Ventas de maquinaria</h3>',
'<p>Unidades vendidas en el año, tractores más cosechadoras, divididas por ',
'las hectáreas de soja sembradas. Si esa cantidad por hectárea sube 10 %, ',
'cuánto se mueve la tierra.</p>',
tabla(c("país", "la tierra se mueve", "representativo"),
      filas_maq, num_desde = 2),
callout("calerta", "La logística no entra en esta respuesta",
  paste0("La pregunta también nombra la infraestructura logística. El flete ",
         "no está en la ecuación. En su lugar está el diésel minorista, y un ",
         "litro más caro va con una tierra más cara. Por eso esta respuesta ",
         "usa el rendimiento y las ventas ",
         "de maquinaria, y deja el diésel afuera.")),

'<h2>3. Soja y tipo de cambio real</h2>',
'<p class="meta">El precio de Chicago y el tipo de cambio real del BIS, en cada país.</p>',
callout("cnota", "La respuesta",
  paste0("Si la soja sube ", SUBA, " %, la hectárea de Mato Grosso sube ",
         "cerca de ",
         pc(capital$suba_tierra_pct[capital$pais == "Brasil" &
                                      capital$especificacion == DEL_TRABAJO][1]),
         " y eso es representativo. La de la zona núcleo sube cerca de ",
         pc(capital$suba_tierra_pct[capital$pais == "Argentina" &
                                      capital$especificacion == DEL_TRABAJO][1]),
         " y la de Iowa cerca de ",
         pc(capital$suba_tierra_pct[capital$pais == "Estados Unidos" &
                                      capital$especificacion == DEL_TRABAJO][1]),
         ". La de Iowa se distingue de un traspaso completo, justo en el ",
         "límite. La de Argentina se distingue de cero y también de un ",
         "traspaso completo. Las tres reacciones no son la misma: esa ",
         "diferencia es representativa.",
         "<br><br>El tipo de cambio real solo se ve en Argentina. Diez puntos ",
         "más del índice, con el promedio del período en 100, son una moneda ",
         "más fuerte, y van con una hectárea ",
         en_palabras(efecto_pct(
           coef_de("Argentina", "tcr_bis_2005_2025_100")$coeficiente,
           PUNTOS_TCR)),
         ". Eso es representativo. En Brasil y en Iowa el mismo ",
         "movimiento no se distingue de cero, y tampoco se puede decir que ",
         "las tres reacciones al tipo de cambio sean distintas.")),
paste0('<h3>Si la soja sube ', SUBA, ' %</h3>'),
'<p>La última columna pregunta si esa suba se distingue de un traspaso ',
'completo, o sea de que la tierra suba el mismo ', SUBA, ' %.</p>',
tabla(c("país", "la tierra sube", "rango", "¿se distingue de cero?",
        paste0("¿se distingue de ", SUBA, " %?")),
      filas_soja, anchos = c(18, 16, 26, 20, 20), num_desde = 2),
if (!is.na(p_igualdad("ln_precio_soja_cbot"))) callout(
  "cinfo", "¿La soja pega distinto en cada país?",
  paste0("Sí. La prueba de que las tres reacciones son la misma se rechaza. ",
         "Mato Grosso capitaliza mucho más que la zona núcleo y que Iowa."))
else NULL,
callout("cnota", "Cuándo se ve esa suba",
  paste0("La tabla de arriba es el mismo año. Esta otra mirada le agrega el ",
         "precio de la tierra del año anterior, para ver en cuánto tiempo ",
         "entra una suba de la soja que se queda. En Brasil el mismo año ",
         "entra un tercio de lo que la hectárea va a subir, la mitad está ",
         "al año siguiente y el 90 % a los 5 años. Esa velocidad se distingue ",
         "del azar. En Iowa entra más de la mitad el mismo año y el 90 % a ",
         "los 2 años, y esa velocidad también se distingue. En Argentina el ",
         "ritmo no se distingue del azar. El tamaño de la suba, en esta ",
         "mirada, no se distingue de cero en ningún país: lo que se mide es ",
         "la velocidad, no un segundo porcentaje.")),
tabla(c("país", "el mismo año", "la mitad", "el 90 %"),
      filas_plazo, num_desde = 2),

'<h3>Si la moneda se fortalece 10 puntos</h3>',
'<p>Un aumento del índice es una apreciación real: la moneda local compra ',
'más canasta extranjera. El promedio de ', cfg$anio_min, '-', cfg$anio_max,
' vale 100 en cada país.</p>',
tabla(c("país", "la tierra se mueve", "representativo"),
      filas_tcr, num_desde = 2),
if (!is.na(p_igualdad("tcr_bis_2005_2025_100"))) callout(
  "cinfo", "¿El tipo de cambio pega distinto en cada país?",
  paste0("No se puede afirmar. La prueba de que las tres reacciones son la ",
         "misma no se rechaza. Argentina es el único donde el vínculo, ",
         "mirado solo, es representativo."))
else NULL,

'<h3>El atraso</h3>',
callout("cnota", "Cuánto tarda en irse un movimiento", texto_atraso),
callout("cinfo", "La soja, el mismo año o más tarde", texto_soja_cambio),
'<p>Con el precio del año anterior adentro, ni la soja ni el tipo de cambio ',
'quedan representativos en ningún país. El efecto de la soja que muestra la ',
'ecuación del trabajo ya está en el precio de ese año, sobre todo en Brasil ',
'y en Iowa. No aparece un efecto tardío medible encima de eso.</p>',

'<h2>4. La hectárea argentina con las instituciones de Brasil</h2>',
'<p class="meta">La ecuación argentina, con el bloque institucional reemplazado, en los veintiún años.</p>',
callout("cinfo", "Qué se reemplazó",
  paste0("La ecuación es la argentina, con los mismos coeficientes. Cada año ",
         "se cambia el riesgo país argentino por el de Brasil de ese año. ",
         "Brasil no tiene una alícuota medida: se pone en 0, igual que en la ",
         "comparación de la brecha de precios. El coeficiente de la alícuota ",
         "no se distingue de cero, así que sacar las retenciones se trata ",
         "como si el productor pasara a cobrar el precio entero de la soja, ",
         "y la hectárea reacciona con lo que ya reacciona a esa soja. La ",
         "brecha cambiaria, que también es institucional y Brasil no mide, ",
         "se pone en cero y se valúa con su propio coeficiente. El ",
         "rendimiento, el tipo de cambio, las tasas, la maquinaria, el ",
         "diésel, la urea y la lluvia quedan en el valor argentino.")),
if (!is.null(riesgo) && !is.null(h2_pr) && nrow(h2_pr) == 1) callout(
  "cnota", "La respuesta",
  paste0("Cambiar solo el riesgo país, que es lo que los dos países miden, ",
         "sube la hectárea simulada. En ", cfg$anio_max, " pasa de ",
         usd(riesgo$precio_simulado_argentino[riesgo$anio == cfg$anio_max]),
         " a ",
         usd(riesgo$precio_simulado_riesgo_brasil[riesgo$anio == cfg$anio_max]),
         ", unos ",
         miles(riesgo$diferencia_usd[riesgo$anio == cfg$anio_max]),
         " dólares, un ",
         pc(riesgo$diferencia_pct[riesgo$anio == cfg$anio_max]),
         ". En la ventana ", e(ventana), " ese cambio cubre el ",
         pc(h2_pr$proporcion_de_la_brecha_cubierta * 100, 0),
         " de la brecha con Mato Grosso. La hipótesis pedía más de la mitad, ",
         "y no llega.",
         "<br><br>Sumando sacar las retenciones y llevar la brecha cambiaria ",
         "a cero, el escenario queda más arriba en los ", nrow(riesgo),
         " años. El extra va de ",
         usd(min(riesgo$diferencia_institucional_usd)), " a ",
         usd(max(riesgo$diferencia_institucional_usd)),
         " por encima de la ecuación argentina. En ", e(ventana),
         " el promedio de ese escenario es <b>",
         usd(mean(riesgo$precio_simulado_institucional_brasil[en_ventana(riesgo$anio)])),
         "</b>, contra ",
         usd(mean(riesgo$precio_simulado_argentino[en_ventana(riesgo$anio)])),
         " de la ecuación con los datos argentinos. En ", cfg$anio_max,
         " da ",
         usd(riesgo$precio_simulado_institucional_brasil[riesgo$anio == cfg$anio_max]),
         ". Sigue debajo de los ",
         usd(panel$precio_ha_usd_2025[panel$pais == "Brasil" &
                                        panel$anio == cfg$anio_max]),
         " que se pagaron ese año en Mato Grosso."))
else NULL,
if (file.exists(file.path(cfg$dir_resultados,
                          "figura_4_institucional_brasil.png"))) c(
  paste0('<img src="figura_4_institucional_brasil.png" alt="',
         'Precio argentino con el riesgo de Brasil, sin retenciones y sin brecha">'),
  '<p class="meta">La línea verde es el riesgo país de Brasil, sin ',
  'retenciones y sin brecha cambiaria. La roja cambia solo el riesgo. La negra es el precio ',
  'pagado y la gris punteada es la ecuación con los datos argentinos.</p>'
) else NULL,
if (!is.null(texto_futuro)) callout("cinfo", "Qué se esperaría hacia adelante",
                                    texto_futuro) else NULL,
callout("calerta", "Dos límites de esta cuenta",
  paste0("En ", sum(riesgo$extrapolacion == "sí"), " de los ", nrow(riesgo),
         " años el riesgo de Brasil queda por debajo de todo lo que Argentina ",
         "tuvo en el período. El año se conserva, y el número es una ",
         "extrapolación. Y la alícuota en cero no se vio nunca en estos ",
         "veintiún años: el salto de las retenciones es un supuesto, no una ",
         "medición. La brecha en cero tampoco se vio: es el supuesto de que ",
         "desaparece la diferencia entre el dólar paralelo y el mayorista.")),
'<h3>Año por año</h3>',
paste0('<p>Dólares de ', cfg$anio_base_precios, ' por hectárea. En negrita, ',
       'los años de ', e(ventana), '. La última columna es el escenario ',
       'institucional contra la ecuación argentina.</p>'),
tabla(c("año", "precio pagado", "ecuación argentina", "solo riesgo de Brasil",
        "instituciones de Brasil", "diferencia"),
      filas_inst, num_desde = 2),

pie_html("12_informe_preguntas.R",
  paste0('El reparto de la brecha está en ',
         '<code>tabla_descomposicion_por_variable.csv</code>, ',
         'los coeficientes en <code>tabla_coeficientes.csv</code> y el ',
         'escenario institucional en <code>tabla_riesgo_brasil.csv</code>.'))
)

ruta_html <- file.path(cfg$dir_resultados, "informe_preguntas.html")
writeLines(unlist(doc), ruta_html, useBytes = TRUE)
mensaje("Informe de las cuatro preguntas escrito: ", ruta_html)
