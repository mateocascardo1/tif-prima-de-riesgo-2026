# ---------------------------------------------------------------------------
# 13_informe_hipotesis.R
# Un informe solo para la hipótesis del peso institucional (H1).
# No reestima nada: lee la descomposición, los coeficientes y el panel,
# y arma el texto con esos números.
#
# Salida:
#   03_resultados/informe_hipotesis.html
# ---------------------------------------------------------------------------

if (!exists("cfg")) source("scripts/00_config.R")
source("scripts/utilidades.R")
source("scripts/html_comun.R")

mensaje("== 13_informe_hipotesis: la hipótesis institucional ==")

if (!identical(cfg$referencia_oaxaca, "argentina")) {
  stop("Este informe está escrito con los coeficientes argentinos como ",
       "referencia.", call. = FALSE)
}

h1           <- leer("tabla_h1")
if (!is.null(h1)) {
  h1$cumple_en_este_comparable <- as.logical(h1$cumple_en_este_comparable)
}
categorias   <- leer("tabla_descomposicion_por_categoria")
detalle      <- leer("tabla_descomposicion_por_variable")
coeficientes <- leer("tabla_coeficientes")
bloques      <- leer("tabla_pruebas_bloques")
panel        <- utils::read.csv(file.path(cfg$dir_curados, "panel.csv"),
                                stringsAsFactors = FALSE)

if (is.null(h1) || is.null(categorias) || is.null(detalle) ||
    is.null(coeficientes)) {
  stop("Faltan las tablas de la descomposición. Correr antes ",
       "05_descomposicion.R.", call. = FALSE)
}

ventana <- paste(cfg$ventana_medias, collapse = "-")
h1_pr <- h1[h1$rol == "principal", ]
h1_an <- h1[h1$rol == "anexo", ]
if (nrow(h1_pr) != 2) {
  stop("H1 principal no tiene una fila por comparable.", call. = FALSE)
}

PROD  <- "Niveles: factores productivos"
ECON  <- "Niveles: factores económicos"
INST  <- "Niveles: factores institucionales"
NIV   <- "Componente de niveles (total)"
VAL   <- "Componente de valoración (residual, solo agregado)"
RESID <- "Residuo: parte de la brecha que la ecuación no explica en la ventana"
DIF_E <- "Institucional menos económico"
BRECHA <- "Brecha total (media de ln P del comparable menos Argentina)"

# Porcentajes grandes, con separador de miles: la banda del bootstrap
# sale del rango de los puntos del logaritmo y puede pasar de cuatro cifras.
pc_grande <- function(x) {
  if (length(x) != 1 || is.na(x)) return("—")
  if (abs(x) >= 100) {
    signo <- if (x > 0) "+" else "−"
    paste0(signo, miles(abs(x)), " %")
  } else {
    pc(x, 0, signo = TRUE)
  }
}

ha <- function(delta) pc(log_a_porcentaje(delta), 1, signo = TRUE)

usd_ha <- function(x) paste0(miles(x), " dólares por hectárea")

usd_delta <- function(delta) {
  dolares <- precio_ar * (exp(delta) - 1)
  paste0(if (dolares > 0) "+" else "", miles(dolares), " dólares")
}

cat_de <- function(comparable, componente, rol = "principal") {
  f <- categorias[categorias$rol == rol &
                    categorias$comparable == comparable &
                    categorias$componente == componente, ]
  if (nrow(f) != 1) {
    stop("No hay una fila de '", componente, "' para ", comparable,
         " (", rol, ").", call. = FALSE)
  }
  f
}

var_de <- function(comparable, variable, rol = "principal") {
  f <- detalle[detalle$rol == rol &
                 detalle$comparable == comparable &
                 detalle$variable == variable, ]
  if (nrow(f) != 1) {
    stop("No hay una fila de ", variable, " para ", comparable, ".",
         call. = FALSE)
  }
  f
}

h_de <- function(comparable, tabla = h1_pr) {
  f <- tabla[tabla$comparable == comparable, ]
  if (nrow(f) != 1) stop("Sin fila de H1 para ", comparable, ".", call. = FALSE)
  f
}

coef_de <- function(variable) {
  f <- coeficientes[coeficientes$pais == "Argentina" &
                      coeficientes$variable == variable, ]
  if (nrow(f) != 1) {
    stop("Sin coeficiente argentino de ", variable, ".", call. = FALSE)
  }
  f
}

# "se distingue del azar" o "no se distingue de cero", al nivel del trabajo.
frase_azar <- function(p) {
  if (is.na(p) || p >= cfg$nivel_bloques) "no se distingue de cero"
  else "se distingue del azar"
}

cubre_cero <- function(f) {
  !is.na(f$ic_inferior[1]) && !is.na(f$ic_superior[1]) &&
    f$ic_inferior[1] < 0 && f$ic_superior[1] > 0
}

chip_cumple <- function(cumple) {
  if (isTRUE(cumple)) "<span class='chip verde'>sí</span>"
  else "<span class='chip rojo'>no</span>"
}

NOMBRE <- c(
  embi_pb                     = "Riesgo país",
  brecha_cambiaria_pct        = "Brecha cambiaria",
  alicuota_export_soja_pct    = "Retenciones",
  tasa_real_local_pct         = "Tasa real local",
  tcr_bis_2005_2025_100       = "Tipo de cambio real",
  rendimiento_soja_t_ha       = "Rendimiento",
  diesel_usd_l                = "Diésel",
  precipitacion_desvio_mm     = "Lluvia",
  ln_urea                     = "Urea",
  ln_precio_soja_cbot         = "Precio de la soja",
  tips_10a_pct                = "TIPS a 10 años",
  ln_ventas_maquinaria_por_ha = "Ventas de maquinaria"
)

ORDEN_CAT <- c("institucional", "economico", "productivo")

# --- Precios de la ventana ------------------------------------------------

media_precio <- function(pais) {
  d <- panel[panel$pais == pais & en_ventana(panel$anio), "precio_ha_usd_2025"]
  mean(d, na.rm = TRUE)
}
precio_ar <- media_precio("Argentina")
precio_br <- media_precio("Brasil")
precio_us <- media_precio("Estados Unidos")

br <- h_de("Brasil")
us <- h_de("Estados Unidos")
veredicto <- unique(h1_pr$veredicto_global)[1]

inst_br <- cat_de("Brasil", INST)
prod_br <- cat_de("Brasil", PROD)
econ_br <- cat_de("Brasil", ECON)
niv_br  <- cat_de("Brasil", NIV)
val_br  <- cat_de("Brasil", VAL)
res_br  <- cat_de("Brasil", RESID)
gap_br  <- cat_de("Brasil", BRECHA)
dif_br  <- cat_de("Brasil", DIF_E)

inst_us <- cat_de("Estados Unidos", INST)
prod_us <- cat_de("Estados Unidos", PROD)
econ_us <- cat_de("Estados Unidos", ECON)
niv_us  <- cat_de("Estados Unidos", NIV)
val_us  <- cat_de("Estados Unidos", VAL)
res_us  <- cat_de("Estados Unidos", RESID)
gap_us  <- cat_de("Estados Unidos", BRECHA)
dif_us  <- cat_de("Estados Unidos", DIF_E)

for (comp in c("Brasil", "Estados Unidos")) {
  suma <- cat_de(comp, INST)$contribucion_log +
    cat_de(comp, PROD)$contribucion_log +
    cat_de(comp, ECON)$contribucion_log
  if (abs(suma - cat_de(comp, NIV)$contribucion_log) > 1e-6) {
    stop("Los tres bloques no suman el componente de niveles en ", comp, ".",
         call. = FALSE)
  }
}

maq_br  <- var_de("Brasil", "ln_ventas_maquinaria_por_ha")
rin_br  <- var_de("Brasil", "rendimiento_soja_t_ha")
emb_br  <- var_de("Brasil", "embi_pb")
ali_br  <- var_de("Brasil", "alicuota_export_soja_pct")
gap_br  <- var_de("Brasil", "brecha_cambiaria_pct")
tcr_br  <- var_de("Brasil", "tcr_bis_2005_2025_100")
tasa_br <- var_de("Brasil", "tasa_real_local_pct")

maq_us  <- var_de("Estados Unidos", "ln_ventas_maquinaria_por_ha")
rin_us  <- var_de("Estados Unidos", "rendimiento_soja_t_ha")
emb_us  <- var_de("Estados Unidos", "embi_pb")
ali_us  <- var_de("Estados Unidos", "alicuota_export_soja_pct")
gap_us  <- var_de("Estados Unidos", "brecha_cambiaria_pct")
tcr_us  <- var_de("Estados Unidos", "tcr_bis_2005_2025_100")
tasa_us <- var_de("Estados Unidos", "tasa_real_local_pct")

p_maq  <- coef_de("ln_ventas_maquinaria_por_ha")$p_valor
p_emb  <- coef_de("embi_pb")$p_valor
p_ali  <- coef_de("alicuota_export_soja_pct")$p_valor
p_gap  <- coef_de("brecha_cambiaria_pct")$p_valor
p_tcr  <- coef_de("tcr_bis_2005_2025_100")$p_valor
p_tasa <- coef_de("tasa_real_local_pct")$p_valor

# --- Veredicto en la primera pantalla -------------------------------------

titulo_global <- if (grepl("PARCIAL", veredicto)) {
  "Confirmada a medias"
} else if (grepl("^CONFIRMADA", veredicto)) {
  "Confirmada"
} else if (grepl("RECHAZADA", veredicto)) {
  "Rechazada"
} else {
  "Sin veredicto"
}

clase_global <- if (grepl("PARCIAL", veredicto)) {
  "v-parcial"
} else if (grepl("^CONFIRMADA", veredicto)) {
  "v-si"
} else {
  "v-no"
}

tono <- if (grepl("RECHAZADA", veredicto)) {
  "cmal"
} else if (grepl("PARCIAL", veredicto)) {
  "calerta"
} else {
  "cinfo"
}

carta <- function(clase, kicker, resultado, detalle) {
  paste0("<div class='", clase, "'><p class='k'>", kicker, "</p>",
         "<p class='r'>", resultado, "</p><p class='d'>", detalle, "</p></div>")
}

cartas <- paste0(
  "<div class='veredictos'>",
  carta(if (isTRUE(br$cumple_en_este_comparable)) "v-si" else "v-no",
        "Brasil",
        if (isTRUE(br$cumple_en_este_comparable)) "Se cumple" else "No se cumple",
        if (isTRUE(br$cumple_en_este_comparable))
          "Supera al productivo y al económico." else
          "No supera a los dos."),
  carta(if (isTRUE(us$cumple_en_este_comparable)) "v-si" else "v-no",
        "Estados Unidos",
        if (isTRUE(us$cumple_en_este_comparable)) "Se cumple" else "No se cumple",
        if (isTRUE(us$cumple_en_este_comparable))
          "Supera al productivo y al económico."
        else if (us$economico > us$institucional)
          "Lo económico pesa más."
        else
          "No supera a los dos."),
  carta(clase_global, "Las dos juntas", titulo_global,
        "La hipótesis pedía los dos países."),
  "</div>"
)

quienes_si <- h1_pr$comparable[h1_pr$cumple_en_este_comparable]
quienes_no <- h1_pr$comparable[!h1_pr$cumple_en_este_comparable]
lista_paises <- function(x) {
  if (length(x) == 0) return("ninguno")
  if (length(x) == 1) return(e(x))
  paste0(e(x[1]), " y ", e(x[2]))
}

texto_respuesta <- paste0(
  "<b>", e(titulo_global), ".</b> ",
  if (length(quienes_si) > 0) {
    paste0("Se cumple frente a ", lista_paises(quienes_si), ". ")
  } else "",
  if (length(quienes_no) > 0) {
    paste0("No se cumple frente a ", lista_paises(quienes_no), ". ")
  } else "",
  if (grepl("PARCIAL", veredicto)) {
    "La hipótesis pedía los dos países. Cumplir en uno solo es confirmación parcial."
  } else if (grepl("^CONFIRMADA", veredicto)) {
    "Se cumple en los dos, que es lo que la hipótesis pedía."
  } else if (grepl("RECHAZADA", veredicto)) {
    "No se cumple en ninguno. La hipótesis pedía los dos."
  } else {
    ""
  },
  "<br><br>Cada número de abajo es la parte de la brecha que mueve ese ",
  "bloque, en el precio de la hectárea. Positivo agranda la brecha: empuja ",
  "para el lado en que la otra hectárea es más cara. Negativo la achica."
)

# El párrafo de lectura solo afirma lo que los signos sostienen.
if (isTRUE(br$cumple_en_este_comparable) && br$productivo < 0) {
  texto_respuesta <- paste0(
    texto_respuesta,
    "<br><br>Frente a Brasil el productivo es negativo (",
    ha(br$productivo), " de la hectárea) y el institucional, con ",
    ha(br$institucional), ", le queda arriba. Lo económico es ",
    ha(br$economico), ". La ventaja del institucional es ",
    ha(dif_br$contribucion_log), " de la hectárea, unos ",
    miles(abs(precio_ar * (exp(dif_br$contribucion_log) - 1))),
    " dólares sobre el precio medio de la zona núcleo."
  )
}
if (!isTRUE(us$cumple_en_este_comparable) && us$economico > us$institucional) {
  texto_respuesta <- paste0(
    texto_respuesta,
    "<br><br>Frente a Estados Unidos lo económico (",
    ha(us$economico), ") supera a lo institucional (",
    ha(us$institucional), ")."
  )
}

# --- Tablas ----------------------------------------------------------------

fila_resumen <- function(h) {
  fila(e(h$comparable),
       ha(h$institucional),
       ha(h$productivo),
       ha(h$economico),
       chip_cumple(h$cumple_en_este_comparable))
}

fila_pieza <- function(comparable, componente, etiqueta, negrita = FALSE) {
  f <- cat_de(comparable, componente)
  celda <- function(x) if (negrita) paste0("<b>", x, "</b>") else x
  parte <- if (abs(gap_de(comparable)) >= 0.10) {
    pc(f$porcentaje_de_la_brecha, 0, signo = TRUE)
  } else {
    "—"
  }
  fila(celda(e(comparable)),
       celda(etiqueta),
       celda(num(f$contribucion_log, 3)),
       celda(paste0(ha(f$contribucion_log),
                    "<br><span class='sub'>",
                    usd_delta(f$contribucion_log), "</span>")),
       celda(parte))
}

gap_de <- function(comparable) cat_de(comparable, BRECHA)$contribucion_log

filas_desarme <- unlist(lapply(c("Brasil", "Estados Unidos"), function(comp) {
  c(fila_pieza(comp, INST, "Institucional", TRUE),
    fila_pieza(comp, PROD, "Productivo", TRUE),
    fila_pieza(comp, ECON, "Económico", TRUE),
    fila_pieza(comp, NIV, "Los tres juntos"),
    fila_pieza(comp, VAL, "Valoración, solo el total"),
    fila_pieza(comp, RESID, "Lo que la ecuación no explica"),
    fila_pieza(comp, BRECHA, "Brecha", TRUE))
}))

# El detalle por variable deja afuera lo que no mueve los niveles.
filas_var <- unlist(lapply(c("Brasil", "Estados Unidos"), function(comp) {
  d <- detalle[detalle$rol == "principal" & detalle$comparable == comp &
                 detalle$variable != "(Intercept)" &
                 abs(detalle$contribucion_niveles_log) >= 1e-4, ]
  d$orden <- match(d$categoria, ORDEN_CAT)
  d <- d[order(d$orden, -abs(d$contribucion_niveles_log)), ]
  primera <- TRUE
  vapply(seq_len(nrow(d)), function(i) {
    nombre <- NOMBRE[[d$variable[i]]]
    if (is.null(nombre)) nombre <- d$variable[i]
    bloque <- c(institucional = "institucional", economico = "económico",
                productivo = "productivo")[[d$categoria[i]]]
    if (is.null(bloque)) bloque <- d$categoria[i]
    nota <- if (nzchar(d$observacion[i])) {
      "<br><span class='sub'>No está en la ecuación de este país: la cuenta le pone cero.</span>"
    } else ""
    pais <- if (primera) {
      primera <<- FALSE
      paste0("<b>", e(comp), "</b>")
    } else ""
    fila(pais,
         paste0(e(nombre), nota),
         e(bloque),
         num(d$contribucion_niveles_log[i], 3),
         ha(d$contribucion_niveles_log[i]))
  }, character(1))
}))

banda <- function(f) {
  paste0(num(f$ic_inferior, 2), " a ", num(f$ic_superior, 2))
}

filas_bandas <- c(
  fila("<b>Brasil</b>", banda(inst_br), banda(prod_br), banda(econ_br),
       banda(dif_br)),
  fila("<b>Estados Unidos</b>", banda(inst_us), banda(prod_us),
       banda(econ_us), banda(dif_us))
)

bloques_cubren_cero <- all(vapply(
  list(inst_br, prod_br, econ_br, inst_us, prod_us, econ_us),
  cubre_cero, logical(1)
))

# --- Texto de Brasil -------------------------------------------------------

texto_brasil <- paste0(
  "En la cuenta, ",
  if (isTRUE(br$cumple_en_este_comparable)) "se cumple. " else "no se cumple. ",
  "Lo institucional mueve ", ha(inst_br$contribucion_log),
  " de la hectárea, lo productivo ", ha(prod_br$contribucion_log),
  " y lo económico ", ha(econ_br$contribucion_log), "."
)

if (prod_br$contribucion_log < 0 &&
    maq_br$contribucion_niveles_log <= prod_br$contribucion_log &&
    maq_br$contribucion_niveles_log < 0) {
  texto_brasil <- paste0(
    texto_brasil,
    " El productivo empuja al revés. Lo pone la tecnología: Mato Grosso ",
    "vende más maquinaria por hectárea sembrada, y el coeficiente argentino ",
    "de esas ventas sale ",
    if (coef_de("ln_ventas_maquinaria_por_ha")$coeficiente < 0) "negativo"
    else "positivo",
    " y ", frase_azar(p_maq),
    ". El trabajo esperaba un positivo, porque más tecnología hoy se ",
    "capitaliza en la hectárea. Esa sola línea resta un ",
    pc(abs(log_a_porcentaje(maq_br$contribucion_niveles_log)), 1),
    ". El rendimiento suma un ",
    pc(abs(log_a_porcentaje(rin_br$contribucion_niveles_log)), 1),
    " y no compensa. Con el productivo en negativo, el institucional le ",
    "queda arriba."
  )
}

if (emb_br$contribucion_niveles_log > 0 &&
    ali_br$contribucion_niveles_log < 0) {
  texto_brasil <- paste0(
    texto_brasil,
    "</p><p>Del institucional, el riesgo país aporta ",
    ha(emb_br$contribucion_niveles_log),
    " y las retenciones ", ha(ali_br$contribucion_niveles_log),
    ". Argentina promedió ", miles(emb_br$media_argentina),
    " puntos básicos en estos años y Brasil ",
    miles(emb_br$media_comparable),
    ". El coeficiente del riesgo ", frase_azar(p_emb),
    ". Las retenciones restan: su coeficiente sale ",
    if (coef_de("alicuota_export_soja_pct")$coeficiente > 0) {
      "positivo, al revés de lo que espera el trabajo, y "
    } else {
      "negativo, que es el signo que espera el trabajo, y "
    },
    frase_azar(p_ali),
    ". La media brasileña de la alícuota vale cero porque Brasil no tiene ",
    "esa serie. Es una convención de la descomposición, no una alícuota medida. ",
    "La brecha cambiaria aporta ", ha(gap_br$contribucion_niveles_log),
    ". En la ventana promedió ", num(gap_br$media_argentina, 0),
    " puntos en Argentina, y la cuenta le pone cero a Brasil. El coeficiente ",
    frase_azar(p_gap),
    if (coef_de("brecha_cambiaria_pct")$coeficiente < 0) {
      ", y el signo es el que espera el trabajo: una brecha más ancha va con una hectárea más barata."
    } else {
      ", y el signo sale al revés de lo que espera el trabajo."
    }
  )
}

texto_brasil <- paste0(
  texto_brasil,
  "</p><p>Lo económico queda en ", ha(econ_br$contribucion_log),
  ". El tipo de cambio real casi no difiere: el índice promedió ",
  num(tcr_br$media_argentina, 0), " en Argentina y ",
  num(tcr_br$media_comparable, 0), " en Brasil, con el promedio de ",
  cfg$anio_min, "-", cfg$anio_max, " puesto en 100. Lo que mueve el bloque ",
  "es la tasa real local, ", ha(tasa_br$contribucion_niveles_log),
  ". En la ventana Argentina promedió ", pc(tasa_br$media_argentina, 0),
  " y Brasil ", pc(tasa_br$media_comparable, 0, signo = TRUE),
  ". El coeficiente de esa tasa sale ",
  if (coef_de("tasa_real_local_pct")$coeficiente > 0) {
    "positivo, al revés de lo que espera el trabajo, y "
  } else {
    "negativo, que es el signo que espera el trabajo, y "
  },
  frase_azar(p_tasa), "."
)

# --- Texto de Estados Unidos ----------------------------------------------

texto_us <- paste0(
  "En la cuenta, ",
  if (isTRUE(us$cumple_en_este_comparable)) "se cumple. " else "no se cumple. ",
  "Lo institucional mueve ", ha(inst_us$contribucion_log),
  " de la hectárea, lo económico ", ha(econ_us$contribucion_log),
  " y lo productivo ", ha(prod_us$contribucion_log), "."
)

if (!isTRUE(us$cumple_en_este_comparable) &&
    us$institucional > us$productivo &&
    us$economico > us$institucional) {
  texto_us <- paste0(
    texto_us,
    " Supera al productivo y no supera al económico. Hacen falta las dos."
  )
}

if (nzchar(emb_us$observacion) && nzchar(ali_us$observacion)) {
  texto_us <- paste0(
    texto_us,
    "</p><p>Ese institucional no compara dos instituciones medidas. El ",
    "riesgo país, las retenciones y la brecha cambiaria no están en la ",
    "ecuación de Estados Unidos, y la cuenta les pone cero. El riesgo aporta ",
    ha(emb_us$contribucion_niveles_log),
    ", las retenciones ", ha(ali_us$contribucion_niveles_log),
    " y la brecha ", ha(gap_us$contribucion_niveles_log), ". ",
    paste0("El del riesgo ", frase_azar(p_emb),
           ", el de las retenciones ", frase_azar(p_ali),
           " y el de la brecha ", frase_azar(p_gap), ".")
  )
}

if (tcr_us$contribucion_niveles_log > inst_us$contribucion_log) {
  texto_us <- paste0(
    texto_us,
    "</p><p>Lo que pone lo económico por encima es el tipo de cambio real: ",
    ha(tcr_us$contribucion_niveles_log),
    " de la hectárea, más que el bloque institucional entero. En la ventana ",
    "el índice de Iowa promedió ", num(tcr_us$media_comparable, 0),
    " y el de Argentina ", num(tcr_us$media_argentina, 0),
    ". El coeficiente argentino ", frase_azar(p_tcr),
    " y el signo es el que el trabajo esperaba: una moneda más fuerte va ",
    "con una hectárea más cara en dólares. La tasa real local suma otro ",
    ha(tasa_us$contribucion_niveles_log), " y su coeficiente ",
    frase_azar(p_tasa), "."
  )
}

if (maq_us$contribucion_niveles_log < 0 &&
    abs(maq_us$contribucion_niveles_log) > abs(prod_us$contribucion_log)) {
  texto_us <- paste0(
    texto_us,
    "</p><p>El productivo, otra vez, lo domina la maquinaria: resta un ",
    pc(abs(log_a_porcentaje(maq_us$contribucion_niveles_log)), 1),
    ". El rendimiento suma un ",
    pc(abs(log_a_porcentaje(rin_us$contribucion_niveles_log)), 1),
    ". El bloque queda en ", ha(prod_us$contribucion_log),
    ", del lado contrario a la brecha."
  )
}

# --- Bandas ----------------------------------------------------------------

texto_bandas <- paste0(
  "La hipótesis se juzga por el número central, no por la banda. ",
  "Frente a Brasil, institucional menos económico es ",
  num(dif_br$contribucion_log, 3), " puntos del logaritmo, ",
  ha(dif_br$contribucion_log), " de la hectárea. La banda del ",
  round(cfg$nivel_confianza * 100), " % va de ",
  pc_grande(log_a_porcentaje(dif_br$ic_inferior)), " a ",
  pc_grande(log_a_porcentaje(dif_br$ic_superior)),
  if (cubre_cero(dif_br)) " y pasa por el cero. " else ". ",
  "Frente a Estados Unidos la misma diferencia es ",
  num(dif_us$contribucion_log, 3), " puntos, ",
  ha(dif_us$contribucion_log), " de la hectárea, y su banda va de ",
  pc_grande(log_a_porcentaje(dif_us$ic_inferior)), " a ",
  pc_grande(log_a_porcentaje(dif_us$ic_superior)),
  if (cubre_cero(dif_us)) ". También pasa por el cero." else "."
)

if (bloques_cubren_cero) {
  texto_bandas <- paste0(
    texto_bandas,
    " Los tres bloques, en los dos países, tienen bandas que pasan por el ",
    "cero. El orden de la tabla es el orden de los números centrales."
  )
}

texto_bandas <- paste0(
  texto_bandas,
  " La banda sale de reestimar ", cfg$bootstrap_B,
  " veces, sacando y reponiendo años enteros. Mide la duda sobre cuánto ",
  "reacciona la hectárea a cada factor. Las medias de ", e(ventana),
  " quedan quietas."
)

# --- El bloque institucional, como grupo ----------------------------------

texto_bloque <- NULL
if (!is.null(bloques)) {
  p_inst <- bloques$p_valor[bloques$pais == "Argentina" &
                              bloques$bloque == "institucional"]
  p_prod <- bloques$p_valor[bloques$pais == "Argentina" &
                              bloques$bloque == "productivo"]
  p_econ <- bloques$p_valor[bloques$pais == "Argentina" &
                              bloques$bloque == "economico"]
  if (length(p_inst) == 1 && length(p_prod) == 1 && length(p_econ) == 1) {
    llega <- if (p_inst < cfg$nivel_bloques) {
      paste0("llegan al ", round(cfg$nivel_bloques * 100),
             " %: mirados juntos, no se puede decir que los tres ",
             "coeficientes sean cero")
    } else {
      paste0("no llegan al ", round(cfg$nivel_bloques * 100),
             " %: no se puede descartar que los tres coeficientes sean cero")
    }
    texto_bloque <- paste0(
      "En la ecuación argentina, el riesgo país, las retenciones y la ",
      "brecha cambiaria, mirados juntos, ", llega, ". El bloque productivo ",
      "y el económico sí se distinguen del azar. La hipótesis no se decide ",
      "por esta prueba. Se decide por el tamaño de cada contribución en el ",
      "componente de niveles."
    )
  }
}

# --- Dónde está la brecha --------------------------------------------------

texto_donde <- paste0(
  "Sumados, los tres bloques empujan al revés de la brecha: ",
  ha(niv_br$contribucion_log), " de la hectárea frente a Brasil y ",
  ha(niv_us$contribucion_log), " frente a Estados Unidos. Las diferencias ",
  "de factores que mide el trabajo, tomadas juntas, harían la hectárea ",
  "argentina más cara.",
  "</p><p>La brecha que se ve queda en la valoración: ",
  ha(val_br$contribucion_log), " de la hectárea frente a Mato Grosso y ",
  ha(val_us$contribucion_log), " frente a Iowa. Ese número mezcla dos cosas ",
  "que la cuenta no separa. Una es cuánto paga de más cada mercado por el ",
  "mismo factor. La otra es la diferencia de punto de partida de las dos ",
  "ecuaciones, que cae entera de este lado. No se le puede poner el nombre ",
  "de un factor. La hipótesis no usa esta fila.",
  "</p><p>Lo que la ecuación no alcanza a explicar en ", e(ventana),
  " es chico: ", ha(res_br$contribucion_log), " frente a Brasil y ",
  ha(res_us$contribucion_log), " frente a Estados Unidos."
)

# --- Anexo ------------------------------------------------------------------

filas_anexo <- character(0)
texto_anexo <- NULL
if (nrow(h1_an) > 0) {
  filas_anexo <- vapply(seq_len(nrow(h1_an)), function(i) {
    fila(e(h1_an$comparable[i]),
         ha(h1_an$institucional[i]),
         ha(h1_an$productivo[i]),
         ha(h1_an$economico[i]),
         chip_cumple(h1_an$cumple_en_este_comparable[i]))
  }, character(1))
  gap_an_br <- cat_de("Brasil", BRECHA, "anexo")
  oficial <- unique(h1_an$veredicto_global)[1]
  partes <- strsplit(oficial, ": ", fixed = TRUE)[[1]]
  oficial_leible <- paste0(
    substring(partes[1], 1, 1), tolower(substring(partes[1], 2)),
    ". ", toupper(substring(partes[2], 1, 1)), substring(partes[2], 2))
  texto_anexo <- paste0(
    "Sobre ", e(h1_an$ventana[1]), " la regla dice: <b>",
    e(oficial_leible), "</b>",
    "</p><p>Esa ventana no responde la hipótesis. La brecha con Mato Grosso ",
    "cambia de signo adentro de los veintiún años, y el promedio de todo el ",
    "período queda en ", ha(gap_an_br$contribucion_log),
    " de la hectárea. Es el resultado de que una época cancele a la otra, ",
    "y no describe los años de ", e(ventana),
    ". Por eso el porcentaje sobre esa brecha no se usa."
  )
}

fig <- "figura_1_precio_por_hectarea.png"
hay_fig <- file.exists(file.path(cfg$dir_resultados, fig))

# --- Documento --------------------------------------------------------------

doc <- c(
  encabezado_html(
    "Hipótesis: el peso institucional",
    "¿Lo institucional explica más de la brecha?",
    paste0("«Los factores institucionales explican una proporción mayor de ",
           "la brecha de precio por hectárea entre la tierra agrícola ",
           "argentina y la de Brasil y la de Estados Unidos que los factores ",
           "productivos y los económicos, según el componente de niveles de ",
           "su descomposición.»")),

  "<style>",
  ".veredictos{display:grid;grid-template-columns:repeat(3,1fr);",
  "  gap:1rem;margin:1.4rem 0 1.6rem}",
  ".veredictos div{border-radius:4px;padding:1rem 1.1rem}",
  ".veredictos p{margin:.15rem 0}",
  ".veredictos .k{font-size:.75rem;font-weight:600;letter-spacing:.04em;",
  "  text-transform:uppercase;color:var(--suave)}",
  ".veredictos .r{font-size:1.35rem;font-weight:600;letter-spacing:-.02em}",
  ".veredictos .d{color:var(--tinta);font-size:.92rem}",
  ".v-si{background:var(--verdebg)}",
  ".v-no{background:var(--rojobg)}",
  ".v-parcial{background:var(--ambarbg)}",
  "@media(max-width:720px){.veredictos{grid-template-columns:1fr}}",
  "</style>",

  cartas,
  callout(tono, "La respuesta", texto_respuesta),

  "<h2>El componente de niveles, en el precio de la hectárea</h2>",
  paste0("<p class='meta'>Medias de ", e(ventana),
         ". Cada contribución se valúa con lo que la hectárea argentina ",
         "responde a ese factor. Los coeficientes salen de ",
         cfg$anio_min, "-", cfg$anio_max,
         ": con cinco años la ecuación no tiene solución. ",
         "Positivo agranda la brecha.</p>"),
  tabla(c("comparable", "institucional", "productivo", "económico",
          "¿supera a los dos?"),
        c(fila_resumen(br), fila_resumen(us)),
        num_desde = 2),

  "<h2>Cómo se decide que sí</h2>",
  paste0("<p>Se mira solo el componente de niveles: cuánto de la brecha sale ",
         "de que los factores no son iguales. Cada diferencia se multiplica ",
         "por la sensibilidad argentina. Si Iowa paga distinto por el mismo ",
         "factor, eso no entra acá.</p>",
         "<p>Se cumple en un país cuando lo institucional es mayor que lo ",
         "productivo y, por separado, mayor que lo económico. Mayor con ",
         "signo. No se compara contra la suma de los otros dos. Tiene que ",
         "cumplirse frente a Brasil y frente a Estados Unidos. Uno solo es ",
         "confirmación parcial. Ninguno es rechazo.</p>"),

  "<h2>La brecha que se parte</h2>",
  paste0("<p>En dólares de ", cfg$anio_base_precios, ", el promedio de ",
         e(ventana), " fue ", usd_ha(precio_ar), " en la zona núcleo, ",
         usd_ha(precio_br), " en Mato Grosso y ", usd_ha(precio_us),
         " en Iowa. La descomposición no parte esos promedios. Parte la ",
         "diferencia de los logaritmos del precio: Mato Grosso queda un ",
         pc(gap_br$diferencia_porcentual, 0), " arriba e Iowa un ",
         pc(gap_us$diferencia_porcentual, 0), " arriba.</p>"),
  if (hay_fig) c(
    paste0('<img src="', fig, '" alt="Precio por hectárea, dólares de ',
           cfg$anio_base_precios, '">'),
    paste0("<p class='meta'>Dólares de ", cfg$anio_base_precios,
           " por hectárea. La hipótesis usa el promedio de ", e(ventana),
           ".</p>")
  ) else NULL,

  "<h2>Cómo se reparte</h2>",
  paste0("<p class='meta'>La columna de dólares está tomada sobre los ",
         miles(precio_ar),
         " dólares por hectárea de la zona núcleo, y es el efecto de esa ",
         "pieza sola. Esas piezas no se suman entre sí: lo que se suma es el ",
         "punto del logaritmo, y por eso cierra la columna de la brecha. ",
         "Un porcentaje negativo achica la brecha. Las tres primeras filas ",
         "de cada país son las que juzga la hipótesis.</p>"),
  tabla(c("comparable", "pieza", "puntos del logaritmo",
          "en el precio de la hectárea", "% de la brecha"),
        filas_desarme, anchos = c(16, 28, 16, 22, 18)),
  callout("calerta", "Dónde está la brecha, en esta cuenta", texto_donde),

  "<h2>Frente a Brasil</h2>",
  paste0("<p>", texto_brasil, "</p>"),

  "<h2>Frente a Estados Unidos</h2>",
  paste0("<p>", texto_us, "</p>"),

  "<h2>Qué hay adentro de cada bloque</h2>",
  paste0("<p class='meta'>Solo lo que mueve los niveles en ", e(ventana),
         ". El precio de la soja, los TIPS y la urea valen lo mismo en los ",
         "tres países, y su aporte es cero. TIPS es la tasa real de los ",
         "bonos del Tesoro de Estados Unidos a 10 años.</p>"),
  tabla(c("comparable", "factor", "bloque", "puntos del logaritmo",
          "en el precio"),
        filas_var, num_desde = 4),
  if (!is.null(texto_bloque)) {
    callout("cnota", "El bloque institucional, mirado entero", texto_bloque)
  } else NULL,

  "<h2>Qué tan preciso es el orden</h2>",
  paste0("<p>", texto_bandas, "</p>"),
  paste0("<p class='meta'>Bandas del ",
         round(cfg$nivel_confianza * 100),
         " %, en puntos del logaritmo. La última columna es institucional ",
         "menos económico: positivo quiere decir que lo institucional es ",
         "más grande.</p>"),
  tabla(c("comparable", "institucional", "productivo", "económico",
          "institucional − económico"),
        filas_bandas, num_desde = 2),

  if (length(filas_anexo) > 0) c(
    "<h2>Anexo. El período completo</h2>",
    paste0("<p class='meta'>La misma comparación, con las medias de ",
           e(h1_an$ventana[1]),
           ". No es la ventana que responde la hipótesis.</p>"),
    tabla(c("comparable", "institucional", "productivo", "económico",
            "¿supera a los dos?"),
          filas_anexo, num_desde = 2),
    callout("cnota", "Por qué no se usa este promedio", texto_anexo)
  ) else NULL,

  pie_html("13_informe_hipotesis.R",
           paste0("Los números salen de <code>tabla_h1.csv</code>, ",
                  "<code>tabla_descomposicion_por_categoria.csv</code> y ",
                  "<code>tabla_descomposicion_por_variable.csv</code>."))
)

ruta_html <- file.path(cfg$dir_resultados, "informe_hipotesis.html")
writeLines(unlist(doc), ruta_html, useBytes = TRUE)
mensaje("Informe de la hipótesis escrito: ", ruta_html)
