# ---------------------------------------------------------------------------
# 07_informe_html.R
# Arma un informe HTML legible con todo lo que produjeron los scripts
# anteriores: de dónde sale cada factor, cómo quedó cada regresión, cómo se
# resolvió la colinealidad en Argentina, el problema de endogeneidad de las
# retenciones y en qué terminó cada hipótesis.
#
# No calcula nada nuevo salvo la comparación de colinealidad, que vuelve a
# estimar la especificación vieja (inflación y tasa nominal por separado)
# para poder mostrar el antes y el después sin números escritos a mano.
#
# Salida:
#   03_resultados/informe.html
# ---------------------------------------------------------------------------

if (!exists("cfg")) source("scripts/00_config.R")
source("scripts/utilidades.R")
source("scripts/html_comun.R")

mensaje("== 07_informe_html: informe para leer ==")

coeficientes  <- leer("tabla_coeficientes")
ajuste        <- leer("tabla_ajuste")
diagnosticos  <- leer("tabla_diagnosticos")
pr_bloques    <- leer("tabla_pruebas_bloques")
capital       <- leer("tabla_capitalizacion")
endogeneidad  <- leer("tabla_endogeneidad_fiscal")
categorias_ob <- leer("tabla_descomposicion_por_categoria")
h1            <- leer("tabla_h1")
h2            <- leer("tabla_h2")
potencial     <- leer("tabla_precio_potencial")
diccionario   <- leer("diccionario", cfg$dir_curados)
panel         <- leer("panel", cfg$dir_curados)
hay_modelos   <- !is.null(ajuste) && "r2_ajustado" %in% names(ajuste)

# Las tablas de la descomposición traen dos ventanas de medias: la principal,
# que es donde se responden las hipótesis, y el período completo como anexo.
solo_rol <- function(tabla, rol_buscado) {
  if (is.null(tabla) || !"rol" %in% names(tabla)) return(tabla)
  tabla[tabla$rol == rol_buscado, , drop = FALSE]
}
cat_pr <- solo_rol(categorias_ob, "principal")
cat_an <- solo_rol(categorias_ob, "anexo")
h1_pr  <- solo_rol(h1, "principal")
h2_pr  <- solo_rol(h2, "principal")
h2_an  <- solo_rol(h2, "anexo")

ventana_pr <- if (!is.null(cat_pr) && "ventana" %in% names(cat_pr)) {
  cat_pr$ventana[1]
} else sprintf("%d-%d", cfg$ventana_medias[1], cfg$ventana_medias[2])
ventana_an <- sprintf("%d-%d", cfg$anio_min, cfg$anio_max)

# --- 1. Los factores ------------------------------------------------------

regresores_todos <- unique(unlist(cfg$regresores, use.names = FALSE))
usados <- c(cfg$dependiente, regresores_todos)

en_que_ecuaciones <- function(v) {
  if (identical(v, cfg$dependiente)) return("variable dependiente")
  donde <- cfg$paises[vapply(cfg$paises,
                             function(p) v %in% cfg$regresores[[p]], logical(1))]
  if (length(donde) == length(cfg$paises)) "las tres" else
    paste(donde, collapse = ", ")
}

filas_factores <- character(0)
if (!is.null(diccionario)) {
  for (v in usados) {
    d <- diccionario[diccionario$variable == v, ]
    if (nrow(d) == 0) next
    filas_factores <- c(filas_factores, fila(
      paste0("<code>", e(v), "</code><br><span class='sub'>",
             e(d$categoria[1]), "</span>"),
      e(d$definicion[1]),
      paste0("<span class='unidad'>", e(d$unidad[1]), "</span>"),
      e(d$fuente[1]),
      e(d$transformacion[1]),
      e(d$signo_esperado[1]),
      e(en_que_ecuaciones(v))
    ))
  }
}

# --- 2. Resultados por país ----------------------------------------------

signo_obtenido <- function(x) ifelse(is.na(x), "—", ifelse(x >= 0, "+", "−"))

# Nombres cortos para armar la ecuación en una línea. La definición va en
# el cuadro de abajo, no en el nombre de la columna.
NOMBRE_CORTO <- c(
  rendimiento_soja_t_ha       = "RINDE",
  ln_precio_soja_cbot         = "CBOT",
  tcr_bis_2005_2025_100       = "TCR",
  tips_10a_pct                = "TIPS",
  tasa_real_local_pct         = "TASA",
  alicuota_export_soja_pct    = "RET",
  ln_ventas_maquinaria_por_ha = "MAQ",
  diesel_usd_l                = "DIESEL",
  ln_urea                     = "UREA",
  embi_pb                     = "EMBI",
  brecha_cambiaria_pct        = "BRECHA",
  precipitacion_desvio_mm     = "LLUVIA"
)

QUE_ES <- c(
  RINDE  = "Toneladas de soja por hectárea.",
  CBOT   = "Precio de la soja en Chicago, en logaritmo.",
  TCR    = "Tipo de cambio real. Subir es una moneda local más fuerte.",
  TIPS   = "Tasa real de Estados Unidos a 10 años.",
  TASA   = "Tasa real local, el costo real del dinero en el país.",
  RET    = "Alícuota de retenciones a la soja. Solo está en Argentina.",
  MAQ    = "Ventas del año de tractores y cosechadoras, divididas por las hectáreas de soja sembradas, en logaritmo.",
  DIESEL = "Precio del diésel minorista, en dólares por litro.",
  UREA   = "Precio internacional de la urea, en logaritmo.",
  EMBI   = "Riesgo país, en puntos básicos.",
  BRECHA = "Brecha entre el dólar paralelo y el mayorista, en puntos. Solo está en Argentina. Hasta 2012 es el blue y desde 2013 el contado con liquidación.",
  LLUVIA = "Milímetros de lluvia de más o de menos respecto del promedio del lugar."
)

se_distingue <- function(p) {
  if (length(p) != 1 || is.na(p)) return("no se puede evaluar")
  if (p < 0.01) "se distingue de cero al 1 %"
  else if (p < 0.05) "se distingue de cero al 5 %"
  else if (p < 0.10) "se distingue de cero solo al 10 %"
  else "no se distingue de cero"
}

signo_esperado_pais <- function(variable, pais) {
  if (variable == "tcr_bis_2005_2025_100") {
    return(if (pais == "Estados Unidos") "-" else "+")
  }
  if (is.null(diccionario)) return("")
  s <- diccionario$signo_esperado[diccionario$variable == variable]
  if (length(s) == 0 || is.na(s[1])) "" else s[1]
}

porque_difiere <- function(variable, pais, beta, p) {
  esp <- signo_esperado_pais(variable, pais)
  obt <- signo_obtenido(beta)
  if (!esp %in% c("+", "-")) return("")
  if ((esp == "+") == (obt == "+")) return("")
  nombre <- NOMBRE_CORTO[[variable]]
  if (is.na(nombre)) nombre <- variable
  motivo <- switch(variable,
    ln_ventas_maquinaria_por_ha =
      "Se esperaba un signo positivo: más tecnología hoy sube los rindes de los años que vienen, y ese precio futuro se capitaliza en la hectárea de hoy. La serie que entra es lo vendido ese año, no el parque de máquinas.",
    diesel_usd_l =
      "Se esperaba que un litro más caro abaratara la tierra, porque es un costo. Un litro más caro va con una tierra más cara.",
    tips_10a_pct =
      "Se esperaba que una tasa real internacional más alta abaratara la tierra, porque el campo se valúa descontando lo que rinde.",
    tasa_real_local_pct =
      "Se esperaba que una tasa real local más alta abaratara la tierra, por la misma razón.",
    alicuota_export_soja_pct =
      "Se esperaba que una alícuota más alta abaratara la tierra. La alícuota se mueve con el precio de la soja, así que este número no mide el efecto del impuesto.",
    embi_pb =
      "Se esperaba que más riesgo país abaratara la tierra.",
    brecha_cambiaria_pct =
      "Se esperaba que una brecha más ancha entre el dólar paralelo y el mayorista abaratara la tierra.",
    tcr_bis_2005_2025_100 = if (pais == "Estados Unidos") {
      "Se esperaba que una moneda más fuerte abaratara la hectárea de Iowa."
    } else {
      "Se esperaba que una moneda más fuerte encareciera la hectárea."
    },
    ""
  )
  if (variable == "diesel_usd_l" && pais == "Brasil") {
    motivo <- paste0(motivo, " En Brasil el diésel se mueve junto con la soja ",
                     "y con las ventas de maquinaria, y la ecuación no separa ",
                     "bien cuál de los tres es.")
  }
  if (!nzchar(motivo)) return("")
  paste0("<b>", nombre, ".</b> ", motivo, " Acá sale ",
         if (obt == "−") "negativo" else "positivo",
         " y ", se_distingue(p), ".")
}

# Las categorías viajan sin acento en los CSV; acá se muestran como se leen.
etiqueta_bloque <- function(x) {
  acentos <- c(economico = "económico")
  ifelse(x %in% names(acentos), unname(acentos[x]), x)
}

bloques_pais <- character(0)
if (hay_modelos && !is.null(coeficientes)) {
  for (pais in cfg$paises) {
    co <- coeficientes[coeficientes$pais == pais &
                         coeficientes$variable != "(Intercept)", ]
    aj <- ajuste[ajuste$pais == pais, ]

    filas_co <- vapply(seq_len(nrow(co)), function(i) {
      nombre <- NOMBRE_CORTO[[co$variable[i]]]
      if (is.na(nombre)) nombre <- co$variable[i]
      esp <- signo_esperado_pais(co$variable[i], pais)
      fila(
        paste0("<b>", e(nombre), "</b>"),
        num(co$coeficiente[i], 4),
        num(co$ee_hac[i], 4),
        num(co$estadistico_t[i], 2),
        chip_p(co$p_valor[i]),
        e(if (esp == "-") "−" else esp),
        paste0("<b>", signo_obtenido(co$coeficiente[i]), "</b>")
      )
    }, character(1))

    b0 <- coeficientes$coeficiente[coeficientes$pais == pais &
                                     coeficientes$variable == "(Intercept)"]
    terminos <- vapply(seq_len(nrow(co)), function(i) {
      nombre <- NOMBRE_CORTO[[co$variable[i]]]
      if (is.na(nombre)) nombre <- co$variable[i]
      b <- co$coeficiente[i]
      texto_b <- num(abs(b), 4)
      signo <- if (!is.na(b) && b < 0 && texto_b != "0,0000") " − " else " + "
      paste0(signo, texto_b, " ", nombre)
    }, character(1))
    base <- if (!is.na(b0[1]) && b0[1] < 0) paste0("−", num(abs(b0[1]), 4))
            else num(b0[1], 4)
    ecuacion <- paste0(
      "ln(precio) = ", base, paste(terminos, collapse = "")
    )
    motivos <- vapply(seq_len(nrow(co)), function(i) {
      porque_difiere(co$variable[i], pais, co$coeficiente[i], co$p_valor[i])
    }, character(1))
    motivos <- motivos[nzchar(motivos)]

    bloques_pais <- c(bloques_pais,
      paste0("<h3>", e(pais), "</h3>"),
      paste0("<p class='meta'>n = ", aj$n, " &middot; regresores = ",
             aj$regresores, " &middot; grados de libertad = ",
             aj$grados_libertad, " &middot; R<sup>2</sup> ajustado = <b>",
             num(aj$r2_ajustado), "</b> &middot; rezago HAC = ",
             aj$rezago_hac, "</p>"),
      paste0("<p style=\"font-family:'SF Mono',Menlo,Consolas,monospace;",
             "font-size:.92rem;line-height:1.7;background:#f1f1ef;",
             "padding:.8rem 1rem;border-radius:4px\">", e(ecuacion), "</p>"),
      tabla(c("variable", "coeficiente", "error estándar", "t",
              "se distingue del azar", "signo esperado", "signo obtenido"),
            filas_co, num_desde = 2),
      if (length(motivos) > 0) callout(
        "cnota", "Dónde el signo no coincide",
        paste(motivos, collapse = " ")
      ) else NULL
    )
  }
}

# Tecnología, diésel, urea y lluvia. El texto sale de los coeficientes y de
# la descomposición de la ventana, para no dejar números escritos a mano.
por_variable <- leer("tabla_descomposicion_por_variable")
por_pr <- solo_rol(por_variable, "principal")

palabra_signo <- function(b) {
  if (is.na(b) || b == 0) "nulo" else if (b > 0) "positivo" else "negativo"
}

frase_paises <- function(variable) {
  if (is.null(coeficientes)) return("")
  co <- coeficientes[coeficientes$variable == variable, ]
  if (nrow(co) == 0) return("")
  partes <- vapply(seq_len(nrow(co)), function(i) {
    paste0("en ", e(co$pais[i]), " es ", palabra_signo(co$coeficiente[i]),
           " y ", se_distingue(co$p_valor[i]))
  }, character(1))
  paste(partes, collapse = "; ")
}

aporte_brecha <- function(variable, comparable) {
  if (is.null(por_pr) || nrow(por_pr) == 0) return("")
  f <- por_pr[por_pr$variable == variable & por_pr$comparable == comparable, ]
  if (nrow(f) == 0) return("")
  lado <- if (f$contribucion_niveles_log[1] < 0) "en contra" else "a favor"
  paste0("frente a ", e(comparable), " mueve ",
         num(f$contribucion_niveles_log[1], 3),
         " puntos del logaritmo del precio (cerca de ",
         num(f$diferencia_porcentual[1], 1),
         " % de la hectárea), el ",
         num(abs(f$porcentaje_de_la_brecha[1]), 0),
         " % de la brecha, ", lado)
}

texto_nuevos <- NULL
if (hay_modelos && !is.null(coeficientes)) {
  maq <- frase_paises("ln_ventas_maquinaria_por_ha")
  die <- frase_paises("diesel_usd_l")
  llu <- frase_paises("precipitacion_desvio_mm")
  texto_nuevos <- paste0(
    "<b>Tecnología.</b> Son las ventas de maquinaria por hectárea sembrada, ",
    "no el parque de máquinas. Se esperaba un signo positivo: más tecnología ",
    "hoy sube los rindes de los años que vienen, y ese precio futuro se ",
    "capitaliza en la hectárea de hoy. Salió al revés: ", maq, ". En la ventana ",
    e(ventana_pr), ", ",
    aporte_brecha("ln_ventas_maquinaria_por_ha", "Brasil"),
    "; ", aporte_brecha("ln_ventas_maquinaria_por_ha", "Estados Unidos"),
    ". Mato Grosso e Iowa venden más maquinaria por hectárea que la zona ",
    "núcleo. Con el coeficiente negativo, la cuenta dice que esas dos ",
    "hectáreas deberían ser más baratas, y la brecha observada va para el ",
    "otro lado.",
    "</p><p><b>Diésel.</b> Reemplaza al flete. Se esperaba que un litro más ",
    "caro bajara el precio de la tierra. Salió al revés: ", die, ". En la ",
    "misma ventana ", aporte_brecha("diesel_usd_l", "Brasil"), "; ",
    aporte_brecha("diesel_usd_l", "Estados Unidos"),
    ". Mueve poco la comparación entre países.",
    "</p><p><b>Urea.</b> Es el precio internacional del fertilizante, el mismo ",
    "en los tres países, y entra como costo. ",
    paste0(toupper(substring(frase_paises("ln_urea"), 1, 1)),
           substring(frase_paises("ln_urea"), 2)),
    ". Como el precio es el mismo, no explica por qué una hectárea vale más ",
    "en un país que en otro: mueve el ajuste de cada ecuación. 2025 es el ",
    "promedio de los doce meses. Desde marzo de 2022 la cotización es ",
    "f.o.b. Medio Oriente; antes era f.o.b. Mar Negro.",
    "</p><p><b>Lluvia.</b> Es cuánto llovió de más o de menos respecto del ",
    "promedio de cada lugar. No tiene un signo esperado. ",
    paste0(toupper(substring(llu, 1, 1)), substring(llu, 2)),
    ". En la ventana ", aporte_brecha("precipitacion_desvio_mm", "Brasil"),
    "; ", aporte_brecha("precipitacion_desvio_mm", "Estados Unidos"),
    ". No mueve la brecha.",
    "</p><p>El crédito no entra. Argentina tiene el saldo del BCRA y Estados ",
    "Unidos el de la Fed, los 21 años. Brasil no tiene esa serie antes de ",
    "2012, y ni repetir la variación ni estimarla con la tasa y el riesgo ",
    "reproduce el saldo medido."
  )
  if (!is.null(diagnosticos)) {
    altos <- diagnosticos$pais == "Brasil" &
      diagnosticos$prueba %in% c(
        "VIF de ln_precio_soja_cbot",
        "VIF de ln_ventas_maquinaria_por_ha",
        "VIF de diesel_usd_l") &
      diagnosticos$estadistico > 10
    if (any(altos)) {
      texto_nuevos <- paste0(
        texto_nuevos,
        "</p><p>En Brasil el precio de la soja, el diésel y las ventas de ",
        "maquinaria se mueven juntos. La ecuación no distingue bien cuál de ",
        "los tres explica el precio de la hectárea.")
    }
  }
}

# La lectura de H1 sale de la tabla, no de un texto fijo. La confirmación
# parcial queda en pie solo si el bloque productivo es negativo y esa
# negatividad la pone la tecnología.
texto_lectura_h1 <- paste0(
  "H1 compara el tamaño de cada bloque, con signo. El veredicto de arriba ",
  "es el de la cuenta."
)
if (!is.null(h1_pr) && nrow(h1_pr) > 0 && !is.null(por_pr)) {
  br <- h1_pr[h1_pr$comparable == "Brasil", ]
  us <- h1_pr[h1_pr$comparable == "Estados Unidos", ]
  maq_br <- por_pr[por_pr$variable == "ln_ventas_maquinaria_por_ha" &
                     por_pr$comparable == "Brasil", ]
  if (nrow(br) == 1 && nrow(maq_br) == 1 &&
      br$productivo[1] < 0 &&
      maq_br$contribucion_niveles_log[1] <= br$productivo[1]) {
    texto_lectura_h1 <- paste0(
      "Frente a Brasil la cuenta da que sí, porque el bloque productivo es ",
      "negativo (", num(br$productivo[1], 3), " puntos) y el institucional, ",
      "con ", num(br$institucional[1], 3),
      ", le queda por encima. Ese negativo lo pone la tecnología: las ventas ",
      "de maquinaria salen con el signo al revés y restan ",
      num(abs(maq_br$contribucion_niveles_log[1]), 3),
      " puntos, más que el rendimiento, el diésel, la urea y la lluvia ",
      "juntos. La ",
      "hipótesis se cumple en la cuenta, y se cumple por ese signo. No es ",
      "evidencia de que las instituciones expliquen la brecha con Mato Grosso."
    )
    if (nrow(us) == 1 && !isTRUE(us$cumple_en_este_comparable[1])) {
      texto_lectura_h1 <- paste0(
        texto_lectura_h1,
        " Frente a Iowa no se cumple: el bloque económico (",
        num(us$economico[1], 3), ") supera al institucional (",
        num(us$institucional[1], 3), ")."
      )
    }
  }
}

# --- 3. Significatividad por bloque ---------------------------------------

ENTERA <- "la ecuación entera"

filas_bloques <- character(0)
if (!is.null(pr_bloques) && "estadistico_f" %in% names(pr_bloques)) {
  filas_bloques <- vapply(seq_len(nrow(pr_bloques)), function(i) {
    entera <- identical(pr_bloques$bloque[i], ENTERA)
    resaltar <- function(x) if (entera) paste0("<b>", x, "</b>") else x
    # El país se escribe solo en su primera fila: la tabla se lee por grupos.
    nuevo_pais <- i == 1 || pr_bloques$pais[i] != pr_bloques$pais[i - 1]
    fila(
      if (nuevo_pais) paste0("<b>", e(pr_bloques$pais[i]), "</b>") else "",
      resaltar(e(etiqueta_bloque(pr_bloques$bloque[i]))),
      pr_bloques$n_variables[i],
      resaltar(num(pr_bloques$estadistico_f[i], 2)),
      chip_p(pr_bloques$p_valor[i])
    )
  }, character(1))
}

# El párrafo de lectura se arma con los resultados, no a mano: si cambia un
# p-valor, cambia la frase.
texto_bloques <- ""
if (length(filas_bloques) > 0) {
  # "y" pasa a "e" delante de palabra que empieza con i (y institucional no).
  unir <- function(x) {
    if (length(x) <= 1) return(paste(x, collapse = ""))
    ultimo <- utils::tail(x, 1)
    nexo <- if (grepl("^i", ultimo)) " e " else " y "
    paste0(paste(utils::head(x, -1), collapse = ", "), nexo, ultimo)
  }
  enumerar <- function(x, singular, plural) {
    if (length(x) == 0) return(NULL)
    if (length(x) == 1) paste0(singular, " el bloque ", unir(x))
    else paste0(plural, " los bloques ", unir(x))
  }
  todos_los_bloques <- setdiff(unique(pr_bloques$bloque), ENTERA)
  frase_pais <- function(p) {
    b <- pr_bloques[pr_bloques$pais == p & pr_bloques$bloque != ENTERA, ]
    si <- etiqueta_bloque(b$bloque[b$p_valor <  cfg$nivel_bloques])
    no <- etiqueta_bloque(b$bloque[b$p_valor >= cfg$nivel_bloques])
    # Un bloque puede no existir en un país: la ecuación no lleva ninguna de
    # sus variables. Eso no es lo mismo que un bloque que no llega.
    sin <- etiqueta_bloque(setdiff(todos_los_bloques, b$bloque))
    partes <- c(enumerar(si, "llega", "llegan"),
                enumerar(no, "no llega", "no llegan"),
                enumerar(sin, "no existe", "no existen"))
    paste0("En <b>", e(p), "</b> ", paste(partes, collapse = ", y "), ".")
  }
  texto_bloques <- paste(vapply(cfg$paises, frase_pais, character(1)),
                         collapse = " ")

  # El contraste solo se afirma si los datos lo sostienen.
  inst <- pr_bloques[pr_bloques$bloque == "institucional", ]
  solo_ar <- nrow(inst) > 0 &&
    identical(inst$pais[inst$p_valor < cfg$nivel_bloques], "Argentina")
  if (solo_ar) {
    texto_bloques <- paste0(
      texto_bloques, " Ese contraste es el resultado central de esta sección: ",
      "<b>Argentina es el único de los tres países donde lo que explica el ",
      "precio de la tierra es el bloque institucional</b>, y no la ",
      "productividad ni los precios internacionales.")
  }
  inst_ar <- if (is.null(coeficientes)) NULL else {
    coeficientes[coeficientes$pais == "Argentina" &
                   coeficientes$categoria == "institucional", ]
  }
  if (!is.null(inst_ar) && nrow(inst_ar) > 0 &&
      all(inst_ar$p_valor >= cfg$nivel_bloques)) {
    texto_bloques <- paste0(
      texto_bloques, " En la sección anterior no se ve: ahí ninguna de las ",
      "dos variables institucionales argentinas llega al 5 % por separado.")
  }
}

# --- 4. Capitalización del precio de la soja ------------------------------

SUBA <- cfg$suba_referencia_pct
DEL_TRABAJO <- "ecuación del trabajo"

# Porcentaje con una decimal. Va aparte de pc() de html_comun.R porque acá
# el valor puede venir en vector (una fila por especificación).
pc <- function(x) paste0(num(x, 1), " %")

# El veredicto no usa los chips de significatividad habituales: acá rechazar
# es el hallazgo, no un fracaso, así que la etiqueta dice qué pasó.
chip_capitaliza <- function(p) {
  if (is.na(p)) return("<span class='chip gris'>—</span>")
  if (p < 0.01) return("<span class='chip ambar'>sí, al 1 %</span>")
  if (p < 0.05) return("<span class='chip ambar'>sí, al 5 %</span>")
  "<span class='chip gris'>no se distingue</span>"
}

filas_capital <- character(0)
if (!is.null(capital) && "suba_tierra_pct" %in% names(capital)) {
  filas_capital <- vapply(seq_len(nrow(capital)), function(i) {
    propia <- identical(capital$especificacion[i], DEL_TRABAJO)
    etiqueta <- if (propia) "la del trabajo" else {
      paste0("<span class='sub'>", e(capital$especificacion[i]), "</span>")
    }
    fila(
      paste0("<b>", e(capital$pais[i]), "</b>"),
      etiqueta,
      paste0("<b>", pc(capital$suba_tierra_pct[i]), "</b>"),
      paste0(pc(capital$suba_tierra_pct_ic_inferior[i]), " a ",
             pc(capital$suba_tierra_pct_ic_superior[i])),
      chip_capitaliza(capital$p_igual_uno[i])
    )
  }, character(1))
}

texto_capital <- ""
texto_no_dice <- ""
if (length(filas_capital) > 0) {
  rechaza <- capital$p_igual_uno < 0.05
  comparable <- capital[capital$especificacion != DEL_TRABAJO, ]

  # Cada frase sale de la fila de la tabla. Si un país pasa o no el 10 %,
  # lo dice el p-valor de esa fila, no un texto escrito de antemano.
  fila_pais <- function(pais, cual = DEL_TRABAJO) {
    f <- capital[capital$pais == pais & capital$especificacion == cual, ]
    if (nrow(f) == 0) NULL else f[1, ]
  }
  distingue_plena <- function(f) {
    !is.null(f) && !is.na(f$p_igual_uno) && f$p_igual_uno < 0.05
  }
  frase_suba <- function(f, quien) {
    if (is.null(f)) return("")
    cierre <- if (distingue_plena(f)) {
      paste0("eso sí se distingue de ", SUBA, " %")
    } else {
      paste0("eso no se distingue de ", SUBA, " %")
    }
    paste0(quien, " sube <b>", pc(f$suba_tierra_pct), "</b>, y ", cierre,
           " (el rango va de ", pc(f$suba_tierra_pct_ic_inferior),
           " a ", pc(f$suba_tierra_pct_ic_superior), ")")
  }
  ar <- fila_pais("Argentina")
  br <- fila_pais("Brasil")
  us <- fila_pais("Estados Unidos")

  texto_no_dice <- paste0(
    "<b>El rango argentino es ancho.</b> Va de ",
    pc(ar$suba_tierra_pct_ic_inferior), " a ",
    pc(ar$suba_tierra_pct_ic_superior),
    ": adentro entran una hectárea que casi no se mueve y una que sigue a ",
    "la soja. La urea se mueve junto con la soja, y el coeficiente de la ",
    "soja queda mezclado con ese costo.")
  if (!is.null(us) && distingue_plena(us)) {
    texto_no_dice <- paste0(
      texto_no_dice, " En Iowa la suba queda por debajo del ", SUBA,
      " % en el límite del 5 %: el rango llega a ",
      pc(us$suba_tierra_pct_ic_superior), ", así que la diferencia es fina.")
  } else if (!is.null(us)) {
    texto_no_dice <- paste0(
      texto_no_dice, " En Iowa el rango también incluye al ", SUBA,
      " %, así que esa hectárea puede estar siguiendo a la soja.")
  }

  texto_capital <- paste0(
    "La soja es la misma para los tres: el mismo precio de Chicago, el ",
    "mismo número. Si sube ", SUBA, " %, ",
    frase_suba(br, "la hectárea de Mato Grosso"), ". ",
    frase_suba(us, "La de Iowa"), ". ",
    frase_suba(ar, "La de la zona núcleo"), ".")

  if (nrow(comparable) > 0) {
    texto_capital <- paste0(
      texto_capital, " Con la lista de variables de Brasil, que es la ",
      "comparación pareja, ",
      frase_suba(comparable[1, ], "la hectárea argentina"), ".")
  }

  # El punto que vale la tesis solo se afirma si los números lo sostienen.
  solo_argentina <- all(capital$pais[rechaza] == "Argentina") && any(rechaza)
  if (solo_argentina) {
    texto_capital <- paste0(
      texto_capital, " <b>Argentina es el único de los tres países donde el ",
      "precio internacional no llega entero a la tierra.</b> Algo se lo come ",
      "en el camino, y no es el clima ni el rendimiento, que ya están ",
      "controlados en la misma ecuación.")
  }
}

# --- 5. Colinealidad: el antes y el después -------------------------------

# Se vuelve a estimar la especificación vieja, con la inflación y la tasa
# nominal por separado, para mostrar de dónde se venía. No se guarda: es
# solo el término de comparación.
par_viejo <- c("inflacion_dic_pct", "tasa_nominal_local_pct")
nuevo     <- "tasa_real_local_pct"
filas_colin <- character(0)
if (hay_modelos) {
  panel_c <- utils::read.csv(file.path(cfg$dir_curados, "panel.csv"),
                             stringsAsFactors = FALSE)
  for (pais in cfg$paises) {
    regs_hoy <- cfg$regresores[[pais]]
    if (!nuevo %in% regs_hoy) next
    regs_antes <- c(setdiff(regs_hoy, nuevo), par_viejo)
    resumen_de <- function(regs) {
      d <- panel_c[panel_c$pais == pais, c(cfg$dependiente, regs)]
      d <- d[stats::complete.cases(d), ]
      if (nrow(d) == 0) return(NULL)
      m <- stats::lm(stats::reformulate(regs, cfg$dependiente), data = d)
      v <- vif_manual(m)
      list(vif = max(v$vif), peor = v$variable[which.max(v$vif)],
           altos = sum(v$vif > 10), r2 = summary(m)$adj.r.squared,
           gl = m$df.residual)
    }
    a <- resumen_de(regs_antes)
    b <- resumen_de(regs_hoy)
    if (is.null(a) || is.null(b)) next
    filas_colin <- c(filas_colin, fila(
      paste0("<b>", e(pais), "</b>"),
      paste0(num(a$vif, 1), " <span class='sub'>(", e(a$peor), ")</span>"),
      paste0("<b>", num(b$vif, 1), "</b> <span class='sub'>(", e(b$peor),
             ")</span>"),
      paste0(a$altos, " &rarr; <b>", b$altos, "</b>"),
      paste0(num(a$r2), " &rarr; <b>", num(b$r2), "</b>"),
      paste0(a$gl, " &rarr; <b>", b$gl, "</b>")
    ))
  }
}

# --- 6. Endogeneidad de la variable fiscal --------------------------------

filas_endo <- character(0)
if (!is.null(endogeneidad) && "coeficiente" %in% names(endogeneidad)) {
  filas_endo <- vapply(seq_len(nrow(endogeneidad)), function(i) fila(
    e(endogeneidad$pais[i]),
    e(endogeneidad$explicativa[i]),
    endogeneidad$n[i],
    num(endogeneidad$coeficiente[i], 2),
    num(endogeneidad$estadistico_t[i], 2),
    chip_p(endogeneidad$p_valor[i]),
    num(endogeneidad$r2[i])
  ), character(1))
}

# --- 7. Hipótesis ---------------------------------------------------------

filas_h1 <- character(0)
if (!is.null(h1_pr) && "institucional" %in% names(h1_pr)) {
  filas_h1 <- vapply(seq_len(nrow(h1_pr)), function(i) fila(
    e(h1_pr$comparable[i]),
    num(h1_pr$institucional[i]),
    num(h1_pr$productivo[i]),
    num(h1_pr$economico[i]),
    if (isTRUE(h1_pr$cumple_en_este_comparable[i]))
      "<span class='chip verde'>sí</span>" else
      "<span class='chip rojo'>no</span>"
  ), character(1))
}

veredicto_h1 <- if (!is.null(h1_pr) && "veredicto_global" %in% names(h1_pr)) {
  unique(h1_pr$veredicto_global)[1]
} else "No se evaluó: faltan las ecuaciones por país."

veredicto_h2 <- if (!is.null(h2_pr) && "veredicto" %in% names(h2_pr)) {
  paste0("<b>", e(h2_pr$veredicto[1]), "</b>",
         if (nzchar(h2_pr$motivo[1])) paste0(" — ", e(h2_pr$motivo[1])) else "")
} else "No se evaluó: falta la ecuación argentina."

brechas <- character(0)
if (!is.null(cat_pr)) {
  tot <- cat_pr[grepl("^Brecha total", cat_pr$componente), ]
  brechas <- vapply(seq_len(nrow(tot)), function(i) fila(
    e(tot$comparable[i]),
    num(tot$contribucion_log[i], 4),
    paste0(num(tot$diferencia_porcentual[i], 1), " %")
  ), character(1))
}

# --- Evolución de la brecha año por año -----------------------------------
# Es la evidencia del quiebre y la razón de que las hipótesis se evalúen en
# una ventana y no en el promedio de los veintiún años.
filas_brecha_anual <- character(0)
filas_resumen_brecha <- character(0)
if (!is.null(panel) && cfg$dependiente %in% names(panel)) {
  dep <- cfg$dependiente
  anios <- sort(unique(panel$anio))
  brecha_de <- function(p) vapply(anios, function(a) {
    c_p <- panel[[dep]][panel$pais == p & panel$anio == a]
    c_a <- panel[[dep]][panel$pais == "Argentina" & panel$anio == a]
    if (length(c_p) == 1 && length(c_a) == 1) c_p - c_a else NA_real_
  }, numeric(1))
  g_br <- brecha_de("Brasil")
  g_us <- brecha_de("Estados Unidos")
  en_ventana <- anios >= cfg$ventana_medias[1] & anios <= cfg$ventana_medias[2]

  filas_brecha_anual <- vapply(seq_along(anios), function(i) {
    destacar <- function(x) if (en_ventana[i]) paste0("<b>", x, "</b>") else x
    fila(destacar(anios[i]), destacar(num(g_br[i], 3)),
         destacar(num(g_us[i], 3)))
  }, character(1))

  bloque <- function(etiqueta, filtro) fila(
    e(etiqueta), num(mean(g_br[filtro], na.rm = TRUE), 3),
    num(mean(g_us[filtro], na.rm = TRUE), 3))
  filas_resumen_brecha <- c(
    bloque(paste0("antes de la ventana (", min(anios), "-",
                  cfg$ventana_medias[1] - 1, ")"), !en_ventana),
    bloque(paste0("ventana (", ventana_pr, ")"), en_ventana),
    bloque(paste0("período completo (", ventana_an, ")"), rep(TRUE, length(anios)))
  )
}

# --- Componentes de la descomposición -------------------------------------
etiqueta_componente <- c(
  "Niveles: factores productivos" = "niveles — productivos",
  "Niveles: factores económicos" = "niveles — económicos",
  "Niveles: factores institucionales" = "niveles — institucionales",
  "Componente de niveles (total)" = "niveles (total)",
  "Componente de valoración (residual, solo agregado)" = "valoración",
  "Residuo: parte de la brecha que la ecuación no explica en la ventana" =
    "residuo no explicado"
)

filas_componentes <- function(tab) {
  if (is.null(tab) || !"componente" %in% names(tab)) return(character(0))
  salida <- character(0)
  for (comp in unique(tab$comparable)) {
    sub <- tab[tab$comparable == comp, ]
    br <- sub$contribucion_log[grepl("^Brecha total", sub$componente)]
    for (nombre in names(etiqueta_componente)) {
      f <- sub[sub$componente == nombre, ]
      if (nrow(f) == 0) next
      pct <- if (length(br) == 1 && abs(br) >= 0.10) {
        paste0(num(f$contribucion_log[1] / br * 100, 0), " %")
      } else "—"
      salida <- c(salida, fila(
        e(comp), e(etiqueta_componente[[nombre]]),
        num(f$contribucion_log[1], 3),
        paste0(num(f$ic_inferior[1], 2), " a ", num(f$ic_superior[1], 2)),
        pct))
    }
    if (length(br) == 1) {
      salida <- c(salida, fila(
        paste0("<b>", e(comp), "</b>"), "<b>brecha total</b>",
        paste0("<b>", num(br, 3), "</b>"), "—", "<b>100 %</b>"))
    }
  }
  salida
}
filas_comp_pr <- filas_componentes(cat_pr)
filas_comp_an <- filas_componentes(cat_an)

# Desarme del bloque institucional en la ventana principal. Se calcula acá
# para que el texto no lleve números escritos a mano.
detalle_var <- leer("tabla_descomposicion_por_variable")
texto_institucional <- NULL
if (!is.null(detalle_var) && "ventana" %in% names(detalle_var)) {
  d <- detalle_var[detalle_var$ventana == ventana_pr &
                     detalle_var$comparable == "Brasil" &
                     detalle_var$categoria == "institucional", ]
  brecha_br <- cat_pr$contribucion_log[cat_pr$comparable == "Brasil" &
                                grepl("^Brecha total", cat_pr$componente)]
  aporte <- function(v) {
    f <- d[d$variable == v, ]
    if (nrow(f) == 1) f else NULL
  }
  ali <- aporte("alicuota_export_soja_pct")
  emb <- aporte("embi_pb")
  gap <- aporte("brecha_cambiaria_pct")
  if (nrow(d) > 0 && !is.null(ali) && !is.null(emb) && length(brecha_br) == 1) {
    pedazo_gap <- if (!is.null(gap)) {
      paste0(" y ", num(gap$contribucion_niveles_log, 3),
             " de la brecha cambiaria")
    } else ""
    lectura_gap <- if (!is.null(gap)) {
      paste0(" La brecha cambiaria también se lee con su coeficiente: en la ",
             "ventana promedió ", num(gap$media_argentina, 0),
             " puntos en Argentina, y la cuenta le pone cero a Brasil porque ",
             "no hay serie. Eso explica ",
             num(gap$contribucion_niveles_log, 3), " puntos de la brecha, un ",
             num(gap$contribucion_niveles_log / brecha_br * 100, 0),
             " % de ella. La serie cambia de cotización en 2013.")
    } else ""
    texto_institucional <- paste0(
      "De los ", num(sum(d$contribucion_niveles_log), 3), " puntos ",
      "institucionales frente a Brasil, ",
      num(ali$contribucion_niveles_log, 3), " vienen de la alícuota, ",
      num(emb$contribucion_niveles_log, 3), " del riesgo soberano",
      pedazo_gap, ". El de la alícuota no es un hallazgo: sale de ",
      "multiplicar el coeficiente argentino, que es ",
      "<b>endógeno y de signo equivocado</b> (sección 6), por una media ",
      "brasileña que vale cero <b>por convención</b>, porque la variable no ",
      "existe en la ecuación de Brasil. El EMBI sí se mide en los dos países: ",
      "Argentina promedió ",
      miles(emb$media_argentina), " puntos básicos en la ventana contra ",
      miles(emb$media_comparable), " de Brasil, y esa diferencia explica ",
      num(emb$contribucion_niveles_log, 3), " puntos de la brecha, o sea un ",
      num(emb$contribucion_niveles_log / brecha_br * 100, 0), " % de ella.",
      lectura_gap)
  }
}

# Recorrido del precio real, para mostrar que el quiebre no lo produce el
# deflactor. También se calcula en vez de escribirse.
texto_recorrido <- NULL
if (!is.null(panel) && "precio_ha_usd_2025" %in% names(panel)) {
  recorrido <- function(p) {
    s <- panel[panel$pais == p & !is.na(panel$precio_ha_usd_2025), ]
    s <- s[order(s$anio), ]
    if (nrow(s) == 0) return(NULL)
    pico <- which.max(s$precio_ha_usd_2025)
    posterior <- s[seq(pico, nrow(s)), ]
    valle <- posterior[which.min(posterior$precio_ha_usd_2025), ]
    list(pico = s[pico, ], valle = valle, ultimo = s[nrow(s), ],
         primero = s[1, ])
  }
  ar <- recorrido("Argentina")
  br <- recorrido("Brasil")
  if (!is.null(ar) && !is.null(br)) {
    texto_recorrido <- paste0(
      "En dólares constantes de ", cfg$anio_base_precios, " la hectárea ",
      "argentina pasó de un máximo de ", miles(ar$pico$precio_ha_usd_2025),
      " en ", ar$pico$anio, " a ", miles(ar$valle$precio_ha_usd_2025), " en ",
      ar$valle$anio, ", y con ", miles(ar$ultimo$precio_ha_usd_2025), " en ",
      ar$ultimo$anio, " sigue por debajo de lo que valía hace más de una ",
      "década. Brasil, en el mismo lapso, pasó de ",
      miles(br$primero$precio_ha_usd_2025), " en ", br$primero$anio, " a un ",
      "máximo de ", miles(br$pico$precio_ha_usd_2025), " en ", br$pico$anio,
      ". La divergencia es real: no la produce el deflactor ni un año suelto.")
  }
}

# --- Armado del documento -------------------------------------------------

filas_nombres <- vapply(names(NOMBRE_CORTO), function(v) {
  nombre <- unname(NOMBRE_CORTO[[v]])
  esp <- if (v == "tcr_bis_2005_2025_100") {
    "+ en Argentina y Brasil, − en Iowa"
  } else {
    s <- signo_esperado_pais(v, "Argentina")
    if (s == "-") "−" else s
  }
  fila(paste0("<b>", e(nombre), "</b>"), e(QUE_ES[[nombre]]), e(esp))
}, character(1))

hoy <- format(Sys.Date(), "%d/%m/%Y")
fig <- "figura_1_precio_por_hectarea.png"
fig_sim <- "figura_2_simulacion.png"
hay_fig <- file.exists(file.path(cfg$dir_resultados, fig))
hay_fig_sim <- file.exists(file.path(cfg$dir_resultados, fig_sim))

doc <- c(
encabezado_html(
  "Brecha de precio de la tierra agrícola — resultados",
  "Brecha de precio de la tierra agrícola",
  paste0("Zona núcleo, Mato Grosso e Iowa, ", cfg$anio_min, "-", cfg$anio_max,
         ". Resultados de la corrida del ", hoy, ".")),

callout("cinfo", "Qué estima el trabajo",
  paste0("Una ecuación por país del logaritmo del precio real de la hectárea ",
         "(<code>", e(cfg$dependiente), "</code>, dólares de 2025), con ",
         "errores estándar Newey-West. Sobre esas tres ecuaciones se arma ",
         "una descomposición de Oaxaca-Blinder a mano, con los coeficientes ",
         "argentinos como referencia, que parte la brecha entre un ",
         "componente de <b>niveles</b> (cuánto difieren los factores) y uno ",
         "de <b>valoración</b> (cuánto difiere lo que el mercado paga por ",
         "cada factor). Son 21 años por país. Las ecuaciones se estiman ",
         "sobre los 21 años; las hipótesis se responden sobre las medias de ",
         e(ventana_pr), ", porque la brecha cambia de signo adentro del ",
         "período (sección 7).")),

if (hay_fig) paste0('<img src="', fig, '" alt="Precio por hectárea">') else NULL,
if (hay_fig_sim) c(
  paste0('<img src="', fig_sim, '" alt="Precio pagado y precio que devuelve la ecuación">'),
  '<p class="meta">La línea simulada es el precio que devuelve la ecuación de cada país con los datos de ese año. No se cambia ningún dato. En Iowa 2005 y 2006, y en Brasil 2020, la línea se corta porque falta un dato y no se completa.</p>'
) else NULL,

'<h2>1. De dónde sale cada factor</h2>',
'<p>Cada fila es una variable que efectivamente entra a alguna ecuación. La',
' columna de transformación dice qué se le hizo al dato crudo; el dato crudo',
' nunca se modifica.</p>',
tabla(c("variable", "qué mide", "unidad", "fuente", "transformación",
        "signo", "ecuaciones"), filas_factores,
      anchos = c(13, 20, 9, 23, 19, 9, 7)),

'<h2>2. Cómo quedó cada regresión</h2>',
if (hay_modelos) c(
  '<p>Primero la ecuación, con los nombres cortos. Después, qué es cada nombre y qué signo se esperaba. El error estándar corrige que el error de un año se parezca al del anterior.</p>',
  callout("cinfo", "Cómo se leen las columnas",
    paste0("<b>Coeficiente.</b> Cuánto se mueve el logaritmo del precio de la hectárea cuando esa variable sube una unidad y el resto queda quieto. En la soja y en la urea, que también están en logaritmo, se lee como un porcentaje: 0,49 quiere decir que la hectárea sube cerca de 4,9 % cuando la soja sube 10 %.",
           "<br><b>Error estándar.</b> La imprecisión de ese coeficiente. Es el EE.",
           "<br><b>t.</b> El coeficiente dividido por su error estándar. Si es grande en valor absoluto, el coeficiente se distingue del azar.",
           "<br><b>Se distingue del azar.</b> 1 % y 5 % quieren decir que sí. 10 % es débil. «No sig.» quiere decir que no se distingue de cero. El mismo cartel se usa en la tabla de bloques.")),
  tabla(c("nombre", "qué es", "signo esperado"), filas_nombres, num_desde = 4)
) else '<p class="vacio">Las ecuaciones no se estimaron en esta corrida.</p>',
bloques_pais,
if (!is.null(texto_nuevos)) callout(
  "cnota", "Tecnología, diésel, urea y lluvia", texto_nuevos) else NULL,

'<h2>3. Qué bloque de factores mueve el precio en cada país</h2>',
callout("cnota", "Por qué se mira el bloque y no cada variable por separado",
  paste0("La tabla anterior mira una variable por vez. Con veintiún años eso ",
         "le juega en contra a los bloques cuyas variables se mueven juntas: ",
         "la ecuación no puede repartir el mérito entre ellas, así que ",
         "ninguna sobresale sola aunque el grupo sí importe. Acá se prueba el ",
         "bloque entero de una vez, que además es la pregunta que ",
         "corresponde, porque <b>H1 está escrita sobre un grupo de factores</b> ",
         "y no sobre una variable suelta. La prueba no cambia ningún ",
         "coeficiente ni ninguna ecuación: cambia qué se le pregunta a la ",
         "ecuación ya estimada.")),
tabla(c("país", "bloque", "variables", "F conjunto", "se distingue del azar"),
      filas_bloques, num_desde = 3),
'<p class="sub">El p-valor es el de la versión F de la prueba de Wald, que',
' compara el estadístico contra los grados de libertad que quedan. La versión',
' asintótica, la de manual, supone muestra grande y con veintiún años rechaza',
' de más; queda en el CSV para que la diferencia esté a la vista.</p>',
if (nzchar(texto_bloques)) callout("cinfo", "Lo que dice la tabla", texto_bloques)
  else NULL,
callout("calerta", "Dos cosas que esta prueba no hace",
  paste0("<b>No arregla la endogeneidad de las retenciones</b> (sección 6). ",
         "Que el bloque institucional resulte significativo dice que esas ",
         "variables están asociadas al precio de la tierra en Argentina, no ",
         "que lo causen; la mitad limpia del bloque es el riesgo país, que ",
         "sí tiene el signo esperado y no arrastra ese problema. Y <b>no ",
         "cambia el veredicto de H1</b>, que compara el tamaño de las ",
         "contribuciones en la descomposición y no si los coeficientes son ",
         "cero. Lo que agrega es la explicación de por qué ese rechazo no ",
         "significa que lo institucional no importe.")),
'<p class="sub">Las variables de un bloque no se combinan en un índice único.',
' Habría que elegir una ponderación y ninguna teoría dice cuál: las',
' retenciones recortan el ingreso y el riesgo país encarece la tasa de',
' descuento, que son lugares distintos de la valuación. La regresión ya',
' pondera, con coeficientes que salen de los datos. La única combinación que',
' el trabajo hace es la tasa real, y ahí la ponderación la fija una identidad',
' exacta (sección 5) y no una decisión.</p>',

paste0('<h2>4. Cuánto sube la tierra si la soja sube ', SUBA, ' %</h2>'),
callout("cnota", "Por qué esta pregunta se puede hacer, y por qué importa",
  paste0("El precio de la tierra y el de la soja están los dos en ",
         "logaritmo, así que el coeficiente de la soja se lee directo: ",
         "cuánto sube la hectárea, en porcentaje, cuando la soja sube. Si ",
         "la soja sube ", SUBA, " % y la hectárea sube ", SUBA, " %, el ",
         "campo está absorbiendo el precio internacional entero. Y ese no ",
         "es un número cualquiera: es lo que predice la valuación por ",
         "ingreso del marco teórico. Un campo vale lo que vale el ingreso ",
         "que va a generar, así que si el precio internacional sube de ",
         "forma permanente y nada más cambia, el ingreso por hectárea sube ",
         "en la misma proporción y el campo tendría que subir lo mismo.")),
tabla(c("país", "ecuación", paste0("si la soja sube ", SUBA, " %, la tierra sube"),
        "rango de confianza 95 %", paste0("¿se distingue de ", SUBA, " %?")),
      filas_capital, anchos = c(14, 24, 20, 22, 20), num_desde = 3),
if (nzchar(texto_capital)) callout("cinfo", "Lo que dice la tabla", texto_capital)
  else NULL,
callout("cnota", "Por qué Argentina aparece dos veces",
  paste0("La ecuación argentina controla por la alícuota y la brasileña no ",
         "la tiene, porque en Brasil no existen las retenciones. Con la ",
         "alícuota adentro, el coeficiente de la soja mide el efecto <b>a ",
         "retención fija</b>, y en Brasil mide el efecto <b>total</b>. ",
         "Comparar esos dos números sería comparar cosas distintas. Por eso ",
         "la tabla repite Argentina con la misma lista de variables que ",
         "Brasil: esa es la fila que hay que mirar para la comparación.")),
callout("cinfo", "Este resultado esquiva el problema de las retenciones",
  paste0("La prueba <b>no usa el coeficiente de la alícuota</b>, que es el ",
         "que no admite lectura causal (sección 6). Pregunta cuánto de la ",
         "soja llega a la tierra con las ecuaciones ya estimadas, así que ",
         "no hereda ese límite.")),
if (nzchar(texto_no_dice)) callout(
  "calerta", "Lo que este resultado no dice", texto_no_dice) else NULL,

'<h2>5. Cómo se resolvió la colinealidad en Argentina</h2>',
callout("cnota", "El problema",
  paste0("La ecuación llevaba la <b>inflación de cierre</b> y la <b>tasa ",
         "nominal local</b> por separado. Por la relación de Fisher son el ",
         "mismo concepto: la tasa nominal es la real más la inflación ",
         "esperada. En Argentina el 98 % de la BADLAR era predecible a ",
         "partir de los demás regresores. Eso no sesga nada, pero infla los ",
         "errores estándar: con un VIF de 63,8 el error estándar queda ocho ",
         "veces más grande de lo que sería sin colinealidad, así que ningún ",
         "coeficiente individual resulta confiable.")),
callout("cinfo", "La solución",
  paste0("Las dos se combinan en una sola variable, la <b>tasa real ",
         "local</b>, usando Fisher exacto y no la resta aproximada: ",
         "<code>r = (1 + i) / (1 + &pi;) &minus; 1</code>. La resta ",
         "<code>i &minus; &pi;</code> solo funciona con inflación baja, y la ",
         "argentina llega a tres dígitos en el período. La inflación y la ",
         "tasa nominal siguen en el panel como descriptivas. Se probaron ",
         "también dejar solo una de las dos: las tres opciones eliminan la ",
         "colinealidad, pero la tasa real es la que mejor ajusta en los tres ",
         "países y es la única que no tira información.")),
tabla(c("país", "VIF máximo antes", "VIF máximo ahora", "VIF &gt; 10",
        "R² ajustado", "grados de libertad"), filas_colin),
'<p class="sub">El “antes” se vuelve a estimar en cada corrida con la',
' especificación vieja, así que la comparación no son números escritos a mano.</p>',

'<h2>6. El problema de endogeneidad de las retenciones</h2>',
callout("calerta", "El coeficiente de la alícuota no admite lectura causal",
  paste0("Sale con signo positivo, o sea que más retenciones se asocian con ",
         "tierra más cara, al revés de lo que postula el trabajo. La razón ",
         "no es un error de medición: la alícuota <b>no es independiente de ",
         "lo que determina el precio de la tierra</b>. El Estado la sube ",
         "cuando el precio internacional de la soja está alto, que es ",
         "justamente cuando el campo vale más.")),
'<p>La función de reacción lo muestra: se regresa la alícuota contra el',
' precio de la soja en CBOT, con el precio del mismo año y con el del año',
' anterior.</p>',
tabla(c("país", "explicativa", "n", "coeficiente", "t", "significativo al",
        "R²"), filas_endo),
callout("cnota", "Cómo se lee",
  paste0("Por cada punto logarítmico que sube el precio de la soja, la ",
         "alícuota sube casi doce puntos porcentuales, y el precio ",
         "internacional explica por sí solo dos tercios de toda su variación ",
         "en veintiún años. La fila del año anterior es la que cierra el ",
         "argumento: el precio <b>precede</b> a la retención, así que la ",
         "causalidad no corre solamente de la política al precio de la ",
         "tierra. Por eso el coeficiente mide asociación y no efecto, y la ",
         "contribución institucional de la descomposición hereda ese mismo ",
         "límite. No se corrige la ecuación: se declara.")),

'<h2>7. La brecha cambia de signo adentro del período</h2>',
callout("calerta", "Por qué las hipótesis no se evalúan sobre el promedio",
  paste0("La brecha con Mato Grosso no es estable: hasta 2013 la hectárea ",
         "brasileña valía bastante menos que la argentina y desde 2021 vale ",
         "bastante más. El promedio de los veintiún años es el resultado de ",
         "que esas dos épocas se cancelen entre sí, así que <b>no describe ",
         "ningún año del panel</b> y descomponerlo sería explicar una brecha ",
         "que nunca existió. Por eso H1 y H2 se responden con las medias de ",
         e(ventana_pr), ", y el período completo queda como anexo.")),
'<p>Diferencia año por año entre el logaritmo del precio del comparable y el',
' de Argentina. Positivo significa que el comparable es más caro. En negrita,',
' los años de la ventana.</p>',
tabla(c("año", "Brasil &minus; Argentina", "EE. UU. &minus; Argentina"),
      filas_brecha_anual, num_desde = 2),
tabla(c("promedio del bloque", "Brasil &minus; Argentina",
        "EE. UU. &minus; Argentina"), filas_resumen_brecha, num_desde = 2),
if (!is.null(texto_recorrido))
  callout("cnota", "No es un artefacto de medición", texto_recorrido) else NULL,

'<h2>8. En qué terminó cada hipótesis</h2>',
paste0('<p class="meta">Medias de ', e(ventana_pr), '. Los coeficientes salen',
       ' siempre de las ecuaciones estimadas sobre ', e(ventana_an), ': con',
       ' cinco años y ', length(cfg$regresores[["Argentina"]]),
       ' regresores una ecuación no tiene solución.</p>'),

'<h3>La brecha que se quiere explicar</h3>',
tabla(c("comparable", "brecha (puntos logarítmicos)",
        "equivalente en precio"), brechas, num_desde = 2),

'<h3>Cómo se reparte esa brecha</h3>',
'<p>Intervalos al 95 % por bootstrap en bloques de años. El bootstrap',
' remuestrea los años que estiman los coeficientes y deja las medias de la',
' ventana en su valor observado, así que el intervalo mide incertidumbre de',
' las sensibilidades, no de los niveles.</p>',
tabla(c("comparable", "componente", "puntos log.", "IC 95 %",
        "% de la brecha"), filas_comp_pr),
if (!is.null(texto_institucional))
  callout("calerta", "El componente institucional está dominado por un artefacto",
          texto_institucional) else NULL,

'<h3>H1 — el peso institucional</h3>',
'<p>Postula que, dentro del componente de niveles, la contribución de los',
' factores institucionales es mayor que la de los productivos y que la de los',
' económicos.</p>',
tabla(c("comparable", "institucional", "productivo", "económico",
        "se cumple"), filas_h1, num_desde = 2),
callout(if (grepl("RECHAZADA", veredicto_h1)) "cmal" else "cnota",
        "Veredicto de H1", e(veredicto_h1)),
callout("cnota", "Cómo se lee", texto_lectura_h1),

'<h3>H2 — el precio potencial</h3>',
'<p>Postula que, si Argentina adoptara los niveles institucionales de Brasil,',
' el aumento del precio cubriría más de la mitad de la brecha con Mato',
' Grosso.</p>',
callout(if (grepl("NO EVALUABLE", veredicto_h2)) "calerta"
        else if (grepl("RECHAZADA", veredicto_h2)) "cmal" else "cnota",
        "Veredicto de H2", veredicto_h2),
if (!is.null(h2_pr) && !is.na(h2_pr$proporcion_de_la_brecha_cubierta[1]))
  callout("cnota", "El número",
    paste0("Converger al riesgo soberano brasileño sube el precio predicho ",
           num(h2_pr$aumento_log_convergencia_total[1], 3), " puntos ",
           "logarítmicos, contra una brecha de ",
           num(h2_pr$brecha_total_log_argentina_brasil[1], 3), " y un umbral ",
           "de ", num(h2_pr$umbral_50_por_ciento[1], 3), ". Cubre el ",
           num(h2_pr$proporcion_de_la_brecha_cubierta[1] * 100, 1),
           " % de la brecha, menos de la mitad que exige la hipótesis. La ",
           "convergencia solo alcanza al EMBI: la alícuota y la brecha ",
           "cambiaria no se reemplazan ahí porque Brasil no tiene esas ",
           "series, y poner un cero sería inventar una medición. El ",
           "escenario de instituciones de Brasil, en el informe de las ",
           "cuatro preguntas, sí las pone en cero: es un supuesto, no la ",
           "prueba de esta hipótesis.")) else NULL,

'<h2>Anexo. La misma descomposición sobre el período completo</h2>',
paste0('<p>Es el cálculo que el trabajo venía haciendo antes de detectar el',
       ' quiebre. Se deja para comparar. Frente a Brasil la brecha es de',
       ' apenas &minus;0,079 puntos, así que la columna de porcentaje no es',
       ' interpretable y queda vacía.</p>'),
tabla(c("comparable", "componente", "puntos log.", "IC 95 %",
        "% de la brecha"), filas_comp_an),
if (!is.null(h2_an)) callout("cnota", "H2 sobre el período completo",
  paste0("<b>", e(h2_an$veredicto[1]), "</b>",
         if (nzchar(h2_an$motivo[1])) paste0(" — ", e(h2_an$motivo[1]))
         else "")) else NULL,
callout("cnota", "El residuo vale cero acá y no allá",
  paste0("Sobre el período completo el residuo es cero hasta el decimoquinto ",
         "decimal, porque la recta de mínimos cuadrados pasa exactamente por ",
         "las medias de su propia muestra. Sobre la ventana no tiene por qué ",
         "anularse, y lo que mide es cuánto de la brecha reciente la ",
         "ecuación no logra explicar. Que haya quedado chico es la señal de ",
         "que usar coeficientes del período completo sobre medias recientes ",
         "no rompe el ajuste.")),

pie_html("07_informe_html.R", c(
  'Las tablas completas, con todos los decimales, están en los <code>.csv</code>',
  ' de <code>03_resultados/</code>; los avisos de datos faltantes, en',
  ' <code>informe.md</code>.'))
)

ruta_html <- file.path(cfg$dir_resultados, "informe.html")
writeLines(unlist(doc), ruta_html, useBytes = TRUE)
mensaje("Informe HTML escrito: ", ruta_html)
