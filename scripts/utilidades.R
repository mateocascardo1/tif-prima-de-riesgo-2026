# ---------------------------------------------------------------------------
# utilidades.R
# Funciones compartidas por los demás scripts: lectura de los crudos,
# conversión de texto a número, guardado de tablas y las verificaciones
# que frenan el análisis cuando falta un dato.
#
# Está separado para no repetir el mismo código en seis archivos.
# ---------------------------------------------------------------------------

# --- Lectura --------------------------------------------------------------

# Lee un archivo de texto quitando la marca BOM que dejan las exportaciones
# de Google Sheets y Excel (si no se saca, el primer encabezado llega sucio).
leer_lineas <- function(ruta) {
  if (!file.exists(ruta)) {
    stop("No se encuentra el archivo: ", ruta, call. = FALSE)
  }
  lineas <- readLines(ruta, encoding = "UTF-8", warn = FALSE)
  if (length(lineas) > 0) lineas[1] <- sub("^\ufeff", "", lineas[1])
  lineas
}

# Lee un CSV crudo como texto, sin que R adivine tipos ni cambie los nombres
# de las columnas. La conversión a número se hace después, explícitamente.
leer_csv_crudo <- function(ruta, sep = ",", saltar = 0) {
  lineas <- leer_lineas(ruta)
  if (saltar > 0) lineas <- lineas[-seq_len(saltar)]
  utils::read.csv(
    text             = paste(lineas, collapse = "\n"),
    sep              = sep,
    colClasses       = "character",
    check.names      = FALSE,
    stringsAsFactors = FALSE
  )
}

# Convierte texto a número. Saca prefijos ("USD 8,000"), separadores de miles
# y espacios. Con decimal = "," interpreta el formato de la planilla de
# retenciones ("23,5" son veintitrés con cinco).
a_numero <- function(x, decimal = ".") {
  x <- as.character(x)
  x <- gsub("[^0-9,.\\-]", "", x)
  if (identical(decimal, ",")) {
    x <- gsub("\\.", "", x)   # el punto es separador de miles
    x <- gsub(",", ".", x)    # la coma es el decimal
  } else {
    x <- gsub(",", "", x)     # la coma es separador de miles
  }
  x[x %in% c("", "-", ".", "NA")] <- NA
  suppressWarnings(as.numeric(x))
}

# Varios archivos terminan con filas de fuente, notas o estadísticas de
# resumen. Esta función se queda solo con las filas cuya primera columna es
# un año de cuatro dígitos, así esas notas nunca entran al panel.
filas_con_anio <- function(df, columna = 1) {
  valores <- trimws(as.character(df[[columna]]))
  df[grepl("^[0-9]{4}$", valores), , drop = FALSE]
}

# --- Procedencia de los crudos -------------------------------------------

# Fuente que un archivo declara en una columna propia. Se la pasa la columna
# ya leída, para poder citar por país o por serie cuando una sola línea de
# fuente para todo el archivo perdería esa distinción.
fuente_de_columna <- function(valores) {
  textos <- unique(trimws(as.character(valores)))
  textos <- textos[!is.na(textos) & textos != ""]
  if (length(textos) == 0) return("sin fuente en el archivo")
  paste0("Columna de fuente del archivo: ", paste(textos, collapse = " | "))
}

# Devuelve la fuente que el propio archivo declara, para el catálogo del
# anexo. Busca, en este orden: una columna que empiece con "fuente", la
# columna "norma" de la planilla de retenciones y una línea que diga
# "Fuente:". Si no encuentra ninguna, lo dice. Acá no se completa con lo que
# se sabe por fuera del archivo: la atribución investigada va al diccionario.
fuente_declarada <- function(ruta, sep = ",") {
  if (!file.exists(ruta)) return("el archivo no está en data/")
  if (grepl("\\.xlsx?$", ruta, ignore.case = TRUE)) {
    return(fuente_declarada_excel(ruta))
  }

  # Varios archivos terminan con filas de nota y no son rectangulares, así
  # que la lectura por columnas puede fallar: ahí se sigue por líneas.
  tabla <- tryCatch(leer_csv_crudo(ruta, sep = sep), error = function(e) NULL)
  if (!is.null(tabla)) {
    nombres <- tolower(names(tabla))
    columnas <- grep("^fuente", nombres)
    if (length(columnas) > 0) {
      fuente <- fuente_de_columna(unlist(tabla[columnas], use.names = FALSE))
      if (!startsWith(fuente, "sin fuente")) return(fuente)
    }
    if ("norma" %in% nombres) {
      norma <- trimws(as.character(tabla[[which(nombres == "norma")[1]]]))
      con_norma <- sum(!is.na(norma) & norma != "")
      return(paste0("Sin línea de fuente. La columna «norma» cita la norma ",
                    "de ", con_norma, " de ", nrow(tabla), " meses y deja ",
                    "vacíos los demás."))
    }
  }

  # La línea de fuente puede venir entre comillas y con las comas de relleno
  # del CSV, a veces en el medio de una fila de datos.
  lineas <- grep("Fuente\\s*:", leer_lineas(ruta), value = TRUE)
  if (length(lineas) > 0) {
    textos <- sub("^.*?(Fuente\\s*:)", "\\1", lineas, perl = TRUE)
    textos <- gsub('""', '"', textos, fixed = TRUE)
    textos <- trimws(sub("[\",[:space:]]+$", "", textos))
    return(paste(unique(textos), collapse = " | "))
  }

  "sin fuente en el archivo"
}

# Los Excel declaran la fuente de dos maneras: las exportaciones de Bloomberg
# dejan el ticker en las primeras filas y las planillas armadas a mano traen
# una hoja de fuentes. De las hojas largas alcanza el encabezado.
fuente_declarada_excel <- function(ruta) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    return("no se pudo leer el Excel: falta el paquete readxl")
  }
  tickers <- character(0)
  textos  <- character(0)
  for (hoja in readxl::excel_sheets(ruta)) {
    # Una hoja dedicada a las fuentes se declara entera: sus celdas son una
    # fuente por fila y no un texto único que se pueda citar.
    if (grepl("fuente", hoja, ignore.case = TRUE)) {
      textos <- c(textos, paste0("El archivo trae la hoja «", hoja,
                                 "» con la fuente de cada año y país."))
      next
    }
    bloque <- suppressMessages(readxl::read_excel(
      ruta, sheet = hoja, col_names = FALSE, .name_repair = "minimal",
      n_max = 200
    ))
    celdas <- trimws(as.character(unlist(lapply(bloque, as.character))))
    celdas <- celdas[!is.na(celdas) & celdas != ""]
    tickers <- c(tickers, grep("(Index|Comdty)$", celdas, value = TRUE))
    textos  <- c(textos, grep("^Fuente\\s*:", celdas, value = TRUE))
  }
  tickers <- unique(tickers)
  textos  <- unique(textos)
  if (length(tickers) > 0) {
    aviso <- paste0("Declara el ticker de Bloomberg (",
                    paste(tickers, collapse = ", "),
                    "), no el organismo que publica la serie.")
    if (length(textos) > 0) {
      aviso <- paste(aviso, paste(textos, collapse = " | "))
    }
    return(aviso)
  }
  if (length(textos) > 0) return(paste(textos, collapse = " | "))
  "sin fuente en el archivo"
}

# Ticker de Bloomberg que declara un Excel, para el catálogo del anexo.
tickers_declarados <- function(ruta) {
  if (!file.exists(ruta) || !grepl("\\.xlsx?$", ruta, ignore.case = TRUE)) {
    return("no aplica")
  }
  if (!requireNamespace("readxl", quietly = TRUE)) return("no aplica")
  tickers <- character(0)
  for (hoja in readxl::excel_sheets(ruta)) {
    bloque <- suppressMessages(readxl::read_excel(
      ruta, sheet = hoja, col_names = FALSE, .name_repair = "minimal",
      n_max = 6
    ))
    celdas <- trimws(as.character(unlist(lapply(bloque, as.character))))
    tickers <- c(tickers, grep("(Index|Comdty)$", celdas, value = TRUE))
  }
  tickers <- unique(tickers[!is.na(tickers)])
  if (length(tickers) == 0) return("no aplica")
  paste(tickers, collapse = ", ")
}

# Promedio anual de una serie fechada. Acepta fechas diarias (AAAA-MM-DD),
# mensuales (AAAA-MM) o un año suelto (AAAA). Con datos diarios o mensuales
# promedia primero dentro de cada mes y después promedia los meses del año,
# para que un mes con más observaciones no pese más que los otros.
#   meses_minimos         meses con dato que necesita un año para entrar.
#   observaciones_minimas observaciones que necesita un mes para contar.
# Devuelve un data.frame con anio, valor y meses. Los años que no llegan al
# mínimo no se devuelven: quedan como hueco, no se completan.
promedio_anual <- function(fechas, valores, meses_minimos = 12,
                           observaciones_minimas = 1) {
  vacio <- data.frame(anio = integer(0), valor = numeric(0),
                      meses = integer(0), stringsAsFactors = FALSE)

  texto <- trimws(as.character(fechas))
  anio  <- suppressWarnings(as.integer(substr(texto, 1, 4)))
  mes   <- suppressWarnings(as.integer(substr(texto, 6, 7)))

  ok <- !is.na(anio) & !is.na(valores)
  if (!all(is.na(mes))) ok <- ok & !is.na(mes)
  anio <- anio[ok]; mes <- mes[ok]; valores <- valores[ok]
  if (length(valores) == 0) return(vacio)

  # Serie ya anual: la fecha no trae mes.
  if (all(is.na(mes))) {
    por_anio <- tapply(valores, anio, mean)
    return(data.frame(anio  = as.integer(names(por_anio)),
                      valor = as.numeric(por_anio),
                      meses = 12L, stringsAsFactors = FALSE))
  }

  clave     <- sprintf("%04d-%02d", anio, mes)
  media_mes <- tapply(valores, clave, mean)
  cuenta_mes <- tapply(valores, clave, length)
  media_mes <- media_mes[cuenta_mes >= observaciones_minimas]
  if (length(media_mes) == 0) return(vacio)

  anio_mes <- as.integer(substr(names(media_mes), 1, 4))
  por_anio <- tapply(as.numeric(media_mes), anio_mes, mean)
  meses    <- tapply(as.numeric(media_mes), anio_mes, length)
  salida <- data.frame(anio  = as.integer(names(por_anio)),
                       valor = as.numeric(por_anio),
                       meses = as.integer(meses),
                       stringsAsFactors = FALSE)
  salida[salida$meses >= meses_minimos, , drop = FALSE]
}

# --- Guardado de resultados ----------------------------------------------

# Escapa el texto que va dentro de una celda HTML.
escapar_html <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

# Guarda una tabla en .csv y en .html (el .html es el que se pega al anexo).
# Por defecto va a 03_resultados; la base del anexo pide 02_curados.
guardar_tabla <- function(df, nombre, titulo = NULL, nota = NULL,
                          directorio = cfg$dir_resultados) {
  if (is.null(titulo)) titulo <- nombre
  ruta_csv <- file.path(directorio, paste0(nombre, ".csv"))
  utils::write.csv(df, ruta_csv, row.names = FALSE, fileEncoding = "UTF-8")

  # Los números se redondean solo para mostrar; el .csv guarda el valor exacto.
  mostrar <- df
  for (j in seq_along(mostrar)) {
    if (is.numeric(mostrar[[j]])) mostrar[[j]] <- round(mostrar[[j]], 4)
    mostrar[[j]] <- escapar_html(ifelse(is.na(mostrar[[j]]), "", mostrar[[j]]))
  }

  encabezado <- paste0("<th>", escapar_html(names(df)), "</th>", collapse = "")
  cuerpo <- if (nrow(mostrar) == 0) {
    "<tr><td colspan=\"100%\">Sin filas.</td></tr>"
  } else {
    paste(
      apply(mostrar, 1, function(fila) {
        paste0("<tr>", paste0("<td>", fila, "</td>", collapse = ""), "</tr>")
      }),
      collapse = "\n"
    )
  }

  html <- c(
    "<!DOCTYPE html>",
    "<html lang=\"es\"><head><meta charset=\"utf-8\">",
    paste0("<title>", escapar_html(titulo), "</title>"),
    "<style>",
    "body{font-family:Georgia,serif;margin:2rem;color:#000}",
    "table{border-collapse:collapse}",
    "th,td{border:1px solid #000;padding:4px 8px;text-align:right}",
    "th{text-align:center}td:first-child,th:first-child{text-align:left}",
    "caption{text-align:left;font-weight:bold;margin-bottom:.5rem}",
    "p.nota{font-size:.9em;max-width:60em}",
    "</style></head><body>",
    paste0("<table><caption>", escapar_html(titulo), "</caption>"),
    paste0("<thead><tr>", encabezado, "</tr></thead>"),
    paste0("<tbody>", cuerpo, "</tbody></table>"),
    if (!is.null(nota)) paste0("<p class=\"nota\">", escapar_html(nota), "</p>") else NULL,
    "</body></html>"
  )
  writeLines(html, file.path(directorio, paste0(nombre, ".html")),
             useBytes = TRUE)

  invisible(ruta_csv)
}

# --- Avisos de datos faltantes -------------------------------------------

# Acumula los motivos por los que algo no se pudo calcular. Nada se calcula
# "por las dudas": si falta un dato, queda escrito acá y se informa.
nuevo_registro_faltantes <- function() {
  data.frame(
    etapa    = character(0),
    elemento = character(0),
    motivo   = character(0),
    stringsAsFactors = FALSE
  )
}

agregar_faltante <- function(registro, etapa, elemento, motivo) {
  rbind(registro, data.frame(
    etapa = etapa, elemento = elemento, motivo = motivo,
    stringsAsFactors = FALSE
  ))
}

# --- Gate: ¿se puede estimar? --------------------------------------------

# Devuelve la lista de variables que la especificación necesita y que el
# panel todavía no tiene (columna inexistente, toda NA, o con años faltantes
# dentro del período). Mientras esta lista no esté vacía, 04, 05 y 06 no
# estiman nada.
verificar_gate <- function(panel, cfg) {
  problemas <- nuevo_registro_faltantes()
  anios <- cfg$anio_min:cfg$anio_max

  for (pais in cfg$paises) {
    columnas <- c(cfg$dependiente, cfg$regresores[[pais]])
    datos_pais <- panel[panel$pais == pais, , drop = FALSE]

    for (columna in columnas) {
      if (!columna %in% names(panel)) {
        problemas <- agregar_faltante(
          problemas, "gate", paste0(pais, " / ", columna),
          "La columna no existe en panel.csv."
        )
        next
      }
      valores <- datos_pais[[columna]][match(anios, datos_pais$anio)]
      faltan <- anios[is.na(valores)]
      if (length(faltan) == length(anios)) {
        problemas <- agregar_faltante(
          problemas, "gate", paste0(pais, " / ", columna),
          "La serie no existe en data/: la columna está completamente vacía."
        )
      } else if (length(faltan) > 0 && !columna %in% cfg$huecos_aceptados) {
        problemas <- agregar_faltante(
          problemas, "gate", paste0(pais, " / ", columna),
          paste0("Faltan ", length(faltan), " año(s) dentro del período: ",
                 paste(faltan, collapse = ", "),
                 ". No está en cfg$huecos_aceptados, así que no se estima.")
        )
      }
    }
  }

  list(ok = nrow(problemas) == 0, problemas = problemas)
}

# Escribe el aviso de que una etapa no se ejecutó y por qué.
escribir_no_estimado <- function(problemas, etapa) {
  ruta <- file.path(cfg$dir_resultados, "no_estimado.csv")
  previo <- if (file.exists(ruta)) {
    utils::read.csv(ruta, colClasses = "character", check.names = FALSE)
  } else {
    nuevo_registro_faltantes()
  }
  problemas$etapa <- etapa
  todo <- unique(rbind(previo, problemas))
  utils::write.csv(todo, ruta, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(ruta)
}

# --- Diagnósticos ---------------------------------------------------------

# Factor de inflación de la varianza, calculado a mano: para cada regresor se
# corre una regresión auxiliar contra los demás y se usa VIF = 1/(1 - R2).
# Se hace así para no depender de un paquete extra y para que el cálculo
# quede a la vista en el anexo.
vif_manual <- function(modelo) {
  X <- stats::model.matrix(modelo)
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  if (ncol(X) < 2) {
    return(data.frame(variable = colnames(X), vif = NA_real_,
                      stringsAsFactors = FALSE))
  }
  vif <- vapply(seq_len(ncol(X)), function(j) {
    aux <- stats::lm(X[, j] ~ X[, -j, drop = FALSE])
    r2 <- summary(aux)$r.squared
    if (is.na(r2) || r2 >= 1) Inf else 1 / (1 - r2)
  }, numeric(1))
  data.frame(variable = colnames(X), vif = vif, stringsAsFactors = FALSE)
}

# Coeficientes con errores estándar Newey-West (HAC).
coeficientes_hac <- function(modelo, rezago) {
  V <- sandwich::NeweyWest(modelo, lag = rezago, prewhite = FALSE,
                           adjust = TRUE)
  prueba <- lmtest::coeftest(modelo, vcov. = V)
  data.frame(
    variable     = rownames(prueba),
    coeficiente  = prueba[, 1],
    ee_hac       = prueba[, 2],
    estadistico_t = prueba[, 3],
    p_valor      = prueba[, 4],
    stringsAsFactors = FALSE,
    row.names    = NULL
  )
}

# Prueba de Wald sobre un conjunto de coeficientes, usando la matriz de
# covarianzas que se le pase (acá siempre la HAC). Se escribe a mano para no
# sumar otro paquete y para que el cálculo quede visible en el anexo.
# H0: todos los coeficientes de 'terminos' son cero a la vez.
#
# Se devuelven las dos versiones del p-valor. La chi-cuadrado es la fórmula
# de manual, pero vale para muestras grandes: con veintiún años rechaza de
# más, o sea que da por significativo lo que no lo es. La versión F divide el
# estadístico por la cantidad de restricciones y lo compara contra los grados
# de libertad que realmente quedan, así que castiga la muestra chica. Esa es
# la que informa el trabajo; la chi-cuadrado queda al lado para que la
# diferencia esté a la vista y no haya que creer en la palabra.
prueba_wald <- function(modelo, V, terminos) {
  b <- stats::coef(modelo)
  gl_den <- stats::df.residual(modelo)
  vacia <- function(gl) {
    data.frame(estadistico = NA_real_, gl = gl, p_valor = NA_real_,
               estadistico_f = NA_real_, gl_denominador = gl_den,
               p_valor_f = NA_real_, stringsAsFactors = FALSE)
  }
  terminos <- terminos[terminos %in% names(b)]
  if (length(terminos) == 0) return(vacia(0L))
  R <- matrix(0, nrow = length(terminos), ncol = length(b),
              dimnames = list(terminos, names(b)))
  for (i in seq_along(terminos)) R[i, terminos[i]] <- 1
  Rb <- R %*% b
  RVR <- R %*% V %*% t(R)
  inversa <- try(solve(RVR), silent = TRUE)
  if (inherits(inversa, "try-error")) return(vacia(length(terminos)))
  W <- as.numeric(t(Rb) %*% inversa %*% Rb)
  q <- length(terminos)
  data.frame(
    estadistico    = W,
    gl             = q,
    p_valor        = stats::pchisq(W, df = q, lower.tail = FALSE),
    estadistico_f  = W / q,
    gl_denominador = gl_den,
    p_valor_f      = stats::pf(W / q, df1 = q, df2 = gl_den, lower.tail = FALSE),
    stringsAsFactors = FALSE
  )
}

# --- Varios ---------------------------------------------------------------

categoria_de <- function(variable) {
  categoria <- unname(cfg$categorias[variable])
  ifelse(is.na(categoria), "sin categoria", categoria)
}

# Convierte una diferencia en puntos logarítmicos a diferencia porcentual.
log_a_porcentaje <- function(delta) (exp(delta) - 1) * 100

mensaje <- function(...) cat(..., "\n", sep = "")

# --- Informe --------------------------------------------------------------

# Arma informe.md a partir de lo que los scripts dejaron en 03_resultados.
# Se genera solo: no conviene editarlo a mano, porque la próxima corrida lo
# pisa. Lo que haya que agregar a mano va en el cuerpo de la tesis.
generar_informe <- function() {
  leer_si_existe <- function(nombre) {
    ruta <- file.path(cfg$dir_resultados, nombre)
    if (file.exists(ruta)) {
      utils::read.csv(ruta, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  }

  faltantes_curado <- leer_si_existe("faltantes_curado.csv")
  no_estimado      <- leer_si_existe("no_estimado.csv")
  h1               <- leer_si_existe("tabla_h1.csv")
  h2               <- leer_si_existe("tabla_h2.csv")
  ajuste           <- leer_si_existe("tabla_ajuste.csv")
  pr_bloques       <- leer_si_existe("tabla_pruebas_bloques.csv")

  lista <- function(df, col_a, col_b) {
    if (is.null(df) || nrow(df) == 0) return("Ninguno.")
    paste0("- **", df[[col_a]], "**: ", df[[col_b]], collapse = "\n")
  }

  estimado <- !is.null(ajuste)

  texto <- c(
    "# Informe de la corrida",
    "",
    paste0("Generado automáticamente por `run_all.R` el ",
           format(Sys.time(), "%d/%m/%Y a las %H:%M"), ". ",
           "No editar a mano: la próxima corrida lo reescribe."),
    "",
    "## 1. Qué datos faltaron o se reemplazaron",
    "",
    "Series pedidas por el diseño del trabajo que no están en `data/`, o que",
    "están con otra definición:",
    "",
    lista(faltantes_curado, "elemento", "motivo"),
    "",
    "## 2. Qué no se pudo estimar",
    "",
    if (is.null(no_estimado) || nrow(no_estimado) == 0) {
      "Todas las etapas se ejecutaron."
    } else {
      paste0(
        "Las siguientes etapas no se ejecutaron porque les faltaba una ",
        "variable o un año. No se estimó nada con datos completados:\n\n",
        lista(no_estimado, "elemento", "motivo")
      )
    },
    "",
    "## 3. Decisiones tomadas y por qué",
    "",
    paste0("- **Variable fiscal**: `", cfg$variable_fiscal, "`. La alícuota ",
           "de derechos de exportación existe solo para Argentina; en Brasil ",
           "y EE. UU. queda vacía y no se le asigna cero, porque cero sería ",
           "afirmar que se midió y dio cero."),
    paste0("- **Precipitación**: `incluir_precipitacion = ",
           cfg$incluir_precipitacion, "`. Entra como desvío respecto de la ",
           "media del período en los tres países. Enero y febrero de 2025 de ",
           "Mato Grosso son de Cuiabá y no de Diamantino, que no midió esos ",
           "dos meses."),
    paste0("- **Riesgo soberano**: `usar_embi = ", cfg$usar_embi, "`. ",
           "2008-2025 es el EMBI Global Diversified del BCRD. 2005-2007 es ",
           "EMBI+ (Ámbito e Ipeadata), porque esa planilla arranca el ",
           "2007-10-29. EE. UU. no lleva la serie."),
    paste0("- **Riesgo país de Brasil, año por año**: la ecuación de ",
           cfg$pais_precio, " se evalúa en cada año y se reemplaza solo `",
           cfg$variable_riesgo_pais, "` por el valor de ",
           cfg$pais_riesgo_reemplazo, " de ese mismo año. La alícuota ",
           "argentina no se toca. Ver `tabla_riesgo_brasil.csv` y ",
           "`figura_3_riesgo_brasil.png`. La figura 4 saca además las ",
           "retenciones. No usa el coeficiente de la alícuota, que no se ",
           "distingue de cero: valúa el impuesto como una suba del precio de ",
           "la soja que le queda al productor. Ver ",
           "`figura_4_institucional_brasil.png`."),
    paste0("- **Alícuotas no nominales**: `aceptar_alicuotas_no_nominales = ",
           cfg$aceptar_alicuotas_no_nominales, "`. El único tramo que no es ",
           "la alícuota nominal vigente es marzo a junio de 2008, cuando ",
           "rigió la móvil de la Res. 125/2008: se toma el 35 % del régimen ",
           "que la antecedió y que el Dto. 1176/2008 repuso. Con FALSE, 2008 ",
           "queda NA."),
    paste0("- **Especificación**: `", cfg$especificacion, "`. Ver ",
           "`03_resultados/especificacion_sugerida.csv` para lo que indican ",
           "las pruebas de raíz unitaria."),
    paste0("- **Referencia de Oaxaca**: `", cfg$referencia_oaxaca, "`."),
    paste0("- **Pruebas por bloque**: además del t de cada variable, cada ",
           "bloque se prueba en conjunto (H0: todos sus coeficientes son ",
           "cero a la vez), porque H1 está escrita sobre un grupo de ",
           "factores y con 21 años las variables de un mismo bloque se ",
           "reparten el mérito y ninguna sobresale sola. Se informa el ",
           "p-valor de la versión F y no el de la chi-cuadrado, que supone ",
           "muestra grande y rechaza de más. Las variables de un bloque NO ",
           "se combinan en un índice: eso exigiría elegir una ponderación ",
           "que ninguna teoría fija. Ver `tabla_pruebas_bloques.csv`."),
    paste0("- **Ventana de las hipótesis**: `ventana_medias = ",
           cfg$ventana_medias[1], "-", cfg$ventana_medias[2], "`. La brecha ",
           "con Mato Grosso cambia de signo adentro del período: hasta 2013 ",
           "Brasil estaba por debajo de Argentina y desde 2021 está por ",
           "encima, así que el promedio de los 21 años no describe ningún ",
           "año del panel. H1 y H2 se evalúan en las MEDIAS de esta ventana ",
           "y los COEFICIENTES salen del período completo (Argentina, los 21 ",
           "años; Brasil, sin 2020; EE. UU., sin 2005 y 2006), porque con 5 ",
           "observaciones y ", length(cfg$regresores[["Argentina"]]),
           " regresores la ecuación no tiene solución. El período completo ",
           "se calcula igual y queda como anexo, en la columna `ventana` de ",
           "las mismas tablas."),
    paste0("- **Regresores**: un indicador por concepto. El límite práctico ",
           "es de 5 a 7 con 21 observaciones. Argentina lleva ",
           length(cfg$regresores[["Argentina"]]), ": ",
           paste(cfg$regresores[["Argentina"]], collapse = ", "), "."),
    "",
    "## 4. Limitaciones",
    "",
    paste0("- **Grados de libertad.** Argentina tiene ",
           length(cfg$regresores[["Argentina"]]), " regresores y 21 años: ",
           "quedan ", 21 - length(cfg$regresores[["Argentina"]]) - 1,
           " grados de libertad. Brasil pierde 2020 por el diésel y EE. UU.",
           " pierde 2005 y 2006 por la maquinaria. Con la urea adentro, las",
           " tres ecuaciones quedan en torno de 9 grados de libertad. Es el",
           " borde de lo estimable: los errores estándar son frágiles."),
    "- **Maquinaria y diésel entran con huecos, y el hueco no se rellena.**",
    "  La maquinaria es el logaritmo de las ventas por hectárea sembrada, no",
    "  el stock: convertirlo exigiría una vida útil que nadie midió. EE. UU.",
    "  no tiene 2005 ni 2006, y esos dos años salen solo de su ecuación. El",
    "  diésel es el minorista en dólares por litro; Brasil 2020 queda vacío",
    "  porque la ANP no relevó septiembre, y ese año sale solo de la ecuación",
    "  brasileña. Los dos huecos caen fuera de 2021-2025. La urea no tiene",
    "  hueco: 2005-2025 trae los doce meses, y 2025 entra.",
    "- **El crédito no entra.** Argentina es el saldo del BCRA y Estados",
    "  Unidos el de la Fed, los 21 años. Brasil no tiene serie y no se",
    "  completa con FAO, así que el cociente no entra a las ecuaciones.",
    "- **La brecha cambiaria entra solo en Argentina.** Cambia de cotización",
    "  en 2013: blue de Ámbito hasta 2012 y CCL desde 2013. Brasil y Estados",
    "  Unidos no tienen la serie. En la descomposición su media vale 0, y en",
    "  el escenario institucional se la pone en cero.",
    "- **Endogeneidad de la alícuota.** Su coeficiente NO admite lectura",
    "  causal. La alícuota no es independiente de lo que determina el precio",
    "  de la tierra: responde al precio internacional de la soja, que sube",
    "  justo cuando el campo vale más. La función de reacción está en",
    "  `tabla_endogeneidad_fiscal.csv`, con el precio del mismo año y el del",
    "  año anterior; que el precio rezagado también la explique indica que el",
    "  precio precede a la alícuota. El coeficiente de la ecuación por país",
    "  mide asociación, no efecto de la política. Por extensión, la",
    "  contribución institucional de la descomposición hereda ese límite.",
    "- **Estabilidad de las sensibilidades.** La descomposición principal usa",
    "  coeficientes del período completo sobre medias de la ventana reciente,",
    "  así que supone que las pendientes no cambiaron en el quiebre. Ese",
    "  supuesto NO se puede testear: una prueba de Chow que parta 21 años en",
    "  dos tramos deja un tramo sin grados de libertad. Lo que sí queda",
    "  medido es el componente de residuo de la descomposición, que es la",
    "  parte de la brecha reciente que la ecuación no explica; vale cero por",
    "  construcción sobre el período completo y no sobre la ventana.",
    "- **Endogeneidad de la maquinaria.** Las ventas ya entran a las tres",
    "  ecuaciones. Quien compra una máquina mira el precio de la tierra, así",
    "  que el coeficiente puede correr en las dos direcciones. El crédito no",
    "  entra.",
    "- **Variables que existen en un solo país.** La alícuota de retenciones",
    "  vale 0 en el vector de medias del comparable por convención de la",
    "  descomposición. Eso hace que toda su contribución caiga en el",
    "  componente de niveles y que el componente institucional dependa de esa",
    "  convención, no solo de los datos. Está advertido en la tabla.",
    "- **Qué parte de lo económico no mueve los niveles.** El precio CBOT y",
    "  la tasa TIPS son los mismos en los tres países, y el tipo de cambio",
    "  real está normalizado a promedio 100 en cada uno. Esas tres tienen",
    "  diferencia de medias cero por construcción y solo pueden actuar por",
    "  sensibilidad. La tasa real local sí difiere entre países, así que",
    "  puede aportar al componente de niveles.",
    "- **Calidad de las fuentes de precio.** La serie de Mato Grosso combina",
    "  texto y figura digitalizada de Colussi et al. (2026). La serie",
    "  argentina no indica fuente en el archivo de origen.",
    "- **Desajuste geográfico.** El precio argentino es de la zona núcleo y el",
    "  rendimiento es de toda la provincia de Buenos Aires. La lluvia de",
    "  Pergamino y la de Diamantino son de una estación, no de la región.",
    "- **Extrapolación.** Los escenarios de convergencia que llevan una",
    "  variable fuera del rango observado en Argentina están marcados en",
    "  `tabla_precio_potencial.csv`. El mismo criterio, año por año y solo",
    "  para el riesgo país, está en `tabla_riesgo_brasil.csv`.",
    "",
    "## 5. Hipótesis",
    ""
  )

  # Las tablas traen las dos ventanas; el veredicto es el de la principal y
  # el anexo se menciona al lado para que la diferencia quede a la vista.
  principal_de <- function(tabla) {
    if (is.null(tabla) || !"rol" %in% names(tabla)) return(tabla)
    tabla[tabla$rol == "principal", , drop = FALSE]
  }
  anexo_de <- function(tabla) {
    if (is.null(tabla) || !"rol" %in% names(tabla)) return(NULL)
    tabla[tabla$rol == "anexo", , drop = FALSE]
  }
  h1_pr <- principal_de(h1); h1_an <- anexo_de(h1)
  h2_pr <- principal_de(h2); h2_an <- anexo_de(h2)

  if (!is.null(pr_bloques) && nrow(pr_bloques) > 0) {
    inst <- pr_bloques[pr_bloques$bloque == "institucional" &
                         pr_bloques$p_valor < cfg$nivel_bloques, ]
    texto <- c(texto,
      paste0("Evidencia conjunta, previa a las dos hipótesis: el bloque ",
             "institucional resulta significativo al ",
             round(cfg$nivel_bloques * 100), " % en ",
             if (nrow(inst) == 0) "ningún país" else paste(inst$pais, collapse = ", "),
             ". Esto no cambia el veredicto de H1, que compara el TAMAÑO de ",
             "las contribuciones de la descomposición y no si los ",
             "coeficientes son cero, ni corrige la endogeneidad de la ",
             "alícuota. Detalle en `tabla_pruebas_bloques.csv`."),
      "")
  }

  if (!is.null(h1_pr) && nrow(h1_pr) > 0) {
    texto <- c(texto,
      paste0("**H1** (la contribución institucional al componente de niveles ",
             "es mayor que la productiva y que la económica), medias de ",
             h1_pr$ventana[1], ": ", unique(h1_pr$veredicto_global)),
      "",
      paste0("| Comparable | Institucional | Productivo | Económico | Cumple |"),
      "| --- | --- | --- | --- | --- |",
      paste0("| ", h1_pr$comparable, " | ", round(h1_pr$institucional, 4),
             " | ", round(h1_pr$productivo, 4), " | ",
             round(h1_pr$economico, 4), " | ",
             ifelse(h1_pr$cumple_en_este_comparable, "sí", "no"), " |"),
      "",
      "La respuesta en castellano, con la brecha en dólares por hectárea, está en `03_resultados/informe_hipotesis.html`.",
      "",
      if (!is.null(h1_an) && nrow(h1_an) > 0) {
        paste0("Anexo, sobre el período completo (", h1_an$ventana[1], "): ",
               unique(h1_an$veredicto_global))
      } else NULL,
      ""
    )
  } else {
    texto <- c(texto,
      "**H1**: no se evaluó. La descomposición no se calculó porque faltan",
      "series (ver la sección 2).", "")
  }

  if (!is.null(h2_pr) && nrow(h2_pr) > 0) {
    texto <- c(texto,
      paste0("**H2** (con convergencia institucional total hacia Brasil, el ",
             "aumento supera el 50 % de la brecha con Mato Grosso), medias ",
             "de ", h2_pr$ventana[1], ": ", h2_pr$veredicto[1],
             if (nzchar(h2_pr$motivo[1])) paste0(" — ", h2_pr$motivo[1])
             else ""),
      "",
      if (!is.null(h2_an) && nrow(h2_an) > 0) {
        paste0("Anexo, sobre el período completo (", h2_an$ventana[1], "): ",
               h2_an$veredicto[1],
               if (nzchar(h2_an$motivo[1])) paste0(" — ", h2_an$motivo[1])
               else "")
      } else NULL,
      "")
  } else {
    texto <- c(texto,
      "**H2**: no se evaluó. El precio potencial no se calculó porque falta",
      "la ecuación argentina (ver la sección 2).", "")
  }

  if (!estimado) {
    texto <- c(texto,
      "---",
      "",
      "> **Estado de la corrida.** No hay ningún resultado econométrico en",
      "> este informe. El panel y los descriptivos sí se produjeron. Las",
      "> regresiones, la descomposición y el precio potencial están",
      "> programados y verificados, y se ejecutan solos en cuanto las series",
      "> de la sección 2 estén en `data/`.", "")
  }

  writeLines(texto, "informe.md", useBytes = TRUE)
  invisible("informe.md")
}
