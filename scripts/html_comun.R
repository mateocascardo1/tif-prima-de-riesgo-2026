# ---------------------------------------------------------------------------
# html_comun.R
# Piezas compartidas por los informes HTML: la hoja de estilo, el formato de
# números y los bloques (tablas, filas, recuadros).
#
# No se ejecuta solo. Lo cargan los informes (07, 09, 11, 12, 13 y 14)
# para que se vean igual y para que un arreglo de formato se haga en un
# solo lugar.
#
# Asume que utilidades.R ya está cargado (usa escapar_html) y que cfg
# ya existe.
# ---------------------------------------------------------------------------

e <- escapar_html

# Tabla csv de una etapa anterior. NULL si todavía no se corrió: el informe
# lo dice, no se cae.
leer <- function(nombre, directorio = cfg$dir_resultados) {
  ruta <- file.path(directorio, paste0(nombre, ".csv"))
  if (!file.exists(ruta)) return(NULL)
  utils::read.csv(ruta, stringsAsFactors = FALSE)
}

# Años en los que se responden las hipótesis. La ventana está en 00_config.R.
en_ventana <- function(anio) {
  anio >= cfg$ventana_medias[1] & anio <= cfg$ventana_medias[2]
}

# Porcentaje con coma decimal. El signo positivo se escribe solo cuando se
# pide: un nivel se lee sin signo, una brecha o un error se lee con signo.
pc <- function(x, dec = 1, signo = FALSE) {
  if (length(x) != 1 || is.na(x)) return("—")
  texto <- num(x, dec)
  if (signo && x > 0) texto <- paste0("+", texto)
  paste0(texto, " %")
}

# Lista en castellano: "Brasil", "Brasil y Estados Unidos",
# "Argentina, Brasil y Estados Unidos".
unir <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("")
  if (length(x) == 1) return(as.character(x))
  paste0(paste(utils::head(x, -1), collapse = ", "), " y ", utils::tail(x, 1))
}

# Un porcentaje suelto, con una decimal. Con signo = TRUE, el positivo lleva
# "+"; el faltante queda en raya, sin un "%" colgado.
pct <- function(x, signo = FALSE) {
  if (length(x) != 1 || is.na(x)) return("—")
  if (signo) paste0(ifelse(x < 0, "", "+"), num(x, 1), " %")
  else paste0(num(x, 1), " %")
}

usd_con_signo <- function(x) {
  if (length(x) != 1 || is.na(x)) return("—")
  paste0(ifelse(x < 0, "", "+"), miles(x))
}

# Número con coma decimal, que es como se lee en el trabajo.
num <- function(x, dec = 3) {
  texto <- formatC(x, format = "f", digits = dec, big.mark = "")
  # Un valor que redondea a cero no lleva signo: "-0,000" se lee como un
  # número negativo y es cero.
  texto <- sub("^-(0\\.?0*)$", "\\1", texto)
  ifelse(is.na(x), "—", gsub(".", ",", texto, fixed = TRUE))
}

# Entero con punto como separador de miles, que es el uso local.
miles <- function(x) {
  ifelse(is.na(x), "—",
         gsub(",", ".", formatC(round(x), format = "d", big.mark = ","),
              fixed = TRUE))
}

# Etiqueta de significatividad, para no obligar a leer el p-valor.
chip_p <- function(p) {
  ifelse(is.na(p), "<span class='chip gris'>—</span>",
  ifelse(p < 0.01, "<span class='chip verde'>1 %</span>",
  ifelse(p < 0.05, "<span class='chip verde'>5 %</span>",
  ifelse(p < 0.10, "<span class='chip ambar'>10 %</span>",
                   "<span class='chip gris'>no sig.</span>"))))
}

# anchos: porcentajes por columna. Si se pasan, la tabla usa layout fijo,
# que es lo único que evita que una columna de texto largo aplaste al resto.
# num_desde: a partir de qué columna los valores son números y van
# alineados a la derecha. Por defecto la tercera.
tabla <- function(encabezados, filas, anchos = NULL, num_desde = 3) {
  if (length(filas) == 0) return("<p class='vacio'>Sin datos.</p>")
  th <- paste0("<th>", encabezados, "</th>", collapse = "")
  cols <- if (is.null(anchos)) NULL else paste0(
    "<colgroup>",
    paste0("<col style=\"width:", anchos, "%\">", collapse = ""),
    "</colgroup>")
  clase <- paste(c(if (!is.null(anchos)) "fija",
                   if (num_desde == 2) "num2"), collapse = " ")
  c(paste0("<table", if (nzchar(clase)) paste0(" class=\"", clase, "\"") else "",
           ">"),
    cols, paste0("<thead><tr>", th, "</tr></thead>"),
    "<tbody>", filas, "</tbody></table>")
}

fila <- function(...) {
  celdas <- list(...)
  paste0("<tr>", paste0("<td>", unlist(celdas), "</td>", collapse = ""), "</tr>")
}

callout <- function(tono, titulo, cuerpo) {
  paste0("<div class='callout ", tono, "'><p class='ct'>", titulo, "</p><p>",
         cuerpo, "</p></div>")
}

# --- Armazón del documento ------------------------------------------------

ESTILO_HTML <- c(
':root{--tinta:#37352f;--suave:#787774;--linea:#e9e9e7;--fondo:#fff;',
'  --verde:#0f7b6c;--verdebg:#ddedea;--rojo:#b3261e;--rojobg:#fbe4e2;',
'  --ambar:#9d6c00;--ambarbg:#fbf3db;--grisbg:#f1f1ef;--azulbg:#e7f3f8}',
'*{box-sizing:border-box}',
'body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Helvetica,',
'  Arial,sans-serif;color:var(--tinta);background:var(--fondo);',
'  max-width:1080px;margin:0 auto;padding:4rem 2.5rem 6rem;line-height:1.6;',
'  font-size:16px;-webkit-font-smoothing:antialiased}',
'h1{font-size:2.5rem;font-weight:700;letter-spacing:-.02em;margin:0 0 .25rem}',
'h2{font-size:1.5rem;font-weight:600;letter-spacing:-.01em;',
'  margin:3.5rem 0 .25rem;padding-bottom:.4rem;border-bottom:1px solid var(--linea)}',
'h3{font-size:1.1rem;font-weight:600;margin:2rem 0 .25rem}',
'p{margin:.6rem 0}',
'p.bajada{color:var(--suave);font-size:1.05rem;margin:0 0 2rem}',
'p.meta{color:var(--suave);font-size:.88rem;margin:.1rem 0 .7rem}',
'p.sub,span.sub{color:var(--suave);font-size:.82rem}',
'p.vacio{color:var(--suave);font-style:italic}',
'code{font-family:"SF Mono",Menlo,Consolas,monospace;font-size:.85em;',
'  background:var(--grisbg);padding:.12em .4em;border-radius:3px}',
'table{border-collapse:collapse;width:100%;margin:.8rem 0 1.4rem;',
'  font-size:.9rem}',
'th{text-align:left;font-weight:600;color:var(--suave);font-size:.78rem;',
'  text-transform:uppercase;letter-spacing:.04em;padding:.5rem .7rem;',
'  border-bottom:1px solid var(--linea);vertical-align:bottom}',
'td{padding:.6rem .7rem;border-bottom:1px solid var(--linea);',
'  vertical-align:top}',
'tbody tr:hover{background:#fafafa}',
'td:nth-child(n+3){text-align:right;font-variant-numeric:tabular-nums}',
'table.fija{table-layout:fixed;font-size:.83rem}',
'table.fija td{text-align:left;word-break:normal;overflow-wrap:anywhere;',
'  hyphens:auto}',
'table.fija td:first-child{word-break:break-all}',
'table.fija span.unidad{white-space:normal}',
'table.num2 td:nth-child(n+2){text-align:right;',
'  font-variant-numeric:tabular-nums}',
'span.chip{display:inline-block;padding:.1em .55em;border-radius:3px;',
'  font-size:.78rem;font-weight:600;white-space:nowrap}',
'.verde{background:var(--verdebg);color:var(--verde)}',
'.rojo{background:var(--rojobg);color:var(--rojo)}',
'.ambar{background:var(--ambarbg);color:var(--ambar)}',
'.gris{background:var(--grisbg);color:var(--suave)}',
'span.unidad{color:var(--suave);font-size:.82rem;white-space:nowrap}',
'div.callout{display:block;padding:1rem 1.2rem;border-radius:4px;',
'  margin:1.2rem 0;font-size:.94rem}',
'div.callout p{margin:.3rem 0}',
'p.ct{font-weight:600}',
'.cnota{background:var(--grisbg)}',
'.cinfo{background:var(--azulbg)}',
'.calerta{background:var(--ambarbg)}',
'.cmal{background:var(--rojobg)}',
'img{max-width:100%;border:1px solid var(--linea);border-radius:4px;',
'  margin:1rem 0}',
'hr{border:0;border-top:1px solid var(--linea);margin:3rem 0}',
'footer{color:var(--suave);font-size:.85rem;margin-top:4rem}'
)

encabezado_html <- function(pestania, titulo, bajada) {
  c('<!DOCTYPE html>',
    '<html lang="es"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    paste0('<title>', pestania, '</title>'),
    '<style>', ESTILO_HTML, '</style></head><body>',
    paste0('<h1>', titulo, '</h1>'),
    paste0('<p class="bajada">', bajada, '</p>'))
}

pie_html <- function(script, cierre = NULL) {
  c('<hr>', '<footer>',
    paste0('Generado por <code>scripts/', script, '</code> el ',
           format(Sys.Date(), "%d/%m/%Y"),
           '. Se rehace entero en cada <code>Rscript run_all.R</code>. '),
    cierre,
    '</footer>', '</body></html>')
}
