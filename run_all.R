# ---------------------------------------------------------------------------
# run_all.R
# Corre el trabajo completo de punta a punta, en orden.
#
# Desde la raíz del proyecto:
#     Rscript run_all.R
#
# Cada script se ejecuta en su propio entorno para que las variables de uno
# no pisen las de otro. La configuración (cfg) es lo único compartido.
# ---------------------------------------------------------------------------

inicio <- Sys.time()

source("scripts/00_config.R")
source("scripts/utilidades.R")

# Se borra el registro de la corrida anterior para que no queden avisos
# viejos mezclados con los nuevos.
ruta_no_estimado <- file.path(cfg$dir_resultados, "no_estimado.csv")
if (file.exists(ruta_no_estimado)) file.remove(ruta_no_estimado)

pasos <- c(
  "scripts/01_curado.R",
  "scripts/02_descriptivos.R",
  "scripts/03_pruebas.R",
  "scripts/04_estimacion.R",
  "scripts/05_descomposicion.R",
  "scripts/06_convergencia.R",
  "scripts/07_informe_html.R",
  "scripts/08_simulacion.R",
  "scripts/09_informe_simulacion.R",
  "scripts/10_riesgo_brasil.R",
  "scripts/11_informe_riesgo_brasil.R",
  "scripts/12_informe_preguntas.R",
  "scripts/13_informe_hipotesis.R",
  "scripts/14_anexo_datos.R"
)

for (paso in pasos) {
  cat("\n", strrep("-", 72), "\n", sep = "")
  source(paso, local = new.env(parent = globalenv()))
}

cat("\n", strrep("-", 72), "\n", sep = "")
generar_informe()
mensaje("Informe escrito: informe.md")

cat("\n", strrep("=", 72), "\n", sep = "")
mensaje("Listo en ", round(as.numeric(difftime(Sys.time(), inicio,
                                               units = "secs")), 1),
        " segundos.")
mensaje("Panel, diccionario y base del anexo: ", cfg$dir_curados, "/")
mensaje("Tablas y figuras:    ", cfg$dir_resultados, "/")
mensaje("Resumen de la corrida: informe.md")

if (file.exists(ruta_no_estimado)) {
  pendientes <- utils::read.csv(ruta_no_estimado, stringsAsFactors = FALSE)
  mensaje("\nATENCIÓN: ", nrow(pendientes), " etapa(s) no se ejecutaron por ",
          "falta de datos. El detalle está en ", ruta_no_estimado,
          " y en la sección 2 de informe.md.")
}
