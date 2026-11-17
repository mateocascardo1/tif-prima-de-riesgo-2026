# ---------------------------------------------------------------------------
# 00_config.R
# Trabajo de Integración Final — brecha de precio de la tierra agrícola
# Argentina (zona núcleo) vs. Brasil (Mato Grosso) vs. EE. UU. (Iowa), 2005-2025
#
# Este archivo concentra TODAS las decisiones del trabajo. Si hay que cambiar
# algo (qué variable fiscal se usa, qué regresores entran, cuántos años),
# se cambia acá y en ningún otro lado.
#
# Nada de esto toca los datos crudos: la carpeta data/ se lee y nunca se escribe.
# ---------------------------------------------------------------------------

cfg <- list()

# --- Carpetas -------------------------------------------------------------
# Los scripts se corren desde la raíz del proyecto (ver README.md).
cfg$dir_crudos     <- "data"          # datos originales, solo lectura
cfg$dir_curados    <- "02_curados"    # panel.csv y diccionario.csv
cfg$dir_resultados <- "03_resultados" # tablas, figuras y avisos

# --- Período --------------------------------------------------------------
# 21 años por país, 63 filas en total. Los archivos que traen años fuera de
# este rango (por ejemplo el precio argentino, que arranca en 1995 y llega a
# 2026) se recortan acá; no se usan para rellenar nada.
cfg$anio_min <- 2005
cfg$anio_max <- 2025

# Año base para expresar los precios en dólares constantes.
cfg$anio_base_precios <- 2025

# --- Países ---------------------------------------------------------------
# El nombre del país es la clave del panel. La región exacta a la que
# corresponde cada serie está documentada en el diccionario, porque no
# coinciden entre sí (el precio argentino es zona núcleo y el rendimiento es
# toda la provincia de Buenos Aires, por ejemplo).
cfg$paises <- c("Argentina", "Brasil", "Estados Unidos")

# --- Decisiones parametrizables ------------------------------------------

# Qué variable representa la política fiscal sobre el sector.
#   "alicuota" -> derechos de exportación a la soja (solo existe para Argentina)
#   "pse"      -> %EAP (PSE) de la OCDE
# Hoy no hay archivo de PSE en data/, así que con "pse" el curado se detiene
# con un mensaje explícito en vez de inventar la serie.
cfg$variable_fiscal <- "alicuota"

# Control climático. Entra en los tres países o en ninguno. Mato Grosso 2025
# se completa con enero y febrero de Cuiabá, porque Diamantino no midió esos
# dos meses; la procedencia queda declarada en el diccionario.
cfg$incluir_precipitacion <- TRUE

# Riesgo soberano. Entra en Argentina y Brasil. 2008-2025 es el EMBI Global
# Diversified (BCRD). 2005-2007 es EMBI+, porque la planilla pública del
# Global Diversified arranca el 2007-10-29 y esos dos meses no alcanzan para
# un promedio anual. EE. UU. no lleva la serie.
cfg$usar_embi <- TRUE

# Alícuota de retenciones. Casi toda la serie se verificó contra el Boletín
# Oficial y es la alícuota nominal vigente al cierre de cada mes. La
# excepción es marzo a junio de 2008, cuando rigió la móvil de la Res.
# 125/2008: ahí se toma el 35 % del régimen que la antecedió y que el Dto.
# 1176/2008 repuso. En TRUE ese año entra al panel y el informe lo declara;
# en FALSE queda NA.
# La brecha cambiaria no usa esta alícuota: es el diferencial de mercado,
# aparte de las retenciones.
cfg$aceptar_alicuotas_no_nominales <- TRUE

# Meses mínimos para dar por válido el promedio anual del CPI-U. El BLS no
# publicó octubre de 2025 por el cierre del gobierno federal, así que 2025
# tiene once meses. Con 12 acá, el año base queda NA y no hay deflactor ni
# dependiente; con 11, 2025 entra con los meses que existen y el informe lo
# declara. No se interpola el mes faltante.
cfg$cpi_meses_minimos <- 11

# Especificación de la ecuación, según lo que digan las pruebas de raíz
# unitaria en 03_pruebas.R:
#   "niveles_MCO" -> series I(0)
#   "ARDL"        -> series I(1) con cointegración
#   "diferencias" -> series I(1) sin cointegración
cfg$especificacion <- "niveles_MCO"

# Descomposición de Oaxaca-Blinder: qué coeficientes se usan como referencia.
#   "argentina" -> especificación principal del trabajo
#   "pooled"    -> solo como ejercicio de robustez
cfg$referencia_oaxaca <- "argentina"

# Escenarios de convergencia institucional hacia Brasil (06_convergencia.R).
cfg$grados_convergencia <- c(0.25, 0.50, 1.00)

# Precio argentino año por año reemplazando solo el riesgo país
# (10_riesgo_brasil.R). En cada año se cambia esa variable por el valor del
# país de origen y todo lo demás queda en el valor argentino de ese año,
# incluida la alícuota: si el país de origen no la tiene, no se la pone en
# cero. Los coeficientes son los de la ecuación en niveles ya estimada; no
# se reestima. Si la variable no está en esa ecuación, el script no publica
# el gráfico.
cfg$pais_precio <- "Argentina"
cfg$pais_riesgo_reemplazo <- "Brasil"
cfg$variable_riesgo_pais <- "embi_pb"

# --- Ventana en la que se evalúan las hipótesis ---------------------------
# La brecha con Mato Grosso cambia de signo adentro del período. Entre 2005 y
# 2013 Brasil estuvo en promedio 0,44 puntos logarítmicos POR DEBAJO de
# Argentina; entre 2021 y 2025 quedó 0,48 POR ENCIMA. El promedio de los
# veintiún años (-0,08) es el resultado de que esas dos épocas se cancelen
# entre sí, y no describe ningún año del panel. Descomponer esa media sería
# explicar una brecha que nunca existió.
#
# Por eso H1 y H2 se evalúan en las MEDIAS de esta ventana. Los COEFICIENTES
# siguen saliendo de los veintiún años: con cinco observaciones y seis a ocho
# regresores la ecuación no tiene solución, así que recortar también la
# estimación no es una opción disponible.
#
# El costo del arreglo, que el informe declara: se supone que las
# sensibilidades no cambiaron en el quiebre. Ese supuesto NO se puede testear,
# porque una prueba de Chow que parta 21 años en 9 y 5 no tiene grados de
# libertad. Lo que sí queda medido es cuánto de la brecha reciente la ecuación
# no logra explicar: es el componente residual de la descomposición, que vale
# cero por construcción sobre el período completo y no sobre la ventana.
#
# El período completo se sigue calculando y queda como anexo, en las mismas
# tablas, bajo la columna 'ventana'.
cfg$ventana_medias <- c(2021, 2025)

# --- Regresores por país --------------------------------------------------
# Con 21 observaciones por ecuación el límite práctico es 5-7 regresores.
# Criterio: UN indicador por concepto, no varios del mismo concepto.
#   productivo   -> rendimiento (la proxy más directa de la renta por hectárea)
#   económico    -> precio de la soja, tipo de cambio real, tasa real,
#                   inflación de cierre y tasa nominal local
#   institucional-> la variable fiscal, y el riesgo soberano si se habilita
#
# Dos variables del diseño entran aunque no cubran los 21 años. El hueco no
# se rellena: se cae ese año de la ecuación de ese país, y de ninguna otra.
# Los dos huecos caen fuera de la ventana 2021-2025, así que las medias con
# las que se responden las hipótesis no cambian.
#   maquinaria -> lo que hay son VENTAS, no el stock por hectárea. Entra como
#                 logaritmo de ventas por hectárea sembrada, para que un país
#                 grande no pese más solo por vender más unidades. Sigue sin
#                 ser stock: convertirlo exigiría una vida útil que nadie
#                 midió. EE. UU. no tiene 2005 ni 2006, y esos dos años salen
#                 de su ecuación.
#   diésel     -> reemplaza al flete. Brasil 2020 queda vacío porque la ANP
#                 no relevó septiembre, y ese año sale de la ecuación
#                 brasileña. Argentina y EE. UU. están completos.
#   urea       -> costo del fertilizante. Es el precio internacional, el
#                 mismo en los tres países, en logaritmo. 2005-2025 tiene
#                 los doce meses. Desde marzo de 2022 la cotización pasa de
#                 f.o.b. Mar Negro a f.o.b. Medio Oriente: el corte lo
#                 publica el Banco Mundial y no se empalma con otra serie.
# Una sigue afuera:
#   crédito / PBI agropecuario -> el cociente está armado, pero Argentina no
#                 tiene NINGÚN año de 2021 a 2025, que es la ventana de las
#                 hipótesis, y Brasil solo tiene 2012-2021. Meterlo dejaría a
#                 Brasil con 9 años y más regresores que años: la ecuación no
#                 tiene solución. No se completan los años que faltan.
# La brecha cambiaria entra solo en Argentina. Cubre 2005-2025 y cambia de
# cotización en 2013 (blue de Ámbito hasta 2012, CCL desde 2013). El empalme
# queda declarado. Brasil y EE. UU. no tienen la serie: en la descomposición
# su media vale 0, y en el escenario institucional se la pone en cero.
#
# La tasa real local SÍ entra, en los tres países, y no reemplaza a los
# TIPS: los TIPS son la tasa real internacional, la misma en los tres
# países, mientras que esta varía por país y puede explicar diferencias de
# nivel. Signo esperado negativo: cuanto más caro el dinero en términos
# reales, más se descuenta la renta futura de la tierra.
#   tasa real local -> Fisher exacto sobre la tasa nominal (BADLAR, meta
#                 Selic, meta de fed funds) y la inflación de cierre (IPC de
#                 San Luis, IPCA y CPI de diciembre).
# La inflación y la tasa nominal NO entran por separado. Estaban las dos y
# eran el mismo concepto medido dos veces: en Argentina daban VIF de 63,8 y
# 36,7, se tapaban entre sí y se llevaban un grado de libertad sin aportar
# variación propia. Combinadas en la tasa real, ningún VIF pasa de 10 en
# ninguna ecuación y el ajuste mejora en los tres países. Quedan en el panel
# como descriptivas.
# El Treasury nominal (USGG10YR) sigue afuera: no es la tasa real de los
# TIPS y no la reemplaza.
# Con usar_embi, incluir_precipitacion, las ventas de maquinaria, el diésel,
# la urea y la brecha cambiaria, Argentina queda con 12 regresores. Es más
# que el techo práctico de 7: con 21 años quedan 8 grados de libertad.
# Brasil queda en 20 años (sin 2020) y 10 regresores; EE. UU. en 19 años
# (sin 2005 y 2006) y 9 regresores. Esas dos quedan en 9 grados de libertad.
# El costo está declarado, no escondido. La urea es el mismo precio en los tres países,
# así que su aporte al componente de niveles de Oaxaca es cero por
# construcción: mueve el ajuste de cada ecuación, no la brecha de niveles.
cfg$regresores <- list(
  "Argentina" = c(
    "rendimiento_soja_t_ha",
    "ln_precio_soja_cbot",
    "tcr_bis_2005_2025_100",
    "tips_10a_pct",
    "tasa_real_local_pct",
    "alicuota_export_soja_pct",
    "brecha_cambiaria_pct",
    "ln_ventas_maquinaria_por_ha",
    "diesel_usd_l",
    "ln_urea"
  ),
  "Brasil" = c(
    "rendimiento_soja_t_ha",
    "ln_precio_soja_cbot",
    "tcr_bis_2005_2025_100",
    "tips_10a_pct",
    "tasa_real_local_pct",
    "ln_ventas_maquinaria_por_ha",
    "diesel_usd_l",
    "ln_urea"
  ),
  # EE. UU. no lleva riesgo soberano, ni alícuota, ni brecha cambiaria.
  "Estados Unidos" = c(
    "rendimiento_soja_t_ha",
    "ln_precio_soja_cbot",
    "tcr_bis_2005_2025_100",
    "tips_10a_pct",
    "tasa_real_local_pct",
    "ln_ventas_maquinaria_por_ha",
    "diesel_usd_l",
    "ln_urea"
  )
)

# Huecos que no frenan la estimación. El año vacío se cae de la ecuación de
# ese país y no se rellena. Cualquier otro hueco sigue frenando todo.
cfg$huecos_aceptados <- c("ln_ventas_maquinaria_por_ha", "diesel_usd_l")

# El EMBI entra solo si se habilita, y solo donde la serie existe.
if (isTRUE(cfg$usar_embi)) {
  cfg$regresores[["Argentina"]] <- c(cfg$regresores[["Argentina"]], "embi_pb")
  cfg$regresores[["Brasil"]]    <- c(cfg$regresores[["Brasil"]], "embi_pb")
}

# El control climático entra en los tres países o en ninguno.
if (isTRUE(cfg$incluir_precipitacion)) {
  for (p in cfg$paises) {
    cfg$regresores[[p]] <- c(cfg$regresores[[p]], "precipitacion_desvio_mm")
  }
}

# Si la variable fiscal elegida es el PSE, reemplaza a la alícuota.
if (identical(cfg$variable_fiscal, "pse")) {
  cfg$regresores <- lapply(cfg$regresores, function(v) {
    v <- setdiff(v, "alicuota_export_soja_pct")
    c(v, "pse_ocde_pct")
  })
}

# --- Variable dependiente -------------------------------------------------
cfg$dependiente <- "ln_precio_ha_usd_2025"

# --- Categorías de cada variable -----------------------------------------
# Se usa para agregar la descomposición por categoría y para saber qué
# variables se reemplazan en el escenario de precio potencial.
cfg$categorias <- c(
  rendimiento_soja_t_ha       = "productivo",
  precipitacion_desvio_mm     = "productivo",
  ln_maquinaria_por_ha        = "productivo",
  ln_ventas_maquinaria_por_ha = "productivo",
  diesel_usd_l                = "productivo",
  urea_usd_t                  = "productivo",
  ln_urea                     = "productivo",
  precio_soja_cbot_usd_t      = "economico",
  ln_precio_soja_cbot         = "economico",
  tcr_bis_2005_2025_100       = "economico",
  tips_10a_pct                = "economico",
  treasury_us_10a_nominal_pct = "economico",
  inflacion_dic_pct           = "economico",
  tasa_nominal_local_pct      = "economico",
  tasa_real_local_pct         = "economico",
  alicuota_export_soja_pct    = "institucional",
  pse_ocde_pct                = "institucional",
  embi_pb                     = "institucional",
  brecha_cambiaria_pct        = "institucional",
  ln_credito_sobre_pbi_agro   = "institucional",
  credito_agro_musd           = "institucional"
)

# --- Inferencia -----------------------------------------------------------
# Errores estándar Newey-West (HAC). El rezago sigue la regla habitual
# floor(4 * (n/100)^(2/9)); con n = 21 da 2.
cfg$rezago_hac <- function(n) floor(4 * (n / 100)^(2 / 9))

# --- Pruebas conjuntas por bloque ----------------------------------------
# La tabla de coeficientes mira una variable por vez. Con veintiún años eso
# le juega en contra a los bloques cuyas variables se mueven juntas: la
# ecuación no puede repartir el mérito entre ellas, así que ninguna
# sobresale por separado aunque el grupo sí importe. En Argentina las dos
# variables institucionales caen justo en ese caso.
#
# Por eso cada bloque se prueba además en conjunto (H0: todos sus
# coeficientes son cero a la vez). Es la pregunta que corresponde, porque H1
# está escrita sobre un grupo de factores y no sobre una variable suelta.
# La prueba NO toca ningún coeficiente ni ninguna ecuación: cambia qué se le
# pregunta a la ecuación ya estimada.
#
# Dos cosas que la prueba no hace, y que el informe declara:
#   - No arregla la endogeneidad de la alícuota. Un bloque institucional
#     significativo dice que esas variables están asociadas al precio de la
#     tierra, no que lo causen.
#   - No cambia el veredicto de H1, que compara el TAMAÑO de las
#     contribuciones en la descomposición y no si los coeficientes son cero.
#
# No se combinan las variables de un bloque en un índice único. Para hacerlo
# habría que elegir una ponderación, y ninguna teoría dice cuál: las
# retenciones recortan el ingreso y el riesgo país encarece la tasa de
# descuento, que son lugares distintos de la valuación. La regresión ya
# pondera con los coeficientes, que salen de los datos. Elegir pesos a mano
# sería reemplazar evidencia por criterio propio. La única combinación que
# el trabajo hace es la tasa real, y ahí la ponderación la fija una
# identidad exacta (Fisher), no una decisión.
#
# El p-valor que se informa es el de la versión F de la prueba, no el de la
# chi-cuadrado: la segunda vale para muestras grandes y con n = 21 rechaza
# de más. La diferencia no es cosmética. Para el bloque institucional de
# Argentina, la chi-cuadrado da 0,008 y la F da 0,028: las dos son
# significativas al 5 %, pero solo una es defendible.
cfg$nivel_bloques <- 0.05

# --- Capitalización del precio internacional ------------------------------
# La dependiente y el precio de la soja están los dos en logaritmo, así que
# el coeficiente de la soja se lee directo: cuánto sube el precio de la
# hectárea, en porcentaje, cuando la soja sube un uno por ciento.
#
# El valor 1 no es un número cualquiera. Es lo que predice la valuación por
# ingreso que arma el marco teórico: si el precio internacional sube de
# forma permanente y nada más cambia, el ingreso por hectárea sube en la
# misma proporción y el campo tendría que subir lo mismo. Que el coeficiente
# valga 1 se llama capitalización plena. Por eso, además del test habitual
# (¿el coeficiente es cero?), se prueba H0: el coeficiente vale 1.
#
# Esa segunda prueba es la que convierte un coeficiente argentino que "no da
# significativo" en un resultado: la ecuación no puede distinguir si vale
# cero o un cuarto, pero sí puede descartar que valga uno.
#
# Cuidado que obliga a una fila extra: la ecuación argentina controla por la
# alícuota y la brasileña no la tiene. Con la alícuota adentro, el
# coeficiente de la soja mide el efecto A RETENCIÓN FIJA, y en Brasil mide
# el efecto total. Comparar esos dos números sería comparar cosas distintas,
# así que la tabla repite Argentina con la misma lista de variables que
# Brasil.
cfg$variable_capitalizacion <- "ln_precio_soja_cbot"
# Suba del precio internacional con la que se expresa el resultado, para que
# el número se lea en precios y no en logaritmos.
cfg$suba_referencia_pct <- 10

# Bootstrap en bloques para los intervalos de la descomposición.
# El largo del bloque es round(n^(1/3)): con n = 21 son bloques de 3 años,
# suficiente para conservar la autocorrelación de series anuales cortas.
cfg$bootstrap_B      <- 999
cfg$largo_bloque     <- function(n) max(2L, round(n^(1 / 3)))
cfg$nivel_confianza  <- 0.95
cfg$semilla          <- 2025  # para que el bootstrap sea reproducible

# --- Verificación del entorno --------------------------------------------
if (!dir.exists(cfg$dir_crudos)) {
  stop(
    "No se encuentra la carpeta '", cfg$dir_crudos, "'. ",
    "Los scripts se corren desde la raíz del proyecto: Rscript run_all.R",
    call. = FALSE
  )
}

for (d in c(cfg$dir_curados, cfg$dir_resultados)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}
