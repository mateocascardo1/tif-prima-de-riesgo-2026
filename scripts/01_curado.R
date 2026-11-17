# ---------------------------------------------------------------------------
# 01_curado.R
# De los archivos crudos de data/ al panel país-año.
#
# Reglas que se respetan en todo el script:
#   1. data/ se lee y nunca se escribe.
#   2. No se interpola, no se extrapola y no se rellena ningún año.
#      Si un año no tiene dato, queda NA y el motivo va al diccionario.
#   3. Toda transformación (sacar el "USD", pasar pulgadas a milímetros,
#      promediar meses) está escrita acá, no en una planilla.
#
# Salidas: 02_curados/panel.csv y 02_curados/diccionario.csv
# ---------------------------------------------------------------------------

if (!exists("cfg")) source("scripts/00_config.R")
source("scripts/utilidades.R")

mensaje("== 01_curado: armando el panel ==")

faltantes <- nuevo_registro_faltantes()
anios <- cfg$anio_min:cfg$anio_max

# Esqueleto del panel: 3 países x 21 años = 63 filas, siempre las mismas.
panel <- expand.grid(
  anio = anios,
  pais = cfg$paises,
  stringsAsFactors = FALSE
)[, c("pais", "anio")]
panel <- panel[order(panel$pais, panel$anio), ]
rownames(panel) <- NULL

# Pega una serie (data.frame con pais, anio y una columna de valores) al panel.
pegar <- function(panel, datos, columna) {
  clave_panel <- paste(panel$pais, panel$anio)
  clave_datos <- paste(datos$pais, datos$anio)
  panel[[columna]] <- datos[[columna]][match(clave_panel, clave_datos)]
  panel
}

# ---------------------------------------------------------------------------
# 1. Precio de la tierra en USD corrientes por hectárea
# ---------------------------------------------------------------------------
# Argentina: la planilla trae los valores como texto ("USD 8,000"), con
# espacios de más en algunos años. Cubre 1995-2026 y se recorta al período.
ruta_ar <- file.path(cfg$dir_crudos, "Valores de campos de 1995 a 2026.xlsx - Hoja1.csv")
crudo_ar <- leer_csv_crudo(ruta_ar, saltar = 4)   # las 4 primeras filas son título
crudo_ar <- filas_con_anio(crudo_ar, 1)
precio_ar <- data.frame(
  pais = "Argentina",
  anio = a_numero(crudo_ar[[1]]),
  precio_ha_usd_corriente = a_numero(crudo_ar[[2]]),
  stringsAsFactors = FALSE
)

# Brasil (Mato Grosso) y EE. UU. (Iowa): un solo archivo, 2002-2025.
# Se recorta a 2005-2025, que es donde Iowa tiene una única fuente (ISU).
ruta_mt_ia <- file.path(cfg$dir_crudos,
                        "Precio de la tierra - precio_tierra_MT_Iowa_2002_2025.csv.csv")
crudo_mt_ia <- filas_con_anio(leer_csv_crudo(ruta_mt_ia), 1)
precio_br <- data.frame(
  pais = "Brasil",
  anio = a_numero(crudo_mt_ia$anio),
  precio_ha_usd_corriente = a_numero(crudo_mt_ia$mato_grosso_usd_ha),
  stringsAsFactors = FALSE
)
precio_us <- data.frame(
  pais = "Estados Unidos",
  anio = a_numero(crudo_mt_ia$anio),
  precio_ha_usd_corriente = a_numero(crudo_mt_ia$iowa_usd_ha),
  stringsAsFactors = FALSE
)

panel <- pegar(panel, rbind(precio_ar, precio_br, precio_us),
               "precio_ha_usd_corriente")

# ---------------------------------------------------------------------------
# 2. Deflactor: IPC de EE. UU. (CPI-U)
# ---------------------------------------------------------------------------
# El trabajo define la dependiente en dólares constantes del año base. Los
# tres precios ya vienen en dólares, así que lo único que hay que neutralizar
# es la pérdida de poder de compra del dólar: un solo deflactor, el de
# EE. UU., para los tres países.
#
# El crudo es la serie mensual CPI-U sin desestacionalizar (BLS CUUR0000SA0,
# descargada de FRED como CPIAUCNS). El promedio anual se calcula acá, no en
# una planilla. Los 251 meses coinciden exactamente con los que devuelve la
# API del BLS.
ruta_cpi <- file.path(cfg$dir_crudos, "cpi_eeuu_mensual_CPIAUCNS.csv")
panel$cpi_eeuu <- NA_real_
anios_cpi_parcial <- integer(0)

if (file.exists(ruta_cpi)) {
  crudo_cpi <- leer_csv_crudo(ruta_cpi)
  cpi <- promedio_anual(crudo_cpi$observation_date,
                        a_numero(crudo_cpi$CPIAUCNS),
                        meses_minimos = cfg$cpi_meses_minimos)
  panel$cpi_eeuu <- cpi$valor[match(panel$anio, cpi$anio)]

  # Años que entran con menos de doce meses: hay que declararlos.
  parciales <- cpi[cpi$meses < 12, , drop = FALSE]
  anios_cpi_parcial <- parciales$anio
  if (nrow(parciales) > 0) {
    faltantes <- agregar_faltante(
      faltantes, "curado", "cpi_eeuu",
      paste0(
        "El promedio anual de ", paste(parciales$anio, collapse = ", "),
        " se calcula con ", paste(parciales$meses, collapse = ", "),
        " meses y no con doce: el BLS no publicó el CPI de octubre de 2025 ",
        "por el cierre del gobierno federal. El faltante no se interpola. ",
        "Como el deflactor entra siempre como cociente contra el año base, ",
        "un sesgo en el nivel del CPI multiplica por igual a los tres ",
        "países en todos los años: en logaritmos es una constante que ",
        "absorbe el intercepto y no mueve ningún coeficiente, brecha ni ",
        "descomposición. Medido sobre 2024, el sesgo de excluir octubre ",
        "es de -0,06 %."
      )
    )
  }
} else {
  faltantes <- agregar_faltante(
    faltantes, "curado", "cpi_eeuu",
    paste0("No hay archivo de IPC de EE. UU. (CPI-U) en data/. ",
           "Sin deflactor, el precio no se puede expresar en dólares de ",
           cfg$anio_base_precios, " y la variable dependiente queda vacía. ",
           "Se espera data/cpi_eeuu_mensual_CPIAUCNS.csv, la serie mensual ",
           "con las columnas observation_date y CPIAUCNS.")
  )
}

# Precio real y logaritmo. Si falta el IPC, las dos columnas quedan NA.
cpi_base <- unique(panel$cpi_eeuu[panel$anio == cfg$anio_base_precios])
cpi_base <- cpi_base[!is.na(cpi_base)]
if (length(cpi_base) == 1) {
  panel$precio_ha_usd_2025 <- panel$precio_ha_usd_corriente * cpi_base / panel$cpi_eeuu
} else {
  panel$precio_ha_usd_2025 <- NA_real_
}
panel$ln_precio_ha_usd_2025 <- ifelse(
  is.na(panel$precio_ha_usd_2025) | panel$precio_ha_usd_2025 <= 0,
  NA_real_, log(panel$precio_ha_usd_2025)
)

# ---------------------------------------------------------------------------
# 3. Rendimiento y superficie sembrada de soja
# ---------------------------------------------------------------------------
# Se leen las hojas por país, no la de comparación, porque las hojas por país
# traen además la superficie sembrada. Las tres terminan con filas de fuente
# y notas, que filas_con_anio() descarta.
leer_productividad <- function(archivo, pais, col_anio, col_rinde, col_superficie) {
  crudo <- filas_con_anio(leer_csv_crudo(file.path(cfg$dir_crudos, archivo)), col_anio)
  data.frame(
    pais = pais,
    anio = a_numero(crudo[[col_anio]]),
    rendimiento_soja_t_ha       = a_numero(crudo[[col_rinde]]),
    superficie_sembrada_soja_ha = a_numero(crudo[[col_superficie]]),
    stringsAsFactors = FALSE
  )
}

rinde <- rbind(
  leer_productividad("Productividad - Argentina (Buenos Aires).csv", "Argentina",
                     "Año (cosecha)", "Rendimiento (t/ha)", "Superficie sembrada (ha)"),
  leer_productividad("Productividad - Brasil.csv", "Brasil",
                     "Año (cosecha)", "Rendimiento (t/ha)", "Superficie sembrada (ha)"),
  leer_productividad("Productividad - Estados Unidos.csv", "Estados Unidos",
                     "Año (cosecha)", "Rendimiento (t/ha)", "Superficie sembrada (ha)")
)
panel <- pegar(panel, rinde, "rendimiento_soja_t_ha")
panel <- pegar(panel, rinde, "superficie_sembrada_soja_ha")

# ---------------------------------------------------------------------------
# 4. Maquinaria agrícola
# ---------------------------------------------------------------------------
# El trabajo pide stock de maquinaria por hectárea sembrada. El archivo que
# hay son VENTAS anuales en unidades (tractores + cosechadoras), que es un
# flujo, no un stock, y además EE. UU. no tiene 2005 ni 2006.
# Entra igual, como logaritmo de ventas por hectárea sembrada: es lo que hay,
# y dividir por la superficie hace comparables a los tres países. No se
# convierte a stock, porque eso exigiría una vida útil que nadie midió. Los
# dos años vacíos de EE. UU. se caen de su ecuación y no se rellenan.
ruta_maq <- file.path(cfg$dir_crudos, "Venta_maquinaria_agricola_AR_BR_US_2005-2025.xlsx")
panel$maquinaria_vendida_unidades <- NA_real_
if (requireNamespace("readxl", quietly = TRUE) && file.exists(ruta_maq)) {
  crudo_maq <- as.data.frame(readxl::read_excel(ruta_maq, sheet = "Series",
                                                col_names = FALSE, .name_repair = "minimal"))
  # Columnas (ver la hoja): 1 = año, 7 = Argentina, 10 = Brasil, 13 = EE. UU.,
  # las tres en "tractores + cosechadoras".
  crudo_maq <- crudo_maq[grepl("^[0-9]{4}$", trimws(as.character(crudo_maq[[1]]))), ]
  maquinaria <- rbind(
    data.frame(pais = "Argentina", anio = a_numero(crudo_maq[[1]]),
               maquinaria_vendida_unidades = a_numero(crudo_maq[[7]])),
    data.frame(pais = "Brasil", anio = a_numero(crudo_maq[[1]]),
               maquinaria_vendida_unidades = a_numero(crudo_maq[[10]])),
    data.frame(pais = "Estados Unidos", anio = a_numero(crudo_maq[[1]]),
               maquinaria_vendida_unidades = a_numero(crudo_maq[[13]]))
  )
  panel <- pegar(panel, maquinaria, "maquinaria_vendida_unidades")
}
# Ventas por hectárea, en logaritmo. No es el stock: es el flujo de ventas
# dividido por la superficie sembrada de ese año. Queda vacío donde falta
# cualquiera de las dos, que es EE. UU. 2005 y 2006.
panel$ln_ventas_maquinaria_por_ha <- log(
  panel$maquinaria_vendida_unidades / panel$superficie_sembrada_soja_ha
)
faltantes <- agregar_faltante(
  faltantes, "curado", "ln_maquinaria_por_ha",
  paste0("El archivo de maquinaria tiene ventas anuales en unidades, no el ",
         "stock por hectárea que define el trabajo, y convertirlo exigiría ",
         "una vida útil que nadie midió. Lo que entra a las ecuaciones es ",
         "ln_ventas_maquinaria_por_ha, el logaritmo de esas ventas sobre la ",
         "superficie sembrada. EE. UU. no tiene 2005 ni 2006, y esos dos ",
         "años se caen de su ecuación: no se rellenan.")
)

# ---------------------------------------------------------------------------
# 5. Precipitación
# ---------------------------------------------------------------------------
# Tres fuentes distintas, una por región, cada una con su unidad.
# El desvío se recalcula acá con una definición única (valor menos el promedio
# del período con dato), porque cada archivo trae su propia base: la de Mato
# Grosso es 2005-2024 y la de Iowa es 2005-2025.
panel$precipitacion_mm <- NA_real_

# Zona núcleo: estación Pergamino, total anual ya calculado en el archivo.
crudo_per <- filas_con_anio(
  leer_csv_crudo(file.path(cfg$dir_crudos,
    "TIF - Precipitaciones 2005-2025 (avance) - Zona núcleo (Pergamino).csv")), 1)
lluvia_ar <- data.frame(
  pais = "Argentina",
  anio = a_numero(crudo_per[["Año"]]),
  precipitacion_mm = a_numero(crudo_per[["Total anual (mm)"]]),
  stringsAsFactors = FALSE
)

# Mato Grosso: estación Diamantino. El archivo trae el detalle mensual y, al
# costado, un bloque anual ya sumado. Acá se suma el detalle mes a mes para
# que la operación quede escrita en el código, y después se compara contra el
# bloque anual como control.
crudo_mt <- leer_csv_crudo(file.path(cfg$dir_crudos,
  "TIF - Precipitaciones 2005-2025 (avance) - Mato Grosso (INMET).csv"))
mensual_mt <- data.frame(
  anio = a_numero(crudo_mt[[1]]),
  mes  = a_numero(crudo_mt[[2]]),
  mm   = a_numero(crudo_mt[[3]]),
  stringsAsFactors = FALSE
)
mensual_mt <- mensual_mt[stats::complete.cases(mensual_mt), ]

# Enero y febrero de 2025 no existen en Diamantino: la estación convencional
# dejó de reportar y la automática del INMET en esa localidad (A949) empieza
# recién el 26/11/2025. Esos dos meses se toman de Cuiabá, capital del estado.
# Es otra estación, así que la procedencia va declarada en el diccionario y en
# la sección 13.
mensual_mt <- rbind(mensual_mt,
                    data.frame(anio = 2025, mes = c(1, 2),
                               mm = c(248.9, 189.24), stringsAsFactors = FALSE))

suma_mt  <- tapply(mensual_mt$mm, mensual_mt$anio, sum)
meses_mt <- tapply(mensual_mt$mm, mensual_mt$anio, length)
total_mt <- as.numeric(suma_mt)
total_mt[meses_mt < 12] <- NA_real_
lluvia_br <- data.frame(
  pais = "Brasil",
  anio = as.numeric(names(suma_mt)),
  precipitacion_mm = total_mt,
  stringsAsFactors = FALSE
)

# Control: donde el archivo ya trae el total anual, la suma mensual tiene que
# dar lo mismo. Si no da, cambió algo en el crudo y hay que mirarlo antes de
# seguir.
bloque_anual <- crudo_mt[grepl("^[0-9]{4}$", trimws(crudo_mt[[5]])), ]
publicado <- a_numero(bloque_anual[["Diamantino (mm)"]])
calculado <- lluvia_br$precipitacion_mm[match(a_numero(bloque_anual[[5]]),
                                              lluvia_br$anio)]
if (any(abs(calculado - publicado) > 0.1, na.rm = TRUE)) {
  stop("La suma mensual de Diamantino no coincide con el total anual del ",
       "archivo de Mato Grosso.")
}

# Corn Belt: se usa Iowa, que es el estado al que corresponde el precio de la
# tierra, y no el promedio de los diez estados del archivo. Pulgadas a mm.
crudo_noaa <- filas_con_anio(
  leer_csv_crudo(file.path(cfg$dir_crudos,
    "TIF - Precipitaciones 2005-2025 (avance) - Corn Belt (NOAA).csv")), 1)
lluvia_us <- data.frame(
  pais = "Estados Unidos",
  anio = a_numero(crudo_noaa[["Año"]]),
  precipitacion_mm = a_numero(crudo_noaa[["IA (pulg.)"]]) * 25.4,
  stringsAsFactors = FALSE
)

panel <- pegar(panel, rbind(lluvia_ar, lluvia_br, lluvia_us), "precipitacion_mm")

# Desvío respecto de la media histórica de cada región, con la misma
# definición en los tres países.
panel$precipitacion_desvio_mm <- NA_real_
for (p in cfg$paises) {
  filas <- panel$pais == p
  media <- mean(panel$precipitacion_mm[filas], na.rm = TRUE)
  panel$precipitacion_desvio_mm[filas] <- panel$precipitacion_mm[filas] - media
}

if (anyNA(panel$precipitacion_mm)) {
  sin_dato <- panel[is.na(panel$precipitacion_mm), c("pais", "anio")]
  faltantes <- agregar_faltante(
    faltantes, "curado", "precipitacion_mm",
    paste0("Años sin total anual completo: ",
           paste(paste0(sin_dato$pais, " ", sin_dato$anio), collapse = "; "),
           ". Pergamino 2019 no tiene meses en la fuente principal y su total ",
           "anual proviene del SMN, no de la misma estación.")
  )
}

# ---------------------------------------------------------------------------
# 6. Tipo de cambio real efectivo (BIS)
# ---------------------------------------------------------------------------
# El archivo viene en base 2020 = 100 y ya trae columnas en base 2005-2025.
# Igual se recalcula la base acá (valor sobre el promedio del período, por
# cien) y se compara con las columnas del archivo, como control.
# Definición: un aumento del índice es apreciación real de la moneda local,
# la misma lectura en los tres países.
crudo_tcr <- filas_con_anio(
  leer_csv_crudo(file.path(cfg$dir_crudos,
    "TIF - Tipo de cambio real BIS (AR, BR, EE. UU.) - Anual.csv"), saltar = 3), 1)

tcr <- rbind(
  data.frame(pais = "Argentina", anio = a_numero(crudo_tcr[["Año"]]),
             tcr_bis_2020_100 = a_numero(crudo_tcr[["Argentina (2020=100)"]]),
             tcr_archivo = a_numero(crudo_tcr[["Argentina (2005–2025=100)"]])),
  data.frame(pais = "Brasil", anio = a_numero(crudo_tcr[["Año"]]),
             tcr_bis_2020_100 = a_numero(crudo_tcr[["Brasil (2020=100)"]]),
             tcr_archivo = a_numero(crudo_tcr[["Brasil (2005–2025=100)"]])),
  data.frame(pais = "Estados Unidos", anio = a_numero(crudo_tcr[["Año"]]),
             tcr_bis_2020_100 = a_numero(crudo_tcr[["EE. UU. (2020=100)"]]),
             tcr_archivo = a_numero(crudo_tcr[["EE. UU. (2005–2025=100)"]]))
)
panel <- pegar(panel, tcr, "tcr_bis_2020_100")

panel$tcr_bis_2005_2025_100 <- NA_real_
for (p in cfg$paises) {
  filas <- panel$pais == p
  base <- mean(panel$tcr_bis_2020_100[filas], na.rm = TRUE)
  panel$tcr_bis_2005_2025_100[filas] <- panel$tcr_bis_2020_100[filas] / base * 100
}

# Control: si el recálculo se aparta del valor del archivo, se avisa.
tcr$clave <- paste(tcr$pais, tcr$anio)
panel$clave <- paste(panel$pais, panel$anio)
diferencia <- abs(panel$tcr_bis_2005_2025_100 -
                    tcr$tcr_archivo[match(panel$clave, tcr$clave)])
panel$clave <- NULL
if (any(diferencia > 0.05, na.rm = TRUE)) {
  faltantes <- agregar_faltante(
    faltantes, "curado", "tcr_bis_2005_2025_100",
    paste0("El recálculo de la base 2005-2025 difiere de la columna del ",
           "archivo en hasta ", round(max(diferencia, na.rm = TRUE), 3),
           " puntos. Se usa el recálculo.")
  )
}

# ---------------------------------------------------------------------------
# 7. Riesgo soberano (EMBI)
# ---------------------------------------------------------------------------
# El BCRD republica el EMBI Global Diversified de JP Morgan en puntos
# porcentuales (3,50 son 350 puntos básicos), desde el 2007-10-29. El
# promedio anual entra cuando el archivo tiene enero y diciembre: 2007 queda
# afuera porque solo trae noviembre y diciembre.
# Esos años, y 2005-2006, se completan con el EMBI+ (Ámbito para Argentina,
# Ipeadata para Brasil). Es una aproximación del nivel, no el mismo índice.
# EE. UU. queda NA. El cero del archivo viejo es relleno, no una medición.
crudo_gd <- leer_csv_crudo(file.path(cfg$dir_crudos, "embi_global_diversified_bcrd.csv"))
crudo_gd$anio <- as.integer(substr(crudo_gd$fecha, 1, 4))
crudo_gd$mes <- as.integer(substr(crudo_gd$fecha, 6, 7))
crudo_gd$argentina_pb <- a_numero(crudo_gd$argentina_pct) * 100
crudo_gd$brasil_pb <- a_numero(crudo_gd$brasil_pct) * 100
crudo_gd <- crudo_gd[crudo_gd$anio >= cfg$anio_min & crudo_gd$anio <= cfg$anio_max, ]

anios_gd <- sort(unique(crudo_gd$anio))
anios_completos <- anios_gd[vapply(anios_gd, function(a) {
  meses <- crudo_gd$mes[crudo_gd$anio == a]
  any(meses == 1) && any(meses == 12)
}, logical(1))]

promedio_pais <- function(columna, pais) {
  sub <- crudo_gd[crudo_gd$anio %in% anios_completos, c("anio", columna)]
  names(sub)[2] <- "valor"
  agg <- aggregate(valor ~ anio, data = sub, FUN = mean, na.rm = TRUE)
  data.frame(pais = pais, anio = agg$anio, embi_pb = agg$valor,
             stringsAsFactors = FALSE)
}
embi_gd <- rbind(
  promedio_pais("argentina_pb", "Argentina"),
  promedio_pais("brasil_pb", "Brasil")
)

crudo_mas <- filas_con_anio(
  leer_csv_crudo(file.path(cfg$dir_crudos,
    paste0("EMBI_promedio_anual_AR_BR_EEUU_2005-2025 - ",
           "EMBI_promedio_anual_AR_BR_EEUU_2005-2025.csv.csv"))), 1)
anios_aprox <- setdiff(cfg$anio_min:cfg$anio_max, anios_completos)
filas_aprox <- crudo_mas[a_numero(crudo_mas$anio) %in% anios_aprox, ]
embi_aprox <- rbind(
  data.frame(pais = "Argentina", anio = a_numero(filas_aprox$anio),
             embi_pb = a_numero(filas_aprox$embi_argentina_pb),
             stringsAsFactors = FALSE),
  data.frame(pais = "Brasil", anio = a_numero(filas_aprox$anio),
             embi_pb = a_numero(filas_aprox$embi_brasil_pb),
             stringsAsFactors = FALSE)
)
embi <- rbind(embi_gd, embi_aprox)
panel <- pegar(panel, embi, "embi_pb")
faltantes <- agregar_faltante(
  faltantes, "curado", "embi_pb",
  paste0("2008-2025 es el promedio anual del EMBI Global Diversified ",
         "(BCRD, republicación de JP Morgan). ",
         paste(anios_aprox, collapse = ", "),
         " no están en esa planilla, que arranca el 2007-10-29, y se ",
         "completan con el EMBI+ de Ámbito e Ipeadata como aproximación ",
         "del nivel. No se usa el tramo de dos meses de 2007. EE. UU. queda ",
         "vacío.")
)

# ---------------------------------------------------------------------------
# 8. Derechos de exportación a la soja (Argentina)
# ---------------------------------------------------------------------------
# Archivo mensual con punto y coma como separador y coma decimal. El valor
# anual es el promedio simple de los doce meses.
# Los meses marcados como "estimacion_propia" o "a_verificar" no son dato
# observado: con aceptar_alicuotas_no_nominales = FALSE, el año entero queda
# NA en vez de mezclar medición con estimación.
crudo_ret <- leer_csv_crudo(
  file.path(cfg$dir_crudos, "retenciones_soja_mensual_2005_2025 (1).csv"), sep = ";")
retenciones <- data.frame(
  anio      = a_numero(crudo_ret$anio),
  mes       = a_numero(crudo_ret$mes),
  alicuota  = a_numero(crudo_ret$alicuota_pct, decimal = ","),
  tipo_dato = trimws(crudo_ret$tipo_dato),
  stringsAsFactors = FALSE
)

# --- Meses verificados contra el Boletín Oficial --------------------------
# El archivo crudo dejaba seis años inutilizables. Acá se corrigen en el
# código, nunca en data/.
#
# CONVENCIÓN: a cada mes se le asigna la alícuota vigente al CIERRE del mes.
# Es la que ya usaba el archivo en diciembre de 2019 (30 %, aunque del 1 al
# 13 rigiera el esquema anterior), en marzo de 2020 y en noviembre de 2007.
# El archivo la rompía en enero de 2025, que traía 33 % pese a que el
# Dto. 38/2025 bajó a 26 % el 27/01; se corrige para que la serie use una
# sola regla. La alternativa sería ponderar por días dentro del mes; mueve
# el promedio anual menos de medio punto y exigiría la fecha exacta de
# todos los cambios del período, no solo de los verificados acá.
verificadas <- data.frame(
  anio     = c(2007,  2008,  2015,  2025,  2025,  2025),
  mes      = c(   1,     7,    12,     1,     7,    12),
  alicuota = c(27.5,    35,    30,    26,    33,    24),
  stringsAsFactors = FALSE
)
# ene-2007: Res. ME 10/2007 (B.O. 12/01/2007) sumó 4 p.p. y llevó la soja de
#   23,5 % a 27,5 %; rige desde el día siguiente a la publicación.
# jul-2008: el Dto. 1176/2008 y la Res. ME 180/2008 (B.O. 21/07/2008)
#   limitaron la vigencia de la Res. 125/2008 y devolvieron la alícuota fija
#   de 35 % de las Res. 368 y 369/2007.
# dic-2015: Dto. 133/2015 (B.O. 17/12/2015), soja de 35 % a 30 %.
# ene-2025: Dto. 38/2025 (B.O. 27/01/2025), baja transitoria de 33 % a 26 %
#   con vigencia hasta el 30/06/2025.
# jul-2025: venció el 30/06/2025 la baja transitoria del Dto. 38/2025 y la
#   alícuota volvió a 33 %.
# dic-2025: Dto. 877/2025 (B.O. 12/12/2025), soja de 26 % a 24 %.

clave_ret <- paste(retenciones$anio, retenciones$mes)
donde <- match(paste(verificadas$anio, verificadas$mes), clave_ret)
retenciones$alicuota[donde]  <- verificadas$alicuota
retenciones$tipo_dato[donde] <- "nominal"

# --- 2008: la alícuota móvil de la Res. 125 -------------------------------
# Del 11/03/2008 al 21/07/2008 rigió la alícuota móvil de la Res. MEyP
# 125/2008, que no era un número fijo sino una función del precio FOB y llegó
# a 46,3 %. Tras 128 días de conflicto y el rechazo en el Senado del 17/07,
# el Dto. 1176/2008 (B.O. 21/07/2008) la dejó sin efecto y repuso el 35 % de
# la Res. 368/2007,
# el mismo que regía en enero y febrero. El año se toma entero al 35 %: es la
# alícuota del régimen que gobernó antes y después, y la móvil quedó
# derogada. No es la que se cobró en marzo, abril, mayo y junio, así que esos
# cuatro meses no se marcan como nominales: el año entra solo con
# aceptar_alicuotas_no_nominales = TRUE y el informe lo declara.
moviles <- retenciones$anio == 2008 & retenciones$mes %in% 3:6
retenciones$alicuota[moviles]  <- 35
retenciones$tipo_dato[moviles] <- "regimen_repuesto"

# --- Equivalente ad valorem del tope de $4 por dólar (set-2018 a nov-2019) -
# El Dto. 793/2018 (B.O. 04/09/2018) unificó la soja en 18 % y le sumó un
# derecho del 12 % que no podía exceder $4 por cada dólar del valor FOB. La
# alícuota legal es entonces 18 + min(12; 400/TC), con el TC en ARS/USD: no
# es una estimación, es la norma evaluada al tipo de cambio del mes. El
# archivo crudo usaba un TC aproximado; acá se usa el mayorista A 3500
# efectivamente observado. Termina en noviembre de 2019 porque el Dto.
# 37/2019 fijó 30 % desde el 14/12/2019.
ruta_tc_ret <- file.path(cfg$dir_crudos, "tc_mayorista_a3500_diario.csv")
if (file.exists(ruta_tc_ret)) {
  crudo_tc_ret <- leer_csv_crudo(ruta_tc_ret)
  tc_ret <- data.frame(
    fecha = trimws(as.character(crudo_tc_ret$fecha)),
    tc    = a_numero(crudo_tc_ret$tc_mayorista_a3500),
    stringsAsFactors = FALSE
  )
  tc_ret <- tc_ret[!is.na(tc_ret$tc) & tc_ret$tc > 0, ]
  tc_mes <- tapply(tc_ret$tc, substr(tc_ret$fecha, 1, 7), mean)

  bajo_tope <- which(retenciones$anio * 100 + retenciones$mes >= 201809 &
                       retenciones$anio * 100 + retenciones$mes <= 201911)
  etiqueta <- sprintf("%04d-%02d", retenciones$anio[bajo_tope],
                      retenciones$mes[bajo_tope])
  tc_del_mes <- unname(tc_mes[etiqueta])
  retenciones$alicuota[bajo_tope] <- 18 + pmin(12, 400 / tc_del_mes)
  retenciones$tipo_dato[bajo_tope] <- ifelse(is.na(tc_del_mes),
                                             "a_verificar", "nominal")
}

anual_ret <- aggregate(alicuota ~ anio, data = retenciones, FUN = mean,
                       na.rm = TRUE, na.action = stats::na.pass)
names(anual_ret)[2] <- "alicuota_export_soja_pct"
meses_dudosos <- aggregate(
  list(dudosos = retenciones$tipo_dato != "nominal"),
  by = list(anio = retenciones$anio), FUN = sum
)
anual_ret$dudosos <- meses_dudosos$dudosos[match(anual_ret$anio, meses_dudosos$anio)]

anios_dudosos <- anual_ret$anio[anual_ret$dudosos > 0]
res_125 <- paste0(
  "El único tramo que no sale de una alícuota fija es la móvil de la Res. ",
  "125/2008, vigente del 11/03/2008 al 21/07/2008: era una función del ",
  "precio FOB y llegó a 46,3 %. Marzo, abril, mayo y junio de 2008 se toman ",
  "al 35 %, la alícuota de la Res. 368/2007 que regía antes de marzo y que ",
  "el Dto. 1176/2008 (B.O. 21/07/2008) repuso tras el rechazo en el Senado ",
  "del 17/07. Todos los demás ",
  "meses de la serie se verificaron contra el Boletín Oficial y entran como ",
  "nominales.")

if (!isTRUE(cfg$aceptar_alicuotas_no_nominales)) {
  anual_ret$alicuota_export_soja_pct[anual_ret$dudosos > 0] <- NA_real_
  faltantes <- agregar_faltante(
    faltantes, "curado", "alicuota_export_soja_pct",
    paste0("Quedan NA los años con meses no nominales (",
           paste(anios_dudosos, collapse = ", "), "). ", res_125,
           " Para incluir esos años hay que poner ",
           "aceptar_alicuotas_no_nominales = TRUE en 00_config.R.")
  )
} else if (length(anios_dudosos) > 0) {
  faltantes <- agregar_faltante(
    faltantes, "curado", "alicuota_export_soja_pct",
    paste0("Entran al panel años con meses que no son la alícuota nominal ",
           "vigente (", paste(anios_dudosos, collapse = ", "),
           "), porque aceptar_alicuotas_no_nominales = TRUE. ", res_125)
  )
}

# La alícuota existe solo para Argentina. A Brasil y EE. UU. NO se les pone
# cero: cero sería afirmar que se midió y dio cero.
panel$alicuota_export_soja_pct <- ifelse(
  panel$pais == "Argentina",
  anual_ret$alicuota_export_soja_pct[match(panel$anio, anual_ret$anio)],
  NA_real_
)

# ---------------------------------------------------------------------------
# 8b. Brecha cambiaria (solo Argentina)
# ---------------------------------------------------------------------------
# Diferencial de mercado, aparte de las retenciones. La alícuota ya está en
# su propia columna; meterla acá repetiría el mismo cálculo.
# El denominador es siempre el mayorista A 3500. El numerador cambia porque
# el CCL público arranca en 2013:
#   2005-2012: blue de Ámbito
#   2013-2025: CCL, punta vendedora
#   brecha = 100 * (paralelo - mayorista) / mayorista
# El año es el promedio de los doce promedios mensuales.
panel$brecha_cambiaria_pct <- NA_real_
ruta_ccl  <- file.path(cfg$dir_crudos, "dolar_ccl_diario.csv")
ruta_blue <- file.path(cfg$dir_crudos, "dolar_blue_diario.csv")
ruta_tc   <- file.path(cfg$dir_crudos, "tc_mayorista_a3500_diario.csv")

if (!file.exists(ruta_ccl) || !file.exists(ruta_blue) || !file.exists(ruta_tc)) {
  faltantes <- agregar_faltante(
    faltantes, "curado", "brecha_cambiaria_pct",
    paste0("Faltan data/dolar_ccl_diario.csv, data/dolar_blue_diario.csv o ",
           "data/tc_mayorista_a3500_diario.csv.")
  )
} else {
  crudo_ccl  <- leer_csv_crudo(ruta_ccl)
  crudo_blue <- leer_csv_crudo(ruta_blue)
  crudo_tc   <- leer_csv_crudo(ruta_tc)
  paralelo <- rbind(
    data.frame(
      fecha    = as.Date(crudo_blue$fecha),
      paralelo = a_numero(crudo_blue$venta),
      origen   = "blue",
      stringsAsFactors = FALSE
    ),
    data.frame(
      fecha    = as.Date(crudo_ccl$fecha),
      paralelo = a_numero(crudo_ccl$venta),
      origen   = "ccl",
      stringsAsFactors = FALSE
    )
  )
  # El blue cubre hasta 2012. Desde el primer día con CCL manda el CCL.
  corte <- min(paralelo$fecha[paralelo$origen == "ccl"], na.rm = TRUE)
  paralelo <- paralelo[paralelo$origen == "ccl" | paralelo$fecha < corte, ]
  mayorista <- data.frame(
    fecha     = as.Date(crudo_tc$fecha),
    mayorista = a_numero(crudo_tc$tc_mayorista_a3500),
    stringsAsFactors = FALSE
  )
  diario <- merge(paralelo, mayorista, by = "fecha")
  diario <- diario[!is.na(diario$paralelo) & !is.na(diario$mayorista) &
                     diario$paralelo > 0 & diario$mayorista > 0, ]
  diario$anio <- as.integer(format(diario$fecha, "%Y"))
  diario$mes  <- as.integer(format(diario$fecha, "%m"))
  diario$brecha <- 100 * (diario$paralelo - diario$mayorista) / diario$mayorista

  con_dato <- diario[!is.na(diario$brecha), ]
  mensual <- aggregate(brecha ~ anio + mes, data = con_dato, FUN = mean)
  dias <- aggregate(brecha ~ anio + mes, data = con_dato, FUN = length)
  names(dias)[3] <- "dias"
  mensual <- merge(mensual, dias, by = c("anio", "mes"))
  mensual$brecha[mensual$dias < 10] <- NA

  anios_brecha <- sort(unique(mensual$anio))
  anual_brecha <- data.frame(
    anio = anios_brecha,
    brecha_cambiaria_pct = NA_real_,
    stringsAsFactors = FALSE
  )
  for (i in seq_along(anios_brecha)) {
    sub <- mensual[mensual$anio == anios_brecha[i] & !is.na(mensual$brecha), ]
    if (nrow(sub) == 12) {
      anual_brecha$brecha_cambiaria_pct[i] <- mean(sub$brecha)
    }
  }

  panel$brecha_cambiaria_pct <- ifelse(
    panel$pais == "Argentina",
    anual_brecha$brecha_cambiaria_pct[match(panel$anio, anual_brecha$anio)],
    NA_real_
  )

  anios_con_dato <- anual_brecha$anio[!is.na(anual_brecha$brecha_cambiaria_pct)]
  faltantes <- agregar_faltante(
    faltantes, "curado", "brecha_cambiaria_pct",
    paste0(
      "2005-2012 usan el dólar blue de Ámbito y 2013-2025 el CCL. Las dos ",
      "se comparan contra el mismo mayorista A 3500 y no descuentan ",
      "retenciones. Entran al panel: ",
      paste(anios_con_dato, collapse = ", "), "."
    )
  )
}

# ---------------------------------------------------------------------------
# 9. Crédito al agro y PBI agropecuario
# ---------------------------------------------------------------------------
# El crédito al agro no sale de FAO. Argentina es el saldo del BCRA al 31
# de diciembre y Estados Unidos el de la Fed al 31 de diciembre. Brasil no
# tiene una serie de esa definición y queda vacío: no se completa con FAO.
# El crédito total de la economía y el valor agregado agropecuario siguen
# en FAO. El denominador del cociente es el valor agregado de agricultura,
# silvicultura y pesca (dominio Macro Indicators, ítem 22016): FAO no publica
# el valor agregado de agricultura sola para Argentina, y para Brasil esa
# serie se corta en 2016. El crédito entra como descriptivo, no como regresor.
crudo_fao <- leer_csv_crudo(file.path(cfg$dir_crudos, "FAOSTAT_data_en_11-9-2026.csv"))
nombres_pais <- c("Argentina" = "Argentina", "Brazil" = "Brasil",
                  "United States of America" = "Estados Unidos")
crudo_fao$pais <- nombres_pais[crudo_fao$Area]
crudo_fao$anio <- a_numero(crudo_fao$Year)
crudo_fao$valor <- a_numero(crudo_fao$Value)

tomar_item <- function(item, columna) {
  sub <- crudo_fao[crudo_fao$Item == item & !is.na(crudo_fao$pais), ]
  out <- data.frame(pais = sub$pais, anio = sub$anio, stringsAsFactors = FALSE)
  out[[columna]] <- sub$valor
  out
}
panel$credito_agro_musd <- NA_real_
panel <- pegar(panel, tomar_item("Total Credit", "credito_total_musd"),
               "credito_total_musd")

# Argentina: saldo al 31 de diciembre de los préstamos en efectivo de todas
# las entidades a quien tiene como actividad principal la agricultura, la
# ganadería, la caza y la silvicultura (código SISCEN 10, todas las monedas).
# Viene en miles de pesos y se pasa a millones de dólares con el tipo de
# cambio de referencia de ese mismo día. No incluye la pesca. Es el crédito
# al agro del panel: no se empalma con FAO.
origen_bcra <- NULL
sin_bcra <- anios
dir_bcra <- file.path(cfg$dir_crudos, "bcra_prestamos")
archivos_bcra <- sort(list.files(
  dir_bcra, pattern = "^act[0-9]{4}\\.xls$", full.names = TRUE
))
saldo_bcra <- function(ruta) {
  d <- as.data.frame(
    suppressMessages(readxl::read_excel(ruta, sheet = "Saldos", col_names = FALSE)),
    stringsAsFactors = FALSE
  )
  hdr <- which(apply(d, 1, function(r) {
    any(as.character(r) == "act00t", na.rm = TRUE)
  }))
  if (length(hdr) != 1) {
    stop("El archivo ", basename(ruta), " no tiene un encabezado act00t.",
         call. = FALSE)
  }
  noms <- as.character(unlist(d[hdr, ]))
  tomar <- function(nombre) {
    i <- which(noms == nombre)
    if (length(i) != 1) {
      stop("Falta la columna ", nombre, " en ", basename(ruta), ".", call. = FALSE)
    }
    d[[i]]
  }
  fecha <- as.character(tomar("actfec"))
  moneda <- suppressWarnings(as.numeric(tomar("actmon")))
  tc <- suppressWarnings(as.numeric(tomar("tipref")))
  total <- suppressWarnings(as.numeric(tomar("act00t")))
  codigo <- as.character(tomar("siscen02"))
  nombre <- as.character(tomar("name"))
  fila <- which(codigo == "10" &
                grepl("Agriculture, hunting", nombre) &
                grepl("1231$", fecha) &
                moneda == 0)
  if (length(fila) != 1) {
    stop("No hay un único saldo de diciembre, todas las monedas, código 10 en ",
         basename(ruta), ". Aparecen ", length(fila), ".", call. = FALSE)
  }
  if (is.na(tc[fila]) || tc[fila] <= 0 || is.na(total[fila]) || total[fila] <= 0) {
    stop("El saldo o el tipo de cambio de ", basename(ruta), " no es usable.",
         call. = FALSE)
  }
  data.frame(
    anio = as.integer(substr(fecha[fila], 1, 4)),
    miles_pesos = total[fila],
    valor = total[fila] / tc[fila] / 1000,
    stringsAsFactors = FALSE
  )
}
if (length(archivos_bcra) > 0) {
  saldos_bcra <- do.call(rbind, lapply(archivos_bcra, saldo_bcra))
  saldos_bcra <- saldos_bcra[saldos_bcra$anio %in% anios, ]
  origen_bcra <- data.frame(
    pais = "Argentina", anio = saldos_bcra$anio,
    valor = saldos_bcra$miles_pesos, stringsAsFactors = FALSE
  )
  argentina <- panel$pais == "Argentina"
  panel$credito_agro_musd[argentina] <-
    saldos_bcra$valor[match(panel$anio[argentina], saldos_bcra$anio)]
  sin_bcra <- anios[!anios %in% saldos_bcra$anio[!is.na(saldos_bcra$valor)]]
}

# Estados Unidos: saldo de la Fed al 31 de diciembre. FRED fecha el cuarto
# trimestre el 1 de octubre. Ya viene en millones de dólares.
ruta_fed <- file.path(cfg$dir_crudos, "ALFAPGACB.csv")
sin_fed <- anios
if (file.exists(ruta_fed)) {
  crudo_fed <- leer_csv_crudo(ruta_fed)
  crudo_fed$anio <- as.integer(substr(crudo_fed$observation_date, 1, 4))
  crudo_fed$mes_dia <- substr(crudo_fed$observation_date, 6, 10)
  cierre_fed <- crudo_fed[crudo_fed$mes_dia == "10-01" & crudo_fed$anio %in% anios, ]
  cierre_fed$valor <- a_numero(cierre_fed$ALFAPGACB)
  eeuu <- panel$pais == "Estados Unidos"
  panel$credito_agro_musd[eeuu] <- cierre_fed$valor[match(panel$anio[eeuu], cierre_fed$anio)]
  sin_fed <- anios[!anios %in% cierre_fed$anio[!is.na(cierre_fed$valor)]]
}
faltantes <- agregar_faltante(
  faltantes, "curado", "credito_agro_musd",
  paste0(
    "No sale de FAO. Argentina es el saldo al 31 de diciembre del BCRA, ",
    "código SISCEN 10, todas las monedas. Estados Unidos es el saldo al 31 ",
    "de diciembre de la Fed (FRED, ALFAPGACB). Brasil no tiene una serie de ",
    "esta definición y queda vacío: no se completa con FAO. No entra como ",
    "regresor.",
    if (length(sin_bcra) > 0) {
      paste0(" Al BCRA le faltan, y no se rellenan: ",
             paste(sin_bcra, collapse = ", "), ".")
    } else {
      " Argentina cubre 2005-2025."
    },
    if (!file.exists(ruta_fed)) {
      " Falta data/ALFAPGACB.csv, así que Estados Unidos queda vacío."
    } else if (length(sin_fed) > 0) {
      paste0(" A la Fed le faltan, y no se rellenan: ",
             paste(sin_fed, collapse = ", "), ".")
    } else {
      " Estados Unidos cubre 2005-2025."
    }
  )
)

# Valor agregado de agricultura, silvicultura y pesca, en millones de USD
# corrientes. 2025 está marcado como estimado (flag E) en los tres países y
# se deja vacío: no se lo reemplaza por el Banco Mundial ni por el BEA.
crudo_pbi <- leer_csv_crudo(file.path(
  cfg$dir_crudos, "FAOSTAT_MK_valor_agregado_agro_11-9-2026.csv"
))
crudo_pbi <- crudo_pbi[crudo_pbi[["Item Code"]] == "22016" &
                       crudo_pbi[["Element Code"]] == "6110", ]
crudo_pbi$pais <- nombres_pais[crudo_pbi$Area]
crudo_pbi$anio <- a_numero(crudo_pbi$Year)
crudo_pbi$pbi_agropecuario_musd <- a_numero(crudo_pbi$Value)
estimados <- !is.na(crudo_pbi$pais) & crudo_pbi$Flag == "E"
anios_estimados <- sort(unique(crudo_pbi$anio[estimados]))
crudo_pbi$pbi_agropecuario_musd[estimados] <- NA_real_
pbi <- crudo_pbi[!is.na(crudo_pbi$pais),
                 c("pais", "anio", "pbi_agropecuario_musd")]
panel <- pegar(panel, pbi, "pbi_agropecuario_musd")

if (length(anios_estimados) > 0) {
  faltantes <- agregar_faltante(
    faltantes, "curado", "pbi_agropecuario_musd",
    paste0("Quedan NA los años que FAO marca como estimados (flag E): ",
           paste(anios_estimados, collapse = ", "),
           ", en los tres países. No se completan con otra fuente.")
  )
}

# ---------------------------------------------------------------------------
# 9b. Diésel minorista
# ---------------------------------------------------------------------------
# Reemplaza al flete, que se dejó afuera porque las rutas no eran comparables.
# El archivo trae dólares por litro, ya armados, con el año vacío donde falta
# un mes. No se rellena.
#
# Brasil 2020 no tiene septiembre: la ANP no relevó precios entre el 18 de
# agosto y el 17 de octubre. Ese año queda vacío y se cae de la ecuación
# brasileña. No se promedian los once meses.
panel$diesel_usd_l <- NA_real_
ruta_diesel <- file.path(cfg$dir_crudos, "diesel_usd_l.csv")

if (!file.exists(ruta_diesel)) {
  faltantes <- agregar_faltante(
    faltantes, "curado", "diesel_usd_l",
    "No hay archivo data/diesel_usd_l.csv."
  )
} else {
  crudo_diesel <- leer_csv_crudo(ruta_diesel)
  diesel <- data.frame(
    pais = trimws(crudo_diesel$pais),
    anio = a_numero(crudo_diesel$anio),
    diesel_usd_l = a_numero(crudo_diesel$diesel_usd_l),
    stringsAsFactors = FALSE
  )
  panel <- pegar(panel, diesel, "diesel_usd_l")

  cobertura <- vapply(cfg$paises, function(p) {
    sub <- panel[panel$pais == p & !is.na(panel$diesel_usd_l), ]
    if (nrow(sub) == 0) {
      paste0(p, ": ningún año")
    } else {
      paste0(p, ": ", min(sub$anio), "-", max(sub$anio), " (", nrow(sub), " años)")
    }
  }, character(1))

  faltantes <- agregar_faltante(
    faltantes, "curado", "diesel_usd_l",
    paste0("Reemplaza al flete. ", paste(cobertura, collapse = "; "),
           ". Es el diésel minorista, con impuestos, en dólares por litro. ",
           "Brasil 2020 queda vacío porque la ANP no relevó septiembre ",
           "(no hubo encuesta entre el 18 de agosto y el 17 de octubre) y ",
           "no se promedian los once meses. Ese año se cae de la ecuación ",
           "brasileña y no se rellena. Argentina y EE. UU. están completos, ",
           "y 2020 está fuera de la ventana en la que se responden las ",
           "hipótesis.")
  )
}

# ---------------------------------------------------------------------------
# 9c. Urea
# ---------------------------------------------------------------------------
# Precio internacional, el mismo en los tres países. El archivo trae los
# meses; el año es el promedio de los doce. Un año sin un mes queda vacío
# y no se rellena. 2025 tiene los doce meses.
#
# El Banco Mundial cambia la cotización en marzo de 2022: hasta febrero es
# f.o.b. Mar Negro y desde marzo es f.o.b. Medio Oriente. No se empalma
# con otra serie. El corte queda declarado.
panel$urea_usd_t <- NA_real_
panel$ln_urea <- NA_real_
ruta_urea <- file.path(cfg$dir_crudos, "urea_mensual.csv")

if (!file.exists(ruta_urea)) {
  faltantes <- agregar_faltante(
    faltantes, "curado", "urea_usd_t",
    "No hay archivo data/urea_mensual.csv."
  )
} else {
  crudo_urea <- leer_csv_crudo(ruta_urea)
  crudo_urea$anio <- a_numero(crudo_urea$anio)
  crudo_urea$mes <- a_numero(crudo_urea$mes)
  crudo_urea$urea_usd_t <- a_numero(crudo_urea$urea_usd_t)
  crudo_urea <- crudo_urea[crudo_urea$anio %in% anios, ]

  anual_urea <- do.call(rbind, lapply(anios, function(anio) {
    meses <- crudo_urea$urea_usd_t[crudo_urea$anio == anio]
    meses <- meses[!is.na(meses)]
    data.frame(
      anio = anio,
      urea_usd_t = if (length(unique(crudo_urea$mes[crudo_urea$anio == anio &
                                                    !is.na(crudo_urea$urea_usd_t)])) == 12) {
        mean(meses)
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  }))

  urea <- do.call(rbind, lapply(cfg$paises, function(p) {
    data.frame(pais = p, anual_urea, stringsAsFactors = FALSE)
  }))
  panel <- pegar(panel, urea, "urea_usd_t")
  panel$ln_urea <- ifelse(
    is.na(panel$urea_usd_t) | panel$urea_usd_t <= 0,
    NA_real_, log(panel$urea_usd_t)
  )

  sin_urea <- sort(unique(panel$anio[is.na(panel$urea_usd_t)]))
  faltantes <- agregar_faltante(
    faltantes, "curado", "urea_usd_t",
    paste0(
      "Precio internacional de la urea, el mismo en los tres países. ",
      "El año es el promedio de los doce meses. ",
      if (length(sin_urea) == 0) {
        "2005-2025 tiene los doce meses, incluido 2025."
      } else {
        paste0("Quedan vacíos, sin rellenar: ",
               paste(sin_urea, collapse = ", "), ".")
      },
      " Desde marzo de 2022 la cotización es f.o.b. Medio Oriente; hasta ",
      "febrero de 2022 era f.o.b. Mar Negro. El corte lo publica el Banco ",
      "Mundial y no se empalma con otra serie."
    )
  )
}

# ---------------------------------------------------------------------------
# 10. Soja, Treasury nominal, inflación y tasa local
# ---------------------------------------------------------------------------
# Los cuatro Excel nuevos son exportaciones de Bloomberg: seis filas de
# metadatos y después la fecha y el valor. data/ no se reescribe.
#
# La soja, la inflación de cierre y la tasa nominal local entran a las tres
# ecuaciones. El Treasury nominal queda afuera: no es la tasa real de los
# TIPS. En 2005 la inflación argentina es la variación de octubre de San
# Luis repetida los doce meses. 2011 y 2012 siguen vacíos y no se rellenan.

fecha_a_texto <- function(x) {
  if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%d", tz = "UTC"))
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  num <- suppressWarnings(as.numeric(x))
  format(as.Date(num, origin = "1899-12-30"), "%Y-%m-%d")
}

# Las primeras seis filas son ticker, fechas y periodicidad. Si queda una
# fila de encabezado (Date, PX_LAST), no es una fecha y se descarta sola.
leer_bloomberg <- function(ruta, hoja, col_fecha, col_valor) {
  crudo <- as.data.frame(readxl::read_excel(
    ruta, sheet = hoja, col_names = FALSE, .name_repair = "minimal"
  ))
  crudo <- crudo[-seq_len(6), , drop = FALSE]
  data.frame(
    fecha = fecha_a_texto(crudo[[col_fecha]]),
    valor = suppressWarnings(as.numeric(crudo[[col_valor]])),
    stringsAsFactors = FALSE
  )
}

# Inflación de cierre de año, diciembre contra diciembre.
# compuesta = TRUE cuando el archivo trae la variación mensual: se componen
# los doce meses. Si no, se toma el dato de diciembre, que ya es interanual.
inflacion_cierre <- function(fechas, valores, compuesta = FALSE) {
  anio <- suppressWarnings(as.integer(substr(fechas, 1, 4)))
  mes  <- suppressWarnings(as.integer(substr(fechas, 6, 7)))
  ok <- !is.na(anio) & !is.na(mes) & !is.na(valores)
  anio <- anio[ok]
  mes <- mes[ok]
  valores <- valores[ok]
  anios <- cfg$anio_min:cfg$anio_max
  valor <- vapply(anios, function(a) {
    sub_v <- valores[anio == a]
    sub_m <- mes[anio == a]
    if (compuesta) {
      if (length(sub_m) == 0 || length(unique(sub_m)) < 12 ||
          any(duplicated(sub_m))) {
        return(NA_real_)
      }
      (prod(1 + sub_v / 100) - 1) * 100
    } else {
      dic <- sub_v[sub_m == 12]
      if (length(dic) != 1) return(NA_real_)
      dic
    }
  }, numeric(1))
  data.frame(anio = anios, valor = valor, stringsAsFactors = FALSE)
}

pegar_por_pais <- function(panel, series, columna) {
  piezas <- lapply(names(series), function(p) {
    df <- data.frame(pais = p, anio = series[[p]]$anio, stringsAsFactors = FALSE)
    df[[columna]] <- series[[p]]$valor
    df
  })
  pegar(panel, do.call(rbind, piezas), columna)
}

huecos_de <- function(panel, columna) {
  unlist(lapply(cfg$paises, function(p) {
    sub <- panel[panel$pais == p, ]
    faltan <- sub$anio[is.na(sub[[columna]])]
    if (length(faltan) == 0) return(character(0))
    paste0(p, " ", paste(faltan, collapse = ", "))
  }))
}

hay_readxl <- requireNamespace("readxl", quietly = TRUE)
panel$precio_soja_cbot_usd_t <- NA_real_
panel$treasury_us_10a_nominal_pct <- NA_real_
panel$inflacion_dic_pct <- NA_real_
panel$tasa_nominal_local_pct <- NA_real_

ruta_soja <- file.path(cfg$dir_crudos, "TIF - Precio del commodity.xlsx")
ruta_rf   <- file.path(cfg$dir_crudos, "TIF - Risk Free.xlsx")
ruta_inf  <- file.path(cfg$dir_crudos, "TIF - Inflacion.xlsx")
ruta_tasa <- file.path(cfg$dir_crudos, "TIF - Tasa de interes local.xlsx")

if (!hay_readxl) {
  faltantes <- agregar_faltante(
    faltantes, "curado", "bloomberg_xlsx",
    paste0("Falta el paquete readxl: no se leyeron los Excel de la soja, ",
           "el Treasury, la inflación ni la tasa local.")
  )
}

if (hay_readxl && file.exists(ruta_soja)) {
  bloque <- leer_bloomberg(ruta_soja, "Worksheet", 1, 4)
  # PX_MID está en centavos de dólar por bushel. Un bushel de soja son
  # 60 libras y una libra son 0,45359237 kg.
  bushels_por_tonelada <- 1000 / (60 * 0.45359237)
  bloque$valor <- (bloque$valor / 100) * bushels_por_tonelada
  anual <- promedio_anual(bloque$fecha, bloque$valor,
                          meses_minimos = 12, observaciones_minimas = 10)
  panel$precio_soja_cbot_usd_t <- anual$valor[match(panel$anio, anual$anio)]
  sin_dato <- sort(unique(panel$anio[is.na(panel$precio_soja_cbot_usd_t)]))
  if (length(sin_dato) > 0) {
    faltantes <- agregar_faltante(
      faltantes, "curado", "precio_soja_cbot_usd_t",
      paste0("El archivo de la soja está, pero no cubre todos los años del ",
             "período. Faltan: ", paste(sin_dato, collapse = ", "),
             ". No se interpolan.")
    )
  }
} else if (hay_readxl) {
  faltantes <- agregar_faltante(
    faltantes, "curado", "precio_soja_cbot_usd_t",
    paste0("Precio de la soja en CBOT. No está data/TIF - Precio del ",
           "commodity.xlsx. Es un regresor económico de las tres ecuaciones.")
  )
}

if (hay_readxl && file.exists(ruta_rf)) {
  bloque <- leer_bloomberg(ruta_rf, "USA", 1, 2)
  anual <- promedio_anual(bloque$fecha, bloque$valor,
                          meses_minimos = 12, observaciones_minimas = 10)
  panel$treasury_us_10a_nominal_pct <- anual$valor[match(panel$anio, anual$anio)]
  sin_dato <- sort(unique(panel$anio[is.na(panel$treasury_us_10a_nominal_pct)]))
  if (length(sin_dato) > 0) {
    faltantes <- agregar_faltante(
      faltantes, "curado", "treasury_us_10a_nominal_pct",
      paste0("El Treasury nominal no cubre todos los años. Faltan: ",
             paste(sin_dato, collapse = ", "), ". No se interpolan.")
    )
  }
} else if (hay_readxl) {
  faltantes <- agregar_faltante(
    faltantes, "curado", "treasury_us_10a_nominal_pct",
    "No está data/TIF - Risk Free.xlsx."
  )
}

coma <- function(x) {
  gsub(".", ",", formatC(x, format = "f", digits = 2), fixed = TRUE)
}

# Argentina: índice de nivel general del IPC de San Luis, base 2003 = 100,
# en su publicación oficial como dato abierto. Es la misma serie que el Excel
# de Bloomberg (coinciden los 239 meses compartidos salvo redondeo, y
# noviembre de 2007, donde Bloomberg tiene el signo cambiado), pero desde la
# fuente primaria y sin el hueco de noviembre de 2011 a febrero de 2012.
# Al venir el índice en niveles, el año es diciembre contra diciembre
# directo, sin componer doce variaciones mensuales.
ruta_sl <- file.path(cfg$dir_crudos, "ipc_san_luis_nivel_general_2003_100.csv")
anios_inf <- cfg$anio_min:cfg$anio_max
ar_anual <- data.frame(anio = anios_inf, valor = NA_real_)
texto_2005 <- "No está el índice de San Luis, así que Argentina queda vacía."

if (file.exists(ruta_sl)) {
  crudo_sl <- leer_csv_crudo(ruta_sl)
  sl <- data.frame(
    ym    = substr(trimws(as.character(crudo_sl$indice_tiempo)), 1, 7),
    nivel = a_numero(crudo_sl$nivel_general),
    stringsAsFactors = FALSE
  )
  sl <- sl[!is.na(sl$nivel), ]
  nivel_de <- function(ym) {
    v <- sl$nivel[sl$ym == ym]
    if (length(v) == 1) v else NA_real_
  }
  ar_anual$valor <- vapply(anios_inf, function(a) {
    previo <- nivel_de(sprintf("%d-12", a - 1))
    cierre <- nivel_de(sprintf("%d-12", a))
    if (is.na(previo) || is.na(cierre)) NA_real_ else (cierre / previo - 1) * 100
  }, numeric(1))

  # 2005: el índice arranca en octubre, así que no existe el diciembre de
  # 2004 contra el cual medir. Se anualiza la variación efectivamente
  # observada entre octubre y diciembre: dos meses medidos, llevados a la
  # tasa equivalente de doce. No se completa ningún mes.
  oct_2005 <- nivel_de("2005-10")
  dic_2005 <- nivel_de("2005-12")
  if (!is.na(oct_2005) && !is.na(dic_2005)) {
    anual_2005 <- ((dic_2005 / oct_2005)^(12 / 2) - 1) * 100
    ar_anual$valor[ar_anual$anio == 2005] <- anual_2005
    texto_2005 <- paste0(
      "El índice de San Luis arranca en octubre de 2005, así que ese año no ",
      "tiene diciembre contra diciembre. Se anualiza la variación observada ",
      "entre octubre y diciembre (",
      coma(100 * (dic_2005 / oct_2005 - 1)),
      " % en dos meses): ", coma(anual_2005),
      " %. Los meses previos no se completan."
    )
  }
}

if (hay_readxl && file.exists(ruta_inf)) {
  # Brasil y EE. UU. siguen saliendo del Excel: son interanuales de
  # diciembre, no variaciones mensuales.
  br <- leer_bloomberg(ruta_inf, "Bra", 1, 2)
  us <- leer_bloomberg(ruta_inf, "USA", 1, 2)
  panel <- pegar_por_pais(panel, list(
    "Argentina"       = ar_anual,
    "Brasil"          = inflacion_cierre(br$fecha, br$valor, compuesta = FALSE),
    "Estados Unidos"  = inflacion_cierre(us$fecha, us$valor, compuesta = FALSE)
  ), "inflacion_dic_pct")
  huecos <- huecos_de(panel, "inflacion_dic_pct")
  detalle <- if (length(huecos) == 0) {
    "Los tres países tienen los veintiún años."
  } else {
    paste0("Quedan vacíos: ", paste(huecos, collapse = "; "),
           ". Esos años no se rellenan.")
  }
  faltantes <- agregar_faltante(
    faltantes, "curado", "inflacion_dic_pct",
    paste0("Entra como regresor en las tres ecuaciones. Argentina es el ",
           "índice de nivel general del IPC de San Luis, diciembre contra ",
           "diciembre, en su publicación oficial como dato abierto. No se ",
           "empalma con el IPC nacional (desde 2017) ni con el de CABA ",
           "(desde 2012): no son la misma canasta. ", texto_2005, " ",
           "Brasil es el IPCA interanual de diciembre y EE. UU. el CPI ",
           "interanual de diciembre. ", detalle)
  )
} else if (hay_readxl) {
  faltantes <- agregar_faltante(
    faltantes, "curado", "inflacion_dic_pct",
    "No está data/TIF - Inflacion.xlsx: faltan Brasil y EE. UU."
  )
}

if (hay_readxl && file.exists(ruta_tasa)) {
  # En la hoja de EE. UU. la tasa local es la meta de fed funds (columnas
  # A y B). Las columnas D a F repiten el Treasury nominal y no se usan.
  ar <- leer_bloomberg(ruta_tasa, "Arg", 1, 2)
  br <- leer_bloomberg(ruta_tasa, "Bra", 1, 2)
  us <- leer_bloomberg(ruta_tasa, "USA", 1, 2)
  panel <- pegar_por_pais(panel, list(
    "Argentina"      = promedio_anual(ar$fecha, ar$valor, 12, 10),
    "Brasil"         = promedio_anual(br$fecha, br$valor, 12, 10),
    "Estados Unidos" = promedio_anual(us$fecha, us$valor, 12, 10)
  ), "tasa_nominal_local_pct")
  huecos <- huecos_de(panel, "tasa_nominal_local_pct")
  detalle <- if (length(huecos) == 0) {
    "Cubre 2005-2025 en los tres países."
  } else {
    paste0("Quedan vacíos: ", paste(huecos, collapse = "; "),
           ". No se interpolan.")
  }
  faltantes <- agregar_faltante(
    faltantes, "curado", "tasa_nominal_local_pct",
    paste0("No entra como regresor: se combina con la inflación en la tasa ",
           "real local y queda en el panel como descriptiva. No es el mismo ",
           "instrumento en los tres países: Argentina es la BADLAR, Brasil ",
           "es la meta de la Selic y EE. UU. es la meta de fed funds. ",
           detalle)
  )
} else if (hay_readxl) {
  faltantes <- agregar_faltante(
    faltantes, "curado", "tasa_nominal_local_pct",
    "No está data/TIF - Tasa de interes local.xlsx."
  )
}

# --- Tasa real local, por Fisher ------------------------------------------
# La tasa nominal y la inflación de cierre son el mismo concepto medido dos
# veces: por Fisher, (1 + i) = (1 + r)(1 + pi). Puestas juntas en la misma
# ecuación con n = 21 se tapaban entre sí (VIF de 63,8 y 36,7 en Argentina)
# y se llevaban un grado de libertad sin aportar variación propia. Se las
# combina en la tasa real, que es la que entra al modelo: es el costo real
# del dinero local, lo que descuenta la renta de la tierra. Las dos series
# quedan en el panel como descriptivas.
# Se usa Fisher exacto y no la resta i - pi, que solo aproxima bien con
# inflación baja y acá la argentina llega a tres dígitos.
panel$tasa_real_local_pct <- ((1 + panel$tasa_nominal_local_pct / 100) /
                              (1 + panel$inflacion_dic_pct / 100) - 1) * 100
huecos <- huecos_de(panel, "tasa_real_local_pct")
faltantes <- agregar_faltante(
  faltantes, "curado", "tasa_real_local_pct",
  paste0("Entra como regresor en las tres ecuaciones, al lado de los TIPS: ",
         "los TIPS son la tasa real internacional y esta es la local. ",
         "Reemplaza a la inflación y a la tasa nominal, que medían el mismo ",
         "concepto y quedan descriptivas. ",
         if (length(huecos) == 0) "Cubre 2005-2025 en los tres países." else
           paste0("Quedan vacíos: ", paste(huecos, collapse = "; "), "."))
)

# ---------------------------------------------------------------------------
# 10b. TIPS: sigue sin el archivo que pide el diseño
# ---------------------------------------------------------------------------
# Si aparece data/tips_10a.csv, entra solo. La fecha va en la primera
# columna y la tasa en la segunda. Un año al que le falte algún mes no
# entra: queda como hueco.
cargar_serie_fechada <- function(archivo) {
  ruta <- file.path(cfg$dir_crudos, archivo)
  if (!file.exists(ruta)) return(NULL)
  crudo <- leer_csv_crudo(ruta)
  if (ncol(crudo) < 2) {
    stop("El archivo ", archivo, " tiene una sola columna; se esperan dos: ",
         "la fecha y el valor.")
  }
  fechas <- trimws(as.character(crudo[[1]]))
  valores <- a_numero(crudo[[2]])

  # El umbral de observaciones por mes solo tiene sentido en una serie
  # diaria: a una mensual, que trae un dato por mes, exigirle diez la
  # borraría entera. Se mira cuántas filas hay por mes para distinguirlas.
  por_mes <- table(substr(fechas[!is.na(valores)], 1, 7))
  es_diaria <- length(por_mes) > 0 && stats::median(as.numeric(por_mes)) >= 5
  minimo <- if (es_diaria) 10 else 1

  promedio_anual(fechas, valores, observaciones_minimas = minimo)
}

# Cada serie declara su archivo y qué decir si no está. La frecuencia la
# deduce el lector: un mes de una serie diaria necesita diez días hábiles,
# el mismo criterio que usa la brecha cambiaria.
economicas_pendientes <- list(
  tips_10a_pct = list(
    archivo = "tips_10a.csv",
    aviso = paste0(
      "Tasa real libre de riesgo, TIPS a 10 años (FRED DFII10). No hay ",
      "archivo de TIPS en data/. TIF - Risk Free.xlsx es el rendimiento ",
      "nominal del Treasury a 10 años (USGG10YR): quedó en ",
      "treasury_us_10a_nominal_pct y no reemplaza a esta serie, porque no ",
      "es la misma definición. Sigue siendo un regresor de las tres ",
      "ecuaciones. Si más adelante se baja el TIPS, alcanza con dejar ",
      "data/tips_10a.csv con la fecha en la primera columna y la tasa en ",
      "la segunda.")
  )
)

for (columna in names(economicas_pendientes)) {
  spec <- economicas_pendientes[[columna]]
  anual <- cargar_serie_fechada(spec$archivo)
  panel[[columna]] <- NA_real_

  if (is.null(anual)) {
    faltantes <- agregar_faltante(faltantes, "curado", columna, spec$aviso)
    next
  }

  # Las tres regiones comparten el mismo precio internacional y la misma
  # tasa real: son variables globales, no una por país.
  panel[[columna]] <- anual$valor[match(panel$anio, anual$anio)]

  sin_dato <- sort(unique(panel$anio[is.na(panel[[columna]])]))
  if (length(sin_dato) > 0) {
    faltantes <- agregar_faltante(
      faltantes, "curado", columna,
      paste0("El archivo ", spec$archivo, " está, pero no cubre todos los ",
             "años del período. Faltan: ", paste(sin_dato, collapse = ", "),
             ". No se interpolan.")
    )
  }
}

pendientes <- list(
  pse_ocde_pct = paste0(
    "%EAP (PSE) de la OCDE. No hay archivo en data/. Es la alternativa a la ",
    "alícuota como variable fiscal; con variable_fiscal = \"pse\" el análisis ",
    "se detiene.")
)
for (columna in names(pendientes)) {
  panel[[columna]] <- NA_real_
  faltantes <- agregar_faltante(faltantes, "curado", columna, pendientes[[columna]])
}

# Transformaciones logarítmicas que dependen de esas series.
panel$ln_precio_soja_cbot <- ifelse(
  is.na(panel$precio_soja_cbot_usd_t) | panel$precio_soja_cbot_usd_t <= 0,
  NA_real_, log(panel$precio_soja_cbot_usd_t)
)
# Mismas unidades en numerador y denominador (millones de USD corrientes).
# Queda NA donde falta el crédito, donde el PBI es estimado, o donde alguno
# de los dos no es positivo. No se rellena con la superficie sojera.
panel$ln_credito_sobre_pbi_agro <- ifelse(
  is.na(panel$credito_agro_musd) | is.na(panel$pbi_agropecuario_musd) |
    panel$credito_agro_musd <= 0 | panel$pbi_agropecuario_musd <= 0,
  NA_real_,
  log(panel$credito_agro_musd / panel$pbi_agropecuario_musd)
)
huecos_ratio <- unlist(lapply(cfg$paises, function(pais) {
  sub <- panel[panel$pais == pais, ]
  faltan <- sub$anio[is.na(sub$ln_credito_sobre_pbi_agro)]
  if (length(faltan) == 0) return(character(0))
  paste0(pais, " ", paste(faltan, collapse = ", "))
}))
if (length(huecos_ratio) > 0) {
  faltantes <- agregar_faltante(
    faltantes, "curado", "ln_credito_sobre_pbi_agro",
    paste0("El cociente se arma solo donde hay crédito al agro y PBI no ",
           "estimado. Queda vacío en: ",
           paste(huecos_ratio, collapse = "; "),
           ". Argentina y Estados Unidos tienen el crédito de los 21 años; donde ",
           "el cociente queda vacío, el límite lo pone el PBI estimado de ",
           "2025. Brasil no tiene crédito al agro, así que el cociente queda ",
           "vacío. Por eso no entra como regresor. No se completa con FAO.")
  )
}
panel$ln_maquinaria_por_ha <- NA_real_

# ---------------------------------------------------------------------------
# 11. Escritura del panel
# ---------------------------------------------------------------------------
orden_columnas <- c(
  "pais", "anio",
  "precio_ha_usd_corriente", "cpi_eeuu", "precio_ha_usd_2025", "ln_precio_ha_usd_2025",
  "rendimiento_soja_t_ha", "superficie_sembrada_soja_ha",
  "maquinaria_vendida_unidades", "ln_ventas_maquinaria_por_ha",
  "ln_maquinaria_por_ha", "diesel_usd_l", "urea_usd_t", "ln_urea",
  "precipitacion_mm", "precipitacion_desvio_mm",
  "precio_soja_cbot_usd_t", "ln_precio_soja_cbot",
  "tcr_bis_2020_100", "tcr_bis_2005_2025_100", "tips_10a_pct",
  "treasury_us_10a_nominal_pct", "inflacion_dic_pct", "tasa_nominal_local_pct",
  "tasa_real_local_pct",
  "alicuota_export_soja_pct", "pse_ocde_pct", "embi_pb", "brecha_cambiaria_pct",
  "credito_agro_musd",
  "credito_total_musd", "pbi_agropecuario_musd",
  "ln_credito_sobre_pbi_agro"
)
panel <- panel[, orden_columnas]

stopifnot(nrow(panel) == length(cfg$paises) * length(anios))
utils::write.csv(panel, file.path(cfg$dir_curados, "panel.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")

# ---------------------------------------------------------------------------
# 12. Diccionario de variables
# ---------------------------------------------------------------------------
# Una fila por variable, incluidas las que no se pudieron construir.
# La columna de ticker se completa solo en las series que salen de Bloomberg.
d <- function(variable, categoria, definicion, unidad, fuente, fecha,
              transformacion, signo, estado = "en el panel",
              ticker = "no aplica") {
  data.frame(
    variable = variable, categoria = categoria, definicion = definicion,
    unidad = unidad, fuente = fuente, ticker_modulo_bloomberg = ticker,
    fecha_descarga = fecha, transformacion = transformacion,
    signo_esperado = signo, estado = estado, stringsAsFactors = FALSE
  )
}

diccionario <- rbind(
  d("pais", "identificador", "País y región de referencia: Argentina es la zona núcleo pampeana, Brasil es el estado de Mato Grosso y Estados Unidos es Iowa.", "texto", "Definición del trabajo", "no consta", "ninguna", "no aplica"),
  d("anio", "identificador", "Año calendario. En rendimiento y superficie, año de cosecha (la campaña 2004/05 es 2005).", "año", "Definición del trabajo", "no consta", "ninguna", "no aplica"),

  d("precio_ha_usd_corriente", "dependiente", "Precio de la hectárea agrícola en dólares corrientes. Argentina: zona núcleo. Brasil: Mato Grosso. EE. UU.: Iowa.", "USD/ha", "Argentina: Minola Tierras Argentinas, inmobiliaria rural, serie propia de la zona núcleo sojera que la firma monitorea desde 1995; la planilla no indica la fuente y la atribución la informa el autor del trabajo. Brasil: Colussi et al. (2026), texto y figura digitalizada. EE. UU.: ISU Land Value Survey, citado por Chandio (2025). Las tres son valuaciones de mercado, no precios de escritura.", "no consta", "Se quita el prefijo «USD» y el separador de miles; se recorta a 2005-2025.", "no aplica"),
  d("cpi_eeuu", "dependiente", "Índice de precios al consumidor de EE. UU. (CPI-U, todos los consumidores urbanos, sin desestacionalizar). Es el deflactor de las tres series de precio, porque los tres precios ya están en dólares.", "índice 1982-1984 = 100", "BLS, serie CUUR0000SA0, descargada de FRED como CPIAUCNS.", "09/11/2026", "Promedio simple de los meses del año, calculado en 01_curado.R. 2025 usa once meses: el BLS no publicó octubre.", "no aplica", "en el panel"),
  d("precio_ha_usd_2025", "dependiente", "Precio de la hectárea en dólares constantes de 2025.", "USD/ha de 2025", "Cálculo propio sobre el precio corriente y el CPI-U.", "no consta", "precio_corriente * CPI(2025) / CPI(t).", "no aplica", "VACÍA: depende del CPI-U"),
  d("ln_precio_ha_usd_2025", "dependiente", "Logaritmo natural del precio por hectárea en dólares de 2025. Es la variable dependiente de las tres ecuaciones.", "ln(USD/ha)", "Cálculo propio.", "no consta", "Logaritmo natural.", "no aplica", "VACÍA: depende del CPI-U"),

  d("rendimiento_soja_t_ha", "productivo", "Rendimiento de soja, producción sobre superficie cosechada. Argentina: provincia de Buenos Aires. Brasil: Mato Grosso. EE. UU.: Iowa.", "t/ha", "Argentina: Secretaría de Agricultura, Estimaciones Agrícolas. Brasil: IBGE, PAM tabla 1612. EE. UU.: USDA NASS Quick Stats.", "08/11/2026", "Ninguna: se toma el valor en t/ha de cada fuente.", "+"),
  d("superficie_sembrada_soja_ha", "productivo", "Superficie sembrada de soja en la región de referencia.", "ha", "Las mismas tres fuentes del rendimiento.", "08/11/2026", "EE. UU.: acres convertidos a hectáreas en el archivo de origen (2,47105 acres por hectárea).", "no aplica"),
  d("maquinaria_vendida_unidades", "productivo", "Ventas anuales de tractores más cosechadoras. Es un flujo de ventas, no el stock por hectárea que define el trabajo.", "unidades", "Argentina: INDEC. Brasil: Anfavea. EE. UU.: AEM.", "no consta", "Suma de tractores y cosechadoras.", "no aplica", "insumo de ln_ventas_maquinaria_por_ha; EE. UU. no tiene 2005 ni 2006"),
  d("ln_ventas_maquinaria_por_ha", "productivo", "Logaritmo de las ventas de maquinaria por hectárea sembrada. Es un flujo de ventas, no el stock que define el trabajo.", "ln(unidades/ha)", "Cálculo propio sobre las ventas y la superficie sembrada.", "09/11/2026", "Logaritmo de ventas dividido superficie sembrada. Queda vacío donde falta cualquiera de las dos.", "+", "regresor; EE. UU. 2005 y 2006 se caen de su ecuación"),
  d("ln_maquinaria_por_ha", "productivo", "Logaritmo del stock de maquinaria por hectárea sembrada.", "ln(unidades/ha)", "Pendiente: la serie disponible es de ventas, no de stock.", "no consta", "No se construye.", "+", "FALTA: convertir ventas en stock exigiría un supuesto de vida útil no verificado"),
  d("diesel_usd_l", "productivo", "Precio del diésel minorista, con impuestos, en la región de referencia. Reemplaza al flete. Argentina: gasoil grado 2 al público en Buenos Aires, Córdoba y Santa Fe. Brasil: óleo diesel común en Mato Grosso (desde 2013 es el diésel B S500, no el S10). EE. UU.: diésel N.º 2 del Medio Oeste, que incluye Iowa.", "USD/l", "Argentina: Secretaría de Energía, precios en estaciones, resolución 1104/04, pasados a dólares con el A3500. Brasil: ANP, Levantamento de Preços, pasados a dólares con el PTAX venta. EE. UU.: EIA, serie EMD_EPD2D_PTE_R20_DPG.", "09/11/2026", "Argentina y Brasil: promedio de los doce precios mensuales en dólares. Un año sin un mes queda vacío. EE. UU.: el promedio anual publicado, en dólares por galón, dividido por 3,785411784.", "-", "regresor; Brasil 2020 se cae de su ecuación"),
  d("urea_usd_t", "productivo", "Precio internacional de la urea, prill spot. Es el mismo valor en los tres países. Hasta febrero de 2022 es f.o.b. Mar Negro; desde marzo de 2022 es f.o.b. Medio Oriente. Es un costo de producir, no el precio de la soja.", "USD/t", "Banco Mundial, Pink Sheet (Commodity Markets Outlook), hoja Monthly Prices, columna Urea, actualizado el 3 de noviembre de 2026. Fuente primaria: Bloomberg Finance L.P. / Green Markets.", "10/11/2026", "Promedio de los doce meses. Un año sin los doce meses queda vacío. No se empalma el cambio de cotización de marzo de 2022.", "-", "en el panel; 2025 tiene los doce meses"),
  d("ln_urea", "productivo", "Logaritmo del precio internacional de la urea. Es común a los tres países, así que su aporte al componente de niveles de Oaxaca es cero por construcción.", "ln(USD/t)", "Cálculo propio sobre urea_usd_t.", "10/11/2026", "Logaritmo natural.", "-", "regresor de las tres ecuaciones"),
  d("precipitacion_mm", "productivo", "Precipitación anual. Argentina: estación Pergamino. Brasil: estación Diamantino, salvo enero y febrero de 2025, que son de Cuiabá. EE. UU.: estado de Iowa.", "mm", "Argentina: clima.pergamino.gob.ar. Brasil: INMET, estación 83309 (Diamantino); enero y febrero de 2025, estación de Cuiabá: 248,9 mm (INMET) y 189,24 mm (Prefeitura de Cuiabá). EE. UU.: NOAA NCEI, Climate at a Glance.", "09/11/2026", "Suma de los doce meses. EE. UU.: pulgadas por 25,4. Un año sin los doce meses queda vacío.", "no aplica", "en el panel"),
  d("precipitacion_desvio_mm", "productivo", "Desvío de la precipitación anual respecto del promedio del período en la misma región.", "mm", "Cálculo propio.", "09/11/2026", "Valor del año menos el promedio 2005-2025 de esa región, con la misma definición en los tres países.", "no aplica", "en el panel"),

  d("precio_soja_cbot_usd_t", "economico", "Precio de la soja en el Chicago Board of Trade, primer futuro continuo. Es el mismo valor en los tres países.", "USD/t", "Bloomberg. El archivo trae centavos de dólar por bushel; la conversión a dólares por tonelada se hace en el curado.", "09/11/2026", "USD/t = (centavos por bushel / 100) × (1000 / (60 × 0,45359237)). Promedio de los promedios mensuales. Un año sin los doce meses queda vacío.", "+", "en el panel", ticker = "S 1 COMB Comdty"),
  d("ln_precio_soja_cbot", "economico", "Logaritmo del precio de la soja en CBOT. Es común a los tres países, así que su aporte al componente de niveles de Oaxaca es cero por construcción.", "ln(USD/t)", "Cálculo propio.", "09/11/2026", "Logaritmo natural.", "+", "en el panel"),
  d("tcr_bis_2020_100", "economico", "Tipo de cambio real efectivo del BIS, canasta amplia, promedio anual de doce meses. Un aumento es apreciación real de la moneda local.", "índice 2020 = 100", "FRED, series RBARBIS, RBBRBIS y RBUSBIS (BIS, tipo de cambio real efectivo, canasta amplia, mensual). El promedio anual de las tres reproduce las columnas del archivo.", "09/11/2026", "Promedio anual tomado del archivo.", "no aplica"),
  d("tcr_bis_2005_2025_100", "economico", "El mismo índice del BIS en base común: promedio 2005-2025 = 100. Misma definición en los tres países.", "índice 2005-2025 = 100", "Cálculo propio sobre la serie del BIS republicada por FRED.", "09/11/2026", "Valor del año sobre el promedio 2005-2025, por cien. Se controla contra la columna equivalente del archivo.", "+ en Argentina y Brasil, - en EE. UU."),
  d("tips_10a_pct", "economico", "Tasa real libre de riesgo: rendimiento de los TIPS a 10 años. Es el mismo valor en los tres países.", "% anual", "FRED, serie DFII10 (rendimiento de mercado de los títulos del Tesoro de EE. UU. a 10 años de vencimiento constante, indexados a la inflación), archivo data/tips_10a.csv.", "09/11/2026", "Promedio de los promedios mensuales de la serie diaria. Un año sin los doce meses, o sin diez días en un mes, queda vacío.", "-", "regresor de las tres ecuaciones"),
  d("treasury_us_10a_nominal_pct", "economico", "Rendimiento nominal del bono del Tesoro de EE. UU. a 10 años. Es el mismo valor en los tres países. No es la tasa real de los TIPS.", "% anual", "Bloomberg, archivo TIF - Risk Free.xlsx.", "09/11/2026", "Promedio de los promedios mensuales. Un año sin los doce meses, o sin diez días en un mes, queda vacío.", "no aplica", "DESCRIPTIVA: no reemplaza a los TIPS y no entra como regresor", ticker = "USGG10YR Index"),
  d("inflacion_dic_pct", "economico", "Inflación de cierre de año, diciembre contra diciembre. Argentina: IPC de San Luis. Brasil: IPCA. EE. UU.: CPI. Con la tasa nominal ya en la ecuación, mide el resguardo de valor de la tierra.", "% anual", "Argentina: Dirección Provincial de Estadística y Censos de San Luis, IPC-SL base 2003 = 100, nivel general, republicado como dato abierto por la Secretaría de Política Económica. Brasil y EE. UU.: Bloomberg, archivo TIF - Inflacion.xlsx.", "09/11/2026", "Argentina: cociente entre el índice de diciembre y el de diciembre del año anterior. En 2005 el índice arranca en octubre, así que se anualiza la variación observada entre octubre y diciembre. Brasil y EE. UU.: el dato interanual de diciembre.", "+", "regresor; 2005 es la variación de octubre a diciembre anualizada", ticker = "BZPIIPCY / CPI YOY"),
  d("tasa_nominal_local_pct", "economico", "Tasa nominal local. Argentina: BADLAR. Brasil: meta de la Selic. EE. UU.: meta de fed funds. No es el mismo instrumento en los tres países.", "% anual", "Bloomberg, archivo TIF - Tasa de interes local.xlsx.", "09/11/2026", "Promedio de los promedios mensuales de la serie diaria. Un año sin los doce meses, o sin diez días en un mes, queda vacío.", "-", "DESCRIPTIVA: es uno de los dos insumos de la tasa real local, que es la que entra a las ecuaciones", ticker = "BADLARPP / BZSTSETA / FDTR"),
  d("tasa_real_local_pct", "economico", "Tasa de interés real local, por la relación de Fisher. Es el costo real del dinero en cada país y entra al lado de los TIPS, que son la tasa real internacional. Reemplaza a la inflación y a la tasa nominal, que medían el mismo concepto dos veces.", "% anual", "Cálculo propio sobre tasa_nominal_local_pct e inflacion_dic_pct.", "09/11/2026", "Fisher exacto: ((1 + tasa nominal) / (1 + inflación) − 1) × 100. No se usa la resta aproximada porque la inflación argentina llega a tres dígitos.", "-", "regresor de las tres ecuaciones"),

  d("alicuota_export_soja_pct", "institucional", "Alícuota de derechos de exportación de la soja (poroto). Existe solo para Argentina; en Brasil y EE. UU. queda vacía y no se le asigna cero.", "%", "Planilla mensual de retenciones con la norma de cada cambio, más seis meses verificados contra el Boletín Oficial (Res. ME 10/2007, Dto. 1176/2008 y Res. ME 180/2008, Dto. 133/2015, Dto. 38/2025, Dto. 526/2025 y Dto. 877/2025).", "09/11/2026", "A cada mes se le asigna la alícuota vigente al cierre; el año es el promedio simple de los doce meses. Set-2018 a nov-2019 se calculan como 18 + mín(12; 400/TC) según el Dto. 793/2018, con el mayorista A 3500 observado. Marzo a junio de 2008, bajo la alícuota móvil de la Res. 125/2008, se toman al 35 %: la alícuota de la Res. 368/2007 que regía antes y que el Dto. 1176/2008 repuso al dejar sin efecto la móvil.", "-"),
  d("pse_ocde_pct", "institucional", "%EAP (PSE) de la OCDE para la soja o el total del sector.", "%", "Pendiente: no hay archivo en data/.", "no consta", "Ninguna.", "+", "FALTA: sin archivo"),
  d("embi_pb", "institucional", "Premio de riesgo soberano. De 2008 a 2025 es el EMBI Global Diversified. 2005-2007 es EMBI+, aproximación del nivel porque la planilla pública del Global Diversified arranca el 2007-10-29.", "puntos básicos", "2008-2025: BCRD, serie histórica del spread del EMBI (JP Morgan EMBI Global Diversified). 2005-2007: Ámbito (Argentina) e Ipeadata (Brasil), EMBI+.", "09/11/2026", "Promedio de los días del año. El BCRD publica puntos porcentuales y se multiplican por 100. EE. UU. queda vacío.", "-", "entra con usar_embi; 2005-2007 es aproximación EMBI+"),
  d("brecha_cambiaria_pct", "institucional", "Brecha entre el dólar paralelo y el mayorista. Solo Argentina; Brasil y EE. UU. quedan vacíos. No descuenta retenciones: eso ya está en la alícuota.", "%", "2005-2012: dólar blue de Ámbito. 2013-2025: CCL de ArgentinaDatos, punta vendedora. Mayorista: BCRA, variable 5, Comunicación A 3500.", "09/11/2026", "100 × (paralelo − mayorista) / mayorista. Promedio de los doce promedios mensuales. El paralelo es el blue hasta 2012 y el CCL desde 2013.", "-", "regresor de Argentina; cambia de cotización en 2013"),
  d("credito_agro_musd", "institucional", "Crédito al agro en dólares corrientes. Argentina: saldo al 31 de diciembre de los préstamos en efectivo a agricultura, ganadería, caza y silvicultura. Estados Unidos: saldo al 31 de diciembre de los préstamos de los bancos comerciales para financiar la producción. Brasil queda vacío.", "millones de USD", "Argentina: BCRA, préstamos por actividades. EE. UU.: FRED, serie ALFAPGACB. Brasil: no hay serie.", "09/11/2026", "Argentina: miles de pesos del código SISCEN 10, todas las monedas, divididos por el tipo de cambio de referencia del 31 de diciembre y por 1.000. No incluye la pesca. EE. UU.: cuarto trimestre, que FRED fecha el 1 de octubre. No sale de FAO.", "no aplica", "DESCRIPTIVA: Argentina y EE. UU. 2005-2025. Brasil vacío. No entra como regresor"),
  d("credito_total_musd", "institucional", "Crédito total de la economía en dólares corrientes. No es el crédito al agro.", "millones de USD", "FAOSTAT, dominio Credit to Agriculture, ítem Total Credit.", "09/11/2026", "Ninguna.", "no aplica", "DESCRIPTIVA: ningún país tiene 2025"),
  d("pbi_agropecuario_musd", "institucional", "Valor agregado de agricultura, silvicultura y pesca, en dólares corrientes. Es el denominador del crédito. Incluye silvicultura y pesca porque la serie de agricultura sola no existe para Argentina y en Brasil se corta en 2016.", "millones de USD", "FAOSTAT, dominio Macro Indicators, ítem Value Added (Agriculture, Forestry and Fishing), elemento Value US$.", "09/11/2026", "Se descarta 2025 porque FAO lo marca como estimado (flag E).", "no aplica", "en el panel; 2025 queda NA"),
  d("ln_credito_sobre_pbi_agro", "institucional", "Logaritmo del crédito al agro sobre el valor agregado de agricultura, silvicultura y pesca. Los dos términos están en millones de USD corrientes. En Argentina el numerador es el saldo del BCRA y en Estados Unidos el de la Fed. Brasil queda vacío.", "ln(ratio)", "Cálculo propio. El denominador es el valor agregado de FAOSTAT.", "09/11/2026", "Logaritmo del cociente. Queda NA donde falta el crédito o el PBI.", "+", "DESCRIPTIVA: Argentina y EE. UU. 2005-2024. 2025 queda vacío porque el PBI es estimado. Brasil vacío")
)

utils::write.csv(diccionario, file.path(cfg$dir_curados, "diccionario.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")

# ---------------------------------------------------------------------------
# 13. Base del anexo: cada dato con su archivo, su fuente y su cotización
# ---------------------------------------------------------------------------
# Dos salidas más en 02_curados/, para compartir junto al trabajo:
#   fuentes.csv     una fila por archivo de data/, con la fuente que el
#                   propio archivo declara, o la falta de ella.
#   base_anexo.csv  el panel en formato largo: una fila por país, año y
#                   variable, con el archivo del que sale, esa fuente, el
#                   tipo de dato y la cotización con la que llegó a dólares.
#
# Ninguna fuente se inventa: lo que el archivo no dice queda dicho que no lo
# dice. La atribución investigada es otra cosa y vive en el diccionario.
# La unidad de la base es el dato anual ya curado: los archivos diarios
# alimentan el promedio y no se copian fila por fila.

# Un valor por país, sin repetir los nombres en cada declaración.
tres <- function(ar, br = ar, us = br) {
  c("Argentina" = ar, "Brasil" = br, "Estados Unidos" = us)
}

region_pais <- tres("Argentina", "Brasil", "Estados Unidos")
region_zona <- tres("Zona núcleo pampeana", "Mato Grosso", "Iowa")
region_eeuu <- tres("Estados Unidos: la serie es la misma en los tres países")
no_aplica   <- tres("no aplica")
en_dolares  <- tres("ya en dólares")
sin_cotizacion <- tres("el archivo no dice la cotización")

# --- Catálogo de archivos -------------------------------------------------
# Los nombres se declaran una sola vez: la base larga los referencia por
# clave, así no hay dos escrituras del mismo nombre de archivo.
arch <- c(
  precio_ar   = "Valores de campos de 1995 a 2026.xlsx - Hoja1.csv",
  precio_mtia = "Precio de la tierra - precio_tierra_MT_Iowa_2002_2025.csv.csv",
  cpi         = "cpi_eeuu_mensual_CPIAUCNS.csv",
  rinde_ar    = "Productividad - Argentina (Buenos Aires).csv",
  rinde_br    = "Productividad - Brasil.csv",
  rinde_us    = "Productividad - Estados Unidos.csv",
  rinde_comp  = "Productividad - Comparación rendimiento.csv",
  maquinaria  = "Venta_maquinaria_agricola_AR_BR_US_2005-2025.xlsx",
  lluvia_ar   = "TIF - Precipitaciones 2005-2025 (avance) - Zona núcleo (Pergamino).csv",
  lluvia_br   = "TIF - Precipitaciones 2005-2025 (avance) - Mato Grosso (INMET).csv",
  lluvia_us   = "TIF - Precipitaciones 2005-2025 (avance) - Corn Belt (NOAA).csv",
  tcr         = "TIF - Tipo de cambio real BIS (AR, BR, EE. UU.) - Anual.csv",
  embi_gd     = "embi_global_diversified_bcrd.csv",
  embi_anual  = paste0("EMBI_promedio_anual_AR_BR_EEUU_2005-2025 - ",
                       "EMBI_promedio_anual_AR_BR_EEUU_2005-2025.csv.csv"),
  retenciones = "retenciones_soja_mensual_2005_2025 (1).csv",
  mayorista   = "tc_mayorista_a3500_diario.csv",
  blue        = "dolar_blue_diario.csv",
  ccl         = "dolar_ccl_diario.csv",
  credito     = "FAOSTAT_data_en_11-9-2026.csv",
  pbi_agro    = "FAOSTAT_MK_valor_agregado_agro_11-9-2026.csv",
  tierra_fao  = "FAOSTAT_data_en_11-9-2026 (1).csv",
  credito_ha  = "FAOSTAT_data_en_11-9-2026 - Crédito por hectárea.csv",
  credito_xls = "FAOSTAT_data_en_11-9-2026.xlsx",
  diesel      = "diesel_usd_l.csv",
  diesel_mes  = "diesel_mensual.csv",
  urea        = "urea_mensual.csv",
  soja        = "TIF - Precio del commodity.xlsx",
  risk_free   = "TIF - Risk Free.xlsx",
  inflacion   = "TIF - Inflacion.xlsx",
  ipc_sanluis = "ipc_san_luis_nivel_general_2003_100.csv",
  tasa        = "TIF - Tasa de interes local.xlsx",
  tips        = "tips_10a.csv",
  fred_agro   = "ALFAPGACB.csv",
  flete       = "flete_puerto_usd_t.csv",
  flete_200   = "flete_ar_referencia_200km.csv",
  ipc_indec   = "sh_ipc_2008anual.xls"
)

# Una fila del catálogo. La fuente y el ticker los lee del propio archivo.
# Cuando el archivo no la trae y es una exportación de un tercero que no se
# reescribe (FAOSTAT, la planilla del BIS, el EMBI del BCRD), la fuente la
# informa el autor del trabajo y la columna declarada_por lo dice. Lo que
# nadie declara queda señalado como tal.
cat_archivo <- function(clave, alimenta, sep = ",", informada = NULL) {
  archivo <- arch[[clave]]
  ruta <- file.path(cfg$dir_crudos, archivo)
  declarada <- fuente_declarada(ruta, sep = sep)
  del_archivo <- file.exists(ruta) && !startsWith(declarada, "sin fuente")

  if (del_archivo) {
    fuente <- declarada
    quien <- "el archivo"
  } else if (!is.null(informada)) {
    fuente <- informada
    quien <- "el autor del trabajo"
  } else if (!file.exists(ruta)) {
    fuente <- declarada
    quien <- "el archivo no está en data/"
  } else {
    fuente <- declarada
    quien <- "nadie: falta la fuente"
  }

  data.frame(
    archivo          = archivo,
    alimenta         = alimenta,
    fuente           = fuente,
    declarada_por    = quien,
    ticker_bloomberg = tickers_declarados(ruta),
    stringsAsFactors = FALSE
  )
}

# Lo que FAOSTAT es, una sola vez: los cinco archivos salen de ahí.
faostat <- paste0(
  "FAOSTAT, base de datos estadística corporativa de la Organización de las ",
  "Naciones Unidas para la Alimentación y la Agricultura (FAO)"
)

catalogo <- rbind(
  cat_archivo("precio_ar", "precio_ha_usd_corriente (Argentina, zona núcleo)",
              informada = paste0("Minola Tierras Argentinas, inmobiliaria ",
                                 "rural (tierrasdeargentina.com.ar): serie ",
                                 "propia de valores de campos de la zona ",
                                 "núcleo sojera, que la firma monitorea desde ",
                                 "1995. No es una estadística oficial ni una ",
                                 "encuesta pública: es la valuación de una ",
                                 "inmobiliaria")),
  cat_archivo("precio_mtia", "precio_ha_usd_corriente (Brasil y EE. UU.)"),
  cat_archivo("cpi", "cpi_eeuu, el deflactor de las tres series de precio"),
  cat_archivo("rinde_ar", "rendimiento_soja_t_ha y superficie_sembrada_soja_ha (Argentina)"),
  cat_archivo("rinde_br", "rendimiento_soja_t_ha y superficie_sembrada_soja_ha (Brasil)"),
  cat_archivo("rinde_us", "rendimiento_soja_t_ha y superficie_sembrada_soja_ha (EE. UU.)"),
  cat_archivo("rinde_comp", "no entra al panel: es el armado de las tres hojas por país",
              informada = paste0("Armado propio sobre las tres hojas de ",
                                 "productividad por país, que sí declaran su ",
                                 "fuente: Secretaría de Agricultura, IBGE y ",
                                 "USDA NASS")),
  cat_archivo("maquinaria", "maquinaria_vendida_unidades"),
  cat_archivo("lluvia_ar", "precipitacion_mm (Argentina)"),
  cat_archivo("lluvia_br", "precipitacion_mm (Brasil)"),
  cat_archivo("lluvia_us", "precipitacion_mm (EE. UU.)"),
  cat_archivo("tcr", "tcr_bis_2020_100 y tcr_bis_2005_2025_100",
              informada = paste0("FRED, series RBARBIS, RBBRBIS y RBUSBIS ",
                                 "(BIS, tipo de cambio real efectivo, canasta ",
                                 "amplia, mensual, 2020 = 100). El promedio ",
                                 "anual de esas tres series coincide con las ",
                                 "columnas del archivo")),
  cat_archivo("embi_gd", "embi_pb, 2008-2025",
              informada = paste0("BCRD (Banco Central de la República ",
                                 "Dominicana), serie histórica del spread del ",
                                 "EMBI de JP Morgan")),
  cat_archivo("embi_anual", "embi_pb, 2005-2007 (EMBI+, aproximación del nivel)"),
  cat_archivo("retenciones", "alicuota_export_soja_pct", sep = ";"),
  cat_archivo("mayorista", "brecha_cambiaria_pct (denominador) y el equivalente del tope de 2018-2019"),
  cat_archivo("blue", "brecha_cambiaria_pct, numerador 2005-2012"),
  cat_archivo("ccl", "brecha_cambiaria_pct, numerador 2013-2025"),
  cat_archivo("credito", "credito_total_musd",
              informada = paste0(faostat, ", dominio Credit to Agriculture")),
  cat_archivo("pbi_agro", "pbi_agropecuario_musd",
              informada = paste0(faostat, ", dominio Macro Indicators, ítem ",
                                 "Value Added (Agriculture, Forestry and ",
                                 "Fishing), elemento Value US$")),
  cat_archivo("tierra_fao", "no entra al panel: uso de la tierra de FAOSTAT",
              informada = paste0(faostat, ", dominio Land Use")),
  cat_archivo("credito_ha", "no entra al panel: crédito por hectárea, derivado de las otras planillas",
              informada = paste0("Armado propio sobre el crédito de ", faostat,
                                 " y la superficie sembrada de las hojas de ",
                                 "productividad")),
  cat_archivo("credito_xls", "no entra al panel: planilla de trabajo de los dos extractos de FAOSTAT",
              informada = paste0("Armado propio sobre los dos extractos de ",
                                 faostat)),
  cat_archivo("diesel", "diesel_usd_l"),
  cat_archivo("diesel_mes", "no entra al panel: es el mensual del que sale el anual"),
  cat_archivo("urea", "urea_usd_t y ln_urea"),
  cat_archivo("soja", "precio_soja_cbot_usd_t y ln_precio_soja_cbot"),
  cat_archivo("risk_free", "treasury_us_10a_nominal_pct"),
  cat_archivo("inflacion", "inflacion_dic_pct (Brasil y EE. UU.; las tres columnas argentinas del archivo ya no se usan)"),
  cat_archivo("ipc_sanluis", "inflacion_dic_pct (Argentina)",
              informada = paste0("Dirección Provincial de Estadística y ",
                                 "Censos de San Luis, IPC-SL base 2003 = 100, ",
                                 "nivel general, republicado como dato ",
                                 "abierto por la Secretaría de Política ",
                                 "Económica en datos.gob.ar (dataset 197, ",
                                 "distribución 197.1)")),
  cat_archivo("tasa", "tasa_nominal_local_pct"),
  cat_archivo("tips", "tips_10a_pct"),
  cat_archivo("fred_agro", "credito_agro_musd (Estados Unidos)",
              informada = paste0(
                "FRED, serie ALFAPGACB (Board of Governors of the Federal ",
                "Reserve System, Loans to Finance Agricultural Production, ",
                "All Commercial Banks, millones de dólares, sin ",
                "desestacionalizar)")),
  cat_archivo("flete", "no entra al panel: el flete salió del diseño"),
  cat_archivo("flete_200", "no entra al panel: tarifa de referencia del flete"),
  cat_archivo("ipc_indec", "no entra al panel: IPC de Gran Buenos Aires, 2008-2012")
)

catalogo <- rbind(catalogo, data.frame(
  archivo = "bcra_prestamos",
  alimenta = "credito_agro_musd (Argentina)",
  fuente = paste0(
    "BCRA, Gerencia de Estadísticas Monetarias, préstamos por actividades ",
    "(SISCEN-0014, Comunicación A 2803). Hoja Saldos de act2005.xls a ",
    "act2025.xls: código 10, saldo al 31 de diciembre, todas las monedas."
  ),
  declarada_por = "el archivo",
  ticker_bloomberg = "no aplica",
  stringsAsFactors = FALSE
))

# Archivos de data/ que nadie declaró en el catálogo. Si aparece uno nuevo,
# el aviso lo señala en vez de dejarlo fuera del anexo en silencio.
sin_catalogar <- setdiff(list.files(cfg$dir_crudos), catalogo$archivo)
if (length(sin_catalogar) > 0) {
  faltantes <- agregar_faltante(
    faltantes, "curado", "catalogo_de_fuentes",
    paste0("Hay archivos en data/ que no están en el catálogo del anexo: ",
           paste(sin_catalogar, collapse = "; "),
           ". Hay que declarar qué alimentan en 01_curado.R.")
  )
}

sin_fuente <- catalogo$archivo[catalogo$declarada_por == "nadie: falta la fuente"]
if (length(sin_fuente) > 0) {
  faltantes <- agregar_faltante(
    faltantes, "curado", "fuentes_sin_declarar",
    paste0("Archivos sin fuente, ni declarada por el archivo ni informada ",
           "por el autor: ", paste(sin_fuente, collapse = "; "),
           ". En el anexo esas celdas dicen «sin fuente en el archivo» y no ",
           "se completan con una atribución inventada.")
  )
}

informadas <- catalogo$archivo[catalogo$declarada_por == "el autor del trabajo"]
if (length(informadas) > 0) {
  faltantes <- agregar_faltante(
    faltantes, "curado", "fuentes_informadas",
    paste0("Estos archivos no declaran su fuente y son exportaciones que no ",
           "se reescriben, así que la fuente la informa el autor del trabajo ",
           "y el anexo lo dice en la columna declarada_por: ",
           paste(informadas, collapse = "; "), ".")
  )
}

fuente_del <- function(clave) {
  catalogo$fuente[match(arch[[clave]], catalogo$archivo)]
}

# --- Procedencia de cada variable ----------------------------------------
# Lo que no se puede leer de los archivos: qué archivo alimenta cada
# variable, en qué región se mide, de qué moneda salió y con qué cotización
# llegó a dólares.
p <- function(archivo, fuente = NULL, region = region_pais,
              moneda = no_aplica, cotizacion = no_aplica,
              tipo = "observado", paises = cfg$paises, origen = NULL,
              declarada_por = NULL) {
  list(archivo = archivo, fuente = fuente, region = region, moneda = moneda,
       cotizacion = cotizacion, tipo = tipo, paises = paises, origen = origen,
       declarada_por = declarada_por)
}

# Quién responde por la fuente cuando la celda de archivo no es un archivo
# de data/: el cálculo propio, la serie que no se construyó, o la mezcla de
# dos archivos. Los demás casos los resuelve el catálogo.
propia  <- tres("cálculo propio en el curado")
tres_vacios <- tres("no hay archivo")

# El diésel es la única serie que el archivo convierte de moneda local, y la
# única que deja a la vista el valor de origen. Su fuente se cita por país:
# son tres organismos distintos y el archivo los distingue por fila.
origen_diesel <- NULL
fuente_diesel <- tres(fuente_del("diesel"))
if (file.exists(ruta_diesel)) {
  pais_diesel <- trimws(crudo_diesel$pais)
  origen_diesel <- data.frame(
    pais  = pais_diesel,
    anio  = a_numero(crudo_diesel$anio),
    valor = a_numero(crudo_diesel$precio_origen),
    stringsAsFactors = FALSE
  )
  fuente_diesel <- vapply(cfg$paises, function(p) {
    fuente_de_columna(crudo_diesel$fuente[pais_diesel == p])
  }, character(1))
}

# El archivo de Mato Grosso e Iowa también trae una columna de fuente por
# país. Se citan las fuentes de los años del período, no las de 2002-2004.
en_periodo <- a_numero(crudo_mt_ia$anio) %in% anios
fuente_precio <- tres(
  fuente_del("precio_ar"),
  fuente_de_columna(crudo_mt_ia$fuente_mato_grosso[en_periodo]),
  fuente_de_columna(crudo_mt_ia$fuente_iowa[en_periodo])
)

calculo <- "Cálculo propio en scripts/01_curado.R."

procedencia <- list(
  precio_ha_usd_corriente = p(
    tres(arch[["precio_ar"]], arch[["precio_mtia"]]),
    fuente = fuente_precio,
    region = region_zona, moneda = tres("USD"),
    cotizacion = tres("el archivo no dice la cotización",
                      "el archivo no dice la cotización",
                      "ya en dólares")),
  cpi_eeuu = p(tres(arch[["cpi"]]), region = region_eeuu),
  precio_ha_usd_2025 = p(
    tres("cálculo propio sobre precio_ha_usd_corriente y cpi_eeuu"),
    fuente = tres(calculo), declarada_por = propia,
    region = region_zona, moneda = tres("USD"),
    cotizacion = tres("ya en dólares, a precios de 2025 por el CPI-U"),
    tipo = "calculado"),
  ln_precio_ha_usd_2025 = p(
    tres("cálculo propio sobre precio_ha_usd_2025"),
    fuente = tres(calculo), declarada_por = propia,
    region = region_zona, tipo = "calculado"),

  rendimiento_soja_t_ha = p(
    tres(arch[["rinde_ar"]], arch[["rinde_br"]], arch[["rinde_us"]]),
    fuente = tres(fuente_del("rinde_ar"), fuente_del("rinde_br"),
                  fuente_del("rinde_us")),
    region = tres("Provincia de Buenos Aires", "Mato Grosso", "Iowa")),
  superficie_sembrada_soja_ha = p(
    tres(arch[["rinde_ar"]], arch[["rinde_br"]], arch[["rinde_us"]]),
    fuente = tres(fuente_del("rinde_ar"), fuente_del("rinde_br"),
                  fuente_del("rinde_us")),
    region = tres("Provincia de Buenos Aires", "Mato Grosso", "Iowa")),
  maquinaria_vendida_unidades = p(tres(arch[["maquinaria"]])),
  ln_ventas_maquinaria_por_ha = p(
    tres("cálculo propio sobre maquinaria_vendida_unidades y superficie_sembrada_soja_ha"),
    fuente = tres(calculo), declarada_por = propia, tipo = "calculado"),
  ln_maquinaria_por_ha = p(
    tres("cálculo propio sobre maquinaria_vendida_unidades y superficie_sembrada_soja_ha"),
    fuente = tres(calculo), declarada_por = propia, tipo = "calculado"),
  urea_usd_t = p(
    tres(arch[["urea"]]),
    region = tres("Precio internacional, el mismo en los tres países"),
    moneda = tres("USD/t"), cotizacion = en_dolares),
  ln_urea = p(
    tres("cálculo propio sobre urea_usd_t"), fuente = tres(calculo),
    declarada_por = propia,
    region = tres("Precio internacional, el mismo en los tres países"),
    tipo = "calculado"),
  diesel_usd_l = p(
    tres(arch[["diesel"]]), fuente = fuente_diesel,
    region = tres("Buenos Aires, Córdoba y Santa Fe", "Mato Grosso",
                  "Medio Oeste (PADD 2, incluye Iowa)"),
    moneda = tres("ARS/l", "BRL/l", "USD/gal"),
    cotizacion = tres("mayorista A 3500, promedio de cada mes",
                      "PTAX venta, promedio de cada mes",
                      "ya en dólares"),
    origen = origen_diesel),
  precipitacion_mm = p(
    tres(arch[["lluvia_ar"]], arch[["lluvia_br"]], arch[["lluvia_us"]]),
    fuente = tres(fuente_del("lluvia_ar"),
                  paste0(fuente_del("lluvia_br"), ". Enero y febrero de 2025 ",
                         "no son de Diamantino sino de Cuiabá: 248,9 mm ",
                         "(INMET) y 189,24 mm (Prefeitura de Cuiabá)"),
                  fuente_del("lluvia_us")),
    region = tres("Estación Pergamino",
                  "Estación Diamantino (83309); Cuiabá en enero y febrero de 2025",
                  "Iowa")),
  precipitacion_desvio_mm = p(
    tres("cálculo propio sobre precipitacion_mm"), fuente = tres(calculo),
    declarada_por = propia,
    region = tres("Estación Pergamino",
                  "Estación Diamantino (83309); Cuiabá en enero y febrero de 2025",
                  "Iowa"),
    tipo = "calculado"),

  precio_soja_cbot_usd_t = p(
    tres(arch[["soja"]]), region = tres("Chicago (CBOT), precio internacional"),
    moneda = tres("USD, centavos por bushel"), cotizacion = en_dolares),
  ln_precio_soja_cbot = p(
    tres("cálculo propio sobre precio_soja_cbot_usd_t"), fuente = tres(calculo),
    declarada_por = propia,
    region = tres("Chicago (CBOT), precio internacional"), tipo = "calculado"),
  tcr_bis_2020_100 = p(tres(arch[["tcr"]])),
  tcr_bis_2005_2025_100 = p(
    tres("cálculo propio sobre tcr_bis_2020_100"), fuente = tres(calculo),
    declarada_por = propia, tipo = "calculado"),
  tips_10a_pct = p(tres(arch[["tips"]]), region = region_eeuu),
  treasury_us_10a_nominal_pct = p(tres(arch[["risk_free"]]), region = region_eeuu),
  inflacion_dic_pct = p(
    tres(arch[["ipc_sanluis"]], arch[["inflacion"]], arch[["inflacion"]]),
    fuente = tres(fuente_del("ipc_sanluis"), fuente_del("inflacion"),
                  fuente_del("inflacion")),
    region = tres("Provincia de San Luis", "Brasil", "Estados Unidos")),
  tasa_nominal_local_pct = p(tres(arch[["tasa"]])),
  tasa_real_local_pct = p(
    tres("cálculo propio sobre tasa_nominal_local_pct e inflacion_dic_pct"),
    fuente = tres(calculo), declarada_por = propia, tipo = "calculado"),

  alicuota_export_soja_pct = p(
    tres(arch[["retenciones"]]), paises = "Argentina"),
  pse_ocde_pct = p(
    tres("no hay archivo en data/"),
    fuente = tres("no hay archivo: la serie no se construyó"),
    declarada_por = tres_vacios),
  embi_pb = p(
    tres(paste0(arch[["embi_gd"]], " (2008-2025) y ", arch[["embi_anual"]],
                " (2005-2007)")),
    fuente = tres(paste0(fuente_del("embi_gd"), " | ", fuente_del("embi_anual"))),
    declarada_por = tres(paste0("el autor del trabajo para 2008-2025 y el ",
                                "archivo para 2005-2007")),
    moneda = tres("puntos básicos"), paises = c("Argentina", "Brasil")),
  brecha_cambiaria_pct = p(
    tres(paste0(arch[["blue"]], " (2005-2012), ", arch[["ccl"]],
                " (2013-2025) y ", arch[["mayorista"]])),
    fuente = tres(paste0(fuente_del("blue"), " | ", fuente_del("ccl"), " | ",
                         fuente_del("mayorista"))),
    declarada_por = tres("el archivo"), moneda = tres("ARS/USD"),
    cotizacion = tres(paste0("blue de Ámbito hasta 2012 y CCL desde 2013, ",
                             "contra el mayorista A 3500")),
    tipo = "calculado", paises = "Argentina"),
  credito_agro_musd = p(
    tres("bcra_prestamos", "no hay archivo", arch[["fred_agro"]]),
    fuente = tres(
      paste0("BCRA, Gerencia de Estadísticas Monetarias, préstamos por ",
             "actividades. Hoja Saldos, código SISCEN 10, saldo al 31 de ",
             "diciembre, todas las monedas."),
      "no hay archivo",
      fuente_del("fred_agro")),
    declarada_por = tres("el archivo", "no hay archivo", "el autor del trabajo"),
    moneda = tres("miles de pesos", "no corresponde", "millones de USD"),
    cotizacion = tres(
      "tipo de cambio de referencia del 31 de diciembre (columna tipref del mismo archivo)",
      "no corresponde",
      "ya en dólares"),
    paises = c("Argentina", "Estados Unidos"), origen = origen_bcra),
  credito_total_musd = p(
    tres(arch[["credito"]]), moneda = tres("USD"), cotizacion = sin_cotizacion),
  pbi_agropecuario_musd = p(
    tres(arch[["pbi_agro"]]), moneda = tres("USD"), cotizacion = sin_cotizacion),
  ln_credito_sobre_pbi_agro = p(
    tres("cálculo propio sobre credito_agro_musd y pbi_agropecuario_musd"),
    fuente = tres(calculo), declarada_por = propia, tipo = "calculado")
)

# Ninguna columna del panel queda fuera del anexo.
variables <- setdiff(names(panel), c("pais", "anio"))
stopifnot(all(variables %in% names(procedencia)))

# El motivo de cada hueco ya está escrito en el registro de faltantes: la
# base del anexo lo cita en vez de repetirlo con otras palabras.
motivos <- tapply(faltantes$motivo, faltantes$elemento,
                  function(x) paste(unique(x), collapse = " "))

fila_anexo <- function(variable) {
  meta <- procedencia[[variable]]
  fila <- data.frame(
    pais             = panel$pais,
    region           = unname(meta$region[panel$pais]),
    anio             = panel$anio,
    variable         = variable,
    valor            = panel[[variable]],
    unidad           = diccionario$unidad[match(variable, diccionario$variable)],
    tipo_dato        = meta$tipo,
    archivo          = unname(meta$archivo[panel$pais]),
    fuente           = if (is.null(meta$fuente)) {
      catalogo$fuente[match(meta$archivo[panel$pais], catalogo$archivo)]
    } else {
      unname(meta$fuente[panel$pais])
    },
    declarada_por    = if (is.null(meta$declarada_por)) {
      catalogo$declarada_por[match(meta$archivo[panel$pais], catalogo$archivo)]
    } else {
      unname(meta$declarada_por[panel$pais])
    },
    moneda_origen    = unname(meta$moneda[panel$pais]),
    cotizacion       = unname(meta$cotizacion[panel$pais]),
    valor_en_moneda_origen = NA_real_,
    nota             = diccionario$transformacion[match(variable, diccionario$variable)],
    stringsAsFactors = FALSE
  )

  if (!is.null(meta$origen)) {
    clave <- paste(fila$pais, fila$anio)
    fila$valor_en_moneda_origen <-
      meta$origen$valor[match(clave, paste(meta$origen$pais, meta$origen$anio))]
  }

  # Países donde la variable no existe: el vacío no es un dato que falte.
  fuera <- !(fila$pais %in% meta$paises)
  fila$tipo_dato[fuera] <- "no corresponde"
  fila$region[fuera] <- "no corresponde"
  fila$moneda_origen[fuera] <- "no corresponde"
  fila$cotizacion[fuera] <- "no corresponde"
  fila$nota[fuera] <- paste0(
    "La variable existe solo en ", paste(meta$paises, collapse = " y "),
    ". No se le asigna cero: cero afirmaría que se midió y dio cero."
  )

  vacio <- is.na(fila$valor) & !fuera
  motivo <- unname(motivos[variable])
  fila$tipo_dato[vacio] <- "vacío"
  fila$nota[vacio] <- if (is.na(motivo)) {
    "Vacío. El motivo está en 03_resultados/faltantes_curado.csv."
  } else {
    paste0("Vacío. ", motivo)
  }

  # 2005 de la inflación argentina no es una medición de los doce meses.
  if (identical(variable, "inflacion_dic_pct")) {
    supuesto <- fila$pais == "Argentina" & fila$anio == 2005 & !is.na(fila$valor)
    fila$tipo_dato[supuesto] <- "supuesto declarado"
  }
  fila
}

base_anexo <- do.call(rbind, lapply(variables, fila_anexo))

guardar_tabla(
  catalogo, "fuentes",
  "Archivos de data/ y la fuente de cada uno, con quién la declara",
  paste0("Una fila por archivo de data/. Cuando el archivo trae su fuente ",
         "—columna de fuente, línea «Fuente:», hoja de fuentes o ticker de ",
         "Bloomberg— se cita esa y declarada_por dice «el archivo». Cuando ",
         "no la trae y es una exportación que no se reescribe, la fuente la ",
         "informa el autor del trabajo y la columna lo dice. «nadie: falta ",
         "la fuente» es el único caso sin respaldo."),
  directorio = cfg$dir_curados
)

guardar_tabla(
  base_anexo, "base_anexo",
  "Base curada del anexo: cada dato con su archivo, su fuente y su cotización",
  paste0("Una fila por país, año y variable del panel. tipo_dato distingue ",
         "observado, calculado, supuesto declarado, vacío y no corresponde. ",
         "declarada_por dice quién responde por la fuente: el archivo, el ",
         "autor del trabajo o nadie. ",
         "La cotización dice con qué tipo de cambio el dato llegó a dólares; ",
         "«ya en dólares» es que el archivo no convierte nada. El signo ",
         "esperado y el papel de cada variable en las ecuaciones están en ",
         "02_curados/diccionario.csv."),
  directorio = cfg$dir_curados
)

# ---------------------------------------------------------------------------
# 14. Aviso en pantalla
# ---------------------------------------------------------------------------
guardar_tabla(faltantes, "faltantes_curado",
              "Series que faltan o que no cumplen la definición del trabajo")

mensaje("Panel escrito: ", nrow(panel), " filas, ", ncol(panel), " columnas.")
mensaje("Diccionario escrito: ", nrow(diccionario), " variables.")
mensaje("Base del anexo escrita: ", nrow(base_anexo), " filas (",
        length(variables), " variables x ", nrow(panel), " filas del panel).")
mensaje("Catálogo de fuentes escrito: ", nrow(catalogo), " archivos. ",
        sum(catalogo$declarada_por == "el archivo"), " citan su fuente, ",
        sum(catalogo$declarada_por == "el autor del trabajo"),
        " la informa el autor, ",
        sum(catalogo$declarada_por == "nadie: falta la fuente"),
        " sin fuente.")
if (nrow(faltantes) > 0) {
  mensaje("\nATENCIÓN: ", nrow(faltantes), " avisos de datos faltantes.")
  for (i in seq_len(nrow(faltantes))) {
    mensaje("  - ", faltantes$elemento[i], ": ", faltantes$motivo[i])
  }
}
