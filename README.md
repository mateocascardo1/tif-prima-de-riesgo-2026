# Brecha de precio de la tierra agrícola: Argentina, Brasil y Estados Unidos (2005-2025)

Código del Trabajo de Integración Final. Estima una regresión del precio por
hectárea por país, descompone la brecha con Oaxaca-Blinder y calcula un
escenario de precio potencial para Argentina.

La soja es el cultivo de referencia. Las tres regiones son la zona núcleo
pampeana, el estado de Mato Grosso y el estado de Iowa.

## Cómo correrlo

```bash
Rscript run_all.R
```

Un solo comando, desde la raíz del proyecto. Tarda menos de un minuto.

La primera vez hay que instalar R (versión 4.6 o posterior) y restaurar los
paquetes:

```bash
Rscript -e 'renv::restore()'
```

`renv` fija las versiones exactas de los cuatro paquetes que se usan
(`readxl`, `tseries`, `sandwich`, `lmtest`), así que la corrida da lo mismo en
cualquier máquina. `boot`, que hace el bootstrap, viene con R.

## Qué produce cada script

Todos están en `scripts/` y se corren en este orden. Cada uno se puede correr
suelto, siempre desde la raíz del proyecto.

| Script | Qué hace |
| --- | --- |
| `00_config.R` | Todas las decisiones del trabajo en un solo lugar: período, qué variable fiscal se usa, qué regresores entran en cada ecuación, especificación, grados de convergencia. Si hay que cambiar algo, se cambia acá. |
| `utilidades.R` | Funciones compartidas: lectura de los crudos, guardado de tablas, el control que frena la estimación, VIF y prueba de Wald. |
| `01_curado.R` | Lee `data/` y escribe `02_curados/panel.csv`, `02_curados/diccionario.csv` y la base del anexo (`base_anexo.csv` y `fuentes.csv`). |
| `02_descriptivos.R` | Estadísticas descriptivas y Figura 1. |
| `03_pruebas.R` | ADF y KPSS por serie y por país, y la especificación que sugieren. |
| `04_estimacion.R` | Una regresión por país con errores Newey-West, diagnósticos y la prueba de igualdad de coeficientes económicos. |
| `05_descomposicion.R` | Oaxaca-Blinder a mano, con intervalos por bootstrap en bloques, y el veredicto de H1. |
| `06_convergencia.R` | Precio potencial por grado de convergencia institucional y el veredicto de H2. |
| `07_informe_html.R` | Informe HTML de la estimación, los diagnósticos y las hipótesis. |
| `08_simulacion.R` | Precio que habría dado cada ecuación, año por año. |
| `09_informe_simulacion.R` | Informe HTML de esa simulación. |
| `10_riesgo_brasil.R` | Precio argentino si el riesgo país hubiera sido el de Brasil. |
| `11_informe_riesgo_brasil.R` | Informe HTML de ese ejercicio. |
| `12_informe_preguntas.R` | Informe de las cuatro preguntas del trabajo. |
| `13_informe_hipotesis.R` | Informe de la hipótesis institucional. |
| `14_anexo_datos.R` | Cuadros del anexo: un país, un año por fila, solo lo que entra en la ecuación. |
| `html_comun.R` | Formato compartido de los informes. No se corre solo. |
| `15_figuras_html.py` | Las figuras en un solo HTML. Se corre aparte, con Python, después de `run_all.R`. |

`run_all.R` los corre en orden (salvo el script de Python) y después escribe
`informe.md` con el resumen de la corrida: qué faltó, qué se decidió y cómo
dieron las hipótesis.

## Carpetas

```
data/          datos crudos. Solo se leen; ningún script escribe acá.
02_curados/    panel.csv, diccionario.csv y la base del anexo (los genera 01_curado.R)
03_resultados/ tablas en .csv y .html, figuras en .png
scripts/       el código
informe.md     resumen de la última corrida (se genera solo)
```

`02_curados/`, `03_resultados/` e `informe.md` no van al repositorio: se
rehacen con `Rscript run_all.R`. `data/` tampoco, porque tiene descargas que
no se pueden redistribuir.

Las tablas salen en `.csv` (para trabajarlas) y en `.html` (para pegarlas al
anexo). La Figura 1 está en blanco y negro, con un tipo de línea distinto por
región, para que se entienda impresa.

## La base del anexo

`01_curado.R` escribe en `02_curados/`, junto al panel, dos tablas pensadas
para compartir:

- `base_anexo.csv` es el panel en formato largo: una fila por país, año y
  variable, con el valor, la unidad, la región medida, el archivo de `data/`
  del que sale, su fuente, quién la declara, el tipo de dato
  (observado, calculado, supuesto declarado, vacío o no corresponde), la
  moneda de origen y la cotización con la que el dato llegó a dólares.
- `fuentes.csv` es el catálogo de archivos: una fila por archivo de `data/`,
  con qué variable alimenta, su fuente, quién la declara y el ticker de
  Bloomberg cuando corresponde.

La fuente se busca primero en el propio archivo: una columna de fuente, una
línea «Fuente:», una hoja de fuentes o el ticker. Ahí la columna
`declarada_por` dice «el archivo». Cuando el archivo no la trae y es una
exportación de un tercero que no se reescribe —los extractos de FAOSTAT, la
planilla del BIS, el EMBI del BCRD, la planilla de valores de campos— la
fuente la informa el autor del trabajo y la columna lo dice, para que no se
confunda lo que el dato trae con lo que el autor sabe. Hoy los 33 archivos
tienen fuente: 24 la declaran y 9 la informa el autor, y esos 9 se listan en
`03_resultados/faltantes_curado.csv` para que se los pueda revisar uno por
uno. Si algún día entra un archivo sin fuente de ninguna de las dos formas,
el anexo dice «sin fuente en el archivo» y el aviso lo señala.

La única serie que llega en moneda local es el diésel: pesos al mayorista
A 3500 en Argentina, reales al PTAX venta en Brasil y dólares por galón en
Estados Unidos. El resto llega en dólares; donde el archivo no dice con qué
cotización se armó ese dólar, la base lo declara.

## Reglas que el código respeta

1. **Los crudos no se tocan.** Toda transformación está en los scripts, no en
   una planilla. Si hay que rehacer algo, se vuelve a correr y da lo mismo.
2. **No se inventan datos.** Nada se interpola, se extrapola ni se rellena. Si
   falta un año, queda vacío y el motivo va al diccionario y al informe.
3. **Si falta un dato, el análisis se frena y lo dice.** Antes de estimar,
   `04_estimacion.R` controla que todas las variables de la especificación
   existan y estén completas. Si no, no estima nada y escribe
   `03_resultados/no_estimado.csv` con el detalle. Un resultado ausente es
   preferible a uno construido sobre supuestos sin declarar.

## Estado actual

El pipeline corre de punta a punta. La ecuación que se estima es la de
niveles (`niveles_MCO` en `00_config.R`). Los precios quedan en dólares
constantes de 2025.

El repositorio trae el código, no los números. Para ver tablas y figuras hay
que tener `data/` en la máquina y correr `Rscript run_all.R`.

## Decisiones que conviene conocer antes de leer los resultados

- **Lluvia, EMBI y la alícuota de 2008 están prendidos** en `00_config.R`
  (`incluir_precipitacion`, `usar_embi`, `aceptar_alicuotas_no_nominales`).
  El comentario al lado de cada uno dice qué entra y qué queda declarado.
- **La alícuota de retenciones existe solo para Argentina.** A Brasil y
  Estados Unidos no se les pone cero: cero afirmaría que se midió y dio cero.
  En la descomposición, una variable ausente en el país comparable vale 0 en su
  vector de medias, así que toda su contribución cae en el componente de
  niveles. Es una convención de medición, no un hallazgo, y está advertida en
  la tabla.
- **Parte de lo económico no mueve los niveles.** El precio CBOT y la tasa
  TIPS son los mismos en los tres países, y el tipo de cambio real está
  normalizado a promedio 100 en cada uno. Esas tres solo pueden actuar por
  sensibilidad. La inflación y la tasa nominal local sí difieren entre países.
- **La descomposición se calcula a mano, no con el paquete `oaxaca`.** Ese
  paquete exige la misma fórmula en las dos ecuaciones, y acá hay variables que
  existen en un país y no en el otro.
- **El componente de valoración se informa solo agregado.** Su reparto por
  variable no está identificado (Oaxaca y Ransom, 1999).

## Qué especificación usar

La regla está fijada de antemano y la aplica `03_pruebas.R`:

- series I(0): `niveles_MCO`;
- series I(1) con cointegración: `ARDL`, previo bounds test;
- series I(1) sin cointegración: `diferencias`.

Con 21 observaciones el ADF y el KPSS tienen poca potencia y a menudo no
coinciden. Cuando pasa, `03_pruebas.R` deja "ambiguo" en
`03_resultados/tabla_raices_unitarias.csv`, en vez de elegir la prueba que
convenga. La ecuación que usa el trabajo igual es la de niveles, porque la
descomposición compara medias del precio.

La descomposición usa siempre la ecuación en niveles, aunque la especificación
elegida sea otra, porque compara medias de la dependiente. Si las dos difieren,
`04_estimacion.R` lo avisa.
