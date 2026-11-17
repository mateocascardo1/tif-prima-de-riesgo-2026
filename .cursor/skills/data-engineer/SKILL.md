---
name: data-engineer
description: Define las reglas del pipeline de regresión en R del TIF. Se activa al modificar el curado del panel, los descriptivos, o al escribir las etapas de pruebas, estimación, descomposición de Oaxaca-Blinder y escenarios de convergencia.
---

# Ingeniería de datos — Pipeline de regresión en R

## Protección del pipeline

`04_estimacion` verifica el gate (`verificar_gate()` en `utilidades.R`): si
falta alguna serie del panel, escribe `no_estimado.csv` y no produce
`modelos.rds`. Los scripts `05_descomposicion` y `06_convergencia` cortan al
no encontrar `modelos.rds` y dejan su propio aviso en `no_estimado.csv`.

## Especificación

Sale de `03_pruebas.R` y queda en `cfg$especificacion`:

| Valor | Condición |
|-------|-----------|
| `niveles_MCO` | Series I(0). |
| `ARDL` | Series I(1) con cointegración. |
| `diferencias` | Series I(1) sin cointegración. |

## Inferencia

- Errores estándar Newey-West (HAC) con rezago `cfg$rezago_hac`.
- VIF calculado a mano (`vif_manual` en `utilidades.R`).
- Bootstrap en bloques: `B = 999`, largo `round(n^(1/3))`, semilla `2025`.
- Oaxaca-Blinder con referencia `argentina`; `pooled` solo como robustez.
- Convergencia institucional hacia Brasil en 25%, 50% y 100%.

## Incorporar una serie nueva

1. La transformación se escribe en `01_curado.R`.
2. Se documenta en el diccionario (`02_curados/diccionario.csv`).
2b. Se declara su procedencia en la sección 13 de `01_curado.R`: el archivo
   del que sale, la región medida, la moneda de origen y la cotización. Sin
   eso el curado se detiene: ninguna columna del panel queda fuera del anexo.
3. No copiar una serie de definición distinta para "completar" el modelo.
4. Con n = 21 no se agregan regresores del mismo concepto (maquinaria, diésel
   y crédito quedan fuera mientras no cumplan la definición del trabajo).
   El diésel minorista reemplaza al flete y queda descriptivo: Brasil 2020
   no tiene el relevamiento de septiembre.

## Flujo del pipeline

    data/ (solo lectura) → 01_curado → panel + diccionario
                                      → 02_descriptivos
                                      → 03_pruebas → cfg$especificacion
                                      → 04_estimacion (gate interno)
                                          ├─ ok → modelos.rds → 05_descomposicion → 06_convergencia
                                          └─ faltan series → no_estimado.csv (05 y 06 también avisan)
