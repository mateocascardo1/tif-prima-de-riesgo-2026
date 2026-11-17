---
name: research
description: Provee el protocolo de búsqueda de fuentes, definiciones de variables y series faltantes del TIF. Se activa cuando se piden fuentes, se consulta el estado de una serie, se busca literatura o se pregunta qué series faltan del panel.
---

# Investigación — Fuentes, variables y series faltantes

Antes de buscar fuera del repositorio, leer estos tres archivos:

1. **`scripts/00_config.R`** — decisiones vigentes (qué regresores entran, qué
   series están apagadas y por qué).
2. **`02_curados/diccionario.csv`** — definición, unidad, fuente, estado y signo
   esperado de cada variable.
3. **`03_resultados/faltantes_curado.csv`** — series que el pipeline detectó como
   faltantes, con el motivo en cada caso.

## Clasificación de una serie

Usar el estado que ya define el diccionario:

| Estado | Significado |
|--------|-------------|
| en el panel | La serie tiene datos en `panel.csv`. |
| DESCRIPTIVA | Está en el panel pero no entra como regresor. |
| APAGADA | Existe pero está desactivada en `00_config.R` (ej. EMBI, precipitación). |
| FALTA | No hay archivo en `data/`. |
| VACÍA | La columna existe pero depende de otra serie faltante (ej. `ln_precio_ha_usd_2025` depende del CPI-U). |

## Ficha de una serie faltante

Si la serie tiene estado FALTA o VACÍA, armar una ficha con:

- **Definición del trabajo**: qué mide y cómo se usa en la ecuación.
- **Unidad**: la que espera el panel.
- **Cobertura requerida**: 2005-2025, anual.
- **Fuente sugerida**: la que pide el diseño del trabajo (CPI-U del BLS, CBOT,
  FRED DFII10, PSE de la OCDE, etc.).
- **Archivo y columnas esperados**: los que `01_curado.R` ya busca (por ejemplo,
  `data/cpi_eeuu.csv` con `anio` y `cpi_u`).
- **Rol en el modelo**: si entra como regresor y en qué ecuaciones.

## Restricciones

- No inventar números ni estimar valores para completar una serie.
- No escribir en `data/`. El archivo lo incorpora el usuario.
- No sustituir una serie por otra de definición distinta (por ejemplo, no
  reemplazar el EMBI Global Diversified por el EMBI+ que ya está en el panel).
