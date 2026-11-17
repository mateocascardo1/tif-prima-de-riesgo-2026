# Registro de cambios — TIF

## 2026-11-11 — La brecha cambiaria vuelve a la ecuación de Argentina

El grupo reabrió solo la brecha cambiaria, que la entrada siguiente de esta fecha había dejado afuera. Entra únicamente en la ecuación de Argentina, en puntos: 100 × (paralelo − mayorista) / mayorista. Hasta 2012 el paralelo es el blue de Ámbito; desde 2013 es el contado con liquidación punta vendedora. No hay factor de enlace. Brasil y Estados Unidos no tienen la serie. En la descomposición, la media del comparable vale 0 por convención, y eso no se lee como una brecha medida en cero. H2 sigue reemplazando solo el EMBI; la alícuota y la brecha se quedan en el valor argentino. El crédito sigue afuera. El resto de las decisiones de esta fecha no cambia. Corrida del 11/11/2026 a las 22:48. En el documento de producción el cambio entra como sugerencias, sin borrar el texto anterior.

## 2026-11-11 — Parte final del TIF: Metodología, Desarrollo y resultados, Conclusiones y anexos
 
### Insumos
- Fuente única de datos: `insumos_TIF.md` (corrida del 10/11/2026 de los scripts en R del grupo), armado a partir de seis PDFs: Entrañas de la estimación, Resultados, Hipótesis, Las cuatro preguntas, Riesgo país de Brasil y Simulación. Cada dato lleva el PDF y la página.
- **Mandan los PDFs sobre `estructura_de_trabajo_TIF.md`.** Diferencias registradas:
  - **Medias:** las de H1, de la descomposición y del precio potencial son las de 2021–2025, con coeficientes estimados sobre 2005–2025. La razón es que la brecha con Mato Grosso cambia de signo dentro del período. El cálculo sobre 2005–2025 queda en el Anexo B.
  - **Precio y estimación:** precio en USD constantes de 2025 (deflactado con el CPI-U), errores Newey-West y *n* = 21/20/19.
  - **Oaxaca-Blinder:** se calcula a mano en R, no con el paquete *oaxaca*.
  - **Cambios de variables:**
    - el diésel reemplaza al flete;
    - entran la tasa real local (Fisher) y la urea;
    - salen la brecha cambiaria y el crédito;
    - el EMBI es el Global Diversified (con EMBI+ en 2005–2007), sin la variable para EE. UU.;
    - el tipo de cambio real es el índice del BIS, donde un aumento es apreciación;
    - la lluvia entra como desvío con signo;
    - la maquinaria entra como ln de las ventas por hectárea.
  - **Regiones:** Iowa en lugar del *Corn Belt*; el rendimiento argentino es el de la provincia de Buenos Aires.
### Proceso
- Los agentes redactaron cada sección en un doc propio y un auditor independiente las revisó, con un máximo de 3 rondas por sección:
  | Sección | Rondas | Resultado |
  |---|---|---|
  | Metodología | 2 | aprobada |
  | Desarrollo y resultados | 2 | aprobada |
  | Conclusiones | 1 | aprobada |
- Docs de trabajo:
  - [TIF 100% – Metodología (borrador)](https://docs.google.com/document/d/1nJkqooi6QodCN6_ac--uT61DV5tgnDuE1UEOD1byXb0/edit)
  - [TIF 100% – Resultados (borrador)](https://docs.google.com/document/d/1zm5gpEGd-WLeb6uL7B4kvboOQE5ocMK13RsPzwJrhQQ/edit)
  - [TIF 100% – Conclusiones (borrador)](https://docs.google.com/document/d/1ZCgOJYKFsyaB1KcHAfKU9d6oO8d9DkfEfGvXQ6UyGMM/edit)
- Figuras (PNG) y código: carpeta de Drive «TIF 100% – Figuras y código».
### Carga en el doc del 100% (TIF - 100% - PROD)
- **Metodología:** entra como sugerencia **S09**. El texto del 40% (incluidas S06 y S07) queda subrayado.
- **Secciones nuevas como texto normal:** Desarrollo y resultados (Tablas 3–11, Figuras 1–5) y Conclusiones.
- **Referencias:** se agregaron 22 fuentes nuevas, con lo que la lista llega a 57.
- **Anexos:** Anexo A (Tabla A1, pruebas) y Anexo B (Tablas B1–B2, Figura B1, persistencia).
- **Figuras:** en el doc quedan como marcadores `[insertar Figura X: archivo.png]`.
- **Revisión de escritura (Agente E):** propuso las sugerencias **S10–S56**:
  - Resumen y Abstract con resultados y veredicto de H1, en 150 palabras como máximo;
  - Planteamiento, Marco teórico (Tabla 1, apartados 2–5) y apartado 7.5 alineados con el modelo en logaritmos;
  - transiciones entre secciones y quita de los «[EN DESARROLLO]».
### Resultados principales
- Brecha 2021–2025: Mato Grosso +61,3% (0,478 puntos log) e Iowa +89,5% (0,639 puntos log).
- Componente de niveles en sentido contrario a la brecha: la brecha queda en la valoración, que se lee como residual agregado.
- **H1: confirmada parcialmente.** Se cumple frente a Brasil, por el signo negativo de la maquinaria. No se cumple frente a EE. UU., donde el componente económico (tipo de cambio real) supera al institucional.
- **Precio potencial (P3/O3):** converger al EMBI de Brasil sube el precio predicho un 7,8% y cubre el 15,7% de la brecha con Mato Grosso, menos de la mitad. Los PDFs llaman a este criterio «H2» y lo dan por rechazado; en el doc se presenta como criterio de P3, sin crear una hipótesis nueva.
### Decisiones tomadas por el orquestador (a confirmar por el grupo)
- **Tipo de cambio real:** el signo esperado es el de la especificación estimada, con la escala invertida respecto del Marco teórico. La adecuación del Marco teórico va en las sugerencias S39–S40, con huecos de fuente.
- **Convergencia parcial:** los valores del 25% y del 50% se derivan por proporción lineal y quedan marcados como «confirmar con la salida de 06_convergencia.R».
- **Anexos:** se reorganizaron en A (Metodología) y B (Resultados), y las tablas de Resultados se renumeraron de la 3 a la 11.
### Pendientes del grupo
- **Huecos:**
  - 8 módulos BMC;
  - resultados de BG, BP y RESET, y de ADF/KPSS serie por serie;
  - descriptiva de los regresores;
  - *R*² simple y EE de la constante;
  - *F* y *p* de igualdad de pendientes;
  - simulación fuera de muestra;
  - precio predicho en las medias en USD;
  - confirmación del 25% y del 50%;
  - error de digitalización (S06);
  - regla ante el rechazo de RESET;
  - referencias de Minola y de las demás fuentes de datos;
  - años y versiones de R y de los paquetes;
  - fechas de consulta.
- **Discrepancia del Wald institucional argentino:** PDF Resultados p. 5 (*F* = 1,48, no significativo) frente a PDF Entrañas p. 7 (*p* = 0,028).
- **Extensión fuera de tope:** Metodología ≈6,3 pp. (tope 5) y Resultados ≈16 pp. (tope 12). Con Arial 11 e interlineado 1,5, los mínimos de palabras y los máximos de páginas no se pueden cumplir a la vez.
- **Decisiones de redacción:**
  - signo del tipo de cambio real frente a S03 y a Nag y Reimer (2011);
  - alcance de P3 y O3 (solo converge el EMBI);
  - Figura 1 del Planteamiento (S22);
  - S20 frente a S01;
  - «Trabajo Integrador Final» frente a «Trabajo de Integración Final».
- **Acciones en el doc:**
  - insertar las figuras desde Drive;
  - actualizar el índice;
  - repetir el encabezado de las tablas largas.
---
 
## 2026-10-12 — Definición de la prima de riesgo implícita (decisión del grupo)
 
- **Decisión:** la prima de riesgo implícita es **toda la brecha** de precio por hectárea entre la tierra argentina y cada comparable, es decir, el descuento con el que el mercado valúa la tierra argentina. Deja de ser el componente de valoración de Oaxaca-Blinder.
- La descomposición reparte esa prima entre factores productivos, económicos e institucionales (componente de niveles). El componente de valoración es la parte que no explican los niveles: la distinta valoración de cada mercado y las diferencias no observadas.
- Se mantiene la distinción con la prima por riesgo país, que es un componente de *r* y una variable del modelo.
- Se aplicó como sugerencia R89 en el apartado 1 (definición) y en el cierre del apartado 5.
- **Componente de valoración (A1.b):** se interpreta como un residual neutro. Incluye las diferencias de coeficientes y la diferencia de constantes, se informa solo en forma agregada y no se atribuye a ningún factor (Jann, 2008). Se aplicó como sugerencia R90 en el cierre del apartado 5.
- **Pendiente:** alinear con esta definición el Resumen, el Abstract y la Introducción, que todavía dicen que la prima es la parte de la brecha debida a la distinta valoración. La revisión integral está en `revision_integral_TIF_2026-10-12.md`.
---
 
## 2026-10-11 — Correcciones de la revisión integral (PROD)
 
### Tabla 1 alineada con los apartados 2 a 4
- **R:** se reemplazó clima por precipitación y derechos de exportación por alícuota impositiva.
- **r:** se agregó la tasa de interés real libre de riesgo (factor económico), los factores pasan a ser "Económicos e institucionales" y la variable de crédito es "créditos hipotecarios al agro sobre el PBI".
- **g:** solo factores productivos; la adopción tecnológica se mide con ventas de maquinaria agrícola. Se quitó la tendencia del precio de la soja porque no es una variable del modelo.
- La excepción de signo del apartado 4 es el crédito hipotecario (positivo), ya no la "reserva de valor".
### Incongruencias resueltas
- La pregunta 2 dice "infraestructura logística".
- Justificación: la descomposición Oaxaca-Blinder proviene de la economía laboral (Blinder, 1973; Oaxaca, 1973).
- Tipo de cambio real de EE. UU.: signo esperado negativo, según Nag y Reimer (2011) y la definición adoptada.
- Variable fiscal unificada como "alícuota impositiva"; "retenciones" pasa a "derechos de exportación".
### Citas y referencias
- CAIR: USD 17.000 con la fuente Reinke (2025), *La Nación*. La referencia se volvió a incorporar.
- NASS con nombre completo en su primera mención.
- Gordon (1959) se cita antes que Gordon y Shapiro (1956), y se agregó el DOI de Gordon y Shapiro.
- Rangos con raya corta (–).
- Wooldridge se cita como traducción: (2009/2010), con "(Trabajo original publicado en 2009)".
- Comillas tipográficas, [sic] en Marcolini y nexos de las citas corregidos.
### Redacción
- Tiempo pasado en los estudios citados.
- Se eliminó el registro coloquial y se arreglaron oraciones rotas.
- Cursivas agregadas: *commodities*, *shocks*, *ceteris paribus*, *t*, *R²*, *H₀*, *y* y los términos de la Tabla 1.
- "Trabajo Integrador Final", "brecha" y paráfrasis de "el presente trabajo".
- Just y Miranowski se quitó del apartado 3, que ahora remite al estado del conocimiento.
### Sugerencias R88 (pendientes de aprobación)
- Se suma la prima de riesgo implícita a la pregunta principal y al objetivo general.
- Se distingue la prima implícita de la prima por riesgo país.
### Pendientes
- Fuente del valor de Mato Grosso (USD 22.400).
- Minola y S&P Global en la nota de la Figura 1.
- DOI sin verificar: Blinder, Oaxaca, Falk y Lee, Latruffe, Weersink y D'Argensio.
- Versión pt/en del DOI de Marques y Telles.
- Referencias de R y del paquete oaxaca.
- La Figura 1 está vacía y quedan dos marcas [EN DESARROLLO].
- Si se acepta R88, revisar si la hipótesis debe pronunciarse sobre la prima.
---
 
## 2026-09-15 — Reencuadre completo de la ficha técnica
 
### Cambio de enfoque metodológico (decisión del grupo)
- **Se abandona el modelo de Gordon** (P = R/(r−g)) como marco de descomposición algebraica.
- **Nuevo enfoque: modelo econométrico.** El objetivo es construir una regresión que estime la contribución de cada factor al precio por hectárea.
- **Tres categorías de factores** reemplazan los dos canales anteriores (riesgo vs. productividad):
  1. **Institucionales:** riesgo soberano (EMBI+), controles cambiarios, derechos de exportación (retenciones), restricciones a la propiedad extranjera (Ley 26.737), disponibilidad de crédito hipotecario rural.
  2. **Económicos:** precios de los commodities, tipo de cambio real.
  3. **Productivos:** rendimiento por hectárea, adopción tecnológica, infraestructura logística.
- **g ya no se asume constante** entre países; la simplificación anterior se descarta.
### Título actualizado
- **Antes:** "Prima de riesgo implícita en la tierra agrícola argentina: De la brecha de precios por hectárea a la descomposición del factor de descuento."
- **Ahora:** "Prima de riesgo implícita en la tierra agrícola argentina: La composición de la brecha de precios por hectárea con Brasil y Estados Unidos."
### Preguntas de investigación (4)
- Q1: ¿Cuánto incide en el precio por hectárea la diferencia en los factores productivos?
- Q2: ¿Cuánto incide en el precio por hectárea la diferencia en los factores institucionales?
- Q3: ¿Cuánto incide en el precio por hectárea la diferencia en los factores económicos?
- Q4: ¿Cómo se modificaría el valor de la tierra ante una convergencia de los factores institucionales a niveles de Brasil?
### Objetivos actualizados
- **General:** Construir un modelo econométrico que identifique y cuantifique la contribución relativa de los factores productivos, económicos e institucionales en la brecha de precios.
- **Particulares:** O1 (productivos) ← Q1; O2 (institucionales + económicos) ← Q2 + Q3; O3 (convergencia) ← Q4.
### Hipótesis
- "Los factores institucionales constituyen el principal determinante de la brecha de precios por hectárea de la tierra agrícola destinada a la producción de soja entre Argentina, Brasil y Estados Unidos, explicando una proporción mayor del diferencial que los factores productivos y económicos."
### Estado del conocimiento
- Reescrito con **enfoque positivo** según feedback del tutor: se presentan los 4 trabajos más cercanos (Featherstone & Baker, D'Argensio & Laurin, Maito, Marcolini Tulliani), explicando qué aporta cada uno y posicionando el TIF como integración.
- Se eliminó la afirmación "no existe un modelo" y se reemplazó por un encuadre constructivo.
### Marco teórico
- Se mantienen los antecedentes teóricos (renta diferencial, capitalización de políticas, riesgo soberano) pero reencuadrados hacia el modelo econométrico de 3 factores.
- Gordon queda como antecedente implícito de la literatura, no como el marco operativo.
### Bibliografía
- 12 fuentes: 6 en inglés + 6 en español/portugués.
- Se incorporaron: Damodaran (2022), Reydon & Plata (2014), Gasques et al. (2014), Lema & Gallacher (2016) — que estaban citados en el texto pero faltaban en la lista.
### Documentos del proyecto actualizados
- `ficha_tecnica_TIF.md` — reescrita de cero con el nuevo enfoque.
- DOCX generado: `Ficha_Tecnica_TIF_Actualizada.docx`.
### Impacto en secciones ya redactadas (pendiente)
- La Introducción, Planteamiento, Estado del Conocimiento, Justificación y Objetivos que están en Google Docs fueron escritos para el esquema de 2 factores. **Necesitan actualización** al esquema de 3 factores antes de la entrega del 40%.
---
 
## 2026-09-01 — Redacción de Introducción, Planteamiento del problema, Estado del conocimiento, Justificación y Objetivos
 
### Objetivos (nuevo)
- Redactado el objetivo general y 3 objetivos particulares.
- **Decisión metodológica: g constante entre países.** *(REVERTIDA en sept. 15)* El modelo asumía la misma tasa de crecimiento esperado (g) para Argentina, Brasil y EE.UU.
- Ficha técnica actualizada con: objetivo 1 revisado, nota sobre g constante, planteamiento ajustado a dos factores (no tres canales), marco teórico ajustado. BCR (2026) incorporada a la bibliografía.
- Google Doc: [TIF - Objetivos](https://docs.google.com/document/d/16JV12kYFcvVOt4uw_LlCvKXO5ZyH23mMgIXO-mUFJEQ/edit)
### Justificación de la investigación (nuevo)
- Redactada cubriendo las 5 dimensiones de la estructura de cátedra: conveniencia/valor teórico, relevancia social (productores, inversores, sector público), implicaciones prácticas (escenario de convergencia), y utilidad metodológica.
- Google Doc: [TIF - Justificación de la investigación](https://docs.google.com/document/d/163TwVsABDmLliIgiDbgcYGi5kjNhq37wmwtU6Ljx9XY/edit)
### Estado del conocimiento (nuevo)
- Redactado con encuadre positivo. *(Necesita reescritura según nuevo enfoque — sept. 15)*
- Google Doc: [TIF - Estado del conocimiento](https://docs.google.com/document/d/1DxbEBu9Pk1k0mR81giZT9ya-auKmOqaXcZ9HblwB0K4/edit)
### Introducción (sección 1)
- Redactada cubriendo los 6 puntos de la presentación de cátedra (a-f). *(Necesita actualización a 3 factores — sept. 15)*
- Google Doc: [TIF - 1. Introducción](https://docs.google.com/document/d/1Onjl6wHh_tN_8Ylxjdl_c1b2nBTFLFTNWdxy9rZoBZo/edit)
### Planteamiento del problema
- Redactado con el marco de dos factores. *(Necesita reescritura con 3 factores — sept. 15)*
- Google Doc: [TIF - Planteamiento del problema](https://docs.google.com/document/d/1WMVuKxLDO23B6WPwAcrTUCzIDdlCH-xbJgdxqGuQmAw/edit)
### Cambios en documentos del proyecto
- **Interlineado corregido** de 2,0 a **1,5 líneas** en `reglas_de_escritura_TIF.md` (según slides de cátedra).
- **Encabezado** agregado: título del TIF (izquierda) / TIF-Licenciatura en Finanzas (derecha).
- **Requisitos de la Introducción** (puntos a-f) incorporados como sección 4 de `reglas_de_escritura_TIF.md`.