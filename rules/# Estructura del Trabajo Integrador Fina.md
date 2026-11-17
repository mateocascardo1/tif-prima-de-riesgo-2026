# Estructura del Trabajo Integrador Final (TIF)
 
## Prima de riesgo implícita en la tierra agrícola argentina: la composición de la brecha de precios por hectárea con Brasil y Estados Unidos
 
**Autores:** Belardo, Cascardo, Medina, Mellicovsky | **Tutor:** Ranucci Santiago
**Carrera:** Licenciatura en Finanzas, UADE | **Cátedra:** Prof. Karina Díaz | **Cuatrimestre:** 2C 2026
**Línea de investigación:** Informe del mercado inmobiliario e índice del salario real (2025)
 
> **Versión:** actualizada el 11/11/2026 a partir del artifact «Mapa del TIF al 40%» (13/10/2026), del registro de cambios, de la revisión integral del 12/10/2026 y de la decisión del grupo de estimar el modelo con el **logaritmo** del precio por hectárea (11/11/2026). Reemplaza la versión de agosto, que seguía organizada en los tres canales de Gordon (renta, tasa de descuento, crecimiento) como método de cálculo. Antes de escribir, confirmar contra el Google Doc, que puede haber cambiado después del 13/10.
 
---
 
## Qué cambió respecto de la versión anterior
 
| Tema | Antes (agosto) | Ahora |
|---|---|---|
| Método | Descomposición algebraica de Gordon, *P* = *R* / (*r* − *g*), en tres canales | Modelo econométrico: una regresión por MCO del logaritmo del precio por hectárea para cada región, y luego descomposición de Oaxaca-Blinder de la brecha |
| Rol de Gordon | Método de cálculo | Solo fundamento teórico: ordena los factores según actúen sobre *R*, *r* o *g* (Tabla 1). No se estiman *r* ni *g* |
| Organización de los factores | Tres canales: renta, descuento, crecimiento | Tres categorías: productivos, económicos e institucionales |
| Prima de riesgo implícita | La parte del descuento atribuible al riesgo | **Toda la brecha** de precio por hectárea con cada comparable (decisión del 12/10/2026). Se distingue de la prima por riesgo país, que es un componente de *r* y entra como regresor |
| Componente de valoración | — | Residual neutro: diferencias de coeficientes más diferencia de constantes. Se informa solo agregado, sin atribuirlo a ningún factor (Jann, 2008) |
| Variable dependiente | Precio por hectárea | **Logaritmo natural del precio por hectárea en USD** (decisión del 11/11/2026). La brecha y su descomposición se leen en puntos logarítmicos, se convierten a porcentaje con (e^Δ − 1) × 100 y se traducen a USD/ha para su presentación. Reemplaza la versión del 40%, que usaba el precio en USD/ha en niveles |
| Escenario de convergencia | Convergencia de riesgo país y condiciones cambiarias | **Precio potencial:** la ecuación argentina con los factores institucionales observados en Brasil, en las medias de 2005–2025, más la serie anual. Se expresa en USD/ha como la exponencial del valor predicho |
| Preguntas | Cuatro (renta, descuento, crecimiento, convergencia) | Una principal (proporción por categoría) y tres secundarias (productivos, económicos, convergencia institucional) |
| Hipótesis | La tasa de descuento explica la mayor parte | H1: los factores institucionales explican una proporción mayor de la brecha que los productivos y los económicos, según el componente de niveles |
| Restricciones a la propiedad extranjera (Ley 26.737) y mercados de futuros | Dentro del análisis | No están entre las variables del modelo. Pendiente: confirmarlo y declararlo como limitación (su efecto queda en la constante, es decir, en la valoración) |
| Interlineado | Doble | **1,5** (reglas_de_escritura_TIF.md) |
| Orden del documento | El de la cátedra (Introducción → Marco de referencia → Objetivos → Hipótesis → Metodología) | El del doc del grupo (ver abajo). La revisión integral sugirió volver al orden de la cátedra; queda para consultar con el tutor |
 
---
 
## Requisitos generales del documento (cátedra)
 
- **Extensión del cuerpo:** 25–30 páginas, desde la Introducción hasta las Conclusiones. Al 40% el cuerpo tenía unas 13 páginas.
- **Extensión máxima total:** 50 páginas, con Carátula, Índice, Anexos y Referencias.
- **Formato:** Arial 11, interlineado 1,5, márgenes de 2,54 cm, texto justificado, sangría de primera línea de 1,27 cm, sin espacio extra entre párrafos, páginas numeradas. Títulos numerados (1., 1.1., 1.1.1.) en Arial 12 negrita.
- **Citas y referencias:** APA 7.ª edición.
- **Bibliografía mínima:** 20 fuentes, al menos 5 en inglés y 5 en español, y como mínimo 50% académicas. Al 40% hay 34 referencias, todas citadas.
- **Enfoque:** cuantitativo, obligatorio en la Licenciatura en Finanzas.
- **Herramientas:** Bloomberg (8 módulos BMC documentados), estadística verificada y R (paquete *oaxaca*).
---
 
## Ficha del modelo (referencia para todas las secciones)
 
- **Unidades y período:** tierra sojera de la zona núcleo pampeana (Argentina), Mato Grosso (Brasil) y el Corn Belt (EE. UU.), con series anuales de 2005 a 2025.
- **Variable dependiente:** ln del precio por hectárea en USD. Pendiente: decidir si se expresa en dólares corrientes o en dólares constantes de 2025 (la revisión integral recomienda deflactar por el IPC de EE. UU.).
- **Estimación:** MCO por región de ln *P*. Cada coeficiente se lee como el cambio porcentual aproximado del precio ante una unidad más del regresor, *ceteris paribus* (100 × β); si un regresor también entra en logaritmos, el coeficiente es una elasticidad. Confiabilidad con el estadístico *t* (1%, 5%, 10%), el *R*² y errores estándar robustos a la heterocedasticidad. Pruebas de raíz unitaria, multicolinealidad, autocorrelación y heterocedasticidad. Si faltan años, panel con coeficientes por país.
- **Descomposición (Oaxaca-Blinder, referencia argentina):**
  media(ln *P*)_C − media(ln *P*)_A = (X̄_C − X̄_A)′β̂_A + X̄_C′(β̂_C − β̂_A)
  El lado izquierdo es la brecha (la prima) en puntos logarítmicos. El primer término es el componente de niveles y se reparte por categoría; el segundo es el componente de valoración (residual), donde X̄ incluye la constante. Se hacen dos descomposiciones: AR–BR y AR–EE. UU. Una contribución negativa indica un factor que reduce la brecha.
- **Lectura de los resultados:** cada contribución se informa en puntos logarítmicos y como proporción de la brecha total. Para brechas grandes, la diferencia de logaritmos subestima la diferencia porcentual, por lo que se convierte con (e^Δ − 1) × 100. Para presentar la prima en dólares, se informa también la brecha en USD/ha.
- **Precio potencial:** la ecuación argentina evaluada con los factores institucionales de Brasil, en las medias y como serie anual. El valor predicho está en logaritmos; se expresa en USD/ha como su exponencial y se compara con el precio predicho actual, no con el observado.
**Variables (Tabla 1 y apartados 2 a 4 del marco teórico):**
 
| Variable | Categoría | Término de Gordon | Medida | Signo esperado |
|---|---|---|---|---|
| Rendimiento | Productivo | *R* | t/ha de soja | + |
| Precipitación | Productivo | *R* | Desvío absoluto respecto de la media histórica | − |
| Adopción tecnológica | Productivo | *g* | Ventas de maquinaria por cada 1.000 ha sembradas | + |
| Flete | Productivo | *R* | USD/t hasta el puerto | − |
| Precio de la soja | Económico | *R* | CBOT, común a los tres países | + |
| Tipo de cambio real | Económico | *R* | Moneda local por USD, al tipo de cambio del exportador | + en AR y BR, − en EE. UU. |
| Tasa real libre de riesgo | Económico | *r* | TIPS a 10 años, común a los tres países | − |
| Alícuota impositiva | Institucional | *R* | Alícuota sobre la soja (derechos de exportación en AR) | − |
| Riesgo soberano | Institucional | *r* | EMBI+ (EE. UU. = 0) | − |
| Brecha cambiaria | Institucional | *R* y *r* | CCL frente al tipo de cambio del exportador; solo AR | − |
| Crédito hipotecario | Institucional | *r* | Créditos hipotecarios al agro / PBI | + |
 
Pendiente: definir qué regresores entran también en logaritmos. La revisión integral sugiere la soja, la maquinaria y el crédito, que así se leen como elasticidades. Las tasas y los porcentajes (alícuota, EMBI, brecha cambiaria, TIPS) quedan en niveles.
 
**Cadena de coherencia:**
 
| Pregunta | Objetivo | Hipótesis | Marco teórico | Dónde se responde |
|---|---|---|---|---|
| Principal: ¿qué proporción de la brecha explica cada categoría? | General: estimar esa proporción, 2005–2025 | H1 | Apartados 1 a 5 | Componente de niveles por categoría, AR–BR y AR–EE. UU. |
| P1: ¿se capitalizan el rendimiento, la tecnología y la infraestructura? | O1: cuantificar esa capitalización | — | 2 | Coeficientes productivos por región |
| P2: ¿explican la soja y el tipo de cambio real las diferencias? | O2: determinar en qué medida | — | 3 | Coeficientes económicos; la soja aporta solo por valoración |
| P3: ¿cómo cambia el precio con una convergencia institucional con Brasil? | O3: estimar el precio potencial | — | 4 y 5 | Ecuación argentina con los factores institucionales de Brasil |
 
---
 
## Estructura sección por sección
 
Estado al 13/10/2026: ✅ escrito · ⚠️ escrito con un punto abierto · ⬜ por escribir (es el 60% restante).
 
### 0. Elementos preliminares (fuera de las 25–30 pp.)
 
- **0.1 Carátula:** logo UADE, título, integrantes, tutor, carrera, materia, cuatrimestre y año, fecha de entrega.
- **0.2 Índice:** automático, con índice de tablas y de figuras (hay al menos la Tabla 1 y la Figura 1). ⚠️ Actualizar la numeración de páginas.
- **0.3 Resumen y Abstract** ⚠️: el porqué de la brecha, el modelo por país, la descomposición, la brecha leída como prima y el precio potencial. Cinco palabras clave en cada idioma. Tiene 158 palabras y el máximo es 150. En el documento final hay que sumar una oración con los resultados principales y la conclusión sobre H1.
### 1. Introducción ⚠️ (2–3 pp. junto con Planteamiento, Preguntas y Justificación)
 
- Las dos etapas del trabajo: primero cómo se forma el precio por hectárea en los tres países y después, a partir de esa composición, las razones de la brecha.
- El método en una línea: modelo de determinantes por país (Just & Miranowski, 1993) y descomposición en niveles y valoración. La brecha total es la prima.
- El encuadre en Finanzas: valuación de activos reales bajo riesgo en economías emergentes y oportunidad de inversión con el escenario de convergencia.
- Los antecedentes de la elección del tema (vínculo de los autores con el sector).
- **Pendiente:** el párrafo de estructura del documento (incluir Resultados y Conclusiones) y quitar el marcador «[EN DESARROLLO]».
- No anticipar resultados.
### 2. Planteamiento del problema ⚠️
 
- Precio y valor; alcance (tierra sojera, tres regiones, 2005–2025, la soja como cultivo de referencia).
- La brecha: zona núcleo USD 17.000/ha (CAIR en Reinke, 2025), Mato Grosso USD 22.400 (Colussi et al., 2026), Iowa USD 28.500 (Chandio, 2025). Conviene agregar la brecha también en la escala del modelo (puntos logarítmicos).
- **Figura 1:** evolución del precio por hectárea, 2005–2025, con nota APA. Pendiente: completarla con las series que alimentan la regresión.
- Explicación fragmentaria en la literatura, que el TIF integra.
- Problema y variables. **Actualizar:** la variable dependiente pasa a ser el logaritmo del precio por hectárea en USD; las explicativas siguen siendo las tres categorías.
- **2.1 Preguntas de investigación** ✅: la principal y P1 a P3 (ver la cadena de coherencia).
### 3. Estado del conocimiento ✅
 
- Dos revisiones (Deaton & Lawley, 2022; Tavares et al., 2022) y ocho antecedentes: Maito (2018); Just y Miranowski (1993); Malassise et al. (2015); Marques y Telles (2023); Villena y Marchetti (2003); Marcolini Tulliani et al. (2015); D'Argensio y Laurin (2009); Yang et al. (2017).
- El vacío son cuatro piezas sin articular: la comparación trilateral se detuvo en la renta; los modelos de determinantes son país por país; el traslado del riesgo soberano se midió en inmuebles; la convergencia se estudió dentro de un país.
- Enfoque en positivo: la literatura aborda el tema por partes y el TIF lo integra.
### 4. Justificación ✅
 
Valor teórico, relevancia social (productores, inversores, sector público), implicaciones prácticas (el escenario de convergencia) y utilidad metodológica (Oaxaca-Blinder, de la economía laboral, aplicada a la tierra agrícola).
 
### 5. Objetivos ✅
 
- **General:** estimar, mediante un modelo econométrico del precio de la tierra en cada país, qué proporción de la brecha de precio por hectárea con Brasil y Estados Unidos explican los factores productivos, los económicos y los institucionales, en 2005–2025.
- **Particulares:** O1 (productivos), O2 (económicos), O3 (precio potencial).
- Pendiente de decidir: la propuesta de pasar a cinco objetivos particulares (agregar uno de descomposición y uno institucional).
### 6. Hipótesis ✅
 
- **H1:** los factores institucionales explican una proporción mayor de la brecha de precio por hectárea entre la tierra argentina y la de Brasil y la de Estados Unidos que los factores productivos y los económicos, según el componente de niveles de su descomposición.
- **Cómo se contrasta:** comparando la parte del componente de niveles de cada categoría en las dos descomposiciones.
- Sugerencia de la revisión, para fijar en Metodología: H1 se confirma si se cumple frente a los dos comparables, se confirma parcialmente si se cumple frente a uno, y se rechaza en los demás casos.
### 7. Marco teórico ✅ (la sección más extensa, junto con el Estado del conocimiento: 8–12 pp.)
 
- **7.1 Fundamento:** precio y valor; Gordon, *P* = *R*₁ / (*r* − *g*), con la Tabla 1 (término → concepto → factores → variables); Gordon como fundamento y no como método; precio ≠ valor como norma (Lence, 2014; Falk & Lee, 1998); definición de la prima de riesgo implícita (toda la brecha) y su diferencia con la prima por riesgo país.
- **7.2 Factores productivos:** renta de la tierra, rendimiento, desvío de la precipitación, adopción tecnológica y flete.
- **7.3 Factores económicos:** precio de la soja (CBOT), tipo de cambio real y tasa real libre de riesgo (TIPS).
- **7.4 Factores institucionales:** capitalización de la política agrícola, alícuota, riesgo soberano, controles cambiarios y crédito hipotecario rural.
- **7.5 Del precio a la brecha** ⚠️: MCO, confiabilidad, Oaxaca-Blinder con referencia argentina, lectura de los componentes y la fórmula. Queda un marcador al final. **Actualizar:** el texto del 40% dice «precio en USD/ha, sin logaritmos». Hay que reemplazarlo por el logaritmo del precio y reescribir la lectura: coeficientes como cambios porcentuales aproximados, brecha en puntos logarítmicos, conversión (e^Δ − 1) × 100, fórmula con media(ln *P*) y un ejemplo numérico hipotético.
- **Marco conceptual:** la cátedra lo pide (definición operativa de cada concepto con fuente). Hoy no es una sección propia. Opciones: agregarlo como 7.6 o como un glosario al cierre del marco teórico. Conceptos: precio por hectárea, brecha de precio, prima de riesgo implícita, prima por riesgo país, componente de niveles, componente de valoración, precio potencial, tasa de capitalización, riesgo soberano, derechos de exportación, brecha cambiaria, crédito hipotecario rural, convergencia institucional.
### 8. Metodología ⚠️ (3–5 pp.; al 40% tenía unas 230 palabras)
 
**Ya escrito:** enfoque y diseño (cuantitativo, correlacional-explicativo, no experimental, longitudinal); datos (series anuales de Bloomberg y organismos oficiales); estimación (MCO por región y pruebas); descomposición (Oaxaca-Blinder detallada, referencia argentina); precio potencial; software (R, *oaxaca*). **Actualizar** la variable dependiente al logaritmo del precio.
 
**Falta para el documento final:**
1. **Fuentes por variable**, con la periodicidad de cada una, incluida la fuente única del precio en cada región.
2. **Los 8 módulos BMC de Bloomberg**, con qué serie se obtiene de cada uno.
3. **Tabla de operacionalización:** variable, definición conceptual, definición operacional, indicador, fuente, y si entra en logaritmos o en niveles.
4. **Especificación en logaritmos:** ln *P* en dólares corrientes o constantes; qué regresores van en logaritmos; cómo se convierten los resultados a porcentaje y a USD/ha; cómo se obtiene el precio potencial como exponencial del valor predicho.
5. **Pruebas con nombre:** por ejemplo ADF/KPSS, VIF, Breusch-Godfrey y Breusch-Pagan, y qué se hace si fallan.
6. **Tratamiento de los grados de libertad:** son 21 observaciones frente a 11 regresores más la constante. Justificar cómo se aborda.
7. **Variables comunes y nulas:** la soja y los TIPS son iguales en los tres países, así que aportan cero al componente de niveles. La alícuota, el EMBI y la brecha cambiaria valen cero en algún comparable. Declarar cómo se estiman y cómo se asignan.
8. **Regla de verificación de H1** y, si se adopta, sensibilidad del precio potencial a una convergencia parcial (por ejemplo, 25%, 50% y 100%).
9. Quitar el marcador «[EN DESARROLLO]».
### 9. Desarrollo y resultados ⬜ (8–12 pp.)
 
**Qué debe incluir:**
1. **Datos:** estadística descriptiva de cada variable por región (media, mediana, desvío, mínimo, máximo) y la Figura 1 completa. Series del precio por hectárea en USD/ha y de los principales regresores.
2. **Estimación por región:** una tabla con los coeficientes de las tres regresiones de ln *P* (coeficiente, error estándar robusto, significatividad, *R*², *n*) y el resultado de las pruebas de diagnóstico. Comparar los signos obtenidos con los esperados de la Tabla 1 y leer cada coeficiente como cambio porcentual o elasticidad.
3. **Factores productivos (P1, O1):** cuánto se capitalizan el rendimiento, la tecnología y el flete en cada país, según los coeficientes.
4. **Factores económicos (P2, O2):** sensibilidad del precio a la soja, al tipo de cambio real y a la tasa libre de riesgo en cada país. Recordar que la soja aporta solo por valoración.
5. **Descomposición:** dos tablas (AR–BR y AR–EE. UU.) con la brecha total (la prima) en puntos logarítmicos, en % (e^Δ − 1) y en USD/ha; el componente de niveles por categoría y por variable, en puntos logarítmicos y como % de la brecha; y el componente de valoración agregado.
6. **Contraste de H1:** comparar las contribuciones por categoría frente a cada comparable y aplicar la regla de verificación.
7. **Precio potencial (P3, O3):** el precio en las medias y la serie anual, en USD/ha como exponencial del valor predicho, comparados con el precio predicho actual de la zona núcleo y con el de Mato Grosso, y la sensibilidad si se adopta.
**Reglas:** cada dato con fuente y fecha; tablas y figuras en APA 7 (número en negrita, título en cursiva, solo líneas horizontales, Nota debajo, indicando en la Nota cuándo las cifras están en logaritmos); interpretación mínima, porque la profunda va en las Conclusiones; un supuesto propio se marca como tal; si falta un resultado, se deja el hueco [dato, fuente] y no se inventa.
 
### 10. Conclusiones ⬜ (2–3 pp.)
 
1. **Respuesta a la pregunta principal:** qué proporción de la brecha (la prima) explica cada categoría frente a Brasil y frente a Estados Unidos.
2. **Respuestas a P1, P2 y P3**, directas y sin repetir las tablas.
3. **H1:** se confirma, se confirma parcialmente o se rechaza, con referencia a los resultados. Un rechazo no invalida el trabajo, pero hay que explicar por qué.
4. **Cumplimiento del objetivo general y de O1 a O3.**
5. **Aporte:** para la literatura (la integración de las cuatro piezas del vacío) y para productores, inversores y el sector público.
6. **Limitaciones:** pocas observaciones y grados de libertad; posible no estacionariedad; endogeneidad de la maquinaria y del crédito; variables nulas en los comparables, que caen enteras en el componente de niveles; valoración residual que incluye la constante; extrapolación fuera de la muestra en la convergencia; la descomposición se hace en medias de logaritmos y su traducción a USD/ha es aproximada; Iowa como proxy del Corn Belt; Ley 26.737 y mercados de futuros fuera del modelo; Gordon aplicado a un activo ilíquido.
7. **Líneas futuras:** panel subnacional; otra referencia para la descomposición (*pooled*); otros cultivos, países o activos reales; la tasa de descuento implícita como medida alternativa de la prima.
**Reglas:** sin datos nuevos ni citas nuevas (salvo excepción justificada); autocrítica; anticipar las objeciones del jurado.
 
### 11. Referencias ✅ (completar en el final)
 
34 entradas en APA 7, en orden alfabético y todas citadas. Para el final:
- agregar las fuentes de datos (precio por región, J. P. Morgan, BCRA, BCB, FRED, USDA, OCDE) y R y el paquete *oaxaca*;
- completar los DOI pendientes;
- mantener la correspondencia uno a uno entre texto y lista.
### 12. Anexos ⬜
 
Numerados (Anexo A, B…) y citados desde el cuerpo: tablas de datos completas, salidas o capturas de los 8 módulos BMC, código de R comentado, pruebas de diagnóstico completas, tablas de sensibilidad y glosario si hace falta.
 
---
 
## Puntos abiertos
 
- **Forma (del artifact al 13/10):** el Resumen tiene 158 palabras (máximo 150); quedan tres «[EN DESARROLLO]» (Introducción, apartado 7.5 y Metodología); actualizar el índice; decidir si va [*sic*] en la cita de Marcolini Tulliani.
- **Logaritmos (11/11):** reemplazar «USD/ha, sin logaritmos» en el apartado 7.5, el Planteamiento y la Metodología del doc; actualizar el artifact; decidir dólares corrientes o constantes y qué regresores van en logaritmos.
- **Variables:** tipo de cambio real con fuentes por país o el índice del BIS; alícuota propia o la de la OCDE; EMBI+ o EMBI Global (J. P. Morgan dejó de actualizar el EMBI+ de Argentina el 30/09/2025).
- **Diseño:** 2005–2025 son 21 años, definir si se mantiene; propuesta de cinco objetivos particulares; vínculo de los autores con el agro en la Introducción; decidir si la Ley 26.737 queda fuera del modelo; orden de las secciones (consultar al tutor).
---
 
## Entregables
 
| Entrega | Contenido | Estado |
|---|---|---|
| 1. Ficha técnica | Tema, título, preguntas, objetivos, hipótesis, marco preliminar, 10 referencias | Aprobada; doc aparte |
| 2. Avance 40% | Carátula, índice, Resumen, Introducción a Marco teórico, Metodología avanzada, Referencias | Escrito; quedan los puntos de forma |
| 3. Trabajo final 100% | Todo lo anterior + Metodología completa + Desarrollo y resultados + Conclusiones + Anexos + Referencias definitivas (20 o más) + revisión integral de APA y coherencia | Por escribir |
 
---
 
## Criterios de evaluación final
 
1. **Estilo:** tercera persona, vocabulario técnico, pasado para los estudios citados y presente para el propio trabajo. Que se entienda el objetivo sin leer todo y que se vea cómo se verifica la hipótesis.
2. **Equilibrio:** el marco de referencia es la sección más extensa.
3. **Ordenamiento:** transiciones entre secciones.
4. **Secuencia:** sin lagunas ni repeticiones.
5. **Exposición oral:** claridad, manejo del tiempo y materiales.
## Coherencia estructural obligatoria
 
```
Pregunta principal ↔ H1 ↔ Objetivo general ↔ Componente de niveles por categoría
P1 ↔ O1 ↔ Apartado 7.2 ↔ Coeficientes productivos
P2 ↔ O2 ↔ Apartado 7.3 ↔ Coeficientes económicos
P3 ↔ O3 ↔ Apartados 7.4 y 7.5 ↔ Precio potencial
                    ↓
               Conclusiones
```
 
Cada vez que se modifica uno de estos elementos, revisar los demás y anotarlo en registro_de_cambios_TIF.md.
 
---
 
*Basado en el material de la cátedra de TIF (Lic. Karina Díaz, UADE, 2C 2026) y en el artifact «Mapa del TIF al 40%».*