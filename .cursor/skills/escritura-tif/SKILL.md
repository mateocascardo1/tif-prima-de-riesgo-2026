---
name: escritura-tif
description: >
  Redacta y edita el Trabajo de Integración Final en Google Docs con español
  académico y APA 7, según las reglas de cátedra del grupo. Usar al escribir,
  reescribir, citar o corregir el TIF, un capítulo, el resumen, el abstract o
  la lista de referencias en un Google Doc. No usar para el pipeline de R ni
  para estimar el modelo.
---

# Escritura del TIF

El texto que entra al Google Doc es una tesis. El chat con el grupo no lo es.

## Antes de escribir

Leer, en este orden, y no reemplazar uno por otro:

1. El Google Doc que se va a editar. El documento de producción es
   «TIF - 100% - PROD»
   (`1AcCzGxOPVkXOQXXZK_pUpexJWgVVf_9V_b7E4cdHhOk`). Los borradores no se
   editan salvo pedido explícito.
2. `rules/# Registro de cambios — TIF.md`, la entrada más nueva.
3. `rules/# Estructura del Trabajo Integrador Fina.md`, la sección que se
   escribe y la cadena de coherencia del final.
4. `rules/# Reglas de escritura — TIF.md`.
5. `rules/# Reglas generales de trabajo — TIF.sty`.

Cuando dos fuentes discrepan, manda la que está más arriba. El documento de
producción no se pisa en silencio: se señala la diferencia.

La entrada del 11/11/2026 corrige a la estructura de octubre en esto: precio
en dólares constantes de 2025, diésel en lugar de flete, entran la tasa real
local y la urea, salen la brecha cambiaria y el crédito, EMBI Global
Diversified, tipo de cambio real del BIS (un aumento es apreciación), Iowa,
Oaxaca calculada a mano, y las medias de la hipótesis en 2021–2025. No volver
a la versión de octubre al redactar Metodología, Resultados o Conclusiones.

## Qué skill leer según la tarea

Las skills bajadas cubren el APA genérico. No mandan sobre la cátedra.

| Tarea | Leer |
|---|---|
| Hueco de formato que las reglas locales no cubren | `.agents/skills/apa7/SKILL.md` |
| Armar o corregir una referencia | `.agents/skills/apa-style-citation/SKILL.md` |
| Tono, cobertura y prudencia de una afirmación | `.agents/skills/academic-writing-style/SKILL.md`, solo la sección de estilo |
| Cita nueva, DOI o «¿el paper dice esto?» | `.agents/skills/ref-verify/SKILL.md` antes de insertarla |

No usar `academic-writing-style` para reordenar el trabajo en cinco capítulos
de revista. La estructura es la del archivo de estructura. El idioma del
documento es español.

## La cátedra gana donde el APA genérico dice otra cosa

- Fuente Arial 11 en el cuerpo. Títulos numerados (1., 1.1., 1.1.1.) en Arial
  12 negrita.
- Hoja A4. Márgenes de 2,54 cm. Texto justificado. Sangría de primera línea
  de 1,27 cm. Sin espacio extra entre párrafos.
- Interlineado 1,5 en todo el documento. La cita de 40 palabras o más va en
  bloque, interlineado simple, sangría de 1,27 cm a izquierda y derecha, sin
  comillas.
- En el paréntesis, «&». Fuera del paréntesis, «y». Tres autores o más: et al.
  desde la primera mención.
- Autor institucional: nombre completo y sigla entre corchetes la primera vez;
  después, solo la sigla.
- Cita en otro idioma: texto original, después la traducción entre corchetes,
  después (autor, año, página). No alcanza con la traducción.
- La lista se titula «Referencias». Cada cita del texto tiene su entrada, y al
  revés. No usar notas al pie para citar.
- Del uno al nueve, en palabras. Del 10 en adelante, en cifras. Porcentajes,
  unidades, fechas y estadísticos, siempre en cifras. Miles con punto y
  decimales con coma (USD 17.000; 0,52).
- Términos en otro idioma y las letras de una variable van en cursiva
  (*farmland*, *t*, *R*²).
- Tablas y figuras: número en negrita, título en cursiva, solo líneas
  horizontales, y debajo una nota con la fuente. Si la cifra está en
  logaritmos, la nota lo dice.
- Tercera persona o primera del plural. Pretérito para un trabajo ya publicado.
  Presente para el propio trabajo y para una afirmación general.
- Un párrafo, una idea. Cada afirmación relevante tiene cita, dato con fecha
  o un razonamiento propio marcado como tal.

## Qué no se inventa

- Una cifra sin fuente identificable y fecha queda como `[dato, fuente]`, o
  se marca como supuesto propio. No se presenta el supuesto como dato.
- Una fuente que no se pudo verificar no entra. Se deja pendiente o se cambia
  por una fuente confirmada. No se cita de memoria.
- Una cita de cita se declara como tal, en APA 7.
- En Resultados no se interpreta de más: la lectura profunda va en
  Conclusiones. En Conclusiones no aparecen datos nuevos ni citas nuevas,
  salvo una excepción justificada.
- El marco teórico necesita al menos 10 citas textuales, una o dos en inglés.

## Coherencia

Si se toca la pregunta, la hipótesis, un objetivo o el marco, se revisan los
cuatro juntos. La prima de riesgo implícita es toda la brecha de precio por
hectárea, no el componente de valoración. Ese componente es un residual y no
se atribuye a un factor.

## Cómo editar el Google Doc

No se borra texto. Cada cambio es una sugerencia numerada C1, C2, C3, en
orden de aparición.

1. Leer el documento entero, o la sección completa si es largo.
2. Tocar solo el tramo pedido.
3. El texto que se propone sacar queda en el documento y se subraya. No se
   tacha y no se elimina. La sigla va al inicio del tramo subrayado, en azul
   y en negrita, para poder encontrarlo: `[M12]` y después el subrayado.
4. El texto nuevo se inserta al lado, con la sigla entre corchetes, en azul
   y en negrita. Ejemplo: el original «Tres» queda subrayado y al lado entra
   « [C1] Dos».
5. No anunciar la decisión. No se escribe «la brecha se toma en cuenta en la
   ecuación» ni ninguna oración que solo declare que una variable entra. Si
   entra, se reescribe el contenido o se propone sacar la frase que la
   excluía. La sigla no va pegada a una frase que no agrega información.
6. Volver a leer el tramo y confirmar que el original sigue ahí, subrayado, y
   que la sugerencia está al lado.
7. En el chat, decir en castellano común qué sección cambió y qué decisión se
   aplicó. El párrafo académico vive en el documento.

Borradores ya usados, que no son el documento de producción:

- Metodología: `1nJkqooi6QodCN6_ac--uT61DV5tgnDuE1UEOD1byXb0`
- Resultados: `1zm5gpEGd-WLeb6uL7B4kvboOQE5ocMK13RsPzwJrhQQ`
- Conclusiones: `1ZCgOJYKFsyaB1KcHAfKU9d6oO8d9DkfEfGvXQ6UyGMM`

Si no se puede abrir el documento, devolver el párrafo listo para pegar y
decirlo. No afirmar que el doc quedó editado.
