# Arma 03_resultados/figuras.html con las seis figuras del trabajo.
# Los números salen del panel y de las tablas de la corrida vigente.
# No estima nada: si cambia la corrida, hay que volver a correr este script.

import csv
from pathlib import Path

RAIZ = Path(__file__).resolve().parents[1]
PANEL = RAIZ / "02_curados" / "panel.csv"
CATS = RAIZ / "03_resultados" / "tabla_descomposicion_por_categoria.csv"
RIESGO = RAIZ / "03_resultados" / "tabla_riesgo_brasil.csv"
SALIDA = RAIZ / "03_resultados" / "figuras.html"

IOWA = "#E36A2E"
ZONA = "#2C8C4A"
MATO = "#3B6FD6"
SOJA = "#2A2A2A"
TINTA = "#37352F"
SUAVE = "#8A8880"
GRID = "#EEEDEB"
VENTANA = "#F4F4F2"
CERO = "#C8C6C0"

FONT = "-apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif"


def leer(path):
    with path.open(newline="") as f:
        return list(csv.DictReader(f))


def num(texto):
    if texto is None or texto == "":
        return None
    return float(texto)


def miles(valor, dec=0):
    if dec == 0:
        return f"{int(round(valor)):,}".replace(",", ".")
    texto = f"{valor:,.{dec}f}"
    return texto.replace(",", "X").replace(".", ",").replace("X", ".")


def logpts(valor, signo=True):
    redondo = round(valor, 3)
    if abs(redondo) < 0.0005:
        return "0,000"
    cuerpo = f"{abs(redondo):.3f}".replace(".", ",")
    if not signo:
        return ("−" if redondo < 0 else "") + cuerpo
    if redondo < 0:
        return "−" + cuerpo
    return "+" + cuerpo


def esc(texto):
    return (texto.replace("&", "&amp;").replace("<", "&lt;")
            .replace(">", "&gt;").replace('"', "&quot;"))


def serie(filas, pais, columna):
    out = []
    for fila in filas:
        if fila["pais"] != pais:
            continue
        valor = num(fila[columna])
        if valor is None:
            continue
        out.append((int(fila["anio"]), valor))
    return sorted(out)


def repeler(etiquetas, minimo=18):
    """Separa etiquetas que comparten la misma x. Cada item: y, texto, color."""
    if not etiquetas:
        return []
    orden = sorted(etiquetas, key=lambda e: e["y"])
    ys = [e["y"] for e in orden]
    for _ in range(12):
        for i in range(1, len(ys)):
            if ys[i] - ys[i - 1] < minimo:
                medio = (ys[i] + ys[i - 1]) / 2
                ys[i - 1] = medio - minimo / 2
                ys[i] = medio + minimo / 2
    techo = min(e["y"] for e in orden) - 8
    piso = max(e["y"] for e in orden) + 8
    # No sacar las etiquetas del área de dibujo si el llamador pasó límites.
    if "piso" in orden[0]:
        piso = orden[0]["piso"]
        techo = orden[0]["techo"]
        if ys[0] < techo:
            ys[0] = techo
        for i in range(1, len(ys)):
            if ys[i] < ys[i - 1] + minimo:
                ys[i] = ys[i - 1] + minimo
        if ys[-1] > piso:
            ys[-1] = piso
            for i in range(len(ys) - 2, -1, -1):
                if ys[i] > ys[i + 1] - minimo:
                    ys[i] = ys[i + 1] - minimo
    for e, y in zip(orden, ys):
        e["y2"] = y
    return orden


class Lienzo:
    def __init__(self, ancho, alto):
        self.ancho = ancho
        self.alto = alto
        self.partes = []
        self.partes.append(
            f'<svg viewBox="0 0 {ancho} {alto}" width="100%" '
            f'role="img" xmlns="http://www.w3.org/2000/svg">'
        )
        self.partes.append(
            "<defs>"
            '<pattern id="hatch" width="7" height="7" patternUnits="userSpaceOnUse" '
            'patternTransform="rotate(32)">'
            '<rect width="7" height="7" fill="#E7EEF8"/>'
            '<line x1="0" y1="0" x2="0" y2="7" stroke="#3B6FD6" stroke-width="2"/>'
            "</pattern>"
            '<pattern id="embi" width="7" height="7" patternUnits="userSpaceOnUse" '
            'patternTransform="rotate(32)">'
            '<rect width="7" height="7" fill="#F7F6F4"/>'
            '<line x1="0" y1="0" x2="0" y2="7" stroke="#D5D3CE" stroke-width="2"/>'
            "</pattern>"
            "</defs>"
        )
        self.partes.append(f'<rect width="{ancho}" height="{alto}" fill="#fff"/>')

    def texto(self, x, y, contenido, **estilo):
        tam = estilo.get("tam", 13)
        peso = estilo.get("peso", 400)
        fill = estilo.get("fill", TINTA)
        ancla = estilo.get("ancla", "start")
        extra = estilo.get("extra", "")
        self.partes.append(
            f'<text x="{x:.1f}" y="{y:.1f}" fill="{fill}" font-size="{tam}" '
            f'font-weight="{peso}" font-family="{FONT}" text-anchor="{ancla}" '
            f'{extra}>{esc(contenido)}</text>'
        )

    def titulo(self, contenido):
        self.texto(self.ancho / 2, 28, contenido, tam=16, peso=650, ancla="middle")

    def leyenda(self, items, y):
        # items: (color, nombre, dash o None)
        ancho_item = []
        for color, nombre, dash in items:
            ancho_item.append(22 + len(nombre) * 6.5 + 18)
        total = sum(ancho_item)
        x = (self.ancho - total) / 2
        for (color, nombre, dash), w in zip(items, ancho_item):
            y1 = y
            extra = f'stroke-dasharray="{dash}"' if dash else ""
            self.partes.append(
                f'<line x1="{x:.1f}" y1="{y1:.1f}" x2="{x + 18:.1f}" y2="{y1:.1f}" '
                f'stroke="{color}" stroke-width="2.4" stroke-linecap="round" {extra}/>'
            )
            self.texto(x + 24, y1 + 4, nombre, tam=12.5, fill=TINTA)
            x += w

    def cerrar(self):
        self.partes.append("</svg>")
        return "\n".join(self.partes)


def grafico_lineas(titulo, series, eje_y, y0, y1, cortes,
                   eje_y2=None, y2_0=None, y2_1=None, cortes2=None,
                   ventana=True, banda=None, ventana_arriba=False,
                   alto=528, ancho=1000):
    # Con eje derecho hace falta lugar para dos columnas de valores: la de
    # la tierra, pegada al gráfico, y la de la soja, más afuera.
    L, R, T, B = 78, (196 if eje_y2 else 108), 86, 46
    pw, ph = ancho - L - R, alto - T - B
    svg = Lienzo(ancho, alto)
    svg.titulo(titulo)

    leyenda = []
    for s in series:
        leyenda.append((s["color"], s["nombre"], s.get("dash")))
    svg.leyenda(leyenda, 54)

    def x_de(anio):
        return L + (anio - 2005) / 20 * pw

    def y_de(valor, a, b):
        return T + (b - valor) / (b - a) * ph

    if ventana:
        x_a = x_de(2020.5)
        x_b = min(x_de(2025.5), L + pw)
        svg.partes.append(
            f'<rect x="{x_a:.1f}" y="{T}" width="{x_b - x_a:.1f}" height="{ph}" '
            f'fill="{VENTANA}"/>'
        )
        y_cartel = T + 18 if ventana_arriba else T + ph - 10
        svg.texto((x_a + x_b) / 2, y_cartel, "2021–2025",
                  tam=11, fill="#B0AEA6", ancla="middle")

    if banda:
        x_a = x_de(banda[0] - 0.5)
        x_b = x_de(banda[1] + 0.5)
        svg.partes.append(
            f'<rect x="{x_a:.1f}" y="{T}" width="{x_b - x_a:.1f}" height="{ph}" '
            f'fill="url(#embi)"/>'
        )
        svg.texto((x_a + x_b) / 2, T + 16, "EMBI+", tam=11, fill=SUAVE,
                  ancla="middle", peso=600)

    for corte in cortes:
        if corte < y0 - 1e-9 or corte > y1 + 1e-9:
            continue
        y = y_de(corte, y0, y1)
        svg.partes.append(
            f'<line x1="{L}" y1="{y:.1f}" x2="{L + pw}" y2="{y:.1f}" '
            f'stroke="{GRID}" stroke-width="1"/>'
        )
        svg.texto(L - 10, y + 4, cortes[corte] if isinstance(cortes, dict) else fmt_corte(corte),
                  tam=11.5, fill=SUAVE, ancla="end")

    if eje_y2:
        for corte in cortes2:
            if corte < y2_0 - 1e-9 or corte > y2_1 + 1e-9:
                continue
            y = y_de(corte, y2_0, y2_1)
            svg.texto(L + pw + 132, y + 4, fmt_corte(corte, dinero=True),
                      tam=11.5, fill=SUAVE, ancla="start")

    # Eje x
    for anio in range(2005, 2026, 2):
        svg.texto(x_de(anio), T + ph + 22, str(anio), tam=11.5, fill=SUAVE,
                  ancla="middle")

    svg.partes.append(
        f'<text transform="translate(18 {T + ph / 2}) rotate(-90)" '
        f'fill="{SUAVE}" font-size="12" font-family="{FONT}" '
        f'text-anchor="middle">{esc(eje_y)}</text>'
    )
    if eje_y2:
        svg.partes.append(
            f'<text transform="translate({ancho - 16} {T + ph / 2}) rotate(90)" '
            f'fill="{SUAVE}" font-size="12" font-family="{FONT}" '
            f'text-anchor="middle">{esc(eje_y2)}</text>'
        )

    etiquetas = []
    for s in series:
        puntos = s["puntos"]
        if not puntos:
            continue
        d = " ".join(
            ("M" if i == 0 else "L") + f"{x_de(a):.1f},{y_de(v, s.get('y0', y0), s.get('y1', y1)):.1f}"
            for i, (a, v) in enumerate(puntos)
        )
        dash = f'stroke-dasharray="{s["dash"]}"' if s.get("dash") else ""
        ancho_linea = s.get("ancho", 2.35)
        svg.partes.append(
            f'<path d="{d}" fill="none" stroke="{s["color"]}" '
            f'stroke-width="{ancho_linea}" stroke-linejoin="round" '
            f'stroke-linecap="round" {dash}/>'
        )
        for anio in s.get("marcas", []):
            valor = dict(puntos).get(anio)
            if valor is None:
                continue
            cx, cy = x_de(anio), y_de(valor, s.get("y0", y0), s.get("y1", y1))
            svg.partes.append(
                f'<circle cx="{cx:.1f}" cy="{cy:.1f}" r="3.4" fill="#fff" '
                f'stroke="{s["color"]}" stroke-width="1.6"/>'
            )
        ultimo_a, ultimo_v = puntos[-1]
        yy = y_de(ultimo_v, s.get("y0", y0), s.get("y1", y1))
        etiquetas.append({
            "y": yy, "texto": s["etiqueta"], "color": s["color"],
            "techo": T + 8, "piso": T + ph - 4,
            # La serie del eje derecho va en una columna propia, para que
            # no se siente encima del valor de la tierra.
            "columna": 1 if "y0" in s else 0,
        })

    for columna, x_etiq in ((0, L + pw + 8), (1, L + pw + 78)):
        grupo = [e for e in etiquetas if e["columna"] == columna]
        for e in repeler(grupo, minimo=18):
            svg.texto(x_etiq, e["y2"] + 4, e["texto"], tam=13, peso=650,
                      fill=e["color"])

    return svg.cerrar()


def fmt_corte(valor, dinero=False):
    if dinero or abs(valor) >= 1000 or valor == 0 and dinero:
        return "$" + miles(valor) if valor != 0 else "$0"
    if isinstance(valor, float) and not valor.is_integer():
        return miles(valor, 1)
    if abs(valor) >= 100:
        return miles(valor)
    # log points or rates
    if abs(valor) < 10:
        texto = f"{valor:.1f}".replace(".", ",")
        if texto == "-0,0":
            return "0"
        return texto.replace("-", "−")
    return miles(valor)


def grafico_barras(titulo, filas, x0, x1, cortes, eje_x):
    ancho, alto = 1000, 520
    L, R, T, B = 228, 86, 78, 58
    pw, ph = ancho - L - R, alto - T - B
    svg = Lienzo(ancho, alto)
    svg.titulo(titulo)

    n = len(filas)
    # Huecos entre grupos: niveles sueltos, total, valoración+residuo, brecha.
    huecos = {3: 16, 4: 14, 6: 16}
    alto_fila = 28
    usado = n * alto_fila + sum(huecos.values())
    y_base = T + max(0, (ph - usado) / 2)

    def x_de(valor):
        return L + (valor - x0) / (x1 - x0) * pw

    posiciones = []
    y = y_base
    for i, fila in enumerate(filas):
        if i in huecos:
            y += huecos[i]
        posiciones.append(y + alto_fila / 2)
        y += alto_fila

    for corte in cortes:
        x = x_de(corte)
        svg.partes.append(
            f'<line x1="{x:.1f}" y1="{T}" x2="{x:.1f}" y2="{T + ph}" '
            f'stroke="{GRID}" stroke-width="1"/>'
        )
        svg.texto(x, T + ph + 22, fmt_eje_log(corte), tam=11.5, fill=SUAVE,
                  ancla="middle")

    cero = x_de(0)
    svg.partes.append(
        f'<line x1="{cero:.1f}" y1="{T}" x2="{cero:.1f}" y2="{T + ph}" '
        f'stroke="{CERO}" stroke-width="1.25"/>'
    )

    for fila, yc in zip(filas, posiciones):
        svg.texto(L - 14, yc + 4, fila["nombre"], tam=13,
                  peso=650 if fila.get("fuerte") else 450,
                  fill=TINTA, ancla="end")
        valor = fila["valor"]
        x = x_de(valor)
        y_barra = yc - 9
        relleno = fila["fill"]
        alto_b = 18
        if valor >= 0:
            svg.partes.append(
                f'<rect x="{cero:.1f}" y="{y_barra:.1f}" width="{max(x - cero, 1.2):.1f}" '
                f'height="{alto_b}" fill="{relleno}" rx="2"/>'
            )
            ancla_x = x + 8
            ancla = "start"
        else:
            svg.partes.append(
                f'<rect x="{x:.1f}" y="{y_barra:.1f}" width="{max(cero - x, 1.2):.1f}" '
                f'height="{alto_b}" fill="{relleno}" rx="2"/>'
            )
            ancla_x = x - 8
            ancla = "end"
        if abs(valor) < 0.03:
            ancla_x = cero + 8
            ancla = "start"
        svg.texto(ancla_x, yc + 4, fila["etiqueta"], tam=13, peso=650,
                  fill=TINTA, ancla=ancla)

    svg.texto(L + pw / 2, alto - 12, eje_x, tam=12, fill=SUAVE, ancla="middle")
    return svg.cerrar()


def fmt_eje_log(valor):
    if abs(valor) < 1e-9:
        return "0"
    texto = f"{valor:.2f}".rstrip("0").rstrip(".")
    if "." not in texto:
        texto += ",0" if False else ""
    texto = texto.replace(".", ",").replace("-", "−")
    return texto


def filas_categoria(tabla, comparable):
    claves = {
        "Niveles: factores productivos": "Niveles: productivos",
        "Niveles: factores económicos": "Niveles: económicos",
        "Niveles: factores institucionales": "Niveles: institucionales",
        "Componente de niveles (total)": "Niveles (total)",
        "Componente de valoración (residual, solo agregado)": "Valoración",
        "Residuo: parte de la brecha que la ecuación no explica en la ventana": "Residuo",
        "Brecha total (media de ln P del comparable menos Argentina)": "Brecha total",
    }
    fills = {
        "Niveles: productivos": "#D7DBE1",
        "Niveles: económicos": "#A9B1BB",
        "Niveles: institucionales": IOWA,
        "Niveles (total)": "#5E6772",
        "Valoración": "url(#hatch)",
        "Residuo": "#E6E8EB",
        "Brecha total": "#242424",
    }
    fuertes = {"Niveles (total)", "Brecha total"}
    por_nombre = {}
    for fila in tabla:
        if (fila["ventana"] == "2021-2025" and fila["rol"] == "principal"
                and fila["comparable"] == comparable
                and fila["componente"] in claves):
            por_nombre[claves[fila["componente"]]] = float(fila["contribucion_log"])
    orden = list(claves.values())
    salida = []
    for nombre in orden:
        valor = por_nombre[nombre]
        signo = nombre != "Brecha total"
        salida.append({
            "nombre": nombre,
            "valor": valor,
            "etiqueta": logpts(valor, signo=signo),
            "fill": fills[nombre],
            "fuerte": nombre in fuertes,
        })
    return salida


def main():
    panel = leer(PANEL)
    cats = leer(CATS)
    riesgo = leer(RIESGO)

    iowa = serie(panel, "Estados Unidos", "precio_ha_usd_2025")
    zona = serie(panel, "Argentina", "precio_ha_usd_2025")
    mato = serie(panel, "Brasil", "precio_ha_usd_2025")
    soja = serie(panel, "Argentina", "precio_soja_cbot_usd_t")
    embi_ar = serie(panel, "Argentina", "embi_pb")
    embi_br = serie(panel, "Brasil", "embi_pb")

    import math
    def diferencia(otra):
        base = dict(zona)
        vals = dict(otra)
        return [(a, math.log(vals[a]) - math.log(base[a]))
                for a in range(2005, 2026) if a in base and a in vals]

    gap_mato = diferencia(mato)
    gap_iowa = diferencia(iowa)

    cortes_precio = {v: ("$0" if v == 0 else "$" + miles(v))
                     for v in range(0, 35001, 5000)}
    cortes_soja = {v: ("$0" if v == 0 else "$" + miles(v))
                   for v in range(0, 801, 200)}

    fig1 = grafico_lineas(
        "Precio de la tierra agrícola y de la soja, 2005-2025",
        [
            {"nombre": "Iowa (EE. UU.)", "color": IOWA, "puntos": iowa,
             "etiqueta": miles(iowa[-1][1])},
            {"nombre": "Zona núcleo (Argentina)", "color": ZONA, "puntos": zona,
             "etiqueta": miles(zona[-1][1])},
            {"nombre": "Mato Grosso (Brasil)", "color": MATO, "puntos": mato,
             "etiqueta": miles(mato[-1][1])},
            {"nombre": "Soja, precio CBOT (eje der.)", "color": SOJA, "puntos": soja,
             "dash": "5 4", "ancho": 1.7, "etiqueta": miles(soja[-1][1]),
             "y0": 0, "y1": 800},
        ],
        "USD de 2025 por hectárea", 0, 35000, cortes_precio,
        eje_y2="Soja, USD por tonelada", y2_0=0, y2_1=800, cortes2=cortes_soja,
    )

    cortes_gap = {v / 10: None for v in range(-8, 10, 2)}
    # fmt uses the key; pass dict
    cortes_gap = {}
    for v in [-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8]:
        cortes_gap[v] = fmt_corte(v)

    fig2 = grafico_lineas(
        "Brecha de precio frente a la zona núcleo, 2005-2025",
        [
            {"nombre": "Iowa − zona núcleo", "color": IOWA, "puntos": gap_iowa,
             "etiqueta": logpts(gap_iowa[-1][1], signo=False)},
            {"nombre": "Mato Grosso − zona núcleo", "color": MATO, "puntos": gap_mato,
             "etiqueta": logpts(gap_mato[-1][1], signo=False)},
        ],
        "Diferencia de ln P, puntos logarítmicos", -0.85, 0.95, cortes_gap,
        alto=500,
    )
    # zero line is already a gridline if 0 is a tick. Good.

    fig3 = grafico_barras(
        "Brecha con Mato Grosso, medias de 2021-2025",
        filas_categoria(cats, "Brasil"),
        -0.40, 0.80,
        [-0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8],
        "Puntos logarítmicos (Mato Grosso − zona núcleo)",
    )
    fig4 = grafico_barras(
        "Brecha con Iowa, medias de 2021-2025",
        filas_categoria(cats, "Estados Unidos"),
        -1.10, 1.55,
        [-1, -0.5, 0, 0.5, 1, 1.5],
        "Puntos logarítmicos (Iowa − zona núcleo)",
    )

    def col(nombre):
        return [(int(f["anio"]), float(f[nombre])) for f in riesgo]

    pagado = col("precio_observado")
    ecuacion = col("precio_simulado_argentino")
    embi = col("precio_simulado_riesgo_brasil")
    inst = col("precio_simulado_institucional_brasil")
    dentro = [int(f["anio"]) for f in riesgo if f["extrapolacion"] == "no"]

    fig5 = grafico_lineas(
        "Precio de la zona núcleo bajo el bloque institucional de Brasil",
        [
            {"nombre": "Precio pagado", "color": ZONA, "puntos": pagado,
             "etiqueta": miles(pagado[-1][1]), "ancho": 2.5},
            {"nombre": "Ecuación argentina", "color": "#8B919A", "puntos": ecuacion,
             "dash": "5 4", "ancho": 1.8, "etiqueta": miles(ecuacion[-1][1])},
            {"nombre": "Con el EMBI de Brasil", "color": IOWA, "puntos": embi,
             "etiqueta": miles(embi[-1][1]), "marcas": dentro},
            {"nombre": "Sin retenciones y sin brecha", "color": MATO, "puntos": inst,
             "etiqueta": miles(inst[-1][1])},
        ],
        "USD de 2025 por hectárea", 10000, 32000,
        {v: ("$0" if v == 0 else "$" + miles(v)) for v in range(10000, 30001, 5000)},
        alto=548,
    )

    cortes_embi = {v: miles(v) for v in range(0, 3001, 500)}
    figb = grafico_lineas(
        "Riesgo país de Argentina y de Brasil, 2005-2025",
        [
            {"nombre": "Argentina", "color": IOWA, "puntos": embi_ar,
             "etiqueta": miles(embi_ar[-1][1]), "ancho": 2.5},
            {"nombre": "Brasil", "color": MATO, "puntos": embi_br,
             "dash": "5 4", "etiqueta": miles(embi_br[-1][1])},
        ],
        "Puntos básicos, promedio anual", 0, 3200, cortes_embi,
        banda=(2005, 2007), ventana_arriba=True,
        alto=500,
    )

    html = f"""<!DOCTYPE html>
<html lang="es">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Figuras — brecha de precio de la tierra</title>
<style>
:root {{ --tinta:#37352f; --suave:#787774; --linea:#e9e9e7; --fondo:#f7f6f4; }}
* {{ box-sizing: border-box; }}
body {{
  font-family: {FONT};
  color: var(--tinta); background: var(--fondo);
  max-width: 1080px; margin: 0 auto; padding: 3.2rem 1.6rem 5rem;
  line-height: 1.55; font-size: 16px;
  -webkit-font-smoothing: antialiased;
}}
h1 {{ font-size: 2rem; font-weight: 700; letter-spacing: -.02em; margin: 0 0 .35rem; }}
p.bajada {{ color: var(--suave); margin: 0 0 2.2rem; max-width: 68ch; }}
article {{
  background: #fff; border: 1px solid var(--linea); border-radius: 10px;
  padding: 1.1rem 1.1rem .4rem; margin: 0 0 1.6rem;
}}
figcaption {{ color: var(--suave); font-size: .92rem; padding: .15rem .35rem 1rem; max-width: 78ch; }}
figcaption strong {{ color: var(--tinta); font-weight: 600; }}
</style>
</head>
<body>
<h1>Figuras</h1>
<p class="bajada">Las seis figuras del trabajo, con la brecha cambiaria dentro de la ecuación argentina. La ventana gris es 2021-2025, que es donde se lee la brecha. La tierra está en dólares de 2025. La soja del primer gráfico es el precio CBOT en dólares corrientes, por eso tiene eje propio.</p>

<article>
<figure>
{fig1}
<figcaption><strong>Figura 1.</strong> Iowa cierra 2025 en 28.538 dólares de 2025 por hectárea, Mato Grosso en 22.403 y la zona núcleo en 17.500. La soja cierra en 383 dólares corrientes por tonelada. El eje izquierdo y el derecho no se leen juntos: uno es la tierra deflactada y el otro es el grano de cada año.</figcaption>
</figure>
</article>

<article>
<figure>
{fig2}
<figcaption><strong>Figura 2.</strong> Cuánto más cara es la hectárea del comparable que la de la zona núcleo, en puntos logarítmicos. Cero quiere decir el mismo precio. En 2025 Iowa está 0,489 por encima (un 63 %) y Mato Grosso 0,247 (un 28 %). La brecha que usa el trabajo no es la de 2025: es el promedio de 2021-2025, 0,639 frente a Iowa (un 89,5 %) y 0,478 frente a Mato Grosso (un 61,3 %).</figcaption>
</figure>
</article>

<article>
<figure>
{fig3}
<figcaption><strong>Figura 3.</strong> De qué está hecha la brecha con Mato Grosso en las medias de 2021-2025. Un número positivo agranda la brecha. Los niveles, sumados, van al revés (−0,090): con los fundamentos de Brasil la tierra argentina no saldría más barata. Casi toda la brecha queda en la valoración (0,565), que es lo que la ecuación no atribuye a diferencias de niveles. La brecha cambiaria entra en la barra naranja y, sola, aporta +0,140. La alícuota empuja al revés (−0,052) y el EMBI suma +0,029. Brasil y Estados Unidos no tienen serie de brecha: en esta cuenta su media vale 0 por convención, y eso no se lee como una brecha medida en cero.</figcaption>
</figure>
</article>

<article>
<figure>
{fig4}
<figcaption><strong>Figura 4.</strong> La misma cuenta frente a Iowa. La brecha total es 0,639, un 89,5 %. Los niveles van más fuerte en contra (−0,501), sobre todo por la maquinaria. La valoración es 1,132. La brecha cambiaria aporta los mismos +0,140 que frente a Brasil, porque el coeficiente es el argentino y la media del comparable también se toma en 0.</figcaption>
</figure>
</article>

<article>
<figure>
{fig5}
<figcaption><strong>Figura 5.</strong> Qué precio devolvería la ecuación argentina cada año. La verde es el precio pagado. La gris es la ecuación con los datos argentinos de ese año. La naranja cambia solo el EMBI por el de Brasil y deja la alícuota y la brecha en el valor argentino. El punto blanco marca los tres años en los que ese EMBI cae dentro del rango argentino (2005, 2015 y 2016); en el resto la naranja es una extrapolación. La azul, además, pone la alícuota y la brecha en cero. Cero de alícuota está fuera de las tasas que Argentina tuvo, así que esa línea entera es una extrapolación. En 2025 la azul cierra en 18.396, por encima de los 17.500 pagados.</figcaption>
</figure>
</article>

<article>
<figure>
{figb}
<figcaption><strong>Figura B1.</strong> EMBI en puntos básicos, promedio del año. La zona rayada de 2005 a 2007 es EMBI+, que se usa para empalmar el nivel de esos tres años. Argentina cierra 2025 en 752 y Brasil en 210.</figcaption>
</figure>
</article>
</body>
</html>
"""
    SALIDA.write_text(html, encoding="utf-8")
    print(SALIDA, SALIDA.stat().st_size)


if __name__ == "__main__":
    main()
