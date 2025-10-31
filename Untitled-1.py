#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Programa de consola para resolver:
  1) Sistemas de ecuaciones lineales (usando linsolve).
  2) Ecuación cuadrática ax^2 + bx + c = 0.
  3) Sistemas generales de ecuaciones (lineales o no) con solve.

Ejemplos de entrada (en el modo de sistemas):
  - Variables: x y
  - Ecuaciones (una por línea):
        2*x + 3*y = 5
        x - y = 1
  (Deja una línea en blanco para terminar)

Notas:
  - Usa punto decimal (ej: 3.5), no coma.
  - Puedes usar funciones: sin(x), cos(x), exp(x), sqrt(x), etc.
"""

from sympy import symbols, Eq, sympify, linsolve, solve, sqrt, S
from sympy.core.sympify import SympifyError

LINE = "—" * 60

def leer_variables():
    raw = input("Ingresa las variables separadas por espacios (ej: x y z): ").strip()
    if not raw:
        raise ValueError("No ingresaste variables.")
    nombres = [v for v in raw.replace(",", " ").replace(";", " ").split() if v]
    syms = symbols(" ".join(nombres))
    # Asegura que syms sea una tupla de símbolos incluso si es uno solo
    if not isinstance(syms, tuple):
        syms = (syms,)
    mapa = {str(s): s for s in syms}
    return syms, mapa

def parse_ecuacion(cadena, locals_dict):
    """
    Convierte una cadena como '2*x + y = 5' en Eq(2*x + y, 5).
    Si no hay '=', asume 'expr = 0'.
    """
    if "=" in cadena:
        left, right = cadena.split("=", 1)
        left = sympify(left, locals=locals_dict)
        right = sympify(right, locals=locals_dict)
        return Eq(left, right)
    else:
        expr = sympify(cadena, locals=locals_dict)
        return Eq(expr, 0)

def imprimir_solucion_linsolve(sol_set, variables):
    """
    linsolve devuelve:
      - FiniteSet() vacío si no hay solución.
      - FiniteSet((x0, x1, ...)) si hay solución única,
      - FiniteSet((expresiones con parámetros)) si infinitas soluciones.
    """
    if not sol_set:
        print("➤ No hay solución (sistema inconsistente).")
        return

    for sol in sol_set:  # normalmente hay un solo elemento en el conjunto
        # sol es una tupla de expresiones
        exprs = list(sol)
        # Detectar si hay parámetros libres (símbolos como tau0, t0, _t, etc.)
        params = sorted({str(s) for e in exprs for s in e.free_symbols if str(s).startswith(('t', 'tau', '_t'))})
        if any(v.free_symbols for v in exprs):
            print("➤ Solución con parámetros (infinitas soluciones):")
        else:
            print("➤ Solución:")
        for v, e in zip(variables, exprs):
            print(f"   {v} = {e}")

def opcion_sistema_lineal():
    print(LINE)
    print("Resolver SISTEMA DE ECUACIONES LINEALES")
    print(LINE)
    vars_syms, locals_dict = leer_variables()
    print("Ingresa las ecuaciones (una por línea). Deja una línea en blanco para terminar:")
    ecuaciones = []
    while True:
        linea = input("> ").strip()
        if not linea:
            break
        try:
            eq = parse_ecuacion(linea, locals_dict)
            ecuaciones.append(eq)
        except SympifyError:
            print("  ⚠ No se pudo interpretar esa línea. Intenta de nuevo.")
        except Exception as e:
            print(f"  ⚠ Error: {e}")

    if not ecuaciones:
        print("No ingresaste ecuaciones.")
        return

    try:
        sol = linsolve(tuple(ecuaciones), *vars_syms)
        imprimir_solucion_linsolve(sol, vars_syms)
    except Exception as e:
        print(f"⚠ Ocurrió un problema resolviendo el sistema lineal: {e}")
        print("   Sugerencia: verifica que el sistema sea lineal y que las variables coincidan.")

def opcion_cuadratica():
    print(LINE)
    print("Resolver ECUACIÓN CUADRÁTICA: a*x^2 + b*x + c = 0")
    print(LINE)
    try:
        a = S(input("a = ").strip())
        b = S(input("b = ").strip())
        c = S(input("c = ").strip())
    except SympifyError:
        print("⚠ Coeficientes inválidos. Usa números como 2, -3, 4.5")
        return

    if a == 0:
        print("⚠ a = 0 ⇒ no es cuadrática. Si quieres resolver lineal, usa el modo de sistemas o general.")
        return

    disc = b**2 - 4*a*c
    x = symbols('x')
    x1 = (-b + sqrt(disc)) / (2*a)
    x2 = (-b - sqrt(disc)) / (2*a)

    print(f"Discriminante: Δ = {disc}")
    print("Raíces:")
    print(f"  x1 = {x1}")
    print(f"  x2 = {x2}")

def opcion_sistema_general():
    print(LINE)
    print("Resolver SISTEMA GENERAL (lineal o no lineal) con sympy.solve")
    print(LINE)
    vars_syms, locals_dict = leer_variables()
    print("Ingresa las ecuaciones (una por línea). Deja una línea en blanco para terminar:")
    ecuaciones = []
    while True:
        linea = input("> ").strip()
        if not linea:
            break
        try:
            eq = parse_ecuacion(linea, locals_dict)
            ecuaciones.append(eq)
        except SympifyError:
            print("  ⚠ No se pudo interpretar esa línea. Intenta de nuevo.")
        except Exception as e:
            print(f"  ⚠ Error: {e}")

    if not ecuaciones:
        print("No ingresaste ecuaciones.")
        return

    try:
        soluciones = solve(ecuaciones, vars_syms, dict=True)
        if not soluciones:
            print("➤ No se encontraron soluciones cerradas o no hay solución.")
            print("   Tip: para sistemas no lineales difíciles, podrías usar métodos numéricos (nsolve) con valores iniciales.")
            return
        print("➤ Soluciones:")
        for i, sol in enumerate(soluciones, 1):
            print(f"  Solución #{i}:")
            for v in vars_syms:
                if v in sol:
                    print(f"    {v} = {sol[v]}")
                else:
                    print(f"    {v} = (libre)")
    except NotImplementedError:
        print("⚠ sympy no pudo resolver simbólicamente este sistema.")
        print("   Tip: intenta reescribirlo o usa nsolve con valores iniciales.")
    except Exception as e:
        print(f"⚠ Error al resolver: {e}")

def menu():
    while True:
        print("\n" + LINE)
        print("👋 Bienvenido/a al RESOLVEDOR MATEMÁTICO")
        print(LINE)
        print("Elige una opción:")
        print("  1) Sistema de ecuaciones LINEALES")
        print("  2) Ecuación CUADRÁTICA")
        print("  3) Sistema GENERAL (lineal o no lineal)")
        print("  0) Salir")
        op = input("Opción: ").strip()
        if op == "1":
            opcion_sistema_lineal()
        elif op == "2":
            opcion_cuadratica()
        elif op == "3":
            opcion_sistema_general()
        elif op == "0":
            print("¡Hasta luego!")
            break
        else:
            print("❓ Opción no válida. Intenta de nuevo.")

if __name__ == "__main__":
    menu()