#!/usr/bin/python
# -*- coding: utf-8 -*-

# A3b codificar.py

"""
@autor: C. Pareja Flores
@titulo: Codigos de rotación
@fecha: enero de 2019
"""

def cod_char(a, k):
    if not a.isalpha():
        return a
    kk = ord(a) + k%26
    aa = chr(kk)
    if a.isupper() and aa.isupper() or a.islower() and aa.islower():
        return aa
    else:
        return chr(kk - 26)
    
def cod_linea(linea, k):
    return "".join([cod_char(a, k) for a in linea ])

def main():
  frase = input("Qué frase quieres codificar? ")
  k     = int(input("¿Desplazamiento? "))
  print(cod_linea(frase, k))

if __name__ == "__main__":
    main()
