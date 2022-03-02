# -*- coding: utf-8 -*-
"""
Created on Fri Oct 11 09:31:14 2019

@author: CPAREJA
"""

import sys

def numerar_lineas(nombre_archivo):
    el_archivo = open(nombre_archivo, "r")
    num_linea = 0
    for linea in el_archivo:
        num_linea = num_linea + 1
        print(num_linea, linea.rstrip())
        if num_linea = 10: # Para que no sea interminable
            el_archivo.close()
            return

def listado(agenda):
    print("telefono   email             direccion")
    print("---------- ---------------   -----------------------------")
    for telefono, datos in agenda.items():
        print(telefono.ljust(10), 
              datos["email"].ljust(17),
              datos["direc"])

def main():
    if len(sys.argv) != 2:
        print ('uso correcto: python ./programa.py' \
            + '<nombre archivo agenda>')
        sys.exit(1)

    nombre_archivo = sys.argv[1]
    numerar_lineas(nombre_archivo)
    print("------------------------------") 

if __name__ == '__main__':
    main()
