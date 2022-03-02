# -*- coding: utf-8 -*-
"""
Created on Mon Feb 17 09:21:26 2020

@author: CPAREJA
"""

import sys
from collections import defaultdict

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print ('uso correcto: python ./estadística_dados.py' \
            + '<nombre archivo>')
        sys.exit(1)

    nombre_archivo = sys.argv[1]
    muestra = open(nombre_archivo, "r")
    total_dados = defaultdict(lambda:0,{})
    total_tiradas = 0
    for linea in muestra:
        dados, frec1, frec2 = linea.split()
        d1, d2 = dados[1], dados[2]
        frec1 = abs(int(frec1[1:-1]))
        # frec2 = abs(int(frec2[0:-1])) # No se va a usar
        total_dados[d1] += frec1
        total_dados[d2] += frec1
        total_tiradas += frec1
    for d, v in total_dados.items():
        print(d, v/(2*total_tiradas))
        