# -*- coding: utf-8 -*-
"""
Created on Mon Feb 17 09:21:26 2020

@author: CPAREJA
"""

import random
from mrjob.job import MRJob
from collections import defaultdict

def gana(d1, d2):
    if d1 == d2:
        return random.random() > 0.5
    else:
        return d1.index(d1) >= d2.index(d2)
   
def suma_doble(pares):
    """Ej. [(1, 10), (2, 20), (3, 30)]  --> (6, 60)"""
    a, b = 0, 0
    for x, y in pares:
        a, b = a + x, b + y
    return a, b

class MRComputoApuestas(MRJob):
   
    def mapper(self, _, line):
        linea = line.split()
        dados, apuesta = linea[0] + linea[1], int(linea[2])
        resultado = apuesta if gana(dados[0], dados[1]) else -apuesta
        yield dados, (1, resultado)
              
    def reducer(self, key, values):
        yield key, suma_doble(values)

def estadisticas(totales):
    frecuencia = defaultdict(lambda:0,{})
    for dados in totales:
        dados[0]
        
if __name__ == '__main__':
    MRComputoApuestas.run()
