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

def demo(nombre_archivo):
    trabajo = MRComputoApuestas(args=[nombre_archivo])
    resultados = []
    with trabajo.make_runner() as runner:
        runner.run()
        for key, value in trabajo.parse_output(runner.cat_output()):
            nuevo_item = key, (value[0], value[1])
            resultados.append(nuevo_item)
    return resultados

def estadisticas(totales):
    frecuencia = defaultdict(lambda:0,{})
    for dados in totales:
        dados[0]
        
if __name__ == '__main__':
    # MRComputoApuestas.run()
    resultado = demo("casino.txt")  
    total_dados = defaultdict(lambda:0,{})
    total_tiradas = 0
    for par, totales in resultado:
        dado1, dado2 = par[0], par[1]
        cuantos, total = totales
        total_dados[dado1] += cuantos
        total_dados[dado2] += cuantos
        total_tiradas += cuantos
    for d, v in total_dados.items():
        print(d, v/(2*total_tiradas))
        