#!/usr/bin/python -tt

# Ejercicio de map-reduce
# Cristóbal Pareja

"""
Enunciado:

Tenemos un archivo de texto plano (ej.: "pride_and_prejudice.txt")
No tiene formato de líneas; esto es, cada párrafo está en una única línea.

Se plantea diseñar un programa que contabiliza cada par de palabras
con mayúscula que aparecen en un mismo párrafo,
con la esperanza de que esta contabilidad dé la relación entre los personajes
de una obra literaria...

Hazlo usando la técnica de map-reduce,
para que nos sirva para obras de gran tamaño.

El programa se usará así:

    >>>cuenta_pares.py < pride_and_prejudice.txt > resultado.txt
"""

# Una solución con map-reduce:

from mrjob.job import MRJob

class MRCharCount(MRJob):

    def mapper(self, _, line):
        sin_valor = ["a", "after", "all", "and", "as", "at", "about", "any", \
                     "away", "but", "for", "if", "in", "it", "let", 
                     # etcétera
                     "the", "this", "these", "of", "on", "or"]
        pal_may = [p for p in line.split() \
                   if p[0].isupper() and p.lower() not in sin_valor]
        for p1 in pal_may:
            for p2 in pal_may:           
                if p1 < p2:
                    yield (p1, p2), 1
                
    def reducer(self, key, values):
        yield key, sum(values)

if __name__ == '__main__':
    MRCharCount.run()
