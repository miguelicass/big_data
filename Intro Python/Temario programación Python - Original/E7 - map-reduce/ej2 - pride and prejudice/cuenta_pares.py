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

    >>>python cuenta_pares.py < pride_and_prejudice.txt > resultado.txt
"""

# Una solución con map-reduce:

from mrjob.job import MRJob

def solo_letras(palabra):
    return ''.join(ch for ch in palabra if ch.isalpha())

class MRCharCount(MRJob):
   
    def mapper(self, _, line):
        sin_valor = ["a", "an", "after", "all", "and", "as",
                     "at", "about", "any", "away",
                     "but", "by", "do", "does",
                     "for", "if", "in", "it",
                     "lady", "let",
                     "miss", "mr", "mrs", "my", "she", "sir",
                     "the", "this", "these", "of", "on", "or", "they",
                     "to", "their", "you", "your", "what", "will"]
        def es_mayuscula(palabra):
            if len(palabra) == 0 or not (palabra[0].isupper()):
                return False
            if len(palabra) == 1:
                return True
            else:
                return palabra[1:].islower()

        linea = line.split()

        if len(linea) > 0:
            palabras = [linea[0]]
            # Eliminamos las palabras después de punto
            # O empiezan por comillas
            for i in range(1, len(linea)): #la palabra primera está ya
                if linea[i-1][-1] not in [".", "?", "!"]:
                    if linea[i][0] not in  ["'", '"']:
                        palabras.append(linea[i])
            pal_may = [p for p in map(solo_letras, palabras) \
                       if es_mayuscula(p) and p.lower() not in sin_valor]
            for p1 in pal_may:
                for p2 in pal_may:           
                    if p1 < p2:
                        yield (p1, p2), 1
                
    def reducer(self, key, values):
        yield key, sum(values)

from collections import defaultdict

def obtener_pares(nombre_archivo):
    trabajo = MRCharCount(args=[nombre_archivo])
    resultados = []
    a_vacio = defaultdict(lambda:0,{})
    diccionario = defaultdict(lambda:a_vacio,{})
    with trabajo.make_runner() as runner:
        runner.run()
        for key, value in trabajo.parse_output(runner.cat_output()):
            elem = key[0], key[1], value
            resultados.append(elem)
            diccionario[key[0]][key[1]] = value
    return resultados, diccionario

if __name__ == '__main__':
    # MRCharCount.run()
    lista, dicc = obtener_pares("pride_and_prejudice.txt")
    print(lista)
    print(diccionario)

"""
if __name__=="__main__":
    import sys
    nombre_archivo = sys.argv[1]
    print(mr_job(nombre_archivo))
"""
