# -*- coding: utf-8 -*-
"""
Created on Mon Nov 18 23:43:41 2019

@author: Laura
"""

import B5_1_2_matriz_duracion_llamadas_mr as mr
import numpy as np

def mr_job(nombre_archivo):
    mr_job = mr.MRSumaLlamadasPaises(args=[nombre_archivo])
    duracion = []
    paises = []
    #se ejecuta el .py con el código map-reduce generado en el apartado anterior
    with mr_job.make_runner() as runner:
        runner.run()
        for key, value in mr_job.parse_output(runner.cat_output()):
            #se obtiene la lista de países a partir de las claves resultado del map-reduce para luego dimensionar la matriz
            if key[0] not in paises:
                paises.append(key[0])
                #se añade cada valor producido por mr a la lista "duracion"
            duracion.append(int(value))
        #se transforma la lista "duracion" en un array
        array_duracion = np.array([duracion])
        #se reorganiza la matriz en tantas filas y columnas como países haya
        array_duracion = array_duracion.reshape(len(paises), len(paises))
    return array_duracion

if __name__=="__main__":
    import sys
    nombre_archivo = sys.argv[1]
    print(mr_job(nombre_archivo))