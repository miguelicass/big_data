# -*- coding: utf-8 -*-
"""
Created on Mon Nov 18 23:43:41 2019

@author: Laura
"""
from mrjob.job import MRJob

class MRSumaLlamadasPaises(MRJob):
    def mapper(self, __, line):
        paises = ["A", "B", "C", "D", "E"]
        #se crean listas con las combinaciones de países emisores y receptores de la lista "paises" disponibles en el fichero
        emisor_receptor = [c for c in line.split(" # ") if c[0].isupper() and c[0] in paises]
        #se obtienen los minutos y segundos de la duración de la llamada, sumando 1 a los minutos si los segundos no son 0
        duracion_llamada = line.split(" # ")[2]
        duracion_llamada_split = duracion_llamada.split(":")
        if int(duracion_llamada_split[1]) != 0:
            duracion_llamada_minutos = int(duracion_llamada_split[0])+1
        else:
            duracion_llamada_minutos = int(duracion_llamada_split[0])
        #se devuelve como clave la concatenación de país emisor-país receptor y como valor la duración de la llamada en minutos
        #redondeada superiormente
        yield (emisor_receptor[0][0]+"-"+emisor_receptor[1][0]), duracion_llamada_minutos
        
    def reducer (self, key, values):
        #se devuelve cada clave creada en el mapper con la suma de todos los valores de dicha clave
        yield key, sum(values)
