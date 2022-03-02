# -*- coding: utf-8 -*-

"""
Created on Mon Feb 17 08:56:39 2020

@author: CPAREJA
"""

import random

dado = ["A", "K", "Q", "J", "8", "7"]

def generar_muestra(n, probs1, probs2, archivo):
    f = open(archivo, "w")
    for i in range(n):
        dado1 = random.choices(dado, probs1)[0]
        dado2 = random.choices(dado, probs2)[0]
        apuesta = 5 * random.randint(1, 100)
        f.write(dado1 + " " + dado2 + " " + str(apuesta) + "\n")
    f.close()

def gana(d1, d2):
    if d1 == d2:
        return random.random() > 0.5
    else:
        return d1.index(d1) >= d2.index(d2)

if __name__ == "__main__":
    probs_dado1 = [0.1, 0.3, 0.2, 0.2, 0.1, 0.1]
    probs_dado2 = [0.2, 0.2, 0.1, 0.3, 0.1, 0.1]
    generar_muestra(10000, probs_dado1, probs_dado2, "casino.txt")
