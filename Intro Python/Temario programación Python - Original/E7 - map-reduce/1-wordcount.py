#!/usr/bin/python -tt
# Contador de palabras secuencial
# y de paso otros contadores: de líneas

import sys

def freq(x,xx):
  # num. de veces que x is in xx  
  n = 0
  for a in xx:
    if a == x:
      n = n+1
  return n
  
def list_words(filename):
  words = []
  f = open(filename,'r') 
  for line in f:
    for w in line.split():
       words.append(w)
  words = list(map(lambda x : x.lower(),words))
  words.sort()
  wordsFreq = []
  while words != []:
    w = words[0]
    n = freq(w,words)
    wordsFreq.append((w,n))  
    words = list(filter(lambda e: e != w , words))
  return wordsFreq
  
def print_words(filename):
  for (x,y) in list_words(filename):
    print(x,y)

def print_top(filename):
  wordFrecs = list_words(filename);  
  frecs = list(map(lambda xy: xy[1], wordFrecs))
  frecs.sort()
  higherFrecs = frecs[0:20]
  seleccionadas = list(filter(lambda xy: xy[1] in higherFrecs, wordFrecs))
  for (x,y) in seleccionadas:
    print(x)

def num_lines(filename):
  n = 0
  f = open(filename,'r') 
  for line in f:
    n = n+1
  return n

def num_palabras(filename):
  np = 0
  f = open(filename,'r') 
  for line in f:
    for p in line.split():
      np = np+1 
  return np

#--------------------------------------------------------------------------
# map, secuencialmente:

# map, secuencialmente:

def palabrasConSuLong(filename):
  listaPalUnos = list([])
  f = open(filename,'r') 
  for line in f:
    for p in line.split():
      par = p,len(p)
      listaPalUnos.append(par)
  return listaPalUnos
    
#--------------------------------------------------------------------------

  
# This basic command line argument parsing code is provided and
# calls the print_words() and print_top() functions which you must define.
def main():
  if len(sys.argv) != 3:
    print("uso: ./wordcount.py {--count | --topcount | --numlines | -- palabrasConSuLong | --numPalabras} file")
    sys.exit(1)
    
  option = sys.argv[1]
  filename = sys.argv[2]
  if option == '--count':
    print_words(filename)
  elif option == '--topcount':
    print_top(filename)
  elif option == '--numlines':
    print(num_lines(filename))
  elif option == '--numPalabras':
    print(num_palabras(filename))
  elif option == '--palabrasConSuLong':
    print(palabrasConSuLong(filename))
  else:
    print('unknown option: ' + option)
    sys.exit(1)

if __name__ == '__main__':
  main()
