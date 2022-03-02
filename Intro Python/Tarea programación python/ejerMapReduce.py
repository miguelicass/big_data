#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 14 19:05:55 2020

@author: macasib
"""

from mrjob.job import MRJob

class MRCharCount(MRJob):

    def mapper(self, _, line):
        ##with open("./annual-number-of-deaths-by-cause.csv", 'r') as fl:
            ##first_line = fl.readline()
            ##cause = first_line.split(',')
            line = line.split(',')
            for index in range(len(line[3:])):
                if (line[index+3] == ""):
                    yield index+3, 0
                else: 
                    try:
                        yield index+3, int(float(line[index+3]))
                    except: 
                         yield index+3, 0
                
    def reducer(self, key, values):
        yield key, sum(values)

if __name__ == '__main__':
    MRCharCount.run()

