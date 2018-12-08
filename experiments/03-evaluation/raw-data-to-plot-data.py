#!/usr/bin/env python

"""
(0,11.91) +- (0,0.28)                                                           
(200,35.94) +- (0,3.41)                                                         
(400,60.7) +- (0,4.69)                                                          
(600,77.99) +- (0,2.41)                                                         
(800,91.39) +- (0,1.15)                                                         
(1000,97.61) +- (0,0.5)                                                         
(1200,98.61) +- (0,0.64)                                                        
(1400,99.65) +- (0,0.07)                                                        
(1600,99.76) +- (0,0.06)                                                        
(1800,99.71) +- (0,0.12)                                                        
(2000,99.75) +- (0,0.09) 
"""

import sys
import numpy as np
import pandas as pd

raw = pd.read_csv(sys.argv[1])
times = pd.DataFrame({'preds': raw['preds'], 'trial': raw['trial'], 'time': raw['time']})
mean = times.groupby('preds').mean()['time']
std = times.groupby('preds').std()['time']

print("preds time error")
for (num,(mu,sigma)) in enumerate(zip(mean,std)):
    #print("({},{}) +- (0,{})".format(num,mu,sigma))
    print("{} {} {}".format(num,mu,sigma))

