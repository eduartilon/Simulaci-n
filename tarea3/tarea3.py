from time import time
from math import sqrt, ceil
from random import randint, random, shuffle
import pandas as pd


def primo(n):
    if n < 4: # 1 2 3
        return True
    if n % 2 == 0:
        return False
    for d in range (3, int(ceil(sqrt(n)))):
        if n % d == 0:
            return False
    return True
dificiles = []
meta = 10
faciles = [randint(1000, 15000) for i in range(meta)]
while len(dificiles) < meta:
    n = randint(50000000, 100000000)
    if n % 2 == 0:
        n += 1
    if primo(n):
        dificiles.append(n)

from multiprocessing import Pool

if __name__== "__main__":
    c1 = faciles + dificiles
    c2 = dificiles + faciles
    cr = c2.copy()
    shuffle(cr)
    ordenes = {'fp': c1, 'dp': c2, 'oa': cr}
    paths = []
    for trabajadores in range (1, 4):
        with Pool(trabajadores) as p:
            for o in ordenes:
                label = o
                datos = ordenes[o]
                for replica in range(10):
                    start = time()
                    p.map(primo, datos)
                    tiempo = 1000 * (time()- start)
                    if replica > 0:
                        print(f'{replica},{trabajadores},{label},{tiempo}')
        paths.append({'replica':replica, 'trabajadores':trabajadores,
                       'dificultad':[label], 'tiempo':tiempo})
    df = pd.DataFrame(paths)
    print(df)
    df.to_excel('test.xlsx', sheet_name='sheet1', index=False)
