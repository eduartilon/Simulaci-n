from math import exp, pi
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

import seaborn as sns


data = {'muestra':[99, 83, 9, 4, 0, 0, 0], 'decimales':[1, 2, 3, 4, 5, 6, 7]} #datos del data.frame

# Create DataFrame
df = pd.DataFrame(data) #comando para el data frame

# Print the output.
print(df)

df['decimales'] = df['decimales'].astype(object) #poner el numero como objeto (pronombre)

df.boxplot(by = 'muestra', column =['decimales'], grid = False) #boxplot by= eje x, column = eje y

plt.savefig('figurap5.png', dpi = 400) #gnerar imagen y guardar
plt.show() #mostrar imagen generada
