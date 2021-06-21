import numpy as np
import pandas as pd
import datetime
import matplotlib.pyplot as plt

def metrics(source_path, dest_path):
    df = pd.read_csv(source_path, sep=',')
    df['SE'] = np.square(df['Obs']-df['Point'])
    df['APE'] = np.absolute((df['Obs']-df['Point'])/df['Obs'])
    index = df.index
    rows = len(index)
    df['contains'] = np.array([True if df['Obs'][i] > df['Lwr'][i] and df['Obs'][i] < df['Upr'][i]
                               else False for i in range(0,rows)])
    df['width'] = df['Upr'] - df['Lwr']
    df.to_csv(dest_path, index = False)