# -*- coding: utf-8 -*-
"""
Created on Wed Nov  4 11:29:30 2020

@author: hb24617
"""
'''
This is synthetic data to demonstrate
'''
import numpy as np

x_res = 1000
x_data = np.linspace(0, 3000, x_res)

# true parameters and a function that takes them
true_pars = [80, 70, -5]
model = lambda x, a, b, c: (a / np.sqrt(x + b) + c)
y_truth = model(x_data, *true_pars)

mu_prim, mu_sec = [1750, 0], [450, 1.5]
cov_prim = [[300**2, 0     ],
            [     0, 0.2**2]]
# covariance matrix of the second dist is trickier
cov_sec = [[200**2, -1     ],
           [    -1,  1.0**2]]
prim = np.random.multivariate_normal(mu_prim, cov_prim, x_res*10).T
sec = np.random.multivariate_normal(mu_sec, cov_sec, x_res*1).T
uni = np.vstack([x_data, np.random.rand(x_res) * 7])

# censoring points that will end up below the curve
prim = prim[np.vstack([[prim[1] > 0], [prim[1] > 0]])].reshape(2, -1)
sec = sec[np.vstack([[sec[1] > 0], [sec[1] > 0]])].reshape(2, -1)

# rescaling to data
for dset in [uni, sec, prim]:
    dset[1] += model(dset[0], *true_pars)

# this code block generates the figure above:
import matplotlib.pylab as plt
plt.figure()
plt.plot(prim[0], prim[1], '.', alpha=0.1, label = '2D Gaussian #1')
plt.plot(sec[0], sec[1], '.', alpha=0.5, label = '2D Gaussian #2')
plt.plot(uni[0], uni[1], '.', alpha=0.5, label = 'Uniform')
plt.plot(x_data, y_truth, 'k:', lw = 3, zorder = 1.0, label = 'True edge')
plt.xlim(0, 3000)
plt.ylim(-8, 6)
plt.legend(loc = 'lower left')
plt.show()

# mashing it all together
dset = np.concatenate([prim, sec, uni], axis = 1)


'''
Here are functions for applying to real data
'''

import pandas as pd

df = pd.read_csv (r'J:\Double Eagle\James\svm\reg\elapsed\test.csv')
x = df.values
x[0]
df = np.genfromtxt(r'J:\Double Eagle\James\svm\reg\elapsed\test.csv',delimiter=',',dtype=None)

def get_flipped(y_data, y_model):
    flipped = y_model - y_data
    flipped[flipped > 0] = 0
    return flipped

def flipped_resid(pars, x, y):
    """
    For every iteration, everything above the currently proposed
    curve is going to be mirrored down, so that the next iterations
    is going to progressively shift downwards.
    """
    y_model = model(x, *pars)
    flipped = get_flipped(y, y_model)
    resid = np.square(y + flipped - y_model)
    #print pars, resid.sum() # uncomment to check the iteration parameters
    return np.nan_to_num(resid)

# plotting the mock data
plt.plot(df['Elapsed_Time'], df['DTMP'], '.', alpha=0.2, label = 'Test data')

# mask bad data (we accidentaly generated some NaN values)
gmask = np.isfinite(df['DTMP'])
dfset = df[np.vstack([gmask, gmask])].reshape((2, -1))

from scipy.optimize import leastsq
guesses =[100, 100, 0]
fit_pars, flag = leastsq(func = flipped_resid, x0 = guesses,
                         args = (dset[0], dset[1]))
# plot the fit:
y_fit = model(x_data, *fit_pars)
y_guess = model(x_data, *guesses)
plt.plot(x_data, y_fit, 'r-', zorder = 0.9, label = 'Edge')
plt.plot(x_data, y_guess, 'g-', zorder = 0.9, label = 'Guess')
plt.legend(loc = 'lower left')
plt.show()

x_data = df['Elapsed_Time']
y_data = df['DTMP']


'''
Run this for curve points

'''
import pandas as pd

df = pd.read_csv (r'J:\Double Eagle\James\svm\reg\elapsed\test.csv')
df = pd.read_excel('Values5.xlsx', sheet_name='Sheet 1')

well_list = df.Well.unique()
well_list.sort()
print(well_list)

well = well_list[4]
print(well)

dfwell = df.loc[df['Well'] == well]
dfwell.head()

stg_list = dfwell.Stage.unique()
stg_list.sort()
print(stg_list)

stg = stg_list[35]
print(stg)
stage = "43"
print(stage)

dfstg = dfwell.loc[dfwell['Stage'] == stg]
dfwell.head()

x_data = dfstg['Elapsed_Time']
print(x_data)
y_data = dfstg['Distance From Perfs']
print(y_data)

from math import e
model = lambda x, a, b: (a * (1 - (e ** (-b * x))))

model = lambda x, a, b: (a * (1 - (exp(-b * x))))

def get_flipped(y_data, y_model):
    flipped = y_model - y_data
    flipped[flipped > 0] = 0
    return flipped

def flipped_resid(pars, x, y):
    y_model = model(x, *pars)
    flipped = get_flipped(y, y_model)
    resid = np.square(y + flipped - y_model)
    return np.nan_to_num(resid)

from scipy.optimize import leastsq

front = 1000
base = .04
guesses =[1200,.09]
guesses =[800,0.1]

guesses =[800,.2]

guesses =[front,base]

fit_pars, flag = leastsq(func = flipped_resid, x0 = guesses, args = (x_data, y_data))

# plotting the mock data
plt.plot(x_data, y_data, '.', alpha=0.2, label = 'Stage data')
# plot the fit:
y_fit = model(x_data, *fit_pars)
y_guess = model(x_data, *guesses)
plt.plot(0,2000)
plt.plot(x_data, y_fit, 'r-', zorder = 0.9, label = 'Base Curve')
plt.plot(x_data, y_guess, 'g-', zorder = 0.9, label = 'Growth Curve')
plt.legend(loc = 'upper right', framealpha=0.1)
plt.show()

out = pd.concat([y_guess,x_data,y_data,y_fit], axis=1)

out.to_csv(well+"-"+stage+" Curve.csv", sep=',', index=False)
