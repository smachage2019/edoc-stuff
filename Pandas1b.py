# -*- coding: utf-8 -*-
"""
Created on Sun Mar  3 18:24:18 2019

@author: seanm
"""
import pandas as pd
import numpy as np
# %%
fam = [1.73, 1.68, 1.71, 1.89]
for height in fam : 
    print(height)
# %%
fam = [1.73, 1.68, 1.71, 1.89]
for index, height in enumerate(fam) :
    print("person " + str(index) + ": " + str(height))    
# %%
# areas list
areas = [11.25, 18.0, 20.0, 10.75, 9.50]

# Code the for loop
for areas in areas :
    print(areas)
# %%
# Change for loop to use enumerate() and update print()
# areas list
areas = [11.25, 18.0, 20.0, 10.75, 9.50]

# Code the for loop
for index, area in enumerate(areas) :
    print("room " + str(index + 1) + ": " + str(area))
# %%
# house list of lists
house = [["hallway", 11.25], 
         ["kitchen", 18.0], 
         ["living room", 20.0], 
         ["bedroom", 10.75], 
         ["bathroom", 9.50]]
         
# Build a for loop from scratch
for x in house :
    print("the " + x[0] + " is " + str(x[1]) + " sqm")
# %%
# Looping data structures PT1
world = {"afghanistan":30.55, "albania":2.77, "algeria":39.21}  
for key, value in world :
    print(key + " -- " + str(value))
# gives error
# %%[
np_height = np.array([1.73, 1.68, 1.71, 1.89, 1.79])
np_weight = np.array([65.4, 59.2, 63.6, 88.4, 68.7])
bmi = np_weight / np_height ** 2
# %%
for val in bmi :
    print(val)
# %%
meas = np.array([np_height, np_weight])
for val in meas :
    print(val)
# not what we want
# %%
for val in np.nditer(meas) :
    print(val)
# Recap:  Dict - for key, val in my_dict.items() :
# Recap:  Numpy array - for val in np.nditer(my_array) :
# %%
# Definition of dictionary
europe = {'spain':'madrid', 'france':'paris', 'germany':'berlin',
          'norway':'oslo', 'italy':'rome', 'poland':'warsaw', 'austria':'vienna' }
          
# Iterate over europe
for x, y in europe.items() :
    print("the capital of " + x + " is " + y)
# %%
for val in np.nditer(np_height) :
    print(str(val) + " inches")
# %%
brics = pd.read_csv("C:/Users/seanm/OneDrive/Documents/Python Scripts/brics.csv", index_col = 0)    
# %%
for val in brics: print(val)
# just get colum names, not what we want
# %%
# lab = row label, row is row
for lab, row in brics.iterrows() :
    print(lab)
    print(row)
# if we just want capital on each iteration
for lab, row in brics.iterrows() :
    print(lab + ": " + row["capital"])  
# %%
# add a new column with length of each name
for lab, row in brics.iterrows() :
    brics.loc[lab, "name_length"] = len(row["country"])
print(brics)
# %%
# This is a better way to do it
brics["name_length"] = brics["country"].apply(len)
print(brics)
# %%
cars = pd.read_csv('C:/Users/seanm/OneDrive/Documents/Python Scripts/cars.csv', index_col = 0)

# Iterate over rows of cars
for lab, row in cars.iterrows() :
    print(lab)
    print(row)
# %%

for lab, row in cars.iterrows() :
    print(lab + ": " + str(row["cars_per_cap"]))  
# %%
# Code for loop that adds COUNTRY column
for lab, row in cars.iterrows() :
    cars.loc[lab, "COUNTRY"] = row["country"].upper()


# Print cars
print(cars)
# example string
string = "this should be uppercase!"
print(string.upper())
# %%
# Use .apply(str.upper)
cars["COUNTRY"] = cars["country"].apply(str.upper)
print(cars)
# %%
# Random Numbers
