# -*- coding: utf-8 -*-
"""
Created on Fri Mar  1 13:46:12 2019

@author: hb24617
"""

# %%
# Pandas, Par1
#
import pandas as pd
import numpy as np
#
bric = pd.read_csv('E:/PYTHON/brics.csv')
dict = {"country": ["Brazil", "Russia", "India", "China", "South Africa"],
        "capital": ["Brazilia", "Moscow", "New Delhi", "Beijing", "Pretoria"],
        "area": [8.516, 17.10, 3.286, 9.597, 1.221],
        "population": [200.4, 143.5, 1252, 1357, 52.98]}
# %%
# import pandas as pd
brics = pd.DataFrame(dict)
brics.index = ["BR", "RU", "IN", "CH", "SA"]
brics
brics.to_csv('E:/PYTHON/brics.csv', sep=',')
# %%
bric = pd.read_csv('E:/PYTHON/brics.csv', index_col=0)
bric
# %%
# Pre-defined lists
names = ['United States', 'Australia', 'Japan', 'India',
         'Russia', 'Morocco', 'Egypt']
dr = [True, False, False, False, True, True, True]
cpc = [809, 731, 588, 18, 200, 70, 45]

# Import pandas as pd
# import pandas as pd

# Create dictionary my_dict with three key:value pairs: my_dict
my_dict = {'country': names, 'drives_right': dr, 'cars_per_cap': cpc}

# Build a DataFrame cars from my_dict: cars
cars = pd.DataFrame(my_dict)

# Print cars
print(cars)
# %%
# import pandas as pd

# Build cars DataFrame
names = ['United States', 'Australia', 'Japan', 'India',
         'Russia', 'Morocco', 'Egypt']
dr = [True, False, False, False, True, True, True]
cpc = [809, 731, 588, 18, 200, 70, 45]
dict = {'country': names, 'drives_right': dr, 'cars_per_cap': cpc}
cars = pd.DataFrame(dict)
print(cars)

# Definition of row_labels
row_labels = ['US', 'AUS', 'JAP', 'IN', 'RU', 'MOR', 'EG']

# Specify row labels of cars
cars.index = row_labels

# Print cars again
print(cars)
cars.to_csv('E:/PYTHON/cars.csv', sep=',')
# %%
# Import pandas as pd
# %%
# import pandas as pd

# Import the cars.csv data: cars
cars = pd.read_csv('E:/PYTHON/cars.csv', index_col=0)

# Print out cars
print(cars)
# %%
# import pandas as pd

# Import the cars.csv data: cars
brics = pd.read_csv('E:/PYTHON/brics.csv', index_col=0)
brics
# %%
# select column
brics["country"]
type(brics["country"])
# do it like this
brics[["country"]]
type(brics[["country"]])
# now all dataframe
brics[["country", "capital"]]
# row access
brics[1:4]
# better to use loc(label-based) and iloc (interger position-based)
# %%
brics.loc["RU"]
# good, but not a dataframe
brics.loc[["RU"]]
# for more rows
brics.loc[["RU", "IN", "CH"]]
# if only want two columns
brics.loc[["RU", "IN", "CH"], ["country", "capital"]]

# select all rows, but only certain columns
brics.loc[:, ["country", "capital"]]
# Square brackets give column and row access through slicing
# loc gives row access, column access and row$col at same time
# %%
# iloc
brics.iloc[[1]]
brics.iloc[[1, 2, 3]]
brics.iloc[[1, 2, 3], [0, 1]]
brics.iloc[:, [0, 1]]
# %%
# exercise
# Import cars data
# import pandas as pd
cars = pd.read_csv('cars.csv', index_col=0)

# Print out country column as Pandas Series
print(cars["country"])

# Print out country column as Pandas DataFrame
print(cars[["country"]])

# Print out DataFrame with country and drives_right columns
print(cars[["country", "drives_right"]])
# %%
# Import cars data
# import pandas as pd
cars = pd.read_csv('cars.csv', index_col=0)

# Print out first 3 observations
print(cars[0:3])

# Print out fourth, fifth and sixth observation
print(cars[3:6])
# %%
# Import cars data
# import pandas as pd
cars = pd.read_csv('cars.csv', index_col=0)

# Print out observation for Japan
print(cars.loc["JAP"])

# Print out observations for Australia and Egypt
print(cars.loc[["AUS", "EG"]])
# %%
# Import cars data
# import pandas as pd
cars = pd.read_csv('cars.csv', index_col=0)

# Print out drives_right value of Morocco
print(cars.loc[["MOR"], ["drives_right"]])

# Print sub-DataFrame
print(cars.loc[["RU", "MOR"], ["country", "drives_right"]])
# %%
# Import cars data
# import pandas as pd
cars = pd.read_csv('cars.csv', index_col=0)

# Print out drives_right column as Series
print(cars["drives_right"])

# Print out drives_right column as DataFrame
print(cars[["drives_right"]])

# Print out cars_per_cap and drives_right as DataFrame
print(cars[["cars_per_cap", "drives_right"]])
# %%
# Comparison Operators - how two python operators relate:boolean


np_height = np.array([1.73, 1.68, 1.71, 1.89, 1.79])
np_weight = np.array([65.4, 59.2, 63.6, 88.4, 68.7])
bmi = np_weight / np_height ** 2

bmi
bmi > 23
bmi[bmi > 23]
# %%
2 < 3
2 == 3
2 <= 3
"carl" < "chris"
3 < 4.1
# %%
bmi > 23
# %%
# Comparison of integers
print(x >= -10)


# Comparison of strings
print("test"<= y)


# Comparison of booleans
print(True > False)
# %%
# Comparison of integers
x = -3 * 6
print(x >= -10)

# Comparison of strings
y = "test"
print("test" <= y)

# Comparison of booleans
print(True > False)
# %%
# Create arrays
# import numpy as np
my_house = np.array([18.0, 20.0, 10.75, 9.50])
your_house = np.array([14.0, 24.0, 14.25, 9.0])

# my_house greater than or equal to 18
print(my_house >= 18)

# my_house less than your_house
print(my_house < your_house)

# %%
# Boolean Operators
# and or not
x = 12
x > 5 and x < 15  # both need to be true for true
y = 5
y < 7 or y > 13  # only one is true
# %%
np.logical_and(bmi > 21, bmi < 22)  # uses Numpy functions
bmi[np.logical_and(bmi > 21, bmi < 22)]
# %%
# Define variables
my_kitchen = 18.0
your_kitchen = 14.0

# my_kitchen bigger than 10 and smaller than 18?
print(my_kitchen > 10 and my_kitchen < 18)

# my_kitchen smaller than 14 or bigger than 17?
print(my_kitchen < 14 or my_kitchen > 17)

# Double my_kitchen smaller than triple your_kitchen?
print((my_kitchen * 2) < (your_kitchen * 3))
# %%
x = 8
y = 9
not(not(x < 3) and not(y > 14 or y > 10))
# %%
# Create arrays
# import numpy as np
my_house = np.array([18.0, 20.0, 10.75, 9.50])
your_house = np.array([14.0, 24.0, 14.25, 9.0])

# my_house greater than 18.5 or smaller than 10
print(np.logical_or(my_house > 18.5, my_house < 10))

# Both my_house and your_house smaller than 11
print(np.logical_and(my_house < 11, your_house < 11))
# %%
z = 4
if z % 2 == 0:
    print("checking " + str(z))
    print("z is even")
# %%
z = 5
if z % 2 == 0:
    print("checking " + str(z))
    print("z is even")  
else: 
    print("z is odd")
# %%
z = 7
if z % 2 == 0:
    print("z is divisible by 2")
elif z % 3 == 0:
    print("z is divisible by 3")
else:
    print("z is neither divisble by 2 nor by 3")
# %%
  area = 10.0
if(area < 9) :
    print("small")
elif(area < 12) :
    print("medium")
else :
    print("large")
# %%
    # Define variables
room = "bed"
area = 14.0

# if-else construct for room
if room == "kit" :
    print("looking around in the kitchen.")
elif room == "bed":
    print("looking around in the bedroom.")
else :
    print("looking around elsewhere.")

# if-else construct for area
if area > 15 :
    print("big place!")
elif area > 10 :
    print("medium size, nice!")
else:
    print("pretty small.")
# %%
# Filtering Pandas DataFrame
# Here we want to select countries with area > 8m km^2
brics
# first need to get column as a Pandas series not DF
brics["area"]
# or
brics.iloc[:,2] > 8
# gives a boolean series
is_huge = brics.iloc[:,2] > 8
brics[is_huge]
# same as this, but all in one line
brics[brics["area"] > 8]
# %%
# with Numpy
np.logical_and(brics["area"] > 8, brics["area"] < 10)
# in one line
brics[np.logical_and(brics["area"] > 8, brics["area"] < 10)]
# %%

# Extract drives_right column as Series: dr
dr = cars["drives_right"]
print(dr)
# Use dr to subset cars: sel
sel = cars[dr]

# Print sel
print(sel)
# %%
cars = pd.read_csv('cars.csv', index_col = 0)

# Convert code to a one-liner
sel = cars[cars["drives_right"]]
# Print sel
print(sel)
# %%
# Create car_maniac: observations that have a cars_per_cap over 500
cpc = cars["cars_per_cap"]
many_cars = cpc > 500
car_maniac = cars[many_cars]

# Print car_maniac
print(car_maniac)
# %%
# Create medium: observations with cars_per_cap between 100 and 500
medium = cars[np.logical_and(cars["cars_per_cap"] < 500, cars["cars_per_cap"] > 100)]



# Print medium
print(medium)
# %%
# While loop, looping if statement as long as true condition
square = 1



while square <= 10:

    print(square)    # This code is executed 10 times

    square += 1      # This code is executed 10 times



print("Finished")  # This code is executed once



square = 0

number = 1



while square < 81:

    square = number ** 2

    print(square)

    number += 1
# %%
error = 50.0
while error > 1 : 
    error = error / 4
    print(error)
# %%
x = 1
while x < 4 : 
    print(x)
    x = x + 1
# %%
offset = 8
while offset != 0 :
    print("correcting....")
    offset = offset - 1
    print(offset)
# %%
# Initialize offset
offset = -6

# Code the while loop
while offset != 0 :
    print("correcting...")
    if offset > 0 :
        offset = offset - 1
    else:
        offset = offset + 1
    print(offset)
# %%
    # For loops
fam = [1.7, 1.68, 1.71, 1.89]
print(fam)
# want each element printed
for height in fam :
    print(height)
    
for index, height in enumerate(fam) :
    print("index " + str(index) + ": " + str(height)) 
    
# %%
for c in "family" :
    print(c.capitalize())
# %%
# areas list
areas = [11.25, 18.0, 20.0, 10.75, 9.50]

# Code the for loop
