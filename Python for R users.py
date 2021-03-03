# -*- coding: utf-8 -*-
"""
Created on Thu Jul 25 20:09:00 2019
Python for R users
@author: hb24617
"""
####################################################################
# The Basics
# Containers: Lists [,] zero index
# Negative indices -1 selects last element of a list
# [0:3] is a range, [0:5:2] suggests range and then step size

# Dictionaries, data is stored in key value pairs {:}

# len() finds the length

# Assign the values to the list
person_list = ['Jonathan', 'Cornelissen', 'male', True, 458]

# Get the first name from the list
print(person_list[0])

# Get the first and last name from the list
print(person_list[0:2])

# Get the employment status
print(person_list[-2])

# Create a dictionary from of the employee information list
person_dict = {
    'fname': person_list[0],
    'lname': person_list[1],
    'sex': person_list[-3],
    'employed': person_list[-2],
    'twitter_followers': person_list[-1]
}

# Get the first and last names from the dict
print(person_dict['fname'])
print(person_dict['lname'])


# Functions, Methods, and Libraries
# Methods have similar behavior to functions...python is object oriented
# everything you create is an object and can have attributes and methods
# Methods are functions that an object can call on itself
# Functions are called on an object

l = [0, 1, 2, 3, 4]
len(l) # call a function
l.len() # not a method

# periods have a special meaning

# Append to a list
l = [1, "2", True]
l.append('appended value')
l

# Update a dictionary
d = {'int)value':3, 'bool_value':False, 'str_value':'hello'}
d.update({'str_value':'new_value', 'new_key':'new_value'})
d

# Methods are specific to the object

# base Python does not have arrays and dataframes
# need nunpy for arrays and matricies and pandas for dataframes
# unlike in R where you load the library and have full access to functions
# in Python you heed to have nump.function or pandas.function
import numpy
arr = numpy.loadtxt('digits.csv', delimiter=',')

import numpy as np
arr = np.loadtxt('digits.csv', delimiter=',')

# can use an alias like np for numpy and pd for pandas

import pandas as pd

df = pd.read_csv('digits.csv')
df.head()

# dataframe has several methods and attributes like head()
############################################################################
# Control flow and loops
# whitespace and indentation matters, code blocks are marked with indent

# if
if 5 == 5:
    print('True')

# if elif
val = 2
if val == 3:
    print('snap')
elif val == 2:
    print('crackle')
else:
    print('pop')
        
# for
    num_val = [1, 2, 3, 4]
    for value in num_val:
        print(value)
    
# Assign 5 to a variable
num_drinks = 5

# if statement
if num_drinks < 0:
    print('error')
# elif statement
elif num_drinks <= 4:
    print('non-binge')
# else statement
else:
    print('binge')
    
# Loops
num_drinks = [5, 4, 3, 3, 3, 5, 6, 10]

# Write a for loop
for drink in num_drinks:
    # if/else statement
    if drink <= 4:
        print('non-binge')
    else:
        print('binge')
        
# Functions
# use the "def" keyword

        
def my_mean(x, y):
    num = x + y
    dem = 2
    return num / dem


my_mean(10,20)   
  
  
def my_sq(x):
    return x ** 2
def my_sq_mean(y, z):
    return (my_sq(y) + my_sq(z)) / 2

my_sq_mean(10, 12)
 ############################################################################ 
 # Lambda Function these are similar to the anomomous functins in r


def add_1(x):
    return x + 1

a1_lam = lambda x: x + 1
a1_lam(3)
    
  
  # Binge status for males
def binge_male(num_drinks):
    if num_drinks <= 5:
        return 'non-binge'
    else:
        return 'binge'
        
# Check
print(binge_male(6))
  
# Binge status for females
def binge_female(num_drinks):
    if num_drinks <= 4:
        return 'non-binge'
    else:
        return 'binge'

# Check
print(binge_female(2))  
  
  
 # A function that returns a binge status
def binge_status(sex, num_drinks):
    if sex == 'male':
        return binge_male(num_drinks)
    else:
        return binge_female(num_drinks) 
  
# Male who had 5 drinks
print(binge_status('male', 5))

# Female who had 5 drinks
print(binge_status('female', 5))
  
  # A function that takes a value and returns its square
def sq_func(x):
    return(x**2)
    
# A lambda function that takes a value and returns its square
sq_lambda = lambda x: x**2

# Use the lambda function
print(sq_lambda(3))
 ######################################################################## 
# Comprehensions are loops that iterate through a list
# perform some function
# append results into a new list

# loop
data = [1, 2, 3, 4, 5]
new = []
for x in data:
    new.append(x**2)
print(new)
 # Comprehension 
data = [1, 2, 3, 4, 5]
new = [x**2 for x in data]
print(new)
  
data = [1, 2, 3, 4, 5]
new = {}
for x in data:
    new[x] = x**2
print(new)

data = [1, 2, 3, 4, 5]
new = {x: x**2 for x in data}
print(new)
 #################################################################### 
# Alternatives for loop, like sapply, lapply and apply in R
# map function and apply method in Python

# Map first argument is function, second is list
# For loop
def sq(x):
    return x**2
l = [1, 2, 3]
for i in l:
    print(sq(i))

# Map
map(sq, l)
list(map(sq, l))
  
  
 # map the binge_female function to num_drinks
print(list(map(binge_female,num_drinks)))
  
# Append dataframes into list with for loop
dfs_list = []
for f in inflam_files:
    dat = pd.read_csv(f)
    dfs_list.append(dat)

# Re-write the provided for loop as a list comprehension: dfs_comp
dfs_comp = [pd.read_csv(f) for f in inflam_files]
print(dfs_comp)  
  
 ##############################################
##############################################
# Selecting data in Pandas

# manually create a dataframe
# passing a dictionary to dataframe
 
df = pd.DataFrame({
        'A': [1, 2, 3],
        'B': [4, 5, 6],
        'C': [7, 8, 9]},
    index = ['X', 'Y', 'Z'])

print(df)
  
# Subsetting Columns
df['A']
df.A  
  df[['A', 'B']]
  
 # Subset rows, use either indicies or row names

df.iloc[0, :]
df.iloc[[0, 1], :]
  
df.loc['X']

df.loc[['X', 'Y']]

# simulaneous row, col  
df.loc['X', 'A']
  
df.loc[['X', 'Y'], ['A', 'B']]
  
# Boolian subset
df[df.A == 3]

df[(df.A == 3) | (df.B == 4)]

# Attributes no () in attribute
df.shape
df.shape()  # error
  
# Print the first row of tips using iloc
print(tips.iloc[0])

# Print all the rows where sex is Female
print(tips.loc[tips.sex == 'Female'])

 # Print the first row of tips using iloc
print(tips.iloc[0])

# Print all the rows where sex is Female
print(tips.loc[tips.sex == 'Female'])

# Print all the rows where sex is Female and total_bill is greater than 15
print(tips.loc[(tips.sex == 'Female') & (tips.total_bill > 15)]) 
  
  # Subset rows and columns

print(tips.loc[tips['sex'] == 'Female', ['total_bill', 'tip', 'sex']])

print(tips.iloc[0:3, 0:3])
  
# Data types: Info methos
# can convert data types by as type 
  
df['A'] = df['A'].astype(str)
str(df)
df.info()
 
# String Accessor
df = pd.DataFrame({'name': ['Daniel  ','  Eric', '  Julia  ']})
df
df['name_strip'] = df['name'].str.strip()
df  

# Category is like factors in R
df['gender_cat'].cat.categories
df.gender_cat.cat.codes

 # Datetime 
  
df = pd.DataFrame({'name': ['Rosaline Franklin', 'William Gosset'],'born': ['1920-07-25', '1876-06-13']})
df['born_dt'] = pd.to_datetime(df['born'])

 df.dtypes

# Datetime accessor
df['born_dt'].dt.day  
df['born_dt'].dt.month
df['born_dt'].dt.year

# Convert the size column
tips['size'] = tips['size'].astype(int)

# Convert the tip column
tips['tip'] = tips['tip'].astype(float)

# Look at the types
print(tips.dtypes)

# Convert sex to lower case
tips['sex'] = tips['sex'].str.lower()

# Convert smoker to upper case
tips['smoker'] = tips['smoker'].str.upper()

# Print the sex and smoker columns
print(tips[['sex', 'smoker']])
 
  # Convert the type of time column
tips['time'] = tips['time'].astype('category')

# Use the cat accessor to print the categories in the time column
print(tips['time'].cat.categories)

# Order the time category so lunch is before dinner
tips['time2'] = tips['time'].cat.reorder_categories(['Lunch', 'Dinner'], ordered=True)

# Use the cat accessor to print the categories in the time2 column
print(tips['time2'].cat.categories)


import pandas as pd

# Load the country_timeseries dataset
ebola = pd.read_csv('country_timeseries.csv')

# Inspect the Date column
print(ebola['Date'].dtype)

# Convert the type of Date column into datetime
ebola['Date'] = pd.to_datetime(ebola['Date'], format='%m/%d/%Y')

# Inspect the Date column
print(ebola['Date'].dtype)
  
import pandas as pd

# Load the dataset and ensure Date column is imported as datetime
ebola = pd.read_csv('country_timeseries.csv', parse_dates=['Date'])

# Inspect the Date column
print(ebola['Date'].dtype)  
  
# Create a year, month, day column using the dt accessor
ebola['year'] = ebola.Date.dt.year
ebola['month'] = ebola.Date.dt.month
ebola['day'] = ebola.Date.dt.day

# Inspect the newly created columns
print(ebola[['year', 'month', 'day']].head())  


# More Pandas: Missing Data
# NaN missing values form numpy  can check for them with pd.notnull or pd.isnull or pd.isna
import pandas as pd

# Load the country_timeseries dataset
jw = pd.read_csv('jw.csv')
jw
a_mean = jw['treatment_a'].mean()
a_mean
jw['a_fill'] = jw['treatment_a'].fillna(a_mean)
jw
###############################################################
import pandas as pd
df = pd.DataFrame(
        {'A': [1, 2, 3],
              'B':[4, 5, 6]
             })
df.apply(np.mean, axis=0)
df.apply(np.mean, axis=1)
df

##############################################
# Tidy - reshape

# Tidy Melt  convets to tidy format

jw_melt = pd.melt(jw, 'name')
jw_melt
# transform back
jw_melt_pivot = pd.pivot_table(jw_melt,
                                     index='name',
                                     columns='variable',
                                     values='value')

jw_melt_pivot

jw_melt_pivot.reset_index()

#######################Group By#####################
# groupby: split-apply-combine
# split data into separate partitions
# apply a function on each partition
# combine the results

jw_melt.groupby('name')['value'].mean()


##########################################################
# Print the rows where total_bill is missing
print(tips.loc[pd.isnull(tips['total_bill'])])

# Mean of the total_bill column
tbill_mean = tips['total_bill'].mean()

##############################################################
################################################################
#####################Plotting with Pandas##################

 

# Fill in missing total_bill
print(tips['total_bill'].fillna(tbill_mean))

# Mean tip by sex
print(tips.groupby('sex')['tip'].mean())

# Mean tip by sex and time
print(tips.groupby(['sex', 'time'])['tip'].mean())

##########################################################
# Pandas plot method plot()
# pass plot the kind argument
# kinds: line, bar, barh, hist, box, kde, density, area, pie, scatter, hexbin
import pandas as pd

# Load the country_timeseries dataset
iris = pd.read_csv('iris.csv')
import matplotlib.pyplot as plt
iris['SepalLength'].plot(kind='hist')
plt.show()
 
cts = iris['Name'].value_counts()
cts.plot(kind='bar')
plt.show()
# here the .value_counts() counts the frequency

iris.plot(kind='scatter', x='SepalLength', y='SepalWidth')
plt.show()

iris.plot(kind='box')
plt.show()

iris.boxplot(by='Name', column='SepalLength')

####################################################################
############################Seaborn####################################
# works well with tidy data: does barplots:barplot, hist:displot,box:boxplot
# scatter:regplot, Seaborn can color points by data and facet plot by data

import seaborn as sns
import matplotlib.pyplot as plt
sns.distplot(iris['SepalLength'])
plt.show()

sns.countplot('Name', data=iris)

sns.boxplot(x='Name', y='SepalLength', data=iris)

sns.regplot(x='SepalLength', y='SepalWidth', data=iris)

sns.regplot(x='SepalLength', y='SepalWidth', data=iris,fit_reg=False)

sns.lmplot(x='SepalLength', y='SepalWidth', data=iris,
                fit_reg=False,
                col='Name', hue='Name')

g = sns.FacetGrid(iris, col="Name")
    ...: g = g.map(plt.hist, "SepalLength")
    ...: plt.show()

# manual facet
import seaborn as sns
import matplotlib.pyplot as plt

# Create a facet
facet = sns.FacetGrid(df, col='column_a', row='column_b')

# Generate a facetted scatter plot
facet.map(plt.scatter, 'column_x', 'column_y')
plt.show()
##################################################
import seaborn as sns
import matplotlib.pyplot as plt

# FacetGrid of time and smoker colored by sex
facet = sns.FacetGrid(iris, col="Name", row='SepalWidth', hue='Name')

# Map the scatter plot of total_bill and tip to the FacetGrid
facet.map(plt.scatter, 'SepalLength', 'SepalWidth')
plt.show()

##########################################################
#################Matplotlib###################################

import matplotlib.pyplot as plt
    ...: plt.hist(iris['SepalLength'])
    ...: plt.show()

plt.scatter(iris['SepalLength'], iris['SepalWidth'])
    ...: plt.show()

fig, ax = plt.subplots()
   ...: ax.scatter(iris['SepalLength'], iris['SepalWidth'])
   ...: ax.set_title('Sepal Length')
   ...: ax.set_xlabel('Sepal Length')
   ...: ax.set_ylabel('Sepal Width')
   ...: plt.show()

fig, ax = plt.subplots()
   ...: ax.scatter(iris['SepalLength'], iris['SepalWidth'])
   ...: ax.set_title('Sepal Length')
   ...: ax.set_xlabel('Sepal Length')
   ...: ax.set_ylabel('Sepal Width')
   ...: plt.xticks(rotation=45) # rotate the x-axis ticks
   ...: plt.show()

fig, ax = plt.subplots()
    ...: ax.scatter(iris['SepalLength'], iris['SepalWidth'])
    ...: plt.show()

fig, (ax1, ax2) = plt.subplots(1, 2)
   ...: ax1.scatter(iris['SepalLength'], iris['SepalWidth'])
   ...: ax2.hist(iris['SepalLength'])
   ...: plt.show()

fig, ax = plt.subplots()
    ...: sns.regplot(x='SepalLength', y='SepalWidth',
    ...:             data=iris, fit_reg=False, ax=ax)
    ...: plt.show()
plt.clf()

# Figure with 2 axes: regplot and distplot
fig, (ax1, ax2) = plt.subplots(1, 2)
sns.distplot(tips['tip'], ax=ax1)
sns.regplot(x='total_bill', y='tip', data=tips, ax=ax2)
plt.show()

################################################################
################################################################
####################NYC flights data###########################
import pandas as pd

# Load the country_timeseries dataset
airlines = pd.read_csv('airlines.csv')
airports = pd.read_csv('airports.csv')
flights = pd.read_csv('flights.csv')
planes = pd.read_csv('planes.csv')
weather = pd.read_csv('weather.csv')

import glob
csv_files = glob.glob('*.csv')
all_dfs = [pd.read_csv(x) for x in csv_files]

all_dfs[0]



import glob
import pandas as pd

# Get a list of all the csv files
csv_files = glob.glob('*.csv')

# List comprehension that loads of all the files
dfs = [pd.read_csv(x) for x in csv_files]

# List comprehension that looks at the shape of all DataFrames
print([x.shape for x in dfs])

# Get the planes DataFrame
planes = dfs[3]
# Count the frequency of engines in our data
print(planes['engines'].value_counts())
# Look at all planes with >= 3 engines
print(planes.loc[(planes.engines >= 3)]) 
# Look at all planes with >= 3 engines and < 100 seats
print(planes.loc[(planes.engines >= 3) & (planes.seats <= 100)])





# Scatter plot of engines and seats
planes.plot(x='engines', y='seats', kind='scatter')
plt.show()

plt.clf()
# Histogram of seats
planes.seats.plot(kind='hist')
plt.show()

# Boxplot of seats by engine
planes.boxplot(column='seats', by='engine')
plt.xticks(rotation=45)
plt.show()

#################################################################
# Manipulating Data

# df_melt.groupby('name')['value'].agg(['mean', 'max'])
# Dummy variables, categorical variables need to be encoded as dummy variables
# One-hot encoding
df = pd.DataFrame({
    ...:        'status':['sick', 'healthy', 'sick'],
    ...:        'treatment_a': [np.NaN, 16, 3],
    ...:        'treatment_b': [2, 11, 1]
    ...:  })
    ...: df
pd.get_dummies(df)

# Recode dates 
# Print time_hour
print(flights['time_hour'])
flights['season'] = flights.time_hour.apply(get_season)

from datetime import date, datetime
Y = 2000 # dummy leap year to allow input X-02-29 (leap day)
seasons = [('winter', (date(Y,  1,  1),  date(Y,  3, 20))),
           ('spring', (date(Y,  3, 21),  date(Y,  6, 20))),
           ('summer', (date(Y,  6, 21),  date(Y,  9, 22))),
           ('autumn', (date(Y,  9, 23),  date(Y, 12, 20))),
           ('winter', (date(Y, 12, 21),  date(Y, 12, 31)))]

def get_season(now):
    if isinstance(now, datetime):
        now = now.date()
    now = now.replace(year=Y)
    return next(season for season, (start, end) in seasons
                if start <= now <= end)

print(get_season(date.today()))

print(get_season((flights.time_hour)))


import datetime as dt

def get_season(date):

    m = date.month * 100
    d = date.day
    md = m + d

    if ((md >= 301) and (md <= 531)):
        s = 0  # spring
    elif ((md > 531) and (md < 901)):
        s = 1  # summer
    elif ((md >= 901) and (md <= 1130)):
        s = 2  # fall
    elif ((md > 1130) and (md <= 229)):
        s = 3  # winter
    else:
        raise IndexError("Invalid date")

    return s

season = get_season(dt.date())

def season(date, hemisphere):
    ''' date is a datetime object
        hemisphere is either 'north' or 'south', dependent on long/lat.
    '''
    md = date.month * 100 + date.day

    if ((md > 320) and (md < 621)):
        s = 0 #spring
    elif ((md > 620) and (md < 923)):
        s = 1 #summer
    elif ((md > 922) and (md < 1223)):
        s = 2 #fall
    else:
        s = 3 #winter

    if hemisphere != 'north':
        if s < 2:
            s += 2 
        else:
            s -= 2

    return s


# Apply the function on data
flights['season'] = flights['time_hour'].apply(get_season)

# Calculate total_delay
flights['total_delay'] = flights['dep_delay'] + flights['arr_delay']

#################################################################
# Calculate total_delay
flights['total_delay'] = flights['dep_delay'] + flights['arr_delay']

# Mean total_delay by carrier
tdel_car = flights.groupby('carrier')['total_delay'].mean().reset_index()
print(tdel_car)

# Mean dep_delay and arr_delay for each season
dadel_season = flights.groupby('season')['dep_delay', 'arr_delay'].mean().reset_index()
print(dadel_season)

# Mean and std delays by origin
del_ori = flights.groupby('origin')['total_delay', 'dep_delay', 'arr_delay'].agg(['mean', 'std'])
print(del_ori)


# Create a figure
fig, (ax1, ax2) = plt.subplots(2,1)

# Boxplot and barplot in the axes
sns.boxplot(x='origin', y='dep_delay', data=flights, ax=ax1)
sns.barplot(x='carrier', y="total_delay", data=tdel_car, ax=ax2)

# Label axes
ax1.set_title('Originating airport and the departure delay')

# Use tight_layout() so the plots don't overlap
fig.tight_layout()
plt.show()