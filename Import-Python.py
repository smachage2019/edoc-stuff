# -*- coding: utf-8 -*-
"""
Created on Wed Jul 24 22:34:12 2019
Importing Data
@author: hb24617
"""

# Chapter 1


# Plane text files
filename = 'hello.txt'
file = open(filename, mode='r') # 'r' is to read
text = file.read()
file.close()
print(text)

with open('hello.txt', 'r') as file:
    print(file.read())



# Open a file: file
file = open('moby_dick.txt', mode='r')
text = file.read()
# Print it
print(text)

# Check whether file is closed
print(file.closed)

# Close file
file.close()

# Check whether file is closed
print(file.closed)
######################################################
# Open a file: file
file = open('moby_dick.txt', 'r')

# Print it
print(file.read())

# Check whether file is closed
print(file.closed)

# Close file
file.close()

# Check whether file is closed
print(file.closed)

# Read & print the first 3 lines
with open('moby_dick.txt') as file:
    print(file.readline())
    print(file.readline())
    print(file.readline())
    print(file.readline())
    print(file.readline())
    print(file.readline())
    
######Flat Files##################
# are typically loaded into a NumPy array or into pandas
    # importing with NumPy - arrays standard for storing numerical data
import numpy as np
filename = 'MNIST_test.txt'
data = np.loadtxt(filename, delimiter=',')
data

    import numpy as np
filename = 'MNIST_test.txt'
data = np.loadtxt(filename, delimiter=',',
                  skiprows=1)
data

import numpy as np
filename = 'MNIST_test.txt'
data = np.loadtxt(filename, delimiter=',',
                  skiprows=1, usecols=[0, 2])
data

data = np.loadtxt(filename, delimiter=',',dtype=str)
#this method is not good for mixed data types like numeric and strings
file = 'digits.csv'
with open(file,'r') as file:
    print(file.read())
# Import package
import numpy as np

# Assign filename to variable: file
file = 'digits.csv'

# Load file as array: digits
digits = np.loadtxt(file, delimiter=',')

# Print datatype of digits
print(type(digits))

# Select and reshape a row
im = digits[21, 1:]
im_sq = np.reshape(im, (28, 28))

# Plot reshaped data (matplotlib.pyplot already loaded as plt)
plt.imshow(im_sq, cmap='Greys', interpolation='nearest')
plt.show()

# Import numpy
import numpy as np

# Assign the filename: file
file = 'digits_header.txt'

# Load the data: data
data = np.loadtxt(file, delimiter='\t', skiprows=1, usecols=[0,3])

# Print data
print(data)

# Assign filename: file
file = 'seaslug.txt'

# Import file: data
data = np.loadtxt(file, delimiter='\t', dtype=str)

# Print the first element of data
print(data[0])

# Import data as floats and skip the first row: data_float
data_float = np.loadtxt(file, delimiter='\t', dtype=float, skiprows=1)

# Print the 10th element of data_float
print(data_float[9])

# Plot a scatterplot of the data
plt.scatter(data_float[:, 0], data_float[:, 1])
plt.xlabel('time (min.)')
plt.ylabel('percentage of larvae')
plt.show()

#for mixed data types np.genfromtxt()
data = np.genfromtxt('titanic.csv', delimiter=',', names=True, dtype=None)
np.shape(data)
data['Fare']
data['Survived']

#np.recfromcsv()

# Assign the filename: file
file = 'titanic.csv'

# Import file using np.recfromcsv: d
d = np.recfromcsv(file, delimiter=',', names=True,dtype=None)

# Print out first three entries of d
print(d[:3])


##Pandas###
# gives you 2-d labeled data structures
import pandas as pd
filename = 'digits.csv'
data = pd.read_csv(filename)
data.head()
# convert to numpy array
data_array = data.values


# Import pandas as pd
import pandas as pd

# Assign the filename: file
file = 'titanic.csv'

# Read the file into a DataFrame: df
df = pd.read_csv(file)

# View the head of the DataFrame
df.head()


# Assign the filename: file
file = 'digits.csv'

# Read the first 5 rows of the file into a DataFrame: data
data = pd.read_csv(file, nrows=5, header=None)

# Build a numpy array from the DataFrame: data_array
data_array = data.values

# Print the datatype of data_array to the shell
print(type(data_array))


# Customizing your pandas import

# Import matplotlib.pyplot as plt
import matplotlib.pyplot as plt

# Assign filename: file
file = 'titanic_corrupt.txt'

# Import file: data
data = pd.read_csv(file, sep='\t', comment='#', na_values='Nothing')

# Print the head of the DataFrame
print(data.head())

# Plot 'Age' variable in a histogram
pd.DataFrame.hist(data[['Age']])
plt.xlabel('Age (years)')
plt.ylabel('count')
plt.show()


# Chapter 2
# Pickled files are serialized

import pickle
with open('pickled_fruit.pkl', 'rb') as file:
    data = pickle.load(file)
    
print(data)

# Excel
import pandas as pd
file = 'Lookup.xlsx'
data = pd.ExcelFile(file)
print(data.sheet_names)
df1 = data.parse('Sheet1')
df2 = data.parse(0)

# check directory
import os
wd = os.getcwd()
os.listdir(wd)

# Import pickle package
import pickle

# Open pickle file and load data: d
with open('data.pkl', 'rb') as file:
    d = pickle.load(file)

# Print d
print(d)

# Print datatype of d
print(type(d))


# Parse the first sheet and rename the columns: df1
df1 = xls.parse(0, skiprows=[0], names=['Country','AAM due to War (2002)'])

# Print the head of the DataFrame df1
print(df1.head())

# Parse the first column of the second sheet and rename the column: df2
df2 = xls.parse(1, usecols=[0], skiprows=[0], names=['Country'])

# Print the head of the DataFrame df2
print(df2.head())

# SAS/Stata files
# SAS: business analytics and biostatistics
# Stata: acedemic social sciences research

import pandas as pd
from sas7bdat import SAS7BDAT
with SAS7BDAT('urdanpp.sas7bdat') as file:
    df_sas = file.to_data_frame()
    
import pandas as pd
data = pd.read_stata('urbanpop.dta')

# Import sas7bdat package
from sas7bdat import SAS7BDAT

# Save file to a DataFrame: df_sas
with SAS7BDAT('sales.sas7bdat') as file:
    df_sas = file.to_data_frame()

# Print head of DataFrame
print(df_sas.head())

# Plot histogram of DataFrame features (pandas and pyplot already imported)
pd.DataFrame.hist(df_sas[['P']])
plt.ylabel('count')
plt.show()

# Import pandas
import pandas as pd

# Load Stata file into a pandas DataFrame: df
df = pd.read_stata('disarea.dta')

# Print the head of the DataFrame df
print(df.head())

# Plot histogram of one column of the DataFrame
pd.DataFrame.hist(df[['disa10']])
plt.xlabel('Extent of disease')
plt.ylabel('Number of countries')
plt.show()


#HDF5 files
# Hierarchical Data Format Version 5
# Good for storing large quantities of numerical data
# can scale up to exabytes

import h5py
filename = 'ligo.hdf5'
data = h5py.File(filename, 'r') 
print(type(data))
# shows the keys in the file
for key in data.keys():
    print(key)

# keys are like directories
    
print(type(data['meta']))
# shows the keys in each ~directory or datatype
for key in data['meta'].keys():
    print(key)
    
print(data['meta']['Description'].value, data['meta']['Detector'].value)

# Import packages
import numpy as np
import h5py

# Assign filename: file
file = 'ligo.hdf5'

# Load file: data
data = h5py.File(file, "r")

# Print the datatype of the loaded file
print(type(data))

# Print the keys of the file
for key in data.keys():
    print(key)

# Get the HDF5 group: group
group = data['strain']

# Check out keys of group
for key in group.keys():
    print(key)

# Set variable equal to time series data: strain
strain = data['strain']['Strain'].value

# Set number of time points to sample: num_samples
num_samples = 10000

# Set time vector
time = np.arange(0, 1, 1/num_samples)

# Plot data
plt.plot(time, strain[:num_samples])
plt.xlabel('GPS Time (s)')
plt.ylabel('strain')
plt.show()

pip install feather-format
#Matlab Matrix Laboratory
# .mat files
# SciPy to the rescue
# scipy.io.loadmat()
# scipy.io.savemat()

import scipy.io
filename = 'workspace.mat'
mat = scipy.io.loadmat(filename)
print(type(mat))
#<class 'dict'>
# keys = MATLAB variable names
# values = objects assigned to variables

print(type(mat['x']))
# <class 'numpy.ndarray'>

# Print the keys of the MATLAB dictionary
# Print the keys of the MATLAB dictionary
print(mat.keys())

# Print the type of the value corresponding to the key 'CYratioCyt'
print(type(mat['CYratioCyt']))

# Print the shape of the value corresponding to the key 'CYratioCyt'
print(np.shape(mat['CYratioCyt']))

# Subset the array and plot it
data = mat['CYratioCyt'][25, 5:]
fig = plt.figure()
plt.plot(data)
plt.xlabel('time (min.)')
plt.ylabel('normalized fluorescence (measure of expression)')
plt.show()


# Chapter 3
# Relational database 
# bunch of linked tables
# relational model
# Todd's 12 Rules/Commandments

#PostgreSQL
#MySQL
#SQLite
# SQL = Structured Query Language

# Creatintg a database engine
from sqlalchemy import create_engine
engine = create_engine('Northwind_large.sqlite')
table_names = engine.table_names()
print(table_names)


# Import necessary module
from sqlalchemy import create_engine

# Create engine: engine
engine = create_engine('sqlite:///Chinook.sqlite')

# Find Table names
# Import necessary module
from sqlalchemy import create_engine

# Create engine: engine
engine = create_engine('sqlite:///Chinook.sqlite')

# Save the table names to a list: table_names
table_names = engine.table_names()

# Print the table names to the shell
print(table_names)

# The "Hello World" of queries is SELECT * FROM Table_Name
# SQL query workflow:
#   - import packages and functions
#   - create database engine
#   - connect to the engine
#   - query the database
#   - save query results to dataframe
#   - close the connection
from sqlalchemy import create_engine
engine = create_engine('sqlite:///Northwind_large.sqlite')
table_names = engine.table_names()
print(table_names)
con = engine.connect()
rs = con.execute("SELECT * FROM Shipper")
df = pd.DataFrame(rs.fetchall())
df.columns = rs.keys()
con.close()
print(df.head())
# using construct
from sqlalchemy import create_engine
engine = create_engine('sqlite:///Northwind_large.sqlite')
with engine.connect() as con:
    rs = con.execute("SELECT * FROM Shipper WHERE CompanyName = 'Speedy Express'")
    df = pd.DataFrame(rs.fetchmany(size=2))
    df.columns = rs.keys()
print(df.head())

# Create engine: engine
engine = create_engine('sqlite:///Chinook.sqlite')

# Open engine in context manager
with engine.connect() as con:
    rs = con.execute("SELECT * FROM Employee ORDER BY BirthDate")
    df = pd.DataFrame(rs.fetchall())

    # Set the DataFrame's column names
df.columns = rs.keys()

# Print head of DataFrame
print(df.head())

# pandas one liner
df = pd.read_sql_query("SELECT * FROM Shipper WHERE CompanyName = 'Speedy Express'", engine)
print(df.head())

# Import packages
from sqlalchemy import create_engine
import pandas as pd

# Create engine: engine
engine = create_engine('sqlite:///Chinook.sqlite')

# Execute query and store records in DataFrame: df
df = pd.read_sql_query(
    "SELECT * FROM Employee WHERE EmployeeId >= 6 ORDER BY BirthDate",
    engine
)

# Print head of DataFrame
print(df.head())

# Inner join
# Open engine in context manager
# Perform query and save results to DataFrame: df
with engine.connect() as con:
    rs = con.execute("SELECT Title, Name FROM Album INNER JOIN Artist on Album.ArtistID = Artist.ArtistID")
    df = pd.DataFrame(rs.fetchall())
    df.columns = rs.keys()

# Print head of DataFrame df
print(df.head())


# Execute query and store records in DataFrame: df
df = pd.read_sql_query(
    "SELECT * FROM PlaylistTrack INNER JOIN Track ON PlaylistTrack.TrackId = Track.TrackId WHERE Milliseconds < 250000",
    engine
)

# Print head of DataFrame
print(df.head())



























