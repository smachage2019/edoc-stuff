# -*- coding: utf-8 -*-
"""
Created on Tue Jul 30 14:52:44 2019
Pandas Foundations
@author: hb24617
"""

#  dataset
print(aapl.head())
type(aapl)
aapl.shape
aapl.columns
aapl.index
# slicing

aapl.iloc[:5,:]
aapl.iloc[-5:,:]
aapl.tail()
aapl.info()

# Broadcasting - assign NAN to every 3rd element of last column
import numpy as np
aapl.iloc[::3, -1] = np.nan # assigning scalar value to column, slice bradcasts value to each row
aapl.head(6)
aapl.info()

# Series
low = aapl['low']
type(low)
low.head()
lows= low.values
type(lows)

df.iloc[::1].tail()
df.iloc[::1].head()

# Import numpy
import numpy as np

# Create array of DataFrame values: np_vals
np_vals = df.values

# Create new array of base 10 logarithm values: np_vals_log10
np_vals_log10 = np.log10(np_vals)

# Create array of new DataFrame by passing df to np.log10(): df_log10
df_log10 = np.log10(df)

# Print original and new data containers
[print(x, 'has type', type(eval(x))) for x in ['np_vals', 'np_vals_log10', 'df', 'df_log10']]

# Building dataframe from scratch
import pandas as pd
data = {'weekday': ['Sun', 'Sun', 'Mon', 'Mon'],
        'city': ['Austin', 'Dallas', 'Austin', 'Dallas'], 
                 'visitors': [139, 237, 326, 456],
                 'signups': [7,12,3,5]}
users = pd.DataFrame(data)
print(users)

# another way
cities = ['Austin', 'Dallas', 'Austin', 'Dallas']
signups = [7,12,3,5]
visitors = [139, 237, 326, 456]
weekdays = ['Sun', 'Sun', 'Mon', 'Mon']
list_labels = ['city', 'signups', 'visitiors', 'weekday']
list_cols = [cities, signups, visitors, weekdays]
zipped = list(zip(list_labels, list_cols)) 
print(zipped)
data = dict(zipped)
users = pd.DataFrame(data)
print(users)

# broadcasting cont
users['fees'] = 0 #broadcasts to entire column
print(users)

# set row and column names
heights = [ 59.0, 65.2, 62.9, 65.4, 63.7, 65.7, 64.1]
data = {'height': heights, 'sex': 'M'}
results = pd.DataFrame(data)
print(results) # M is broadcasted to entire column Sex
results.columns = ['height (in)', 'sex']
results.index = ['A', 'B', 'C', 'D', 'E', 'F', 'G']
print(results)

# importing & exporting
import pandas as pd
filepath = 'C:/Users/hb24617/Documents/Python/pandas/ISSN_D_tot.csv'
sunspots = pd.read_csv(filepath)
sunspots.info()

sunspots.iloc[10:20, :]
 # problems, no column headers
 # website gives clues:
 # column 0-2 gregorian date (y, m, d)
 # column 3 Date as fraction of year
 # daily total sunspot number
 # definitive/provision indicator (confidence) 1 or 0
 # missing values in column 4 indicated by -1
 
 sunspots = pd.read_csv(filepath, header=None)
sunspots.iloc[10:20, :]
col_names = ['year', 'month', 'day', 'dec_date', 'sunspots', 'definite']
sunspots = pd.read_csv(filepath, header=None, names=col_names)
sunspots.iloc[10:20, :]
sunspots = pd.read_csv(filepath, header=None, names=col_names, na_values={'sunspots':[' -1']},
                                                                          parse_dates=[[0, 1, 2]])
sunspots.iloc[10:20, :]
sunspots.info()

sunspots.index = sunspots['year_month_day']
sunspots.index.name = 'date'
sunspots.info()

# trimming
cols = ['sunspots', 'definite']
sunspots = sunspots[cols]
sunspots.iloc[10:20, :]

#writing files
out_csv = 'sunspots.csv'
sunspots.to_csv(out_csv)
out_tsv = 'sunspots.tsv'
sunspots.to_csv(out_tsv, sep='\t')
out_xlsx = 'sunspots.xlsx'
sunspots.to_excel(out_xlsx)

file_messy = 'C:/Users/hb24617/Documents/Python/pandas/messy_stock_data.tsv'
# Read the raw file as-is: df1
df1 = pd.read_csv(file_messy)

# Print the output of df1.head()
print(df1.head())

# Read in the file with the correct parameters: df2
df2 = pd.read_csv(file_messy, delimiter=' ', header=3, comment='#')

# Print the output of df2.head()
print(df2.head())

# Save the cleaned up DataFrame to a CSV file without the index
df2.to_csv(file_clean, index=False)

# Save the cleaned up DataFrame to an excel file without the index
df2.to_excel('file_clean.xlsx', index=False)

# plotting
apple = 'C:/Users/hb24617/Documents/Python/pandas/aapl.csv'
import matplotlib.pyplot as plt
appl = pd.read_csv(apple, index_col='date', parse_dates=True)
appl.head(6)
close_arr = appl['close'].values
type(close_arr)
plt.plot(close_arr)

close_series = aapl['close']
type(close_series)
plt.plot(close_series)

close_series.plot() # plot series directly
plt.show()

aapl.plot() # all series at once
plt.show()