# -*- coding: utf-8 -*-
"""
Created on Mon Mar  1 15:59:50 2021

@author: hb24617
"""
import numpy as np
import pandas as pd

df = pd.read_csv("E:\\python\\Values-use-tidy.csv")

df.shape
df.columns

# drop columns
df.drop(['Coord System', 'EPSG', 'Acquisition Date (Local)', 'Acquisition Time (Local)','North Reference  - Local Tangent Plane'], axis=1, inplace=True)

df.shape

# Select particular columns while reading
df_spec = pd.read_csv("E:\\python\\Values-use-tidy.csv", usecols=['Easting ft (Abs)', 'Northing ft (Abs)', 'Depth ft (TVDSS)'])

# Reading a part of the dataframe
df_partial = pd.read_csv("E:\\python\\Values-use-tidy.csv", nrows=7609)
df_partial.shape

# sampling a dataframe for testing
# n: The number of rows in the sample
# frac: The ratio of the sample size to the whole dataframe size
df_sample = df.sample(n=1000)
df_sample.shape

df_sample2 = df.sample(frac=0.1)
df_sample2.shape

# checking the missing values
df.isna().sum()

# Filling missing values
# fill with mode
mode = df['Moment'].value_counts().index[0]
df['Moment'].fillna(value=mode, inplace=True)
df['Moment'].value_counts()
# fill with mean
avg = df['Cum Moment'].mean()
df['Cum Moment'].fillna(value=avg, inplace=True)

df.isna().sum().sum()

# dropping missing values
df = pd.read_csv("E:\\python\\Values-use-tidy.csv")
df.isna().sum()
df.dropna(axis=0, how='any', inplace=True)
df.isna().sum().sum()
df.shape
df.tail()

# selecting rows based on conditions
msm_data = df[(df.Status == 'MSM') & (df.Solution == '3SA-post')]

# query data
df2 = df.query('-2 < Magnitude < 0')
df['Magnitude'].plot(kind='hist', figsize=(8,5))
df2['Magnitude'].plot(kind='hist', figsize=(8,5))

mags = df[df['Magnitude'].isin([-2,-1,-3,-3.4])][:20]

# The groupby function
stats = df[['Well','Stage','Solution','Magnitude','Confidence','Distance From Perfs']].groupby(['Well','Stage','Solution']).mean()
stats.to_csv (r'E:/python/stats.csv', index = False, header=True)
df[['Well','Stage','Solution','Magnitude','Confidence','Distance From Perfs']].groupby(['Well','Stage','Solution']).agg(['mean','count','max','min'])

# the where function
df['Balance'] = df['Same'].where(df['Same'] >= 1, 0)
df

# rank
df['rank'] = df['Stage'].rank(method='first', ascending=False).astype('int')
df

# Number of unique values in a column
df.Well.nunique()
df.Stage.nunique()

df.memory_usage() # memmory usage in bytes

# Reducing the decimal points of floats
df.round(1) # number of desired decimals

# Calculating the percentage change through a column
ser = df['Elapsed_Time']
ser.pct_change()

# Filtering based on strings
df[df.Status.str.startswith('M')]

df.style.highlight_max(axis=0, color='darkgreen')
