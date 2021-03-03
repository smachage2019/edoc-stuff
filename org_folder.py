# -*- coding: utf-8 -*-
"""
Created on Wed Feb 27 10:03:16 2019

@author: hb24617
"""
#organize a folder by modified date
import os
import shutil
import os.path, time
from datetime import datetime
 
path = 'C:\\Users\\NITIN\\Desktop\\New folder'
 
lis = os.listdir(path)
lis.sort(key=lambda x: os.stat(os.path.join(path, x)).st_mtime)
 
# List only the files in the folder
files = [f for f in os.listdir(path) if os.path.isfile(os.path.join(path, f))]
 
 
# In[51]:
 
# change the current path 
os.chdir(path)
for x in files:
    # Get the last modified time and the creation time
    creation_time_string = time.ctime(os.path.getmtime(os.path.join(path, x)))
    modified_time_string = time.ctime(os.path.getmtime(os.path.join(path, x)))
    creation_datetime_obj = datetime.strptime(creation_time_string, '%a %b %d %H:%M:%S %Y')
    modified_datetime_obj = datetime.strptime(modified_time_string, '%a %b %d %H:%M:%S %Y')
    modified_date  = str(modified_datetime_obj.day) + '-' + str(modified_datetime_obj.month) + '-' + str(modified_datetime_obj.year)
    # print(modified_date)
    
    if(os.path.isdir(modified_date)):
         shutil.move(os.path.join(path, x), modified_date)
    else:
        os.makedirs(modified_date)
        shutil.move(os.path.join(path, x), modified_date)