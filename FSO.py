# -*- coding: utf-8 -*-
"""
Created on Tue Mar  2 11:26:18 2021

@author: hb24617
"""
# File System Operations

# Show Current Directory
import os
os.getcwd()

print(f'Current Working Directory is: {os.getcwd()}')

# Check Directory or Existing File
os.path.exists('python')

os.path.exists('python/tst.py')

# Join Path components, this is better for cross platform.
os.path.exists(os.path.join('python','tst.py'))

# Create a Directory
os.mkdir('test_dir')

print(f"test_dir existing: {os.path.exists('test_dir')}")

os.makedirs(os.path.join('test_dir', 'level_1', 'level_2', 'level_3'))

# Show Directory Content
os.listdir('test_dir')
os.listdir(os.getcwd())

from glob import glob
list(glob(os.path.join(os.getcwd(),'*.wav')))

# move files, moves .wavs
import shutil
for file in list(glob(os.path.join(os.getcwd(),'*.wav'))):
    shutil.move(file, 'test_dir')
# puts them back    
for file in list(glob(os.path.join('test_dir', '*.wav'))):
    os.rename(
        file, 
        os.path.join(
            os.getcwd(), 
            os.path.basename(file)
    ))
    
# copy files
shutil.copy(
    os.path.join(os.getcwd(), 'rec.wav'),
    os.path.join('test_dir')
)

# copy and rename
shutil.copy(
    os.path.join(os.getcwd(), 'rec.wav'),
    os.path.join('test_dir', 'rec(1).wav')
)

# Delete Directories or Files
os.remove(os.path.join('test_dir', 'rec(1).wav'))
os.rmdir(os.path.join('test_dir', 'level_1', 'level_2', 'level_3')) # not work
os.rmdir(os.path.join('test_dir', 'level 1')) # not work

shutil.rmtree(os.path.join('test_dir', 'level_1'))
