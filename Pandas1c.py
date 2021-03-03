# -*- coding: utf-8 -*-
"""
Created on Sun Mar  3 20:50:11 2019

@author: seanm
"""

import numpy as np
# %%
# Random Numbers - Pseudo-random,starting from seed
np.random.rand()

np.random.seed(123)
np.random.rand()
np.random.rand()
# Random, but consistent between runs
# %%
# Coin Toss
np.random.seed(123)
coin = np.random.randint(0, 2)
print(coin)
if coin == 0:
    print("heads")
else:
    print("tails")
# %%
# Set the seed
np.random.seed(123)

# Generate and print random float
print(np.random.rand())
# %%
# Use randint() to simulate a dice
dice = np.random.randint(1, 7)

# Generate and print random float
print(dice)

# Use randint() again
dice = np.random.randint(1, 7)
print(dice)
# %%
# Empire State Bet
# Starting step
step = 50

# Roll the dice
dice = np.random.randint(1, 7)

# Finish the control construct
if dice <= 2:
    step = step - 1
elif dice <= 5:
    step = step + 1
else:
    step = step + np.random.randint(1, 7)

# Print out dice and step
print(dice)
print(step)
# Random Walk is what this is if we repeat 100x
# %%
np.random.seed(123)
outcomes = []
# range defines how many times to run for
for x in range(10):
    coin = np.random.randint(0, 2)
    if coin == 0:
        outcomes.append("heads")
    else:
        outcomes.append("tails")
print(outcomes)
# not a random walk
# %%
np.random.seed(123)
tails = [0]
for x in range(10):
    coin = np.random.randint(0, 2)
    tails.append(tails[x] + coin)
print(tails)
# this is a random walk, due to incremental changes based on previous outcome
# %%
# Initialize random_walk
random_walk = [0]

# Complete the ___
for x in range(100):
    # Set step: last element in random_walk
    step = random_walk[-1]
    # Roll the dice
    dice = np.random.randint(1, 7)
    # Determine next step
    if dice <= 2:
        step = max(0, step - 1)
    elif dice <= 5:
        step = step + 1
    else:
        step = step + np.random.randint(1, 7)
        
    # append next_step to random_walk
    random_walk.append(step)

# Print random_walk
print(random_walk)
# %%
# Plotting
np.random.seed(123)
# Initialization
random_walk = [0]

for x in range(100):
    step = random_walk[-1]
    dice = np.random.randint(1, 7)

    if dice <= 2:
        step = max(0, step - 1)
    elif dice <= 5:
        step = step + 1
    else:
        step = step + np.random.randint(1, 7)

    random_walk.append(step)

# Import matplotlib.pyplot as plt
import matplotlib.pyplot as plt

# Plot random_walk
plt.plot(random_walk)


# Show the plot
plt.show()
# %%
# Distriburiton of Random Walks
np.random.seed(123)
final_tails = []
for x in range(1000000):
    tails = [0]
    for x in range(10):
        coin = np.random.randint(0, 2)
        tails.append(tails[x] + coin)
    final_tails.append(tails[-1])
plt.hist(final_tails, bins=10)
plt.show()
# %%
np.random.seed(123)
# Initialize all_walks (don't change this line)
all_walks = []

# Simulate random walk 10 times
for i in range(10):

    # Code from before
    random_walk = [0]
    for x in range(100):
        step = random_walk[-1]
        dice = np.random.randint(1, 7)

        if dice <= 2:
            step = max(0, step - 1)
        elif dice <= 5:
            step = step + 1
        else:
            step = step + np.random.randint(1, 7)
        random_walk.append(step)

    # Append random_walk to all_walks
    all_walks.append(random_walk)

# Print all_walks
print(all_walks)
# %%
np.random.seed(123)
# initialize and populate all_walks
all_walks = []
for i in range(10):
    random_walk = [0]
    for x in range(100):
        step = random_walk[-1]
        dice = np.random.randint(1, 7)
        if dice <= 2:
            step = max(0, step - 1)
        elif dice <= 5:
            step = step + 1
        else:
            step = step + np.random.randint(1, 7)
        random_walk.append(step)
    all_walks.append(random_walk)

# Convert all_walks to Numpy array: np_aw
np_aw = np.array(all_walks)

# Plot np_aw and show
plt.plot(np_aw)
plt.show()

# Clear the figure
plt.clf()

# Transpose np_aw: np_aw_t
np_aw_t = np.transpose(np_aw)

# Plot np_aw_t and show
plt.plot(np_aw_t)
plt.show()
# %%
np.random.seed(123)
# Simulate random walk 250 times
all_walks = []
for i in range(250):
    random_walk = [0]
    for x in range(100):
        step = random_walk[-1]
        dice = np.random.randint(1, 7)
        if dice <= 2:
            step = max(0, step - 1)
        elif dice <= 5:
            step = step + 1
        else:
            step = step + np.random.randint(1, 7)

        # Implement clumsiness
        if np.random.rand() <= 0.001:
            step = 0

        random_walk.append(step)
    all_walks.append(random_walk)

# Create and plot np_aw_t
np_aw_t = np.transpose(np.array(all_walks))
# plt.plot(np_aw_t)
# plt.show()
# Select last row from np_aw_t: ends
ends = np_aw_t[-1]

# Plot histogram of ends, display plot
plt.hist(ends)
"""
The histogram of the previous exercise was created
from a Numpy array ends, that contains 500 integers.
Each integer represents the end point of a random walk.
To calculate the chance that this end point is greater
than or equal to 60, you can count the number of integers
in ends that are greater than or equal to 60 and divide
that number by 500, the total number of simulations.
"""
sum = np.mean(ends >= 60)
print(sum)
# 0.784
sum * 100
# 78.4%
