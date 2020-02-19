#!/usr/bin/python3
import sys

sys.path.append('/home/jon/Desktop/Phage2Shark')

import subprocess
import os
import datetime
import shutil
import matplotlib.pyplot as plt
import Wrapp_funcs as fns

# Python wrapper for the P2S project.
# Handles input and data for the Fortran code.

# Jon Parsons
# 10-3-19


## Set up - input values; labels and names for outputs
# Calls functions contained in Wrapp_funcs.py

## TODO:
# Remove the following
# Bacterial growth
# Phage decay rate
# Bacterial decay rate
# lysogen growth rate modifier
# bacterial interactions strength
# Newcoral Threshold

# Date
time = datetime.datetime.now()
time_vals = "{}{}{}".format(time.day,time.month,time.year)

# Values for input
grid = 100 # grid size
numT = 500 # Number of time steps
corcov_ini = 0.5 # Initial coral coverage
pisc_mass = 20 # Piscivore mass
hunt_avg = 6 # Average number of days between a succesful piscivore hunt
burst = 50 # Burst size of lytic event
fish_ini = 900*corcov_ini # Initial fish population
f_Grate = 0.003 # Fish growth rate
diff_co = 0.01 # Diffusion pressure for bacterial layer
dis_flag = 'N' # Flag for disaster events N - None
dis_lev = 5 # Severity of disasters
adj_flag = 'N' # Flag to indicate a change in values at the half-way point
adj_time = numT/2
## Values for second half if a change is indicated
pisc_mass_2 = 20 # Piscivore mass
hunt_avg_2 = 6 # Average number of days between a succesful piscivore hunt
burst_2 = 50 # Burst size of lytic event
fish_ini_2 = 600 # Fish population adjusted
f_Grate_2 = 0.003 # Fish growth rate
diff_co_2 = 0.01 # Diffusion pressure for bacterial layer

in_vals = [grid,numT,corcov_ini,pisc_mass,hunt_avg,burst,fish_ini,f_Grate,
            diff_co,dis_flag,dis_lev,adj_flag,adj_time,pisc_mass_2,
            hunt_avg_2,burst_2,f_Grate_2,diff_co_2]

# Names of output files
out_files = ["coral_fraction","coral_total","coral_average","coral_delta",
            "bact_pop","phage_pop","lys_pop","bact_spec","phage_spec",
            "lys_spec","fish_pop","fish_delta","algae_perc","phage_lys_rat",
            "shark_evt","vmr","time"]

## Initial user inputs. Determine type of run. Single, ranged, averaging ##
choice_list = ['S','R','A']

_=os.system('clear')
print("Welcome to Phage to Shark \n")
print("Current Values of input parameters:")
print("Initial Coral Coverage {} | Piscivore Mass {}".format(corcov_ini,pisc_mass))
print("Hunting Success Average {}| Burst Size {}".format(hunt_avg,burst))
print("Initial Fish Population {}| Fish Growth Rate {}".format(fish_ini,f_Grate))
print("Diffustion Coefficient {}".format(diff_co))
if dis_flag == 'N':
    print("No Disease")
else:
    print("Disease level set to {}".format(dis_lev))

if adj_flag == 'N':
    print("No Adjustments")
else:
    print("Adjustments will occur at timesteo {}".format(adj_time))
        
print("\nTo run a single simulation Enter 'S'\n", \
     "To run a set of simulations with an iterated variable enter 'R' \n" \
     "To run a set of simulations and average the results enter 'A' \n")

# Validate input
while True:
    type = input("Choice: ")
    type = type.upper()
    # Check that choice is valid
    if type not in choice_list:
        print("\n Choice not recognized. Please try again. \n")
        continue
    # Continue if in list
    else:
        break

if type == 'S':
    fns.single(in_vals,out_files,time_vals)

if type == 'R':
    fns.ranged(in_vals,out_files,time_vals)
