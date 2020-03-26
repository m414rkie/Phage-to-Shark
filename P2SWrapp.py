#!/usr/bin/python3
import sys

#sys.path.append('/home/jon/Desktop/Phage2Shark')

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

# Date
time = datetime.datetime.now()
time_vals = "{}_{}_{}".format(time.day,time.month,time.year)

# Values for input
grid = 100 # grid size
numT = 350 # Number of time steps
corcov_ini = 0.5 # Initial coral coverage
pisc_mass = 15 # Piscivore mass
hunt_avg = 4 # Average number of days between a succesful piscivore hunt
burst = 10 # Burst size of lytic event
fish_ini = 800*corcov_ini # Initial fish population
f_Grate = 0.003 # Fish growth rate
diff_co = 0.01 # Diffusion pressure for bacterial layer
dis_flag = 'N' # Flag for disaster events N - None
dis_lev = 5 # Severity of disasters
adj_flag = 'N' # Flag to indicate a change in values at the half-way point
adj_time = numT/2
seed = 0
## Values for second half if a change is indicated
pisc_mass_2 = 20 # Piscivore mass
hunt_avg_2 = 6 # Average number of days between a succesful piscivore hunt
burst_2 = 50 # Burst size of lytic event
fish_ini_2 = 600 # Fish population adjusted
f_Grate_2 = 0.003 # Fish growth rate
diff_co_2 = 0.01 # Diffusion pressure for bacterial layer

# Names of output files
out_files = ["coral_fraction","coral_total","coral_average","coral_delta",
            "bact_pop","phage_pop","lys_pop","bact_spec","phage_spec",
            "lys_spec","fish_pop","fish_delta","algae_perc","phage_lys_rat",
            "shark_evt","vmr","time"]

# Initial user inputs. Determine type of run. Single, ranged, averaging ##
choice_list = ['S','R','A']
seed_list = ['I','P']

_=os.system('clear')
print("Welcome to Phage to Shark \n")
print("Current Values of input parameters:")
print("Initial Coral Coverage  {} \nPiscivore Mass          {}".format(corcov_ini,pisc_mass))
print("Hunting Success Average {} \nBurst Size              {}".format(hunt_avg,burst))
print("Initial Fish Population {} \nFish Growth Rate        {}".format(fish_ini,f_Grate))
print("Diffusion Coefficient   {}".format(diff_co))
print("\nFlags:")
if dis_flag == 'N':
    print("No Disease")
else:
    print("Disease level set to {}".format(dis_lev))

if adj_flag == 'N':
    print("No Adjustments")
else:
    print("Adjustments will occur at timestep {}".format(adj_time))

# Determine initial values to be used
adj_flg = input("\nAdjust any of these values? (Y/N) ")
adj_flg = adj_flg.upper()

if adj_flg == 'Y':
    corcov_ini,pisc_mass,hunt_avg,burst,fish_ini,f_Grate,diff_co,dis_flag,dis_lev, \
    adj_flag,adj_time,pisc_mass_2,hunt_avg_2,burst_2,fish_ini_2,f_Grate_2,diff_co_2 \
    = fns.ini_adj(corcov_ini,pisc_mass,hunt_avg,burst,fish_ini,f_Grate,diff_co,dis_flag, \
    dis_lev,adj_flag,adj_time,pisc_mass_2,hunt_avg_2,burst_2,fish_ini_2,f_Grate_2,diff_co_2)

# Random seed generation
print("\nGenerate a Random Seed (P) or Input a seed (I)?")
print("\nPlease note that choosing a 'stats' run will generate a different seed \
        for each run.")

while True:
    sd_flag = input("Choice: ")
    sd_flag = sd_flag.upper()
    if sd_flag not in seed_list:
        print("\n Choice not recognized. Please try again. \n")
        continue
    else:
        break

if sd_flag == 'I':
    try:
        seed_in = int(input("Enter an integer to be used as the seed: "))
    except ValueError:
        print("Not an integer, please try again")

    seed = seed_in

if sd_flag == 'P':
    print("A random seed will be generated")
    seed = time.day+time.microsecond+time.second
    print("The random seed is {}".format(seed))

# Get number of timesteps
try:
    t_steps = int(input("Enter the number of timesteps to simulate (350 default): ") or '0')
except ValueError:
    print("Time requires an integer, please try again")

if t_steps == 0:
    t_steps = 350

numT = t_steps

# Insert seed value to parameter list
in_vals = [grid,numT,corcov_ini,pisc_mass,hunt_avg,burst,fish_ini,f_Grate,
            diff_co,dis_flag,dis_lev,adj_flag,adj_time,seed,pisc_mass_2,
            hunt_avg_2,burst_2,f_Grate_2,diff_co_2]

print("\nTo run a single simulation Enter 'S'")
print("To run a set of simulations with an iterated variable enter 'R'")
print("To run a set of simulations with the same parameters enter 'A'")

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

# Runs
if type == 'S':
    fns.single(in_vals,out_files,time_vals)

if type == 'R':
    fns.ranged(in_vals,out_files,time_vals)

if type == 'A':
    fns.stats_run(in_vals,out_files,time_vals)
