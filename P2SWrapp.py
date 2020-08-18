#!/usr/bin/python3
import sys
import subprocess
import os
import datetime
import shutil
import matplotlib.pyplot as plt
import Wrapp_funcs as fns # Assumes same directory as Wrapp_funcs.py

# Python wrapper for the P2S project.
# Handles input and data for the Fortran code.

# Jon Parsons
# 10-3-19

# Calls functions contained in Wrapp_funcs.py

# Date
time = datetime.datetime.now()
time_vals = "{}_{}_{}".format(time.day,time.month,time.year)

# Values for input
grid = 100 # grid size, indice 0
numT = 350 # Number of time steps, indice 1
corcov_ini = 0.5 # Initial coral coverage, indice 2
pisc_mass = 20 # Piscivore mass, indice 3
hunt_avg = 8 # Average number of days between a succesful piscivore hunt, indice 4
burst = 50 # Burst size of lytic event, indice 5
fish_ini = 800*corcov_ini # Initial fish population, indice 6
f_Grate = 0.003 # Fish growth rate, indice 7
diff_co = 0.1 # Diffusion pressure for bacterial layer, indice 8
dis_flag = 'N' # Flag for disaster events N - None, indice 9
dis_lev = 5 # Severity of disasters, indice 10
adj_flag = 'N' # Flag to indicate a change in values at the half-way point, indice 11
adj_time = numT/2#  indice 12
seed = 0 #  indice 13
## Values for second half if a change is indicated
pisc_mass_2 = 20 # Piscivore mass, indice 14
hunt_avg_2 = 6 # Average number of days between a succesful piscivore hunt, indice 15
burst_2 = 50 # Burst size of lytic event, indice 16
fish_ini_2 = 600 # Fish population adjusted, indice 17
f_Grate_2 = 0.003 # Fish growth rate, indice 18
diff_co_2 = 0.1 # Diffusion pressure for bacterial layer, indice 19

# vector containing simulation parameters
in_vals = [grid,numT,corcov_ini,pisc_mass,hunt_avg,burst,fish_ini,f_Grate,
            diff_co,dis_flag,dis_lev,adj_flag,adj_time,seed,pisc_mass_2,
            hunt_avg_2,burst_2,fish_ini_2,f_Grate_2,diff_co_2]

# Names of output files
out_files = ["coral_fraction","coral_total","coral_average","coral_delta",
            "bact_pop","phage_pop","lys_pop","bact_spec","phage_spec",
            "lys_spec","fish_pop","fish_delta","algae_perc","phage_lys_rat",
            "shark_evt","vmr","time"]

# Initial user inputs. Determine type of run. Single, ranged, averaging ##
choice_list = ['S','R','C']
seed_list = ['I','P']
# Clear terminal and set for user input
_=os.system('clear')
print("Welcome to Phage to Shark \n")
print("Current Values of input parameters:")
print("Initial Coral Coverage  {} \nPiscivore Mass          {}".format(corcov_ini,pisc_mass))
print("Hunting Success Average {} \nBurst Size              {}".format(hunt_avg,burst))
print("Initial Fish Population {} \nFish Growth Rate        {}".format(fish_ini,f_Grate))
print("Diffusion Coefficient   {}".format(diff_co))
print("\nFlags:") # print flags and severity if flags are not 'N'
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
    in_vals = fns.ini_adj(in_vals)

# Random or user seed generation
print("\nGenerate a Random Seed (P) or Input a seed (I)?")

# check user input
while True:
    sd_flag = input("Choice: ")
    sd_flag = sd_flag.upper()
    if sd_flag not in seed_list:
        print("\n Choice not recognized. Please try again. \n")
        continue
    else:
        break
# for user set seed
if sd_flag == 'I':
    while True:
        try:
            seed_in = int(input("Enter an integer to be used as the seed: "))
        except ValueError:
            print("Not an integer, please try again")
            continue
        break
    in_vals[13] = seed_in
# for random(ish) seed
if sd_flag == 'P':
    print("A random seed will be generated")
    seed = time.day+time.microsecond+time.second
    print("The random seed is {}".format(seed))

# user input for type of run
print("\nTo run a single simulation Enter 'S'")
print("To run a set of simulations with an iterated variable enter 'R'")
print("To run a set of calibrations enter 'C'")

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
if type == 'C':
    fns.stability_run(in_vals,out_files,time_vals)

                            #### END MAIN  ####
################################################################################
