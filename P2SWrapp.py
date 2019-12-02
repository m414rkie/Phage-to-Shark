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

# Date
time = datetime.datetime.now()
time_vals = "{}{}{}".format(time.day,time.month,time.year)

# Values for input
grid = 100 # grid size
numT = 2000 # Number of time steps
corcov_ini = 0.5 # Initial coral coverage
nwcor_thr = 1.0 # Lower bound for average coral health that can make new coral
pisc_mass = 20 # Piscivore mass
hunt_avg = 6 # Average number of days between a succesful piscivore hunt
b_Grate = 1.0 # Bacterial growth rate
b_corN_str = 1.0 # Bacterial interaction strength with new coral spawning
Ads_fac = 1.0 # Adsorption factor coefficient. Legacy
b_death = 0.5 # Natural rate of bacterial death
burst = 50 # Burst size of lytic event
p_death = 0.5 # Natural rate of phage decay
f_eat = 500 # Multiplier of fish effects on algae
f_Grate = 0.003 # Fish growth rate
diff_co = 0.01 # Diffusion pressure for bacterial layer
ly_mod = 1.5 # Modifier for Lysogen growth rate
dis_flag = 'N' # Flag for disaster events N - None
dis_lev = 5 # Severity of disasters
adj_flag = 'N' # Flag to indicate a change in values at the half-way point
## Values for second half if a change is indicated
nwcor_thr_2 = 1.0 # Lower bound for average coral health that can make new coral
pisc_mass_2 = 20 # Piscivore mass
hunt_avg_2 = 6 # Average number of days between a succesful piscivore hunt
b_Grate_2 = 1.0 # Bacterial growth rate
b_corN_str_2 = 1.0 # Bacterial interaction strength with new coral spawning
Ads_fac_2 = 1.0 # Adsorption factor coefficient. Legacy
b_death_2 = 0.5 # Natural rate of bacterial death
burst_2 = 50 # Burst size of lytic event
p_death_2 = 0.5 # Natural rate of phage decay
f_eat_2 = 260 # Multiplier of fish effects on algae
f_Grate_2 = 0.003 # Fish growth rate
diff_co_2 = 0.01 # Diffusion pressure for bacterial layer

shift = 16 # amt to shift for output graphing

in_vals = [grid,numT,corcov_ini,nwcor_thr,pisc_mass,hunt_avg,b_Grate,b_corN_str,
            Ads_fac,b_death,burst,p_death,f_eat,f_Grate,diff_co,
            ly_mod,dis_flag,dis_lev,adj_flag,nwcor_thr_2,pisc_mass_2,
            hunt_avg_2,b_Grate_2,b_corN_str_2,Ads_fac_2,b_death_2,
            burst_2,p_death_2,f_eat_2,f_Grate_2,diff_co_2]

# Names of output files
out_files = ["coral_fraction","coral_total","coral_average","coral_delta",
            "bact_pop","phage_pop","lys_pop","bact_spec","phage_spec",
            "lys_spec","fish_pop","fish_delta","algae_perc","phage_lys_rat",
            "shark_evt","vmr","time"]

## Initial user inputs. Determine type of run. Single, ranged, averaging ##
choice_list = ['S','R','A']

_=os.system('clear')
print("Phage to Shark \n")
print("To run a single simulation Enter 'S' \n", \
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
