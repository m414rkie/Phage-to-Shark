#!/usr/bin/python3

import subprocess
import os
import glob
import datetime
import shutil
import matplotlib.pyplot as plt

# Python wrapper for the P2S project.
# Handles input and data for the Fortran code.

# Jon Parsons
# 10-3-19

################################################################################
# Subroutine to determine if a directory exists. If directory exists, append a
#  numeric modifier and recheck. Returns directory name with lowest appended
#  numeric
def dir_find(direc):
    # direc - original directory name, char
    i = 0

    dirchk = direc

    flag = False

    while os.path.isdir(dirchk):
        i += 1
        dirchk = direc + "_{}".format(i)

    if i > 1:
        direc = direc + "_{}".format(i-1)

    return direc
    # modified return of input

################################################################################
# Subroutine to graph outputs based on data held in external files.
def graph(xfil,yfil,xnum,ynum,flg,vals,dire,vr_nm):
    # xfil  - file that holds x-axis values
    # yfil  - file that holds y-axis values
    # xnum  - indice of unit array for x-axis
    # ynum  - indice of unit array for y-axis
    # flg   - flag for indicating which type of simulation was run
    #           flg == 1 -> single ; flg == 2 -> ranged ; flg == 3 -> stats
    # vals  - if simulation was 'ranged' holds values of that variable, else
    #         not used
    # dire  - holds directory to find datafiles subfolder in as well as where to
    #         place completed graphs
    # vr_nm - if simulation was 'ranged', holds which variable was iterated,
    #         else not used

    # Change to subfolder with data files
    dire2 = dir_find(dire)
    os.chdir(dire2+"/General")
    # Holds units for plotting
    units = ["","(BU)","(BU/25cm^2)","(dBU/25cm^2)","(1/ml)","(1/ml)","(1/ml)",
            "(1/ml)","(1/ml)","(1/ml)","(FM)","(dFM)","","","","","(Days)"]

    # determine number of files needed for the type of run
    if flg == 1:
        nm = 1
    elif flg == 2:
        nm = 5
    else:
        nm = 9
    # set title of plot
    if flg == 2:
        plt_title = "{} - {} \n Variable: {}".format(xfil,yfil,vr_nm)
    else:
        plt_title = "{} - {}".format(xfil,yfil)
    # determine axis labels, name and append to plot
    x_lab = xfil + " " + units[xnum]
    y_lab = yfil + " " + units[ynum]
    plt_name = "{}{}.png".format(xfil,yfil)
    plt.ticklabel_format(useOffset=False)
    plt.title(plt_title)
    plt.xlabel(x_lab)
    plt.ylabel(y_lab)

    # data read and plotting loop
    i = 1 # counting indice
    while i <= nm:
        # initialize data vectors
        x_data = []
        y_data = []
        # obtain file names
        if flg == 1:
            xfile = xfil + ".dat"
            yfile = yfil + ".dat"
        elif flg == 2 or flg == 3: # same naming format for ranged and stat
            xfile = xfil + "{}.dat".format(i)
            yfile = yfil + "{}.dat".format(i)
        else:
            print("ERROR: Unknown Graph flag.")

        # open files and read in, splits by space or tab
        with open(xfile,'r') as x:
            x_dat = x.read().splitlines()
        with open(yfile,'r') as y:
            y_dat = y.read().splitlines()
        # parse the input data to plotting vectors
        for line in x_dat:
            dxt = line.split()
            x_data.append(float(dxt[0]))
        for lines in y_dat:
            dyt = lines.split()
            y_data.append(float(dyt[0]))
        # Plot data
        if flg == 3:
            plt.scatter(x_data,y_data)
        else:
            plt.plot(x_data,y_data)

        i += 1 # update count

    if flg == 2: # apply legend for 'range' runs
            plt.legend([vals[0],vals[1],vals[2],vals[3],vals[4]], loc='upper right')

    # save externally and clear
    plt.savefig(plt_name,bbox_inches='tight')
    plt.clf()
    plt_dir = dire2 + "/General/{}".format(plt_name)
    # copy plot to correct folder
    shutil.copy(plt_dir,dire2)
    # user information
    print("Graph can be found in {}".format(plt_dir))
    # No returns

################################################################################
# Subroutine for user to choose which data to graph against each other
def graph_choice(ndir,datfiles,t_flag,var_vals,vr_nm):
    # ndir     - parent directory
    # datfiles - contains the names of the data files
    # t_flag   - flag containing the type of run ;  t_flag == 1 -> single
    #               t_flag == 2 -> ranged ; t_flag == 3 -> stat
    # var_vals - if ranged, contains values of selected variable
    # vr_nm    - if ranged, contains which variable was iterated

    # change to correct directory
    os.chdir(ndir+"/General")
    exit_input = ' ' # holds value for exit

    # Default graphs made here
    # contains datfiles vector indices for the data to plot automatically
    comp_list = [11,5,6,7,8,10,9,16]
    # Explicit commands for plotting non-time data
    graph(datfiles[10],datfiles[4],10,4,t_flag,var_vals,ndir,vr_nm)
    graph(datfiles[10],datfiles[5],10,5,t_flag,var_vals,ndir,vr_nm)
    graph(datfiles[5],datfiles[4],5,4,t_flag,var_vals,ndir,vr_nm)
    for val in comp_list: # iterate through list and plot against time, coral
        graph(datfiles[0],datfiles[val-1],0,val-1,t_flag,var_vals,ndir,vr_nm)
        graph(datfiles[16],datfiles[0],0,val-1,t_flag,var_vals,ndir,vr_nm)

    # Becomes false when user enters the exit_input char
    while True:
        # reset terminal, display choices
        _=os.system('clear')
        print("Choose 2 sets of data to plot against each other. Enter nothing \
                to exit. One domain per graph please.")
        print("Coral Percentage    -  1 | Coral Total          - 2")
        print("Coral Average       -  3 | Coral Delta          - 4")
        print("Bacteria Population -  5 | Phage Population     - 6")
        print("Lysogen Population  -  7 | Bacteria Species     - 8")
        print("Phage Species       -  9 | Lysogen Species      - 10")
        print("Fish Population     - 11 | Fish Delta           - 12")
        print("Algae Percentage    - 13 | Phage-Lysogen Ratio  - 14")
        print("Shark Events        - 15 | VMR                  - 16")
        print("Time                - 17 | --------- \n")
        print("Coral Domain      |   Algae Domain      |   Barrier Domain")
        print("                  |                     |                    ")
        print("Bacteria pop - 21 |   Bacteria pop - 31 |   Bacteria pop - 41")
        print("Phage pop    - 22 |   Phage pop    - 32 |   Phage pop    - 42")
        print("Lys pop      - 23 |   Lys pop      - 33 |   Lys pop      - 43")
        print("Bact spec    - 24 |   Bact spec    - 34 |   Bact spec    - 44")
        print("Phage spec   - 25 |   Phage spec   - 35 |   Phage spec   - 45")
        print("Lys spec     - 26 |   Lys spec     - 36 |   Lys spec     - 46")
        print("Pha/lys rat  - 27 |   Pha/lys rat  - 37 |   Phag/lys rat - 47")
        print("vmr          - 28 |   vmr          - 38 |   vmr          - 48")
        print("Perc. cover  - 29 |   Perc. cover  - 39 |   Perc. cover  - 49")
        print("Time         - 30 |   Time         - 40 |   Time         - 50")
        # user input
        try:
            x_data = int(input("\n First set of data (x axis): \n") or '0')
        except ValueError:
            print("\n Input not recognized. \n")
            continue
        if x_data == 0:
            break
        try:
            y_data = int(input("\n Second set of data (y axis): \n") or '0')
        except ValueError:
            print("\n Input not recognized. \n")
            continue
        # match to indices of array (starts at 0)
        x_data -= 1
        y_data -= 1
        # get names of files
        x_file = datfiles[x_data]
        y_file = datfiles[y_data]
        # pass to graphing subroutine
        graph(x_file,y_file,x_data,y_data,t_flag,var_vals,ndir,vr_nm)
        # No returns

################################################################################
# subroutine to make a directory, will not override if already exists. Instead
# appends a number before creating
def dir_make(direc):
    # direc - name of directory

    # initialize
    i = 0
    # determine relative path to current directory
    rel_path = os.path.dirname(__file__)
    # change path and assigne testing directory
    os.chdir(rel_path)
    dirchk = direc
    # if path exists, append numeric and check again
    while os.path.isdir(dirchk):
        i += 1
        dirchk = direc + "{}".format(i)
    # update input directory
    if i > 0:
        direc = direc + "{}".format(i)

    # assign subdirectories that need to be copied
    dir2_2cpy = rel_path + "/General"
    cor_dir =  rel_path + "/Coral"
    file_2cpy = rel_path + "/inputs.dat"
    file_tcpy = rel_path + "/times.dat"
    # make new directory and copy subdirectories
    os.mkdir(direc)
    shutil.move(dir2_2cpy,direc)
    shutil.move(cor_dir,direc)
    shutil.move(file_2cpy,direc)
    shutil.move(file_tcpy,direc)
    # Returns modified directory
    return direc

################################################################################
# Subroutine to call the fortran source code
def run(in_list):
    # in_list - contains variables to pass to fortran code

    # create file with input variables
    in_file = "inputs.dat"
    in_file_p = open(in_file,'w')
    # write those variables
    for i,val in enumerate(in_list):
        in_file_p.write("{}\n".format(in_list[i]))
    in_file_p.close()
    # call the fortran code
    subprocess.call("./Phage2Shark.x < {}".format(in_file),
                        shell=True, executable='/bin/bash')

    # No returns
################################################################################
# Subroutine to adjust the initial values of the simulations
def ini_adj(vars):
    # vars - contains the variables that may be adjusted

    # vector containing valid user choices
    adj_lst = len(vars) - 1
    # choices which do not require special handling
    spec_chcs = [2,9,10,11,12]
    # Acceptable choices for adjustments of intensityies
    level_chcs = [1,2,3,4,5]
    # Acceptable character inputs
    event_chcs = ['H','D','N']
    adjust_chcs = ['D','N']

    adj_chc = 50
    while adj_chc != 0:
        print("\nSelect which value to change\n")
        print("\nNote: Negative entries will be treated as absolute values\n")
        print("\nNote: (Adj) refers to values which will change if Adjustment \
         Flag is a value other than 'N' \n")
        print("1 - Initial Coral Coverage  {} \n2 - Piscivore Mass          {}".format(vars[2],vars[3]))
        print("3 - Hunting Success Average {} \n4 - Burst Size              {}".format(vars[4],vars[5]))
        print("5 - Initial Fish Population {} \n6 - Fish Growth Rate        {}".format(vars[6],vars[7]))
        print("7 - Diffusion Coefficient   {} \n\n8 - Event Flag              {}".format(vars[8],vars[9]))
        print("9 - Event Level             {} \n10- Adjustment Flag         {}".format(vars[10],vars[11]))
        print("11- Adjustment Time         {} \n\n12- Piscivore Mass (Adj)    {}".format(vars[12],vars[14]))
        print("13- Hunting Success (Adj)   {} \n14- Burst Size (Adj)        {}".format(vars[15],vars[16]))
        print("15- Fish Population (Adj)   {} \n16- Fish Growth Rate (Adj)  {}".format(vars[17],vars[18]))
        print("17- Diffustion Co. (Adj)    {} \n".format(vars[19]))
        # user inputs
        while True:
            adj_chc = int(input("Choice (Leave blank to choose none): ") or '0')
            if adj_chc == 0:
                return vars
            elif adj_chc > adj_lst:
                print("\nChoice not recognized, please try again")
                continue
            else:
                break
        # map to vector
        if adj_chc <= 11:
            adj_chc +=1
        else:
            adj_chc += 2

        # Coral percentage between 0,1
        if adj_chc == 2:
            print("\nTypical Value: {}".format(vars[adj_chc]))
            while True:
                nw_val = float(input("\nNew Value (Between 0 and 1): ") or '0')
                if (nw_val < 0.01) or (nw_val > 1.0):
                    print("\nInvalid entry, try again")
                    continue
                else:
                    vars[adj_chc] = nw_val
                    break

        # Event flag handling
        if adj_chc == 9:
            print("\nCurrent Value: {}".format(vars[adj_chc]))
            while True:
                nw_val = input("\nNew Value (N-None, H-Hurricane, D-Disease): ")
                nw_val = nw_val.upper()
                if nw_val not in event_chcs:
                    print("\nInvalid Choice, try again")
                else:
                    vars[adj_chc] = nw_val
                    break

            if nw_val != 'N':
                print("\nChoose the severity as an integer (1-5): ")
                while True:
                    try:
                        sev = int(input("\nLevel: "))
                    except ValueError:
                        print("Not an integer, try again.")
                    if sev not in level_chcs:
                        print("Outside of range (1-5), try again")
                    else:
                        vars[adj_chc+1] = sev
                        break

        # Adjustment flag handling
        if adj_chc == 11:
            print("\nCurrent Value: {}".format(vars[adj_chc]))
            while True:
                nw_val = input("\nNew Value (N-No adjustment, D-Adjust): ")
                nw_val = nw_val.upper()
                if nw_val not in adjust_chcs:
                    print("\nInvalid Choice, try again")
                else:
                    vars[adj_chc] = nw_val
                    break

            if nw_val != 'N':
                print("\nChoose the timestep to change the values (integer): ")
                try:
                    td = abs(int(input("\nLevel: ")))
                except ValueError:
                    print("Not an integer, try again.")
                vars[adj_chc+1] = td
        # event severity
        if adj_chc == 10:
            print("\nCurrent Value: {}".format(vars[adj_chc]))
            try:
                sev = int(input("\nNew Level as an integer (1-5): "))
            except ValueError:
                print("Not an integer, try again.")
            vars[adj_chc] = sev
        # timestep of adjustment
        if adj_chc == 12:
            print("\nCurrent Value: {}".format(vars[adj_chc]))
            try:
                sev = abs(int(input("\nNew time as an integer: ")))
            except ValueError:
                print("Not an integer, try again.")
            vars[adj_chc] = sev
        # all other adjustments
        if adj_chc not in spec_chcs:
            print("Typical value: {}".format(vars[adj_chc]))
            vars[adj_chc] = abs(float(input("New Value: ")))

    return vars
    # return modified inputs
################################################################################
# Subroutine to handle single runs
def single(inputs,outfiles,time):
    # inputs   - vector of input variables
    # outfiles - contains names of data files
    # time     - contains date in dd_mm_yyy format with run type and number

    # obtain relative path
    rel_path = os.path.dirname(__file__)

    # pass to running subroutine
    run(inputs)
    # set graph flags for single run
    var_vals = [1] # no iteration of parameters, set to one
    gr_flag = 1
    # determine final directory
    out_dir = rel_path + "/Runs/" + time + "_single"
    # make that directory
    out_di = dir_make(out_dir)
    # pass to graphing subroutines
    quick_results(outfiles,out_di,0)
    graph_choice(out_di,outfiles,gr_flag,var_vals,"A")

    # No returns

################################################################################
# subroutine to handle ranged runs
def ranged(inputs,outfiles,time):
    # inputs   - vector of input variables
    # outfiles - contains names of data files
    # time     - contains date in dd_mm_yyy format with run type and number

    # set number of runs and initialize
    num_runs = 4
    var_vals = []
    # relative working directory
    rel_path = os.path.dirname(__file__)

    # vector with names of variables that can be iterated over
    vars = ["Initial Coral","Piscivore Mass","Days Between Hunts",
            "Average Burst Size","Initial Fish Population",
            "Fish Growth Rate","Diffusion Coefficient",]
    # clear terminal and set up for user input
    _=os.system('clear')
    print("Please select the variable to range over:\n")
    print("Initial Coral Coverage  - 1")
    print("Piscivore Mass          - 2")
    print("Hunting Success Average - 3")
    print("Burst Size              - 4")
    print("Initial Fish Population - 5")
    print("Fish Growth Rate        - 6")
    print("Diffusion Coefficient   - 7\n")
    # user input
    try:
        var_it = int(input("Variable Choice: \n") or '0')
    except ValueError:
        print("Input not recognized. \n")
    # map to vectors
    var_it += 1
    var_nm = var_it-2
    # further user input, low bound then upper
    print("Typical Value: {}\n".format(inputs[var_it]))
    var_min = float(input("Lower Bound: "))
    var_max = float(input("Upper Bound: "))
    # determine step size
    var_del = (var_max - var_min)/(num_runs)
    # display user choices
    print("\nVariable Max: {}".format(var_max))
    print("Variable Min: {}".format(var_min))
    print("Size of step: {}\n".format(var_del))
    # user continuation
    try:
        cont = (input("Press enter to continue\n"))
    except ValueError:
        print("Input not recognized. \n")

    # set parameter to be adjusted to lower bound
    inputs[var_it] = var_min
    dirgen = rel_path
    i = 0 # initialize
    while i <= num_runs:
        # run simulation with current values
        run(inputs)
        # attach current value to holding vector
        var_vals.append(inputs[var_it])
        # update current value
        inputs[var_it] += var_del
        i += 1 # update iterator
        # Rename datafiles such that they are not overriden
        quick_results(outfiles,dirgen,i-1)
        for ind in outfiles:
            name_or = rel_path + "/General/{}.dat".format(ind)
            name_nw = rel_path + "/General/{}{}.dat".format(ind,i)
            os.rename(name_or,name_nw)

    # set output path to reflect run type
    out_dir = rel_path + "/Runs/" + time + "_range"
    # make that path
    out_dir = dir_make(out_dir)
    # copy the at-a-glance files
    for file in glob.glob(r'glance*'):
        shutil.move(file,out_dir)
    # set graph flags to ranged
    gr_flag = 2
    # pass to graphing subroutines
    graph_choice(out_dir,outfiles,gr_flag,var_vals,vars[var_nm])

    # No returns

################################################################################
## Function to handle statistical averages of multiple runs
# Runs simulation 10 times with the same parameters and finds average deltas
# between the outputs.
def stats_run(inputs,outfiles,time):
    # inputs   - vector of input variables
    # outfiles - contains names of data files
    # time     - contains date in dd_mm_yyy format with run type and number

    num_runs = 9 # number of runs to perform
    # get relative path
    rel_path = os.path.dirname(__file__)
    # parameter names
    vars = ["Initial Coral","Piscivore Mass","Days Between Hunts",
            "Average Burst Size","Initial Fish Population",
            "Fish Growth Rate","Diffusion Coefficient",]
    # clear terminal
    _=os.system('clear')

    # generate a new seed and assign to parameters
    timeg = datetime.datetime.now()
    seed = timeg.day+timeg.microsecond+timeg.second
    inputs[13] = seed

    i = 0 # initialize
    while i <= num_runs:
        # run the simulation with current seed
        run(inputs)
        # update iterator
        i += 1
        # rename data files
        for l,ind in enumerate(outfiles):
            name_or = rel_path + "/General/{}.dat".format(ind)
            name_nw = rel_path + "/General/{}{}.dat".format(ind,i)
            os.rename(name_or,name_nw)

    # set output path to reflect stats run
    out_dir = rel_path + "/Runs/" + time + "_stats"
    # make that path
    dir_make(out_dir)
    # set flags to reflect stats and pass to graphing subroutines
    gr_flag = 3
    var_vals = [1]
    graph_choice(out_dir,outfiles,gr_flag,var_vals,"B")

    # No returns

################################################################################
# subroutine to provide 'at-a-glance' stats for a given run. Includes max and
# min of tracked values
def quick_results(f_names,in_dir,run):
    # f_names   - array of names of files that hold the data
    # dat_dir   - directory data files can be found
    # f_dir_out - directory output file will go
    dat_dir = in_dir+"/General"
    os.chdir(dat_dir)
    if run == 0:
        f_out = "glance.dat"
    else:
        f_out = "glance_r{}.dat".format(run)
    with open(f_out, 'a') as FO: # write header
        FO.write("At a Glance Numbers \n Variable | Start | Final | Max | Time \
            | Min | Time \n")
    for file in f_names: # get the data
        if run == 0:
            F = file + ".dat"
        else:
            F = file + "{}.dat".format(run)

        data = [] # holds data
        with open(F,'r') as x:
            d = x.read().splitlines()

        for line in d: # put data in a list
            ff = line.split()
            data.append(float(ff[0]))
        # determine datapoints of interest
        first = data[0]
        last = data[-1]
        T = len(data)
        mx = max(data)
        max_t = data.index(max(data))
        mn = min(data)
        min_t = data.index(min(data))
        # put it in the file
        with open(f_out, 'a') as FO:
            FO.write("{} | {} | {} | {} | {} | {} | {} \n". \
            format(file,first,last,mx,max_t,mn,min_t))
    # copy file to final
    shutil.move(f_out,in_dir)
    os.chdir(in_dir)
