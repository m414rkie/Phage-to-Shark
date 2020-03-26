#!/usr/bin/python3

import subprocess
import os
import datetime
import shutil
import matplotlib.pyplot as plt

# Python wrapper for the P2S project.
# Handles input and data for the Fortran code.

# Jon Parsons
# 10-3-19

################################################################################


def dir_find(direc):
    i = 0

    dirchk = direc

    flag = False

    while os.path.isdir(dirchk):
        i += 1
        dirchk = direc + "_{}".format(i)

    if i > 1:
        direc = direc + "_{}".format(i-1)

    return direc

################################################################################

def graph(xfil,yfil,xnum,ynum,flg,vals,dire,vr_nm):

    dire2 = dir_find(dire)

    os.chdir(dire2+"/General")

    units = ["","(BU)","(BU/25cm^2)","(dBU/25cm^2)","(1/ml)","(1/ml)","(1/ml)",
            "(1/ml)","(1/ml)","(1/ml)","(FM)","(dFM)","","","","","(Days)"]

    if flg == 1:
        nm = 1
    elif flg == 2:
        nm = 5
    else:
        nm = 9

    if flg == 2:
        plt_title = "{} - {} \n Variable: {}".format(xfil,yfil,vr_nm)
    else:
        plt_title = "{} - {}".format(xfil,yfil)

    x_lab = xfil + " " + units[xnum]
    y_lab = yfil + " " + units[ynum]
    plt_name = "{}{}.png".format(xfil,yfil)
    plt.ticklabel_format(useOffset=False)
    plt.title(plt_title)
    plt.xlabel(x_lab)
    plt.ylabel(y_lab)

    i = 1
    if flg == 1:
        iter = nm
    else:
        iter = nm

    while i <= iter:
        x_data = []
        y_data = []

        if flg == 1:
            xfile = xfil + ".dat"
            yfile = yfil + ".dat"
        elif flg == 2:
            xfile = xfil + "{}.dat".format(i)
            yfile = yfil + "{}.dat".format(i)
        elif flg == 3:
            xfile = xfil + "{}.dat".format(i)
            yfile = yfil + "{}.dat".format(i)
        else:
            print("ERROR: Unknown Graph flag.")


        with open(xfile,'r') as x:
            x_dat = x.read().splitlines()
        with open(yfile,'r') as y:
            y_dat = y.read().splitlines()

        for line in x_dat:
            dxt = line.split()
            x_data.append(float(dxt[0]))
        for lines in y_dat:
            dyt = lines.split()
            y_data.append(float(dyt[0]))

        if flg == 3:
            plt.scatter(x_data,y_data)
        else:
            plt.plot(x_data,y_data)

        i += 1

    if flg == 2:
            plt.legend([vals[0],vals[1],vals[2],vals[3],vals[4]], loc='upper right')

    plt.savefig(plt_name,bbox_inches='tight')
    plt.clf()
    plt_dir = dire2 + "/General/{}".format(plt_name)

    shutil.copy(plt_dir,dire2)

    print("Graph can be found in {}".format(plt_dir))
################################################################################

def graph_choice(ndir,datfiles,t_flag,var_vals,vr_nm):

    os.chdir(ndir+"/General")
    exit_input = ' '

    # Default graphs made here
    comp_list = [11,5,6,7,8,10,9,16]

    graph(datfiles[10],datfiles[4],10,4,t_flag,var_vals,ndir,vr_nm)
    graph(datfiles[10],datfiles[5],10,5,t_flag,var_vals,ndir,vr_nm)
    graph(datfiles[5],datfiles[4],5,4,t_flag,var_vals,ndir,vr_nm)
    for val in comp_list:
        graph(datfiles[0],datfiles[val-1],0,val-1,t_flag,var_vals,ndir,vr_nm)
        graph(datfiles[16],datfiles[0],0,val-1,t_flag,var_vals,ndir,vr_nm)


    while True:
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

        x_data -= 1
        y_data -= 1

        x_file = datfiles[x_data]
        y_file = datfiles[y_data]

        graph(x_file,y_file,x_data,y_data,t_flag,var_vals,ndir,vr_nm)

################################################################################

def dir_make(direc):
    i = 0

    rel_path = os.path.dirname(__file__)

    os.chdir(rel_path)
    dirchk = direc

    while os.path.isdir(dirchk):
        i += 1
        dirchk = direc + "{}".format(i)

    if i > 0:
        direc = direc + "{}".format(i)


    dir2_2cpy = rel_path + "/General"
    cor_dir =  rel_path + "/Coral"
    file_2cpy = rel_path + "/inputs.dat"

    os.mkdir(direc)
    shutil.move(dir2_2cpy,direc)
    shutil.move(cor_dir,direc)
    shutil.move(file_2cpy,direc)
    return direc

################################################################################

## Subroutine to call the fortran source code
def run(in_list):
    in_file = "inputs.dat"
    in_file_p = open(in_file,'w')

    for i,val in enumerate(in_list):
        in_file_p.write("{}\n".format(in_list[i]))

    in_file_p.close()

    subprocess.call("./Phage2Shark.x < {}".format(in_file),
                        shell=True, executable='/bin/bash')

################################################################################

## Subroutine to adjust the initial values of the simulations
def ini_adj(cc,pm,ha,bu,fi,fg,dc,df,dl,af,at,pm2,ha2,bu2,fi2,fg2,dc2):

    adj_lst = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17]
    spec_chcs = [1,8,9,10,11]
    lev_chcs = [1,2,3,4,5]
    vars = [cc,pm,ha,bu,fi,fg,dc,df,dl,af,at,pm2,ha2,bu2,fi2,fg2,dc2]
    event_chcs = ['H','D','N']
    adj_chcs = ['D','N']

    adj_chc = 50
    while adj_chc != 0:
        print("\nSelect which value to change\n")
        print("\nNote: Negative entries will be treated as absolute values\n")
        print("1 - Initial Coral Coverage  {} \n2 - Piscivore Mass          {}".format(vars[0],vars[1]))
        print("3 - Hunting Success Average {} \n4 - Burst Size              {}".format(vars[2],vars[3]))
        print("5 - Initial Fish Population {} \n6 - Fish Growth Rate        {}".format(vars[4],vars[5]))
        print("7 - Diffusion Coefficient   {} \n\n8 - Event Flag              {}".format(vars[6],vars[7]))
        print("9 - Event Level             {} \n10- Adjustment Flag         {}".format(vars[8],vars[9]))
        print("11- Adjustment Time         {} \n\n12- Piscivore Mass (Adj)    {}".format(vars[10],vars[11]))
        print("13- Hunting Success (Adj)   {} \n14- Burst Size (Adj)        {}".format(vars[12],vars[13]))
        print("15- Fish Population (Adj)   {} \n16- Fish Growth Rate (Adj)  {}".format(vars[14],vars[15]))
        print("17- Diffustion Co. (Adj)    {} \n".format(vars[16]))

        while True:
            adj_chc = int(input("Choice (Leave blank to choose none): ") or '0')
            if adj_chc == 0:
                return vars
            elif adj_chc not in adj_lst:
                print("\nChoice not recognized, please try again")
                continue
            else:
                break
        # Coral percentage between 0,1
        if adj_chc == 1:
            print("\nTypical Value: {}".format(vars[adj_chc-1]))
            while True:
                nw_val = float(input("\nNew Value (Between 0 and 1): "))
                if (nw_val < 0.01) or (nw_val > 1.0):
                    print("\nInvalid entry, try again")
                    continue
                else:
                    vars[adj_chc-1] = nw_val
                    break

        # Event flag handling
        if adj_chc == 8:
            print("\nCurrent Value: {}".format(vars[adj_chc-1]))
            while True:
                nw_val = input("\nNew Value (N-None, H-Hurricane, D-Disease): ")
                nw_val = nw_val.upper()
                if nw_val not in event_chcs:
                    print("\nInvalid Choice, try again")
                else:
                    vars[adj_chc-1] = nw_val
                    break

            if nw_val != 'N':
                print("\nChoose the severity as an integer (1-5): ")
                while True:
                    try:
                        sev = int(input("\nLevel: "))
                    except ValueError:
                        print("Not an integer, try again.")
                    if sev not in lev_chcs:
                        print("Outside of range (1-5), try again")
                    else:
                        vars[adj_chc] = sev
                        break

        # Adjustment flag handling
        if adj_chc == 10:
            print("\nCurrent Value: {}".format(vars[adj_chc-1]))
            while True:
                nw_val = abs(input("\nNew Value (N-No adjustment, D-Adjust): "))
                nw_val = nw_val.upper()
                if nw_val not in adj_chcs:
                    print("\nInvalid Choice, try again")
                else:
                    vars[adj_chc-1] = nw_val
                    break

            if nw_val != 'N':
                print("\nChoose the timestep to change the values (integer): ")
                try:
                    td = abs(int(input("\nLevel: ")))
                except ValueError:
                    print("Not an integer, try again.")
                vars[adj_chc] = td

        if adj_chc == 9:
            print("\nCurrent Value: {}".format(vars[adj_chc-1]))
            try:
                sev = int(input("\nNew Level as an integer (1-5): "))
            except ValueError:
                print("Not an integer, try again.")
            vars[adj_chc-1] = sev

        if adj_chc == 11:
            print("\nCurrent Value: {}".format(vars[adj_chc-1]))
            try:
                sev = abs(int(input("\nNew time as an integer: ")))
            except ValueError:
                print("Not an integer, try again.")
            vars[adj_chc-1] = sev

        if adj_chc not in spec_chcs:
            print("Typical value: {}".format(vars[adj_chc-1]))
            vars[adj_chc-1] = abs(float(input("New Value: ")))

    return vars

################################################################################

## Subroutine to handle single runs
def single(inputs,outfiles,time):

    rel_path = os.path.dirname(__file__)

    run(inputs)

    var_vals = [1]

    out_dir = rel_path + "/Runs/" + time + "_single"

    out_di = dir_make(out_dir)
    gr_flag = 1
    graph_choice(out_di,outfiles,gr_flag,var_vals,"A")

################################################################################

def ranged(inputs,outfiles,time):

    num_runs = 4
    var_vals = []

    rel_path = os.path.dirname(__file__)

    vars = ["Initial Coral","Piscivore Mass","Days Between Hunts",
            "Average Burst Size","Initial Fish Population",
            "Fish Growth Rate","Diffusion Coefficient",]

    _=os.system('clear')
    print("Please select the variable to range over:\n")
    print("Initial Coral Coverage  - 1")
    print("Piscivore Mass          - 2")
    print("Hunting Success Average - 3")
    print("Burst Size              - 4")
    print("Initial Fish Population - 5")
    print("Fish Growth Rate        - 6")
    print("Diffusion Coefficient   - 7\n")

    try:
        var_it = int(input("Variable Choice: \n") or '0')
    except ValueError:
        print("Input not recognized. \n")

    var_it += 1
    var_nm = var_it-2

    print("Typical Value: {}\n".format(inputs[var_it]))
    var_min = float(input("Lower Bound: "))
    var_max = float(input("Upper Bound: "))

    var_del = (var_max - var_min)/(num_runs)

    print("\nVariable Max: {}".format(var_max))
    print("Variable Min: {}".format(var_min))
    print("Size of step: {}\n".format(var_del))

    try:
        cont = (input("Press enter to continue\n"))
    except ValueError:
        print("Input not recognized. \n")

    inputs[var_it] = var_min
    i = 0
    while i <= num_runs:

        run(inputs)
        var_vals.append(inputs[var_it])
        inputs[var_it] += var_del
        i += 1

        for ind in outfiles:
            name_or = rel_path + "/General/{}.dat".format(ind)
            name_nw = rel_path + "/General/{}{}.dat".format(ind,i)
            os.rename(name_or,name_nw)


    out_dir = rel_path + "/Runs/" + time + "_range"

    out_dir = dir_make(out_dir)

    gr_flag = 2
    graph_choice(out_dir,outfiles,gr_flag,var_vals,vars[var_nm])

################################################################################
## Function to handle statistical averages of multiple runs
# Runs simulation 10 times with the same parameters and finds average deltas
# between the outputs.

def stats_run(inputs,outfiles,time):

    num_runs = 9

    rel_path = os.path.dirname(__file__)

    vars = ["Initial Coral","Piscivore Mass","Days Between Hunts",
            "Average Burst Size","Initial Fish Population",
            "Fish Growth Rate","Diffusion Coefficient",]

    _=os.system('clear')

    i = 0

    # generate a new seed
    timeg = datetime.datetime.now()
    seed = timeg.day+timeg.microsecond+timeg.second
    inputs[13] = seed

    while i <= num_runs:

        run(inputs)

        i += 1

        for l,ind in enumerate(outfiles):
            name_or = rel_path + "/General/{}.dat".format(ind)
            name_nw = rel_path + "/General/{}{}.dat".format(ind,i)
            os.rename(name_or,name_nw)


    out_dir = rel_path + "/Runs/" + time + "_stats"

    dir_make(out_dir)

    gr_flag = 3
    var_vals = [1]
    graph_choice(out_dir,outfiles,gr_flag,var_vals,"B")
