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
    else:
        nm = 5

    if flg != 1:
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
    while i <= nm:
        x_data = []
        y_data = []

        if flg == 1:
            xfile = xfil + ".dat"
            yfile = yfil + ".dat"
        else:
            xfile = xfil + "{}.dat".format(i)
            yfile = yfil + "{}.dat".format(i)
            plt.legend([vals[0],vals[1],vals[2],vals[3],vals[4]], loc='upper right')


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

        plt.plot(x_data,y_data)
        i += 1

    plt.savefig(plt_name,bbox_inches='tight')
    plt.clf()
    plt_dir = dire2 + "/General/{}".format(plt_name)

    shutil.copy(plt_dir,dire2)

################################################################################

def graph_choice(ndir,datfiles,t_flag,var_vals,vr_nm):

    os.chdir(ndir+"/General")
    exit_input = ' '

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

    os.chdir("/home/jon/Desktop/Phage2Shark")
    dirchk = direc

    while os.path.isdir(dirchk):
        i += 1
        dirchk = direc + "{}".format(i)

    if i > 0:
        direc = direc + "{}".format(i)


    dir2_2cpy = "/home/jon/Desktop/Phage2Shark/General"
    cor_dir = "/home/jon/Desktop/Phage2Shark/Coral"
    file_2cpy = "inputs.dat"

    os.mkdir(direc)
    shutil.move(dir2_2cpy,direc)
    shutil.move(cor_dir,direc)
    shutil.move(file_2cpy,direc)
    return direc

################################################################################

## Subroutine to run the simulation a single time
def run(in_list):
    in_file = "inputs.dat"
    in_file_p = open(in_file,'w')

    for i,val in enumerate(in_list):
        in_file_p.write("{}\n".format(in_list[i]))

    in_file_p.close()

    subprocess.call("./Phage2Shark.x < {}".format(in_file),
                        shell=True, executable='/bin/bash')

################################################################################

def single(inputs,outfiles,time):

    run(inputs)

    var_vals = [1]

    out_dir = "/home/jon/Desktop/Phage2Shark/" + "Runs/" + time + "_single"

    out_di = dir_make(out_dir)
    gr_flag = 1
    graph_choice(out_di,outfiles,gr_flag,var_vals,"A")

################################################################################

def ranged(inputs,outfiles,time):

    num_runs = 4
    var_vals = []

    dir = "/home/jon/Desktop/Phage2Shark/General/"

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
            name_or = dir + "{}.dat".format(ind)
            name_nw = dir + "{}{}.dat".format(ind,i)
            os.rename(name_or,name_nw)


    out_dir = "/home/jon/Desktop/Phage2Shark/" + "Runs/" + time + "_range"

    dir_make(out_dir)

    gr_flag = 2
    graph_choice(out_dir,outfiles,gr_flag,var_vals,vars[var_nm])

################################################################################
## Function to handle statistical averages of multiple runs
# Runs simulation 10 times with the same parameters and finds average deltas
# between the outputs.

def stats_run(inputs,outfiles,time):

    num_runs = 9
    num_iter = 4


    var_vals = []
    avg_vals = [[0 for x in range(len(outfiles))] for y in range(num_iter)]

    dir = "/home/jon/Desktop/Phage2Shark/General/"

    vars = ["Initial Coral","Piscivore Mass","Days Between Hunts",
            "Average Burst Size","Initial Fish Population",
            "Fish Growth Rate","Diffusion Coefficient",]

    print("Please select the variable to range over:")
    print("Initial Coral Coverage  - 1  | Piscivore Mass             - 2")
    print("Hunting Success Average - 3  | Burst Size                 - 4")
    print("Initial Fish Population - 5  | Fish Growth Rate           - 6")
    print("Diffusion Coefficient   - 7")
    try:
        var_it = int(input("Variable Choice: \n") or '0')
    except ValueError:
        print("Input not recognized. \n")


    var_it += 1
    var_nm = var_it -2

    print("Typical Value: {}".format(inputs[var_it]))
    var_min = float(input("\n Lower Bound: "))
    var_max = float(input("\n Upper Bound: "))

    var_del = (var_max - var_min)/(num_runs)

    print("Variable Max: {} \n  \
            Variable Min: {} \n \
            Size of step: {} \n".format(var_max,var_min,var_del))

    inputs[var_it] = var_min
    i = 0
    j = 0

    while j <= num_iter:

        var_vals.append(inputs[var_it])
        inputs[var_it] += var_del

        while i <= num_runs:

            run(inputs)

            i += 1

            for l,ind in enumerate(outfiles):
                with open("{}.dat".format(ind), "rb") as fl:
                    v1 = real(fl.readline())
                    fl.seek(-1024, os.SEEK_END)
                    while fl.read(1) != b"\n":
                        f.seek(-1024, os.SEEK_CUR)
                    v2 = real(fl.readline())
                    avg_vals[j][l] += (v2-v1)

                    fl.close()

                name_or = dir + "{}.dat".format(ind)
                name_nw = dir + "{}{}.dat".format(ind,i)
                os.rename(name_or,name_nw)

        j += 1

    avg_vals = [[0 for x in range(len(outfiles))] for y in range(num_iter)]
    for n in range(len(outfiles)):
        for y in range(num_iter):
            avg_vals[n][y] = avg_vals[n][y]/(num_runs + 1)

    out_dir = "/home/jon/Desktop/Phage2Shark/" + "Runs/" + time + "_stats"

    dir_make(out_dir)

    graph_choice_stats(out_dir,outfiles,avg_vals,var_vals,vars[var_nm])
