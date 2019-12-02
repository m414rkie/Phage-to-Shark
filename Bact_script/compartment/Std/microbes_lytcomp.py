#!/usr/bin/python3

import matplotlib.pyplot as plt

# Short script to analyse the LV equations
# Corollary to the Phage to Shark project.

# Jon Parsons
# 11-23-19


a = 1.92E-8 # GP/Day : 2.0E-8 ml/hr
db = 0.5 # 1/Day : 0.0208 1/hr for 0.5 1/Day
bu = 70.0 # Count
dp = 0.4 # 1/Day : 0.02 1/hr for 0.5 1/Day
r = 0.55 # 1/Day : 0.0417 1/hr for 1 1/Day
lr = 1.0 # 1/Day: 0.0417 1/hr for 1 1/Day

################################################################################

def graph(b_data,p_data,l_data,m_data,vmr_data,t_data):


    plt_title = "Populations of Bacteria and Phage"

    x_lab = "Time (hr)"
    y_lab = "Microbes (1/ml)"
    plt_name = "mic_pop.png"
    plt.title(plt_title)
    plt.xlabel(x_lab)
    plt.ylabel(y_lab)

    plt.plot(t_data,b_data,label="Bacteria")
    plt.plot(t_data,p_data,label="Phage")
    plt.plot(t_data,l_data,label="Lysogens")
    plt.legend(loc='upper right')

    plt.savefig(plt_name,bbox_inches='tight')
    plt.clf()

    x_lab = "Time (hr)"
    y_lab = "Microbes (1/ml)"

    plt_name = "bac_pop.png"
    plt.title(plt_title)
    plt.xlabel(x_lab)
    plt.ylabel(y_lab)

    plt.plot(t_data,b_data,label="Bacteria")
    plt.plot(t_data,l_data,label="Lysogens")
    plt.legend(loc='upper right')

    plt.savefig(plt_name,bbox_inches='tight')
    plt.clf()

    x_lab = "Time (hr)"
    y_lab = "VMR"

    plt_name = "vmr.png"
    plt.title(plt_title)
    plt.xlabel(x_lab)
    plt.ylabel(y_lab)

    plt.plot(t_data,vmr_data,label="VMR")
    plt.legend(loc='upper right')

    plt.savefig(plt_name,bbox_inches='tight')
    plt.clf()

    x_lab = "Cells (1/ml)"
    y_lab = "VLPs (1/ml)"

    plt_name = "v_m.png"
    plt.title(plt_title)
    plt.xlabel(x_lab)
    plt.ylabel(y_lab)

    plt.plot(m_data,p_data,label="Virus and Cells")
    plt.legend(loc='upper right')

    plt.savefig(plt_name,bbox_inches='tight')
    plt.clf()
################################################################################

def com_carr(kdel,b,spec,lcarry):
    lcarry = kdel*(b)*spec/(25.0E12)
    return(lcarry)

################################################################################

def spec_find(pop,spec):
    spec = 51.215*(pop**0.0336)
    return(spec)

################################################################################
cmin = 75.0E9
cmax = 300.0E9

cdelta = (cmax - cmin)/4.0

cc = cmin

tsteps = 5000

b_ini = 1.0E5
p_ini = 8*b_ini

b_s = 1
b_s = spec_find(b_ini,b_s)

p_s = 1
p_s = spec_find(p_ini,p_s)

print(b_s,p_s)

bin = b_s*dp/(a*bu)
pin = p_s*(r*(1.0 - bin/cc) - db)/a

bn = bin
pn = pin

lc = cc

b_pop = []
p_pop = []
l_pop = []
vmr = []
m_pop = []
t = []

# Speciation from both compartments?

f = open("Timebact.dat",'w')

f.write("T      Bact  Phage     Lys   VMR  P_spec B_spec \n")

tchange = [1000,2000,3000,4000]

for i in range(tsteps):

    if i in tchange:
        cc += cdelta
        print(cc)

    lkdel = cc - bn
    lc = com_carr(lkdel,bn,b_s,lc)
    ln = (lc - lc*1E-6/lr)

    p_s = spec_find(pn,p_s)

    pn = p_s*(r*(1.0 - bin/cc) - db)/a


    b_pop.append(bn/25)
    p_pop.append(pn/25)
    l_pop.append(ln/25)
    m_pop.append((ln+bn)/25)
    t.append(i+1)
    vmr.append((pn)/(bn+ln))
    f.write("%4d %7.0f %7.0f %7.0f %4.1f %4.0f %4.0f \n" %(i, bn/25, pn/25, ln/25, vmr[i], p_s, b_s))
f.close()
graph(b_pop,p_pop,l_pop,m_pop,vmr,t)
