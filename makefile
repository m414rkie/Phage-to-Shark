# Makefile for the Phage to Shark source code

CC = gfortran
CFLAGS = -Wall -Wno-tabs -g -fbacktrace

ALL: P2S link clean

P2S: 
	$(CC) -c   P2Smod.f90
	
link:
	$(CC) -o  Phage2Shark.x coral_subs.F90 P2Smod.f90 fish_subs.F90 P2Smain.f90 nonlifesubs.f90 microbes_subs.F90 disasters.f90

clean:
	rm *.mod
	rm *.o