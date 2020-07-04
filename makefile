# Makefile for the Phage to Shark source code

CC = gfortran
CFLAGS = -Wall -Wno-tabs -g -fbacktrace

ALL: P2S link clean

P2S: 
	$(CC) -c   P2Smod.F90
	
link:
	$(CC) $(CFLAGS) -o  Phage2Shark.x coral_subs.F90 P2Smod.F90 fish_subs.F90 P2Smain.F90 nonlifesubs.F90 microbes_subs.F90 disasters.F90

clean:
	rm *.mod
	rm *.o