# Makefile for the Phage to Shark source code

# compiler
CC = gfortran
# compiler flags: includes common warnings (with the exception of tab warnings), debugging information, quit on variable overflow
# first level optimization
CFLAGS = -Wall -Wno-tabs -g -fbacktrace -ffpe-trap=overflow

ALL: P2S link clean
# modules
P2S: 
	$(CC) -c   P2Smod.F90
# executable
link:
	$(CC) $(CFLAGS) -o  Phage2Shark.x coral_subs.F90 P2Smod.F90 fish_subs.F90 P2Smain.F90 nonlifesubs.F90 microbes_subs.F90 disasters.F90
# cleanup
clean:
	rm *.mod
	rm *.o