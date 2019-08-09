## Phage to Shark
# Code Map

A listing of files and the subroutines/functions contained within.

Code written by: Jon Parsons
Primary investigators: Dr. P. Katira
                       Dr. C. Silveira
                       Dr. A. Luque

__P2Smain.f90__
Contains the master loop. No subroutines or functions contained.

__coral\_subs.F90__
Contains subroutines related to the coral layer.

* corpop: Sets initial coral distribution and health
* corexp: Sets new coral
* newcoral: Sets new coral during spawning season
* growth: Grows existing coral
* decay: Handles interaction between coral and algae

__disasters.f90__
Contains the subroutines for disastrous events.

* hurricane: Handles simulation of a hurricane or other similar event
* disease: Handles simulation of a disease or other similar event

__fish\_subs.F90__
Contains the subroutines pertaining to the fish elements

* fishinteraction: Handles interaction of fish with algae
* shark: Handles simulation of piscivores

__microbes\_subs.f90__
Contains the subroutines for the microbial community

* kgrid: Determines the microbial carrying capacity at each point
* diffuse: Allows the bacteria population to diffuse based on population differences
* bactgrowptw: Grows the microbial community. The ptw suffix is deprecated, the code non longer simulates a piggyback the winner model
* microbepopptw: Generates the initial population of the microbial layer. The ptw suffix is deprecated


__nonlifesubs.f90__
Contains the subroutines which do no pertain the the evolution of the system. Mainly data handling and user input.

* printtofile: Accepts a 2-D array(3,N) which is printed in xyz format
* dircheck: Checks to see if a directory exists and creates it if not
* datacollect: Handles data collection and output for the entire reef
* inputs: Handles user input
* chartoup: Turns a char variable into uppercase
* domainout: Handles data collection and output for specific regions (coral//algae//interface)
* var/_adjuster: Adjusts parameters at a predetermined time based on user input

__P2Smod.f90__
Contains the global variables and functions for the simulation

* fishdelta: Returns change in fish population
* percentcor: Returns percentage of floor with coral
* virpopptw: Returns phage population. The ptw suffix is deprecated
* tempratio: Returns the temperance ratio
* lysratio: Returns the lysogen to bacteria ratio
* bactouch: Returns the bacterial influence parameter
