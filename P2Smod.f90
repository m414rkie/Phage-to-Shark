module globalvars

implicit none

type microbevar
	sequence
	integer				:: totalpop
	integer 			:: numspecies
end type microbevar


	integer									:: grid, clusnum						! Array size
	real									:: norm, nearsum, test					! Variables for interactions
	real									:: fish
	real, allocatable						:: holding(:,:), coral(:,:)				 ! Layer names
	real, allocatable						:: kbact(:,:)							! Holds carrying capacity for bacteria
	type (microbevar), allocatable			:: bacteria(:,:), phage(:,:), lys(:,:)	! Layer names
	type (microbevar), allocatable			:: bacthold(:,:) 
	integer, allocatable					:: seed(:)								! Random number holding array
	integer									:: distance								! radial distance of coral clusters
	real									:: percentcover							! Percent of grid to have coral on it 'groundcover'
	real									:: fgrowfact, fisheat
	real									:: fishtot
	real									:: kalg, kcor, kbar
	real									:: hunger, shrkevt, clock
	integer									:: numnew = 0
	real									:: popconstant
	real									:: pi = acos(-1.0)
	integer									:: randall = 33
	real									:: threshold
	real									:: alpha, beta, avgspec
	logical,allocatable						:: check(:,:)
	real									:: lysperc
	real									:: coralfishmult
	real, allocatable						:: coralpercent(:,:)
	integer									:: numtime, numday	    					! Number of timesteps
	! Function Variables					
	! Sub Variables
	real									:: growpercent, growpercmod			! Flat percentage growth for coral
	real									:: decayconst						! Percent of coral loss per nearby algae 
	real									:: fisheatmult, coraltot			! Multiplier for fisheat
	real									:: caught, dayavg					! Amount of fish left after shark (%), Avg. num. of days
	real									:: dayavgtot
	real									:: phagedie							! Amount of phage that don't die each cycle
	! PTW vars
	real									:: bacDeath, adsorp
	character*1								:: disFLag
	integer									:: disSevere, sickDays
	real									:: sharkMass
	real									:: rate								! Bacteria Growth rate adjuster
	real									:: corBacNew, corBacGrow
	real									:: adsorpFac
	real									:: bacBurst
	
end module


module functions

contains

real function fishdelta(pop)

use globalvars

implicit none
	real		:: pop
	
	coralfishmult = 1000.0*percentcor(grid)
	
	fishdelta = fgrowfact*(1.0 - pop/(coralfishmult))*pop
	
end function fishdelta

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real function percentcor(size)

use globalvars

implicit none
	integer					:: size
	integer					:: corcount
	logical					:: wherecor(size,size)
	
wherecor = (coral .ne. 0.0)

corcount = count(wherecor)

percentcor = float(corcount)/(float(size)**2)

end function
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real function virpopptw(carry,bacpop)

use globalvars

implicit none
	real	:: carry, bacpop
	
virpopptw = (rate*(1.0 - (bacpop/carry)) - bacdeath)/adsorp

end function
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real function tempratio(i,j)

use globalvars

implicit none
	integer :: i, j
	
	
tempratio = (real(phage(i,j)%totalpop)/real(bacteria(i,j)%numspecies))
	
end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real function bactouch(pop)

use globalvars

implicit none
	real	:: pop, slope
	real	:: maxpop, minpop
	
maxpop = 10000000.0
minpop = 100000.0
slope = -1.0/(maxpop-minpop)

if (pop .lt. minpop) then
	bactouch = 0.0
end if

if (pop .gt. maxpop) then
	bactouch = -1.0
end if


pop = pop/25.0
pop = pop*100000000
write(*,*) "pop", pop

bactouch = slope*pop
	
end function
	
end module