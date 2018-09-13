module globalvars

implicit none

type microbevar
	sequence
	integer				:: totalpop
	integer 			:: numspecies
end type microbevar


	integer									:: grid, clusnum						! Array size
	real									:: norm, nearsum, test					! Variables for interactions
	real, allocatable						:: holding(:,:), coral(:,:), fish(:,:)  ! Layer names
	real, allocatable						:: kbact(:,:)							! Holds carrying capacity for bacteria
	type (microbevar), allocatable			:: bacteria(:,:), phage(:,:), lys(:,:)	! Layer names
	type (microbevar), allocatable			:: bacthold(:,:) 
	integer, allocatable					:: seed(:)								! Random number holding array
	integer									:: clock, distance						! System time and radial distance of coral clusters
	real									:: percentcover							! Percent of grid to have coral on it 'groundcover'
	real									:: fishlocal, fgrowfact, fisheat
	real									:: hunger
	integer									:: numnew = 0
	real									:: popconstant
	real									:: pi = acos(-1.0)
	integer									:: randall = 12
	real									:: avgpop, threshold
	integer									:: maxspec, minispec
	logical,allocatable						:: check(:,:)
	real									:: phlyratio, lysperc
	real									:: coralfishmult
	real, allocatable						:: coralpercent(:,:)
	integer									:: numtime	    					! Number of timesteps and clusters of coral
	! Function Variables					
	real									:: rate								! Bacteria Growth rate adjuster
	! Popsub Variables
	real									:: tightclustermult					! Determines the increase in coral in cluster
	! Sub Variables
	real									:: growpercent 						! Flat percentage growth for coral
	real									:: decayconst						! Percent of coral loss per nearby algae 
	real									:: fisheatmult						! Multiplier for fisheat
	real									:: algaemod, coralmod, barriermod	! Multipliers for bact. carrying capacity
	real									:: specmult							! Species growth multiplier for bact.
	real									:: abundperc						! Percentage growth for an abundance shift
	real									:: caught							! Amount of fish left after shark (%)
	real									:: phagedie							! Amount of phage that don't die each cycle
	! PTW vars
	real									:: bacdeath, adsorp, bactmod
	
end module


module functions

contains

real function fishdelta(input,pop)

use globalvars

implicit none
	real		:: input, pop
	
	fishdelta = fgrowfact*(coralfishmult*input - pop)
	
end function fishdelta

real function bacgrowth(totalpop,carry,spec)

use globalvars

implicit none
	real		:: totalpop, carry, spec
	
bacgrowth = rate*(1.0 - (totalpop/carry))*spec
	
end function bacgrowth

real function percentcor(size)

use globalvars

implicit none
	integer					:: size
	integer					:: algcount, corcount
	logical					:: wherecor(size,size)
	
wherecor = (coral .ne. 0.0)

corcount = count(wherecor)

percentcor = float(corcount)/(float(size)**2)

end function

real function virpopptw(carry,bacpop)

use globalvars

implicit none
	real	:: carry, bacpop
	
virpopptw = (rate*(1.0 - (bacpop/carry)) - bacdeath)/adsorp

end function
	
	




end module