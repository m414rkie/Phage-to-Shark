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
	real									:: fishdeltmult						! Factor in the fish growth attached to carrying capacity
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
	
end module


module functions

contains

real function fishdelta(input,pop)

use globalvars

implicit none
	real		:: input, pop
	
	fishdelta = -fgrowfact*(pop - fishdeltmult*coralfishmult*input)
	
end function fishdelta

real function bacgrowth(totalpop,specpop,carry)

use globalvars

implicit none
	real		:: totalpop, specpop, carry
	
bacgrowth = rate*(1.0 - (real(totalpop)/real(carry)))*real(specpop)
	
end function bacgrowth

real function percentcor(size)

use globalvars

implicit none
	integer					:: size
	integer					:: algcount, corcount
	logical					:: wherecor(size,size)
	
wherecor = (coral .ne. 0.0)

corcount = count(wherecor)

percentcor = real(corcount)/(real(size)**2)

end function







end module