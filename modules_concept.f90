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
	type (microbevar) , allocatable			:: bacteria(:,:), phage(:,:), lys(:,:)	! Layer names
	integer, allocatable					:: seed(:)								! Random number holding array
	integer									:: clock, distance						! System time and radial distance of coral clusters
	real									:: percentcover							! Percent of grid to have coral on it 'groundcover'
	real									:: fishlocal, fgrowfact, fisheat
	real									:: sharkmod, hunger
	integer									:: numnew = 0
	real									:: popconstant
	real									:: pi = acos(-1.0)
	integer									:: randall = 12
	real									:: avgpop, threshold
	integer									:: maxspec
	logical,allocatable						:: check(:,:)
	real, allocatable						:: perabund(:,:,:)
	real									:: phlyratio
	real									:: coralfishmult
	real, allocatable						:: coralpercent(:,:)
	integer									:: numtime	    				! Number of timesteps and clusters of coral


	
end module


module functions

contains

real function fishdelta(input,pop)

use globalvars

implicit none
	real		:: input, pop
	
	fishdelta = -fgrowfact*(coralfishmult*input - pop)
	
end function fishdelta

real function bacgrowth(totalpop,specpop,carry)

use globalvars

implicit none
	real		:: totalpop, specpop, carry
	real		:: rate	
	
rate = 0.8
bacgrowth = 0.0

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

percentcor = corcount/(real(size**2))

end function







end module