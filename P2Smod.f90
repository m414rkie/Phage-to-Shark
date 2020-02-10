module globalvars

implicit none

type microbevar
	sequence
	integer*8				:: totalpop
	integer*8 			:: numspecies
end type microbevar

! Input variables
	! Initial
	integer														:: grid ! Size of grid
	integer														:: numtime ! Number of time steps
	real															:: percentcover	! Initial percentage of coral cover
	real															:: threshold ! Avg. coral requirement for new coral
	real															:: sharkMass ! Mass of piscivores in reef
  real															:: dayavg ! Avg. number of days between shark events
	real															:: rate	! Bacteria Growth rate adjuster
	real															:: corBacNew ! Influence of bacteria on new coral/coral growth
	real															:: adsorpFac ! Factor influencing adsorption coefficient
	real															:: bacDeath ! Natural rate of bacterial death
	real															:: bacBurst ! Number of phage that emerge from a lysis event
	real															:: phagedie	! Natural rate of phage death
	real															:: fish_ini ! Initial population of fish
	real															:: fgrowfact ! Rate of fish growth
	real															:: diffco ! Diffusion coefficient
	real															:: ly_mod ! Lysogen growth rate modifier
  ! Second values
	character*1												:: var_adjust_flag ! Flag determines if values will change
	integer														:: t_adj ! Holds the time that the variables will adjust
	real															:: threshold_2nd ! Avg. coral requirement for new coral
	real															:: sharkMass_2nd ! Mass of piscivores in reef
  real															:: dayavg_2nd ! Avg. number of days between shark events
	real															:: rate_2nd	! Bacteria Growth rate adjuster
	real															:: corBacNew_2nd ! Influence of bacteria on new coral/coral growth
	real															:: adsorpFac_2nd ! Factor influencing adsorption coefficient
	real															:: bacDeath_2nd ! Natural rate of bacterial death
	real															:: bacBurst_2nd ! Number of phage that emerge from a lysis event
	real															:: phagedie_2nd	! Natural rate of phage death
	real															:: fish_ini_2nd ! fish population, adjusted
	real															:: fgrowfact_2nd ! Rate of fish growth
	real															:: diffco_2nd ! Diffusion coefficient
	real															:: decayconst_2nd	! Percent of coral loss per nearby algae

	integer														:: clusnum ! Array size
	real															:: norm, nearsum, test	! Variables for interactions
	real															:: fish ! Holds fish population.
	real, allocatable									:: holding(:,:), coral(:,:)	! Layer names
	real(kind=8), allocatable					:: kbact(:,:)	! Holds carrying capacity for bacteria
	type (microbevar), allocatable		:: bacteria(:,:), phage(:,:), lys(:,:)	! Layer names
	type (microbevar), allocatable		:: bacthold(:,:) ! Bacteria temporary arry
	integer, allocatable							:: seed(:)	! Random number holding array
	integer														:: distance	! radial distance of coral clusters
	real															:: fisheat	! amount of pressure fish puts on algae
	real															:: fishtot, growavg ! Data holding variables
	real*8														:: kalg, kcor, kbar ! Microbial carrying capacity. algae/coral/interface
	real															:: hunger, shrkevt ! Shark variables
	integer														:: numnew ! Holds number of coral growths total
	real															:: avgspec ! Avg. num species
	logical,allocatable								:: check(:,:) ! Array for use in finding non-coral gridpoints
	integer														:: numday ! Number of days between shark events
	! Function Variables
	! Sub Variables
	real															:: coraltot ! Total coral, not percentage
	real															:: caught	! Amount of fish left after shark (%)
	real															:: dayavgtot ! For use in finding avg. number of days between shark events
	real															:: fish_carry
	! PTW vars
	character*1												:: disFLag ! Flag for disasters
	integer														:: disSevere, sickDays ! Severity of disasters

end module

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module functions

contains

real function fishdelta(pop)
! Function determines change in fish population

use globalvars

implicit none
	real		:: pop

	! standard LK growth equation
	fishdelta = fgrowfact*(1.0 - pop/(fish_carry))*pop

end function fishdelta

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real function percentcor(size)
! Function determines percentage of coral cover

use globalvars

implicit none
	integer					:: size ! Size of square array
	integer					:: corcount ! Holds count of points w/ coral
	logical					:: wherecor(size,size) ! grid of logic

! Set .true. for coral points
wherecor = (coral .ne. 0.0)

! Count number of coral pieces
corcount = count(wherecor)

! Find percentage
percentcor = float(corcount)/(float(size)**2)

end function
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real*8 function virpop_dom(carry,bacpop,ad,spec)
! Steady state solution for the phage population
! Uses LK equations as a base

use globalvars

implicit none
	real*8	:: carry, bacpop, ad, spec
					 ! capacity; bacteria pop; effective adsorption coefficient, richness

virpop_dom = spec*(rate*(1.0 - (bacpop/carry)) - bacdeath)/ad

end function
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real*8 function vmr_calc(i,j)
! Finds the temperance ratio
!** Deprecated **

use globalvars

implicit none
	integer		:: i, j

! Phage population over bacteria population
vmr_calc = (real(phage(i,j)%totalpop,8)/real(bacteria(i,j)%totalpop+lys(i,j)%totalpop,8))

end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real*8 function comp_carry(k_diff,spec,pop)
! Calculates the effective lysogen carrying capacity

use globalvars

implicit none
	integer*8		:: K_diff, pop
	real*8			:: spec ! Species Count
	real*8			:: max_pak = 2.5E13 ! Physical limit

comp_carry = real(K_diff,8)*(spec*real(pop,8)/(max_pak))

end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer*8 function lys_pop(lys_carry_loc)
! Calculates the lysogen population

use globalvars

implicit none
	real*8		:: lys_carry_loc ! Local lysogen carrying capacity
	real*8		:: ir = 4.167E-8 ! Induction rate
	real*8		:: rate_l ! Lysogenic growth rate

! Adjust based on user input. Base is lytic growth rate
rate_l = rate*ly_mod

lys_pop = int(lys_carry_loc-(lys_carry_loc/rate_l)*(ir-bacdeath),8)

end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real*8 function lysratio(i,j)
! Finds lysogen ratio
! ** Deprecated **

use globalvars

implicit none
	integer		:: i, j

! Lysogen pop over bacteria pop
lysratio = (real(lys(i,j)%totalpop,8)/real(bacteria(i,j)%totalpop,8))

if (lysratio .lt. 0) then
	lysratio = 0.0
end if

if (lysratio .gt. 1.0) then
	lysratio = 1.0
end if

end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real*8 function bactouch(pop)
! Determines level of bacterial influence for a given population

use globalvars

implicit none
	real*8	:: pop, slope ! local population, damage slope
	real*8	:: maxpop, minpop ! Determine max or min influence

! Initialize values - Based on in vivo measurements
maxpop = 25.0*1.0E7
minpop = 25.0*1.0E5
slope = 1.0/(maxpop-minpop)

! Limit minimum output
if (pop .lt. minpop) then
	bactouch = 0.0
! Limit maximum output
else if (pop .gt. maxpop) then
	bactouch = 1.0
else
	bactouch = slope*pop
end if

end function

end module
