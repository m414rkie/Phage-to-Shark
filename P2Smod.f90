module globalvars

implicit none

type microbevar
	sequence
	integer*8				:: totalpop
	integer		 			:: numspecies
end type microbevar

! Input variables
	! Initial
	integer														:: grid ! Size of grid
	integer														:: numtime ! Number of time steps
	real															:: percentcover	! Initial percentage of coral cover
	real															:: sharkMass ! Mass of piscivores in reef
  real															:: dayavg ! Avg. number of days between shark events
	real															:: bacBurst ! Number of phage that emerge from a lysis event
	real															:: fish_ini ! Initial population of fish
	real															:: fgrowfact ! Rate of fish growth
	real															:: diffco ! Diffusion coefficient
  ! Second values
	character*1												:: var_adjust_flag ! Flag determines if values will change
	integer														:: t_adj ! Holds the time that the variables will adjust
	real															:: sharkMass_2nd ! Mass of piscivores in reef
  real															:: dayavg_2nd ! Avg. number of days between shark events
	real															:: bacBurst_2nd ! Number of phage that emerge from a lysis event
	real															:: fish_ini_2nd ! fish population, adjusted
	real															:: fgrowfact_2nd ! Rate of fish growth
	real															:: diffco_2nd ! Diffusion coefficient

! Internal Variables
	real															:: rate !  Bacterial Growth Rate
	real															:: bacdeath ! Natural Bacterial Death rate
  real															:: phagedie ! Natural rate of phage death
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

use globalvars, only: fgrowfact, fish_carry

implicit none
	real		:: pop

	! standard LK growth equation
	fishdelta = fgrowfact*(1.0 - pop/(fish_carry))*pop

end function fishdelta

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real function percentcor(size)
! Function determines percentage of coral cover

use globalvars, only: coral

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

use globalvars, only: rate, bacdeath

implicit none
	real*8	:: carry, bacpop, ad, spec
					 ! capacity; bacteria pop; effective adsorption coefficient, richness

virpop_dom = spec*(rate*(1.0 - (bacpop/carry)) - bacdeath)/ad

end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real*8 function comp_carry(k_diff,pop)
! Calculates the effective lysogen carrying capacity

implicit none
	integer*8		:: K_diff, pop
	real*8			:: max_pak = 2.5E13 ! Physical limit

comp_carry = real(K_diff,8)*(real(pop,8)/(max_pak))

end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer*8 function lys_pop(lys_carry_loc)
! Calculates the lysogen population

use globalvars, only: rate

implicit none
	real*8		:: lys_carry_loc ! Local lysogen carrying capacity
	real*8		:: ir = 4.167E-8 ! Induction rate
	real*8		:: rate_l, ly_mod ! Lysogenic growth rate

! Adjust real growth rate of lysogens
ly_mod = 2.0
rate_l = rate*ly_mod

lys_pop = int(lys_carry_loc-(lys_carry_loc/rate_l)*(ir),8)

end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
integer function richness(k_mod,k_max)
! Determines the richness of the microbial community

implicit none
	real*8	:: k_mod, k_max ! Size of sample population

richness = ceiling(100.0*(k_mod/k_max))

end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real*8 function bactouch(pop)
! Determines level of bacterial influence for a given population

implicit none
	real*8	:: pop, slope ! local population, damage slope
	real*8	:: maxpop, minpop ! Determine max or min influence

! Initialize values - Based on in vivo measurements
maxpop = 25.0*1.0E8
minpop = 25.0*1.0E4
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
