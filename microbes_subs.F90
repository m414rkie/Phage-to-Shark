subroutine kgrid

! Assigns the carrying capacity of each gridpoint for the bacteria layer

use globalvars, only: kbact, grid, coral, kalg, kcor, kbar

implicit none
	integer			:: i, j	! Looping integers
	real				:: locations(2*grid,2*grid)

kalg = 25.0E7  ! 25*1e7 ! Factor of 25 since our grid is 5X5X1 cm^3 at the
kcor = 25.0E6  ! 25*1e6   microbe level
kbar = 200.0E6 ! 25*8e6

locations = 0.0

! Initialize to all algae
kbact = kalg

! Set coral locations
do j = 1, grid, 1
	do i = 1, grid, 1

		if (coral(i,j) .ne. 0) then
			kbact(2*i-1,2*j-1) = kcor
			kbact(2*i-1,2*j) = kcor
			kbact(2*i,2*j) = kcor
			kbact(2*i,2*j-1) = kcor
		end if

	end do
end do

! Loops to determine barrier interaction
j_loop: do j = 1, grid, 1

	i_loop: do i = 1, grid, 1

		if (i .lt. grid) then
			if ((coral(i,j) .ne. coral(i+1,j))) then
				kbact(2*i,2*j) = kbar
				kbact(2*i+1,2*j) = kbar
				kbact(2*i,2*j+1) = kbar
				kbact(2*i+1,2*j+1) = kbar
			end if
		end if

		if (j .lt. grid) then
			if ((coral(i,j) .ne. coral(i,j+1))) then
				kbact(2*i,2*j) = kbar
				kbact(2*i+1,2*j) = kbar
				kbact(2*i,2*j+1) = kbar
				kbact(2*i+1,2*j+1) = kbar
			end if
		end if

	end do i_loop

end do j_loop

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine diffuse

! Diffusion subroutine. Primary driver is population difference

use globalvars, only: bacteria, grid, diffco

implicit none
	real*8, dimension(2*grid,2*grid)	:: delta		! Holds overall change from diffusion
	integer														:: i, j			! Looping integers

! Initializations
delta = 0.0

! Working loops
do j = 1, 2*grid, 1
	do i = 1, 2*grid, 1

		! x+1
		if ((i .lt. 2*grid) .and. (bacteria(i,j)%totalpop .gt. bacteria(i+1,j)%totalpop)) then
			delta(i+1,j) = delta(i+1,j) +  diffco*(bacteria(i,j)%totalpop - bacteria(i+1,j)%totalpop)
			delta(i,j) = delta(i,j) - diffco*(bacteria(i,j)%totalpop - bacteria(i+1,j)%totalpop)
		end if

		! x-1
		if ((i .gt. 1) .and. (bacteria(i,j)%totalpop .gt. bacteria(i-1,j)%totalpop)) then
			delta(i-1,j) = delta(i-1,j) + diffco*(bacteria(i,j)%totalpop - bacteria(i-1,j)%totalpop)
			delta(i,j) = delta(i,j) - diffco*(bacteria(i,j)%totalpop - bacteria(i-1,j)%totalpop)
		end if

		! y+1
		if ((j .lt. 2*grid) .and. (bacteria(i,j)%totalpop .gt. bacteria(i,j+1)%totalpop)) then
			delta(i,j+1) = delta(i,j+1) + diffco*(bacteria(i,j)%totalpop - bacteria(i,j+1)%totalpop)
			delta(i,j) = delta(i,j) - diffco*(bacteria(i,j)%totalpop - bacteria(i,j+1)%totalpop)
		end if

		! y-1
		if ((j .gt. 1) .and. (bacteria(i,j)%totalpop .gt. bacteria(i,j-1)%totalpop)) then
			delta(i,j-1) = delta(i,j-1) + diffco*(bacteria(i,j)%totalpop - bacteria(i,j-1)%totalpop)
			delta(i,j) = delta(i,j) - diffco*(bacteria(i,j)%totalpop - bacteria(i,j-1)%totalpop)
		end if

		! x-1, y-1
		if ((j .gt. 1) .and. (i .gt. 1) .and. (bacteria(i,j)%totalpop .gt. bacteria(i-1,j-1)%totalpop)) then
			delta(i-1,j-1) = delta(i-1,j-1) + diffco*(bacteria(i,j)%totalpop - bacteria(i-1,j-1)%totalpop)
			delta(i,j) = delta(i,j) - diffco*(bacteria(i,j)%totalpop - bacteria(i-1,j-1)%totalpop)
		end if

		! x+1, y-1
		if ((j .gt. 1) .and. (i .lt. 2*grid) .and. (bacteria(i,j)%totalpop .gt. bacteria(i+1,j-1)%totalpop)) then
			delta(i+1,j-1) = delta(i+1,j-1) + diffco*real(bacteria(i,j)%totalpop - bacteria(i+1,j-1)%totalpop)
			delta(i,j) = delta(i,j) - diffco*real(bacteria(i,j)%totalpop - bacteria(i+1,j-1)%totalpop)
		end if

		! x-1, y+1
		if ((j .lt. 2*grid) .and. (i .gt. 1) .and. (bacteria(i,j)%totalpop .gt. bacteria(i-1,j+1)%totalpop)) then
			delta(i-1,j+1) = delta(i-1,j+1) + diffco*real(bacteria(i,j)%totalpop - bacteria(i-1,j+1)%totalpop)
			delta(i,j) = delta(i,j) - diffco*real(bacteria(i,j)%totalpop - bacteria(i-1,j+1)%totalpop)
		end if

		! x+1, y+1
		if ((j .lt. 2*grid) .and. (i .lt. 2*grid) .and. (bacteria(i,j)%totalpop .gt. bacteria(i+1,j+1)%totalpop)) then
			delta(i+1,j+1) = delta(i+1,j+1) + diffco*real(bacteria(i,j)%totalpop - bacteria(i+1,j+1)%totalpop)
			delta(i,j) = delta(i,j) - diffco*real(bacteria(i,j)%totalpop - bacteria(i+1,j+1)%totalpop)
		end if

	end do
end do

! Final update and trim
bacteria%totalpop = bacteria%totalpop + delta

where (bacteria%totalpop .lt. 0) bacteria%totalpop = 0

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine bactgrow_dom
! Subroutine to evolve the microbial population using the steady state solutions
! of the lotka-volterra eqns.

use globalvars, only: kbact, bacteria, phage, lys, phagedie, grid, kcor, &
 											kalg, bacBurst
use functions, only: virpop_dom, lys_pop

implicit none
	integer :: i, j ! Looping integers
	real*8	:: adsorp = 4.8E-10 ! Adsorption coefficient
	real*8	:: fish_imp ! Holds fish impact parameter
	real*8	:: burst_eff ! Effective burst size
	real*8	:: cc ! Unmodded local carrying capacity
	real*8	:: D_ph ! effective phage death rate
	real*8  :: cc_eff ! effective carrying capacity, fish accounted for
	real*8 	:: burst_hi, burst_lo, ph_death_hi, ph_death_lo
	real*8	:: richness ! richness variables

burst_hi = bacBurst*4.0 ! increase to lower coral bac pop
burst_lo = bacBurst*0.25 ! decrease to increase alg bac pop
ph_death_hi = phagedie*8.0 ! increase to increase alg bac pop
ph_death_lo = phagedie*0.6 ! decrease to lower coral bac pop

fish_imp = 1.0
call fishinteraction(fish_imp)

! Loops
do i = 1, 2*grid, 1
	do j = 1, 2*grid, 1

		! Determine effective burst size, phage death rate.
		cc = kbact(i,j)
		if (cc .eq. kcor) then ! lowest
			burst_eff = burst_hi
			D_ph = ph_death_lo
		else if (cc .eq. kalg) then ! highest
			burst_eff = burst_lo
			D_ph = ph_death_hi
		else ! middle
			burst_eff = bacBurst
			D_ph = phagedie
		end if

		! establish the impact of fish
		cc_eff = cc*fish_imp
  	! Bacteria - Steady State
		bacteria(i,j)%totalpop = (D_ph/(burst_eff*adsorp))

		! microbial richness
		richness = fish_imp*fish_imp * cc/bacteria(i,j)%totalpop

		bacteria%numspecies = richness
		phage%numspecies = richness

		! expand to entire community
		bacteria(i,j)%totalpop = richness*bacteria(i,j)%totalpop

		! Limit population to carrying capacity
	  if (bacteria(i,j)%totalpop .gt. cc_eff) bacteria(i,j)%totalpop = cc_eff


		! Phage - Steady State, Compartment model. Function in P2Smod.f90
		phage(i,j)%totalpop = virpop_dom(cc,bacteria(i,j)%totalpop,&
													adsorp,richness)

		!! Lysogenic compartment
		! Set number of lysogenic bacteria
		lys(i,j)%totalpop = lys_pop(bacteria(i,j)%totalpop,cc)

	end do
end do

! Remove any negatives
where (phage%totalpop .lt. 0) phage%totalpop = 0
where (lys%totalpop .lt. 0) lys%totalpop = 0
where (bacteria%totalpop .lt. 0) bacteria%totalpop = 0

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine microbepop_dom
! Initial microbe layer populations

use globalvars, only : bacteria, phage, lys, kbact, grid

implicit none
	real*8	:: area, average
	integer	:: i

area = real((2*grid)**2)

bacteria%numspecies = 5
phage%numspecies = 5
lys%numspecies = 1

bacteria%totalpop = int(0.6*kbact)

phage%totalpop = 5*bacteria%totalpop

lys%totalpop = int(0.1*real(bacteria%totalpop))

write(*,*) "Initializing Microbial Layer"
! Cycle to remove instabilities
do i = 1, 20, 1
	call bactgrow_dom
end do

! Write statements
average = sum(bacteria%numspecies)/area
write(*,*) "Average number of species:", average

end subroutine
