subroutine kgrid

! Assigns the carrying capacity of each gridpoint for the bacteria layer

use globalvars, only: kbact, grid, coral, kalg, kcor, kbar

implicit none
	integer			:: i, j			! Looping integers

kalg = 25.0E7 ! 25*1e7 ! Factor of 25 since our grid is 5cm X 5cm at the microbe
kcor = 25.0E6 ! 25*1e6     level
kbar = 125.0E6 ! 25*5e6

! Initialize to all algae
kbact = kalg

! Set coral locations
do i = 1, grid, 1

	do j = 1, grid, 1

		if (coral(i,j) .ne. 0) then
			kbact(2*i-1,2*j-1) = kcor
			kbact(2*i-1,2*j) = kcor
			kbact(2*i,2*j) = kcor
			kbact(2*i,2*j-1) = kcor
		end if

	end do

end do

! Loops to determine barrier interaction
do i = 1, 2*grid, 1

	do j = 1, 2*grid, 1

		if (i .lt. 2*grid) then
			if ((i .lt. 2*grid) .and. (kbact(i,j) .ne. kbact(i+1,j))) then
				kbact(i,j) = kbar
			end if
		end if

		if (j .lt. 2*grid) then
			if ((j .lt. 2*grid) .and. (kbact(i,j) .ne. kbact(i,j+1))) then
				kbact(i,j) = kbar
			end if
		end if

	end do

end do

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine diffuse

! Diffusion subroutine. Primary driver is population difference

use globalvars, only: bacteria, grid, diffco

implicit none
	real, dimension(2*grid,2*grid)	:: delta		! Holds overall change from diffusion
	integer													:: i, j			! Looping integers

! Initializations
delta = 0.0

! Working loops
do i = 1, 2*grid, 1

	do j = 1, 2*grid, 1

		! x+1
		if ((i .lt. 2*grid) .and. (bacteria(i,j)%totalpop .gt. bacteria(i+1,j)%totalpop)) then

			delta(i+1,j) = delta(i+1,j) +  diffco*real(bacteria(i,j)%totalpop - bacteria(i+1,j)%totalpop)
			delta(i,j) = delta(i,j) - diffco*real(bacteria(i,j)%totalpop - bacteria(i+1,j)%totalpop)

		end if

		! x-1
		if ((i .gt. 1) .and. (bacteria(i,j)%totalpop .gt. bacteria(i-1,j)%totalpop)) then

			delta(i-1,j) = delta(i-1,j) + diffco*real(bacteria(i,j)%totalpop - bacteria(i-1,j)%totalpop)
			delta(i,j) = delta(i,j) - diffco*real(bacteria(i,j)%totalpop - bacteria(i-1,j)%totalpop)

		end if

		! y+1
		if ((j .lt. 2*grid) .and. (bacteria(i,j)%totalpop .gt. bacteria(i,j+1)%totalpop)) then

			delta(i,j+1) = delta(i,j+1) + diffco*real(bacteria(i,j)%totalpop - bacteria(i,j+1)%totalpop)
			delta(i,j) = delta(i,j) - diffco*real(bacteria(i,j)%totalpop - bacteria(i,j+1)%totalpop)

		end if

		! x-1
		if ((j .gt. 1) .and. (bacteria(i,j)%totalpop .gt. bacteria(i,j-1)%totalpop)) then

			delta(i,j-1) = delta(i,j-1) + diffco*real(bacteria(i,j)%totalpop - bacteria(i,j-1)%totalpop)
			delta(i,j) = delta(i,j) - diffco*real(bacteria(i,j)%totalpop - bacteria(i,j-1)%totalpop)

		end if

		! x-1, y-1
		if ((j .gt. 1) .and. (i .gt. 1) .and. (bacteria(i,j)%totalpop .gt. bacteria(i-1,j-1)%totalpop)) then

			delta(i-1,j-1) = delta(i-1,j-1) + diffco*real(bacteria(i,j)%totalpop - bacteria(i-1,j-1)%totalpop)
			delta(i,j) = delta(i,j) - diffco*real(bacteria(i,j)%totalpop - bacteria(i-1,j-1)%totalpop)

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
bacteria%totalpop = bacteria%totalpop + int(delta)

where (bacteria%totalpop .lt. 0) bacteria%totalpop = 0

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine bactgrow_dom
! Subroutine to evolve the microbial population using the steady state solutions
! of the lotka-volterra eqns.

use globalvars, only: kbact, bacteria, phage, lys, phagedie, grid, kcor, kbar, &
 											kalg, bacthold, bacBurst
use functions, only: virpop_dom, comp_carry, lys_pop, richness

implicit none
	real*8		:: lys_carry_loc ! Local lysogen carrying capacity
	integer*8	:: K_del ! Difference in carrying capacity and usage
	integer   :: i, j ! Looping integers
	real*8		:: adsorp = 4.8E-10 ! Adsorption coefficient
	real			:: fish_imp ! Holds fish impact parameter
	real*8		:: burst_eff, cc ! Effective burst size, unmodded carrying capacity
	real			:: spec , D_ph ! Richness of system, effective phage death rate

fish_imp = 1.0
call fishinteraction(fish_imp)

! Loops
do i = 1, 2*grid, 1

	do j = 1, 2*grid, 1

		! Find effective burst size - No interaction with the fish layer
		! simple values
		cc = kbact(i,j)
		if (cc .eq. kcor) then
			burst_eff = bacBurst*2.0
			D_ph = phagedie*0.01
		else if (cc .eq. kalg) then
			burst_eff = bacBurst
			D_ph = phagedie
		else
			burst_eff = 0.01*bacBurst
			if (burst_eff .lt. 1.0) then
				burst_eff = 1.0
			end if
			D_ph = phagedie*2.0
		end if
  	! Bacteria - Steady State, Compartment model
		bacteria(i,j)%totalpop = int((D_ph/(burst_eff*adsorp)),8)

		spec = richness(real(bacteria(i,j)%totalpop,8),kbact(i,j),real(fish_imp,8),kbar)

		bacteria%numspecies = spec
		phage%numspecies = spec
		lys%numspecies = 1

		bacteria(i,j)%totalpop = int(real(spec,8)*bacteria(i,j)%totalpop,8)

		! Limit population to carrying capacity
	  if (bacteria(i,j)%totalpop .gt. int(kbact(i,j)*fish_imp,8)) then
			bacteria(i,j)%totalpop = int(kbact(i,j)*fish_imp,8)
		end if

		! Phage - Steady State, Compartment model. Function in P2Smod.f90
		phage(i,j)%totalpop = int(virpop_dom(fish_imp*kbact(i,j),real(bacteria(i,j)%totalpop,8), &
													adsorp,real(spec,8)),8)

		!! Lysogenic compartment
		! Find difference between carrying capacity and bacteria population
		K_del = (int(kbact(i,j)*fish_imp,8) - bacthold(i,j)%totalpop)

		! Determine carrying capacity in lysogenic compartment.
		! Function in P2Smod.F90
		lys_carry_loc = comp_carry(K_del,bacthold(i,j)%totalpop)

		! Set number of lysogenic bacteria
		lys(i,j)%totalpop = lys_pop(lys_carry_loc)

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

use globalvars

implicit none
	real*8	:: area, average
	integer	:: t

area = real((2*grid)**2)

bacteria%numspecies = 5
phage%numspecies = 5
lys%numspecies = 1

bacteria%totalpop = int(0.1*kbact,8)

phage%totalpop = 5*bacteria%totalpop

lys%totalpop = int(0.1*real(bacteria%totalpop),8)

bacthold = bacteria

write(*,*) "Initializing Microbial Layer"
! Cycle to remove instabilities
do t = 1, 25, 1

	call bactgrow_dom
	bacthold = bacteria

end do

! Write statements
average = sum(bacteria%numspecies)/area
write(*,*) "Average number of species:", average

end subroutine
