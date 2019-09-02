subroutine kgrid

! Finds the carrying capacity of each gridpoint for the bacteria layer

use globalvars

implicit none
	integer							:: i, j								! Looping integers

kalg = 225.0E9 ! 25*9e9 ! Factor of 25 since our grid is 5cm X 5cm at the microbe
kcor = 75.0E9 ! 25*3e9     level
kbar = 300.0E9 ! 25*12e9

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

use globalvars

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

use globalvars
use functions

implicit none
	real*8    :: vmr_loc ! VMR at current grid point
	real*8		:: lys_carry_loc ! Local lysogen carrying capacity
	real*8		:: K_del ! Difference in carrying capacity and usage
	integer*8	:: bact_spec
	integer   :: i, j ! Looping integers
	integer*8 :: bactdelta, phagetot ! Change in bact. pop; total phage pop
	real*8 	  :: bactchange, phlyratio ! Normalized bact change; phage-lysogen ration
	real*8		:: lys_frac ! Fraction of bacteria that are lysogenic
	real*8		:: Adsorp_eff ! Adjusted adsorption coefficient
	real*8		:: adsorp = 4.8E-10 ! Unadjusted adsorption coefficient

! Loops
do i = 1, 2*grid, 1

	do j = 1, 2*grid, 1

		! Determine lysogenic fraction of bacteria, function in P2Smod.f90
		lys_frac = lysratio(i,j)

		! Determine the temperance ratio, function in P2Smod.f90
		vmr_loc = vmr_calc(i,j)

		bact_spec = bacteria(i,j)%numspecies

		! Adsorption coefficient
		Adsorp_eff = adsorp

  	! Bacteria - Steady State, Compartment model
		bacteria(i,j)%totalpop = int(bact_spec*(phagedie/(bacBurst*Adsorp_eff)),8)

		! Limit population to carrying capacity
		if (bacteria(i,j)%totalpop .gt. int(kbact(i,j),8)) then
			bacteria(i,j)%totalpop = int(kbact(i,j),8)
		end if

		! Phage - Steady State, Compartment model. Function in P2Smod.f90
		phagetot = int(virpop_dom(kbact(i,j),real(bacteria(i,j)%totalpop,8), &
										Adsorp_eff,bact_spec),8)

		phage(i,j)%totalpop = phagetot

		K_del = (kbact(i,j) - bacteria(i,j)%totalpop)

		lys_carry_loc = comp_carry(K_del,bact_spec,bacteria(i,j)%totalpop)

		! Set number of lysogenic bacteria
		lys(i,j)%totalpop = lys_pop(lys_carry_loc)

		! Determine the species count from population
		lys(i,j)%numspecies = int(75.374*float(lys(i,j)%totalpop)**alpha)
		phage(i,j)%numspecies = int(75.374*float(phage(i,j)%totalpop)**alpha)
		bacteria(i,j)%numspecies = int(75.374*float(bacteria(i,j)%totalpop)**alpha)

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
	integer :: i, j


area = real((2*grid)**2)

bacteria%totalpop = int(0.01*kbact,8)

phage%totalpop = 5*bacteria%totalpop

lys%totalpop = int(0.4*real(bacteria%totalpop),8)

do i = 1, 2*grid, 1
	do j = 1, 2*grid, 1
		lys(i,j)%numspecies = int(75.374*float(lys(i,j)%totalpop)**alpha)
		phage(i,j)%numspecies = int(75.374*float(phage(i,j)%totalpop)**alpha)
		bacteria(i,j)%numspecies = int(75.374*float(bacteria(i,j)%totalpop)**alpha)
	end do
end do

! Write statements
average = sum(bacteria%numspecies)/area
write(*,*) "Average number of species:" ,average

end subroutine
