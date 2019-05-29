subroutine kgrid

! Finds the carrying capacity of each gridpoint for the bacteria layer

use globalvars

implicit none
	integer							:: i, j								! Looping integers

kalg = 9000.0
kcor = 3000.0
kbar = 12000.0

! Initializations
kbact = kalg

! Loops for initial set up, no barrier interaction
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
			if ((j .gt. 2*grid) .and. (kbact(i,j) .ne. kbact(i,j+1))) then
				kbact(i,j) = kbar
			end if
		end if

	end do

end do

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine diffuse

! Diffusion subroutine. Primary driver is population pressure

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

subroutine bactgrowptw

use globalvars
use functions

implicit none
	real    :: temratio
	integer :: i, j
	integer :: bactdelta, phagetot
	real 	  :: bactchange, phlyratio

do i = 1, 2*grid, 1

	do j = 1, 2*grid, 1

		temratio = tempratio(i,j)
		adsorp =(adsorpFac/real(lys(i,j)%totalpop))
		bactdelta = 0.0
	! PTW Model steady-state
	!	bacteria(i,j)%totalpop = int(sqrt((kbact(i,j)*phagedie)/(bacBurst*adsorp)))

	! Standard model steady state
		bacteria(i,j)%totalpop = ceiling(phagedie/(bacBurst*adsorp))
		if (bacteria(i,j)%totalpop .gt. nint(kbact(i,j))) then
			bacteria(i,j)%totalpop = nint(kbact(i,j))
		end if

		bactdelta = bacteria(i,j)%totalpop - bacthold(i,j)%totalpop

		bactchange = bactdelta/real(kbact(i,j))))

		if (temratio .le. 3.0) then
			phlyratio = 0.05 + bactchange
		else if ((temratio .gt. 3.0) .and. (temratio .le. 11.0)) then
			phlyratio = 0.4 + bactchange
		else if (temratio .gt. 11.0) then
			phlyratio = 0.9	+ bactchange
		end if

		if (phlyratio .gt. 0.95) then
			phlyratio = 0.95
		end if

		phagetot = nint(virpopptw(kbact(i,j),float(bacteria(i,j)%totalpop)))

		phage(i,j)%totalpop = phagetot
		lys(i,j)%totalpop = int(abs(phlyratio*real(bacteria(i,j)%totalpop)))

		lys(i,j)%numspecies = int(75.374*float(lys(i,j)%totalpop)**alpha)
		phage(i,j)%numspecies = int(75.374*float(phage(i,j)%totalpop)**alpha)
		bacteria(i,j)%numspecies = int(75.374*float(bacteria(i,j)%totalpop)**alpha)

	end do

end do

where (phage%totalpop .lt. 0) phage%totalpop = 0
where (lys%totalpop .lt. 0) lys%totalpop = 0

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine microbepopptw

use globalvars

implicit none
	real	:: area, average
	integer :: i, j


area = real((2*grid)**2)

bacteria%totalpop = int(0.6*kbact)

phage%totalpop = 5*bacteria%totalpop

lys%totalpop = int(0.8*real(bacteria%totalpop))
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
