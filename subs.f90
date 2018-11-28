subroutine growth(x,y,arrin,arrout)
	
! Grows the input grid location based on value and neighbors.
	
use globalvars
use functions
	
implicit none
	integer, intent(in)							:: x, y					! Input coordinates
	real,dimension(grid,grid), intent(in) 		:: arrin				! Input array
	real,dimension(grid,grid), intent(out)		:: arrout				! Output array
	real										:: bactcoral, grow
	integer										:: i

! Initializations
bactcoral = 0.0

call random_seed(size=randall)
call cpu_time(clock)
seed = clock + (/(i-1,i=1,randall)/)
call random_seed(put=seed)

bactcoral = real((bacteria(2*x,2*y)%totalpop+bacteria(2*x-1,2*y)%totalpop &
			+bacteria(2*x-1,2*y-1)%totalpop+bacteria(2*x,2*y-1)%totalpop))/real(4*algaemod*avgpop)
			
if (bactcoral .lt. 0.0) then
	bactcoral = 0.0
end if

if (bactcoral .gt. 1.0) then
	bactcoral = 0.99
end if

call random_number(growpercent)

growpercent = growpercent*0.1
grow = 1.0 + growpercent*(1.0 - bactcoral)

if (grow .lt. 1.0) then
	grow = 1.0
end if

arrout(x,y) = arrin(x,y)*grow

where (arrout .gt. 5.0) arrout = 5.0

end subroutine
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
subroutine decay(x,y,arrin)
	
! Represents the algae killing the coral

use globalvars
	
implicit none
	integer,intent(in)			:: x, y			! Input coordinates
	real,dimension(grid,grid)	:: arrin		! Input array
	integer						:: algcount		! Amount of algae near input coordinates
	
	! Initializations
	algcount = 0	
	decayconst	= 0.03

	
	! Checks for algae around the input gridpoint and out-of-bounds
	if ((x .gt. 1) .and. (holding(x-1,y) .eq. 0.0)) then
		algcount = algcount + 1
	end if
	
	if ((x .lt. grid) .and. (holding(x+1,y) .eq. 0.0)) then
		algcount = algcount + 1
	end if

	if ((y .lt. grid) .and. (holding(x,y+1) .eq. 0.0)) then
		algcount = algcount + 1
	end if
	
	if ((y .gt. 1) .and. (holding(x,y-1) .eq. 0.0)) then
		algcount = algcount + 1
	end if

	if ((x .lt. grid) .and. (y .lt. grid) .and. (holding(x+1,y+1) .eq. 0.0)) then
		algcount = algcount + 1
	end if

	if ((x .gt. 1) .and. (y .gt. 1) .and. (holding(x-1,y-1) .eq. 0.0)) then
		algcount = algcount + 1
	end if
	
	if ((x .lt. grid) .and. (y .gt. 1) .and.(holding(x+1,y-1) .eq. 0.0)) then
		algcount = algcount + 1
	end if
	
	if ((x .gt. 1) .and. (y .lt. grid) .and. (holding(x-1,y+1) .eq. 0.0)) then
		algcount = algcount + 1
	end if
	
	if (algcount .lt. 1) then
		return
	end if
	
	if(algcount .eq. 0) then
		decayconst = 0.0
	else
		call fishinteraction(decayconst,x,y)
		decayconst = decayconst*float(algcount)
	end if
	! Calls the fish layer to reduce the effect of algae on coral
	
	! Coral being eaten.
	if (decayconst .lt. 0.0) then
		decayconst = 0.0
	end if
				
	! Coral less the algae eating it
	if (decayconst .gt. 0.0) then
		arrin(x,y) = arrin(x,y) - decayconst
	end if
	! Resets negative values to zero
	if (arrin(x,y) .le. 0.05) then
		arrin(x,y) = 0.0
	end if
	if (arrin(x,y) .gt. 5.0) then
		arrin(x,y) = 5.0
	end if

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine fishinteraction(modify,i,j)

! Interaction of fish with algae layer. Lessens the pressure of algae against the fish.

use functions
use globalvars

	real						:: modify				! Input variable to be modified
	integer,intent(in)			:: i, j					! Looping integers


	fisheat = fisheatmult*fishdelta(sum(coral),sum(fish))/(sum(coral)*coralfishmult)
	
	if (fisheat .ge. 0.5) then
		fisheat = 0.5
	end if

	modify = modify*(1.0-fisheat)*(1.0 - 0.25*(sum(fish)/(sum(coral)*coralfishmult)))

end subroutine
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
subroutine kgrid

! Finds the carrying capacity of each gridpoint for the bacteria layer

use globalvars

implicit none
	integer							:: i, j								! Looping integers
	real,dimension(2*grid,2*grid)	:: kdelta							! Change in carrying capacity

! Initializations 
kbact = avgpop
kdelta = 0.0

! Loops for initial set up, no barrier interaction
do i = 1, grid, 1
	
	do j = 1, grid, 1
	
		if (coral(i,j) .ne. 0) then 
			kbact(2*i-1,2*j-1) = avgpop*coralmod
			kbact(2*i-1,2*j) = avgpop*coralmod
			kbact(2*i,2*j) = avgpop*coralmod
			kbact(2*i,2*j-1) = avgpop*coralmod
		else if (coral(i,j) .eq. 0) then
			kbact(2*i-1,2*j-1) = avgpop*algaemod
			kbact(2*i,2*j) = avgpop*algaemod
			kbact(2*i,2*j-1) = avgpop*algaemod
			kbact(2*i-1,2*j) = avgpop*algaemod
		end if
			
	end do
	
end do

! Loops to determine barrier interaction
do i = 1, 2*grid, 1
	
	do j = 1, 2*grid, 1

		if ((i .lt. 2*grid) .and. (kbact(i,j) .ne. kbact(i+1,j))) then
			kdelta(i,j) = avgpop*barriermod - kbact(i,j)
		end if
		
		if ((j .gt. 2*grid) .and. (kbact(i,j) .ne. kbact(i,j+1))) then
			kdelta(i,j) = avgpop*barriermod - kbact(i,j)
		end if
	
	end do
	
end do

! Final updating of the layer
kbact = kbact + kdelta

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
subroutine diffuse

! Diffusion subroutine. Primary driver is population pressure

use globalvars

implicit none
	real, dimension(2*grid,2*grid)				:: delta		! Holds overall change from diffusion
	integer										:: i, j			! Looping integers
	real										:: diffco		! Diffusion coefficient
	
! Initializations
diffco = 0.00001
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
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine mixing

! Mixing subroutine for redistributing species

use globalvars

implicit none
	integer									:: i,j 			! Looping integers
	real									:: mixpress		! Pressure for mixing
	integer									:: delta		! Change in number of species
								
mixpress = 0.0001

! Working loops
do i = 1, 2*grid, 1
	
	do j = 1, 2*grid, 1
		
		delta = 0
		! x+1
		if (i .lt. 2*grid) then
	
			delta = delta + bacteria(i,j)%numspecies - bacteria(i+1,j)%numspecies
			
		end if
		
		! x-1
		if (i .ne. 1) then
		
			delta = delta + bacteria(i,j)%numspecies - bacteria(i-1,j)%numspecies
		
		end if
		
		! y+1
		if (j .lt. 2*grid) then
			
			delta = delta + bacteria(i,j)%numspecies - bacteria(i,j+1)%numspecies
			
		end if
		
		! y-1
		if (j .ne. 1) then
			
			delta = delta + bacteria(i,j)%numspecies - bacteria(i,j-1)%numspecies
			
		end if
		
		! x-1, y-1
		if ((j .gt. 1) .and. (i .gt. 1)) then
			
			delta = delta + bacteria(i,j)%numspecies - bacteria(i-1,j-1)%numspecies
			
		end if
		
		! x+1, y-1
		if ((j .gt. 1) .and. (i .lt. 2*grid)) then
			
			delta = delta + bacteria(i,j)%numspecies - bacteria(i+1,j-1)%numspecies
			
		end if
		
		! x-1, y+1
		if ((j .lt. 2*grid) .and. (i .gt. 1)) then
			
			delta = delta + bacteria(i,j)%numspecies - bacteria(i-1,j+1)%numspecies
			
		end if
		
		! x+1, y+1
		if ((j .lt. 2*grid) .and. (i .lt. 2*grid)) then
			
			delta = delta + bacteria(i,j)%numspecies - bacteria(i+1,j+1)%numspecies
			
		end if
		
		if (delta .lt. 0) then
			delta = 0
		end if
		
			bacteria(i,j)%numspecies = bacteria(i,j)%numspecies + int(real(delta)*mixpress)
		
	end do
	
end do

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine shark

use globalvars

implicit none
	real			:: catch
	integer			:: i
	
	
call cpu_time(clock)
seed = clock + 8*(/(i-1,i=1,randall)/)
call random_seed(put=seed)
call random_number(catch)

hunger = (dayavg - 1.0)/dayavg
shrkevt = 0.0
if (catch .ge. hunger) then
	fish = fish*caught
	numday = numday + 1
	shrkevt = 1.0
	write(*,*) "SHARK!"
else
	fish = fish
end if

end subroutine
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine bactgrowptw

use globalvars
use functions

implicit none
	real	:: burst, interact
	real    :: temratio
	integer :: i, j	
	integer :: bactdelta, phagetot
	real	:: phagechange, phlyratio

do i = 1, 2*grid, 1

	do j = 1, 2*grid, 1

		temratio = tempratio(kbact(i,j),i,j)
		adsorp =(0.02/real(lys(i,j)%totalpop))
		bacdeath = 0.2
		bactdelta = 0.0
	
		bacteria(i,j)%totalpop = int(sqrt((kbact(i,j)*phagedie)/(50.0*adsorp)))
		bactdelta = bacteria(i,j)%totalpop - bacthold(i,j)%totalpop

		phagechange = rate*(1.0-(real(bacteria(i,j)%totalpop)/real(kbact(i,j))))	

		if (temratio .le. 3.0) then
			phlyratio = 0.05 + phagechange
		else if ((temratio .gt. 3.0) .and. (temratio .le. 11.0)) then
			phlyratio = 0.4 + phagechange
		else if (temratio .gt. 11.0) then
			phlyratio = 0.9	+ phagechange
		end if
			
		if (phlyratio .gt. 0.95) then
			phlyratio = 0.95
		end if
		
		phagetot = int(virpopptw(kbact(i,j),float(bacteria(i,j)%totalpop)))
		
		phage(i,j)%totalpop = phagetot
		lys(i,j)%totalpop = int(abs(phlyratio*real(bacteria(i,j)%totalpop)))

		lys(i,j)%numspecies = int(75.374*float(lys(i,j)%totalpop)**alpha)
		phage(i,j)%numspecies = int(75.374*float(phage(i,j)%totalpop)**alpha)
		bacteria(i,j)%numspecies = int(75.374*float(bacteria(i,j)%totalpop)**alpha)

	end do
	
end do
	
end subroutine
	
	