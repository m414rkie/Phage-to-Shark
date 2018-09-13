subroutine growth(x,y,arrin,arrout)
	
! Grows the input grid location based on value and neighbors.
	
use globalvars
use functions
	
implicit none
	integer, intent(in)							:: x, y					! Input coordinates
	real,dimension(grid,grid), intent(in) 		:: arrin				! Input array
	real,dimension(grid,grid), intent(out)		:: arrout				! Output array
	real										:: bactcoral, grow

! Initializations
bactcoral = 0.0

bactcoral = (bacteria(2*x,2*y)%totalpop+bacteria(2*x-1,2*y)%totalpop &
			+bacteria(2*x-1,2*y-1)%totalpop+bacteria(2*x,2*y-1)%totalpop)/(algaemod*avgpop*4.0)
			
if (bactcoral .lt. 0.0) then
	bactcoral = 0.0
end if

if (bactcoral .gt. 1.0) then
	bactcoral = 0.9
end if

grow = 1.0 + growpercent*(1.0 - bactcoral)

if (grow .lt. 1.0) then
	grow = 1.0
end if

where (arrout .gt. 5.0) arrout = 5.0

arrout(x,y) = arrin(x,y)*grow

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
	
	! Calls the fish layer to reduce the effect of algae on coral
	call fishinteraction(decayconst,x,y)
	
	! Coral being eaten.
	if (decayconst .lt. 0.0) then
		decayconst = 0.0
	end if
	
	! Coral less the algae eating it
	if (decayconst .gt. 0.0) then
		arrin(x,y) = arrin(x,y) - decayconst*real(algcount)
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


	fisheat = fisheatmult*fishdelta(sum(coral),sum(fish))/real(grid**2)

	modify = modify*8.0*fisheat

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
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine bactgrow

! Grows the bacteria layer, both totalpop and species

use globalvars
use functions
	
implicit none
	integer		:: i, j						! Looping integers
	real		:: delbactpop				! determines new species and change in bacteria population 
	real		:: percentevent
	
! Initializations
delbactpop = 0.0
	
do i = 1, 2*grid, 1
	
	do j = 1, 2*grid, 1
	
		lysperc = 0.0
		delbactpop = 0.0
	
		lysperc = real(lys(i,j)%totalpop)/real(bacteria(i,j)%totalpop)
		
		if (0.1*real(phage(i,j)%totalpop) .ge. 0.2*real(bacteria(i,j)%totalpop)) then
			bacteria(i,j)%totalpop = bacteria(i,j)%totalpop - int(0.2*real(bacteria(i,j)%totalpop))
		else if (0.1*real(phage(i,j)%totalpop) .lt. 0.2*real(bacteria(i,j)%totalpop)) then
			bacteria(i,j)%totalpop = bacteria(i,j)%totalpop - int(0.2*real(phage(i,j)%totalpop))
		end if
		
		! Finds change in population
		delbactpop = bacgrowth(real(bacteria(i,j)%totalpop),kbact(i,j),real(bacteria(i,j)%numspecies))

		! Determines how many new species show up
		bacteria(i,j)%numspecies = bacteria(i,j)%numspecies + int(delbactpop)

		call random_number(percentevent)

		if (percentevent .gt. (1.0 - lysperc)) then
			
			delbactpop = delbactpop + abundperc*real(bacteria(i,j)%totalpop)

		end if

		bacteria(i,j)%totalpop = bacteria(i,j)%totalpop + int(delbactpop)

	end do
	
end do
	
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
	
	
call system_clock(count=clock)
seed = clock + 8*(/(i-1,i=1,randall)/)
call random_seed(put=seed)
call random_number(catch)

if (catch .gt. (1.0-hunger)) then
	fish = fish*caught
	hunger = 0.0
	write(*,*) "SHARK!"
else
	hunger = hunger + 0.05
	fish = fish
end if

end subroutine
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine phagelysgrow

use globalvars

implicit none
	integer		:: delta, specdelta, phagecheck, lyscheck
	integer		:: i, j
	real		:: popratio, deltratio, specratio

phage%totalpop = int(real(phage%totalpop)*phagedie)
	
do i = 1, 2*grid, 1
	
	do j = 1, 2*grid, 1
			
		delta = 0
		specdelta = 0
		phagecheck = 0
		popratio = 0.0
		deltratio = 0.0
		specratio = 0.0
		
		delta = (bacteria(i,j)%totalpop - bacthold(i,j)%totalpop)

		deltratio = real(bacteria(i,j)%totalpop)/real(bacthold(i,j)%totalpop)
				
		specratio = real(phage(i,j)%totalpop)/real(bacteria(i,j)%numspecies)
		
		if (specratio .lt. 1.0) then
			specratio = 1.0
		end if
		
		if (specratio .ge. 4.0) then
			popratio = 0.0
		else
			popratio = 0.35
		end if
		
		if (deltratio .lt. 1.0) then
			deltratio = 1.0
		end if
		
		if ((deltratio .lt. 1.20) .and. (deltratio .ge. 1.0)) then
			popratio = popratio + 0.35
		else 
			popratio = popratio + 0.1
		end if		
		
		specdelta = (bacteria(i,j)%numspecies - bacthold(i,j)%numspecies)
		
		if ((specdelta .ge. phage(i,j)%numspecies) .or. (specdelta .ge. lys(i,j)%numspecies)) then
			specdelta = int(real(lys(i,j)%numspecies)*0.3)
		end if
		
		if (delta .lt. 0) then
			phagecheck = 50.0*abs(delta)*popratio + phagecheck
		else if (delta .gt. 0) then
			phagecheck = int(real(bacthold(i,j)%totalpop)*5.0) + phagecheck
		end if
	
	!!!!!!!!!!!JUST CHANGED THE FIVE RIGHT TTHERE
	
		lyscheck = int((1.0 - popratio)*delta)
	
		phage(i,j)%totalpop = phage(i,j)%totalpop + phagecheck
		lys(i,j)%totalpop = lys(i,j)%totalpop + lyscheck
	
		phage(i,j)%numspecies = phage(i,j)%numspecies + int(real(specdelta)*popratio)
		lys(i,j)%numspecies = lys(i,j)%numspecies + int(real(specdelta)*(1.0 - popratio))		
		
	end do

end do

where (phage%totalpop .lt. 0) phage%totalpop = 0.0
where (lys%totalpop .lt. 0) lys%totalpop = 0.0

end subroutine

subroutine bactgrowptw

use globalvars
use functions

implicit none
	real	:: burst, interact
	real    :: temratio
	integer :: i, j	
	integer :: bactdelta, phagetot

do i = 1, 2*grid, 1

	do j = 1, 2*grid, 1

		adsorp = (0.035/real(lys(i,j)%totalpop))
		bacdeath = 0.2!*real(bacteria(i,j)%totalpop)
		phlyratio = 0.0
		bactdelta = 0.0
	
		bacteria(i,j)%totalpop = int(sqrt((kbact(i,j)*phagedie)/(50.0*adsorp)))
		bactdelta = bacteria(i,j)%totalpop - bacthold(i,j)%totalpop
	
		if (bactdelta .ge. 0.0) then
			bacteria(i,j)%numspecies = bacteria(i,j)%numspecies + 4
			phage(i,j)%numspecies = phage(i,j)%numspecies + 4
			lys(i,j)%numspecies = lys(i,j)%numspecies + 4
		else if (bactdelta .lt. 0.0) then
			bacteria(i,j)%numspecies = bacteria(i,j)%numspecies - 4
			phage(i,j)%numspecies = phage(i,j)%numspecies - 4
			lys(i,j)%numspecies = lys(i,j)%numspecies - 4
		end if

		temratio = (real(phage(i,j)%totalpop + lys(i,j)%totalpop)/real(bacteria(i,j)%numspecies))
	
		if (temratio .le. 3.0) then
			phlyratio = 0.05
		else if ((temratio .gt. 3.0) .and. (temratio .le. 11.0)) then
			phlyratio = 0.4
		else if (temratio .gt. 11.0) then
			phlyratio = 0.9
		end if
		
		phagetot = int(virpopptw(kbact(i,j),float(bacteria(i,j)%totalpop)))
		
		phage(i,j)%totalpop = int(abs((1.0 - phlyratio)*real(phagetot)))
		lys(i,j)%totalpop = int(abs(phlyratio*real(phagetot)))
	end do
	
end do
	
end subroutine
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	












	