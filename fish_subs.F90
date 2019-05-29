subroutine fishinteraction(modify)

! Interaction of fish with algae layer. Lessens the pressure of algae against the fish.

use functions
use globalvars

	real						:: modify				! Input variable to be modified

	fisheat = fisheatmult*150.0*fishdelta(fish)/(1000.0*percentcor(grid))

	if (fisheat .lt. 0.0) then
		fisheat = 0.0
	end if

	if (fisheat .ge. 0.9) then
		fisheat = 0.9
	end if

	modify = modify*(1.0 - fisheat)

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine shark(f)

use globalvars

implicit none
	real			:: catch
	integer			:: f

call random_number(catch)

hunger = (dayavg - 1.0)/dayavg
shrkevt = 0.0

caught = sharkMass*5.1885
caught = dayavg*0.002739*caught

if (catch .ge. hunger) then
	fish = fish - caught
	numday = numday + 1
	shrkevt = 1.0
	if (f .eq. 1) then
		write(*,*) "SHARK!"
	end if
else
	fish = fish
end if

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
