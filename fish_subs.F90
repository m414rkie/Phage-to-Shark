subroutine fishinteraction(modify)

! Interaction of fish with algae layer. Lessens the pressure of algae against coral

use functions, only: fishdelta
use globalvars, only: fish, fgrowfact, fish_carry, fisheatmult

	real,intent(inout)			:: modify				! Input variable to be modified

	! Ratio of change in fish population to carrying capacity
	fisheat = fisheatmult*fishdelta(fish)/fish + fgrowfact*fish/fish_carry
	! Set maximal values
	if (fisheat .lt. 0.0) then
		fisheat = fgrowfact*fish/fish_carry
	end if

	if (fisheat .ge. 0.99) then
		fisheat = 0.99
	end if

	! Adjust input values
	modify = modify*(1.0 - fisheat)
	write(*,*) "Fish Modifiers", fisheat

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine shark(f)
! Subroutine to determine piscivore interactions

use globalvars

implicit none
	real			:: catch ! Will determine if hunt is succesful
	integer		:: f ! Flag to determine if user is notified

call random_number(catch)

! Determine the number to beat.
hunger = (dayavg - 1.0)/dayavg
shrkevt = 0.0

! The 4.5 is without shark population evolving, just maintaining
caught = sharkMass*4.5 !sharkMass*5.1885 Second part is with shark growth
caught = dayavg*0.002739*caught ! Adjust from yearly to daily numbers

! Determine if hunt is succesful, adjust fish population if so
if (catch .ge. hunger) then
	fish = fish - caught
	numday = numday + 1
	shrkevt = 1.0
	if (f .eq. 1) then
		write(*,*) "SHARK!"
	end if
end if

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
