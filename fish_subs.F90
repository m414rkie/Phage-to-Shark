subroutine fishinteraction(modify)

! Interaction of fish with algae layer. Lessens the pressure of algae against coral

use globalvars, only: fish, fish_carry

	real*8,intent(inout)	:: modify				! Input variable to be modified
	real*8								:: fisheat, carry_mod, grow_mod

	! Ratio of change in fish population to carrying capacity
	carry_mod = fish/fish_carry
	if (carry_mod .gt. 1.0) then
		carry_mod = 1.0
	end if
	! Ratio of fish growth to natural growth rate. Floor of 0
	grow_mod = 1.0 - carry_mod
	if (grow_mod .gt. 1.0) then
		grow_mod = 1.0
	else if (grow_mod .lt. 0.0) then
		grow_mod = 0.0
	end if

	fisheat = 0.9*grow_mod + 0.1*carry_mod

	if (fisheat .ge. 0.8) then
		fisheat = 0.8
	end if
	! Adjust input values
	modify = modify*(1.0 - fisheat)

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine shark(f)
! Subroutine to determine piscivore interactions

use globalvars, only : sharkMass, dayavg, numday, fish, shrkevt

implicit none
	real		:: catch, caught ! random variable, amount to be removed
	real		:: hunger ! comparison variable
	integer	:: f ! Flag to determine if user is notified

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
