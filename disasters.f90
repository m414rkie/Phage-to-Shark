subroutine hurricane

use globalvars
use functions

implicit none
	real						:: inicor, fincor
	real						:: randcor
	integer						:: l, c
	integer						:: i, j
	integer, allocatable		:: corloch(:,:)
	
inicor = percentcor(grid)
	
if (disSevere .eq. 1) then
	fincor = 0.8*inicor
else if (disSevere .eq. 2) then
	fincor = 0.55*inicor
else if (disSevere .eq. 3) then
	fincor = 0.4*inicor
else if (disSevere .eq. 4) then
	fincor = 0.25*inicor
else
	fincor = 0.1*inicor
end if
	
check = (coral .ne. 0.0)

c = 0

! Sends the count to an integer and allocates algaeloc
l = count(check)
allocate(corloch(2,l))
corloch = 0

! Do loops to find exact coordinates of coral
do i = 1, grid, 1
	
	do j = 1, grid, 1
		
		if (coral(i,j) .ne. 0) then
			! Saves the locations 
			c = c + 1
			corloch(1,c) = i
			corloch(2,c) = j
		end if
		
	end do
	
end do

do while (percentcor(grid) .gt. fincor) 

call random_number(randcor)

randcor = randcor*float(c)

coral(corloch(1,nint(randcor)),corloch(2,nint(randcor))) = 0.0

end do

end subroutine

subroutine disease

use globalvars

implicit none
	
if (disSevere .eq. 1) then
	sickDays = 5
else if (disSevere .eq. 2) then
	sickDays = 15
else if (disSevere .eq. 3) then
	sickDays = 25
else if (disSevere .eq. 4) then
	sickDays = 35
else
	sickDays = 45
end if

end subroutine





























