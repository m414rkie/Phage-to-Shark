subroutine corpop

use globalvars
use functions

implicit none
	real						:: perc
	integer						:: i, j, sizint, x, y
	real						:: coordinate(2)			! Holds x,y coordinates of center of cluster
	real						:: choice, size
	real						:: healtharr

perc = percentcor(grid)

do while (perc .lt. percentcover)

	call random_number(coordinate)
	call random_number(choice)
	call random_number(size)

	size  = 8.0*size + 1.0	
	sizint = floor(size)
	coordinate = float(grid)*coordinate

	call random_number(healtharr)
	healtharr = 1.0 + healtharr*4.0	
	
	!if (choice .le. 0.5) then

		x = floor(coordinate(1)) + 1
		y = floor(coordinate(2)) + 1	
		
		do j = y, y+sizint, 1	
	
			do i = x, x+sizint, 1
		
			if ((j .ge. 1) .and. (j .le. grid) .and. (i .ge. 1) .and. (i .le. grid)) then
				coral(i,j) = coral(i,j) + healtharr
			end if
					
			end do
	
		end do
	
	perc = percentcor(grid)

	where (coral .lt. 0.05) coral = 0.0
	where (coral .gt. 5.0) coral = 5.0

end do



end subroutine