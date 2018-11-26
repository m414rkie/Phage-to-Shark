subroutine corpop

use globalvars
use functions

implicit none
	real						:: perc
	integer						:: i, j, sizint, x, y, hch
	real						:: coordinate(2)			! Holds x,y coordinates of center of cluster
	real						:: choice, size, vali
	real						:: healtharr(10)

call random_seed(size=randall)
call cpu_time(clock)
seed = clock + (/(i-1,i=1,randall)/)	
call random_seed(put=seed)

call random_number(healtharr)
healtharr = healtharr*4.0
where (healtharr .lt. 1.0) healtharr = 1.0

perc = percentcor(grid)
hch = 1

do while (perc .lt. percentcover)

	if(hch .gt. 10) then
		hch = 1
	end if

	call random_number(coordinate)
	call random_number(choice)
	call random_number(size)
	seed = seed + 1
	call random_seed(put=seed)
	size  = 8.0*size + 1.0	
	sizint = floor(size)
	coordinate = float(grid)*coordinate

	vali = healtharr(hch)
	!if (choice .le. 0.5) then

		x = floor(coordinate(1)) + 1
		y = floor(coordinate(2)) + 1	
		
		do j = y, y+sizint, 1	
	
			do i = x, x+sizint, 1
		
			if ((j .ge. 1) .and. (j .le. grid) .and. (i .ge. 1) .and. (i .le. grid)) then
				coral(i,j) = coral(i,j) + vali
			end if
					
			end do
	
		end do
	
	perc = percentcor(grid)
	hch = hch + 1

	where (coral .lt. 0.05) coral = 0.0
	where (coral .gt. 5.0) coral = 5.0

end do



end subroutine