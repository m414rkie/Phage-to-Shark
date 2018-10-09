subroutine corpop

use globalvars
use functions

implicit none
	real						:: perc
	real,allocatable			:: square(:,:), circle(:,:), oval(:,:)
	integer						:: i, j
	real,dimension(grid,grid)	:: arrin					! Input array
	real, allocatable			:: coordinate(:)			! Holds x,y coordinates of center of cluster
	real						:: rad, dec					! Spreads the increase across the cluster.
	real						:: choice, size

call random_seed(size=randall)
call system_clock(count_rate=clock)
seed = clock + 34*(/(i-1,i=1,randall)/)	
call random_seed(put=seed)


allocate(coordinate(2))

perc = percentcor(grid)

while (perc .lt. percentcover)

	call random_number(coordinate)
	call random_number(choice)
	call random_number(size)
	
	size  = 8.0*size + 1.0	
	coordinate = grid*coordinate
	
	if (choice .le. 0.33) then
		
		allocate(square(size,size))
		square = 1.0
		
		do i = 1, size, 1
			do j = 1, size, 1
				
				if ((i .eq. (size-1)) .or. (j .eq. (size-1)) .or. (i .eq. 1) .or. (j .eq. 1)) then
					square(i,j) = square(i,j)*0.8
				end if
				
			end do
		end do

		x = floor(coordinate(1,k)) + 1
		y = floor(coordinate(2,k)) + 1	
		
		do j = y, y+size, 1	
	
			do i = x, x+size, 1
		
			if ((j .ge. 1) .and. (j .le. grid) .and. (i .ge. 1) .and. (i .le. grid)) then
				arrin(i,j) = square((size-i),(size-j))
					
			end do
	
		end do
	
end do

	
end subroutine