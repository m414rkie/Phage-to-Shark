subroutine corpop

use globalvars
use functions

implicit none
	real						:: perc
	real,allocatable			:: square(:,:), circle(:,:), oval(:,:)
	integer						:: i, j, sizint, x, y
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


do while (perc .lt. percentcover)
	write(*,*) perc
	call random_number(coordinate)
	call random_number(choice)
	call random_number(size)
	
	size  = 8.0*size + 1.0	
	sizint = floor(size)
	coordinate = grid*coordinate
	write(*,*) coordinate

	if (choice .le. 0.5) then

		allocate(square(sizint,sizint))
		square = 1.0
		
		do i = 1, sizint, 1
			do j = 1, sizint, 1
				
				if ((i .eq. (sizint)) .or. (j .eq. (sizint)) .or. (i .eq. 1) .or. (j .eq. 1)) then
					square(i,j) = 0.8
				end if
				
			end do
		end do

		x = floor(coordinate(1)) + 1
		y = floor(coordinate(2)) + 1	
		
		do j = y, y+sizint, 1	
	
			do i = x, x+sizint, 1
		
			if ((j .ge. 1) .and. (j .le. grid) .and. (i .ge. 1) .and. (i .le. grid)) then
				coral(i,j) = coral(i,j) + square((i-x+1),(j-y+1))
			end if
					
			end do
	
		end do
		
		deallocate(square)
	
	else if ((choice .gt. 0.5)) then! .and. (choice .le. 0.66) then
		
		allocate(circle(sizint,sizint))
		circle = 1.0
		
		x = floor(coordinate(1)) + 1
		y = floor(coordinate(2)) + 1	
		
		do j = y, y+sizint, 1	
	
			do i = x, x+sizint, 1
		
			rad = sqrt(float(i-x)**2 + float(j-y)**2)
			if (rad .lt. 1.0) then
				rad = 1.0
			end if
			
			if ((j .ge. 1) .and. (j .le. grid) .and. (i .ge. 1) .and. (i .le. grid)) then
				coral(i,j) = coral(i,j) + circle((i-x+1),(j-y+1))*(0.98**rad)
			end if
					
			end do
	
		end do
	
		deallocate(circle)
	
	end if
	
	perc = percentcor(grid)
	
end do

coral = coral*5.0

where (coral .lt. 0.0) coral = abs(coral)
where (coral .gt. 5.0) coral = 5.0


end subroutine