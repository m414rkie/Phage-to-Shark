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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine corexp

! Subroutine grows the coral at a slow rate from existing coral to represent growth.

use globalvars
use functions

implicit none
	real					:: avgcoral					! The average coral
	real					:: temp						! Holds a random number which checks to see if a new coral is generated
	real					:: coord					! Holds the coordinates of the new coral
	integer					:: x, y, i, j, l, c			! Integers for coordinates, looping, and algae locations
	integer,allocatable		:: algaeloc(:,:)			! Holds the locations where there is algae and not coral
	real					:: bactfact, neighbors
	
	
! Finds average coral

avgcoral  = sum(coral)/(percentcor(grid)*(float(grid)**2))
! Initialize the 'counting' integer to update algaeloc locations
c = 0

! Determines how many locations are not coral
check = (coral .eq. 0.0)

! Sends the count to an integer and allocates algaeloc
l = count(check)
allocate(algaeloc(2,l))
algaeloc = 0
 
! Checks coral average against threshold, checks against probability of generation, if pass calls random
! locations in algaeloc and places coral.
if (avgcoral .ge. 0.5*threshold) then

	call random_number(temp)

! Do loops to find exact coordinates of coral
do i = 1, grid, 1
	
	do j = 1, grid, 1
		
	neighbors = 0
	x=i ; y=j
	! Checks for coral around the input gridpoint and out-of-bounds
	if ((x .gt. 1) .and. (coral(x-1,y) .ne. 0.0)) then
		neighbors = 1.0
	end if
	
	if ((x .lt. grid) .and. (coral(x+1,y) .ne. 0.0)) then
		neighbors = neighbors + 1.0
	end if

	if ((y .lt. grid) .and. (coral(x,y+1) .ne. 0.0)) then
		neighbors = neighbors + 1.0
	end if
	
	if ((y .gt. 1) .and. (coral(x,y-1) .ne. 0.0)) then
		neighbors = neighbors + 1.0
	end if

	if ((x .lt. grid) .and. (y .lt. grid) .and. (coral(x+1,y+1) .ne. 0.0)) then
		neighbors = neighbors + 1.0
	end if

	if ((x .gt. 1) .and. (y .gt. 1) .and. (coral(x-1,y-1) .ne. 0.0)) then
		neighbors = neighbors + 1.0
	end if
	
	if ((x .lt. grid) .and. (y .gt. 1) .and.(coral(x+1,y-1) .ne. 0.0)) then
		neighbors = neighbors + 1.0
	end if
	
	if ((x .gt. 1) .and. (y .lt. grid) .and. (coral(x-1,y+1) .ne. 0.0)) then
		neighbors = neighbors + 1.0
	end if
		
		if ((coral(i,j) .eq. 0.0) .and. (neighbors .ne. 0.0)) then
			
			! Saves the locations 
			c = c + 1
			algaeloc(1,c) = i
			algaeloc(2,c) = j
		
		end if
		
	end do
	
end do

		! Logic statements for coordinates
		call random_number(coord)
				
		x = algaeloc(1,floor(l*coord))
		y = algaeloc(2,floor(l*coord))
		
		bactfact = corBacNew*real(kbact(2*x,2*y)+kbact(2*x-1,2*y)+kbact(2*x-1,2*y-1)+kbact(2*x,2*y-1))/real(maxval(kbact)*4.0)
		
		if (temp .ge. bactfact) then
		
			numnew = numnew + 1
		
			coral(x,y) = 3.5
		
			deallocate(algaeloc)
			
		end if

end if

end subroutine	