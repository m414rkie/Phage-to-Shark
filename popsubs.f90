subroutine newcoral

! Subroutine generates new coral when the average coral of the grid is above a user-input threshold. Does not trigger each time,
! there is a check do determine is a new coral is made.

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
if (avgcoral .ge. threshold) then

	call random_number(temp)

! Do loops to find exact coordinates of algae
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
		
		bactfact = corBacNew*real((bacteria(2*x,2*y)%totalpop+bacteria(2*x-1,2*y)%totalpop &
			+bacteria(2*x-1,2*y-1)%totalpop+bacteria(2*x,2*y-1)%totalpop))/real(kbact(2*x,2*y) &
			+kbact(2*x-1,2*y)+kbact(2*x-1,2*y-1)+kbact(2*x,2*y-1))
			
		if (temp .ge. bactfact) then
		
			numnew = numnew + 1
		
			coral(x,y) = 3.5
		
			deallocate(algaeloc)
			
		end if

end if

end subroutine	
		
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine microbepopptw

use globalvars

implicit none
	real	:: area, average
	real	:: ran
	integer :: i, j
	
	
area = real((2*grid)**2)

bacteria%totalpop = int(0.6*kbact)

phage%totalpop = 3*bacteria%totalpop

lys%totalpop = int(0.8*real(bacteria%totalpop))

lys(i,j)%numspecies = int(float(lys(i,j)%totalpop)**beta)
phage(i,j)%numspecies = int(float(phage(i,j)%totalpop)**beta)
bacteria(i,j)%numspecies = int(float(bacteria(i,j)%totalpop)**alpha)

! Write statements
average = sum(bacteria%numspecies)/area
write(*,*) "Average number of species:" ,average

end subroutine







	
	
	
	
	
	
	
	
	
	
	



