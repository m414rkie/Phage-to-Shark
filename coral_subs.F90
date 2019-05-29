subroutine corpop

use globalvars
use functions

implicit none
	real						:: perc
	integer					:: i, j, sizint, x, y
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
	real					:: temp						! Holds a random number which checks to see if a new coral is generated
	real					:: coord					! Holds the coordinates of the new coral
	integer					:: x, y, i, j, l, c			! Integers for coordinates, looping, and algae locations
	integer,allocatable		:: algaeloc(:,:)			! Holds the locations where there is algae and not coral
	real					:: bactfact, neighbors

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

! Do loops to find exact coordinates of coral
do i = 1, grid, 1

	do j = 1, grid, 1

	neighbors = 0
	! Checks for coral around the input gridpoint and out-of-bounds
	if ((i .gt. 1) .and. (coral(i-1,j) .ne. 0.0)) then
		neighbors = 1.0
	end if

	if ((i .lt. grid) .and. (coral(i+1,j) .ne. 0.0)) then
		neighbors = neighbors + 1.0
	end if

	if ((j .lt. grid) .and. (coral(i,j+1) .ne. 0.0)) then
		neighbors = neighbors + 1.0
	end if

	if ((i .gt. 1) .and. (coral(i,j-1) .ne. 0.0)) then
		neighbors = neighbors + 1.0
	end if

	if ((i .lt. grid) .and. (j .lt. grid) .and. (coral(i+1,j+1) .ne. 0.0)) then
		neighbors = neighbors + 1.0
	end if

	if ((i .gt. 1) .and. (j .gt. 1) .and. (coral(i-1,j-1) .ne. 0.0)) then
		neighbors = neighbors + 1.0
	end if

	if ((i .lt. grid) .and. (j .gt. 1) .and.(coral(i+1,j-1) .ne. 0.0)) then
		neighbors = neighbors + 1.0
	end if

	if ((i .gt. 1) .and. (j .lt. grid) .and. (coral(i-1,j+1) .ne. 0.0)) then
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

if (x .lt. 1) x = 1
if (y .lt. 1) y = 1
if (x .gt. grid) x = grid
if (y .gt. grid) y = grid

call random_number(temp)
bactfact = corBacNew*real(kbact(2*x,2*y)+kbact(2*x-1,2*y)+kbact(2*x-1,2*y-1)+kbact(2*x,2*y-1))/real(maxval(kbact)*4.0)

if (temp .ge. bactfact) then
	numnew = numnew + 1

	coral(x,y) = 3.5

end if

deallocate(algaeloc)

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine newcoral

! Subroutine generates new coral when the average coral of the grid is above a user-input threshold. Does not trigger each time,
! there is a check do determine if a new coral is made.

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

		if (x .lt. 1) x = 1
		if (y .lt. 1) y = 1
		if (x .gt. grid) x = grid
		if (y .gt. grid) y = grid

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

subroutine growth(arrin,arrout,growpercin)

! Grows the input grid location based on value and neighbors.

use globalvars
use functions

implicit none
	real,dimension(grid,grid), intent(in) 		:: arrin				! Input array
	real,dimension(grid,grid), intent(out)		:: arrout				! Output array
	real,intent(in)														:: growpercin
	real																			:: bactcoral, grow, bacteff
	integer																		:: x, y

do x = 1, grid, 1

	do y = 1, grid, 1

		bactcoral = 0.0

		! On coral directly
		bactcoral = real((bacteria(2*x,2*y)%totalpop+bacteria(2*x-1,2*y)%totalpop &
								+bacteria(2*x-1,2*y-1)%totalpop+bacteria(2*x,2*y-1)%totalpop))

		! Adjacent to the coral square on the lower bounds
		bactcoral = bactcoral + real(bacteria(x,2*y)%totalpop+bacteria(x,2*y-1)%totalpop)
		bactcoral = bactcoral + real(bacteria(2*x,y)%totalpop+bacteria(2*x-1,y)%totalpop)
		bactcoral = bactcoral + real(bacteria(x+1,y+1)%totalpop)


		! Adjacent to the coral square on the upper bounds
		if (x .lt. grid) then
			bactcoral = bactcoral + real(bacteria(2*x+1,2*y)%totalpop+bacteria(2*x+1,2*y-1)%totalpop)
			bactcoral = bactcoral + real(bacteria(2*x+1,y)%totalpop)
		end if

		if (y .lt. grid) then
			bactcoral = bactcoral + real(bacteria(2*x,2*y+1)%totalpop+bacteria(2*x-1,2*y+1)%totalpop)
			bactcoral = bactcoral + real(bacteria(x,2*y+1)%totalpop)
		end if

		if ((x .lt. grid).and.(y .lt. grid)) then
			bactcoral = bactcoral + real(bacteria(2*x+1,2*y+1)%totalpop)
		end if

		if (bactcoral .lt. 0.0) then
			bactcoral = 0.0
		end if

		bacteff = bactouch(bactcoral)

		call random_number(growpercent)

		growpercent = growpercent*growpercin
		grow = 1.0 + growpercent*(1.0 + bacteff)
		if (grow .lt. 1.0) then
			grow = 1.0
		end if

		arrout(x,y) = arrin(x,y)*grow

	end do

end do

where (arrout .gt. 5.0) arrout = 5.0

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine decay(arrin)

! Represents the algae killing the coral

use globalvars

implicit none
	real,dimension(grid,grid)	:: arrin		! Input array
	integer					 					:: algcount		! Amount of algae near input coordinates
	integer										:: x, y

	! Initializations
	algcount = 0
	decayconst	= 0.001
	call fishinteraction(decayconst)

do x = 1, grid, 1

	do y = 1, grid, 1

		! Checks for algae around the input gridpoint and out-of-bounds
		if ((x .gt. 1) .and. (holding(x-1,y) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		if ((x .lt. grid) .and. (holding(x+1,y) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		if ((y .lt. grid) .and. (holding(x,y+1) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		if ((y .gt. 1) .and. (holding(x,y-1) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		if ((x .lt. grid) .and. (y .lt. grid) .and. (holding(x+1,y+1) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		if ((x .gt. 1) .and. (y .gt. 1) .and. (holding(x-1,y-1) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		if ((x .lt. grid) .and. (y .gt. 1) .and.(holding(x+1,y-1) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		if ((x .gt. 1) .and. (y .lt. grid) .and. (holding(x-1,y+1) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		if (algcount .lt. 1) then
			return
		end if

		if(algcount .eq. 0) then
			decayconst = 0.0
		else
			decayconst = decayconst*float(algcount)
		end if

		! Coral being eaten.
		if (decayconst .gt. 0.09) then
			decayconst = 0.09
		end if

		! Coral less the algae eating it
		if (decayconst .gt. 0.0) then
			arrin(x,y) = arrin(x,y) - decayconst
		end if
		! Resets negative values to zero
		if (arrin(x,y) .le. 0.05) then
			arrin(x,y) = 0.0
		end if
		if (arrin(x,y) .gt. 5.0) then
			arrin(x,y) = 5.0
		end if
	end do
end do
end subroutine
