subroutine corpop
! Sets initial coral distribution

use globalvars
use functions

implicit none
	real						:: perc ! Current percentage of coral
	integer					:: i, j, sizint, x, y ! Looping integers, size of shape, coords
	real						:: coordinate(2)	! Holds x,y coordinates of center of cluster
	real						:: choice, size ! shape choice, size of shape
	real						:: healtharr ! health of coral being placed. one to five

! Initialize
perc = percentcor(grid)

! Working loop
do while (perc .lt. percentcover)

	! Get random numbers for placement
	call random_number(coordinate)
	call random_number(choice)
	call random_number(size)

	! Size of shape
	size  = 8.0*size + 1.0
	! Make integer
	sizint = floor(size)
	! Determine where the shape goes
	coordinate = float(grid)*coordinate

	! Get number for health
	call random_number(healtharr)
	! Fit between 1 and five
	healtharr = 1.0 + healtharr*4.0

	! Not implemented, only one shape right now
	!if (choice .le. 0.5) then

		! Set coordinates to integers that fit in the grid
		x = floor(coordinate(1)) + 1
		y = floor(coordinate(2)) + 1

		! Loop through shape and place
		do j = y, y+sizint, 1

			do i = x, x+sizint, 1

			! Ensure that we aren't going out of bounds.
			if ((j .ge. 1) .and. (j .le. grid) .and. (i .ge. 1) .and. (i .le. grid)) then
				coral(i,j) = coral(i,j) + healtharr
			end if

			end do

		end do

	! Trim outliers
	where (coral .lt. 0.05) coral = 0.0
	where (coral .gt. 5.0) coral = 5.0

	! Update percentage
	perc = percentcor(grid)

end do

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine corexp

! Subroutine grows the coral into new spots

use globalvars
use functions

implicit none
	real								:: temp	! Holds a random number which checks to see if a new coral is generated
	real								:: coord ! Holds the coordinates of the new coral
	integer							:: x, y, i, j, l, c	! Integers for coordinates, looping, and algae locations
	integer,allocatable	:: algaeloc(:,:)	! Holds the locations where there is algae and not coral
	real*8							:: bactfact, neighbors ! Bacterial influence factor, holds number of coral neighbors

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
		! Must be adjacent to coral
		if ((coral(i,j) .eq. 0.0) .and. (neighbors .ne. 0.0)) then

			! Saves the locations of algae into algaeloc
			c = c + 1
			algaeloc(1,c) = i
			algaeloc(2,c) = j

		end if

	end do

end do

! Logic statements for coordinates
call random_number(coord)

x = algaeloc(1,floor(c*coord))
y = algaeloc(2,floor(c*coord))

! Check for bounds
if (x .lt. 1) x = 1
if (y .lt. 1) y = 1
if (x .gt. grid) x = grid
if (y .gt. grid) y = grid

call random_number(temp)
! Bactfact = sum of all bacteria on location new coral may be placed as a ratio of  bacteria pop to carrycing capacity
bactfact = corBacNew*real((bacteria(2*x,2*y)%totalpop+bacteria(2*x-1,2*y)%totalpop &
	+bacteria(2*x-1,2*y-1)%totalpop+bacteria(2*x,2*y-1)%totalpop),8)/real(kbact(2*x,2*y) &
	+kbact(2*x-1,2*y)+kbact(2*x-1,2*y-1)+kbact(2*x,2*y-1),8)

! Set new coral if bacteria is not enough to prevent
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
	real								:: avgcoral	! The average coral in the reef
	real								:: temp	! Holds a random number which checks to see if a new coral is generated
	real								:: coord	! Holds the coordinates of the new coral
	integer							:: x, y, i, j, l, c	! Integers for coordinates, looping, and algae locations
	integer,allocatable	:: algaeloc(:,:)	! Holds the locations where there is algae and not coral
	real*8							:: bactfact, neighbors ! bacterial influence factor, number of neighbors


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

		! Boundary conditions
		if (x .lt. 1) x = 1
		if (y .lt. 1) y = 1
		if (x .gt. grid) x = grid
		if (y .gt. grid) y = grid

		! Bacteria factor, bacteria pop / carrying capacity
		bactfact = corBacNew*real((bacteria(2*x,2*y)%totalpop+bacteria(2*x-1,2*y)%totalpop &
			+bacteria(2*x-1,2*y-1)%totalpop+bacteria(2*x,2*y-1)%totalpop),8)/real(kbact(2*x,2*y) &
			+kbact(2*x-1,2*y)+kbact(2*x-1,2*y-1)+kbact(2*x,2*y-1),8)

		! New coral unless bacteria prevent
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
	real,dimension(grid,grid), intent(in) 		:: arrin ! Input array
	real,dimension(grid,grid), intent(out)		:: arrout	! Output array
	real,intent(in)														:: growpercin ! Natural growth rate
	real*8																		:: bactcoral, grow, bacteff ! Bacterial influences
	integer																		:: x, y ! Looping integers

! Initialize
growavg = 0.0

! Loops
do x = 1, grid, 1

yloop:	do y = 1, grid, 1

		! Cycle if location is too unhealthy
		if (arrin(x,y) .lt. 0.05) then
			cycle yloop
		end if

		! Cycle if location is too healthy
		if (arrin(x,y) .ge. 5.0) then
			cycle yloop
		end if

		! Reset
		bactcoral = 0.0

		! bact. pop on coral directly
		bactcoral = real((bacteria(2*x,2*y)%totalpop+bacteria(2*x-1,2*y)%totalpop &
								+bacteria(2*x-1,2*y-1)%totalpop+bacteria(2*x,2*y-1)%totalpop),8)

		! Adjacent to the coral square on the lower bounds
		! x
		if (y .gt. 1) then
			bactcoral = bactcoral + real(bacteria(2*x,2*y-1)%totalpop+bacteria(2*x,2*y-1)%totalpop,8)
		end if

		! y
		if (x .gt. 1) then
			bactcoral = bactcoral + real(bacteria(2*x,2*y)%totalpop+bacteria(2*x-1,2*y)%totalpop,8)
		end if

		! Adjacent to the coral square on the upper bounds
		! y
		if (x .lt. grid) then
			bactcoral = bactcoral + real(bacteria(2*x+1,2*y)%totalpop+bacteria(2*x+1,2*y-1)%totalpop,8)
		end if

		! x
		if (y .lt. grid) then
			bactcoral = bactcoral + real(bacteria(2*x,2*y+1)%totalpop+bacteria(2*x-1,2*y+1)%totalpop,8)
		end if

		! normalize to area of region covered
		bactcoral = bactcoral/(12.0)

		! Edge cases
		if (bactcoral .lt. 0.0) then
			bactcoral = 0.0
		end if

		! Check amount of influence
		bacteff = bactouch(bactcoral)
		if (bacteff .gt. 1.0) then
			bacteff = 1.0
		end if

		call random_number(growpercent)

		! Set to favor lower growth amounts, centers on 0.05 percent growth
    growpercent = exp(-growpercent**0.25) - exp(-1.0)

		! Growth with bacterial influence
		grow = 1.0 + growpercent*(1.0 - bacteff)
		if (grow .lt. 1.0) then
			grow = 1.0
		end if

		! Sum to average
		growavg = growavg + sngl(growpercent*(1.0-bacteff))

		! Grow location
		arrout(x,y) = arrin(x,y)*sngl(grow)

	end do yloop

end do

! Normalize the average growth
growavg = growavg/(grid*grid)

! Set to maximum
where (arrout .gt. 5.0) arrout = 5.0

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine decay(arrin,arrout)

! Represents the algae killing the coral.

use globalvars

implicit none
	real,dimension(grid,grid)	:: arrin, arrout ! Input array
	integer					 					:: algcount	! Amount of algae near input coordinates
	integer										:: x, y ! Location being examined
	real											:: decay_loc, decay_loc_ini ! decay factor; initial decay

! Initialize
decavg = 0.0

! Initialize the decay constant to initial value
decay_loc_ini = decayconst

! Determine the fish influence, in fish_subs.F90
call fishinteraction(decay_loc_ini)

do x = 1, grid, 1

yloop:	do y = 1, grid, 1

		! Cycle if coral is below threshold
		if (arrin(x,y) .lt. 0.05) then
			cycle yloop
		end if

		! Initialize the algae location
		algcount = 0

		! Set local decay factor
		decay_loc = decay_loc_ini

		! Checks for algae around the input gridpoint and out-of-bounds
		! x lower bound
		if ((x .gt. 1) .and. (holding(x-1,y) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		! x upper bound
		if ((x .lt. grid) .and. (holding(x+1,y) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		! y upper bound
		if ((y .lt. grid) .and. (holding(x,y+1) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		! y lower bound
		if ((y .gt. 1) .and. (holding(x,y-1) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		! top right corner
		if ((x .lt. grid) .and. (y .lt. grid) .and. (holding(x+1,y+1) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		! bottom left corner
		if ((x .gt. 1) .and. (y .gt. 1) .and. (holding(x-1,y-1) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		! top left corner
		if ((x .lt. grid) .and. (y .gt. 1) .and.(holding(x+1,y-1) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		! top right corner
		if ((x .gt. 1) .and. (y .lt. grid) .and. (holding(x-1,y+1) .eq. 0.0)) then
			algcount = algcount + 1
		end if

		! cycle if this location has no algal neighbors
		if (algcount .lt. 1) then
			cycle yloop
		end if

		! Multiply decay factor by number of algal neighbors
		decay_loc = decay_loc*float(algcount)

		! Coral being eaten.
		if (decay_loc .gt. 1.0) then
			decay_loc = 1.0 ! Maximal value
		end if
		! Coral less the algae eating it
		if (decay_loc .gt. 0.0) then
			arrout(x,y) = arrin(x,y)*(1.0 - decay_loc)
		end if

		! Sum for averaging
		decavg = decavg + decay_loc

		! Resets extreme values
		if (arrout(x,y) .le. 0.05) then
			arrout(x,y) = 0.0
		end if
		if (arrout(x,y) .gt. 5.0) then
			arrout(x,y) = 5.0
		end if

	end do yloop

end do

! Find average of decay
decavg = decavg/(grid*grid)

end subroutine
