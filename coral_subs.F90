subroutine corpop
! Sets initial coral distribution

use globalvars, only: percentcover, grid, coral
use functions, only: percentcor

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
	size  = 3.0*size + 1.0
	! Make integer
	sizint = nint(size)
	! Determine where the shape goes
	coordinate = float(grid)*coordinate

	! Get number for health
	call random_number(healtharr)
	! Fit between 1 and five
	healtharr = 1.0 + healtharr*9.0

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
	where (coral .gt. 10.0) coral = 10.0

	! Update percentage
	perc = percentcor(grid)

end do

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine corexp(new)
! Subroutine grows the coral into new spots

use globalvars, only: coral, bacteria, lys, grid, check, numnew
use functions, only: bactouch

implicit none
	integer,intent(in)	:: new
	integer							:: spawned(2,new)
	real								:: temp	! Holds a random number which checks to see if a new coral is generated
	real								:: coord ! Holds the coordinates of the new coral
	integer							:: x, y, i, j, l, c	! Integers for coordinates, looping, and algae locations
	integer,allocatable	:: algaeloc(:,:)	! Holds the locations where there is algae and not coral
	real								:: bactfact, neighbors ! Bacterial influence factor, holds number of coral neighbors
	real								:: bact_imp, totbact
	integer							:: tstep_new

! Initialize the 'counting' integer to update algaeloc locations
c = 0
tstep_new = 0

! Determines how many locations are not coral
check = (coral .eq. 0.0)

! Sends the count to an integer and allocates algaeloc
l = count(check)
allocate(algaeloc(2,l))
algaeloc = 0

! Checks coral average against threshold, checks against probability of
! generation, if passes calls random locations in algaeloc and places coral.

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

spawned = 0

! Logic statements for coordinates
do i = 1, new, 1

102	call random_number(coord)

	x = algaeloc(1,floor(c*coord))
	y = algaeloc(2,floor(c*coord))

	do j = 1, new, 1
			if ((x .eq. spawned(1,i)).or.(y .eq. spawned(2,i))) then
				goto 102
			end if
	end do

	spawned(1,i) = x
	spawned(2,i) = y

	! Check for bounds
	if (x .lt. 1) x = 1
	if (y .lt. 1) y = 1
	if (x .gt. grid) x = grid
	if (y .gt. grid) y = grid

	call random_number(temp)
	bactfact = real((bacteria(2*x,2*y)%totalpop+bacteria(2*x-1,2*y)%totalpop &
		+bacteria(2*x-1,2*y-1)%totalpop+bacteria(2*x,2*y-1)%totalpop))

	totbact = bactfact + real((lys(2*x,2*y)%totalpop+lys(2*x-1,2*y)%totalpop &
		+lys(2*x-1,2*y-1)%totalpop+lys(2*x,2*y-1)%totalpop))

	bact_imp = bactouch(totbact)
	! Set new coral if bacteria is not enough to prevent
	if (temp .ge. bact_imp) then
		numnew = numnew + 1
		tstep_new = tstep_new + 1
		coral(x,y) = 2.5
	end if

end do

write(*,*) "Number of new coral growths:", tstep_new

deallocate(algaeloc)

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine growth(arrin,arrout,deconst)
use functions
! Grows the input grid location based on value and neighbors.

use globalvars, only: grid, bacteria, lys, growavg
use functions, only: bactouch, percentcor

implicit none
	real,dimension(grid,grid), intent(in) 	:: arrin ! Input array
	real,intent(in)													:: deconst ! growth modifier for disease
	real,dimension(grid,grid), intent(out)	:: arrout	! Output array
	real																		:: bactcoral, lyscoral, tot_bac
	real																		:: grow, bacteff ! Bacterial influences
	integer																	:: x, y ! Looping integers
	real																		:: cur_perc, growpercent, ran

! Initialize
growavg = 0.0

! Loops
do x = 1, grid, 1

yloop:	do y = 1, grid, 1

		! Reset
		bactcoral = 0.0
		lyscoral = 0.0

		! bact. pop on coral directly
		bactcoral = real((bacteria(2*x,2*y)%totalpop+bacteria(2*x-1,2*y)%totalpop &
								+bacteria(2*x-1,2*y-1)%totalpop+bacteria(2*x,2*y-1)%totalpop))
		lyscoral = real((lys(2*x,2*y)%totalpop+lys(2*x-1,2*y)%totalpop &
								+lys(2*x-1,2*y-1)%totalpop+lys(2*x,2*y-1)%totalpop))

		! normalize to area of region covered
		tot_bac = bactcoral + lyscoral

		! Edge cases
		if (bactcoral .lt. 0.0) then
			bactcoral = 0.0
		end if

		! Check amount of influence from bacteria
		bacteff = bactouch(tot_bac)

		call random_number(growpercent)
		call random_number(ran)

		! Determine growth
		if (ran .lt. 0.5) then
			growpercent = 0.1 + 0.05*growpercent
		else
			growpercent = 0.1 - 0.05*growpercent
		end if

		! growth avg = 0.1, at full capacity bacteria should win
		grow = 1.0 + (deconst*growpercent - 0.15*bacteff)

		! Sum to average
		growavg = growavg + grow

		! Grow location
		arrout(x,y) = arrin(x,y)*grow

	end do yloop

end do

! Normalize the average growth
cur_perc = percentcor(grid)
growavg = growavg/(float(grid*grid))

! Set to maximum
where (arrout .gt. 5.0) arrout = 5.0
where (arrout .lt. 0.05) arrout = 0.0

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
