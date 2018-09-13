subroutine fishdist(arrout)

! Subroutine for populating fish layer. Currently an even layer is created using coral 'biomass' 
! as an initializer with a multiplier.

use globalvars
use functions

implicit none
	real,dimension(grid,grid)				:: arrout				! Output array
	real									:: coraltot, fishtot	! Summed values of the arrays

! Initializing 
coraltot = sum(coral)/5.0
fishtot = coralfishmult*coraltot*percentcor(grid)

! Distribution across grid
arrout = fishtot/real(grid**2)

write(*,*) "Populating the initial fish layer."

end subroutine

subroutine hppop(arrin)

! Generates a population of coral and algae. Algae is represented by 0 in a gridpoint. 

use globalvars

implicit none
	real,dimension(grid,grid)			    :: arrin			! Input arrays
	integer									:: i, j				! Looping integers
	
write(*,*) "Populating the initial coral layer."

! Generates a seed for use in populating layers
call system_clock(count=clock)
seed = clock + 8*(/(i-1,i=1,randall)/)
call random_seed(put=seed)
call random_number(arrin)

end subroutine

subroutine tightcluster(arrin)
	
! Generates additional coral as circular clusters with a user-input grid radius at random points on the grid.
! Will likely be used as a base to generate more linear distributions.
	
use globalvars

	real,dimension(grid,grid)				:: arrin					! Input array
	integer									:: i,j,x,y, k				! Looping integers
	real, allocatable						:: coordinate(:,:)			! Holds x,y coordinates of center of cluster
	real									:: rad						! Spreads the increase across the cluster.
																		!  interacts with counter to linearly decrease the 
																		!  increase in coral with distance from center

! Allocations
allocate(coordinate(2,clusnum))

! Initializations
counter = 0

!call random_seed(size=randall)
call system_clock(count_rate=clock)
seed = clock + 34*(/(i-1,i=1,randall)/)	
call random_seed(put=seed)
call random_number(coordinate)

! Fitting to grid size
coordinate = grid*coordinate

do k = 1, clusnum, 1

x = floor(coordinate(1,k)) + 1
y = floor(coordinate(2,k)) + 1	
	
write(*,*) "Cluster at:", x, y

	do j = -distance, distance, 1
	
		do i = -distance, distance, 1
		
			rad = sqrt(real(i**2) + real(j**2))
			
			if (rad .eq. 0) then
				rad = 0.9
			end if
			
			if (((y+j) .le. grid) .and. ((y+j) .gt. 0) .and. ((x+i) .le. grid) .and. ((x+i) .gt. 0)) then
				arrin(x+i,y+j) = arrin(x+i,y+j) + tightclustermult*(1/rad)
			end if

			if (arrin(x+i,y+j) .gt. 1.0) then
				arrin(x+i,y+j) = 1.0
			end if			
				
		end do

	end do

end do
	
! Setting coral cover to percent input

where (arrin .lt. (1.0-percentcover)) arrin = 0.0
	
end subroutine
		
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		
subroutine newcoral

! Subroutine generates new coral when the average coral of the grid is above a user-input threshold. Does not trigger each time,
! there is a check do determine is a new coral is made.

use globalvars
use functions

implicit none
	real					:: coraltot, area			! Used to calculate the average coral
	real					:: avgcoral					! The average coral
	real					:: temp						! Holds a random number which checks to see if a new coral is generated
	real					:: coord					! Holds the coordinates of the new coral
	integer					:: x, y, i, j, l, c			! Integers for coordinates, looping, and algae locations
	integer,allocatable		:: algaeloc(:,:)			! Holds the locations where there is algae and not coral
	real					:: bactfact
	
	
! Finds average coral
coraltot  = sum(coral)
area 	  = float(grid)**2
avgcoral  = coraltot/area

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
	
	call system_clock(count=clock)
	seed = clock + 3*(/(i-1,i=1,randall)/)
	call random_seed(put=seed)
	call random_number(temp)

! Do loops to find exact coordinates of algae
do i = 1, grid, 1
	
	do j = 1, grid, 1
		
		if (coral(i,j) .eq. 0.0) then
			
			! Saves the locations 
			c = c + 1
			algaeloc(1,c) = i
			algaeloc(2,c) = j
		
		end if
		
	end do
	
end do

		! Logic statements for coordinates
		seed = seed*2
		call random_seed(put=seed)
		call random_number(coord)
				
		x = algaeloc(1,floor(l*coord))
		y = algaeloc(2,floor(l*coord))
		
		bactfact = (kbact(2*x,2*y)+kbact(2*x-1,2*y)+kbact(2*x-1,2*y-1)+kbact(2*x,2*y-1))/(maxval(kbact)*3.8)
		
		if (temp .ge. bactfact) then
		
			numnew = numnew + 1
		
			coral(x,y) = 0.9
		
			deallocate(algaeloc)
			
		end if

end if

end subroutine	
		
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine bacteriapop

! Subroutine populates initial bacteria layer with total population and number of species

use globalvars
use functions

implicit none
	real 				:: avgspec, ran						! Average num. of species, random number, minimum number of species
	real				:: average, area					! Average of species, area of grid
	integer				:: i, j, k							! Looping integers
	integer				:: allck
		
write(*,*) "Populating initial Bacteria layer."


! Initializations
area = (2.0*float(grid))**2

bacteria%totalpop = int(0.7*kbact)
bacteria%numspecies = 1

! Random number generation for species distribution 
call random_seed(size=randall)
call system_clock(count=clock)
seed = clock + 4*(/(i-1,i=1,randall)/)
call random_seed(put=seed)

! Fills species grid
do i = 1, 2*grid, 1
	
	do j = 1, 2*grid, 1
	
			call random_number(ran)
			
			ran = floor(maxspec*ran + minispec*(1.0-ran))
			
			bacteria(i,j)%numspecies = (int(ran))
			
	end do

end do

! Write statements
average = sum(bacteria%numspecies)/area
write(*,*) "Average number of species:" ,average

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine phagepop

use globalvars

implicit none
	integer			:: i, j
!	real			:: bactmod
	
bactmod = phlyratio
	
do i = 1, 2*grid, 1
	
	do j = 1, 2*grid, 1
	
		phage(i,j)%totalpop = int(5.0*real(bacteria(i,j)%totalpop)*bactmod)
		phage(i,j)%numspecies = int((real(bacteria(i,j)%numspecies)*bactmod))
		
	end do
	
end do

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine lysogenpop

use globalvars

implicit none
	integer			:: i, j
	
do i = 1, 2*grid, 1

	do j = 1, 2*grid, 1
	
		lys(i,j)%totalpop = int((1.0 - phlyratio)*float(bacteria(i,j)%totalpop))
		lys(i,j)%numspecies = (bacteria(i,j)%numspecies - phage(i,j)%numspecies)
	
	end do
	
end do

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine microbepopptw

use globalvars

implicit none
	real	:: area, average
	real	:: ran
	integer :: i, j
	
	
area = real((2*grid)**2)

bacteria%totalpop = int(0.7*kbact)

phage%totalpop = 3*bacteria%totalpop

lys%totalpop = int(0.5*real(bacteria%totalpop))

! Fills species grid
do i = 1, 2*grid, 1
	
	do j = 1, 2*grid, 1
	
			call random_number(ran)
			
			ran = floor(maxspec*ran + minispec*(1.0-ran))
			
			bacteria(i,j)%numspecies = (int(ran))
			
	end do

end do

! Write statements
average = sum(bacteria%numspecies)/area
write(*,*) "Average number of species:" ,average

phage%numspecies = int(0.6*real(bacteria%numspecies))

lys%numspecies = int(0.4*real(bacteria%numspecies))

end subroutine







	
	
	
	
	
	
	
	
	
	
	



