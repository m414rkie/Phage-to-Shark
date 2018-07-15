PROGRAM concept
!
! This is a proof of concept for use in a program that will model
! interaction of coral reefs
!
! History
! 1.00a - Initial program construction
! 1.00b - Coral and algae interaction
! 1.00c - Fish layer added
! 1.00d - Coral/Algae layer population routines
! 1.00d - Bacteria layer added
!
! Version   Programmer         Date       Description
! -------   ----------         --------   --------------------
! 1.00e     Jon Parsons        6-23-18	  Proof of concept
!
! IN args/commons              Units      Description
! -----------------            -----      ----------------------
! input variables              units      variable purpose
!
! OUT args/commons             Units      Description
! ----------------             -----      ----------------------------
! output variables             units      variable purpose
! 
!
! Special requirements:
! * Module file -  modules_concept.f90
! * Subroutines -  subs_concept.f90
!				   concept_popsubs.f90
!
! ------------------ Code -----------------------------

use globalvars
use functions

implicit none
	integer					:: i, j, t						! Looping integers; n is random seed holder
	integer					:: allck

! User input 
write(*,*) "Enter the dimension of the grid (square):"
read(*,*) grid
write(*,*) "Dimension: ", grid
write(*,*) "Enter the number of time steps :"
read(*,*) numtime
write(*,*) "Enter percentage of bed with coral:"
read(*,*) percentcover
write(*,*) "Number of coral clusters?"
read(*,*) clusnum
write(*,*) "Please input distance for the tightly clustered coral clusters:"
read(*,*) distance
write(*,*) "New coral threshold?"
read(*,*) threshold
write(*,*) "Average bacteria population?"
read(*,*) avgpop

! Allocation statements
allocate(coral(grid,grid), stat=allck)
	if (allck .ne. 0) stop "Coral Allocation Failed"
allocate(holding(grid,grid), stat=allck)
	if (allck .ne. 0) stop "Holding Allocation Failed"
allocate(fish(grid,grid), stat=allck)
	if (allck .ne. 0) stop "Fish Allocation Failed"
allocate(check(grid,grid), stat=allck)
	if (allck .ne. 0) stop "Check Allocation Failed"
allocate(bacteria(2*grid,2*grid), stat=allck)
	if (allck .ne. 0) stop "Bacteria Allocation Failed"
allocate(bacthold(2*grid,2*grid), stat=allck)
	if (allck .ne. 0) stop "Bacteria Hold Allocation Failed"
allocate(kbact(2*grid,2*grid), stat=allck)
	if (allck .ne. 0) stop "Kbact Allocation Failed"
allocate(phage(2*grid,2*grid), stat=allck)
	if (allck .ne. 0) stop "Phage Allocation Failed"
allocate(lys(2*grid,2*grid), stat=allck)
	if (allck .ne. 0) stop "Lys Allocation Failed"
allocate(seed(randall), stat=allck)
	if (allck .ne. 0) stop "Seed Allocation Failed"
allocate(coralpercent(2,numtime), stat=allck)
	if (allck .ne. 0) stop "Coralpercent Allocation Failed"

! Initializing grids
coral 				= 0.0
holding 			= 0.0
fgrowfact			= 0.25
bacteria%totalpop 	= 0
bacteria%numspecies = 0
sharkmod			= 0.0
hunger 				= 0.3
phlyratio 			= 0.7
coralfishmult 		= 1.5

! Populates the coral/algae layer
call hppop(coral)
call tightcluster(coral)

! Increases overal population of coral as each gridpoint will be between zero and one beforehand
coral = 5.0*coral

holding = coral

write(*,*) "0.0 represents pure algae; greater than zero represents coral, higher number represents more coral"
write(*,*) "Files are written as (x,y,z) where z is the population/biomass"

call fishdist(fish)

! Populating initital bacteria layer
call kgrid
call bacteriapop

! Populating initial phage layer
call phagepop
call lysogenpop

bacthold = bacteria

t = 0
call datacollect(t)

! Outer loops iterates time, i and j iterate x and y respectively
do t = 1, numtime, 1

	write(*,*) "Beginning timestep", t
	
	call shark
		
		do i = 1, grid, 1
	
			do j = 1, grid, 1
		
				!call neighborsum(i,j,holding,nearsum)
				call growth(i,j,coral,coral)
				call decay(i,j,coral)				
	
			end do
	
		end do
		
		fish = fish + fishdelta(sum(coral),sum(fish))
		call newcoral
		call kgrid
		call bactgrow
		call diffuse
		call mixing	
		
		call datacollect(t)
		
 		holding = coral
		bacthold = bacteria

end do

write(*,*) "Total number of new coral growths:", numnew

deallocate(coral)
deallocate(holding)
deallocate(fish)
deallocate(check)
deallocate(bacteria)
deallocate(kbact)
deallocate(seed)

 
end program