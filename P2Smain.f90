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
	integer					:: i, j, t, l					! Looping integers; n is random seed holder
	integer					:: allck
	real					:: fishdelt
	integer					:: seaslen, fertile, buds

call inputs

! Allocation statements
allocate(coral(grid,grid), stat=allck)
	if (allck .ne. 0) stop "Coral Allocation Failed"
	coral = 0.0
allocate(holding(grid,grid), stat=allck)
	if (allck .ne. 0) stop "Holding Allocation Failed"
	holding = 0.0
allocate(fish(grid,grid), stat=allck)
	if (allck .ne. 0) stop "Fish Allocation Failed"
	fish = 0.0
allocate(check(grid,grid), stat=allck)
	if (allck .ne. 0) stop "Check Allocation Failed"
allocate(bacteria(2*grid,2*grid), stat=allck)
	if (allck .ne. 0) stop "Bacteria Allocation Failed"
allocate(bacthold(2*grid,2*grid), stat=allck)
	if (allck .ne. 0) stop "Bacteria Hold Allocation Failed"
allocate(kbact(2*grid,2*grid), stat=allck)
	if (allck .ne. 0) stop "Kbact Allocation Failed"
	kbact = 0.0
allocate(phage(2*grid,2*grid), stat=allck)
	if (allck .ne. 0) stop "Phage Allocation Failed"
allocate(lys(2*grid,2*grid), stat=allck)
	if (allck .ne. 0) stop "Lys Allocation Failed"
allocate(seed(randall), stat=allck)
	if (allck .ne. 0) stop "Seed Allocation Failed"

	

! Initializing grids and variables
coral 				= 0.0
holding 			= 0.0
bacteria%totalpop 	= 0
bacteria%numspecies = 0
! Function Variables
rate 				= 0.5
fgrowfact			= 1.0
! Sub Variables
!growpercent 		= 0.01
fisheatmult			= 1.0
algaemod			= 1.3
coralmod			= 0.8
barriermod 			= 1.0
specmult			= 0.1
abundperc			= 0.001
caught				= 0.95
dayavg				= 5.0
numday				= 0.0
phagedie			= 0.5
alpha				= 0.0336
fertile = 0

call cpu_time(clock)
seed = clock + 3*(/(i-1,i=1,randall)/)
call random_seed(put=seed)

! Populates the coral/algae layer
call corpop

holding = coral

write(*,*) "0.0 represents pure algae; greater than zero represents coral, higher number represents more coral"
write(*,*) "Files are written as (x,y,z) where z is the population/biomass"

call fishdist(fish)

! Populating initital bacteria layer
call kgrid
call microbepopptw

bacthold = bacteria

t = 0

call datacollect(t)

write(*,*) "Coral percentage:", percentcor(grid)

! Outer loops iterates time, i and j iterate x and y respectively
do t = 1, numtime, 1
	write(*,*) "Beginning timestep", t
		
	if (mod(t,182) .eq. 0) then
		write(*,*) "Coral spawning begins"
		fertile = 14
	end if
	
	write(*,*) "Coral percentage:", percentcor(grid)
	buds = nint(percentcor(grid)*10.0)
		
		do i = 1, grid, 1
	
			do j = 1, grid, 1
		
				call growth(i,j,coral,coral)
				call decay(i,j,coral)				
	
			end do
	
		end do
		
		if (fertile .gt. 0) then
			do l = 1, buds, 1
				call newcoral
			end do		
		end if
		
		call shark
		fish = fish + fish*(1.0+fishdelta(sum(coral),sum(fish)))
		call kgrid
		call diffuse
		call mixing
		call bactgrowptw
		
		call datacollect(t)
		
 		holding = coral
		bacthold = bacteria
		fertile = fertile - 1

end do

write(*,*) "Total number of new coral growths:", numnew
write(*,*) "Average number of days between shark attack:", float(numtime)/numday

deallocate(coral)
deallocate(holding)
deallocate(fish)
deallocate(check)
deallocate(bacteria)
deallocate(kbact)
deallocate(seed)

 
end program