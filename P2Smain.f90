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
	integer					:: i, t, l					! Looping integers
	integer					:: allck
	integer					:: fertile, buds, mstime(8)
	real						:: growpercmod

call inputs

! Allocation statements
allocate(coral(grid,grid), stat=allck)
	if (allck .ne. 0) stop "Coral Allocation Failed"
	coral = 0.0
allocate(holding(grid,grid), stat=allck)
	if (allck .ne. 0) stop "Holding Allocation Failed"
	holding = 0.0
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
allocate(seed(33), stat=allck)
	if (allck .ne. 0) stop "Seed Allocation Failed"

! Initializing grids and variables

fish 					= 0.0
coral 				= 0.0
holding 			= 0.0
bacteria%totalpop 	= 0
bacteria%numspecies = 0
! Sub Variables
alpha			= 0.0336
fertile 	= 0
numday 		= 0
numnew 		= 0

call date_and_time(values=mstime)
!call cpu_time(clock)
seed = (mstime(8))
call random_seed(put=seed)

! Populates the coral/algae layer
call corpop

holding = coral

write(*,*) "0.0 represents pure algae; greater than zero represents coral, higher number represents more coral"
write(*,*) "Files are written as (x,y,z) where z is the population/biomass"

fish = 990.0*percentcor(grid)

! Populating initital bacteria layer
call kgrid
call microbepopptw

bacthold = bacteria

fertile = 0
sickDays = 0
t = 0

write(*,*) "Beginning Equilibration"

do t = 1, 90, 1

if (t .eq. 18) then
	write(*,*) "Equilibration 20% Complete."
else if (t .eq. 36) then
	write(*,*) "Equilibration 40% Complete."
else if (t .eq. 54) then
	write(*,*) "Equilibration 60% Complete."
else if (t .eq. 72) then
	write(*,*) "Equilibration 80% Complete."
end if

		buds = nint(percentcor(grid)*10.0)

		call diffuse

		growpercmod = 0.1

				call growth(holding,coral,growpercmod)
				call decay(coral)

		call shark(0)
		fish = fish + fishdelta(fish)

		do i = 1, 2*buds, 1
			call corexp
		end do
		call kgrid
		call bactgrowptw

 		holding = coral
		bacthold = bacteria

end do

write(*,*) "Equilibration Complete"

t = 0

call datacollect(t)

call domainout(t)

write(*,*) "Coral percentage:", percentcor(grid)

! Outer loops iterates time, i and j iterate x and y respectively
do t = 1, numtime, 1


	write(*,*) "Beginning timestep", t

	if ((t .gt. 181).and.(mod(t,182) .eq. 0)) then
		write(*,*) "Coral spawning begins"
		fertile = 14
	end if

	if ((t .eq. 200).and.(disFlag .eq. "H")) then
		call hurricane
		write(*,*) "Hurricane!"
	end if

	if ((t .eq. 200).and.(disFlag .eq. "D")) then
		call disease
		write(*,*) "Disease!"
	end if

	write(*,*) "Coral percentage:", percentcor(grid)
	write(*,*) "Fish population:", fish
	buds = nint(percentcor(grid)*10.0)

	if (sickDays .ge. 1) then
		growpercmod = 0.00001
	else
		growpercmod = 0.1
	end if

		call diffuse

		call growth(holding,coral,growpercmod)
		call decay(coral)

		if (fertile .gt. 0) then
			do l = 1, buds, 1
				call newcoral
			end do
		end if

		call shark(1)
		fish = fish + fishdelta(fish)
		if (fish .lt. 0) then
				fish = 0.0
		end if

		if (sickDays .lt. 1) then
		do i = 1, 2*buds, 1
			call corexp
		end do
		end if
		call kgrid
		call bactgrowptw

		holding = coral
		bacthold = bacteria
		fertile = fertile - 1
		sickDays = sickDays - 1

		call datacollect(t)
		call domainout(t)

end do

write(*,*) "Total number of new coral growths:", numnew
write(*,*) "Average number of days between shark attack:", float(numtime)/float(numday)

deallocate(coral)
deallocate(holding)
deallocate(check)
deallocate(bacteria)
deallocate(kbact)
deallocate(seed)


end program
