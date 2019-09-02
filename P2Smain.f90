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
! Jon Parsons
! 8-4-19
!
! Special requirements:
! * Module file - P2Smod.f90
! * Subroutines - coral_subs.F90
!									fish_subs.F90
! 								microbes_subs.F90
! 								nonlifesubs.f90
!
!
! ------------------ Code -----------------------------

use globalvars
use functions

implicit none
	integer					:: i, t, l	! Looping integers
	integer					:: allck ! Error flag
	integer					:: fertile, buds, mstime(8) ! flags for coral ferility
																							! Number of coral buds; timing array
	real						:: growpercmod ! Adjusts coral growth percentage. For use in
																 ! diseases
	real						:: def_mod ! Default coral growth percentage

! Handle user inputs - in nonlifesubs.f90
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
def_mod		= 0.1

call date_and_time(values=mstime)
seed = 2
!seed = (mstime(8)) ! New seed each run
call random_seed(put=seed) ! Consistent seed

! Populates initial coral layer - coral_subs.F90
call corpop

! Initiallize coral holding layer
holding = coral

write(*,*) "0.0 represents pure algae; greater than zero represents coral, higher number represents more coral"
write(*,*) "Files are written as (x,y,z) where z is the population/biomass"

! initial fish population, P2Smod.f90
fish = 990.0*percentcor(grid)

! Populating initital bacteria layer - microbe_subs.F90
call kgrid
call microbepop_dom

! Initiallize bacteria holding layer
bacthold = bacteria

! Initillizations
fertile = 0
sickDays = 0
t = 0

write(*,*) "Beginning Equilibration"

! Equilibration loops. Data is not output
do t = 1, 300, 1

if (t .eq. 60) then
	write(*,*) "Equilibration 20% Complete."
else if (t .eq. 120) then
	write(*,*) "Equilibration 40% Complete."
else if (t .eq. 180) then
	write(*,*) "Equilibration 60% Complete."
else if (t .eq. 240) then
	write(*,*) "Equilibration 80% Complete."
end if

		! Determine how many coral may potentially spawn
		buds = nint(percentcor(grid)*10.0)

		! Bacteria diffusion - microbe_subs.F90
		call diffuse

		! Set coral growth percentage
		growpercmod = def_mod

		! Coral growth and coral loss subroutines - coral_subs.F90
		call growth(holding,coral,growpercmod)
		call decay(holding,coral)

		! This section for determining spawning season. 2 'weeks' twice a 'year'
		if ((t .gt. 181).and.(mod(t,182) .eq. 0)) then
			write(*,*) "Coral spawning begins"
			fertile = 14
		end if

		! spawning season. More chances of coral spawning during this time
		if (fertile .gt. 0) then
			do l = 1, 5*buds, 1
				call newcoral
			end do
		end if

		! Shark event subroutine
		call shark(0)
		! Adjust fish population
		fish = fish + fishdelta(fish)

		! Code for new coral without spawning season bonus
		do i = 1, buds, 1
			call corexp
		end do

		! Adjust microbial carrying capacity-  microbe_subs.F90
		call kgrid
		! Grow the microbial community - microbe_subs.F90
		call bactgrow_dom

		! Update holding layers
 		holding = coral
		bacthold = bacteria

end do

write(*,*) "Equilibration Complete"

! Set flag to indicate data collection is about to begin
t = 0

! Beginning of data collection - nonlifesubs.f90
call datacollect(t)
call domainout

! user output
write(*,*) "Coral percentage:", percentcor(grid)

! Outer loops iterates time
do t = 1, numtime, 1

	! This section to indicate to the user significant events
	write(*,*) "Beginning timestep", t

	write(*,*) "Fish population:", fish

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

	! Exit loop if coral population is too low
	write(*,*) "Coral percentage:", percentcor(grid)
	if (percentcor(grid) .lt. 0.01) then
		write(*,*) "Reef has died. :("
		goto 103
	end if

	! Determine number of potential new coral buds
	buds = nint(percentcor(grid)*10.0)

	! If sickness enabled, sets growth to effectively zero
	if (sickDays .gt. 1) then
		growpercmod = 0.0000001
	else
		growpercmod = def_mod
	end if

	! Microbial diffusion
	call diffuse

	! Coral growth and decay
	call growth(holding,coral,growpercmod)
	call decay(holding,coral)

	! User output
	write(*,*) "Average coral loss:", decavg
	write(*,*) "Average coral Growth:", growavg

	! Spawning season code
	if (fertile .gt. 0) then
		do l = 1, 5*buds, 1
			call newcoral
		end do
	end if

	! Shark events
	call shark(1)
	! Update fish population
	fish = fish + fishdelta(fish)
	! Disallow negative numbers
	if (fish .lt. 0) then
			fish = 0.0
	end if

	! Disallow spawning during disease
	if (sickDays .lt. 1) then
	  do i = 1, buds, 1
	  	call corexp
	  end do
	end if

	! Update microbe carying capacity
	call kgrid
	! Grow microbe layer
	call bactgrow_dom

	! Update holding layers
	holding = coral
	bacthold = bacteria

	! data output
	call datacollect(t)
	call domainout

	! Adjust number of spawning season days and deisease days
	fertile = fertile - 1
	sickDays = sickDays - 1

	! At the halfway point, adjust parameters
	if ((t .eq. (numTime/2)).and.(var_adjust_flag .eq. "D")) then
		call var_adjuster(t)
	end if

! End of main loop
end do

! User output
103 write(*,*) "Total number of new coral growths:", numnew
write(*,*) "Average number of days between shark attack:", float(numtime)/float(numday)

! Deallocation statements
deallocate(coral)
deallocate(holding)
deallocate(check)
deallocate(bacteria)
deallocate(kbact)
deallocate(seed)

! Bye
end program
