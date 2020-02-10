PROGRAM P2S
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

! TO DO:
! Initial fish population is user input
! Clean deprecated inputs
! New method for adjustment of carrying capacity
! 		- keep cc array clean and shift adjustment to the bacterial subs

use globalvars
use functions

implicit none
	integer					:: t	! Looping integers
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
fish 					= fish_ini
coral 				= 0.0
holding 			= 0.0
bacteria%totalpop 	= 0
bacteria%numspecies = 0
! Sub Variables
fertile 	= 0
numday 		= 0
numnew 		= 0
def_mod		= 1.0

call date_and_time(values=mstime)
!seed = 4 ! Consistent Seed
seed = (mstime(8)) ! New seed each run
call random_seed(put=seed)

! Populates initial coral layer - coral_subs.F90
call corpop

! Initiallize coral holding layer
holding = coral

write(*,*) "0.0 represents pure algae; greater than zero represents coral, higher number represents more coral"
write(*,*) "Files are written as (x,y,z) where z is the population/biomass"

! initial fish population, P2Smod.f90
fish_carry = 1000.0*percentcor(grid) + 100.0

! Determining bacterial carrying capacity
call kgrid
! Initiallize bacteria holding layer
call microbepop_dom

! Initillizations
fertile = 0
sickDays = 0
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
	fish_carry = 1000.0*percentcor(grid) + 100.0


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

	if (percentcor(grid) .gt. 0.99) then
		write(*,*) "Algae has been removed"
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

	! User output
	write(*,*) "Average coral Growth:", growavg

	! Spawning season code
	if (fertile .gt. 0) then
			call corexp(4*buds)
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
	  	call corexp(1*buds)
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
	if ((t .eq. t_adj).and.(var_adjust_flag .eq. "D")) then
		call var_adjuster(t)
		fish = fish_ini_2nd
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
