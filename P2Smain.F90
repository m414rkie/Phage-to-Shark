PROGRAM P2S
!
! Jon Parsons
! 8-4-19
!
! ------------------ Code -----------------------------

use globalvars
use functions, only: percentcor, fishdelta

implicit none
	integer					:: t	! Looping integers
	integer					:: allck ! Error flag
	integer					:: fertile, buds ! flag for coral ferility, number of coral buds
	integer					:: bgrid ! size of microbe layer
	real						:: growpercmod ! Adjusts coral growth percentage. For use in
																 ! diseases
	real						:: def_mod ! Default coral growth percentage
	real						:: cor_per ! holds percentage of coral
! Timing variables
	real						:: t_tot_i, t_tot_f ! Overall timing
	real						:: t_ini_i, t_ini_f ! initialization timing
	real						:: t_dif_i, t_dif_f, t_dif_s ! diffusion timing
	real						:: t_mic_i, t_mic_f, t_mic_s ! microbe timing
	real						:: t_cor_i, t_cor_f, t_cor_s ! coral timing
	real						:: t_ccd_i, t_ccd_f, t_ccd_S ! carrying capacity timing
	real						:: t_dat_i, t_dat_f, t_dat_s ! data collection timing
	character*9		  :: timing_file ! file name for timings

! timing initializations
timing_file = "times.dat"
t_dif_s = 0.0
t_mic_s = 0.0
t_cor_s = 0.0
t_ccd_s = 0.0
t_dat_s = 0.0

call cpu_time(t_tot_i)

! Handle user inputs - in nonlifesubs.f90
call inputs
bgrid = 2*grid ! microbial grid is twice as large as coral grid
! Allocation statements
allocate(coral(grid,grid), stat=allck)
	if (allck .ne. 0) stop "Coral Allocation Failed"
	coral = 0.0
allocate(holding(grid,grid), stat=allck)
	if (allck .ne. 0) stop "Holding Allocation Failed"
	holding = 0.0
allocate(check(grid,grid), stat=allck)
	if (allck .ne. 0) stop "Check Allocation Failed"
allocate(bacteria(bgrid,bgrid), stat=allck)
	if (allck .ne. 0) stop "Bacteria Allocation Failed"
bacteria%totalpop = 0.0
bacteria%numspecies = 0.0
allocate(kbact(bgrid,bgrid), stat=allck)
	if (allck .ne. 0) stop "Kbact Allocation Failed"
kbact = 0.0
allocate(phage(bgrid,bgrid), stat=allck)
	if (allck .ne. 0) stop "Phage Allocation Failed"
phage%totalpop = 0.0
phage%numspecies = 0.0
allocate(lys(bgrid,bgrid), stat=allck)
	if (allck .ne. 0) stop "Lys Allocation Failed"
lys%totalpop = 0.0
lys%numspecies = 0.0
allocate(seed(33), stat=allck)
	if (allck .ne. 0) stop "Seed Allocation Failed"

! Initializing grids and variables
rate = 1.0 ! Bacterial Growth rate
bacdeath = 0.7 ! Bacterial death rate
phagedie = 0.5 ! Phage death rate
fish 		 = fish_ini ! initial fish population
coral 	 = 0.0 ! initialize to 0
holding  = 0.0 ! initialize to 0
! Sub Variables
fertile  = 0 ! determines amount of times to call coral spawning subroutines
numday 	 = 0 ! determines number of days between succesful shark attacks
numnew 	 = 0 ! number of new coral growth in simulation
sickDays = 0 ! number of days of disease left
def_mod	 = 1.0 ! default coral growth modifier

call cpu_time(t_ini_i)

! Populates initial coral layer - coral_subs.F90
call corpop

! Initiallize coral holding layer
holding = coral

! initial fish cc, P2Smod.f90
fish_carry = 1000.0*percentcor(grid)

! Determining bacterial carrying capacity
call kgrid
! Initiallize bacteria holding layer
call microbepop_dom

call cpu_time(t_ini_f)

! Initilize time
t = 0

! Beginning of data collection - nonlifesubs.f90

call cpu_time(t_dat_i)
call datacollect(t)
call domainout
call cpu_time(t_dat_f)
t_dat_s = t_dat_s + (t_dat_f - t_dat_i)

! user output - initial coral coverage
write(*,*) "Coral percentage:", percentcor(grid)
write(*,*) "Initializing"

! initialization loop
growpercmod = def_mod
do t = 1, 20, 1
	cor_per = percentcor(grid)
	! determine current carrying capacity of fish
	fish_carry = 1000.0*cor_per
	! Determine number of potential new coral buds
	buds = nint(cor_per*10.0)
	! Microbial diffusion
	call diffuse
	! Shark events
	call shark(1)
	! Update fish population
	fish = fish + fishdelta(fish)
	! Disallow negative numbers
	if (fish .lt. 0) then
			fish = 0.0
	end if
	! Update microbe carying capacity
	call kgrid
	! Grow microbe layer
	call bactgrow_dom
	! Update holding layers
	holding = coral
! End of initialization loop
end do

! primary loop
! Outer loop, iterates time
do t = 1, numtime, 1

	cor_per = percentcor(grid)
	! This section to indicate to the user significant events
	write(*,*) "Beginning timestep", t
	write(*,*) "Fish population:", fish
	! determine current carrying capacity of fish
	fish_carry = 1000.0*cor_per
	! determine if it is coral spawning season
	if ((t .gt. 181).and.(mod(t,182) .eq. 0)) then
		write(*,*) "Coral spawning begins"
		fertile = 14
	end if
	! determine if a disaster event happens
	if ((t .eq. numtime/2).and.(disFlag .eq. "H")) then
		call hurricane
		write(*,*) "Hurricane!"
	end if
	! determine if a disease strikes
	if ((t .eq. numtime/2).and.(disFlag .eq. "D")) then
		call disease
		write(*,*) "Disease!"
	end if
	! Exit loop if coral population is too low or too high
	cor_per = percentcor(grid)
	write(*,*) "Coral percentage:", cor_per
	if (cor_per .lt. 0.01) then
		write(*,*) "Reef has died. :("
		goto 103
	end if
	if (cor_per .gt. 0.99) then
		write(*,*) "Algae has been removed"
		goto 103
	end if

	! Determine number of potential new coral buds
	buds = nint(cor_per*10.0)

	! If sickness enabled, sets growth to effectively zero
	if (sickDays .gt. 1) then
		growpercmod = 0.0000001
	else
		growpercmod = def_mod
	end if

	! Microbial diffusion
	call cpu_time(t_dif_i)
	call diffuse
	call cpu_time(t_dif_f)
	t_dif_s = t_dif_s + (t_dif_f - t_dif_i)

	! Shark events
	call shark(1)
	! Update fish population
	fish = fish + fishdelta(fish)
	! Disallow negative numbers
	if (fish .lt. 0) then
			fish = 0.0
	end if

	call cpu_time(t_cor_i)
	! Coral growth and decay
	call growth(holding,coral,growpercmod)
	! User output
	write(*,*) "Average coral growth Percentage:", growavg
	! Disallow spawning during disease
	if (sickDays .lt. 1) then
	  	call corexp(2*buds)
	end if
	! Spawning season, adds additional chances for new coral
	if (fertile .gt. 0) then
			call corexp(10*buds)
	end if
	call cpu_time(t_cor_f)
	t_cor_s = t_cor_s + (t_cor_f - t_cor_i)

	! Update microbe carying capacity
	call cpu_time(t_ccd_i)
	call kgrid
	call cpu_time(t_ccd_f)
	t_ccd_s = t_ccd_s + (t_ccd_f - t_ccd_i)

	! Grow microbe layer
	call cpu_time(t_mic_i)
	call bactgrow_dom
	call cpu_time(t_mic_f)
	t_mic_s = t_mic_s + (t_mic_f - t_mic_i)

	! Update holding layers
	holding = coral

	! data output
	call cpu_time(t_dat_i)
	call datacollect(t)
	call domainout
	call cpu_time(t_dat_f)
	t_dat_s = t_dat_s + (t_dat_f - t_dat_i)

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

call cpu_time(t_tot_f)

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

! Write timing data
open(unit=90,file=timing_file,status="unknown",position="append")
write(90,*) "Values in seconds"
write(90,*) "Total time: ", t_tot_f - t_tot_i
write(90,*) "Seconds/day: ", (t_tot_f-t_tot_i)/float(numtime)
write(90,*) "Diffusion time: ", t_dif_s, "Fraction: ", t_dif_s/(t_tot_f-t_tot_i)
write(90,*) "Microbe time: ", t_mic_s, "Fraction: ", t_mic_s/(t_tot_f-t_tot_i)
write(90,*) "Coral time: ", t_cor_s, "Fraction: ", t_cor_s/(t_tot_f-t_tot_i)
write(90,*) "Microbe Capacity time: ", t_ccd_s, "Fraction: ", t_ccd_s/(t_tot_f-t_tot_i)
write(90,*) "Data Collection time: ", t_dat_s, "Fraction: ", t_dat_s/(t_tot_f-t_tot_i)
close(90)

! Bye
end program
