subroutine printtofile(arrin,size,file)

! Prints to a file in x-y-z format

use globalvars

implicit none
	integer, intent(in)							:: size
	real,dimension(size,size),intent(in)		:: arrin
	character(len=*), intent(in)				:: file
	integer 									:: i, j

open(unit=19,file=trim(file),status="replace",position="append")

do i = 1, size, 1
	do j = 1, size, 1
		write(19,*) i, j, arrin(i,j)
	end do
	write(19,*)
end do

close(19)

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine dircheck(path)
! Checks to see if a directoy exists.
! Creates directory if not already there

implicit none
	character(len=*)		:: path
	character(len=100)		:: makepath
	logical					:: check


	inquire(file=trim(path)//'/.', exist=check)

	if (check .eqv. .false.) then
		makepath = "mkdir -p "//trim(path)
		call system(makepath)
	end if

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine datacollect(tim)
! Data collection subroutine. Reef averages and totals

use globalvars
use functions

implicit none
	integer			  :: tim
	real (kind=8) :: avgcoral, lysum, bacspec, phagespec
	real (kind=8)	:: phagelysratio, phagesum, bacsum
	real (kind=8) :: cordelt, lysspec
	character*50	:: corfile
	character*50	:: corpath
	character*50	:: genpath
	character*50	:: cor_perc_file
	character*50	:: cor_tot_file
	character*50	:: bact_pop_file, fish_tot_file
	character*50	:: cor_avg_file, phage_lys_rat_file
	character*50	:: phage_pop_file, lys_pop_file, phage_spec_file
	character*50	:: lys_spec_file, bact_spec_file, cor_delt_file
	character*50	:: fish_delt_file, alg_perc_file, vmr_file
	character*50	:: shark_evt_file, time_file
	real				  :: fdel
	real				  :: alg
	real (kind=8)	:: vmr, lysperc

	integer				:: i, j

! Data manipulation
phagesum = 0.0
bacsum = 0.0
lysum = 0.0
bacspec = 0.0
phagespec = 0.0
lysspec = 0.0

! Find microbe populations and species
do i = 1, 2*grid, 1
	do j = 1, 2*grid, 1
		phagesum = phagesum + phage(i,j)%totalpop
		bacsum = bacsum + bacteria(i,j)%totalpop
		lysum = lysum + lys(i,j)%totalpop
		bacspec = bacspec + bacteria(i,j)%numspecies
		phagespec = phagespec + phage(i,j)%numspecies
		lysspec = lysspec + lys(i,j)%numspecies
	end do
end do

! Average them. factor of 0.04 to normalize from 25 cm^3 to 1 ml
bacsum = bacsum*0.04/(real(2*grid,8)**2)
phagesum = phagesum*0.04/(real(2*grid,8)**2)
lysum = lysum*0.04/(real(2*grid,8)**2)
bacspec = bacspec/(real(2*grid,8)**2)
phagespec = phagespec/(real(2*grid,8)**2)
lysspec = lysspec/(real(2*grid,8)**2)

! Average coral value over the reef
avgcoral = sum(coral)/(real(grid)**2)
! Phage - lysogen ratio
phagelysratio = real(phagesum,8)/real(sum(lys%totalpop),8)
! VMR
vmr = phagesum/(bacsum+lysum)
! Change in fish population
fdel = fishdelta(fish)
! Percentage of algal cover
alg = 1.0 - percentcor(grid)
! Fraction of lysogenic population
lysperc = lysum/bacsum
! Change in coral overall
cordelt = (sum(coral) - sum(holding))

! Format statements
50 format ("Coral/coraltime",1i4,".dat")

! File path statements
corpath   = "~/Desktop/Phage2Shark/Coral"
genpath   = "~/Desktop/Phage2Shark/General"

call dircheck(corpath)
call dircheck(genpath)

! File names
cor_perc_file = "General/coral_fraction.dat"
cor_tot_file = "General/coral_total.dat"
bact_pop_file = "General/bact_pop.dat"
fish_tot_file = "General/fish_pop.dat"
cor_avg_file = "General/coral_average.dat"
phage_lys_rat_file = "General/phage_lys_rat.dat"
phage_pop_file = "General/phage_pop.dat"
lys_pop_file = "General/lys_pop.dat"
phage_spec_file = "General/phage_spec.dat"
lys_spec_file = "General/lys_spec.dat"
bact_spec_file = "General/bact_spec.dat"
cor_delt_file = "General/coral_delta.dat"
fish_delt_file = "General/fish_delta.dat"
alg_perc_file = "General/algae_perc.dat"
vmr_file = "General/vmr.dat"
shark_evt_file = "General/shark_evt.dat"
time_file = "General/time.dat"

! Statements to control output of reef picture
if (tim .eq. 0) then
	corfile   = "Coral/coraltime00.dat"
else if (tim .ne. 0) then
	write(corfile,50) tim
end if

! Prints out coral array in xyz
call printtofile(coral,grid,corfile)

! Open files
open(unit=15,file=cor_perc_file,status="unknown",position="append")
open(unit=20,file=cor_tot_file,status="unknown",position="append")
open(unit=21,file=bact_pop_file,status="unknown",position="append")
open(unit=22,file=fish_tot_file,status="unknown",position="append")
open(unit=23,file=cor_avg_file,status="unknown",position="append")
open(unit=24,file=phage_lys_rat_file,status="unknown",position="append")
open(unit=25,file=phage_pop_file,status="unknown",position="append")
open(unit=26,file=lys_spec_file,status="unknown",position="append")
open(unit=27,file=bact_spec_file,status="unknown",position="append")
open(unit=28,file=cor_delt_file,status="unknown",position="append")
open(unit=29,file=fish_delt_file,status="unknown",position="append")
open(unit=30,file=alg_perc_file,status="unknown",position="append")
open(unit=31,file=vmr_file,status="unknown",position="append")
open(unit=32,file=shark_evt_file,status="unknown",position="append")
open(unit=33,file=time_file,status="unknown",position="append")
open(unit=34,file=lys_pop_file,status="unknown",position="append")
open(unit=35,file=phage_spec_file,status="unknown",position="append")

! Write to files
write(15,*) percentcor(grid) ! Coral percentage
write(20,*) sum(coral) ! Coral total
write(21,*) bacsum + lysum! Bacteria pop
write(22,*) fish ! fish pop
write(23,*) avgcoral ! average coral
write(24,*) phagelysratio ! Phage-lysogen ratio
write(25,*) phagesum ! Phage pop
write(26,*) lysspec ! lysogen species
write(27,*) bacspec ! bacteria species
write(28,*) cordelt ! change in coral
write(29,*) fdel ! change in fish
write(30,*) alg ! algae percentage
write(31,*) vmr ! VMR
write(32,*) shrkevt ! If a shark hunt was succesful, prints 1, else 0
write(33,*) tim ! current timesep
write(34,*) lysum ! lysogen pop
write(35,*) phagespec

! Close files
close(15)
close(20)
close(21)
close(22)
close(23)
close(24)
close(25)
close(26)
close(27)
close(28)
close(30)
close(31)
close(32)
close(33)
close(34)
close(35)

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine inputs
! User inputs, sort of self-documenting

use globalvars

implicit none
	character*1		:: disFlagin, var_adjust_flagin ! Flags for disasters and adjustments

disFlag = "N" ! default

! User Inputs - First section
write(*,*) "Enter the dimension of the grid (square):"
read(*,*) grid
write(*,*) "Enter the number of time steps :"
read(*,*) numtime
write(*,*) "Enter percentage of bed with coral:"
read(*,*) percentcover
write(*,*) "Enter the mass of Piscivores."
read(*,*) sharkMass
write(*,*) "Enter the average number of days between shark attacks."
read(*,*) dayavg
write(*,*) "Enter the burst size of infected bacteria."
read(*,*) bacBurst
write(*,*) "Enter the initial fish population."
read(*,*) fish_ini
write(*,*) "Enter the rate of fish growth."
read(*,*) fgrowfact
write(*,*) "Enter the diffusion coefficient."
read(*,*) diffco
write(*,*) "Enter 'H' for a hurricane, or 'D' for a disease."
write(*,*) "Any other entry will result in neither."
read(*,*) disFlagin
! User inputs - Second section
write(*,*) "To adjust variables on the second half, enter 'D.'"
write(*,*) "Any other entry will result in no value change"
read(*,*) var_adjust_flagin

call chartoup(var_adjust_flagin,var_adjust_flag)

if (var_adjust_flag .eq. "D") then
	write(*,*) "Time to change values?"
	read(*,*) t_adj
	write(*,*) "Enter the mass of Piscivores."
	read(*,*) sharkMass_2nd
	write(*,*) "Enter the average number of days between shark attacks."
	read(*,*) dayavg_2nd
	write(*,*) "Enter the burst size of infected bacteria."
	read(*,*) bacBurst_2nd
	write(*,*) "Enter the population of fish"
	read(*,*) fish_ini_2nd
	write(*,*) "Enter the rate of fish growth."
	read(*,*) fgrowfact_2nd
	write(*,*) "Enter the diffusion coefficient."
	read(*,*) diffco_2nd
end if

call chartoup(disFlagin,disFlag)

if ((disFlag .eq. "H").or.(disFlag .eq. "D")) then
	write(*,*) "Enter the severity of the disaster, 1 - 5 as an integer."
	write(*,*) "A higher number increases the severity."

11	read(*,*) disSevere

	if ((disSevere .lt. 1).or.(disSevere .gt. 5)) then
		write(*,*) "Input the severity as an integer between 1 and 5"
		goto 11
	end if

end if


end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine chartoup(stringin,stringout)

! converts text input to upper case

implicit none
	character(*)					:: stringin
	character(len(stringin))		:: stringout
	integer							:: i, j

do i = 1, len(stringin), 1
	j = iachar(stringin(i:i))
		if(j .ge. iachar("a") .and. j .le. iachar("z")) then
			stringout(i:i) = achar(iachar(stringin(i:i))-32)
		else
			stringout(i:i) = stringin(i:i)
		end if
end do

end subroutine


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine domainout
! Collects data and outputs for coral, algae, and interface regions

use globalvars

implicit none
	integer				:: algCount, corCount, barCount, sumCount
	logical				:: kcounter(2*grid,2*grid)
	real(kind=8)	:: AvgBactCapUse ! Holds average bacteria carrying capacity usage
	real(kind=8)	:: bact_pop_cor, bact_pop_alg, bact_pop_bar ! bact pop
	real(kind=8)	:: lys_pop_cor, lys_pop_alg, lys_pop_bar ! lys pop
	real(kind=8)	:: phage_pop_cor, phage_pop_alg, phage_pop_bar ! phage pop
	real(kind=8)	:: bact_spec_cor, bact_spec_alg, bact_spec_bar ! bact spec
	real(kind=8)	:: lys_spec_cor, lys_spec_alg, lys_spec_bar ! lys spec
	real(kind=8)	:: phage_spec_cor, phage_spec_alg, phage_spec_bar ! phage spec
	real(kind=8)	:: phage_lys_rat_cor, phage_lys_rat_alg, phage_lys_rat_bar ! phage lys ratio
	real(kind=8)	:: vmr_cor, vmr_alg, vmr_bar ! vmr

	character*50	:: bact_pop_cor_file, bact_pop_alg_file, bact_pop_bar_file ! Bact pop
	character*50	:: phage_lys_rat_cor_file, phage_lys_rat_alg_file, phage_lys_rat_bar_file ! phage lys ratio
	character*50	:: phage_pop_cor_file, phage_pop_alg_file, phage_pop_bar_file ! phage pop
	character*50 	:: lys_pop_cor_file, lys_pop_alg_file, lys_pop_bar_file ! pys pop
  character*50	:: phage_spec_cor_file, phage_spec_alg_file, phage_spec_bar_file ! Phage spec
	character*50	:: lys_spec_cor_file, lys_spec_alg_file, lys_spec_bar_file ! lys spec
  character*50	:: bact_spec_cor_file, bact_spec_alg_file, bact_spec_bar_file  ! bact spec
	character*50	:: vmr_cor_file, vmr_alg_file, vmr_bar_file ! vmr
	character*50	:: area_cor_file, area_alg_file, area_bar_file ! Percs. of area

	integer				:: i, j ! Looping integers

! File names
bact_pop_cor_file = "General/bact_pop_cor.dat"
bact_pop_alg_file = "General/bact_pop_alg.dat"
bact_pop_bar_file = "General/bact_pop_bar.dat"
phage_lys_rat_cor_file = "General/phage_lys_rat_cor.dat"
phage_lys_rat_alg_file = "General/phage_lys_rat_alg.dat"
phage_lys_rat_bar_file = "General/phage_lys_rat_bar.dat"
phage_pop_cor_file = "General/phage_pop_cor.dat"
phage_pop_alg_file = "General/phage_pop_alg.dat"
phage_pop_bar_file = "General/phage_pop_bar.dat"
lys_pop_cor_file = "General/lys_pop_cor.dat"
lys_pop_alg_file = "General/lys_pop_alg.dat"
lys_pop_bar_file = "General/lys_pop_bar.dat"
phage_spec_cor_file = "General/phage_spec_cor.dat"
phage_spec_alg_file = "General/phage_spec_alg.dat"
phage_spec_bar_file = "General/phage_spec_bar.dat"
lys_spec_cor_file = "General/lys_spec_cor.dat"
lys_spec_alg_file = "General/lys_spec_alg.dat"
lys_spec_bar_file = "General/lys_spec_bar.dat"
bact_spec_cor_file = "General/bact_spec_cor.dat"
bact_spec_alg_file = "General/bact_spec_alg.dat"
bact_spec_bar_file = "General/bact_spec_bar.dat"
vmr_cor_file = "General/vmr_cor.dat"
vmr_alg_file = "General/vmr_alg.dat"
vmr_bar_file = "General/vmr_bar.dat"
area_cor_file = "General/area_cor.dat"
area_alg_file = "General/area_alg.dat"
area_bar_file = "General/area_bar.dat"

! Initializations
bact_pop_cor = 0.0
bact_pop_alg = 0.0
bact_pop_bar = 0.0
lys_pop_cor = 0.0
lys_pop_alg = 0.0
lys_pop_bar = 0.0
phage_pop_cor = 0.0
phage_pop_alg = 0.0
phage_pop_bar = 0.0
bact_spec_cor = 0.0
bact_spec_alg = 0.0
bact_spec_bar = 0.0
lys_spec_cor = 0.0
lys_spec_alg = 0.0
lys_spec_bar = 0.0
phage_spec_cor = 0.0
phage_spec_alg = 0.0
phage_spec_bar = 0.0
phage_lys_rat_cor = 0.0
phage_lys_rat_alg = 0.0
phage_lys_rat_bar = 0.0
vmr_cor = 0.0
vmr_alg = 0.0
vmr_bar = 0.0

! Open the files
open(unit=41,file=bact_pop_cor_file,status="unknown",position="append")
open(unit=42,file=bact_pop_alg_file,status="unknown",position="append")
open(unit=43,file=bact_pop_bar_file,status="unknown",position="append")
open(unit=44,file=phage_lys_rat_cor_file,status="unknown",position="append")
open(unit=45,file=phage_lys_rat_alg_file,status="unknown",position="append")
open(unit=46,file=phage_lys_rat_bar_file,status="unknown",position="append")
open(unit=47,file=phage_pop_cor_file,status="unknown",position="append")
open(unit=48,file=phage_pop_alg_file,status="unknown",position="append")
open(unit=49,file=phage_pop_bar_file,status="unknown",position="append")
open(unit=50,file=lys_pop_cor_file,status="unknown",position="append")
open(unit=51,file=lys_pop_alg_file,status="unknown",position="append")
open(unit=52,file=lys_pop_bar_file,status="unknown",position="append")
open(unit=53,file=phage_spec_cor_file,status="unknown",position="append")
open(unit=54,file=phage_spec_alg_file,status="unknown",position="append")
open(unit=55,file=phage_spec_bar_file,status="unknown",position="append")
open(unit=56,file=lys_spec_cor_file,status="unknown",position="append")
open(unit=57,file=lys_spec_alg_file,status="unknown",position="append")
open(unit=58,file=lys_spec_bar_file,status="unknown",position="append")
open(unit=59,file=bact_spec_cor_file,status="unknown",position="append")
open(unit=60,file=bact_spec_alg_file,status="unknown",position="append")
open(unit=61,file=bact_spec_bar_file,status="unknown",position="append")
open(unit=62,file=vmr_cor_file,status="unknown",position="append")
open(unit=63,file=vmr_alg_file,status="unknown",position="append")
open(unit=64,file=vmr_bar_file,status="unknown",position="append")
open(unit=65,file=area_cor_file,status="unknown",position="append")
open(unit=66,file=area_alg_file,status="unknown",position="append")
open(unit=67,file=area_bar_file,status="unknown",position="append")

! Count grid locations of each region
kcounter = (kbact .eq. kalg)

algCount = count(kcounter)

kcounter = (kbact .eq. kcor)

corCount = count(kcounter)

kcounter = (kbact .eq. kbar)

barCount = count(kcounter)

AvgBactCapUse = 0.0

sumCount = algCount + corCount + barCount

write(65,*) float(corCount)/(4*grid*grid)
write(66,*) float(algCount)/(4*grid*grid)
write(67,*) float(barCount)/(4*grid*grid)

! Ensure things are working out
if (sumCount .ne. (4*grid*grid)) then
	write(*,*) "Issue in Domain outputs"
end if

do i = 1, 2*grid, 1

yloop:	do j = 1, 2*grid, 1

		! The two statements below are useful for testing, should not occur else
		if(bacteria(i,j)%totalpop .eq. 0) then
		!	write(*,*) "Bacteria pop zero at ", i, j
			cycle yloop
		end if

		if(kbact(i,j) .eq. 0) then
		!	write(*,*) "Bacteria carrying capacity zero at ", i, j
			cycle yloop
		end if

		! sum variables for algal region
		if (kbact(i,j) .eq. kalg) then
			bact_pop_alg = bact_pop_alg + bacteria(i,j)%totalpop
			lys_pop_alg = lys_pop_alg + lys(i,j)%totalpop
			phage_pop_alg = phage_pop_alg + phage(i,j)%totalpop
			bact_spec_alg = bact_spec_alg + bacteria(i,j)%numspecies
			lys_spec_alg = lys_spec_alg + lys(i,j)%numspecies
			phage_spec_alg = phage_spec_alg + phage(i,j)%numspecies
			phage_lys_rat_alg = phage_lys_rat_alg + (real(phage(i,j)%totalpop,8)/real(lys(i,j)%totalpop,8))
			vmr_alg = vmr_alg + (real(phage(i,j)%totalpop,8)/real(bacteria(i,j)%totalpop,8))
		end if

		! sum variables for coral region
		if (kbact(i,j) .eq. kcor) then
			bact_pop_cor = bact_pop_cor + bacteria(i,j)%totalpop
			lys_pop_cor = lys_pop_cor + lys(i,j)%totalpop
			phage_pop_cor = phage_pop_cor + phage(i,j)%totalpop
			bact_spec_cor = bact_spec_cor + bacteria(i,j)%numspecies
			lys_spec_cor = lys_spec_cor + lys(i,j)%numspecies
			phage_spec_cor = phage_spec_cor + phage(i,j)%numspecies
			phage_lys_rat_cor = phage_lys_rat_cor + (real(phage(i,j)%totalpop,8)/real(lys(i,j)%totalpop,8))
			vmr_cor = vmr_cor + (real(phage(i,j)%totalpop,8)/real(bacteria(i,j)%totalpop,8))
		end if

		! Sum variables for barrier region
		if (kbact(i,j) .eq. kbar) then
			bact_pop_bar = bact_pop_bar + bacteria(i,j)%totalpop
			lys_pop_bar = lys_pop_bar + lys(i,j)%totalpop
			phage_pop_bar = phage_pop_bar + phage(i,j)%totalpop
			bact_spec_bar = bact_spec_bar + bacteria(i,j)%numspecies
			lys_spec_bar = lys_spec_bar + lys(i,j)%numspecies
			phage_spec_bar = phage_spec_bar + phage(i,j)%numspecies
			phage_lys_rat_bar = phage_lys_rat_bar + (real(phage(i,j)%totalpop,8)/real(lys(i,j)%totalpop,8))
			vmr_bar = vmr_bar + (real(phage(i,j)%totalpop,8)/real(bacteria(i,j)%totalpop,8))
		end if

		! Sum capacity use
		AvgBactCapUse = AvgBactCapUse + bacteria(i,j)%totalpop/kbact(i,j)

	end do yloop

end do

! Average capacity utilization
AvgBactCapUse = AvgBactCapUse/float(4*grid*grid)
write(*,*) "Average bacteria carrying capacity utilization: ", AvgBactCapUse

! Normalize values
bact_pop_cor = bact_pop_cor*0.04/real(corCount,8)
bact_pop_alg = bact_pop_alg*0.04/real(algCount,8)
bact_pop_bar = bact_pop_bar*0.04/real(barCount,8)
lys_pop_cor = lys_pop_cor*0.04/real(corCount,8)
lys_pop_alg = lys_pop_alg*0.04/real(algCount,8)
lys_pop_bar = lys_pop_bar*0.04/real(barCount,8)
phage_pop_cor = phage_pop_cor*0.04/real(corCount,8)
phage_pop_alg = phage_pop_alg*0.04/real(algCount,8)
phage_pop_bar = phage_pop_bar*0.04/real(barCount,8)
bact_spec_cor = bact_spec_cor/real(corCount,8)
bact_spec_alg = bact_spec_alg/real(algCount,8)
bact_spec_bar = bact_spec_bar/real(barCount,8)
lys_spec_cor = lys_spec_cor/real(corCount,8)
lys_spec_alg = lys_spec_alg/real(algCount,8)
lys_spec_bar = lys_spec_bar/real(barCount,8)
phage_spec_cor = phage_spec_cor/real(corCount,8)
phage_spec_alg = phage_spec_alg/real(algCount,8)
phage_spec_bar = phage_spec_bar/real(barCount,8)
phage_lys_rat_cor = phage_lys_rat_cor/real(corCount,8)
phage_lys_rat_alg = phage_lys_rat_cor/real(algCount,8)
phage_lys_rat_bar = phage_lys_rat_bar/real(barCount,8)
vmr_cor = vmr_cor/real(corCount,8)
vmr_alg = vmr_alg/real(algCount,8)
vmr_bar = vmr_bar/real(barCount,8)

! write to files
write(41,*) bact_pop_cor
write(42,*) bact_pop_alg
write(43,*) bact_pop_bar
write(44,*) phage_lys_rat_cor
write(45,*) phage_lys_rat_alg
write(46,*) phage_lys_rat_bar
write(47,*) phage_pop_cor
write(48,*) phage_pop_alg
write(49,*) phage_pop_bar
write(50,*) lys_pop_cor
write(51,*) lys_pop_alg
write(52,*) lys_pop_bar
write(53,*) phage_spec_cor
write(54,*) phage_spec_alg
write(55,*) phage_spec_bar
write(56,*) lys_spec_cor
write(57,*) lys_spec_alg
write(58,*) lys_spec_bar
write(59,*) bact_spec_cor
write(60,*) bact_spec_alg
write(61,*) bact_spec_bar
write(62,*) vmr_cor
write(63,*) vmr_alg
write(64,*) vmr_bar

! Close files
close(41) ; close(42) ; close(43)
close(44) ; close(45) ; close(46)
close(47) ; close(48) ; close(49)
close(50) ; close(51) ; close(52)
close(53) ; close(54) ; close(55)
close(56) ; close(57) ; close(58)
close(59) ; close(60) ; close(61)
close(62) ; close(63) ; close(64)
close(65) ; close(66) ; close(67)

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine var_adjuster(tflag)
! Adjusts variables from initial values to the values set for the second half
! of the run

use globalvars

implicit none
	integer		:: tflag ! Tells what timestep this happens in

write(*,*) "Adjusting parameters at time", tflag

! Adjust parameters to those set by user inputs
threshold = threshold_2nd
sharkMass = sharkMass_2nd
dayavg = dayavg_2nd
bacBurst = bacBurst_2nd
fgrowfact = fgrowfact_2nd
diffco = diffco_2nd

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
