subroutine printer(arr,big)
	
! Printing subroutine for arrays of two dimensions

implicit none
	real,dimension(big,big),intent(in)			:: arr	! Input matix
	integer,intent(in)							:: big	! Size of matrix
	integer										:: i, j	! Looping integers

! Format statement
50 format(5g11.5)		

! Writing loop
do i =1,big
	write(*,50)(arr(i,j),j=1,big)
end do

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine printbact(filea,fileb,filec)

! Prints to a file in x-y-int-int format. Column 3 totalpop, Column 4 number of species

use globalvars

implicit none
	character*50, intent(in)					:: filea, fileb, filec
	integer 									:: i, j
	
open(unit=16,file=trim(filea),status="replace",position="append")
open(unit=17,file=trim(fileb),status="replace",position="append")
open(unit=18,file=trim(filec),status="replace",position="append")

do i = 1, 2*grid, 1
	do j = 1, 2*grid, 1
		write(16,*) i, j, bacteria(i,j)%totalpop, bacteria(i,j)%numspecies
		write(17,*) i, j, phage(i,j)%totalpop, phage(i,j)%numspecies
		write(18,*) i, j, lys(i,j)%totalpop, lys(i,j)%numspecies
	end do
end do

close(16)
close(17)
close(18)

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine dircheck(path)

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
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine datacollect(tim)

use globalvars
use functions

implicit none
	integer			:: tim
	real			:: avgcoral, avgbact
	real (kind=8)	:: phagelysratio, phagesum, vmRatio, bacsum
	character*50	:: corfile
	character*50	:: percfile
	character*50	:: corpath
	character*50	:: genpath
	character*50	:: totbactfile, totfishfile
	character*50	:: avgcoralfile, phlyratiofile
	character*50	:: micropopfile, microspecfile, microratiofile
	character*50	:: cortotfile, corgrowfile
	
! Data manipulation
avgcoral = sum(coral)/(real(grid)**2)
avgbact = real(sum(bacteria%totalpop))/(real(2*grid)**2)
phagesum = real(sum(phage%totalpop))
bacsum = real(sum(bacteria%totalpop))
phagelysratio = real(phagesum)/real(sum(lys%totalpop))
vmRatio = (phagesum)/bacsum
	
! Format statements
50 format ("Coral/coraltime",1i4,".dat")

! File path statements
corpath   = "~/Desktop/Phage2Shark/Coral"
genpath   = "~/Desktop/Phage2Shark/General"

call dircheck(corpath)
call dircheck(genpath)

percfile	  = "General/perctime.dat"
totbactfile   = "General/bacttime.dat"
totfishfile   = "General/fishtottime.dat"
avgcoralfile  = "General/avgcortime.dat"
phlyratiofile = "General/phagelysratio.dat"
micropopfile  = "General/microbepops.dat"
microspecfile = "General/microbespecs.dat"
cortotfile 	  = "General/cortottime.dat"
corgrowfile	  = "General/Corgrowth.dat"
microratiofile = "General/vmr.dat"

if (tim .eq. 0) then
	corfile   = "Coral/coraltime00.dat"
else if (tim .ne. 0) then
	write(corfile,50) tim
end if
	
call printtofile(coral,grid,corfile)
	
	open(unit=15,file=percfile,status="unknown",position="append")
	open(unit=20,file=totbactfile,status="unknown",position="append")
	open(unit=21,file=totfishfile,status="unknown",position="append")
	open(unit=22,file=avgcoralfile,status="unknown",position="append")
	open(unit=23,file=phlyratiofile,status="unknown",position="append")
	open(unit=24,file=micropopfile,status="unknown",position="append")
	open(unit=25,file=microspecfile,status="unknown",position="append")
	open(unit=26,file=cortotfile,status="unknown",position="append")
	open(unit=27,file=corgrowfile,status="unknown",position="append")
	open(unit=28,file=microratiofile,status="unknown",position="append")
	
if (tim .eq. 0) then 
	write(15,*) "Time Percentage-of-Coral"
	write(20,*) "Time Bacteria-Pop."
	write(21,*) "Time Total-Fish-Mass"
	write(22,*) "Time Average-Coral"
	write(23,*) "Time Phage/Lysogen-Ratio"
	write(24,*) "Time Bacteria Lytic Lysogenic"
	write(25,*) "Time Bacteria Lytic Lysogenic"
	write(26,*) "Time Total-Coral-Mass"
	write(27,*) "Time Growth Shark-Event"
	write(28,*) "Time VMR"
end if
	
	
!!! Bacteria/Phage prefactors are based on a 100x100 grid, with each grid being 10cm a side.
!!!  For calculation purposes the values are normalized by divided by 1e8. With the above grid
!!!  and accounting for a 10m water column, we work out to a final 1/10 division.
write(15,*) tim, percentcor(grid)
write(20,*) tim, sum(bacteria%totalpop)*0.1
write(21,*) tim, fish
write(22,*) tim, avgcoral
write(23,*) tim, phagelysratio
write(24,*) tim, 0.1*sum(bacteria%totalpop), 0.1*phagesum, sum(lys%totalpop)*0.1
write(25,*) tim, sum(bacteria%numspecies)/(grid*grid), sum(phage%numspecies)/(grid*grid), sum(lys%numspecies)/(grid*grid)
write(26,*) tim, sum(coral)
if (shrkevt .eq. 0.0) then
	write(27,*) tim, (sum(coral)-sum(holding))/sum(coral)
else
	write(27,*) tim, (sum(coral)-sum(holding))/sum(coral), shrkevt/1000.0
end if
write(28,*) tim, vmRatio

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

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine inputs

use globalvars

implicit none
	character*1		:: disFlagin

disFlag = "N"

! User Inputs
write(*,*) "Enter the dimension of the grid (square):"
read(*,*) grid
write(*,*) "Enter the number of time steps :"
read(*,*) numtime
write(*,*) "Enter percentage of bed with coral:"
read(*,*) percentcover
write(*,*) "New coral threshold?"
read(*,*) threshold
write(*,*) "Enter the mass of Piscivores."
read(*,*) sharkMass
write(*,*) "Enter the average number of days between shark attacks."
read(*,*) dayavg
write(*,*) "Enter the value of the bacterial growth rate."
read(*,*) rate
write(*,*) "Enter the effect of bacteria on new coral."
read(*,*) corBacNew
write(*,*) "Enter the effect of bacteria on growing coral."
read(*,*) corBacGrow
write(*,*) "Enter the adsorption coefficient factor."
read(*,*) adsorpFac
write(*,*) "Enter the ratio of bacteria that die each timestep."
read(*,*) bacDeath
write(*,*) "Enter the burst size of infected bacteria."
read(*,*) bacBurst
write(*,*) "Enter the ratio of phage that die each timestep."
read(*,*) phagedie
write(*,*) "Enter the fish impact multiplier on algae."
read(*,*) fisheatmult
write(*,*) "Enter the rate of fish growth."
read(*,*) fgrowfact
write(*,*) "Enter 'H' for a hurricane, or 'D' for a disease."
write(*,*) "Any other entry will result in neither."
read(*,*) disFlagin

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine againstouts(tim)

use globalvars
use functions

implicit none
	integer				:: tim
	character*50		:: againstpath	
	character*50		:: vmr_micab_file, fgrow_fbio_file
	character*50		:: algae_fbio_file, cor_fbio_file
	real				:: fdel
	real				:: alg
	real(kind=8)		:: phagesum, vmr, bacsum
	
againstpath	  = "~/Desktop/Phage2Shark/General"

call dircheck(againstpath)

vmr_micab_file = "General/vmrmic.dat"
fgrow_fbio_file = "General/fdelftot.dat"
algae_fbio_file = "General/algftot.dat"

phagesum = real(sum(phage%totalpop))
bacsum = real(sum(bacteria%totalpop))
vmr = phagesum/bacsum
fdel = fishdelta(fish)
alg = 1.0 - percentcor(grid)

open(unit=30,file=vmr_micab_file,status="unknown",position="append")
open(unit=31,file=fgrow_fbio_file,status="unknown",position="append")
open(unit=32,file=algae_fbio_file,status="unknown",position="append")

if (tim .eq. 0) then
	write(30,*) "VMR Microbes"
	write(31,*) "Fdelta Fish"
	write(32,*) "Algae Fish"
end if

write(30,*) vmr, phagesum+bacsum
write(31,*) fish, fdel
write(32,*) alg, fish

close(30)
close(31)
close(32)

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine domainout(tim)

use globalvars

implicit none
	integer				:: tim
	character*50		:: mic_algFile, mic_corFile, mic_barFile
	character*50		:: vmr_algFile, vmr_corFile, vmr_barFile
	integer				:: algCount, corCoun, barCount
	real				:: algAvg, corAvg, barAvg
	logical				:: kcounter(2*grid,2*grid)
	real(kind=8)		:: vmrAlgSum, vmrCorSum, vmrBarSum
	real(kind=8)		:: bmicAlgSum, pmicAlgSum, lmicAlgSum
	real(kind=8)		:: bmicCorSum, pmicCorSum, lmicCorSum
	real(kind=8)		:: bmicBarSum, pmicBarSum, lmicBarSum
	
	integer				:: i, j
	
	
mic_algFile = "General/microbes_algdom.dat"
mic_corFile = "General/microbes_cordom.dat"
mic_barFile = "General/microbes_bardom.dat"
vmr_algFile = "General/vmr_algdom.dat"
vmr_corFile = "General/vmr_cordom.dat"
vmr_barFile = "General/vmr_bardom.dat"

bmicAlgSum = 0.0
pmicAlgSum = 0.0
lmicAlgSum = 0.0
bmicCorSum = 0.0
pmicCorSum = 0.0
lmicCorSum = 0.0
bmicBarSum = 0.0
pmicBarSum = 0.0
lmicBarSum = 0.0
vmrAlgSum = 0.0
vmrCorSum = 0.0
vmrBarSum = 0.0

open(unit=41,file=mic_algFile,status="unknown",position="append")
open(unit=42,file=mic_corFile,status="unknown",position="append")
open(unit=43,file=mic_barFile,status="unknown",position="append")
open(unit=44,file=vmr_algFile,status="unknown",position="append")
open(unit=45,file=vmr_corFile,status="unknown",position="append")
open(unit=46,file=vmr_barFile,status="unknown",position="append")

if (tim = 0) then
	write(41,*) "Time	Bacteria	Lytic	Lysogenic"
	write(42,*) "Time	Bacteria	Lytic	Lysogenic"
	write(43,*) "Time	Bacteria	Lytic	Lysogenic"
	write(44,*) "Time	VMR"
	write(45,*) "Time	VMR"
	write(46,*) "Time	VMR"
end if
	
kcounter = (kbact .eq. kalg)

algCount = count(kcounter)

kcounter = (kbact .eq. kcor)

corCount = count(kcounter)

kcounter = (kbact .eq. kbar)

barCount = count(kcounter)

do i = 1, 2*grid, 1
	
	do j = 1, 2*grid, 1
	
		if (kbact(i,j) .eq. kalg)
			bmicAlgSum = bmicAlgSum + bacteria(i,j)%totalpop
			lmicAlgSum = lmicAlgSum + lys(i,j)%totalpop
			pmicAlgSum = pmicAlgSum + phage(i,j)%totalpop
			vmrAlgSum = vmrAlgSum + phage(i,j)%totalpop/bacteria(i,j)%totalpop
		end if
	
		if (kbact(i,j) .eq. kcor)
			bmicCorSum = bmicCorSum + bacteria(i,j)%totalpop
			lmicCorSum = lmicCorSum + lys(i,j)%totalpop
			pmicCorSum = pmicCorSum + phage(i,j)%totalpop
			vmrCorSum = vmrCorSum + phage(i,j)%totalpop/bacteria(i,j)%totalpop
		end if
		
		if (kbact(i,j) .eq. kbar)
			bmicBarSum = bmicBarSum + bacteria(i,j)%totalpop
			lmicBarSum = lmicBarSum + lys(i,j)%totalpop
			pmicBarSum = pmicBarSum + phage(i,j)%totalpop
			vmrBarSum = vmrBarSum + phage(i,j)%totalpop/bacteria(i,j)%totalpop
		end if
		
	end do

end do
	
write(41,*) tim, bmicAlgSum/float(algCount), pmicAlgSum/float(algCount), lmicAlgSum/float(algCount)
write(42,*) tim, bmicCorSum/float(corCount), pmicCorSum/float(corCount), lmicCorSum/float(corCount)
write(43,*) tim, bmicBarSum/float(barCount), pmicBarSum/float(barCount), lmicBarSum/float(barCount)
write(44,*) tim, vmrAlgSum/float(algCount)
write(45,*) tim, vmrCorSum/float(corCount)
write(46,*) tim, vmrBarSum/float(corCount)

close(41)
close(42)
close(43)
close(44)
close(45)
close(46)

end subroutine




