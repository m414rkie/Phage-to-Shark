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
	real			:: avgcoral, avgfish, avgbact
	real			:: phagelysratio
	character*50	:: corfile, fishfile, lysfile	
	character*50	:: bactfile, genfile, phagefile
	character*50	:: kfile, percfile
	character*50	:: corpath, fishpath, bactpath
	character*50	:: genpath, phagepath, lyspath
	character*50	:: kpath
	character*50	:: totbactfile, totfishfile
	character*50	:: avgcoralfile, phlyratiofile
	character*50	:: micropopfile, microspecfile
	character*50	:: cortotfile
	
! Data manipulation
avgcoral = sum(coral)/(real(grid)**2)
avgfish = sum(fish)/(real(grid)**2)
avgbact = sum(bacteria%totalpop)/(real(2*grid)**2)
phagelysratio = real(sum(phage%totalpop))/real(sum(lys%totalpop))
	
! Format statements
50 format ("Coral/coraltime",1i2,".dat")
51 format ("Fish/fishtime",1i2,".dat")
52 format ("Bacteria/bacttime",1i2,".dat")
53 format ("Phage/phagetime",1i2,".dat")
54 format ("General/kgrid",1i2,".dat")
55 format ("Lys/lystime",1i2,".dat")
56 format ("General/perctime.dat")

! File path statements
corpath   = "~/Desktop/Phage2Shark/Coral"
fishpath  = "~/Desktop/Phage2Shark/Fish"
bactpath  = "~/Desktop/Phage2Shark/Bacteria"
genpath   = "~/Desktop/Phage2Shark/General"
phagepath = "~/Desktop/Phage2Shark/Phage"
lyspath   = "~/Desktop/Phage2Shark/Lys"
kpath	  = "~/Desktop/Phage2Shark/Kbact"

call dircheck(corpath)
call dircheck(fishpath)
call dircheck(bactpath)
call dircheck(genpath)
call dircheck(phagepath)
call dircheck(lyspath)
call dircheck(kpath)

percfile	  = "General/perctime.dat"
totbactfile   = "General/bacttime.dat"
totfishfile   = "General/fishtottime.dat"
avgcoralfile  = "General/avgcortime.dat"
phlyratiofile = "General/phagelysratio.dat"
micropopfile  = "General/microbepops.dat"
microspecfile = "General/microbespecs.dat"
cortotfile 	  = "General/cortottime.dat"

if (tim .eq. 0) then
	corfile   = "Coral/coraltime00.dat"
	fishfile  = "Fish/fishtime00.dat"
	lysfile   = "Lys/lystime00.dat"
	bactfile  = "Bacteria/bacttime00.dat"
	phagefile = "Phage/phagetime00.dat"
	kfile 	  = "Kbact/ktime00.dat"
else if (tim .ne. 0) then
	write(corfile,50) tim
	write(fishfile,51) tim
	write(lysfile,55) tim
	write(bactfile,52) tim
	write(phagefile,53) tim
	write(kfile,54) tim
end if
	
	
	
call printtofile(fish,grid,fishfile)
call printtofile(coral,grid,corfile)
call printtofile(kbact,2*grid,kfile)	
	
open(unit=15,file=percfile,status="unknown",position="append")
open(unit=20,file=totbactfile,status="unknown",position="append")
open(unit=21,file=totfishfile,status="unknown",position="append")
open(unit=22,file=avgcoralfile,status="unknown",position="append")
open(unit=23,file=phlyratiofile,status="unknown",position="append")
open(unit=24,file=micropopfile,status="unknown",position="append")
open(unit=25,file=microspecfile,status="unknown",position="append")
open(unit=26,file=cortotfile,status="unknown",position="append")
	
write(15,*) tim, percentcor(grid)
write(20,*) tim, sum(bacteria%totalpop)
write(21,*) tim, sum(fish)
write(22,*) tim, avgcoral
write(23,*) tim, phagelysratio
write(24,*) tim, sum(bacteria%totalpop), sum(phage%totalpop), sum(lys%totalpop)
write(25,*) tim, sum(bacteria%numspecies), sum(phage%numspecies), sum(lys%numspecies)
write(26,*) tim, sum(coral)

close(15)
close(20)	
close(21)	
close(22)	
close(23)	
close(24)	
close(25)	
close(26)		
	
end subroutine
	


	
	
	




