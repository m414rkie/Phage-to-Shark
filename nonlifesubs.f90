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
	character, intent(in)					:: file
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

subroutine printbact(filea,fileb)

! Prints to a file in x-y-int-int format. Column 3 totalpop, Column 4 number of species

use globalvars

implicit none
	character*50, intent(in)					:: filea, fileb
	integer 									:: i, j
	
open(unit=16,file=trim(filea),status="replace",position="append")
open(unit=17,file=trim(fileb),status="replace",position="append")

do i = 1, 2*grid, 1
	do j = 1, 2*grid, 1
		write(16,*) i, j, bacteria(i,j)%totalpop, bacteria(i,j)%numspecies
		write(17,*) i, j, phage(i,j)%totalpop, phage(i,j)%numspecies
	end do
end do

close(16)
close(17)

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!