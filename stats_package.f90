! Program written to accept input of files and perform a correlation 
!  analysis on the contents as well as find the slope of each, and 
!  average slope. Written for Phage2Shark project.

! Accepts files with data written as x y. Headers acceptable.

! Jon Parsons
! 1-29-2019

program statistics

implicit none
	integer						:: file_count, dat_len
	real, allocatable			:: dat_array(:,:)
	integer						:: i, j, EOF
	character*25, allocatable   :: fname_list(:)

write(*,*) "All files must have the same number of data points and be in the same directory as this program."

Write(*,*) "Number of files being analysed?"
read(*,*) file_count

allocate(fname_list(file_count))

do i = 1, file_count, 1
	write(*,*) "Enter the name of file", i
	read(*,*) fname_list(i)
end do

open(unit=15, file=trim(fname_list(1)), status="old")
read(15,*)

dat_len = 0

do
	read(15,*, iostat=EOF)
	if (EOF .ne. 0) exit
	dat_len = dat_len + 1
end do

close(15)

write(*,*) "Number of data points:", dat_len

allocate(dat_array(2*file_count,dat_len))

write(*,*) "Reading data"

do i = 1, file_count, 1

	write(*,*) "Reading", trim(fname_list(i))
	
	open(unit=15, file=trim(fname_list(i)), status="old")
	read(15,*)
	
	do j = 1, dat_len, 1
		read(15,*) dat_array(i,j), dat_array(i+1,j)
	end do
	
end do

write(*,*) "Data entry complete"
write(*,*) "Finding Slopes"

call slope(file_count,dat_len,dat_array,fname_list)



end program

subroutine slope(numFil,numEnt,arrin,nameIn)
! Finds the slope each file using the forward finite difference method
! Determined for ten steps

implicit none
	integer,intent(in)		:: numFil,numEnt
	real,intent(in)			:: arrin(numFil*2,numEnt)
	character*25,intent(in) :: nameIn(numFil)
	integer					:: i, j, tenth
	real					:: f, x, y
	
	f(x,y) = 0.1*(y-x)
	
open(unit=15,file="Slopes.dat",status="replace",position="append")

do i = 1, numFil, 1
	write(15,'(A15,1x)', advance="no") trim(nameIn(i))
end do

tenth = floor(numEnt/10.0)

do i = 11, numEnt, 10
	do j = 1, numFil, 1
		write(15,'(i2,1F6.3)', advance = "no") arrin(1,i), f(arrin(j+1,i),arrin(j+1,i-10))
	end do
end do



end subroutine


























