! Module handles are I/O, such as reading and writing into files.
module intIO
   contains
   subroutine readUnsorted(arr, arrLen)
      implicit none
      character(len=500) :: filename, line
      integer, allocatable, dimension(:), intent(out) :: arr
      logical :: lexist
      integer :: IOStatus, i
      integer, intent(out) :: arrLen

      arrLen = 0

      ! Prompt user for valid file name
      do 
         write (*,*) "Enter the file name with unsorted numbers: "
         read(*,'(A)') filename
         inquire(file=filename, exist=lexist)
         if (lexist) then
            exit
         end if
      end do

      ! Open the file
      open(unit=10, file=filename, status="old", action="read")
      
      ! Count how many numbers are in the file
      do
         read(10, '(A)', iostat=IOStatus) line
         if (IOStatus /= 0) then
            exit  
         end if
         arrLen = arrLen + 1
      end do
      
      close(10)

      ! Allocate memory in array
      allocate(arr(arrLen))

      ! Store file data in array
      open(unit=10, file=filename, status="old", action="read")

      do i = 1,arrLen
         read(10, *) arr(i) 
      end do
      
      close(10)

   end subroutine readUnsorted

   subroutine writeSorted(arr, arrLen)
      character (len=256) :: fname, userResponse, status
      logical :: lexist
      integer, intent(in) :: arr(:)
      integer, intent(in) :: arrLen
      integer :: i

      
      fname = 'sortedNUM.txt'
      ! Open file and if default file is exists, then user is prompted to overwrite or give for a different filename
      do
         inquire(file=fname, exist=lexist)

         if (.not. lexist) then
            status = 'new'
            exit 
         else 
            write(*,*) 'File "', trim(fname), '" already exists. Type "y" to overwrite. Type "n" to choose a different file.'
            read (*,'(A)') userResponse
            if (userResponse == 'y' .or. userResponse == 'Y') then 
               status = 'replace'
               exit
            else
               write (*,*) 'Enter a filename to store sorted numbers: '
               read (*,'(A)') fname
               status = 'new'
            end if

            
         end if

      end do

      open(unit=9,file=fname,status=status,action='write')

      do i = 1, arrLen
         write(9, '(I0)') arr(i)
      end do
      close(9)
      write(*,*) "Sorting is complete and sorted in the file called: ", trim(fname)

   end subroutine 


end module intIO

