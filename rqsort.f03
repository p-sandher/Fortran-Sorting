! Program rqsort uses recursive quicksort to sort an array of integers
! Online reference for implementing recursive qsort
! https://www.geeksforgeeks.org/quick-sort/
program rqsort 
   use intIO

   implicit none
   integer :: start, arrLen
   integer, allocatable, dimension(:) :: arr
   real :: startTime, stopTime, executionTime
   start = 1

   write (*,*) "Recursive Quicksort Program"

   call readUnsorted(arr, arrLen)
   call cpu_time(startTime)
   call recursiveQsort(arr, start, arrLen)
   call cpu_time(stopTime)
   call writeSorted(arr, arrLen)

   deallocate(arr)

   executionTime = stopTime - startTime
   write (*,*) "Execution Time :", executionTime

   contains 
   
   ! Subroutine to sort an array of integers using recursive QuickSort
   recursive subroutine recursiveQsort(arr, smaller, greater)
      integer, intent(inout) :: smaller, greater
      integer, dimension(:), intent(inout) :: arr
      integer :: pivot, right, left

      if(smaller < greater) then
         ! Find the pivot number
         pivot = split(arr, smaller, greater)
         left = pivot-1
         ! sort the left side of the partition
         call recursiveQsort(arr, smaller, left)
         right = pivot+1
         ! sort the right side of the partition
         call recursiveQsort(arr, right, greater)
      end if

   end subroutine recursiveQsort

   ! Function uses the pivot to partition and sort integers to one side
   integer function split(arr, smaller, greater)
      integer :: pivot, i, j
      integer, intent(inout) :: smaller, greater !check
      integer, dimension(:), intent(inout) :: arr

      i = smaller - 1
      j = smaller 
      pivot = arr(greater)
      ! Iterate and move integers smaller than the pivot and moves to the left of the pivot
      do j = smaller, greater
         if (arr(j) < pivot) then
            i = i + 1
            call swap(arr(i), arr(j))
         end if
      end do

      call swap(arr(i+1), arr(greater))
      split = i + 1

   end function 

   ! Subroutine to swap the integers of two variables
   subroutine swap(num1, num2)
      integer, intent(inout) :: num1, num2
      integer :: temp 

      temp = num1 
      num1 = num2 
      num2 = temp 
   end subroutine swap

end program rqsort 


! is it fine that i used allocatable
! https://craftofcoding.wordpress.com/2018/01/26/arrays-and-memory-in-fortran/
! how to cite 
! do i have to create a subprogram for timing, am i incorrect for doing 
! real :: startTime, stopTime

!    call cpu_time(startTime)

! call cpu_time(stopTime)
! how to use compiler flags
! should i make arrLen a constant
! can there be a newline at the end
! mod file?
! Stack, no intent in and out