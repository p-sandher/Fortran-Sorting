! Program iqsort uses interative quicksort to sort an array of integers
program iqsort
   use stackADT
   use intIO

   implicit none
   integer, allocatable, dimension(:) :: arr
   integer :: arrLen 
   real :: startTime, stopTime, executionTime


   write (*,*) "Iterative Quicksort Program"

   call readUnsorted(arr, arrLen)
   call cpu_time(startTime)
   call iterativeQsort(arr, arrLen)
   call cpu_time(stopTime)
   call writeSorted(arr, arrLen)

   deallocate(arr)

   executionTime = stopTime - startTime
   write (*,*) "Execution Time :", executionTime

   contains 
   
   ! Subroutine to sort an array using iterative quicksort
   subroutine iterativeQsort(arr, arrLen)
      implicit none
      integer, dimension(:), intent(inout) :: arr
      integer, intent(in) :: arrLen
      type(node), pointer :: stack => null()
      integer :: left, right, i, j, pivot

      ! Initialize stack
      call push(stack, arrLen)
      call push(stack, 1)
      
      do while (.not. isEmpty(stack))
         
         left = pop(stack)
         right = pop(stack)
   
         do while (left < right)
            ! Find the pivot
            i = left
            j = right
            pivot = arr((left + right) /2)
   
            do while (i <= j)

               ! Find integer on the left of the paritition that is less than pivot
               do while(arr(i) < pivot)
                  i = i + 1
               end do

               ! Find integer on the right of the paritition that is greater than pivot
               do while (pivot < arr(j))
                  j = j - 1
               end do
               ! Swap the two integers, if the left is less than or equal to right
               if (i <= j) then
                  call swap(arr(i), arr(j))
                  i = i + 1
                  j = j - 1
               end if
               
            end do
            
            ! Numbers on the left partitiion are pushed to the stack
            if (j > 0) then 
               call push(stack, j)
               call push(stack, left)
            end if 

            ! Numbers on the right partitiion are pushed to the stack
            if (i < right) then
               call push(stack, right)
               call push(stack, i)           
            end if

            left = i
         end do
      end do
      call clear(stack)
   
   end subroutine iterativeQsort

   ! subroutine to swap the integers of two variables
   subroutine swap(num1, num2)
      integer, intent(inout) :: num1, num2
      integer :: temp 
      temp = num1 
      num1 = num2 
      num2 = temp 
   end subroutine swap

end program iqsort
