! Module that holds subprograms related to the stack
module stackADT
   type :: node 
         integer :: data 
         type(node), pointer :: next => null()
   end type node

   contains 
   ! Function that pops number from the stack
   integer function pop(stack)
      type (node), pointer, intent(inout) :: stack
      type (node), pointer :: popNode 

      if (associated(stack)) then
         popNode => stack 
         pop = popNode%data
         stack => stack%next
         deallocate(popNode) 
      end if
   end function pop

   ! Subroutine that pushes a number onto the stack
   subroutine push(stack, value)
      type(node), pointer, intent(inout) :: stack 

      integer, intent(in) :: value 
      type(node), pointer :: newNode

      allocate(newNode)

      newNode%data = value 
      newNode%next => stack
      stack =>newNode
   end subroutine push 

   ! Subroutine that clears the stack
   subroutine clear(stack)
      type(node), pointer, intent(inout) :: stack
      type(node), pointer ::nextNode

      do while (associated(stack))
         nextNode => stack%next 
         deallocate(nextNode)
         stack =>nextNode 
      end do
   end subroutine clear
   
   ! Subroutine that checks if the stack is empty
   logical function isEmpty(stack)
      type(node), pointer, intent(in) :: stack
      if (associated(stack)) then
         isEmpty = .FALSE.
      else 
         isEmpty = .TRUE.
      end if
   end function isEmpty

end module stackADT


