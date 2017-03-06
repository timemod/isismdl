!
! This file contains subroutine donowt that does nothing. These
! subroutines can be used to defeat 'eigenwijze' optimalisation.
! The subroutines are placed in a separate file in order to ensure
! that the compiler will not know that the subroutine actually does not
! do anything.
!

subroutine donowt(dum1, dum2)
    use kinds
    real(kind = SOLVE_RKIND), intent(in) :: dum1, dum2
    ! intentionally do nothing

    real(kind = SOLVE_RKIND) :: x
    
    x = dum1 + dum2

   return
end subroutine donowt

