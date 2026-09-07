! This file contains several fortran functions and subroutines
! to extract information about stored solve Jacobian.
! These fortran functions and subroutines can be called directly in C.
!

! Returns the dimension of the solve jacobian
function get_solve_jac_dim() result(res)
    use msvars
    use iso_c_binding, only : c_int
    integer(c_int) :: res
    if (solve_jac_stored) then   
       res = mdl%nfb
    else 
       res = 0
    endif
end function get_solve_jac_dim 

subroutine get_solve_jac_info(numfb_out, jac)
    use msvars
    use iso_c_binding, only : c_int, c_double
    integer(c_int), intent(out) :: numfb_out(mdl%nfb)
    real(c_double), intent(out) :: jac(mdl%nfb, mdl%nfb)

   numfb_out = mdl%numfb
   jac = solve_jac

   ! now we can deallocate the solve jacobian
   call clear_solve_jac

end subroutine get_solve_jac_info 

