! This file contains several fortran functions and subroutines
! to extract information about stored solve Jacobian.
! These fortran functions and subroutines can be called directly in C.
!

! Returns the dimension of the solve jacobian
function get_solve_jac_dim() result(res)
    use msvars
    use iso_c_binding
    integer(c_int) :: res
    if (solve_jac_stored) then   
       res = mdl%nfb
    else 
       res = 0
    endif
end function get_solve_jac_dim 
