module set_solve_opts
    use mws_type
    use kinds
    use solve_options_type
    type(solve_options), pointer, save :: options_set
    type(solve_options), target, save :: tmp_options

end module set_solve_opts

!
! the following functions are called from C and are therefore not 
! part of the module
!

subroutine init_set_solve_opts(mws_index, use_mws)
    use modelworkspaces
    use set_solve_opts
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: mws_index, use_mws

    if (use_mws /= 0) then
        ! directly modify the options in the mws
        options_set => mws_array(mws_index)%solve_opts
    else 
        ! create of copy of the options of the mws, and
        ! use these option. This is used when the user has 
        ! supplied options in the call of solve.
        tmp_options = mws_array(mws_index)%solve_opts
        options_set => tmp_options
    endif

end subroutine init_set_solve_opts

subroutine set_mode(mode)
    use set_solve_opts
    use iso_c_binding, only : c_char
    character(c_char), intent(in) :: mode
    options_set%mode = mode
end subroutine set_mode

subroutine set_maxit(maxit)
    use set_solve_opts
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: maxit
    options_set%maxit = maxit
end subroutine set_maxit
