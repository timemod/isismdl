module get_solve_opts
    use mws_type
    use kinds
    use solve_options_type
    type(solve_options), pointer, save :: options
end module get_solve_opts

!
! the following functions are called from C and are therefore not 
! part of the module
!
subroutine init_get_solve_opts(mws_index)
    use modelworkspaces
    use get_solve_opts
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: mws_index

    options => mws_array(mws_index)%solve_opts
end subroutine init_get_solve_opts

function get_mode()
    use iso_c_binding, only : c_char
    use get_solve_opts
    character(c_char) :: get_mode
    get_mode = options%mode 
end function get_mode

function get_start()
    use iso_c_binding, only : c_char
    use get_solve_opts
    character(c_char) :: get_start
    get_start = options%start
end function get_start

function get_maxit()
    use iso_c_binding, only : c_int
    use get_solve_opts
    integer(c_int) :: get_maxit
    get_maxit = options%maxit
end function get_maxit
