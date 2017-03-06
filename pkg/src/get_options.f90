module get_options
    use mws_type
    use kinds
    use solve_options_type
    type(solve_options), pointer, save :: options
    
    contains
        integer function get_imode()
            integer :: i
            do i = 1, size(MODES) 
                if (options%mode == MODES(i)) then
                    get_imode = i
                    return
                endif
            end do
            get_imode = -1
            call rexit("Internal error: mode not found")
        end function get_imode

        integer function get_istart()
            integer :: i
            do i = 1, size(START_OPTIONS) 
                if (options%start == START_OPTIONS(i)) then
                    get_istart = i
                    return
                endif
            end do
            get_istart = -1
            call rexit("Internal error: start option not found")
        end function get_istart

        integer function logical2int(lvalue)
            logical, intent(in) :: lvalue
            if (lvalue) then
                logical2int = 1
            else 
                logical2int = 0
            endif
        end function logical2int

end module get_options

!
! the following functions is called from C and is therefore not 
! part of the module
!
subroutine init_get_options(mws_index)
    use modelworkspaces
    use get_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: mws_index

    options => mws_array(mws_index)%solve_opts
end subroutine init_get_options
