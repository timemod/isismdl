module get_solve_opts
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

subroutine get_solve_options(imode, istart, maxit, maxmat, rlxspeed, rlxmin, &
           rlxmax, cstpbk, cnmtrx, xrelax, mratex, uplead)

    use get_solve_opts
    use iso_c_binding, only : c_int, c_double
    integer(c_int), intent(out):: imode, istart, maxit, maxmat, mratex, uplead
    real(c_double), intent(out):: rlxspeed, rlxmin, rlxmax, cstpbk, cnmtrx, &
                                  xrelax
    
    imode = get_imode()
    istart = get_istart()
    maxit = options%maxit
    maxmat = options%maxmat
    rlxspeed = options%rlxspeed
    rlxmin = options%rlxmin
    rlxmax = options%rlxmax
    cstpbk = options%cstpbk
    cnmtrx = options%cnmtrx
    xrelax = options%xrelax
    mratex = options%mratex
    uplead = logical2int(options%uplead)
end subroutine get_solve_options

subroutine get_solve_dbgopts(priter, prexen, jacprt, suptst, xsuptt, prscal)
    use get_solve_opts
    use iso_c_binding, only : c_int
    integer(c_int), intent(out):: priter, prexen, jacprt, suptst, xsuptt, &
                                  prscal
    priter = logical2int(options%priter)
    prexen = logical2int(options%prexen)
    jacprt = logical2int(options%jacprt)
    suptst = logical2int(options%suptst)
    xsuptt = logical2int(options%xsuptt)
    prscal = logical2int(options%prscal)
end subroutine get_solve_dbgopts
