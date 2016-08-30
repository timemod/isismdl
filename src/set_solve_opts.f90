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

subroutine set_mode(imode)
    use set_solve_opts
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: imode
    options_set%mode = MODES(imode)
end subroutine set_mode

subroutine set_start(istart)
    use set_solve_opts
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: istart
    options_set%start = START_OPTIONS(istart)
end subroutine set_start

subroutine set_maxit(maxit)
    use set_solve_opts
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: maxit
    options_set%maxit = maxit
end subroutine set_maxit

subroutine set_maxmat(maxmat)
    use set_solve_opts
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: maxmat
    options_set%maxmat = maxmat
end subroutine set_maxmat

subroutine set_rlxspeed(rlxspeed)
    use set_solve_opts
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: rlxspeed
    options_set%rlxspeed = rlxspeed
end subroutine set_rlxspeed

subroutine set_rlxmin(rlxmin)
    use set_solve_opts
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: rlxmin
    options_set%rlxmin = rlxmin
end subroutine set_rlxmin

subroutine set_rlxmax(rlxmax)
    use set_solve_opts
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: rlxmax
    options_set%rlxmax = rlxmax
end subroutine set_rlxmax

subroutine set_cstpbk(cstpbk)
    use set_solve_opts
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: cstpbk
    options_set%cstpbk = cstpbk
end subroutine set_cstpbk

subroutine set_cnmtrx(cnmtrx)
    use set_solve_opts
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: cnmtrx
    options_set%cnmtrx = cnmtrx
end subroutine set_cnmtrx

subroutine set_xrelax(xrelax)
    use set_solve_opts
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: xrelax
    options_set%xrelax = xrelax
end subroutine set_xrelax

subroutine set_mratex(mratex)
    use set_solve_opts
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: mratex
    options_set%mratex = mratex
end subroutine set_mratex

subroutine set_uplead(uplead)
    use set_solve_opts
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: uplead
    options_set%uplead = uplead /= 0
end subroutine set_uplead

subroutine set_solve_dbgopts(priter, prexen, jacprt, suptst, xsuptt, prscal)
    use set_solve_opts
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: priter, prexen, jacprt, suptst, xsuptt, prscal

    options_set%priter = priter /= 0
    options_set%prexen = prexen /= 0
    options_set%jacprt = jacprt /= 0
    options_set%suptst = suptst /= 0
    options_set%xsuptt = xsuptt /= 0
    options_set%prscal = prscal /= 0

end subroutine set_solve_dbgopts

subroutine set_erropt(erropt)
    use set_solve_opts
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: erropt
    options_set%erropt = erropt
end subroutine set_erropt
