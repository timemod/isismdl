subroutine set_mode(imode)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: imode
    options_set%mode = MODES(imode)
end subroutine set_mode

subroutine set_start(istart)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: istart
    options_set%start = START_OPTIONS(istart)
end subroutine set_start

subroutine set_maxit(maxit)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: maxit
    options_set%maxit = maxit
end subroutine set_maxit

subroutine set_maxmat(maxmat)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: maxmat
    options_set%maxmat = maxmat
end subroutine set_maxmat

subroutine set_rlxspeed(rlxspeed)
    use set_options
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: rlxspeed
    options_set%rlxspeed = rlxspeed
end subroutine set_rlxspeed

subroutine set_rlxmin(rlxmin)
    use set_options
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: rlxmin
    options_set%rlxmin = rlxmin
end subroutine set_rlxmin

subroutine set_rlxmax(rlxmax)
    use set_options
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: rlxmax
    options_set%rlxmax = rlxmax
end subroutine set_rlxmax

subroutine set_cstpbk(cstpbk)
    use set_options
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: cstpbk
    options_set%cstpbk = cstpbk
end subroutine set_cstpbk

subroutine set_cnmtrx(cnmtrx)
    use set_options
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: cnmtrx
    options_set%cnmtrx = cnmtrx
end subroutine set_cnmtrx

subroutine set_xrelax(xrelax)
    use set_options
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: xrelax
    options_set%xrelax = xrelax
end subroutine set_xrelax

subroutine set_mratex(mratex)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: mratex
    options_set%mratex = mratex
end subroutine set_mratex

subroutine set_uplead(uplead)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: uplead
    options_set%uplead = uplead /= 0
end subroutine set_uplead

subroutine set_repopt(repopt)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: repopt
    options_set%repopt = repopt
end subroutine set_repopt

subroutine set_ratrepopt(ratrepopt)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: ratrepopt
    options_set%ratrepopt = ratrepopt
end subroutine set_ratrepopt

subroutine set_ratreport_rep(rep, repfull)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: rep, repfull
    options_set%ratreport_rep     = rep
    options_set%ratfullreport_rep = repfull
end subroutine set_ratreport_rep

subroutine set_bktmax(bktmax)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: bktmax
    options_set%bktmax = bktmax
end subroutine set_bktmax

subroutine set_xtfac(xtfac)
    use set_options
    use iso_c_binding, only : c_double
    real(c_double), intent(in) ::xtfac
    options_set%xtfac = xtfac
end subroutine set_xtfac

subroutine set_solve_dbgopts(priter, prexen, jacprt, suptst, xsuptt, prscal)
    use set_options
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
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: erropt
    options_set%erropt = erropt
end subroutine set_erropt
