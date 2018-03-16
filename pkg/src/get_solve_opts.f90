subroutine get_solve_options(imode, istart, maxit, maxmat, rlxspeed, rlxmin, &
           rlxmax, cstpbk, cnmtrx, xrelax, mratex, uplead, erropt, &
           repopt, ratrepopt, ratreport_rep, ratfullreport_rep, bktmax, xtfac, &
           svdtest_tol)

    use get_options
    use iso_c_binding, only : c_int, c_double
    integer(c_int), intent(out):: imode, istart, maxit, maxmat, mratex, &
                                  uplead,  erropt, repopt, ratrepopt, &
                                  ratreport_rep, ratfullreport_rep, bktmax
    real(c_double), intent(out):: rlxspeed, rlxmin, rlxmax, cstpbk, cnmtrx, &
                                  xrelax, xtfac, svdtest_tol
    
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
    erropt = options%erropt
    repopt = options%repopt
    ratrepopt = options%ratrepopt
    ratreport_rep = options%ratreport_rep
    ratfullreport_rep = options%ratfullreport_rep
    bktmax = options%bktmax
    xtfac = options%xtfac
    svdtest_tol = options%svdtest_tol
end subroutine get_solve_options

subroutine get_solve_dbgopts(priter, prexen, jacprt, suptst, xsuptt, prscal)
    use get_options
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
