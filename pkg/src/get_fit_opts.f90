subroutine get_fit_options(maxiter, cvgabs, mkdcrt, cvgrel, zero_ca, warn_ca, &
                           repopt, svdtest_tol, accurate_jac, zealous, scale_method, &
                           warn_zero_col)
    use get_options
    use iso_c_binding, only : c_int, c_double
    integer(c_int), intent(out):: maxiter, repopt, zero_ca, warn_ca, &
                                  accurate_jac, zealous, scale_method, &
                                  warn_zero_col
    real(c_double), intent(out):: cvgabs, mkdcrt, cvgrel, svdtest_tol

    maxiter = options%fit%maxiter
    cvgabs = options%fit%cvgabs
    mkdcrt = options%fit%mkdcrt
    cvgrel = options%fit%cvgrel
    repopt = options%fit%repopt
    zero_ca = logical2int(options%fit%zeroca)
    warn_ca = logical2int(options%fit%warnca)
    svdtest_tol = options%fit%svdtest_tol
    accurate_jac = logical2int(options%fit%accurate_jac)
    zealous = logical2int(options%fit%zealous)
    scale_method = options%fit%scale_method
    warn_zero_col = logical2int(options%fit%warn_zero_col)

end subroutine get_fit_options

subroutine get_fit_dbgopts(prica, prijac, supsot)
    use get_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(out) :: prica, prijac, supsot
    prica = logical2int(options%fit%prica)
    prijac = logical2int(options%fit%prijac)
    supsot = logical2int(options%fit%supsot)
end subroutine get_fit_dbgopts
