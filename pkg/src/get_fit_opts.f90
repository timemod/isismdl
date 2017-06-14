subroutine get_fit_options(maxiter, cvgabs, mkdcrt, repopt)
    use get_options
    use iso_c_binding, only : c_int, c_double
    integer(c_int), intent(out):: maxiter, repopt
    real(c_double), intent(out):: cvgabs, mkdcrt

    maxiter = options%fit%maxiter
    cvgabs = options%fit%cvgabs
    mkdcrt = options%fit%mkdcrt
    repopt = options%fit%repopt

end subroutine get_fit_options

subroutine get_fit_dbgopts(prica, prijac, supsot)
    use get_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(out) :: prica, prijac, supsot
    prica = logical2int(options%fit%prica)
    prijac = logical2int(options%fit%prijac)
    supsot = logical2int(options%fit%supsot)
end subroutine get_fit_dbgopts
