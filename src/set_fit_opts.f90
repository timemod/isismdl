subroutine set_fit_maxit(maxiter)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: maxiter
    options_set%fit%maxiter = maxiter
end subroutine set_fit_maxit

subroutine set_fit_cvgabs(cvgabs)
    use set_options
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: cvgabs
    options_set%fit%cvgabs = cvgabs
end subroutine set_fit_cvgabs

subroutine set_fit_mkdcrt(mkdcrt)
    use set_options
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: mkdcrt
    options_set%fit%mkdcrt =  mkdcrt
end subroutine set_fit_mkdcrt

subroutine set_fit_repopt(repopt)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: repopt
    options_set%fit%repopt = repopt
end subroutine set_fit_repopt


subroutine set_fit_dbgopts(prica, prijac, supsot)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: prica, prijac, supsot

    options_set%fit%prica  = prica /= 0
    options_set%fit%prijac = prijac /= 0
    options_set%fit%supsot = supsot /= 0
end subroutine set_fit_dbgopts
