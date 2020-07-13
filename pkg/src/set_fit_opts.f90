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

subroutine set_fit_cvgrel(cvgrel)
    use set_options
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: cvgrel
    options_set%fit%cvgrel = cvgrel
end subroutine set_fit_cvgrel

subroutine set_fit_zero_ca(zero_ca)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: zero_ca
    options_set%fit%zeroca  = zero_ca /= 0
end subroutine set_fit_zero_ca

subroutine set_fit_warn_ca(warn_ca)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: warn_ca
    options_set%fit%warnca  = warn_ca /= 0
end subroutine set_fit_warn_ca

subroutine set_fit_accurate_jac(acc_jac)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: acc_jac
    options_set%fit%accurate_jac  = acc_jac /= 0
end subroutine set_fit_accurate_jac

subroutine set_fit_zealous(zealous)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: zealous
    options_set%fit%zealous  = zealous /= 0
end subroutine set_fit_zealous

subroutine set_fit_warn_zero_row(warn_zero_row)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: warn_zero_row
    options_set%fit%warn_zero_row  = warn_zero_row /= 0
end subroutine set_fit_warn_zero_row

subroutine set_fit_warn_zero_col(warn_zero_col)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: warn_zero_col
    options_set%fit%warn_zero_col  = warn_zero_col /= 0
end subroutine set_fit_warn_zero_col


subroutine set_fit_repopt(repopt)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: repopt
    options_set%fit%repopt = repopt
end subroutine set_fit_repopt

subroutine set_fit_scale_method(scale_method)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: scale_method
    options_set%fit%scale_method = scale_method
end subroutine set_fit_scale_method

subroutine set_fit_dbgopts(prica, prijac, supsot)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: prica, prijac, supsot

    options_set%fit%prica  = prica /= 0
    options_set%fit%prijac = prijac /= 0
    options_set%fit%supsot = supsot /= 0
end subroutine set_fit_dbgopts

subroutine set_fit_svdtest_tol(tol)
    use set_options
    use iso_c_binding, only : c_double
    real(c_double), intent(in) :: tol
    options_set%fit%svdtest_tol = tol
end subroutine set_fit_svdtest_tol

subroutine set_fit_chkjac(chkjac)
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: chkjac
    options_set%fit%chkjac  = chkjac /= 0
end subroutine set_fit_chkjac
