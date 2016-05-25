subroutine get_max_lag_lead_fortran(model_index, maxlag, maxlead)
    use modelworkspaces
    integer, intent(in) :: model_index
    integer, intent(out) :: maxlag, maxlead
    maxlag = mws_array(model_index)%mdl%mxlag
    maxlead = mws_array(model_index)%mdl%mxlead
end subroutine get_max_lag_lead_fortran

subroutine set_period_fortran(model_index, start, end, freq, ier)
    use modelworkspaces
    integer, intent(in) :: model_index
    integer, dimension(2), intent(in) :: start, end
    integer, intent(in) :: freq
    logical, intent(out) :: ier
    call mws_setper(mws_array(model_index), start, end, freq, ier)
end subroutine set_period_fortran

subroutine get_data_all(mws_index, nvar, ntime, jtb, jte, mat)
    ! store the values of all model variables for periods between jtb and 
    ! jte in mat. the variables are stored in alphabetical order.
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index, nvar, ntime, jtb, jte
    real(c_double), dimension(ntime, nvar), intent(out) :: mat

    call get_mdl_data(mws_array(mws_index), mws_array(mws_index)%mdl%nrv,  &
                      mws_array(mws_index)%mdl%indexv, ntime, jtb, jte, mat)
end subroutine get_data_all

subroutine get_ca_fortran(mws_index, nca, ica, ntime, jtb, jte, mat)
    ! store the values of all model variables for periods between jtb and 
    ! jte in mat. the variables are stored in alphabetical order.
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index, nca, ica(nca), ntime, jtb, jte
    real(c_double), dimension(ntime, nca), intent(out) :: mat

    call get_ca(mws_array(mws_index), nca, ica, ntime, jtb, jte, mat)

end subroutine get_ca_fortran

subroutine set_data_fortran(mws_index, nvar, ivar, ntime, jtb, jte, mat, icol)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index, nvar, ivar(*), ntime, jtb, jte, &
                                  icol(*)
    real(c_double), dimension(ntime, nvar), intent(in) :: mat
    call set_data(mws_array(mws_index), nvar, ivar, ntime, jtb, jte, mat, icol)
end subroutine set_data_fortran

subroutine set_ca_fortran(mws_index, nvar, ivar, ntime, jtb, jte, mat, icol)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index, nvar, ivar(*), ntime, jtb, jte, &
                                  icol(*)
    real(c_double), dimension(ntime, nvar), intent(in) :: mat
    call set_ca(mws_array(mws_index), nvar, ivar, ntime, jtb, jte, mat, icol)
end subroutine set_ca_fortran

subroutine set_fix_fit_fortran(mws_index, nvar, ivar, ntime, jtb, jte, mat, &
                               icol, fix_)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index, nvar, ivar(*), ntime, jtb, jte, &
                                  icol(*)
    real(c_double), dimension(ntime, nvar), intent(in) :: mat
    integer(c_int), intent(in) :: fix_
        
    logical fix
    fix = fix_ /= 0
    call set_fix_fit(mws_array(mws_index), nvar, ivar, ntime, jtb, jte, mat, &
                     icol, fix)
end subroutine set_fix_fit_fortran

subroutine set_rms_fortran(mws_index, var_index, value)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index, var_index
    real(c_double), intent(in) :: value
    logical :: error
    call set_rms(mws_array(mws_index), var_index, value, error)
end subroutine set_rms_fortran

subroutine run_equations_fortran(mws_index, jtb, jte)
    use modelworkspaces
    use iso_c_binding
    use msvars
    use msslvq
    integer(c_int), intent(in) :: mws_index, jtb, jte

    integer :: i, errflg

    call msvarsinit(mws_array(mws_index))
    
    do i = 1, mws_array(mws_index)%mdl%neq 
        call solve_equation(i, .false., jtb, jte, errflg)
    end do
end subroutine run_equations_fortran

subroutine solve_fortran(mws_index, jtb, jte, error)
    use modelworkspaces
    use iso_c_binding
    use msvars
    use msimul
    integer(c_int), intent(in) :: mws_index, jtb, jte
    integer(c_int), intent(out) :: error

    integer :: i, errflg

    call prepare_solve(mws_array(mws_index), jtb, jte, errflg)
    call simul
    error = simerr
    call msclear
end subroutine solve_fortran

subroutine filmdt_fortran(mws_index, jtb, jte)
    use modelworkspaces
    use msvars
    use msfill
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index, jtb, jte

    call msvarsinit(mws_array(mws_index))
    call fill_mdl_data(jtb, jte, 2)

end subroutine filmdt_fortran


subroutine remove_mws_fortran(model_index)
    use modelworkspaces
    integer, intent(in) :: model_index
    call clear_mws(mws_array(model_index))
    call remove_mws(model_index)
end subroutine remove_mws_fortran

