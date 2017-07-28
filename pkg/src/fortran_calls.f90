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

subroutine get_data_fortran(mws_index, nvar, ivar, ntime, jtb, jte, mat)
    ! store the values of model ivar for periods between jtb and 
    ! jte in mat.
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index, nvar, ivar(nvar), ntime, jtb, jte
    real(c_double), dimension(ntime, nvar), intent(out) :: mat

    call get_mdl_data(mws_array(mws_index), nvar, ivar, ntime, jtb, jte, mat)

end subroutine get_data_fortran

subroutine get_ca_fortran(mws_index, nca, ica, ntime, jtb, jte, mat)
    ! store the values of all model variables for periods between jtb and 
    ! jte in mat. the variables are stored in alphabetical order.
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index, nca, ica(nca), ntime, jtb, jte
    real(c_double), dimension(ntime, nca), intent(out) :: mat

    call get_ca(mws_array(mws_index), nca, ica, ntime, jtb, jte, mat)

end subroutine get_ca_fortran

subroutine get_fix_fit_fortran(mws_index, nvar, ivar, ntime, jtb, mat, &
                               fix_)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index, nvar,  ntime, jtb
    integer(c_int), intent(out) :: ivar(*)
    real(c_double), dimension(ntime, nvar), intent(out) :: mat
    integer(c_int), intent(in) :: fix_
        
    logical :: fix
    fix = fix_ /= 0
    call get_fix_fit(mws_array(mws_index), nvar, ivar, ntime, jtb, mat, &
                     fix)
end subroutine get_fix_fit_fortran

subroutine get_param_fortran(mws_index, ipar, value, length)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index, ipar, length
    real(c_double), dimension(length), intent(out) :: value
    call get_par(mws_array(mws_index), ipar, value, length)
end subroutine get_param_fortran

function set_param_fortran(mws_index, ipar, value, length)
    ! set a model parameter. Returns 0 on success, and 1 if length
    ! does not agree with the actual length of the parameter.
    use modelworkspaces
    use iso_c_binding
    integer(c_int) :: set_param_fortran
    integer(c_int), intent(in) :: mws_index, ipar, length
    real(c_double), dimension(length), intent(in) :: value
    set_param_fortran = set_par(mws_array(mws_index), ipar, value, length)
end function set_param_fortran

subroutine set_data_fortran(mws_index, nvar, ivar, ntime, jtb, jte, mat, icol, &
                            upd_mode)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index, nvar, ivar(*), ntime, jtb, jte, &
                                  icol(*), upd_mode
    real(c_double), dimension(ntime, nvar), intent(in) :: mat
    call set_data(mws_array(mws_index), nvar, ivar, ntime, jtb, jte, mat, &
                  icol, upd_mode)
end subroutine set_data_fortran

subroutine set_ca_fortran(mws_index, nvar, ivar, ntime, jtb, jte, mat, icol, &
                          upd_mode)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index, nvar, ivar(*), ntime, jtb, jte, &
                                  icol(*), upd_mode
    real(c_double), dimension(ntime, nvar), intent(in) :: mat
    call set_ca(mws_array(mws_index), nvar, ivar, ntime, jtb, jte, mat, &
                icol, upd_mode)
end subroutine set_ca_fortran

subroutine set_fix_fit_fortran(mws_index, nvar, ivar, ntime, jtb, jte, mat, &
                               icol, fix_, upd_mode)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index, nvar, ivar(*), ntime, jtb, jte, &
                                  icol(*), upd_mode
    real(c_double), dimension(ntime, nvar), intent(in) :: mat
    integer(c_int), intent(in) :: fix_
        
    logical fix
    fix = fix_ /= 0
    call set_fix_fit(mws_array(mws_index), nvar, ivar, ntime, jtb, jte, mat, &
                     icol, fix, upd_mode)
end subroutine set_fix_fit_fortran

subroutine set_rms_fortran(mws_index, var_index, value)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index, var_index
    real(c_double), intent(in) :: value
    logical :: error
    call set_rms(mws_array(mws_index), var_index, value, error)
end subroutine set_rms_fortran

function has_rms_fortran(mws_index)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index
    integer(c_int) :: has_rms_fortran

    logical :: has_rms_

    has_rms_ = has_rms(mws_array(mws_index))
    if (has_rms_) then
        has_rms_fortran =  1
    else 
        has_rms_fortran =  0
    endif
    
end function has_rms_fortran

subroutine get_rms_fortran(mws_index, values)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: mws_index
    real(c_double), intent(out) :: values(*)

    call get_rms(mws_array(mws_index), values)
end subroutine get_rms_fortran


! run all equations in the model starting a each time
subroutine mdlpas_fortran(mws_index, jtb, jte)
    use modelworkspaces
    use iso_c_binding
    use msvars
    use msslvq
    integer(c_int), intent(in) :: mws_index, jtb, jte

    integer :: i, j, errflg, ieq

    call msvarsinit(mws_array(mws_index))
        
    do j = jtb, jte
        do i = 1, mws_array(mws_index)%mdl%neq 
            ieq  = mws_array(mws_index)%mdl%order(i)
            if (ieq > 0) call solve_equation(ieq, .false., j, j, errflg)
        enddo
    end do
end subroutine mdlpas_fortran

subroutine run_eqn_fortran(mws_index, neq, eqnums, jtb, jte)
    use modelworkspaces
    use iso_c_binding
    use msvars
    use msslvq
    integer(c_int), intent(in) :: mws_index, neq, jtb, jte
    integer(c_int), intent(in) :: eqnums(neq)
    ! 
    ! run a number of equations
    !
    integer :: ieq, errflg
    
    call msvarsinit(mws_array(mws_index))
        
    do ieq = 1, neq
        call solve_equation(eqnums(ieq), .false., jtb, jte, errflg)
    end do
end subroutine run_eqn_fortran

subroutine solve_fortran(mws_index, jtb, jte, opts_present, error)
    use modelworkspaces
    use iso_c_binding
    use msvars
    use msimul
    use set_options
    use msimot
    integer(c_int), intent(in) :: mws_index, jtb, jte, opts_present
    integer(c_int), intent(out) :: error

    integer :: errflg
    type(solve_options), pointer :: opt

    if (opts_present /= 0) then
        ! Options specified by user in the call of solve.
        ! The options are present in module set_solve_opts.
        opt => options_set
    else 
        ! use options of the mws
        opt => mws_array(mws_index)%solve_opts
    endif
    call prepare_solve(mws_array(mws_index), opt, jtb, jte, errflg)
    call simul
    error = mws_array(mws_index)%simerr
    call msclear
end subroutine solve_fortran

subroutine filmdt_fortran(mws_index, jtb, jte, report_type)
    use modelworkspaces
    use msvars
    use msfill
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: mws_index, jtb, jte, report_type

    call msvarsinit(mws_array(mws_index))
    call fill_mdl_data(jtb, jte, report_type)

end subroutine filmdt_fortran

subroutine remove_mws_fortran(model_index)
    use modelworkspaces
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: model_index
    call clear_mws(mws_array(model_index))
    call remove_mws(model_index)
end subroutine remove_mws_fortran

subroutine clear_fit_fortran(model_index)
    use modelworkspaces
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: model_index
    call clear_fit(mws_array(model_index))
end subroutine clear_fit_fortran

subroutine clear_fix_fortran(model_index)
    use modelworkspaces
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: model_index
    call clear_fix(mws_array(model_index))
end subroutine clear_fix_fortran


subroutine set_test(mws_index, ivar, value)
    ! set convergence test value
    use modelworkspaces
    use iso_c_binding, only : c_int, c_double
    integer(c_int), intent(in) :: mws_index, ivar
    real(c_double), intent(in) :: value
    mws_array(mws_index)%test(ivar) = value
end subroutine set_test

function get_test(mws_index, ivar, alphabet)
    ! get convergence test value for variable ivar with or without
    ! alphabetical order
    use modelworkspaces
    use iso_c_binding, only : c_int, c_double
    real(c_double) :: get_test
    integer(c_int), intent(in) :: mws_index, ivar, alphabet

    integer iv
    if (alphabet == 0) then
        iv = ivar
    else 
        iv = mws_array(mws_index)%mdl%indexv(ivar)
    endif
    get_test = mws_array(mws_index)%test(iv)
end function get_test

function get_ftrelax(mws_index, iendex)
    ! get Fair-Taylor relaxtion factor for endogenous leads iendex
    ! (not in alphabetical order).
    use modelworkspaces
    use iso_c_binding, only : c_int, c_double
    real(c_double) :: get_ftrelax
    integer(c_int), intent(in) :: mws_index, iendex
    get_ftrelax = mws_array(mws_index)%ftrelax(iendex)
end function get_ftrelax

subroutine activate_equation(mws_index, eqnum)
    use modelworkspaces
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: mws_index, eqnum
    call activate_eq(mws_array(mws_index)%mdl,  eqnum)
end subroutine activate_equation

subroutine deactivate_equation(mws_index, eqnum)
    use modelworkspaces
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: mws_index, eqnum
    call deactivate_eq(mws_array(mws_index)%mdl,  eqnum)
end subroutine deactivate_equation

subroutine set_ftrelax(mws_index, ivar, value)
    ! set Fair-Taylor relaxation factor
    use modelworkspaces
    use iso_c_binding, only : c_int, c_double
    integer(c_int), intent(in) :: mws_index, ivar
    real(c_double), intent(in) :: value
    mws_array(mws_index)%ftrelax(ivar) = value
end subroutine set_ftrelax

subroutine clone_mws_fortran(model_index, model_index_clone)
    ! 
    ! read the model from the file
    !
    use modelworkspaces
    integer, intent(in)               :: model_index
    integer, intent(out)              :: model_index_clone

    model_index_clone = create_mws()
    if (model_index_clone < 0) return

    mws_array(model_index_clone) = mws_array(model_index)
    call clone_fit(mws_array(model_index_clone))
    call clone_fix(mws_array(model_index_clone))
    
end subroutine clone_mws_fortran
