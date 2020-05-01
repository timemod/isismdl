subroutine get_max_lag_lead_fortran(model_index, maxlag, maxlead)
    use modelworkspaces
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: model_index
    integer(c_int), intent(out) :: maxlag, maxlead
    maxlag = mws_array(model_index)%mdl%mxlag
    maxlead = mws_array(model_index)%mdl%mxlead
end subroutine get_max_lag_lead_fortran

function set_period_fortran(model_index, start, end, freq)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: model_index
    integer(c_int), dimension(2), intent(in) :: start, end
    integer(c_int), intent(in) :: freq
    integer(c_int) :: set_period_fortran
    logical :: error
    call mws_setper(mws_array(model_index), start, end, freq, error)
    if (error) then
        set_period_fortran =  1
    else 
        set_period_fortran =  0
    endif
end function set_period_fortran

subroutine get_data_fortran(model_index, nvar, ivar, ntime, jtb, jte, mat)
    ! store the values of model ivar for periods between jtb and 
    ! jte in mat.
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: model_index, nvar, ivar(nvar), ntime, jtb, jte
    real(c_double), dimension(ntime, nvar), intent(out) :: mat

    call get_mdl_data(mws_array(model_index), nvar, ivar, ntime, jtb, jte, mat)

end subroutine get_data_fortran

subroutine get_ca_fortran(model_index, nca, ica, ntime, jtb, jte, mat)
    ! store the values of all model variables for periods between jtb and 
    ! jte in mat. the variables are stored in alphabetical order.
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: model_index, nca, ica(nca), ntime, jtb, jte
    real(c_double), dimension(ntime, nca), intent(out) :: mat

    call get_ca(mws_array(model_index), nca, ica, ntime, jtb, jte, mat)

end subroutine get_ca_fortran

subroutine get_fix_fit_fortran(model_index, nvar, ivar, ntime, jtb, mat, &
                               fix_)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: model_index, nvar,  ntime, jtb
    integer(c_int), intent(out) :: ivar(*)
    real(c_double), dimension(ntime, nvar), intent(out) :: mat
    integer(c_int), intent(in) :: fix_
        
    logical :: fix
    fix = fix_ /= 0
    call get_fix_fit(mws_array(model_index), nvar, ivar, ntime, jtb, mat, &
                     fix)
end subroutine get_fix_fit_fortran

subroutine get_param_fortran(model_index, ipar, value, length)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: model_index, ipar, length
    real(c_double), dimension(length), intent(out) :: value
    call get_par(mws_array(model_index), ipar, value, length)
end subroutine get_param_fortran

function set_param_fortran(model_index, ipar, value, length)
    ! set a model parameter. Returns 0 on success, and 1 if length
    ! does not agree with the actual length of the parameter.
    use modelworkspaces
    use iso_c_binding
    integer(c_int) :: set_param_fortran
    integer(c_int), intent(in) :: model_index, ipar, length
    real(c_double), dimension(length), intent(in) :: value
    set_param_fortran = set_par(mws_array(model_index), ipar, value, length)
end function set_param_fortran

subroutine set_data_fortran(model_index, nvar, ivar, ntime, jtb, jte, mat, icol, &
                            upd_mode)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: model_index, nvar, ivar(*), ntime, jtb, jte, &
                                  icol(*), upd_mode
    real(c_double), dimension(ntime, nvar), intent(in) :: mat
    call set_data(mws_array(model_index), nvar, ivar, ntime, jtb, jte, mat, &
                  icol, upd_mode)
end subroutine set_data_fortran

subroutine set_ca_fortran(model_index, nvar, ivar, ntime, jtb, jte, mat, icol, &
                          upd_mode)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: model_index, nvar, ivar(*), ntime, jtb, jte, &
                                  icol(*), upd_mode
    real(c_double), dimension(ntime, nvar), intent(in) :: mat
    call set_ca(mws_array(model_index), nvar, ivar, ntime, jtb, jte, mat, &
                icol, upd_mode)
end subroutine set_ca_fortran

subroutine set_fix_fit_fortran(model_index, nvar, ivar, ntime, jtb, jte, mat, &
                               icol, fix_, upd_mode)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: model_index, nvar, ivar(*), ntime, jtb, jte, &
                                  icol(*), upd_mode
    real(c_double), dimension(ntime, nvar), intent(in) :: mat
    integer(c_int), intent(in) :: fix_
        
    logical fix
    fix = fix_ /= 0
    call set_fix_fit(mws_array(model_index), nvar, ivar, ntime, jtb, jte, mat, &
                     icol, fix, upd_mode)
end subroutine set_fix_fit_fortran

subroutine set_rms_fortran(model_index, var_index, value)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: model_index, var_index
    real(c_double), intent(in) :: value
    logical :: error
    call set_rms(mws_array(model_index), var_index, value, error)
end subroutine set_rms_fortran

function has_rms_fortran(model_index)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: model_index
    integer(c_int) :: has_rms_fortran

    logical :: has_rms_

    has_rms_ = has_rms(mws_array(model_index))
    if (has_rms_) then
        has_rms_fortran =  1
    else 
        has_rms_fortran =  0
    endif
    
end function has_rms_fortran

subroutine get_rms_fortran(model_index, values)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: model_index
    real(c_double), intent(out) :: values(*)

    call get_rms(mws_array(model_index), values)
end subroutine get_rms_fortran


! run all equations in the model starting a each time
subroutine mdlpas_fortran(model_index, jtb, jte)
    use modelworkspaces
    use iso_c_binding
    use msvars
    use msslvq
    integer(c_int), intent(in) :: model_index, jtb, jte

    integer :: i, j, errflg, ieq

    call msvarsinit(mws_array(model_index))
        
    do j = jtb, jte
        do i = 1, mws_array(model_index)%mdl%neq 
            ieq  = mws_array(model_index)%mdl%order(i)
            if (ieq > 0) call solve_equation(ieq, .false., j, j, errflg)
        enddo
    end do
end subroutine mdlpas_fortran

subroutine run_eqn_fortran(model_index, neq, eqnums, jtb, jte)
    use modelworkspaces
    use iso_c_binding
    use msvars
    use msslvq
    integer(c_int), intent(in) :: model_index, neq, jtb, jte
    integer(c_int), intent(in) :: eqnums(neq)
    ! 
    ! run a number of equations
    !
    integer :: ieq, errflg
    
    call msvarsinit(mws_array(model_index))
        
    do ieq = 1, neq
        call solve_equation(eqnums(ieq), .false., jtb, jte, errflg)
    end do
end subroutine run_eqn_fortran

subroutine solve_fortran(model_index, jtb, jte, opts_present, error)
    use modelworkspaces
    use iso_c_binding
    use msvars
    use msimul
    use set_options
    use msimot
    integer(c_int), intent(in) :: model_index, jtb, jte, opts_present
    integer(c_int), intent(out) :: error

    integer :: errflg
    type(solve_options), pointer :: opt

    if (opts_present /= 0) then
        ! Options specified by user in the call of solve.
        ! The options are present in module set_solve_opts.
        opt => options_set
    else 
        ! use options of the mws
        opt => mws_array(model_index)%solve_opts
    endif
    call prepare_solve(mws_array(model_index), opt, jtb, jte, errflg)
    call simul
    error = mws_array(model_index)%simerr
    call msclear
end subroutine solve_fortran

subroutine filmdt_fortran(model_index, jtb, jte, report_type)
    use modelworkspaces
    use msvars
    use msfill
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: model_index, jtb, jte, report_type

    call msvarsinit(mws_array(model_index))
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


subroutine set_test(model_index, ivar, value)
    ! set convergence test value
    use modelworkspaces
    use iso_c_binding, only : c_int, c_double
    integer(c_int), intent(in) :: model_index, ivar
    real(c_double), intent(in) :: value
    mws_array(model_index)%test(ivar) = value
end subroutine set_test

function get_test(model_index, ivar, alphabet)
    ! get convergence test value for variable ivar with or without
    ! alphabetical order
    use modelworkspaces
    use iso_c_binding, only : c_int, c_double
    real(c_double) :: get_test
    integer(c_int), intent(in) :: model_index, ivar, alphabet

    integer iv
    if (alphabet == 0) then
        iv = ivar
    else 
        iv = mws_array(model_index)%mdl%indexv(ivar)
    endif
    get_test = mws_array(model_index)%test(iv)
end function get_test

function get_ftrelax(model_index, iendex)
    ! get Fair-Taylor relaxtion factor for endogenous leads iendex
    ! (not in alphabetical order).
    use modelworkspaces
    use iso_c_binding, only : c_int, c_double
    real(c_double) :: get_ftrelax
    integer(c_int), intent(in) :: model_index, iendex
    get_ftrelax = mws_array(model_index)%ftrelax(iendex)
end function get_ftrelax

subroutine activate_equation(model_index, eqnum)
    use modelworkspaces
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: model_index, eqnum
    call activate_eq(mws_array(model_index)%mdl,  eqnum)
end subroutine activate_equation

subroutine deactivate_equation(model_index, eqnum)
    use modelworkspaces
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: model_index, eqnum
    call deactivate_eq(mws_array(model_index)%mdl,  eqnum)
end subroutine deactivate_equation

subroutine set_ftrelax(model_index, ivar, value)
    ! set Fair-Taylor relaxation factor
    use modelworkspaces
    use iso_c_binding, only : c_int, c_double
    integer(c_int), intent(in) :: model_index, ivar
    real(c_double), intent(in) :: value
    mws_array(model_index)%ftrelax(ivar) = value
end subroutine set_ftrelax

function has_free_mws_fortran()
    ! 
    ! finds the index of  the model from the file
    !
    use modelworkspaces
    use iso_c_binding, only : c_int
    integer(c_int) :: has_free_mws_fortran
    integer i
    i = find_free_mws()

    if (i >= 1) then
        has_free_mws_fortran =  1
    else 
        has_free_mws_fortran =  0
    endif

end function has_free_mws_fortran

function clone_mws_fortran(model_index)
    ! 
    ! read the model from the file
    !
    use modelworkspaces
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: model_index
    integer(c_int) :: clone_mws_fortran
    integer ::  model_index_clone

    model_index_clone = create_mws()
    if (model_index_clone < 0) return

    mws_array(model_index_clone) = mws_array(model_index)
    call clone_fit(mws_array(model_index_clone))
    call clone_fix(mws_array(model_index_clone))

    clone_mws_fortran = model_index_clone
    
end function clone_mws_fortran

subroutine set_dbgeqn_fortran(model_index, dbgeqn_)
    ! sets dbgeqn
    use modelworkspaces
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: model_index, dbgeqn_

    logical :: dbgeqn

    dbgeqn = dbgeqn_ /= 0
    mws_array(model_index)%dbgeqn = dbgeqn
end subroutine set_dbgeqn_fortran

function get_dbgeqn_fortran(model_index)
    ! gets dbgeqn
    use modelworkspaces
    use iso_c_binding, only : c_int
    integer(c_int) :: get_dbgeqn_fortran
    integer(c_int), intent(in) :: model_index
    
    if (mws_array(model_index)%dbgeqn) then
        get_dbgeqn_fortran = 1
    else 
        get_dbgeqn_fortran = 0
    endif
end function get_dbgeqn_fortran

subroutine set_jc_fortran(model_index, jc)
    ! set jc (index of last solve period relative to the model period)
    use modelworkspaces
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: model_index, jc
    mws_array(model_index)%jc = jc
end subroutine set_jc_fortran

function get_jc_fortran(model_index)
    ! get the index of the current period just solved
    ! relative to the start of the solve period.
    ! is -1 before the first solve
    use modelworkspaces
    use iso_c_binding, only : c_int
    integer(c_int) :: get_jc_fortran
    integer(c_int), intent(in) :: model_index
    get_jc_fortran = mws_array(model_index)%jc
end function get_jc_fortran
