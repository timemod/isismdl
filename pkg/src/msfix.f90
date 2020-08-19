module msfix
    use msvars
    use kinds

    logical, save :: has_fix_vars = .false.

    contains

        subroutine prepare_fix(jt)
            ! this function is called before the model will be solved for one
            ! particular period: make fixed variables in this period exogenous.
            use msvars
            use nuna
            integer, intent(in) :: jt

            type(mdl_variable), pointer :: fix_var
            real(kind = MWS_RKIND) :: value

            has_fix_vars = .false.

            if (mws%fix_vars%var_count == 0) return

            fix_var => mws%fix_vars%first
            do
                value = get_mdl_var_value(fix_var, jt)
                if (.not. nuifna(value)) then
                    has_fix_vars = .true.
                    fixvars(fix_var%var_index) = value
                    mdl%lik(fix_var%var_index) = .false.
                endif
                fix_var => fix_var%next
                if (.not. associated(fix_var)) return
            end do

        end subroutine prepare_fix

        subroutine reset_fix
            ! this function is called after the model has been solved for one
            ! particular period: all active frml variables are made endogenous
            integer :: i
            if (has_fix_vars) then
               do i = 1, mws%mdl%nca
                   if (mws%mdl%ica(i) > 0) mws%mdl%lik(mws%mdl%ica(i)) = .true.
               end do
            endif
        end subroutine reset_fix

        subroutine clear_msfix
            has_fix_vars = .false.
        end subroutine clear_msfix

end module msfix
