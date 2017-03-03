module msfix
    use msvars
    use kinds

    logical, save :: has_fix_vars = .false.
    logical, dimension(:), allocatable, save :: lik_old

    contains

        subroutine prepare_fix(jt)
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
                    if (.not. allocated(lik_old)) then
                        allocate(lik_old(mdl%nrv))
                        lik_old = mdl%lik
                    endif
                    has_fix_vars = .true.
                    curvars(fix_var%var_index) = value
                    mdl%lik(fix_var%var_index) = .false.
                endif
                fix_var => fix_var%next
                if (.not. associated(fix_var)) return
            end do

        end subroutine prepare_fix

        subroutine reset_fix
            if (has_fix_vars) then
                mdl%lik = lik_old
            endif
        end subroutine reset_fix

        subroutine clear_msfix
            has_fix_vars = .false.
            if (allocated(lik_old)) deallocate(lik_old)
        end subroutine clear_msfix

end module msfix
