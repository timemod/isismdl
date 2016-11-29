module mws_type
    use model_type
    use mws_params
    use period_type
    use kinds
    use nuna
    use mdlvars
    use solve_options_type

    type modelworkspace
        integer :: perlen = 0 ! length of the model period
        type(period) :: start_period
        type(period) :: end_period
        real(kind = MWS_RKIND), dimension(:, :), allocatable :: &
                             mdl_data, constant_adjustments, lags, &
                             leads
        real(kind = MWS_RKIND), dimension(:), allocatable :: test, ftrelax
        type(model) :: mdl
        type(mdl_variables) :: fix_vars
        type(mdl_variables) :: fit_targets
        type(solve_options) :: solve_opts
        logical :: dbgeqn
        real(kind = SOLVE_RKIND), dimension(:), allocatable :: rmsu

    end type modelworkspace

    contains

        subroutine mwsinit(mws, error)
            !
            ! initialise mws after reading a model
            !
            use output_utils, only : isismdl_error
            use nucnst, only : Rmeps
            type(modelworkspace), intent(inout) :: mws
            integer, intent(out) :: error
        
            integer :: stat
            
            error = 0

            allocate(mws%lags(mws%mdl%nrv, mws%mdl%mxlag), stat = stat)
            if (stat == 0) allocate(mws%leads(mws%mdl%nrv, mws%mdl%mxlead), &
                                    stat = stat)
            if (stat == 0) allocate(mws%test(mws%mdl%nrv), stat = stat)
            if (stat == 0) allocate(mws%ftrelax(mws%mdl%nendex), stat = stat)
            if (stat /= 0) then
                call isismdl_error("Not enough memory to allocate the mws")
                error = 1
                return
            endif
            mws%lags = NA
            mws%leads = NA
            mws%test = sqrt(Rmeps)
            mws%ftrelax = NA
            mws%dbgeqn = .false.
            call set_default_options(mws%mdl, mws%solve_opts)
        
        end subroutine mwsinit

        subroutine clear_mws(mws)
            type(modelworkspace), intent(inout) :: mws
            integer :: stat
            mws%perlen = 0
            deallocate(mws%mdl_data, stat = stat)
            deallocate(mws%lags, stat = stat)
            deallocate(mws%leads, stat = stat)
            deallocate(mws%constant_adjustments, stat = stat)
            deallocate(mws%test, stat = stat)
            deallocate(mws%ftrelax, stat = stat)
            deallocate(mws%lags, stat = stat)
            deallocate(mws%leads, stat = stat)
            deallocate(mws%rmsu, stat = stat)
            call clear_mdl_variables(mws%fix_vars)
            call clear_mdl_variables(mws%fit_targets)
            call deallocate_model(mws%mdl)
        end subroutine clear_mws

        ! 
        ! Sets a model parameter. Returns 0 on succes and 1 if argument
        ! length does not agree with the actual length of the parameter.
        ! 
        integer function set_par(mws, i, value, length)
            type(modelworkspace), intent(inout) :: mws
            integer, intent(in) :: i, length
            real(kind = MWS_RKIND), dimension(length), intent(in) :: value

            integer :: strt

            if (length /= mws%mdl%nvalp(i)) then
                set_par = 1
                return
            endif
            strt = mws%mdl%cfptr(i)
            mws%mdl%coef(strt : strt + length - 1) = value(1 : length)
            set_par = 0
        end function set_par

        ! 
        ! get a model parameter
        ! 
        subroutine get_par(mws, i, value, length)
            type(modelworkspace), intent(inout) :: mws
            integer, intent(in) :: i, length
            real(kind = MWS_RKIND), dimension(length), intent(out) :: value

            integer :: n, strt

            strt  = mws%mdl%cfptr(i)
            value = mws%mdl%coef(strt : strt + length - 1)

        end subroutine get_par

        !
        ! set the model period and initialise the mws
        ! 
        subroutine mws_setper(mws, start, end, freq, error)
            type(modelworkspace), intent(inout) :: mws
            integer, dimension(2), intent(in) :: start, end
            integer, intent(in) :: freq
            logical, intent(out) :: error

            integer :: stat

            mws%start_period%year = start(1)
            mws%start_period%subperiod = start(2)
            mws%start_period%frequency = freq
            mws%end_period%year = end(1)
            mws%end_period%subperiod = end(2)
            mws%end_period%frequency = freq

            mws%perlen = get_period_count(mws%start_period, mws%end_period)

            deallocate(mws%mdl_data, stat = stat)
            deallocate(mws%constant_adjustments, stat = stat)
            call mddata(mws, error)
            mws%lags = NA
            mws%leads = NA

            call clear_mdl_variables(mws%fix_vars)
            call clear_mdl_variables(mws%fit_targets)
            if (allocated(mws%rmsu)) deallocate(mws%rmsu)
        end subroutine mws_setper

        !
        ! This subroutine checks if array data has been allocated.
        ! If not, then the array is allocated and initialised.
        !
        subroutine mddata(mws, error)
            use output_utils, only : isismdl_error
            type(modelworkspace), intent(inout) :: mws
            logical, intent(out) :: error
        
            integer :: stat
        
            error = .false.
        
            if (allocated(mws%mdl_data)) return
        
            if (mws%mdl%nrv == 0 .or. mws%perlen == 0) then
                call isismdl_error("Model has not been loaded")
                error = .true.
                return
            endif
        
            allocate(mws%mdl_data(mws%mdl%nrv, mws%perlen), stat = stat)
            if (stat == 0) allocate(mws%constant_adjustments(mws%mdl%nca, &
&                                                            mws%perlen), &
&                                   stat = stat)
            if (stat /= 0) then
                call isismdl_error("Not enough memory to allocate the mws")
                error = .true.
                return
             endif
        
            ! initialise data arrays
            mws%mdl_data = NA
            mws%constant_adjustments = 0
        
        end subroutine mddata

        function period_ok(mws, ivar, itime)
            type(modelworkspace), intent(in) :: mws
            logical :: period_ok
            integer, intent(in) :: ivar, itime
            !
            ! This function returns .true. if itime is
            ! a valid time index for variable ivar
            !
            period_ok = itime >= 1 - mws%mdl%mxlag .and. &
                        itime <= mws%perlen + mws%mdl%mxlead
        end function period_ok

        subroutine get_var_value(mws, ivar, itime, value, error)
            type(modelworkspace), intent(in) :: mws
            integer, intent(in) :: ivar, itime
            real(kind = MWS_RKIND), intent(out) :: value
            logical, intent(out) :: error

            if (.not. period_ok(mws, ivar, itime)) then
                error = .true.
                value = NA
                return
            endif

            error = .false.
            if (itime <= 0) then
                value = mws%lags(ivar, itime + mws%mdl%mxlag) 
            elseif (itime <= mws%perlen) then
                value = mws%mdl_data(ivar, itime)
            else
                value = mws%leads(ivar, itime - mws%perlen) 
            endif
        end subroutine get_var_value

        subroutine set_var_value(mws, ivar, itime, value, error)
            type(modelworkspace), intent(inout) :: mws
            integer, intent(in) :: ivar, itime
            real(kind = MWS_RKIND) :: value
            logical, intent(out) :: error

            error = .false.

            if (.not. period_ok(mws, ivar, itime)) then
                error = .true.
                return
            endif

            if (itime < 1) then
                mws%lags(ivar, itime + mws%mdl%mxlag) = value
            elseif (itime .le. mws%perlen) then
                mws%mdl_data(ivar, itime) = value
            else
                mws%leads(ivar, itime - mws%perlen) = value
            endif
        end subroutine set_var_value

        subroutine get_mdl_data(mws, nvar, ivar, ntime, jtb, jte, mat)
            type(modelworkspace), intent(in) :: mws 
            integer, intent(in) :: nvar, ivar(*), ntime, jtb, jte
            real(kind = MWS_RKIND), dimension(ntime, nvar), intent(out) :: mat

            integer :: i, j, jt
            logical :: error
        
            do i = 1, nvar
                do jt = jtb, jte
                    j = jt - jtb + 1
                    ! j is row index of mat
                    call get_var_value(mws, ivar(i), jt, mat(j, i), error)
                end do
            end do
        end subroutine get_mdl_data

        subroutine set_data(mws, nvar, ivar, ntime, jtb, jte, mat, icol)
            type(modelworkspace), intent(inout) :: mws 
            integer, intent(in) :: nvar, ivar(*), ntime, jtb, jte, icol(*)
            real(kind = MWS_RKIND), dimension(ntime, *), intent(in) :: mat

            integer :: i, j, jt, jstart, jend
            integer, intrinsic :: min, max
            logical :: error

            jstart = max(jtb, 1 - mws%mdl%mxlag)
            jend   = min(jte,  mws%perlen +  mws%mdl%mxlead)

            do i = 1, nvar
                do jt = jstart, jend
                    j = jt - jtb + 1
                    ! j is row index of mat
                    call set_var_value(mws, ivar(i), jt, mat(j, icol(i)), error)
                end do
            end do
        
        end subroutine set_data

        subroutine set_ca(mws, nvar, ivar, ntime, jtb, jte, mat, icol)
            ! set constant_adjustments
            type(modelworkspace), intent(inout) :: mws 
            integer, intent(in) :: nvar, ivar(*), ntime, jtb, jte, icol(*)
            real(kind = MWS_RKIND), dimension(ntime, *), intent(in) :: mat

            integer :: i, j, jt, jstart, jend, aci
            integer, intrinsic :: min, max

            jstart = max(jtb, 1)
            jend   = min(jte,  mws%perlen)

            do i = 1, nvar
                aci = mws%mdl%aci(ivar(i))
                if (aci <= 0) cycle
                do jt = jstart, jend
                    j = jt - jtb + 1
                    mws%constant_adjustments(aci, jt) = mat(j, icol(i))
                end do
            enddo
        end subroutine set_ca

        subroutine get_ca(mws, nca, ica, ntime, jtb, jte, mat)
            type(modelworkspace), intent(in) :: mws 
            integer, intent(in) :: nca, ica(nca), ntime, jtb, jte
            real(kind = MWS_RKIND), dimension(ntime, nca), intent(out) :: mat

            integer :: i, j, jt
            logical :: error

            mat = NA
        
            do i = 1, nca
                do jt = max(jtb, 0), min(jte, mws%perlen)
                    j = jt - jtb + 1
                    ! j is row index of mat
                    mat(j, i) = mws%constant_adjustments(ica(i), jt)
                end do
            end do
        end subroutine get_ca

        subroutine get_fix_fit(mws, nvar, ivar, ntime, jtb, jte, mat, fix)
            ! get fix or fit values, depending on variable fix
            type(modelworkspace), intent(inout), target :: mws 
            integer, intent(in) :: nvar, ntime, jtb, jte
            integer, intent(out) :: ivar(*)
            real(kind = MWS_RKIND), dimension(ntime, *), intent(out) :: mat
            logical, intent(in) :: fix

            type(mdl_variables), pointer :: mdl_vars
            type(mdl_variable), pointer  :: cur

            integer :: i, jt

            if (fix) then
                mdl_vars => mws%fix_vars
            else 
                mdl_vars => mws%fit_targets
            endif

            i = 0
            cur => mdl_vars%first
            do
                i = i + 1
                ivar(i) = cur%var_index
                do jt = jtb, jte
                    mat(jt, i) = get_mdl_var_value(cur, jt)
                end do
                cur => cur%next
                if (.not. associated(cur)) return
            end do
        end subroutine get_fix_fit

        subroutine set_fix_fit(mws, nvar, ivar, ntime, jtb, jte, mat, icol, &
                               fix)
            ! set fix or fit values, depending on variable fix
            type(modelworkspace), intent(inout), target :: mws 
            integer, intent(in) :: nvar, ivar(*), ntime, jtb, jte, icol(*)
            real(kind = MWS_RKIND), dimension(ntime, *), intent(in) :: mat
            logical, intent(in) :: fix

            integer :: i, j, jt, jstart, jend, vcnt, ierr, iv, im
            integer, intrinsic :: min, max
            real(kind = MWS_RKIND) :: fix_value

            type(mdl_variables), pointer :: mdl_vars

            if (fix) then
                mdl_vars => mws%fix_vars
            else 
                mdl_vars => mws%fit_targets
            endif

            jstart = max(jtb, 1)
            jend   = min(jte,  mws%perlen)
            vcnt = jend - jstart + 1
            im = jstart - jtb + 1

            do i = 1, nvar
                iv = ivar(i)
                if (fix .and. .not. is_frml(mws%mdl, iv)) cycle
                call add_mdl_variable(mdl_vars, iv, vcnt, jstart, &
                                      mat(im, icol(i)), ierr)
                if (fix) then
                    ! update mdl data
                    do j = 1, vcnt
                        fix_value = mat(j + im - 1, icol(i))
                        if (.not. nuifna(fix_value)) then
                            mws%mdl_data(iv, jstart + j - 1) = fix_value
                        endif
                    end do
                endif
                if (ierr /=0) return
            end do

        end subroutine set_fix_fit

        logical function isfixp(mws, jt)
            ! returns .true. if period jt has fix values
            type(modelworkspace), intent(in) :: mws
            integer, intent(in) :: jt
            isfixp = has_mdl_var(mws%fix_vars, jt)
        end function isfixp

        subroutine set_rms(mws, var_index, value, error)
            type(modelworkspace), intent(inout) :: mws
            integer, intent(in) :: var_index
            real(kind = MWS_RKIND), intent(in) :: value
            logical, intent(out) :: error
        
            integer ::  aci, stat

            if (.not. allocated(mws%rmsu)) then
                allocate(mws%rmsu(mws%mdl%nca), stat = stat)
                if (stat /= 0) then
                    error = .true.
                    return
                endif
                mws%rmsu = NA
            endif

            aci = mws%mdl%aci(var_index)
            error = aci <= 0
            if (.not. error) mws%rmsu(aci) = value

        end subroutine set_rms

        subroutine mknumu(mws, numu, numr, nu)
            use nucnst
            type(modelworkspace), intent(in) :: mws 
            integer(kind = SOLVE_IKIND), intent(out) :: numu(*), numr(*), nu

            ! record all exogenous CAs with rms > 0 in numu
            ! number of CAs recorded put into nu

            integer(kind = SOLVE_IKIND) ::  knu,i

            if (.not. allocated(mws%rmsu)) then
                nu = 0
                return
            endif

            knu = 0

            do i = 1, mws%mdl%nca
               if (mws%mdl%ica(i) .gt. 0 ) then
                  if (mws%mdl%lik(mws%mdl%ica(i)) ) then
!                          variable is endogenous ==> CA is exogenous
                     if (.not. nuifna(mws%rmsu(i)) .and. mws%rmsu(i) > Rzero) then
                        knu       = knu + 1
                        numu(knu) = mws%mdl%ica(i)
                        numr(knu) = i
                     endif
                  endif
               endif
            end do

            nu = knu

            return
        end subroutine mknumu

        logical function isfitp(mws, jt)
            type(modelworkspace), intent(in) :: mws 
            integer, intent(in) :: jt
            ! returns .true. if period jt has fit targets
            isfitp = has_mdl_var(mws%fit_targets, jt)
            return
        end function isfitp

end module mws_type
