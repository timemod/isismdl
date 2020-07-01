module mws_type
    use model_type
    use mws_params
    use period_type
    use kinds
    use nuna
    use mdlvars
    use solve_options_type

    type modelworkspace
        integer :: perlen = 0      ! length of the model period
        integer :: data_perlen = 0 ! length of the model data period
        type(period) :: start_period
        type(period) :: end_period
        real(kind = MWS_RKIND), dimension(:, :), allocatable :: &
                             mdl_data, constant_adjustments 
        real(kind = MWS_RKIND), dimension(:), allocatable :: test, ftrelax
        type(model) :: mdl
        type(mdl_variables) :: fix_vars
        type(mdl_variables) :: fit_targets
        type(solve_options) :: solve_opts
        logical :: dbgeqn
        real(kind = SOLVE_RKIND), dimension(:), allocatable :: rmsu
        integer :: simerr = -1
        integer :: jc = -1 ! index of current solve period relative to model
                           ! period in fortran (fortran period)

    end type modelworkspace

    private :: update_value

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

            allocate(mws%test(mws%mdl%nrv), stat = stat)
            if (stat == 0) allocate(mws%ftrelax(mws%mdl%nendex), stat = stat)
            if (stat /= 0) then
                call isismdl_error("Not enough memory to allocate the mws")
                error = 1
                return
            endif
            mws%test = sqrt(Rmeps)
            mws%ftrelax = NA
            mws%dbgeqn = .false.
            mws%jc = -1
            call set_default_options(mws%solve_opts)
        
        end subroutine mwsinit

        subroutine clear_mws(mws)
            type(modelworkspace), intent(inout) :: mws
            integer :: stat
            mws%perlen = 0
            mws%data_perlen = 0
            mws%simerr = -1
            mws%dbgeqn = .false.
            mws%jc = -1
            deallocate(mws%mdl_data, stat = stat)
            deallocate(mws%constant_adjustments, stat = stat)
            deallocate(mws%test, stat = stat)
            deallocate(mws%ftrelax, stat = stat)
            call clear_fit(mws)
            call clear_fix(mws)
            call deallocate_model(mws%mdl)
        end subroutine clear_mws

        subroutine clear_fit(mws)
            type(modelworkspace), intent(inout) :: mws
            integer :: stat
            deallocate(mws%rmsu, stat = stat)
            call clear_mdl_variables(mws%fit_targets)
        end subroutine clear_fit

        subroutine clear_fix(mws)
            type(modelworkspace), intent(inout) :: mws
            call clear_mdl_variables(mws%fix_vars)
        end subroutine clear_fix

        subroutine clone_fit(mws) 
            type(modelworkspace), intent(inout) :: mws
            type(mdl_variables) :: clone
            call clone_mdl_variables(mws%fit_targets, clone)
            mws%fit_targets = clone
        end subroutine clone_fit

        subroutine clone_fix(mws) 
            type(modelworkspace), intent(inout) :: mws
            type(mdl_variables) :: clone
            call clone_mdl_variables(mws%fix_vars, clone)
            mws%fix_vars = clone
        end subroutine clone_fix

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

            integer :: strt

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
            mws%data_perlen = mws%perlen + mws%mdl%mxlag + mws%mdl%mxlead

            deallocate(mws%mdl_data, stat = stat)
            deallocate(mws%constant_adjustments, stat = stat)
            call mddata(mws, error)

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
        
            allocate(mws%mdl_data(mws%mdl%nrv, mws%data_perlen), stat = stat)
            if (stat == 0) allocate(mws%constant_adjustments(mws%mdl%nca, &
&                                                        mws%data_perlen), &
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

        function period_ok(mws, itime)
            type(modelworkspace), intent(in) :: mws
            logical :: period_ok
            integer, intent(in) :: itime
            !
            ! This function returns .true. if itime is
            ! a valid time index
            !
            period_ok = itime >= 1 - mws%mdl%mxlag .and. &
                        itime <= mws%perlen + mws%mdl%mxlead
        end function period_ok

        subroutine get_var_value(mws, ivar, itime, value, error)
            type(modelworkspace), intent(in) :: mws
            integer, intent(in) :: ivar, itime
            real(kind = MWS_RKIND), intent(out) :: value
            logical, intent(out) :: error

            error = .false.
            if (.not. period_ok(mws, itime)) then
                error = .true.
                value = NA
                return
            endif

            value = mws%mdl_data(ivar, itime + mws%mdl%mxlag)
        end subroutine get_var_value

        subroutine set_var_value(mws, ivar, itime, value, error)
            type(modelworkspace), intent(inout) :: mws
            integer, intent(in) :: ivar, itime
            real(kind = MWS_RKIND) :: value
            logical, intent(out) :: error

            error = .false.

            if (.not. period_ok(mws, itime)) then
                error = .true.
                return
            endif

            mws%mdl_data(ivar, itime + mws%mdl%mxlag) = value

        end subroutine set_var_value

        subroutine get_mdl_data(mws, nvar, ivar, ntime, jtb, jte, mat)
            type(modelworkspace), intent(in) :: mws 
            integer, intent(in) :: nvar, ivar(*), ntime, jtb, jte
            real(kind = MWS_RKIND), dimension(ntime, nvar), intent(out) :: mat

            integer :: i, j, jt, iv, jstart, jend, jtd

            jstart = max(jtb, 1 - mws%mdl%mxlag)
            jend   = min(jte,  mws%perlen +  mws%mdl%mxlead)
        
            if (jstart > jtb .or. jend < jte) then
                mat = NA
            endif

            do jt = jstart, jend
                jtd = jt + mws%mdl%mxlag
                j = jt - jtb + 1
                ! j is row index of mat
                do i = 1, nvar
                    iv = ivar(i)
                    mat(j, i) = mws%mdl_data(iv, jtd) 
                end do
            end do
        end subroutine get_mdl_data

        subroutine set_data(mws, nvar, ivar, ntime, jtb, jte, mat, icol, &
                            upd_mode)
            type(modelworkspace), intent(inout) :: mws 
            integer, intent(in) :: nvar, ivar(*), ntime, jtb, jte, icol(*), &
                                   upd_mode
            real(kind = MWS_RKIND), dimension(ntime, *), intent(in) :: mat

            integer :: i, j, jt, jstart, jend, iv, jtd
            real(kind = MWS_RKIND) :: new_value

            jstart = max(jtb, 1 - mws%mdl%mxlag)
            jend   = min(jte,  mws%perlen +  mws%mdl%mxlead)

            if (upd_mode == REPLACE) mws%mdl_data = NA

            do jt = jstart, jend
                jtd = jt + mws%mdl%mxlag
                j = jt - jtb + 1
                ! j is row index of mat
                do i = 1, nvar
                    iv = ivar(i)
                    new_value = mat(j, icol(i))
                    if (upd_mode <= UPD) then
                        new_value = new_value
                    else 
                        new_value = update_value(mws%mdl_data(iv, jtd), &
                                                 new_value, upd_mode)
                    endif
                    mws%mdl_data(iv, jtd) = new_value
                end do
            end do

        end subroutine set_data

        subroutine set_ca(mws, nvar, ivar, ntime, jtb, jte, mat, icol, &
                          upd_mode)
            ! set constant_adjustments
            type(modelworkspace), intent(inout) :: mws 
            integer, intent(in) :: nvar, ivar(*), ntime, jtb, jte, icol(*), &
                                   upd_mode
            real(kind = MWS_RKIND), dimension(ntime, *), intent(in) :: mat
            real(kind = MWS_RKIND) :: new_value

            integer :: i, j, jt, jstart, jend, aci, jtd

            jstart = max(jtb, 1 - mws%mdl%mxlag)
            jend   = min(jte,  mws%perlen +  mws%mdl%mxlead)

            do jt = jstart, jend
                j = jt - jtb + 1
                jtd = jt + mws%mdl%mxlag
                do i = 1, nvar
                    aci = mws%mdl%aci(ivar(i))
                    if (aci <= 0) cycle
                    new_value = mat(j, icol(i))
                    if (upd_mode <= UPD) then
                        new_value = new_value
                    else 
                        new_value = update_value( &
                                        mws%constant_adjustments(aci, jtd), &
                                        new_value, upd_mode)
                    endif
                    mws%constant_adjustments(aci, jtd) = new_value
                end do
            enddo
        end subroutine set_ca

        subroutine get_ca(mws, nca, ica, ntime, jtb, jte, mat)
            type(modelworkspace), intent(in) :: mws 
            integer, intent(in) :: nca, ica(nca), ntime, jtb, jte
            real(kind = MWS_RKIND), dimension(ntime, nca), intent(out) :: mat

            integer :: i, j, jt, jstart, jend, jtd

            jstart = max(jtb, 1 - mws%mdl%mxlag)
            jend   = min(jte,  mws%perlen +  mws%mdl%mxlead)

            if (jstart > jtb .or. jend < jte) then
                mat = NA
            endif
        
            do jt = jstart, jend
                j = jt - jtb + 1
                jtd = jt + mws%mdl%mxlag
                ! j is row index of mat
                do i = 1, nca
                    mat(j, i) = mws%constant_adjustments(ica(i), jtd)
                end do
            end do
        end subroutine get_ca

        subroutine get_fix_fit(mws, nvar, ivar, ntime, jtb, mat, fix)
            ! get fix or fit values, depending on variable fix
            type(modelworkspace), intent(in), target :: mws 
            integer, intent(in) :: nvar, ntime, jtb
            integer, intent(out) :: ivar(nvar)
            real(kind = MWS_RKIND), dimension(ntime, *), intent(out) :: mat
            logical, intent(in) :: fix

            type(mdl_variables), pointer :: mdl_vars
            type(mdl_variable), pointer  :: cur

            integer :: i, it

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
                do it = 1, ntime
                    mat(it, i) = get_mdl_var_value(cur, it + jtb - 1)
                end do
                cur => cur%next
                if (.not. associated(cur)) return
            end do
        end subroutine get_fix_fit

        subroutine set_fix_fit(mws, nvar, ivar, ntime, jtb, jte, mat, icol, &
                               fix, upd_mode)
            ! set fix or fit values, depending on variable fix
            type(modelworkspace), intent(inout), target :: mws 
            integer, intent(in) :: nvar, ivar(*), ntime, jtb, jte, icol(*), &
                                   upd_mode
            real(kind = MWS_RKIND), dimension(ntime, *), intent(in) :: mat
            real(kind = MWS_RKIND) :: fix_value
            logical, intent(in) :: fix

            integer :: i, jstart, jend, vcnt, ierr, iv, im, it, itd

            type(mdl_variables), pointer :: mdl_vars

            if (fix) then
                mdl_vars => mws%fix_vars
            else 
                mdl_vars => mws%fit_targets
            endif

            jstart = max(jtb, 1 - mws%mdl%mxlag)
            jend   = min(jte,  mws%perlen +  mws%mdl%mxlead)

            vcnt = jend - jstart + 1
            im = jstart - jtb + 1

            do i = 1, nvar
                iv = ivar(i)
                if (fix .and. .not. is_frml(mws%mdl, iv)) cycle
                call add_mdl_variable(mdl_vars, iv, vcnt, jstart, &
                                      mat(im, icol(i)), upd_mode, ierr)
                if (ierr /=0) return ! not enough memory
                if (fix) then
                    ! update mdl data
                    do it = 1, vcnt
                        itd = it + mws%mdl%mxlag
                        fix_value = mat(im + it - 1, icol(i))
                        if (.not. nuifna(fix_value)) then
                            mws%mdl_data(iv, jstart + itd - 1) = fix_value
                        endif
                     end do
                 endif
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

        logical function has_rms(mws) 
            type(modelworkspace), intent(in) :: mws
            has_rms = allocated(mws%rmsu)
        end function has_rms

        subroutine get_rms(mws, values)
            type(modelworkspace), intent(in) :: mws
            real(kind = MWS_RKIND), intent(out) :: values(1:mws%mdl%nca)
    
            if (.not. has_rms(mws)) then
                values = NA
            else
                values = mws%rmsu
            endif
        end subroutine get_rms

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

        function update_value(value, update_val, upd_mode)
            use mws_params
            real(kind = MWS_RKIND), intent(inout) :: value
            real(kind = MWS_RKIND), intent(in) :: update_val
            real(kind = MWS_RKIND) :: update_value
            integer, intent(in) :: upd_mode

            select case (upd_mode)
            case (REPLACE, UPD)
                update_value = update_val
            case (UPD_NA)
                if (nuifna(value)) then
                    update_value = update_val
                else 
                    update_value = value
                endif
            case (UPD_VAL)
                if (.not. nuifna(update_val)) then
                    update_value = update_val
                else 
                    update_value =  value
                endif
            end select
        end function update_value

    end module mws_type
