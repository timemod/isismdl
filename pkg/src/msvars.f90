module msvars
    !
    ! Work variables for solving a model
    !
    use mws_type
    use kinds
    use solve_options_type

    integer(kind = LI_IKIND), save :: la_lwork
    real(kind = MWS_RKIND), dimension(:, :), allocatable, save :: jacob, solve_jac
    logical, save :: solve_jac_stored = .false.
    integer(kind = MC_IKIND), dimension(:), allocatable, save :: pivot
    real(kind = MWS_RKIND), dimension(:), allocatable, save :: qrtau, la_work
    real(kind = MWS_RKIND), dimension(:), allocatable, target, &
&          save :: yp, curvars, fixvars
    real(kind = MWS_RKIND), dimension(:), allocatable, save :: &
&         lags_leads, ca, fbval, fbfun, scale, fbwork, fbstep, dx, df
    real(kind = MWS_RKIND), dimension(:, :), allocatable, save :: endo_leads

    logical, save :: matrix = .false.
    integer, save :: jf, jl  ! first and last period to solve
    integer, save :: itrtot, njcmat
    logical, save :: change  ! ??
    real(kind = MWS_RKIND) :: relax

    type(modelworkspace), pointer :: mws
    type(model), pointer :: mdl
    type(solve_options), target, save :: opts
    character(len = 1), save :: mode
    character(len = 1) :: method

    private :: get_lwork

    contains

        subroutine msoptsinit(model_index) 
            use modelworkspaces
            integer, intent(in) :: model_index
            ! create a local copy of the solve options stored in the mws
            ! a local copy is used, because set_solve_options or set_fit_options
            ! may modify the options is msvars that should not be written to
            ! the mws
            opts = mws_array(model_index)%solve_opts
        end subroutine msoptsinit

        subroutine msvarsinit(model_index)
            use modelworkspaces
            integer, intent(in) :: model_index

            ! initialise msvars except for opts, which is initialized separately in 
            ! subroutine msoptsinit

            mws => mws_array(model_index)
            mdl => mws%mdl

            ! initialisations
            solve_jac_stored = .false.
            mws%simerr = 0

            ! make sure that all allocated arrays are deallocated
            call msclear
        end subroutine msvarsinit

        subroutine prepare_solve(jf_in, jl_in, error)
            integer, intent(in)  :: jf_in, jl_in
            integer, intent(out) :: error

            ! error = 0 if ok
            !       = 1 if not enough memory available

            integer, external :: ms_get_lwork_nwto, ms_get_lwork_nwqr
            integer :: stat

            error = 0

            jf = jf_in
            jl = jl_in
            itrtot = 0

            mode = opts%mode
            if (mode == "?") then
                if (has_endo_lead(mdl)) then
                    mode = "X"
                else 
                    mode = "D"
                endif
            else if (mode == 'X' .and. .not. has_endo_lead(mdl)) then
                mode = 'D'
                call rwarn("Model has no leads ==> using dynamic mode")
            endif

            method = opts%method
            if (method == '?') then
                if (mdl%nfb == 0) then
                    method = 'G' ! seidel
                else
                    method = 'B' ! broyden
                endif
            else if (method /= 'G' .and. mdl%nfb == 0) then
                method = 'G'
                call rwarn("Cannot apply Newton or Broyden method to " // &
                    "model with no feedback variables. Using the Seidel method.")
            endif

            allocate(yp(mdl%nrv), stat = stat)
            if (stat == 0) allocate(curvars(mdl%nrv), stat = stat)
            if (stat == 0) allocate(fixvars(mdl%nrv), stat = stat)
            if (stat == 0) allocate(lags_leads(mdl%nd), stat = stat)
            if (stat == 0) allocate(ca(mdl%nrv), stat = stat)
            if (stat /= 0) goto 999

            if (mode == 'X' .and. mdl%nendex > 0) then
                ! endogenous leads for the Fair-Taylor
                ! method
                allocate(endo_leads(mws%mdl%nendex, mws%perlen), stat = stat)
                if (stat /= 0) goto 999
            endif

            if (mode == 'R' .or. method == 'G') return

            la_lwork = get_lwork()
            allocate(la_work(la_lwork), stat = stat)
            if (stat == 0) allocate(fbval(mdl%nfb), stat = stat)
            if (stat == 0) allocate(fbfun(mdl%nfb), stat = stat)
            if (stat == 0) allocate(scale(mdl%nfb), stat = stat)
            if (stat == 0) allocate(fbwork(mdl%nfb), stat = stat)
            if (stat == 0) allocate(fbstep(mdl%nfb) , stat = stat)
            if (stat /= 0) goto 999

            if (method == 'B') then
                allocate(dx(mdl%nfb), stat = stat)
                if (stat == 0) allocate(df(mdl%nfb), stat = stat)
                if (stat /= 0) goto 999
            endif

            return

999         continue ! out of memory
            error = 1
            call msclear

        end subroutine prepare_solve

        subroutine msclear(do_clear_solve_jac)

            logical, intent(in), optional :: do_clear_solve_jac
            logical :: do_clear_solve_jac_local

            matrix = .false.

            if (present(do_clear_solve_jac)) then
                do_clear_solve_jac_local = do_clear_solve_jac
            else
                do_clear_solve_jac_local = .true.
            endif

            if (allocated(jacob))      deallocate(jacob)
            if (allocated(pivot))      deallocate(pivot)
            if (allocated(qrtau))      deallocate(qrtau)
            if (allocated(la_work))    deallocate(la_work)
            if (allocated(yp))         deallocate(yp)
            if (allocated(curvars))    deallocate(curvars)
            if (allocated(fixvars))    deallocate(fixvars)
            if (allocated(lags_leads)) deallocate(lags_leads)
            if (allocated(ca))         deallocate(ca)
            if (allocated(fbval))      deallocate(fbval)
            if (allocated(fbfun))      deallocate(fbfun)
            if (allocated(scale))      deallocate(scale)
            if (allocated(fbwork))     deallocate(fbwork)
            if (allocated(fbstep))     deallocate(fbstep)
            if (allocated(dx))         deallocate(dx)
            if (allocated(df))         deallocate(df)
            if (allocated(endo_leads)) deallocate(endo_leads)

            if (do_clear_solve_jac_local) call clear_solve_jac

        end subroutine msclear

        ! deallocate the stored jacobian
        subroutine clear_solve_jac 
            solve_jac_stored = .false.
            if (allocated(solve_jac)) deallocate(solve_jac)
        end subroutine clear_solve_jac

        integer function get_lwork()
            use kinds
            use liqrco
            use liqrsq

            integer :: lwork_qr, lwrk1, lwrk2
            real(kind = SOLVE_RKIND) :: rdum(1,1), rwork(1)

            integer(kind = LAPACK_IKIND) :: info

            lwork_qr = 0
            lwrk1 = 0
            lwrk2 = 0

            if (method == 'B' .or. method == 'Q') then
                ! calculate work space of QR decomposition
                call qrco(rdum, mdl%nfb, mdl%nfb, mdl%nfb, &
&                         rdum(:, 1), rdum(1, 1), rwork, -1_LI_IKIND)
                lwrk1 = nint(rwork(1))
                call qrsq(rdum, mdl%nfb, mdl%nfb, rdum(:, 1), &
&                         rdum(:, 1), rwork, -1_LI_IKIND)
                lwrk2 = nint(rwork(1))
                lwork_qr = max(lwrk1, lwrk2)
            endif

            select case (method)
            case ('N')
                get_lwork = 5 * mdl%nfb
            case ('B')
               ! workspace for dorgqr
               call dorgqr(mdl%nfb, mdl%nfb, mdl%nfb, rdum, &
&                          mdl%nfb, rdum, rwork, -1, info)
               get_lwork = max(lwork_qr, nint(rwork(1)))
            case ('Q')
               get_lwork = lwork_qr
            case default
                get_lwork = 0
            end select

        end function get_lwork

end module msvars
