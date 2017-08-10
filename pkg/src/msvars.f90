module msvars
    !
    ! Work variables for solving a model
    !
    use mws_type
    use kinds
    use solve_options_type

    integer(kind = LI_IKIND), save :: jacdim,  la_lwork
    real(kind = MWS_RKIND), dimension(:, :), allocatable, save :: jacob
    integer(kind = MC_IKIND), dimension(:), allocatable, save :: pivot
    real(kind = MWS_RKIND), dimension(:), allocatable, save :: qrtau, la_work
    real(kind = MWS_RKIND), dimension(:), allocatable, target, &
&          save :: yp, curvars, fixvars
    real(kind = MWS_RKIND), dimension(:), allocatable, save :: &
&         lags_leads, ca, fbval, fbfun, scale, fbwork, fbstep, dx, df
    real(kind = MWS_RKIND), dimension(:, :), allocatable, save :: endo_leads


    logical, save :: matrix = .false.
    integer, save :: jf, jl  ! first and last period to solve
    integer, save :: jc  ! current period
    integer, save :: itrtot, njcmat
    logical, save :: change  ! ??
    real(kind = MWS_RKIND) :: relax

    type(modelworkspace), pointer :: mws
    type(model), pointer :: mdl
    type(solve_options), pointer :: opts
    character(len = 1), save :: mode
    character(len = 1) :: method

    private :: get_lwork

    contains

        subroutine msvarsinit(mws_in)
            type(modelworkspace), target, intent(inout) :: mws_in

            mws => mws_in
            mdl => mws%mdl

            !
            ! make sure that all allocated arrays are deallocated
            !
            call msclear

        end subroutine msvarsinit

        subroutine prepare_solve(mws_in, opts_in, jf_in, jl_in, error)
            type(modelworkspace), target, intent(inout) :: mws_in
            type(solve_options), target, intent(in) :: opts_in
            integer, intent(in)  :: jf_in, jl_in
            integer, intent(out) :: error
            ! error = 0 if ok
            !       = 1 if not enough memory available

            integer, external :: ms_get_lwork_nwto, ms_get_lwork_nwqr
            integer :: stat
    
            jf = jf_in
            jl = jl_in
            opts => opts_in
            itrtot = 0

            call msvarsinit(mws_in)

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

            error = 0

            allocate(yp(mdl%nrv), stat = stat)
            if (stat == 0) allocate(curvars(mdl%nrv), stat = stat)
            if (stat == 0) allocate(fixvars(mdl%nrv), stat = stat)
            if (stat == 0) allocate(lags_leads(mdl%nd), stat = stat)
            if (stat == 0) allocate(ca(mdl%nrv), stat = stat)
            if (stat /= 0) then
                error = 1
                return
            endif

            if (mode == 'R' .or. method == 'G') return

            la_lwork = get_lwork()

            allocate(la_work(la_lwork), stat = stat)
            if (stat == 0) allocate(fbval(mdl%nfb), stat = stat)
            if (stat == 0) allocate(fbfun(mdl%nfb), stat = stat)
            if (stat == 0) allocate(scale(mdl%nfb), stat = stat)
            if (stat == 0) allocate(fbwork(mdl%nfb), stat = stat)
            if (stat == 0) allocate(fbstep(mdl%nfb) , stat = stat)
            if (stat /= 0) then
                error = 1
                return
            endif

            if (method == 'B') then
                allocate(dx(mdl%nfb), stat = stat)
                if (stat == 0) allocate(df(mdl%nfb), stat = stat)
                if (stat /= 0) then
                    error = 1
                    return
                endif
            endif

            if (mode == 'X' .and. mdl%nendex > 0) then
                ! endogenous leads for the Fair-Taylor
                ! method
                allocate(endo_leads(mws%mdl%nendex, mws%perlen), stat = stat)
                if (stat /= 0) then
                    error = 1
                    return
                endif
            endif

        end subroutine prepare_solve

        subroutine msclear

            integer :: stat

            matrix = .false.

            deallocate(jacob, stat = stat)
            deallocate(pivot, stat = stat)
            deallocate(qrtau, stat = stat)
            deallocate(la_work, stat = stat)
            deallocate(yp, stat = stat)
            deallocate(curvars, stat = stat)
            deallocate(fixvars, stat = stat)
            deallocate(lags_leads, stat = stat)
            deallocate(ca, stat = stat)
            deallocate(fbval, stat = stat)
            deallocate(fbfun, stat = stat)
            deallocate(scale, stat = stat)
            deallocate(fbwork, stat = stat)
            deallocate(fbstep, stat = stat)
            deallocate(dx, stat = stat)
            deallocate(df, stat = stat)
            deallocate(endo_leads, stat = stat)

        end subroutine msclear

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
