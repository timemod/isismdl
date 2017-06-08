module msimul
    use msvars

    private :: simulx, msftcg, msftup, simulb, soljtc, &
&              store_solution, store_solution_prepare_next

    logical, private, save :: forwards, update_lags, chklead
    integer, save :: simerr
    logical, private :: do_fit

contains

subroutine simul
use msutil
use msimot
use msfitm
use msfix
use nucnst

!     simulates the model under current settings
!     Warning: here we must have jf >= 1 and jl <= mws%perlen

!     variable simerr signals errors
!                 0    all ok
!                 1    simulation not possible
!                 2    simulation stopped
!                 3    initial lags/leads missing/invalid
!                 4    invalid parameter values detected
!                 5    if Fair Taylor has NOT converged
!                 6    not enough memory available

!     no need to print any messages (already done)
!     variable can be used for cleanup purposes

logical ::  quit
integer ::  usedat,ndiver, retcod, fit_err
real    ::  told, tnew

! set error flag in global common to 0
! initialize output system
! initialize lags and leads (and check for validity)

simerr = 0
chklead = .true.

! set report option and print solve option
repopt = opts%repopt
call print_solve_options

call chkpar(quit)
if (quit) then
    jc = jf
    simerr = 4
    goto 999
endif

call init_lags_leads_check(quit)
if (quit) then
    jc = jf
    simerr = 3
    goto 999
endif

call cpu_time(told)

ndiver = 0

! set scale = 1. for Newton/Broyden method
if (opts%mode /= 'R') then
    if (opts%method == 'N' .or. opts%method == 'B' .or. opts%method == 'Q') then
        scale = Rone
    endif
endif

! Initialize curvars and feedback variables
call mszini(jf)

! warning message for unavailable previous starting values first period
! if using strict previous method
if (opts%start == 'P' .and. jf == 1) then
   call simot2
endif

! check for fit procedure and call startup routine
if (opts%mode /= 'R') then
    call init_fit_work(do_fit, fit_err)
    if (fit_err > 0) then
        simerr = 6
        call report_solve_error(simerr)
        goto 999
   endif
else
    do_fit = .false.
endif

! flag to handle lags: update them during the solution process
! or fix them at original values of the mws
update_lags = opts%mode == 'D' .or. opts%mode == 'X'
forwards = opts%mode /= 'B'
    
if (opts%mode == 'X') then
    call simulx(ndiver, retcod)
else
    if (opts%start == 'C') then
       usedat = 1
    elseif (opts%start == 'D') then
       usedat = 2
    else
       usedat = 0
    endif
    call simulb(usedat, ndiver, retcod)
endif

if (retcod /= 0)  then
    simerr = retcod
    call report_solve_error(simerr)
endif

! final messages about CPU secs and total iterations/divergence
call cpu_time(tnew)
call simot3(tnew - told, itrtot, ndiver)

change = .true.

999 continue

call clear_msfix

return
end subroutine simul

!-----------------------------------------------------------------------

subroutine simulx(ndiver, retcod)
use msutil
use msimox
use msratop

! simulates the model under current settings
! with rational expectations
! FairTaylor method

! Return code in argument <retcod>
! returns standard codes from routine simulb
!     5     if Fair Taylor has NOT converged
!     0     if all ok

integer ::  ndiver, retcod
integer ::  usedat, iratex, noncvg, imax, jmax

real(kind = SOLVE_RKIND) :: xomax, xnmax, dismax

! initialisation for simulb in first iteration
if (opts%start == 'C') then
    usedat = 1
elseif (opts%start == 'D') then
    usedat = 2
else
    usedat = 0
endif

do iratex = 1, opts%mratex
    
   if (opts%prexen) then
       ! print expectation guesses
       call simox1(iratex)
   endif

   if (iratex > 1) then
       ! after first ratex iteration always use current period
       ! starting values for feedback variables
       ! and also reset lags and leads for first period
       !           no need to check leads again
      usedat  = 1
      chklead = .false.
      call reset_lags_leads(jf)
   endif

   ! solve model for the full current time period
   call simulb(usedat, ndiver, retcod)
   if (retcod /= 0 ) exit

   ! check ratex convergence
   call msftcg(noncvg,imax,jmax,dismax,xomax,xnmax)
   if (noncvg == 0) then
        exit  ! convergence
   else
       ! no convergence; print a message if required
       if (opts%ratrepopt /= RATOPT_MINIMAL .and. &
          ((mod(iratex, opts%ratreport_rep)   == 0)  .or. &
           (mod(iratex, opts%ratfullreport_rep) == 0)))  then
           call simox2(iratex, noncvg)
       endif
       if ((opts%ratrepopt == RATOPT_FULLREP  &
            .or. opts%ratrepopt == RATOPT_FULLREPSCRN) &
              .and. mod(iratex, opts%ratfullreport_rep) == 0) then
           call simox4(jmax, imax, xomax, xnmax, dismax)
       endif
       if (iratex /= opts%mratex ) then
         ! still iterating so prepare new values for leads
         call msftup
       endif
   endif

end do ! end ratex loop

if (retcod /= 0) then
    ! errors in simulb
    ! !!! DO Nothing here. Not needed (yet)
   continue
elseif (noncvg == 0) then
    ! Fair Taylor convergence
    call simox3(iratex)
else
    ! Fair Taylor has not converged
    retcod = 5
    call simox2(iratex, noncvg)
    if (opts%xsuptt) then
        ! largest discrepancy
        call simox4(jmax, imax, xomax, xnmax, dismax)
    else
        ! all remaining discrepancies
        call simox5
    endif
endif

return
end subroutine simulx

!-----------------------------------------------------------------------

subroutine msftcg(noncvg,imax,jmax,dismax,xomax,xnmax)
use nucnst

!     check ratex convergence
!     returns number of not converged leads in argument <noncvg>
!     and some summary data about what has not converged in args after noncvg

!     opts%xtfac is used to multiply test() for more less restrictive convergence test
!     it must be >= 2.0

integer ::  imax,jmax
integer ::  noncvg
real(kind = SOLVE_RKIND) :: dismax, xomax, xnmax
real(kind = SOLVE_RKIND) :: xold, xnew, absdif, pcvg

integer ::  k,i,j

noncvg = 0
dismax = Rzero

do k = 1, mdl%nendex
    i = mdl%iendex(k)
    if (i .le. 0) cycle
    if (.not. mdl%lik(i)) cycle

    do j = jf + 1, jl

        xold = endo_leads(k, j)
        xnew = mws%mdl_data(i, j + mdl%mxlag)
        absdif = abs(xnew-xold)
        if (abs(xold) .gt. Rone) absdif = absdif/abs(xold)

!                must use convergence criterion larger than the one used
!                in solving the model in the inner loop of simulb

!                tried this (works):   pcvg = Rfour * mmws%test(i)
!                but it seems to give some erratic behaviour of iterations
!                in the final stage (last 30 iterations)

!                you need at least twice mws%test(i) so be safe and use double that
!                here we use 10*mws%test(i) to give algorithm some room for manoeuvre
!                probably could do with some investigation
!                but as user you can always make mws%test(i) smaller

        pcvg = opts%xtfac * mws%test(i)

        if (absdif .gt. pcvg) then
            noncvg = noncvg + 1
            if(absdif/pcvg .gt. dismax) then
                dismax = absdif/pcvg
                imax = i
                jmax = j
                xomax = xold
                xnmax = xnew
            endif
        endif

    enddo
end do

return
end subroutine msftcg

!-----------------------------------------------------------------------

subroutine msftup
use nuna

! update endogenous leads with FairTaylor relaxation
! approx. Jacobi iteration for linear systems

real(kind = SOLVE_RKIND) :: xold, xnew, xrlx
integer ::  k, i, j

xnew = NA

do k = 1, mdl%nendex

   i = mdl%iendex(k)
   if (i <= 0) cycle
   if (.not. mdl%lik(i)) cycle

   !  relaxation factor may be different for each lead
   !  constant within loop

   if (nuifna(mws%ftrelax(k))) then
       xrlx =  opts%xrelax
   else
       xrlx = mws%ftrelax(k)
   endif

   do j = jf + 1, jl
       xold = endo_leads(k, j)
       xnew = mws%mdl_data(i, j + mdl%mxlag)
       xnew = xold + xrlx * (xnew - xold)
       endo_leads(k, j) = xnew
       mws%mdl_data(i, j + mdl%mxlag) = xnew
   enddo

   ! update leads beyond last period if needed

   if (opts%uplead) then
       do j = jl + 1, jl + mdl%ibx2(i + 1) - mdl%ibx2(i)
           mws%mdl_data(i, j + mdl%mxlag) = xnew
       enddo
   endif

end do

return
end subroutine msftup

!-----------------------------------------------------------------------

subroutine simulb(usedat, ndiver, retcod)
use msutil
use msimot

! simulates the model under current settings
! no rational expectations

integer ::  ndiver, retcod
integer ::  usedat
logical ::  quit
integer  :: jstart, jend, jt, step, jtd

ndiver = 0

if (forwards) then
   jstart = jf
   jend   = jl
   step   = 1
else
   jstart = jl
   jend   = jf
   step   = -1
endif

jc = jstart
! generate string with period  for output system
call sjcstr(jc)

do jt = jstart, jend, step

    jtd = jt + mdl%mxlag

    ! set ca's (constant adjustments)
    ! if usedat is 1 then
    !     set current starting values and exogenous vars
    ! else if usedat = 2 then
    !     set exogenous vars and
    !     use current period values if valid
    ! else
    !     set exogenous variables only
    !     (curvars already contains solution of previous period
    !      or start values for feedback variables)
    ! endif

   call retrieve_ca(jt)

   if (usedat == 1) then
       curvars = mws%mdl_data(:, jtd)
   elseif (usedat == 2) then
      call cpvech(curvars, mws%mdl_data(1, jtd), mdl%nrv, mdl%lik)
   else
      call cpvecf(curvars, mws%mdl_data(1, jtd), mdl%nrv, mdl%lik)
   endif
    
   ! solve current period
   call soljtc(retcod, ndiver, jt)

   if (retcod == 1) then

       return

   elseif (retcod == 2) then

       call store_solution(jt)
       return

   elseif (jt == jend) then

      ! Last period
      ! store solution and done
      call store_solution(jt)
      return

   else

      ! update period indicators for messages
      jc = jt + step
      call sjcstr(jc)

      call store_solution_prepare_next(jt, quit)
      if (quit) then
         retcod = 1
         return
      endif

   endif

end do   ! end time loop

return
end subroutine simulb

!-----------------------------------------------------------------------

subroutine store_solution(jt)
    use msutil
    integer, intent(in) :: jt

    ! Store the solution for the current period in the mws

    call store_ca(jt)
    mws%mdl_data(:, jt + mdl%mxlag) = curvars
    return
end subroutine store_solution

!-----------------------------------------------------------------------

subroutine store_solution_prepare_next(jt, quit)
    use msutil
    use msimob
    integer, intent(in) :: jt
    logical, intent(out) :: quit

    ! Store the solution and prepare the next period

    ! 1. store solution (curvars and ca) in mdl_data
    ! 2. get new leads and new leads for the next period
    ! 3. print messages about the number of missing lags and leads

    integer :: nonval_lag, nonval_lead

    quit = .false.

    if (.not. update_lags) then
        ! for the static and residual check modes,
        ! the new lags should be updated BEFORE mdl_data
        ! has been updated
        call getnlg(jt, forwards, .true., nonval_lag)
    endif

    ! put results for current period in the mws
    call store_solution(jt)

    ! for all modes except static and residual check, the new lags should
    ! be based on the updated model worksspace
    if (update_lags) call getnlg(jt, forwards, .true., nonval_lag)

    ! get new leads. for the backward mode the updated values of
    ! the leads are used
    call getnld(jt, forwards, chklead, nonval_lead)

    if (nonval_lag  > 0) call simob2(nonval_lag)
    if (nonval_lead > 0) call simob4(nonval_lead)
    if (nonval_lag > 0 .or. nonval_lead > 0) quit = opts%erropt == ERROPT_STOP

end subroutine store_solution_prepare_next

!-----------------------------------------------------------------------

subroutine soljtc(retcod,ndiver,jt)
use mssolve
use msfix
use nucnst
use msfitm
use msolot

! solves the model for the current period targetc (jc)
! calls required solving subroutine depending on mode

! at call, curvars contains starting values
! the result is stored in curvars

! alternate return 1 for missing/invalid exo's or starting values
! alternate return 2 for numerical problems or non-convergence
! alternate return 3 for other errors (write)

integer ::  retcod,ndiver, jt
integer ::  ndiverp

logical ::  dofit_now

call solot1

call prepare_fix(jt)

if (opts%mode == 'R') then
    call reschk(retcod)
    continue
else
    ! if fit to be executed then
    !     call prefit for any special initialization
    ! else
    !    test for new jac every period

    if (do_fit) then
        dofit_now = isfitp(mws, jt)
    else
        dofit_now = .false.
    endif
    if (dofit_now) then
        call prefit
    elseif (opts%njacpd == 1 .and. opts%method /= 'G') then
        ! new jacobian at start of every period; set scale to 1.0
        matrix = .false.
        scale = Rone
    elseif(opts%njacpd == 2 .and. opts%method /= 'G') then
        ! new jacobian at start of every period and keep old scaling
        matrix = .false.
    endif

    ndiverp = ndiver
    call solone(retcod,ndiver)

    ! if going to execute fit procedure
    ! but no convergence achieved for this period
    ! and come here (user wants to continue)
    !  ==> NO fit procedure will be executed (has no purpose)
    if (retcod == 0 ) then
        if (dofit_now .and. ndiver == ndiverp) then
            call solfit(retcod, ndiver, jt)
        endif
    endif
endif

call reset_fix

return
end subroutine soljtc

subroutine report_solve_error(error)
    use output_utils
    integer, intent(in) :: error

    select case (error)
    case (1)
        call isismdl_warn("Simulation not possible")
    case (2)
        call isismdl_warn("Simulation stopped")
    case (3)
        call isismdl_warn("Initial lags/leads missing/invalid. Simulation not possible.")
    case (4)
        call isismdl_warn("Invalid parameter values detected. Simulation not possible")
    case (5)
        call isismdl_warn("Fair-Taylor has not converged")
    case (6)
        call isismdl_warn("Out of memory. Simulation not succesfull")
    case default
        call isismdl_warn("Unknown problem in solve. Simulation not succesfull")
    end select

end subroutine report_solve_error


end module msimul
