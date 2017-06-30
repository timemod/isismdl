!     helper functions and subroutines
!     for all solve subroutines
 
!-----------------------------------------------------------------------

module msutil
use msvars
use nuna

private  chkexo, chkca, chkvfb, get_next_lag_forwards, get_next_lag_backwards, &
&        get_next_lead_forwards, get_next_lead_backwards

contains

    logical function tstvfb()
        ! check for valid numbers in feedback variables
        ! return .true.  if all are valid
        ! return .false. at first non valid feedback variable

        integer ::  i

        tstvfb = .true.

        do i = 1, mdl%nfb
            if (nuifna(curvars(mdl%numfb(i)))) then
               tstvfb = .false.
               return
            endif
        enddo

        return
    end function tstvfb

!-----------------------------------------------------------------------

    logical function tstcvg()
        use nuna

        ! check for convergence
        ! return .true.  if solution has converged
        ! return .false. if not

        integer :: fbnum, i

       ! assume solution has not converged

        tstcvg = .false.

        ! check convergence of feedback variables

        do i = 1, mdl%nfb

           fbnum = mdl%numfb(i)
           if (mdl%lik(fbnum)) then
              if (abs(curvars(fbnum)) < 1.0_ISIS_RKIND) then
                 if (abs(curvars(fbnum) - yp(fbnum)) > mws%test(fbnum)) return
              else
                 if (abs((curvars(fbnum) - yp(fbnum)) &
&                    / curvars(fbnum)) > mws%test(fbnum)) return
              endif
           endif

        end do

!             all feedback variables have converged
!             check others for convergence

        do i = 1, mdl%nrv

            if (.not. mdl%lik(i)) cycle
            if (nuifna(curvars(i))) cycle

!                 if curvars(i) not okay with all feedback variables okay then
!                     variable i is a de facto post recursive variable
!                     and a further test is not needed
 
!                 if curvars(i) is okay but yp(i) is not okay then
!                     convergence has not yet been achieved
!                     this may happen at very first iteration

            if (nuifna(yp(i))) return

            if (abs(curvars(i)) < 1.0_ISIS_RKIND) then
                if(abs(curvars(i) - yp(i)) > mws%test(i)) return
            else
                if (abs((curvars(i) - yp(i)) / curvars(i)) > mws%test(i)) return
            endif

        end do

!             now we have convergence

        tstcvg = .true.

        return
    end function tstcvg

!-----------------------------------------------------------------------

    subroutine chkxa(quit)
        use msolot
        logical, intent(out) :: quit

        ! check exogenous variables and CAs for validity for period jc
        ! quit == .true.  on invalid/missing or quit simulation

        integer :: nonval

        call chkexo(nonval)
        call chkca(nonval)

        if (nonval > 0) then
            call solot4(nonval)
            quit = opts%erropt == ERROPT_STOP
        else 
            quit = .false.
        endif

        return
    end subroutine chkxa

!-----------------------------------------------------------------------

    subroutine chkxaf(quit)
        use msolot
       logical, intent(out) :: quit

       ! check exogenous variables and CAs for validity for period jc
       ! check feedback values for validity
       ! quit = .true. on invalid/missing or quit simulation

       integer :: nonval

       call chkxa(quit)

       ! test for invalid numbers in feedback variables
       call chkvfb(nonval)
       if (nonval > 0) then
           call solot6(nonval)
           quit = .true.
       endif

       return
    end subroutine chkxaf

!-----------------------------------------------------------------------

    subroutine chkexo(nonval)
        use msolot
        integer, intent(out) :: nonval

        ! check exogenous variables for validity and/or missing

        integer :: i
    
        nonval = 0

        do i = 1, mdl%nrv
            if (mdl%lik(i)) cycle
            if (.not. nuifna(curvars(i))) cycle
            call solot2(i)
            nonval = nonval + 1
        end do

        return
    end subroutine chkexo

!-----------------------------------------------------------------------

    subroutine chkca(nonval)
        use kinds   
        use msolot
        use nucnst
        integer, intent(inout) :: nonval

        ! check constant adjustments for validity and/or missing

        integer :: canum, i

        do i = 1, mdl%nca
            canum = mdl%ica(i)
            if (canum <= 0) cycle
            if (.not. nuifna(ca(canum)) ) cycle
            if (mdl%lik(canum)) then
                call solot3(canum)
                nonval = nonval + 1
            else
                ca(canum) = Rzero
            endif
        end do

        return
    end subroutine chkca

!-----------------------------------------------------------------------

    subroutine chkvfb(nonval)
        use msolot
        integer, intent(out) :: nonval

        ! check feedback variables for valid values

        integer :: i
    
        nonval = 0

        do i = 1, mdl%nfb
            if (nuifna(curvars(mdl%numfb(i)))) then
                call solot5(mdl%numfb(i))
                nonval = nonval + 1
           endif
        end do
        return
    end subroutine chkvfb

!-----------------------------------------------------------------------

subroutine chkpar(quit)
    use msimod
    logical, intent(out) ::   quit
     
    ! check parameters  for validity and/or missing
    ! quit set to .true. if simulation to be stopped
    
    integer ::  i,p,nonval,csptr
    
    nonval = 0
    
    do p = 1, mdl%nrp
        csptr = mdl%cfptr(p)
        do i = 1, mdl%nvalp(p)
            if (.not. nuifna(mdl%coef(csptr+i-1))) cycle
            call simod4(p, i)
            nonval = nonval + 1
        end do
    end do
    
    quit = nonval .ne. 0

end subroutine chkpar

!-----------------------------------------------------------------------

subroutine init_lags_leads_check(quit)
use msimod
logical, intent(out) :: quit
 
! initialize lags and leads (and check for validity)
! NOTE: here jf and jl (the sample indicators) must
!       be within the legal period for the data arrays
!       jf >= 1 and jl <= mws%perlen

! quit == .true.  for quit/stop simulation


logical :: error

integer ::   nonval, i, j, jlag, jlead, k, jstart

nonval = 0
quit = .false.

! jstart is the first period of the simulation
if (mode == 'B') then
    jstart = jl   ! backwards: start at the last period
else
    jstart = jf
endif

! set period string for output system
call sjcstr(jstart)

do i = 1, mdl%nrv

   ! lags
   do j = mdl%ibx1(i), mdl%ibx1(i + 1) - 1
       ! 1 + j - ibx1(i) is the lag applied
       ! jlag is the time period for which we want data
       jlag = jstart - (1 + j - mdl%ibx1(i))
       call get_var_value(mws, i, jlag, lags_leads(j), error)
       if (nuifna(lags_leads(j))) then
           call simod1(i, 1 + j - mdl%ibx1(i))
           nonval = nonval + 1
       endif
   enddo

   ! leads
   if (opts%uplead .and. mdl%lik(i) ) then

      ! endogenous lead and update beyond sample horizon
      !  get the leads from data with (implicit) update beyond jl
      do j = mdl%ibx2(i), mdl%ibx2(i + 1) - 1
           jlead = jstart + j - mdl%ibx2(i) + 1
           if( jlead > jl ) then
              lags_leads(j) = mws%mdl_data(i, jl + mdl%mxlag)
           else
              lags_leads(j) = mws%mdl_data(i, jlead + mdl%mxlag)
           endif
      enddo

   else

       ! exogenous lead or not updating beyond sample horizon
       ! get the leads from data
       do j = mdl%ibx2(i), mdl%ibx2(i + 1) - 1
           jlead = jstart + j - mdl%ibx2(i) + 1
           call get_var_value(mws, i, jlead, lags_leads(j), error)
       enddo

   endif

   do j = mdl%ibx2(i), mdl%ibx2(i + 1) - 1
       if (nuifna(lags_leads(j)) ) then
           call simod2(i, 1 + j - mdl%ibx2(i))
           nonval = nonval + 1
       endif
   enddo

enddo

if (nonval > 0) then
    call simod3(nonval)
    quit = opts%erropt == ERROPT_STOP
endif

if (mode == "X" .and. allocated(endo_leads)) then
    ! Setup check time series for endogenous leads
    ! Only used for rational expectation mode (the
    ! Fair-Taylor-mode). Not used in the ratex procedure.
    do k = 1, mdl%nendex
        i = mdl%iendex(k)
        if (i .le. 0) cycle
        if (.not. mdl%lik(i)) cycle
        do j = jf + 1, jl
            endo_leads(k, j) = mws%mdl_data(i, j + mdl%mxlag)
        enddo
     enddo
endif

do k = 1, mdl%nendex

    i = mdl%iendex(k)
    if (i .le. 0) cycle
    if (.not. mdl%lik(i)) cycle

    ! if updating leads beyond sample (lik(i)=.true. here) then
    ! fill in leads beyond sample horizon from last value for period jl

    if (opts%uplead) then
        do j = jl + 1, jl + mdl%ibx2(i + 1) - mdl%ibx2(i)
            mws%mdl_data(i, j + mdl%mxlag) = mws%mdl_data(i, jl + mdl%mxlag)
        enddo
    endif

enddo

return
end subroutine init_lags_leads_check

!-----------------------------------------------------------------------

subroutine reset_lags_leads(jf)
    integer, intent(in) :: jf
    
    ! reset lags and leads for first period of a simulation
    ! called in ratex mode after first ratex iteration
    ! should NOT be used to initialize d-vector (use
    ! init_lags_leads_check)
    
    integer ::  i, j, jlag, jlead

    do i = 1, mdl%nrv
    
       ! lags
       do j = mdl%ibx1(i), mdl%ibx1(i + 1) - 1
           jlag = jf - (1 + j - mdl%ibx1(i))
           lags_leads(j) = mws%mdl_data(i, jlag + mdl%mxlag)
       enddo
    
       ! leads
       do j = mdl%ibx2(i), mdl%ibx2(i + 1) - 1
           jlead = jf + j - mdl%ibx2(i) + 1
           lags_leads(j) = mws%mdl_data(i, jlead + mdl%mxlag)
       enddo
    enddo
    
    return
end subroutine reset_lags_leads

!-----------------------------------------------------------------------

subroutine getnlg(jt, forwards, chk, nonval)
    integer, intent(in) :: jt       ! period
    logical, intent(in) :: forwards ! .true. for forward propagation
    logical, intent(in) :: chk
    integer, intent(out) :: nonval  ! the number of missing new lags

    ! update lags_lags vector for lags in next period

    integer :: ivar
    logical :: is_na

    nonval = 0

    do ivar = 1, mdl%nrv
        if (mdl%ibx1(ivar) < mdl%ibx1(ivar + 1)) then
            if (forwards) then
                call get_next_lag_forwards(ivar, jt, chk, is_na)
            else
                call get_next_lag_backwards(ivar, jt, chk, is_na)
            endif
            if (chk .and. is_na) nonval = nonval + 1
        endif
    enddo

    return
end subroutine getnlg

!-----------------------------------------------------------------------

subroutine get_next_lag_forwards(ivar, jt, chk, is_na)
    use msimob
    integer, intent(in) :: ivar, jt
    logical, intent(in) :: chk
    logical, intent(out) :: is_na

    integer :: j

    !shift lags_leads upwards (do nothing if only 1 lag)
    do j = mdl%ibx1(ivar + 1) - 1, mdl%ibx1(ivar) + 1, -1
        lags_leads(j) = lags_leads(j - 1)
    enddo

    ! set -1 (next period) for this variable
    ! get the value from the model data at the current period
    lags_leads(mdl%ibx1(ivar)) = mws%mdl_data(ivar, jt + mdl%mxlag)

    if (chk) then
        is_na = nuifna(lags_leads(mdl%ibx1(ivar)))
        if (is_na) call simob1(ivar, 1)
    endif

end subroutine get_next_lag_forwards

!-----------------------------------------------------------------------

subroutine get_next_lag_backwards(ivar, jt, chk, is_na)
    use msimob
    integer, intent(in) :: ivar, jt
    logical, intent(in) :: chk
    logical, intent(out) :: is_na

    integer :: j, maxlag, itime
    logical :: error

    do j = mdl%ibx1(ivar), mdl%ibx1(ivar + 1) - 2
        lags_leads(j) = lags_leads(j + 1)
    enddo

    ! set the value of the largest lag in the next period.
    ! for backwards the next period lies before
    ! the current period
    maxlag = mdl%ibx1(ivar + 1) - mdl%ibx1(ivar)
    itime = jt - 1 - maxlag
    call get_var_value(mws, ivar, itime, lags_leads(mdl%ibx1(ivar + 1) - 1), &
&                      error)

    if (chk) then
        is_na = nuifna(lags_leads(mdl%ibx1(ivar + 1) - 1))
        if (is_na) call simob1(ivar, maxlag)
    endif

end subroutine get_next_lag_backwards

!-----------------------------------------------------------------------

subroutine getnld(jt, forwards, chk, nonval)
    integer, intent(in) :: jt
    logical, intent(in) :: forwards
    logical, intent(in) :: chk
    integer, intent(out) :: nonval  ! the number of missing new leads

    ! update lags_leads vector for leads in next period
    ! get longest leads from mws%leads or mws%mdl_data

    integer :: ivar
    logical :: is_na

    nonval = 0

    do ivar = 1, mdl%nrv
        if (mdl%ibx2(ivar) < mdl%ibx2(ivar + 1)) then
            if (forwards) then
                call get_next_lead_forwards(ivar, jt, chk, is_na)
            else
                call get_next_lead_backwards(ivar, jt, chk, is_na)
            endif
            if (chk .and. is_na) nonval = nonval + 1
        endif
    enddo

    return
end subroutine getnld

!-----------------------------------------------------------------------

subroutine get_next_lead_forwards(ivar, jt, chk, is_na)
    use msimob
    integer, intent(in) :: ivar, jt
    logical, intent(in) :: chk
    logical, intent(out) :: is_na

    integer :: j, maxlead, itime
    logical :: error

    !shift leads (do nothing if only 1 lead)
    do j = mdl%ibx2(ivar), mdl%ibx2(ivar + 1) - 2
        lags_leads(j) = lags_leads(j + 1)
    enddo

    ! set +longest lead (next period) for this variable
    maxlead = mdl%ibx2(ivar + 1) - mdl%ibx2(ivar)
    itime = jt + maxlead + 1

    call get_var_value(mws, ivar, itime, lags_leads(mdl%ibx2(ivar + 1) - 1),&
&                      error)

    if (chk) then
        is_na = nuifna(lags_leads(mdl%ibx2(ivar + 1) - 1))
        if (is_na) call simob3(ivar, maxlead)
    endif

end subroutine get_next_lead_forwards

!-----------------------------------------------------------------------

subroutine get_next_lead_backwards(ivar, jt, chk, is_na)
    use msimob
    integer, intent(in) :: ivar, jt
    logical, intent(in) :: chk
    logical, intent(out) :: is_na

    integer :: j

    ! shift leads (do nothing if only 1 lead)
    do j = mdl%ibx2(ivar + 1 ) - 1, mdl%ibx2(ivar) + 1, -1
        lags_leads(j) = lags_leads(j - 1)
    enddo

    ! set +1 lead for this variable for the next period,
    ! get the value from the model data at the current period
    lags_leads(mdl%ibx2(ivar)) = mws%mdl_data(ivar, jt + mdl%mxlag)

    if (chk) then
        is_na = nuifna(lags_leads(mdl%ibx2(ivar)))
        if (is_na) call simob3(ivar, 1)
    endif

end subroutine get_next_lead_backwards

!-----------------------------------------------------------------------

   subroutine store_ca(jt)
       integer, intent(in) :: jt

       ! store current CAs in the mws

       integer :: i, jtd

       jtd = jt + mdl%mxlag

       do i = 1, mdl%nca
           if (mdl%ica(i) > 0) mws%constant_adjustments(i, jtd) = &
                              ca(mdl%ica(i))
       enddo

       return
   end subroutine store_ca

!-----------------------------------------------------------------------

   subroutine retrieve_ca(jt)
       integer, intent(in) :: jt

       ! copy CAs from the mws to array ca

       integer :: i, jtd

       jtd = jt + mdl%mxlag

       do i = 1, mdl%nca
           if (mdl%ica(i) > 0) ca(mdl%ica(i)) =   &     
&               mws%constant_adjustments(i, jtd) 
           enddo

       return
   end subroutine retrieve_ca

!-----------------------------------------------------------------------

subroutine cpvecf(dst, src, n, lik)

!     conditional copy n elements from src vector to dst vector
!     only copy an element if lik(i) is .false.

use model_params
integer, intent(in) ::  n
real(kind = ISIS_RKIND), intent(in) :: src(*)
real(kind = ISIS_RKIND), intent(out) :: dst(*)
logical(kind = MC_LKIND), intent(in) :: lik(*)

integer ::  i

do i = 1, n
   if (.not. lik(i)) dst(i) = src(i)
enddo

return
end subroutine cpvecf

!-----------------------------------------------------------------------

subroutine cpvech(dst, src, n, lik)

! conditional copy n elements from src vector to dst vector
! only copy an element if lik(i) is .false. (exogenous)
!   XOR
! if source is not NA

use model_params
integer(kind = MC_IKIND), intent(in) :: n
real(kind = SOLVE_RKIND), intent(in) :: src(*)
real(kind = SOLVE_RKIND), intent(out) :: dst(*)
logical(kind = MC_IKIND), intent(in) :: lik(*)

integer ::  i

do i = 1, n
   if (.not. lik(i) ) then
      dst(i) = src(i)
   elseif (.not. nuifna(src(i))) then
      dst(i) = src(i)
   endif
enddo

return
end subroutine cpvech

!-----------------------------------------------------------------------

subroutine mszini(period)
integer, intent(in) :: period

!     Initialize curvars and set initial starting values of feedback variables
!     for the first solve period

!     start
!      C            strict current values only
!      P            strict previous values only but current if in
!                      first period of Model data period

!      K            relaxed current
!                     use current value if NOT NA else use previous

!      L            relaxed previous
!                     use previous value if NOT NA else use current

!     Must be careful in initializing if using current
!     must be done in such a way that if initial values
!     are a solution of the model then these should remain intact
!     so that later on algorithm can detect immediate convergence
!     avoiding unnecessary jacobian computations


real(kind = SOLVE_RKIND) ::  zx
integer :: fbnum, j
logical :: error

if (opts%start == 'C' ) then
    ! ------   current
    curvars = mws%mdl_data(:, period + mdl%mxlag)
elseif (opts%start == 'P' ) then
    ! ------   previous
    curvars =  mws%mdl_data(:, max(period - 1, 1) + mdl%mxlag)
elseif (opts%start == 'D' ) then
    ! ------   1. relaxed current
    !          2. redo feedback variables
    curvars = mws%mdl_data(:, period + mdl%mxlag)
    do j = 1, mdl%nfb
        fbnum = mdl%numfb(j)
        call get_var_value(mws, fbnum, period, zx, error)
        if (nuifna(zx)) then
            call get_var_value(mws, fbnum, period - 1, zx, error)
         endif
         curvars(fbnum) = zx
      enddo
elseif (opts%start == 'Q') then
    ! ------   1. relaxed previous
    !          2. redo feedback variables
    curvars = mws%mdl_data(:, max(period - 1, 1) + mdl%mxlag)
    do j = 1, mdl%nfb
        fbnum = mdl%numfb(j)
        call get_var_value(mws, fbnum, period - 1, zx, error)
        if (nuifna(zx)) then
            call get_var_value(mws, fbnum, period, zx, error)
        endif
        curvars(fbnum) = zx
  enddo

endif

return
end subroutine mszini

end module msutil
