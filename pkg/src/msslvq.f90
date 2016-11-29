module msslvq

contains

subroutine solve_equation(eqnum, update,  jt1, jt2, errflg)
use msvars
use mssneq
use nuna
use mdlvars
 
! solves equation eqnum for all periods between jt1 and jt2
! if update == .true.  then do not store NA or missing result
! if update == .false. then result is always stored

integer, intent(in) :: eqnum, jt1, jt2
logical, intent(in) :: update
integer, intent(out):: errflg

real(kind = SOLVE_RKIND) :: result
integer ::        lhs,jca, jtime
integer ::        ier

logical  :: error, old_lik
integer, external ::  bysget
character(len = 1) :: eqtype
type(mdl_variable) :: fix_var
real(kind = SOLVE_RKIND) :: fix_value

errflg = 0

eqtype = char(bysget(mdl%etype, eqnum))
if(ichar(eqtype) .gt. 96) then
   errflg = 1
   goto 1000
endif

lhs = mdl%lhsnum(eqnum)
fix_var = find_mdl_var(mws%fix_vars, lhs)

! set jca to the index of the constant adjustment if any

jca = mdl%aci(lhs)
if( jca .ne. 0 ) then
   if (mdl%ica(jca) .lt. 0 ) then
      jca = 0
   endif
endif

do jtime = jt1, jt2

   if (.not. period_ok(mws, lhs, jtime)) cycle

   if (fix_var%var_index == lhs) then
       old_lik = mdl%lik(lhs)
       fix_value = get_mdl_var_value(fix_var, jtime)
       if (.not. nuifna(fix_value)) then
           mdl%lik(lhs) = .false.
           mws%mdl_data(lhs, jtime) = fix_value
       endif
   endif

   ! now test behavioural equation
   if (eqtype == 'B' .or. eqtype == 'M') then
       ! If outside main period no valid result is possible
       ! but if lhs is endogenous then
       ! it may be required to store invalid lhs
       ! if lhs is fixed nothing to store (no CA storage available)
       if (jtime < 1 .or. jtime > mws%perlen) then
           if (mdl%lik(lhs) ) then
               goto 480
           else
               cycle
           endif
       else if (mdl%lik(lhs)) then
           ! if lhs is endogenous ca() must be valid
           if (nuifna(mws%constant_adjustments(jca, jtime))) goto 480
       else
           ! if lhs is fixed lhs must be valid
           if (nuifna(mws%mdl_data(lhs, jtime))) goto 480
       endif

   endif

   if (eqtype == 'I') then
      ! identity
      call msisng(result, eqnum, jtime, ier)

   else if (eqtype == 'B') then

       ! behavioural explicit equation
       ! if lhs is endogenous then add CA to result
       ! if lhs is fixed      then calculate CA

      call msisng(result, eqnum, jtime, ier)
      if (ier == 0) then
          if (mdl%lik(lhs)) then
              result = result + mws%constant_adjustments(jca, jtime)
          else
              result = mws%mdl_data(lhs, jtime) - result
          endif
      endif

   else if (eqtype == 'M') then

       ! implicit behavioural equation
       ! if lhs is fixed then CA = - rhs (excluding CA)
       ! else newton solve equation
      if (mdl%lik(lhs) ) then
          call msinwt(result, lhs, jca, eqnum, jtime, ier)
      else
          call msisng(result,eqnum, jtime, ier)
          if( ier .eq. 0 ) result = - result
      endif

   else if (eqtype == 'N') then
       ! implicit identity (solve with newton)
       call msinwt(result, lhs, 0, eqnum, jtime, ier)
   endif

   ! result holds solution
   if (ier == 0) then
       goto 490
   else if( ier .ne. 3 ) then
       ! no message for normal missing rhs
       call msverr(ier, jtime, eqnum)
       if (ier > 3 ) goto 490
       if (ier == 1 ) goto 1000
   endif

!  FALL THROUGH for ier == 3 (missing rhs)

480 continue
   !  missing or NA in rhs
   if (update) cycle
   result = NA

490 continue
   ! store result
   if (mdl%lik(lhs)) then
       call set_var_value(mws, lhs, jtime, result, error)
   else
       mws%constant_adjustments(jca, jtime) = result
   endif

   if (fix_var%var_index == lhs) then
       mdl%lik(lhs) = old_lik
   endif

end do

1000 continue

return
end subroutine solve_equation

end module msslvq
