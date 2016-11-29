module msfbvl
use msvars
use kinds

contains

subroutine msprlg(retcod)
    integer, intent(out) :: retcod
    ! run the so-called prologue equations of a model
    call fbvl(retcod, mdl%order, 1_MC_IKIND, mdl%loops - 1_MC_IKIND)
    return
end subroutine msprlg

subroutine msloop(retcod)
    integer, intent(out) :: retcod
    ! run the simultaneous equations of a model
    call fbvl(retcod, mdl%order, mdl%loops, mdl%loope)
    return
end subroutine msloop

subroutine mseplg(retcod)
    integer, intent(out) :: retcod
    ! run the epilogue equations of a model
    call fbvl(retcod, mdl%order, mdl%loope + 1_MC_IKIND, mdl%neq)
    return
end subroutine mseplg

!-----------------------------------------------------------------------

subroutine fbvl(retcod, order, ostart, oend)
use mscalc
use nuna

! run part of the equations of a model in specified order

! retcod
! 0  all ok
! 1  error

integer, intent(out) :: retcod
integer(kind = MC_IKIND), intent(in) ::  order(*), ostart, oend
real*8   result, result2, zsave, delta

integer ::  p, pstart, pend, eqnum, lhs
integer ::  xcod

integer, external ::  bysget
character*1 eqtyp
real(kind = SOLVE_RKIND), pointer, dimension(:) :: rhsvars

pstart = ostart
pend   = oend

retcod = 0

if (opts%mode == 'R') then
    rhsvars => yp
else
    rhsvars => curvars
endif

do p = pstart, pend

    ! skip equation if not in ordering or inactive

    eqnum = order(p)
    if (eqnum == 0) cycle
    eqtyp = char(bysget(mdl%etype, eqnum))
    if (ichar(eqtyp) > 96) cycle

    lhs = mdl%lhsnum(eqnum)

    call modequ(xcod, result, mdl%nrecp(eqnum) + 1_MC_IKIND,  &
&               mdl%nrecp(eqnum + 1))

    if (xcod == 0) then

       ! here a test should be inserted too see if result is out of range
       !  when default size real < sizeof real*8

       if (eqtyp == 'I') then

           ! identity
           curvars(lhs) = result

       else if (eqtyp == 'B') then

           ! behavioural equation
           ! if endogenous add constant adjustment else calculate ca

           if (mdl%lik(lhs)) then
               curvars(lhs) = result + ca(lhs)
           else
               curvars(lhs) = yp(lhs)
               ca(lhs)      = yp(lhs) - result
           endif

       else if( eqtyp == 'M' .and. .not. mdl%lik(lhs)) then

           ! implicit frml equation with exogenous left hand side

           curvars(lhs) = yp(lhs)
           ca(lhs)   = - result

       else if (eqtyp == 'M' .or. eqtyp == 'N') then

           ! implicit equation with endogenous left hand side
           ! calculate numerical derivative and do 1 newton step

           zsave  = rhsvars(lhs)
           delta  = get_delta(zsave)
           rhsvars(lhs) = zsave + delta

           ! avoid finite precision rounding errors (BHH)
           ! try to defeat "eigenwijze" optimize
           call donowt(delta, zsave)
           delta = rhsvars(lhs) - zsave

           call modequ(xcod, result2, mdl%nrecp(eqnum) + 1_MC_IKIND, &
&                      mdl%nrecp(eqnum + 1))
           if (xcod > 0 ) then
               continue
           else if(abs(result2-result) .lt. .000001 * delta) then
                 ! ** zero derivative **
                 xcod = 4
           else if (eqtyp == 'M') then
               result  = result  + ca(lhs)
               result2 = result2 + ca(lhs)
           endif

           ! restore rhs to original value
           rhsvars(lhs) = zsave

           if (xcod == 0) then
               ! store new approximation
               curvars(lhs) = zsave - delta / (result2 - result) * result
           endif
       else
           !  ** unknown equation type **
           xcod = 5
       endif

    endif

    if (xcod /= 0 ) then
        ! Error:  put NA in result
        if (mdl%lik(lhs)) then
            curvars(lhs) = NA
        else if( eqtyp .eq. 'B' .or. eqtyp .eq. 'M' ) then
            curvars(lhs) = yp(lhs)
            ca(lhs)   = NA
        endif
    endif

    if (xcod == 1 .or. xcod == 5) then

        ! ** 1: serious error
        ! ** 5: unknown equation type
        ! Exit immediately: cannot continue

        !call snwerr(xcod, eqnum)
        retcod = 1
        return

    elseif (xcod == 2 .or. xcod == 4 ) then

        ! ** 2: error **
        ! ** 4: zero derivative implicit equation **
        ! retcod = 2 ????

        !call snwerr(xcod, eqnum)

! !!!!  elseif( xcod .eq. 3 ) then

!            ** missing rhs value ***
! !!!!       call snwerr(3, eqnum)

    endif

enddo

return

    contains

        function get_delta(val)
             real(kind = SOLVE_RKIND) :: get_delta
             real(kind = SOLVE_RKIND), intent(in) :: val
             real(kind = SOLVE_RKIND), parameter :: eps = 0.0001_8
             get_delta =  max(eps, eps * abs(val))

        end function get_delta

!        subroutine snwerr(ier, iequ)
!        !use msimot
!        use  mdl_name_utils
!        integer, intent(in) :: ier, iequ
!
!        ! print error messages for errors detected in solve equation
!        ! called by msloop, msprlg, mseplg
!
!        integer ::  sotypq, sotypf
!        integer, external ::  bysget
!
!        if ((ier .eq. 1) .or. (ier .eq. 5)) then
!             sotypq = O_ERRQ
!             sotypf = O_ERRF
!        else
!             sotypq = O_WMSG
!             sotypf = O_WMSG
!        endif
!
!        call mcf7ex(name, nlen, mdl%ienames(iequ), mdl%enames)
!
!        select case (ier)
!
!        case (1)
!             write(str,'(2a)') '** Serious error in equation ', name(:nlen)
!
!        case (2)
!             write(str,'(2a)') '** Error in equation ',name(:nlen)
!
!        case (3)
!             return
!
!        case (4)
!             write(str,'(2a)') '** Zero derivative in implicit equation ', &
!&                 name(:nlen)
!
!        case (5)
!             write(str,'(a,i3)') '** Unknown equation type: code=', &
!&             bysget(mdl%etype,iequ)
!             call strout(sotypq)
!             write(str,'(2a)') '** in equation '          , name(:nlen)
!
!        case default
!            write(str,'(a,i4)') '** Unknown error code in snwerr', ier
!
!        end select
!
!        call strout(sotypf)
!
!        return
!        end subroutine snwerr

end subroutine fbvl

end module msfbvl
