module mssneq
    use msvars
    use mscalc
    
    contains

    subroutine msisng(xresult, iequ, jtime,ier)
        use nuna
        integer, intent(in) ::  iequ, jtime
        integer, intent(out) :: ier
        real(kind = SOLVE_RKIND), intent(out) :: xresult
        
        real(kind = SOLVE_RKIND) :: temp
        
        !     call single equation solver (onequ)
        !     xresult contains the result (or missing/invalid value if ier > 0 )
        
        !     ier   return status
        !      0    all ok
        !      1      syntax error in equation
        !      2      computational problems
        !      3      missing rhs variable
        
        ier = 0
        
        call onequ(ier, temp, mdl%nrecp(iequ) + 1_MC_IKIND, mdl%nrecp(iequ+1), &
                   jtime)
        if (ier /= 0) then
           xresult = NA
        else
           xresult = temp
        endif
        
        return
    end subroutine msisng

    !---------------------------------------------------------------------------

    subroutine msinwt(xresult, lhs, jca, iequ, jtime, ier)
        
        ! solve a single (implicit) equation with Newton
        ! and numerical derivatives
        
        ! xresult contains solution for lhs variable on exit
        ! missing/invalid value if ier > 0 and ier <=3 or ier == 7
        ! last value in all other cases
        
        ! jca  if not 0 then index in constant_adjustments
        
        ! ier     return status
        !   0      all ok (convergence)
        !   1      syntax error in equation
        !   2      computational problems
        !   3      missing rhs variable
        !   4      not converged within 50 iterations
        !   5      zero derivative
        !   6      function value not converged but rhs has converged
        !   7      missing initial value for implicit variable
      
        use nuna
        integer, intent(in)  ::  lhs, jca, iequ
        integer, intent(in)  ::  jtime
        integer, intent(out) :: ier
        real(kind = SOLVE_RKIND), intent(out) :: xresult

        real(kind = SOLVE_RKIND) :: result, result2, delres, caval
        real(kind = SOLVE_RKIND) :: xsave, zsave, zsave2, delta
        real(kind = SOLVE_RKIND), pointer  :: xdst
        
        ! loop for implicit equation with lik(lhs)=true
        ! for type M (implicit frml) add CA to result
        
        real(kind = SOLVE_RKIND), parameter :: DELEPS = 1d-2, DRVEPS = 1d-6, &
                                               RESTOL = 1d-4
        real(kind = SOLVE_RKIND), parameter :: Qone = 1.0d0
        integer ::  iter
        logical ::    error
        integer(kind = MC_IKIND) :: ipb, ipe
        
        ier = 0

        if (jtime <= 0) then
            xdst => mws%lags(lhs, jtime + mws%mdl%mxlag) 
        elseif (jtime <= mws%perlen) then
            xdst => mws%mdl_data(lhs, jtime)
        else
            xdst => mws%leads(lhs, jtime - mws%perlen) 
        endif
        
        ! test if xdst contains valid starting value
        ! if not try 1 period lagged value
        ! if still not valid then quit with ier = 7
        
        if (nuifna(xdst)) then
           call get_var_value(mws, lhs, jtime - 1, xsave, error)
           if (nuifna(xsave)) then
               xresult = xsave
               ier = 7
               return
           endif
           xdst = xsave
        endif
        
        ! save current value of destination
        ! for restoring at end of loop
        
        xsave = xdst
        zsave = xdst
        
        if (jca /= 0) caval = mws%constant_adjustments(jca, jtime)
        
        ipb = mdl%nrecp(iequ) + 1
        ipe = mdl%nrecp(iequ + 1)
        
        do  iter= 1, 50
        
           call onequ(ier, result, ipb, ipe, jtime)
           if (ier /= 0) goto 600
           if (jca /= 0) result = result + caval
        
           ! get current value of implicit lhs variable
        
           zsave = xdst
        
           delta = DELEPS + DELEPS * abs(zsave)
        
           xdst = zsave + delta
        
           call onequ(ier, result2, ipb, ipe, jtime)
           if( ier .ne. 0 ) goto 600
        
           ! avoid finite precision errors
        
           delta = xdst - zsave
        
           if( jca .ne. 0 ) result2 = result2 + caval
        
           delres = result2 - result
           if( abs(delres) .lt. DRVEPS*delta ) goto 150
        
           zsave2 = zsave - delta/delres * result
        
           ! test for convergence of x value
           if (abs(zsave) < Qone) then
               if (abs(zsave2 - zsave) <= mws%test(lhs)) goto 160
           else
               if (abs((zsave2-zsave)/zsave) <= mws%test(lhs)) goto 160
           endif
        
           xdst = zsave2
        
        enddo
        ier = 4
        goto 300
        
  150   ier = 5
        goto 300
        
  160   continue
        ! make sure result has also converged
        if (abs(result) > RESTOL) ier = 6
        
  300   continue
        ! store result
        
        xresult = zsave
        goto 800
        
        ! error returns from onequ
        ! store missing/invalid in xresult and restore initial value of xdst
  600   xresult = NA
      
        ! restore initial value of xdst
      
  800   xdst = xsave
        
        return
    end subroutine msinwt

!-----------------------------------------------------------------------

subroutine msverr(ier, jtime, iequ)
use mdl_name_utils
use msimot

!     print error message for single equation solver

integer :: ier, jtime, iequ
logical :: print_msg

character(len = 8) :: stj

call mcf7ex(name, nlen, mdl%ienames(iequ), mdl%enames)
call sjttmp( stj, jtime)

print_msg = .true.

select case (ier)

case (1)
    write(str, '(2a)' ) 'in equation: ',name(:nlen)
    call strout(O_ERRQ)
    str = 'quitting this equation'

case (2)
    ! computational error (message already printed)
    print_msg = .false.

case (3) 
    !  missing rhs (no message)
    print_msg = .false.

case (4)
    str = 'No convergence in 50 iterations'

case (5)
    str = 'Zero derivative in implicit equation'

case (6)
    str = 'Implicit equation has not converged'
    call strout(O_ERRQ)
    str = 'No better point found'

case (7)
    str = 'Missing/invalid initial value in implicit equation'

case default
    return

end select

if (print_msg) call strout(O_ERRF)

write(str, '(4a)') 'in period ',stj,' for equation: ',name(:nlen)
call strout(O_ERRF)

return
end subroutine msverr

end module mssneq
