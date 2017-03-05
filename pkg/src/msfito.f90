!, Output for fit procedure

module msfito
use msimot

contains

subroutine fitot0(outset)
    integer, intent(in) ::  outset
    ! set repopt parameter for output of normal solve during fit procedure
    !  outset == 1 then set for fit procedure
    !  else        then reset to opts%repopt
    if (outset == 1) then
        if (opts%fit%supsot) repopt = REP_MINIMAL
    else 
        repopt = opts%repopt
    endif
return
end subroutine fitot0

subroutine fitota(iv)
    use mdl_name_utils
    integer(kind = SOLVE_IKIND), intent(in) :: iv
    
    ! print message for endogenous fit CA
    ! vnptr is name pointer
    
    call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
    write(str, '(3a)' ) 'WARNING: Fit CA for ', name(:nlen) , &
    &         ' is endogenous and not used in the fit procedure'
    call strout(O_WMSG)
    return
end subroutine fitota

subroutine fitotb(iv)
    use mdl_name_utils
    integer(kind = SOLVE_IKIND), intent(in) :: iv
    
    ! print message for exogenous fit target
    ! vnptr is name pointer
    
    call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
    write(str,'(2a)') 'Fit target should be endogenous ',name(:nlen)
    call strout(O_ERRM)
    
    return
end subroutine fitotb

subroutine fitot2(nonval)
    integer, intent(in) :: nonval
    
    ! print message for total of missing fit targets
    
    write(str,'(i5,2a)') nonval, &
    &    ' inconsistent/missing/invalid fit targets/CA detected'
    call strout(O_ERRF)
    
    return
end subroutine fitot2

subroutine fitot3(errcode)
    integer, intent(in) :: errcode
    
    if( errcode .eq. 2 ) then
        str = 'No fit targets found for this period'
    else if( errcode .eq. 3 ) then
        str = 'No CAs available for Fit'
    else if( errcode .eq. 4 ) then
        str = 'Not enough endogenous CAs for Fit'
    else if( errcode .eq. 5 ) then
        str = 'Only Newton/Broyden method allowed for Fit'
    endif
    
    if (errcode .ne. 2) then
       call strout(O_ERRM)
    else
       call strout(O_WMSG)
    endif
    
    str = 'Fit procedure will not be executed in ' // perstr
    if (errcode .ne. 2) then
        call strout(O_ERRF)
    else
        call strout(O_WMSG)
    endif
    
end subroutine fitot3

subroutine fitot4(fiter,newjac,dcond,delwmx,dlwmxp,iv, wmxtyp,prihdr)
    use mdl_name_utils
    
    ! print progress of current fit
    
    integer ::  fiter,iv,wmxtyp
    logical ::  newjac,prihdr
    real(kind = SOLVE_RKIND) :: dcond, delwmx, dlwmxp
    
    integer ::       clen, hdrlen, rpos
    character(len = 6) :: chdr(6)
    integer ::       chln(6), cwid(6)
    integer ::       k
    
    save chdr, chln, cwid, hdrlen
    
    data chdr / 'Fiter' , 'Icond', 'Ratio', 'Delwmx', 'Type' , 'Name'/
    data chln /    5    ,    5   ,    5   ,    6    ,   4    ,   4   /
    data cwid /    6    ,    9   , NUMWID , NUMWID  ,   4    ,   0   /
    
    if( prihdr ) then
       call strini(' ', 1)
       do  k=1,6
           clen = chln(k)
           rpos = spos + max(cwid(k) - clen, 0)
           str(rpos : rpos + clen - 1) = chdr(k)(:clen)
           if( cwid(k) .ne. 0 ) then
               spos = spos + cwid(k) + ICSPAC
           endif
       enddo
       hdrlen = spos
       call strout(O_OUTN)
    endif
    
    call strini(' ', 1 )
    write(str, '(i6)' ) fiter
    spos = 1 + cwid(1)
    if( newjac ) then
       str(spos:spos) = 'D'
       spos = spos + ICSPAC
       write(str(spos:spos + 9 - 1),'(1p,e9.2)', round = 'compatible') dcond
    else
       spos = spos + ICSPAC
    endif
    spos = spos + 9 + ICSPAC
    
    if( dlwmxp .ne. 0.0 ) then
       call nvlfmt(delwmx/dlwmxp, str(spos : spos + NUMWID - 1))
    endif
    spos = spos + NUMWID + ICSPAC
    
    call nvlfmt(delwmx, str(spos : spos + NUMWID - 1))
    spos = spos + NUMWID + ICSPAC
    
    if( wmxtyp .eq. 1 ) then
        str(spos : spos + 3 - 1 ) = 'Abs'
    elseif( wmxtyp .eq. 2 ) then
        str(spos : spos + 3 - 1 ) = 'Rel'
    endif
    spos = spos + 4 + ICSPAC
    
    if (iv .ne. 0 ) then
        call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
        if( hdrlen + nlen .gt. lwidth ) then
            call strout(O_OUTN)
            call strini( ' ', lwidth - nlen - 1)
        endif
        str(spos : spos + nlen - 1) = name(:nlen)
    endif
    
    call strout(O_OUTN)
    
    return
end subroutine fitot4

subroutine fitot5(relcvg)

    ! relative convergence very bad
    ! no better point can be found
    
    real(kind = SOLVE_RKIND) :: relcvg
    
    write(str,'(a,f5.3)', round = 'compatible') &
    &     'Fit relative convergence above ',relcvg
    call strout(O_ERRF)
    write(str,'(a)' ) 'Cannot locate a better point'
    call strout(O_ERRF)
    
        return
end subroutine fitot5
    
subroutine fitot6(iv, crit)
    use mdl_name_utils
    
    !     print warning about relatively large CA
    
    integer(kind = SOLVE_IKIND) :: iv
    real(kind = SOLVE_RKIND) ::  crit
    
    call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
    
    write(str, '(3a,f8.5)', round = 'compatible') 'Ca of ', &
    &     name(:nlen), ' possibly too big; criterium ', crit
    
    call strout(O_WMSG)
    
    return
end subroutine fitot6

subroutine fitot7(nbig)
    integer, intent(in) ::  nbig
    write(str, '(i5,a)') nbig, ' residuals > threshold'
    call strout(O_WMSG)
    return
end subroutine fitot7

subroutine fitot8(ier,dcond)
    use nucnst
    
    ! print message about bad condition of Fit D matrix
    
    integer, intent(in) ::  ier
    real(kind = SOLVE_RKIND), intent(in) :: dcond
    
    if( ier .eq. 2 ) then
       str = 'Fit Error - total loss of precision of D matrix'
       call strout(O_ERRQ)
    elseif(ier .eq. 1) then
       str = 'Fit Warning - D matrix is ill conditioned'
       call strout(O_ERRQ)
       write(str,'(1a,1p,e9.2,1a,1p,e9.2)', round = 'compatible') &
    &   'Inverse condition= ', dcond, ' < Machine prec**.5= ', sqrt(Rmeps)
       call strout(O_ERRQ)
       str = 'Derivatives of fit targets are dependent ...'
       call strout(O_ERRF)
    endif
    return
end subroutine fitot8

subroutine fitot9(fiter,fcvgd,djcnt,maxiter)
    integer ::  fiter, djcnt, maxiter
    logical ::  fcvgd
    
    if( fcvgd ) then
       write(str,'(3a,i4,2a,i4,a)' ) 'Fit convergence in ',perstr , &
                                    ' after ',fiter,' iterations', &
                                    ' and '  ,djcnt,' jacobians'
       call strout(O_OUTN)
    else
       write(str,'(3a,i4,2a,i4,a)' ) 'NO Fit convergence in ',perstr , &
                                    ' after ',fiter,' iterations', &
                                    ' and '  ,djcnt,' jacobians'
       call strout(O_OUTN)
       if (fiter >= maxiter) then
          write(str,'(a)' ) 'Maximum number of iterations reached!'
          call strout(O_OUTN)
       endif
    endif
    
    return
end subroutine fitot9

subroutine fitot10(fiter)
    
    ! Fit jacobian is invalid numbers
    
    integer, intent(in) :: fiter
    
    write(str, '(a,i4)') 'The fit jacobian contains invalid values at iteration', &
    & fiter
    call strout(O_ERRQ)
    if (.not. mws%dbgeqn) then
      str = '** Suggestion: use set_solve_debugmode(dbgon);'
      call strout(O_ERRF)
    endif
    
    return
end subroutine fitot10

subroutine fitot11


    str = 'Not enough memory for the fit procedure'
    call strout(O_ERRM)
    return
end subroutine fitot11

subroutine fitot12(matitr, nu)
    ! Output the number of iterations required to calculate the Jacobian
    use kinds
    integer, intent(in) :: matitr
    integer(kind = SOLVE_IKIND), intent(in) :: nu
    
    write(str, '(a,i4)') 'Total number of iterations for accurate calculation fit' // &
    &  ' Jacobian', matitr
    call strout(O_OUTN)
    write(str, '(a,g4.1)', round = 'compatible') &
    &  'Average number of iterations per column', matitr / nu
    call strout(O_OUTN)
    
    return
end subroutine fitot12

subroutine fitot13
    write(str, '(a,i4)') &
    &  'Numerical problem during the calculation of the fit Jacobian'
    call strout(O_ERRQ)
    if (opts%fit%accurate_jac) then
        write(str, '(a,i4)') 'Possibly no convergence when solving the model'
        call strout(O_ERRQ)
    endif
end subroutine fitot13

subroutine fitot14
    str = 'Not enough memory to allocate the fit jacobian'
    call strout(O_ERRM)
    return
end subroutine fitot14

subroutine fitot15
    str = 'Not enough memory for the svd analysis of the fit jacobian'
    call strout(O_ERRM)
    return
end subroutine fitot15

subroutine fitotc_invalid(iv)
    use mdl_name_utils
    
    ! print message about invalid values in column of derivatives in fit jacobian
    
    integer(kind = SOLVE_IKIND) :: iv
    
    call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
    
    write(str, '(3a)' ) 'Warning: Column of derivative for ', name(:nlen), &
    &                   ' contains invalid values'
    
    call strout(O_WMSG)
    
    return
end subroutine fitotc_invalid

subroutine fitotc(iv)
    use mdl_name_utils

    ! print message about zero column of derivatives in fit jacobian

    integer(kind = SOLVE_IKIND) :: iv

    call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)

    write(str, '(3a)' ) 'Warning: Column of derivative for ', name(:nlen), &
&                   ' consists of (almost) zero entries'
    
    call strout(O_WMSG)
    
    return
end subroutine fitotc
    
subroutine fitonu(nu, numu)
    integer(kind = SOLVE_IKIND) :: nu, numu(*)
    
    ! Output names of CAs used by fit procedure
    ! uses a specialised name output routine
    
    character(len = 25) :: hdr
    
    write(hdr,'(i5,a)') nu,' CAs used by Fit'
    
    call msnmot(nu,  numu(:nu), mdl%ivnames, mdl%vnames, hdr)
    
    return
end subroutine fitonu

subroutine fitonu_fix(nu, numu)
    integer(kind = SOLVE_IKIND) :: nu, numu(*)
    
    ! Output names of CAs used by fit procedure.
    ! uses a specialised name output routine.
    ! This verion is used when the fit CAs have changed
    ! because some CAs were disabled by the fit procedure.
    
    write(str,'(a)') '----> List of CAs used by Fit changed ' // &
    &   ' because some variables are fixed/unfixed '
    call strout(O_OUTB)
    
    call fitonu(nu, numu)
    
    return
end subroutine fitonu_fix

subroutine fitodj(dj, fiter, numw, numu, nw, nu, nu_max)
    integer(kind = SOLVE_IKIND), intent(in) :: numw(*), numu(*), nw, nu, nu_max
    real(kind = SOLVE_RKIND), intent(in) :: dj(nu_max, nw)
    integer, intent(in) :: fiter
    
    ! output of D matrix
    
    character(len = 80) :: dhdr
    
    write(dhdr,'(4a,i4)') 'Transpose of matrix D (scaled with rms)', &
    & ' in period ', perstr, ' at iteration ',fiter
    
    call matotn(dj, nu_max, nu, nw, numu(:nu), numw(:nw), trim(dhdr))
    
    call strini(' ', 1)
    call strout(O_OUTB)
    
    
    return
end subroutine fitodj

subroutine fitoca(delu, numu, nu,fiter)
    use mdl_name_utils
    
    real(kind = SOLVE_RKIND), intent(in) ::  delu(*)
    integer, intent(in) :: fiter
    integer(kind = SOLVE_IKIND), intent(in) :: numu(*), nu
    
    integer ::   maxlen
    
    integer ::   i, ires
    
    integer ::   colwid, rpos
    
    real(kind = SOLVE_RKIND) :: temp(2)
    character(len = 12), save :: chdr(2)
    data chdr / 'New value', 'Change' /
    
    colwid = max(NUMWID, 12)
    
    maxlen = maxnli(mdl%ivnames, int(numu(:nu), MC_IKIND), &
    &               1_MC_IKIND, int(nu, MC_IKIND))
    
    call strini( ' ', 1 )
    call strout(O_OUTB)
    
    write(str,'(3a,i4)') 'CA table in period ', perstr, ' at iteration', fiter
    call strout(O_OUTB)
    
    call strini(' ', 1 + maxlen)
    
    do  i= 1, 2
       nlen = len_trim(chdr(i))
       rpos = spos + colwid - nlen
       str(rpos : rpos + nlen - 1) = chdr(i)
       spos = spos + colwid + ICSPAC
    enddo
    call strout(O_OUTB)
    
    call strini(' ', 1)
    call strout(O_OUTB)
    
    do  i = 1, nu
    
       ires = numu(i)
       call mcf7ex(name, nlen, mdl%ivnames(ires), mdl%vnames)
       call strini(name(:nlen), 1 + maxlen)
       temp(1) = ca(ires)
       temp(2) = delu(i)
    
       call svlfmt(temp,1,2,colwid)
       call strout(O_OUTB)
    
    enddo
    
    return
end subroutine fitoca

end module msfito
