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

    if (opts%repopt == REP_NONE) return
    
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
    
    if (opts%repopt == REP_NONE) return

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
    if (opts%repopt == REP_NONE) return
    
    write(str,'(i5,2a)') nonval, &
    &    ' inconsistent/missing/invalid fit targets/CA detected'
    call strout(O_ERRF)
    
    return
end subroutine fitot2

subroutine fitot3(errcode)
    integer, intent(in) :: errcode

    if (opts%repopt == REP_NONE) return
    
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

subroutine fitot4(fiter, newjac, dcond, delwmx, dlwmxp, wmxidx, &
                  wmxtyp, delsmx, delsmxp, smxidx, smxtyp, deltyp, prihdr)
    use mdl_name_utils
    
    ! print progress of current fit
    integer, intent(in) :: fiter, wmxidx, smxidx, wmxtyp, smxtyp, deltyp
    logical, intent(in) :: newjac, prihdr
    real(kind = SOLVE_RKIND), intent(in) :: dcond, delwmx, dlwmxp, delsmx, &
                                            delsmxp
    
    integer :: clen, hdrlen, rpos
    character(len = 6), target:: chdr_z(8), chdr_lazy(6)
    integer, target :: chln_z(8), cwid_z(8), chln_lazy(6), cwid_lazy(6)
    
    character(len = 6), dimension(:), pointer:: chdr
    integer, dimension(:), pointer :: chln, cwid
    
    integer :: k, mxtyp, mxidx
    logical :: zealous
    
    save chdr, chln, cwid, hdrlen
    
    ! headers for the zealous fit procedure
    data chdr_z / 'Fiter' , 'Icond' , 'Delwmx' , 'Delsmx' , 'Deltyp' , 'Ratio' , 'Type' , 'Name' /
    data chln_z /    5    ,    5    ,    6     ,   6      ,   6      ,   5     ,   4    ,   4    /
    data cwid_z /    6    ,    9    ,  NUMWID  ,  NUMWID  ,   6      , NUMWID  ,   4    ,   0    /
    
    ! headers for the lazy fit procedure
    data chdr_lazy / 'Fiter' , 'Icond' , 'Ratio'  , 'Delwmx' , 'Type' , 'Name' /
    data chln_lazy /    5    ,    5    ,    5     ,    6     ,   4    ,   4    /
    data cwid_lazy /    6    ,    9    ,  NUMWID  ,   NUMWID ,   4    ,   0    /

    if (opts%repopt == REP_NONE) return

    zealous = opts%fit%zealous
    
    mxidx = wmxidx
    mxtyp = wmxtyp
    if (deltyp == 2) then
        mxidx = smxidx
        mxtyp = smxtyp
    endif
    
    if (prihdr) then
       call strini(' ', 1)
       if (zealous) then
           chdr => chdr_z
           chln => chln_z
           cwid => cwid_z
       else 
           chdr => chdr_lazy
           chln => chln_lazy
           cwid => cwid_lazy
       endif
       do  k = 1, size(chdr)
           clen = chln(k)
           rpos = spos + max(cwid(k) - clen, 0)
           str(rpos : rpos + clen - 1) = chdr(k)(:clen)
           if (cwid(k) /= 0) spos = spos + cwid(k) + ICSPAC
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
    
    ! ratio for lazt
    if (.not. zealous) then
        if (dlwmxp /= 0.0 ) then
            call nvlfmt(delwmx / dlwmxp, str(spos : spos + NUMWID - 1))
        endif
        spos = spos + NUMWID + ICSPAC
    endif
    
    ! delwmx
    call nvlfmt(delwmx, str(spos : spos + NUMWID - 1))
    spos = spos + NUMWID + ICSPAC
    
    if (zealous) then
        ! delsmx
        if (delsmx .ge. 0_ISIS_RKIND) then
           ! maximum change of residual
           call nvlfmt(delsmx, str(spos : spos + NUMWID - 1))
        endif
        spos = spos + NUMWID + ICSPAC
        
        ! deltyp
        if (deltyp == 1) then
            str(spos : spos) = 'w'
        else
            str(spos : spos) = 's'
        endif
        spos = spos + 6 + ICSPAC
    endif
    
    ! ratio for zealous
    if (zealous) then
        if (deltyp == 1 .and. dlwmxp /= 0_ISIS_RKIND)  then
            call nvlfmt(delwmx / dlwmxp, str(spos : spos + NUMWID - 1))
        elseif (deltyp == 2 .and. delsmxp /= 0_ISIS_RKIND)  then
            call nvlfmt(delsmx / delsmxp, str(spos : spos + NUMWID - 1))
        endif
        spos = spos + NUMWID + ICSPAC
    endif
    
    
    ! type
    if (mxtyp == 1) then
        str(spos : spos + 3 - 1 ) = 'Abs'
    elseif (mxtyp == 2) then
        str(spos : spos + 3 - 1 ) = 'Rel'
    endif
    spos = spos + 4 + ICSPAC
    
    if (mxidx .ne. 0 ) then
        call mcf7ex(name, nlen, mdl%ivnames(mxidx), mdl%vnames)
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

    if (opts%repopt == REP_NONE) return
    
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

    if (opts%repopt == REP_NONE) return
    
    call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
    
    write(str, '(3a,f8.5)', round = 'compatible') 'Ca of ', &
    &     name(:nlen), ' possibly too big; criterium ', crit
    
    call strout(O_WMSG)
    
    return
end subroutine fitot6

subroutine fitot7(nbig)
    integer, intent(in) ::  nbig

    if (opts%repopt == REP_NONE) return
    
    write(str, '(i5,a)') nbig, ' residuals > threshold'
    call strout(O_WMSG)
    return
end subroutine fitot7

subroutine fitot8(ier,dcond)
    use nucnst
    
    ! print message about bad condition of Fit D matrix
    
    integer, intent(in) ::  ier
    real(kind = SOLVE_RKIND), intent(in) :: dcond

    if (opts%repopt == REP_NONE) return
    
    if (ier == 2) then
       str = 'Fit Error - D matrix is rank deficient (the inverse condition is exactly zero).'
       call strout(O_ERRQ)
    elseif (ier == 1) then
       str = 'Fit Warning - D matrix is ill conditioned.'
       call strout(O_ERRQ)
       write(str,'(1a,1p,e9.2,1a,1p,e9.2)', round = 'compatible') &
    &   'Inverse condition= ', dcond, ' < Machine prec**.5= ', sqrt(Rmeps)
       call strout(O_ERRQ)
    endif
    str = 'Derivatives of fit targets are dependent or'
    call strout(O_ERRQ)
    str = "for one or more fit targets all derivatives are (almost) zero ..."
    call strout(O_ERRF)
    str = "Tip: try to use svd analysis. See the documentation of method set_fit_options."
    call strout(O_ERRF)
    return
end subroutine fitot8

subroutine fitot9(fiter, fcvgd, djcnt, maxiter, matitr, nu)
    use kinds
    integer, intent(in) ::  fiter, djcnt, maxiter, matitr
    logical, intent(in) ::  fcvgd
    integer(kind = SOLVE_IKIND), intent(in) :: nu

    if (opts%repopt == REP_NONE) return
    
    if (fcvgd) then
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

    if (matitr > 0) then
        write(str, '(a,i6)') 'Total number of iterations for accurate ' // &
                             'calculation fit Jacobian ', matitr
        call strout(O_OUTN)

        write(str, '(a,g4.1)', round = 'compatible') &
          'Average number of iterations per column ', matitr / (nu * djcnt)
       call strout(O_OUTN)

    endif

    
    return
end subroutine fitot9

subroutine fitot10(fiter)
    
    ! Fit jacobian is invalid numbers
    
    integer, intent(in) :: fiter
    
    if (opts%repopt == REP_NONE) return
    
    write(str, '(a,i4)') 'The fit jacobian contains invalid values at iteration', &
    & fiter
    call strout(O_ERRQ)
    if (.not. mws%dbgeqn) then
      str = '** Suggestion: use mdl$set_debug_eqn(TRUE)'
      call strout(O_ERRF)
    endif
    
    return
end subroutine fitot10

subroutine fitot11
    
    if (opts%repopt == REP_NONE) return

    str = 'Not enough memory for the fit procedure'
    call strout(O_ERRM)
    return
end subroutine fitot11

subroutine fitot13
    
    if (opts%repopt == REP_NONE) return
    
    write(str, '(a,i4)') &
    &  'Numerical problem during the calculation of the fit Jacobian'
    call strout(O_ERRQ)
    if (opts%fit%accurate_jac) then
        write(str, '(a,i4)') 'Possibly no convergence when solving the model'
        call strout(O_ERRQ)
    endif
end subroutine fitot13

subroutine fitot14
    
    if (opts%repopt == REP_NONE) return
    
    str = 'Not enough memory to allocate the fit jacobian'
    call strout(O_ERRM)
    return
end subroutine fitot14

subroutine fitot15
    
    if (opts%repopt == REP_NONE) return
    
    str = 'Not enough memory for the svd analysis of the fit jacobian'
    call strout(O_ERRM)
    return
end subroutine fitot15

subroutine fitotc_invalid(iv)
    use mdl_name_utils
    
    ! print message about invalid values in column of derivatives in fit jacobian
    
    integer(kind = SOLVE_IKIND) :: iv
    
    if (opts%repopt == REP_NONE) return
    
    call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
    
    write(str, '(3a)' ) 'Warning: Some derivatives for fit target ', name(:nlen), &
    &                   ' contains invalid values'
    
    call strout(O_WMSG)
    
    return
end subroutine fitotc_invalid

subroutine fitotc(iv, l1_norm)
    use mdl_name_utils
    integer(kind = SOLVE_IKIND), intent(in) :: iv
    real(kind = SOLVE_RKIND), intent(in) :: l1_norm

    ! print message about zero column of derivatives in dj (the transpose of the fit jacobian)

    
    if (opts%repopt == REP_NONE) return

    call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)

    if (l1_norm == 0) then
        write(str, '(3a)') 'Error: All derivatives of fit target ',  &
              name(:nlen), ' are exactly zero.'
    else 
        write(str, '(3a, g10.2)') 'Warning: Derivatives of fit target ',  &
              name(:nlen), ' are almost zero. L1-norm: ', &
                  l1_norm
    endif
    
    call strout(O_WMSG)
    
    return
end subroutine fitotc

subroutine fitotr(iv, l1_norm)
    use mdl_name_utils
    integer(kind = SOLVE_IKIND), intent(in) :: iv
    real(kind = SOLVE_RKIND), intent(in) :: l1_norm

    ! print message about zero row of derivatives in dj (the transpose of the fit jacobian)

    
    if (opts%repopt == REP_NONE) return

    call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)

    if (l1_norm == 0) then
        write(str, '(3a)') 'Warning: All derivatives with respect to fit instrument ', &
              name(:nlen), ' are exactly zero.'
    else 
        write(str, '(3a, g10.2)') 'Warning: Derivatives with respect to fit instrument ',  &
              name(:nlen), ' are almost zero. L1-norm: ', &
                  l1_norm
    endif
    
    call strout(O_WMSG)
    
    return
end subroutine fitotr
    
subroutine fitonu(nu, numu)
    integer(kind = SOLVE_IKIND) :: nu, numu(*)
    
    ! Output names of CAs used by fit procedure
    ! uses a specialised name output routine
    
    character(len = 25) :: hdr
    
    if (opts%repopt == REP_NONE) return
    
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
    
    if (opts%repopt == REP_NONE) return
    
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
    
    if (opts%repopt == REP_NONE) return
    
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
    
    if (opts%repopt == REP_NONE) return
    
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
