 
!     all output from solone subroutine and it's callee's
 
module msolot
    use msimot
    use msvars
    private :: svdhdr, svdln

contains

    !-----------------------------------------------------------------------

    subroutine solot1
        ! initialize local output
        call simot1
        return
    end subroutine solot1

!-----------------------------------------------------------------------

subroutine solot2(iv)
use mdl_name_utils

! print message for invalid/missing exogenous variable
! iv is variable index

integer ::   iv

call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
write(str,'(2a)') 'Missing or invalid value for exogenous variable ', name(:nlen)
call strout(O_ERRM)
return
end subroutine solot2

!-----------------------------------------------------------------------

subroutine solot3(iv)
use mdl_name_utils

! print message for invalid/missing constant adjustment
! iv is variable index

integer ::   iv

call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
write(str,'(2a)') 'Missing or invalid value for CA of ',name(:nlen)
call strout(O_ERRM)
return
end subroutine solot3

!-----------------------------------------------------------------------

subroutine solot4(nonval)
! print total number of invalid/missing exo/ca

integer ::  nonval

write(str,'(i5,2a)') nonval,' missing or invalid exos or CAs in period ', perstr
call strout(O_ERRF)
return
end subroutine solot4

!-----------------------------------------------------------------------

subroutine solot5(iv)
use msvars
use mdl_name_utils
integer(kind = MC_IKIND), intent(in) :: iv

!     print message for missing or invalid feedback value
!     iv is variable index


call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
write(str,'(2a)') 'Missing or invalid initial value for feedback variable ', &
&   name(:nlen)
call strout(O_ERRM)

return
end subroutine solot5

!-----------------------------------------------------------------------

subroutine solot6(nonval)

!     print total of missing/invalid feedback values

integer ::  nonval

write(str, '(i5,a)' ) nonval ,' missing or invalid initial feedback values'
call strout(O_ERRF)
return
end subroutine solot6

!-----------------------------------------------------------------------

subroutine solot7(itr)
use mdl_name_utils

! print overview of feedback values at iteration itr

integer ::  itr

integer ::   maxlen, maxcol, colwid
integer ::   j,jb,je
real(kind = SOLVE_RKIND) :: temp(20)

maxlen = maxnli(mdl%ivnames, mdl%numfb, 1_MC_IKIND, mdl%nfb)
colwid = max(NUMWID, maxlen)
maxcol = max( 1, min( 20, lwidth / (colwid + ICSPAC) ))

write(str,'(a,i4)') 'Feedback Variables at iteration ',itr
call strout(O_OUTB)

do jb = 1, mdl%nfb, maxcol

   je   = min(mdl%nfb, jb + maxcol - 1)

   ! insert variable names right justified within column width

   call strini( ' ', 1)
   call snmfmt(mdl%vnames, mdl%ivnames, mdl%numfb, jb, je, colwid)
   call strout(O_OUTB)

   call strini( ' ', 1)
   do j = jb, je
      temp(j - jb + 1) = curvars(mdl%numfb(j))
   end do
   call svlfmt(temp, 1, je - jb + 1, colwid)
   call strout(O_OUTB)

end do

return
end subroutine solot7

!-----------------------------------------------------------------------

subroutine solot8(itr)

! invalid feedback values have resulted

integer ::  itr

write(str, '(a,i4)') &
&  'Invalid values for feedback variables detected at iteration', itr
call strout(O_ERRQ)
str = 'Further iterations are useless'
call strout(O_ERRF)

if (.not. mws%dbgeqn) then
  str = '** Suggestion: use set_solve_debugmode(dbgon);'
  call strout(O_ERRF)
  str = '    before the current solve command in your Isis job'
  call strout(O_ERRF)
  str = '    to locate the equation causing the problem.'
  call strout(O_ERRF)
  str = '    See the Reference Manual for further details.'
  call strout(O_ERRF)
endif

return
end subroutine solot8

!-----------------------------------------------------------------------

subroutine solot9(itr)

!     print message that newton method is taking a stepback
!     but only if output option requires it

integer ::  itr

if (repopt == REP_FULLREP) then
   write(str,'(a,i4)') 'Stepback: new Newton matrix at iteration', itr
   call strout(O_OUTN)
endif

return
end subroutine solot9

!-----------------------------------------------------------------------

subroutine solota(itr, relax)

!     print message that newton method is taking a stepback
!     and what the new relaxation factor will be
!     but only if output option requires it

integer ::  itr
real(kind = SOLVE_RKIND) :: relax

if (repopt == REP_FULLREP) then
   write(str,'(a,f5.3,a,i4)', round = 'compatible') &
&   'Linesearch: setting relaxation factor to ',relax, ' at iteration ',itr
   call strout(O_OUTN)
endif

return
end subroutine solota

!-----------------------------------------------------------------------

subroutine solotb(matlst)

! print message for newton method can't continue

logical ::   matlst

if (repopt >= REP_PERIOD) then
    if(.not. matlst) then
        str = 'Max. number of Newton matrix updates for this period exhausted'
        call strout(O_OUTN)
    endif
    str =  'Cannot find a better point ...'
    call strout(O_OUTN)
endif

return
end subroutine solotb

!-----------------------------------------------------------------------

subroutine solotc(itr, relax)

!     print message for relaxation factor in newton method
!     if output options require it

integer, intent(in) ::  itr
real(kind = SOLVE_RKIND), intent(in) :: relax

if (repopt == REP_FULLREP) then
   write(str,'(a,f5.3,a,i4)', round = 'compatible') &
&       'Set relaxation factor to ', relax, ' at iteration ',itr
   call strout(O_OUTN)
endif

return
end subroutine solotc

!-----------------------------------------------------------------------

subroutine svdhdr(split, mxlen)

!     output of solve discrepancy/ies header line
!     mxlen is (maximum name length) modified if mxlen < length(variable)
!     split set to true if lines will be split else false

logical ::  split
integer ::  mxlen

character(len = 10) :: hdr(6)
integer ::        hln(6)
integer ::        rpos, hlen
integer ::        i

!     header line entries (effective length must be <= NUMWID)

save hdr, hln
data hdr / 'Variable', 'Old value', 'New value', 'Difference' , 'A/R', 'Test' /
data hln /  8,9,9,10,3,4 /

if( mxlen .lt. hln(1) ) mxlen = hln(1)
split = mxlen + 4*NUMWID + 3 + 5*ICSPAC .gt. lwidth

if( .not. split ) then
!        fits on one line
!        prepare first header line entry
   call strini( hdr(1), 1 + mxlen + ICSPAC)
else
!        break over two lines
   call strini( ' ', 1 )
endif

!     remainder of header line

do  i=2,6
   hlen = hln(i)
   if( i .ne. 5 ) then
      rpos = spos + NUMWID - hlen
      str(rpos : rpos + hlen - 1 ) = hdr(i)(1:hlen)
      spos = spos + NUMWID + ICSPAC
   else
      str( spos : spos + hlen - 1) = hdr(i)(1:hlen)
      spos = spos + hlen + ICSPAC
   endif
enddo

call strout(O_OUTN)

return
end subroutine svdhdr

!-----------------------------------------------------------------------

subroutine svdln(split, mxnlen, curnam, curlen,oldval,newval,disc, dsctyp, test)

!     output of solve discrepancy line

logical, intent(in) ::         split
integer, intent(in) ::         mxnlen
character*(*), intent(in) :: curnam
integer, intent(in) ::  curlen
real(kind = SOLVE_RKIND), intent(in) :: oldval, newval, disc, test

character*(*)  dsctyp

if( .not. split ) then
   call strini(curnam(:curlen), 1 + mxnlen + ICSPAC)
else
   write(str,'(2a)') 'Variable ',curnam(:curlen)
   call strout(O_OUTN)
   call strini( ' ', 1 )
endif

!     insert data in string buffer and write

!     old value
call nvlfmt(oldval, str(spos : spos + NUMWID - 1))
spos = spos + NUMWID + ICSPAC

!     new value
call nvlfmt(newval, str(spos : spos + NUMWID - 1))
spos = spos + NUMWID + ICSPAC

!     difference
call nvlfmt(disc, str(spos : spos + NUMWID - 1))
spos = spos + NUMWID + ICSPAC

!     type of difference
str(spos : spos + len(dsctyp) - 1) = dsctyp
spos = spos + len(dsctyp) + ICSPAC

!     test criterium for this variable
call nvlfmt(test, str(spos : spos + NUMWID - 1))

call strout(O_OUTN)

return
end subroutine svdln

!-----------------------------------------------------------------------

subroutine solotd(itr)
use mdl_name_utils
use nuna

! print overview of not converged variables if requested

integer, intent(in) ::  itr
character(len = 3) :: dtyp, armax

logical ::        split
real(kind = SOLVE_RKIND) :: dismax, dval
real(kind = SOLVE_RKIND) :: Qone
parameter    (Qone = 1.0d0)
integer ::  i, imax, mxlen

if (opts%iendsm .eq. 3 .and. repopt <= REP_MINIMAL) return

write(str,901) perstr,itr
call strout(O_OUTN)

dismax = 0.
imax   = 0

if (opts%suptst) then
    str = 'Largest remaining discrepancy'
    call strout(O_OUTN)
else
    str = 'All remaining discrepancies'
    call strout(O_OUTN)
    mxlen = maxnln(mdl%ivnames, 1_MC_IKIND, mdl%nrv)
    call svdhdr(split, mxlen)
endif

do i = 1, mdl%nrv

!         skip exogenous variable

    if (.not. mdl%lik(i)) cycle

    if (.not. nuifna(curvars(i)) .and. .not. nuifna(yp(i)) ) then

        dval = abs(curvars(i) - yp(i))
        dtyp = 'Abs'
        if (abs(curvars(i)) .gt. Qone) then
            dtyp = 'Rel'
            dval = dval / abs(curvars(i))
        endif

        if (dval .le. mws%test(i)) cycle

        if (opts%suptst .and. dval/mws%test(i) .gt. dismax) then
            imax   = i
            dismax = dval / mws%test(i)
            armax  = dtyp
        endif

    else if( .not. opts%suptst) then
        dtyp = ' - '
        if( nuifna(yp(i)) ) then
            dval = yp(i)
        else
            dval = curvars(i)
        endif
    endif

    if (.not. opts%suptst) then
        call mcf7ex(name, nlen, mdl%ivnames(i), mdl%vnames)
        call svdln(split, mxlen, name, nlen, yp(i), curvars(i), &
&                  dval, dtyp, mws%test(i))
    endif

end do

if (opts%suptst) then
    if (imax .ne. 0 ) then
        call mcf7ex(name, nlen, mdl%ivnames(imax), mdl%vnames)
        call svdhdr(split, nlen)
        call svdln(split,nlen, name,nlen,yp(imax),curvars(imax), &
&                  dismax*mws%test(imax),armax,mws%test(imax))
    else
        str = 'All values invalid (no largest discrepancy)'
        call strout(O_OUTN)
    endif
endif

901 format('No convergence for ',a,' in ',i5,' iterations')

return
end subroutine solotd

!-----------------------------------------------------------------------

    subroutine solotf(itr)
        ! print happy message about convergence in itr iterations
        integer, intent(in) ::  itr

        if (repopt > REP_MINIMAL) then
           write(str,905) trim(perstr), itr
           call strout(O_OUTN)
        endif
905     format('Convergence for ',a,' in ',i4,' iterations')
        return
    end subroutine solotf

end module msolot
