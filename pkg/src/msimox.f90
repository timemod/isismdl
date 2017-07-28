module msimox
    use msimot
    use kinds
    private :: rtxhdr, rtxdln

contains

subroutine simox1(iratex)
use msvars
use mdl_name_utils

! print guesses for rational expectations
integer ::  iratex
character(len =  8) :: target
integer(kind = MC_IKIND) :: kendex(20)
real(kind = MWS_RKIND) :: temp(20)

integer ::  j, k, jlast, kstep, kh, jt, ih, jtd
integer ::   maxlen, colwid

if (opts%repopt == REP_NONE) return

maxlen = maxnlz(mdl%ivnames, mdl%iendex, 1_MC_IKIND, mdl%nendex)
colwid = max(maxlen, NUMWID )

write(str, '(a,i4)') 'Expectation Guesses before round ',iratex
call strout(O_OUTB)

kstep = min( 20, (lwidth - 10) / (colwid + ICSPAC) )
jlast = 0

do kh = 1, mdl%nendex, kstep

   k = 0
   do j = jlast + 1, mdl%nendex
      if (mdl%iendex(j) .gt. 0) then
         k = k + 1
         kendex(k) = mdl%iendex(j)
         if (k .eq. kstep) exit
      endif
   end do
   jlast = j
   if (k .eq. 0) return

   call strini( ' ', 11 )

   ! insert names right justified

   call snmfmt(mdl%vnames, mdl%ivnames, kendex, 1, k, colwid)
   call strout(O_OUTB)

   ! write numbers right justified

   do jt = jf + 1, jl

      jtd = jt + mdl%mxlag

      call sjttmp(target,jt)
      call strini(target, 11)

      do ih = 1, k
         temp(ih) = mws%mdl_data(kendex(ih), jtd)
      end do
      call svlfmt(temp, 1, k, colwid)
      call strout(O_OUTB)

   end do

end do

return
end subroutine simox1

!-----------------------------------------------------------------------

subroutine rtxhdr(split, mxlen, tlen)

!     output of ratex discrepancy/ies header line
!     mxlen is (maximum name length) (modified if < len(variable))
!     split set to true if lines will be split (else false)

logical, intent(out) ::  split
integer, intent(inout) ::  mxlen
integer, intent(in) ::  tlen

integer ::  i
character(len = 10) :: hdr(7)
integer ::        hln(7)
integer ::        rpos, hlen

!     header line entries (effective length must be <= NUMWID)

save hdr, hln
data hdr / 'Variable', 'Period', 'Old value', 'New value', &
&          'Difference' , 'A/R', 'Test' /
data hln /  8,6,9,9,10,3,4 /

if( mxlen .lt. hln(1) ) mxlen = hln(1)
split  = mxlen + 7 + 4*NUMWID + 3 + 6*ICSPAC .gt. lwidth

if( .not. split ) then
!        fits on one line
!        prepare first header line entry
   call strini( hdr(1), 1 + mxlen + ICSPAC)
else
!        break over two lines
   call strini( ' ', 1 )
endif

!     remainder of header line

do  i=2,7
   hlen = hln(i)
   if( i .eq. 2 ) then
      rpos = spos + max(tlen - hlen, 0)
      str(rpos : rpos + hlen - 1 ) = hdr(i)(1:hlen)
      spos = spos + tlen + ICSPAC
   elseif( i .ne. 6 ) then
      rpos = spos + NUMWID - hlen
      str(rpos : rpos + hlen - 1 ) = hdr(i)(1:hlen)
      spos = spos + NUMWID + ICSPAC
   else
      str(spos : spos + hlen - 1 ) = hdr(i)(1:hlen)
      spos = spos + hlen + ICSPAC
   endif
enddo

call strout(O_OUTN)

return
end subroutine rtxhdr

!-----------------------------------------------------------------------

subroutine rtxdln(split,mxlen,curnam,curlen,target,oldval,newval, &
&                 disc, dsctyp, test, first)

!     output of ratex discrepancy line

logical, intent(in) ::         split
integer, intent(in) ::         mxlen
character(len = *), intent(in) ::  curnam
integer, intent(in)  :: curlen
character(len = *), intent(in) :: target
real(kind = SOLVE_RKIND), intent(in) :: oldval, newval, disc
character(len = *), intent(in) :: dsctyp
real(kind = SOLVE_RKIND), intent(in) :: test
logical, intent(in) :: first

if (opts%repopt == REP_NONE) return

if( .not. split ) then
   call strini(curnam(:curlen), 1 + mxlen + ICSPAC)
else
   if( first ) then
      write(str,'(2a)') 'Variable ',curnam(:curlen)
      call strout(O_OUTN)
   endif
   call strini( ' ', 1)
endif

!     insert data in string buffer and write

!     period
str(spos : spos + len(target) - 1) = target
spos = spos + len(target) + ICSPAC

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
end subroutine rtxdln

!-----------------------------------------------------------------------

subroutine simox2(iratex,noncvg)
use msratop

!     print a message on non-convergence of rational expectations

integer ::  noncvg, iratex

if (opts%repopt == REP_NONE) return

write(str,901) noncvg,iratex
call strout(O_OUTN)

901 format('No convergence for ',i8,' expectation values ', &
&      'after ',i8,' iterations')

return
end subroutine simox2

!-----------------------------------------------------------------------

subroutine simox3(iratex)

!     rational expectations have converged

integer ::  iratex

if (opts%repopt == REP_NONE) return

if( iratex .le. 9999 ) then
   write(str,'(a,i4,a)') 'Convergence for consistent expectations after', &
&       iratex,' rounds'
else
   write(str,'(a,i6,a)') 'Convergence for consistent expectations after', &
&       iratex,' rounds'
endif
call strout(O_OUTN)

return
end subroutine simox3

!-----------------------------------------------------------------------

subroutine simox4(jmax,imax,xomax,xnmax,dismax)
use msvars
use mdl_name_utils
use msratop

!     print a report on the largest discrepancy during a rational expectations
!     simulation (the verbosity depends on opts%xsuptt)
!     all calculations must have been done by calling routine

integer, intent(in) ::   jmax, imax
real(kind = SOLVE_RKIND), intent(in) :: xomax, xnmax, dismax

real(kind = SOLVE_RKIND) ::  cvgtst

character(len = 8) :: target
character(len = 3) ::  cdum
logical ::        split

if (opts%repopt == REP_NONE) return

str =  'Largest remaining discrepancy'
call strout(O_OUTN)

call mcf7ex(name, nlen, mdl%ivnames(imax), mdl%vnames)

call sjttmp(target,jmax)

if (abs(xomax) > 1.0_SOLVE_RKIND) then
    cdum = 'Rel'
else 
    cdum = 'Abs'
endif

!     header line

call rtxhdr(split, nlen, len(target))

!     next line with variable name or single line with variable name
cvgtst = opts%xtfac * mws%test(imax)
call rtxdln(split, nlen, name, nlen, target, xomax, xnmax, &
&           dismax * cvgtst, cdum, cvgtst, .true.)

return
end subroutine simox4

!-----------------------------------------------------------------------

subroutine simox5
use mdl_name_utils
use msvars

! print a report of all remaining discrepancies in a
! rational expectations simulation


character(len = 3) :: cdum
character(len = 8) :: target
logical ::        split, first
integer ::        mxnlen
real(kind = SOLVE_RKIND) :: xold, xnew, absdif, cvgtst

integer ::  i,j,k

if (opts%repopt == REP_NONE) return

str = 'All remaining discrepancies'
call strout(O_OUTB)

mxnlen = maxnli(mdl%ivnames, mdl%iendex, 1_MC_IKIND, mdl%nendex)
call rtxhdr(split, mxnlen, len(target))

do k = 1, mdl%nendex

   i = mdl%iendex(k)
   if (i .le. 0) cycle
   if (.not. mdl%lik(i)) cycle

   first = .true.

   do j = jf + 1, jl

       xold   = endo_leads(k, j)
       xnew   = mws%mdl_data(i, j + mdl%mxlag)
       absdif = abs(xnew-xold)
       cdum   = 'Abs'
       if (abs(xold) > 1.0_SOLVE_RKIND) then
           absdif = absdif/abs(xold)
           cdum   = 'Rel'
       endif

       cvgtst = opts%xtfac * mws%test(i)
       if (absdif .gt. cvgtst) then
          if (first ) then
              call mcf7ex(name, nlen, mdl%ivnames(i), mdl%vnames)
          endif
          call sjttmp(target,j)
          call rtxdln(split, mxnlen, name, nlen, target,xold,xnew, &
&                     absdif, cdum, cvgtst, first)
          if( first ) first = .not. first
       endif

   end do
end do

return
end subroutine simox5

end module msimox
