module msnwut
use kinds

private :: msjaco, msjacf, msczjc, deljac

contains

subroutine msjac(retcod, itr, matitr)
use msvars
use msjcot
use scalemat
integer, intent(out) :: retcod, matitr
integer, intent(in)  :: itr

!   retcod  1 if jacobian is near to singular
!                      or other problems

!     generate jacobian by calling appropriate routines


integer ::   ier

if (mdl%fboflg == 0 .or. mdl%fbomem <= 0) then
    ! full slow method
    call msjaco(retcod, matitr)
else
    ! sparse fast method
    call msjacf(retcod, matitr)
endif

if (retcod /= 0) return

itrtot = itrtot + matitr
njcmat = njcmat + 1

!     check for (almost) 0 columns
!     print jacobian if required
!     and only then quit on zero check errors

call msczjc(ier)

if (opts%jacprt) then
   call jacot3(itr)
endif

if( ier .eq. 1 ) then
   retcod = 1
   return
endif

! determine scale factors from jacobian and scale jacobian
if (opts%scalmd /= NO_SCALING) then
    call scale_matrix(jacob, jacdim, mdl%nfb, scale, opts%scalmd, ier)
endif

if (opts%prscal .and. opts%scalmd >= 0) then
    call soloti
endif

return
end subroutine msjac

!-----------------------------------------------------------------------

subroutine msjaco(retcod, matitr)
use msvars
use msfbvl
use nuna
use nucnst
use msjcot
 
!   makes an estimate of the jacobian of the system
!       F = x - g(x)
!   wrt x = the feedback variables
!   by finite differences at the current point (fbval,fbfun)
 
!   Old method : all equations
 
!   retcod 1 if jacobian is near to singular
!                      or other problems
 
integer, intent(out) :: retcod, matitr

integer ::       fbnum, i, j

real(kind = SOLVE_RKIND) :: delta

! reset all feedback values to initial state
do i = 1, mdl%nfb
   curvars(mdl%numfb(i)) = fbval(i)
end do

!*IF PRZFBC
!      call przfbc(xvnlen, 0)
!*ENDIF

matitr = 0

do i = 1, mdl%nfb

   if (mdl%lik(mdl%numfb(i))) then

      delta       = deljac(fbval(i))
      curvars(mdl%numfb(i)) = fbval(i) + delta

      ! avoid errors caused by finite precision (BHH)
      ! try to defeat "eigenwijze" optimize

      call donowt(delta, fbval(i))
      delta = curvars(mdl%numfb(i)) - fbval(i)

      call msloop(retcod)
      if (retcod .ne. 0) return

!*IF PRZFBC
!            call przfbc(xvnlen,i)
!*ENDIF
      matitr = matitr + 1
      do j = 1, mdl%nfb

          fbnum = mdl%numfb(j)

          ! test for invalid ==> can't continue

          if (nuifna(curvars(fbnum))) then
               call jacot1(fbnum, mdl%numfb(i))
               retcod = 1
               return
          endif

          ! calculate column j of jacobian of F=x-g(x)
          !       e is appropriate unit vector
          ! F(x+delta*e) = x+delta*e - g(x+delta*e)
          !             = x+delta*e - zdelta
          ! F(x)         = x - g(x)
          !             = x - zold
 
          !deriv * delta = F(x+delta*e) - F(x)
          !              = delta*e + g(x) - g(x+delta*e)
          !              = delta*e + zold - zdelta
          !              = delta*e + x-F(x) - zdelta
          !deriv         = e + (x-F(x) - zdelta)/delta
          jacob(j,i) = (-curvars(fbnum) + fbval(j) - fbfun(j)) / delta
          curvars(fbnum)    = fbval(j)

      end do

   else

       ! exogenous feedback ==> 0 derivative
       jacob(:, i) = Rzero

   endif

   ! prepare I-H

   jacob(i,i) = jacob(i,i) + Rone

end do

return
end subroutine msjaco

!-----------------------------------------------------------------------

subroutine msjacf(retcod, matitr)
use msvars
use msfbvl
use nucnst
use msjcot
 
!   makes an estimate of the jacobian of the system
!       F= x - g(x)
!   wrt x = the feedback variables
!   by finite differences at the current point (fbval,fbfun)
 
!   New method : only equations needed
 
!   retcod  1 if jacobian is near to singular
!                      or other problems
 
integer, intent(out) :: retcod, matitr

integer :: fbnum, i, j, k
real(kind = SOLVE_RKIND) :: delta

! reset all feedback values to initial state
do i = 1, mdl%nfb
    curvars(mdl%numfb(i)) = fbval(i)
end do

!*IF PRZFBC
!      call przfbc(xvnlen,0)
!*ENDIF

matitr = 0

do i = 1, mdl%nfb

   if (mdl%lik(mdl%numfb(i))) then

      delta       = deljac(fbval(i))
      curvars(mdl%numfb(i)) = fbval(i) + delta

      ! avoid errors caused by finite precision (BHH)
      ! try to defeat "eigenwijze" optimize
      call donowt(delta, fbval(i))
      delta = curvars(mdl%numfb(i)) - fbval(i)

      ! walk through selected equations for this feedback variable
      ! and the last nfb equations to get feedback variables right
      call fbvl(retcod, mdl%fbordr, mdl%fboptr(i), mdl%fboptr(i + 1) - 1_MC_IKIND)
      if (retcod /= 0) return
      call fbvl(retcod, mdl%order, mdl%loope - mdl%nfb + 1_MC_IKIND, mdl%loope)
      if (retcod /= 0 ) return
!*IF PRZFBC
!            call przfbc(xvnlen,mdl%numfb(i))
!*ENDIF

      matitr = matitr + 1
      do j = 1, mdl%nfb

          fbnum = mdl%numfb(j)

          ! test for invalid ==> can't continue
          if (nuifna(curvars(fbnum))) then
               call jacot1(fbnum, mdl%numfb(i))
               retcod = 1
               return
          endif

         !      calculate column j of jacobian of F=x-g(x)
         !      e is appropriate unit vector
         !       F(x+delta*e) = x+delta*e - g(x+delta*e)
         !                    = x+delta*e - zdelta
         !       F(x)         = x - g(x)
         !                    = x - zold

         !       deriv * delta = F(x+delta*e) - F(x)
         !                     = delta*e + g(x) - g(x+delta*e)
         !                     = delta*e + zold - zdelta
         !                     = delta*e + x-F(x) - zdelta
         !       deriv         = e + (x-F(x) - zdelta)/delta

          jacob(j,i) = (-curvars(fbnum) + fbval(j) - fbfun(j)) / delta

      end do

      ! undo all changes in curvars
      do k = mdl%fboptr(i), mdl%fboptr(i + 1) - 1
          j = mdl%lhsnum(mdl%fbordr(k))
          curvars(j) = yp(j)
      end do
      do k = 1, mdl%nfb
          curvars(mdl%numfb(k)) = fbval(k)
      end do

   else

       ! exogenous feedback ==> 0 derivative
       jacob(:, i) = 0

   endif

   ! prepare I-H

   jacob(i,i) = jacob(i,i) + Rone

end do

matitr = (mdl%fboptr(mdl%nfb + 1) - 1 + matitr * matitr) / &
&        (mdl%loope - mdl%loops + 1)

return
end subroutine msjacf

!-----------------------------------------------------------------------

subroutine soloti()
use msvars
use msimot
use mdl_name_utils

! print feedback scale factors

integer ::  xnumwid
parameter(XNUMWID=2*NUMWID)
integer :: maxfbn

!  maxfbn  length of longest feedback name

integer ::       clen, rpos, strind
character(len = 20) :: chdr(3)
integer ::       chln(3), cwid(3), xwid(3)
integer ::       i,k

save chdr, chln, cwid

! chdr      column header text
! chln      length of same
! cwid      minimum column width (0 determined later on)

data chdr / 'Feedback variable' ,'Scaling factor','Current value'/
data chln /    17               ,   14           ,   13          /
data cwid /    0                , XNUMWID         , NUMWID       /


! indent of each line

strind = 6

call strini(' ', 1)
call strout(O_OUTN)
spos = strind
if( opts%scalmd .eq. 0 ) then
   str(spos:) = 'Scaling factors of feedback variables (Normbal)'
elseif( opts%scalmd .eq. 1 ) then
   str(spos:) = 'Scaling factors of feedback variables (Limbal)'
else
   str(spos:) = 'Scaling factors of feedback variables (Simpc)'
endif
call strout(O_OUTN)

call strini(' ', 1)
call strout(O_OUTN)

! calculate length of longest feedback name

maxfbn  = maxnli(mdl%ivnames, mdl%numfb, 1_MC_IKIND, mdl%nfb)
xwid(1) = max(maxfbn,chln(1))

call strini(' ', strind)
str(spos:spos+chln(1)-1) = chdr(1)
spos = spos + xwid(1) + ICSPAC

do k = 2, 3
   clen = chln(k)
   rpos = spos + max(cwid(k) - clen, 0)
   str(rpos : rpos + clen - 1) = chdr(k)(:clen)
   xwid(k) = max(cwid(k),clen)
   spos = spos + xwid(k) + ICSPAC
end do

call strout(O_OUTN)


!     <Name> <feedback value>  <scale value>

do i = 1, mdl%nfb

    call strini(' ', strind)
    call mcf7ex(name, nlen, mdl%ivnames(mdl%numfb(i)), mdl%vnames)
    str(spos : spos + nlen - 1) = name(:nlen)
    spos = spos + xwid(1) + ICSPAC

    call xvlfmt(scale(i) , str(spos : spos + XNUMWID - 1))
    spos = spos + xwid(2) + ICSPAC

    call evlfmt(fbval(i) , str(spos : spos + NUMWID - 1))
    spos = spos + xwid(3) + ICSPAC

    call strout(O_OUTN)

end do

call strini(' ', 1)
call strout(O_OUTN)

return
end subroutine soloti

!-----------------------------------------------------------------------

      real(kind = ISIS_RKIND) function deljac( val )
          real(kind = ISIS_RKIND) :: val

          ! calculate perturbation for computing numerical derivative
          ! of a function at point val
          !     this must be done in such a way that enough digits are
          !     perturbed (to get a meaningful result)
 
          !     jaceps is applied relatively to absolute value of val
          !     and adding the result to jaceps itself
          !     if |val| is less than the machine precision then the pertubation
          !     will be numerically significant
 
          ! when global variable jacmod == 2 return value is set for
          ! backward differencing

          real(kind = ISIS_RKIND), parameter :: jaceps = 0.0001_ISIS_RKIND

          deljac = max(jaceps, jaceps * abs(val))
          return
    end function deljac

!-----------------------------------------------------------------------

subroutine solotg(itr, Fbmax, Fquot, Fcrit, imax)
use msvars
use msimot
use mdl_name_utils

integer, intent(in) ::  itr, imax
real(kind = SOLVE_RKIND), intent(in) :: Fbmax, Fquot, Fcrit

integer ::       clen, hdrlen, rpos
character(len = 6) :: chdr(6)
integer ::       chln(6), cwid(6)
integer ::       k

save chdr, chln, cwid, hdrlen

data chdr / 'iter' , 'Fquot', 'Fcrit', 'Fbmax', 'scale', 'Fbname'/
data chln /    4   ,    5   ,    5   ,   5    ,    5   ,   6     /
data cwid /    6   , NUMWID , NUMWID , NUMWID, NUMWID ,   0      /

if (repopt /= REP_FULLREP) return

if( itr .eq. 0 ) then
   call strini(' ', 1)
   do k = 1, 6
       clen = chln(k)
       rpos = spos + max(cwid(k) - clen, 0)
       str(rpos : rpos + clen - 1) = chdr(k)(:clen)
       if (cwid(k) /= 0 ) then
           spos = spos + cwid(k) + ICSPAC
       endif
   end do
   hdrlen = spos
   call strout(O_OUTN)
endif

call strini(' ', 1 )
write(str, '(i6)' ) itr
spos = 1 + cwid(1) + ICSPAC

if (imax .ne. 0) then
    call nvlfmt(Fquot , str(spos : spos + NUMWID - 1))
    spos = spos + NUMWID + ICSPAC

    call nvlfmt(Fcrit , str(spos : spos + NUMWID - 1))
    spos = spos + NUMWID + ICSPAC

    call evlfmt(Fbmax , str(spos : spos + NUMWID - 1))
    spos = spos + NUMWID + ICSPAC
    call nvlfmt(scale(imax) , str(spos : spos + NUMWID - 1))
    spos = spos + NUMWID + ICSPAC
    call mcf7ex(name, nlen, mdl%ivnames(mdl%numfb(imax)), mdl%vnames)
    if( hdrlen + nlen .gt. lwidth ) then
        call strout(O_OUTN)
        call strini( ' ', lwidth - nlen - 1)
    endif
    str(spos : spos + nlen - 1) = name(:nlen)
else
    ! invalid feedback value(s)
    ! Invalid printed right justified

    str(spos : spos + NUMWID -1) = '      -'
    spos = spos + NUMWID + ICSPAC
    str(spos : spos + NUMWID -1) = '      -'
    spos = spos + NUMWID + ICSPAC
    rpos = spos + max(NUMWID - 7,0)
    str(rpos : rpos + 7 - 1) = 'Invalid'
endif

call strout(O_OUTN)

return
end subroutine solotg

!-----------------------------------------------------------------------

subroutine evlfmt( x, numstr )

!     basic number format routine
!     fixed width and use E format
!     Warning: if 12 in formats changed then adjust NUMWID in mpsdat
!              and recompile

use nuna
real(kind = SOLVE_RKIND), intent(in) :: x
character*(*), intent(out) :: numstr

character(len = 10) ::  EFMT
parameter( EFMT = '(1p,e12.5)' )

if (.not. nuifna(x) ) then
    write(numstr, EFMT) x
else
    numstr = 'Invalid'
endif

return
end subroutine evlfmt

!-----------------------------------------------------------------------

subroutine xvlfmt( x, numstr )

!     basic number format routine
!     fixed width and use E format
!     Warning: if 12 in formats changed then adjust NUMWID in mpsdat
!              and recompile

use nuna
real(kind = SOLVE_RKIND) :: x

character*(*) numstr

character(len = 11) :: EFMT
parameter( EFMT = '(1p,e24.17)' )

if( .not. nuifna(x) ) then
   write(numstr, EFMT) x
else
   numstr = 'Invalid'
endif

return
end subroutine xvlfmt

!-----------------------------------------------------------------------

subroutine msnfbc(Fbmax, Fimax, nfb, fbfun, scale)

!     compute criterion for feedback variables
!     WARNING: when this subroutine is used in ratex procedure,
!     then input variable nfb is the dimension of the
!     ratex matrix.
!     Fbmax will be largest scaled function value (fbfun/scale)
!     Fimax will be index

use kinds
integer, intent(out) :: Fimax
integer(kind = LI_IKIND), intent(in) :: nfb
real(kind = ISIS_RKIND), intent(out) :: Fbmax
real(kind = ISIS_RKIND), intent(in) :: fbfun(*), scale(*)

real(kind = ISIS_RKIND) ::  t,f
integer(kind = LI_IKIND) :: i,isav

!     ensure that test t > f executed at least once
f    = -1
isav = 0
do i = 1, nfb
    t = abs(fbfun(i)) / scale(i)
    if(t .gt. f) then
        f    = t
        isav = i
    endif
end do

Fimax = isav
Fbmax = f

return
end subroutine msnfbc

!*IF PRZFBC
!!-----------------------------------------------------------------------
!
!      subroutine przfbc(xvnlen, fbnum)
!      use msvars
!      use msimot
!
!      integer xvnlen,fbnum
!
!*CALL MP$MPSDAT$VF9
!
!      integer i,eqnum,vnum
!
!      call strini(' ', 1 )
!      if( fbnum .eq. 0 ) then
!         call strini('Initial curvars', 9)
!      else
!         call strini('Feedback ', 10)
!         call mcf7ex(name, nlen, mdl%ivnames(fbnum), mdl%vnames)
!         str(spos : spos + nlen - 1) = name(:nlen)
!      endif
!      call strout(O_OUTN)
!
!      do 100 i=mdl%loops,mdl%loope
!         eqnum = mdl%order(i)
!         vnum  = mdl%lhsnum(eqnum)
!         call strini(' ', 1 )
!         call mcf7ex(name, nlen, mdl%ivnames(vnum), mdl%vnames)
!         str(spos : spos + nlen - 1) = name(:nlen)
!         spos = spos + xvnlen + ICSPAC
!         call evlfmt(curvars(vnum), str(spos : spos + NUMWID - 1))
!         spos = spos + NUMWID
!         call strout(O_OUTN)
! 100  continue
!
!      call strini(' ', 1)
!      call strout(O_OUTN)
!
!      return
!      end subroutine przfbc
!*ENDIF

!-----------------------------------------------------------------------

subroutine mslsg1(retcod,rcod,itr,bcnt, Fcrit,Fquot,Fquotp,Fbmax,Fbmaxp, matlst)
use msvars
use msutil
use msfbvl
use nuna
use msolot

!     evaluate simultaneous block of model
!     backtrack while result feedback variables are invalid
!                     or criterion is > opts%cstpbk
!     return code
!       rcod  1   valid feedback variables and Fcrit < opts%cstpbk
!                 bcnt > 0 gives number of backtracks
!                 if 0 then no backtracks
!             2   invalid feedback values or Fcrit >= opts%cstpbk
!                   restart with new jacobian
!             3   too many backtracks (iterations)
!             4   cannot decrease relax any further

integer ::  retcod,rcod,itr,bcnt
logical ::  matlst
real(kind = SOLVE_RKIND) :: Fcrit, Fquot, Fquotp, Fbmax, Fbmaxp
real(kind = SOLVE_RKIND) :: f
integer ::  imax, i

rcod = 0
bcnt = 0

! do
!          ....
! while( rcod .eq. 0)
10 continue

   if (opts%priter) then
       call solot7(itr)
   endif

   ! call simultaneous block

   call msloop(retcod)
   if (retcod .ne. 0 ) return

   imax = 0

   if (.not. tstvfb()) then
       Fcrit = 2 * opts%cstpbk
   else
!            calculate largest scaled discrepancy
!            new function values

       do i = 1, mdl%nfb
           fbwork(i) = curvars(mdl%numfb(i)) - yp(mdl%numfb(i))
       end do
       call msnfbc(Fbmax, imax, mdl%nfb, fbwork, scale)

!            Both Fbmax and Fbmaxp are valid here

!            the result of this computation can become NA. invalid
!            if Fbmaxp (previous Fbmax) is zero or too small
!            Fcrit will also become invalid
!            test follows below

       Fquot = Fbmax / Fbmaxp
       if (opts%arith) then
          Fcrit = (Fquot + Fquotp) / 2
       else
          Fcrit = sqrt(Fquot * Fquotp)
       endif
   endif

   call solotg(itr, Fbmax, Fquot, Fcrit, imax)

   if (nuifna(Fcrit)) then

       ! -- step is acceptable but make sure iteration continues

       rcod = 1

   elseif (Fcrit < opts%cstpbk) then

       ! -- acceptable step

       rcod = 1

   elseif (itr >= opts%maxit ) then

       ! -- too many iterations (function evaluations)

       rcod = 3

   elseif( ((.not. matlst) .and. bcnt >= opts%bktmax) .or. &
&           relax < opts%rlxmin / opts%rlxspeed) then

       !   -- unacceptable step
       !   -- no more backtracking steps allowed/possible

       if ((.not. matlst) .and. njcmat < opts%maxmat) then
           !jacobian out of date but new jacobian allowed
           rcod = 2
           call solot9(itr)
       else
           ! no better point found
           rcod = 4
           call solotb(matlst)
       endif

   else

       ! -- decrease relax for next try

       relax = relax * opts%rlxspeed
       do i = 1, mdl%nfb
          if (mdl%lik(mdl%numfb(i))) then
              f = fbval(i) + relax * fbstep(i)
              curvars(mdl%numfb(i)) = f
              yp(mdl%numfb(i)) = f
          endif
       end do
       call solota(itr, relax)
       bcnt = bcnt + 1
       itr  = itr  + 1

   endif

if (rcod == 0) goto 10

return
end subroutine mslsg1

subroutine msczjc(retcod)
use msvars
use nuv
use nucnst

!     Check for (almost) zero columns and rows in Jacobian

!     retcod
!       0        all ok
!       1        at least 1 column/row exactly 0
!       2        at least 1 column/row almost 0 (nfb * Rmeps)

integer ::  retcod
integer ::   k

real(kind = ISIS_RKIND) :: rsum
integer :: nfbk

retcod = 0

!Check columns

nfbk = mdl%nfb

do k = 1, mdl%nfb

   if (mdl%lik(mdl%numfb(k))) then
       rsum = dasum(nfbk, jacob(:, k), 1)

       if (rsum == Rzero) then
           call msczot(k, 1, 'C')
           if (retcod == 0) then
               retcod = 1
           endif
       else if (rsum <= mdl%nfb * Rmeps) then
           call msczot(k, 2, 'C')
           if (retcod == 0 ) then
               retcod = 2
           endif
       endif
   endif

enddo

! Check rows

do k = 1, mdl%nfb

   if (mdl%lik(mdl%numfb(k)) ) then
       rsum = dasum(nfbk, jacob(k,1), int(jacdim, ISIS_IKIND))
       if (rsum == Rzero ) then
           call msczot(k, 1, 'R')
           if (retcod == 0) then
               retcod = 1
           endif
       else if (rsum <= mdl%nfb * Rmeps) then
           call msczot(k, 2, 'R')
           if (retcod == 0) then
               retcod = 2
           endif
       endif
   endif

enddo

return

contains

    subroutine msczot(ifb, errcod, indicator)
    use msimot
    use mdl_name_utils
    integer, intent(in) :: ifb, errcod
    character(len = 1), intent(in) ::  indicator
    !  print message for (almost) 0 column/row
    !  caused by feedback variable ifb

    integer ::  otype

    call mcf7ex(name, nlen, mdl%ivnames(mdl%numfb(ifb)), mdl%vnames)

    if (indicator == 'C') then
        if (errcod == 1) then
            write(str,'(2a)') 'Zero column for feedback variable ', name(:nlen)
            otype = O_ERRQ
        else
            write(str,'(2a)') 'Almost zero column for feedback variable ', &
                     name(:nlen)
            otype = O_WMSG
        endif
    else
        if (errcod == 1) then
            write(str,'(2a)') 'Zero row for feedback variable ', name(:nlen)
            otype = O_ERRQ
         else
             write(str,'(2a)') 'Almost zero row for feedback variable ', &
                              name(:nlen)
            otype = O_WMSG
         endif
    endif

    call strout(otype)

    return
    end subroutine msczot

end subroutine msczjc

end module msnwut
