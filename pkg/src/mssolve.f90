module mssolve
use msvars
private :: solnw, solve_gauss_seidel

contains

subroutine solone(retcod,ndiver)
 
    ! solves the model for the current period (jc)
    ! calls required solving subroutine depending on method
    
    ! at call, curvars contains starting values
    ! the result is stored in curvars
     
    ! alternate return 1 for missing/invalid exo's or starting values
    ! alternate return 2 for numerical problems or non-convergence
    ! alternate return 3 for other errors (write)
     
    integer ::  retcod,ndiver
    
    if (method == 'G') then
        call solve_gauss_seidel(retcod, ndiver)
    else
        ! 3 methods combined:
        !   - Broyden secant method
        !   - Newton with delayed calculation of jacobian (LU)
        !   - Newton with delayed calculation of jacobian (QR)
        ! njcmat counts number of jac updates per period
        ! at start of period set to 0
        njcmat = 0
        call solnw(retcod, ndiver)
    endif
    
    return
end subroutine solone

! ---------------------------------------------------------------------

subroutine solnw(retcod, ndiver)
use msutil
use msnwut
use msbrdn
!use msnwto
!use msnwqr
use msfbvl
use msolot


!     solves the model for the current period (jc)
!     - using the broyden/secant method
!     - using the newton/secant method (delayed calculation of jacobian)
!       and using a QR decomposition

 
!     at call, z contains starting values
!     the result is stored in z
 
!     retcod
!      1 for missing/invalid exo's or starting values
!      2 for numerical problems or non-convergence

integer ::        retcod,ndiver

logical ::        matlst
logical ::        convgd, quit

integer ::        xcod,xsptyp, imax, itr, i, rcod, tcod, bcnt, ier

real(kind = SOLVE_RKIND) :: Fstart, Fbmax, Fbmaxp, Fquot, Fquotp, Fcrit
real(kind = SOLVE_RKIND) :: ypfbi

!     Fbmax  largest scaled change in feedback variables
!     Fquot  ratio of current and previous maximum scaled change
!                   in scaled feedback variables
!     Fcrit  geometric mean current and previous Fquot

!     initialize initial convergence speed threshold


Fstart = min(opts%cstpbk, opts%cnmtrx, opts%crelax)
if( Fstart > Rhalf ) then
   Fstart = Rhalf
else
   Fstart = Rhalf * Fstart
endif

Fquot = Fstart
Fquotp= Fstart
Fcrit = Fstart
relax = opts%rlxmax

retcod = 0

! check exo's and CAs for validity
! check for invalid numbers in feedback variables
call chkxaf(quit)
if (quit) then
   retcod = 1
   return
endif

! store starting values ( yp(*) = z(*) )
! call prologue
! store result
yp = curvars


call msprlg(xcod)
if (xcod /= 0) then
   retcod = 2
   return
endif

yp = curvars

itr = 0

if (opts%priter) then
    call solot7(itr)
endif

! call simultaneous block
call msloop(xcod)
if (xcod /= 0) then
   retcod = 2
   return
endif

! test for invalid numbers in feedback variables

imax = 0

if (.not. tstvfb() ) then

   ! invalid values in feedback set
   ! if first iteration stop (numerical problems)
   ! since there is no use in continuing with current feedback values

    tcod = 2
    call solot8(itr)
    retcod = 2
    return

else if (tstcvg()) then
    ! have convergence ==> quit loop
    tcod = 1
    goto 500

else if (opts%maxit ==  0) then
    ! no iterations, so no convergence, but print overview variables via solotd
    tcod = 2
    goto 500
else

    ! setup x and function values

    do i = 1, mdl%nfb
        fbval(i) = yp(mdl%numfb(i))
        fbfun(i) = fbval(i) - curvars(mdl%numfb(i))
    end do

    call msnfbc(Fbmax, imax, mdl%nfb, fbfun, scale)

    ! store result z in yp
    ! needed for convergence testing
    !  and fast jacobian method (for reset)

    yp = curvars
endif

call solotg(itr, Fbmax, Fquot, Fcrit, imax)

! Here no invalid fbvalues and not converged
! Start Newton

! logical matlst indicates whether a jacobian was computed
!                  in the previous iteration
! logical matrix (in common pars) indicates whether a new
!                    jacobian has to be computed

! the first time (with matrix.eq..false.) the jacobian (i-h) is made
! also if convergence is not fast enough (measured by Fcrit), a new
! jacobian is made
 

matlst = .false.

tcod = 0

100 continue

!     if a new jacobian is about to be computed
!     set matlst flag to indicate that jacobian is "up to date"

xsptyp = 0
if (.not. matrix) then
    matlst = .true.
    xsptyp = 1
endif

if (method == "B") then
    call solve_brdn(xcod, itr)
    continue
!elseif (method == "N") then
!    call solve_nwtn(xcod, itr)
!elseif (method == "Q") then
!    call solve_nwqr(xcod, itr)
endif

if (xcod .ne. 0 ) then
    retcod = 2
    return
endif

if (xsptyp == 1) then

    ! recompute function value since scale may have changed

    call msnfbc(Fbmax, imax, mdl%nfb, fbfun, scale)

    ! reset variables for convergence speed to initial values
    ! i.e. forget what happened upto now
 
    Fquot  = Fstart
    Fquotp = Fstart
    Fcrit  = Fstart

    ! if calculated a new matrix then print synopsis again
    ! since scale may have changed

    call solotg(itr, Fbmax, Fquot, Fcrit, imax)

endif

Fbmaxp = Fbmax

! save new feedback values for testing

do i = 1, mdl%nfb
    yp(mdl%numfb(i)) = curvars(mdl%numfb(i))
end do

! call simultaneous block
itr = itr + 1
call mslsg1(xcod,rcod,itr,bcnt, Fcrit,Fquot,Fquotp,Fbmax,Fbmaxp,matlst)
if (xcod .ne. 0 ) then
    retcod = 2
    return
endif

if (rcod > 2 ) then
    ! -- FAILURE
    tcod = 3
elseif (rcod == 2) then
    ! -- stepback from current point
    ! -- calculate new matrix
    curvars = yp
    matrix = .false.
elseif (tstcvg()) then
    ! -- have convergence
    tcod = 1
elseif (itr >= opts%maxit) then
    ! -- no convergence
    tcod = 2
else
!      -- accept new step:

!         set matlst flag to indicate that a jacobian is now
!         out of date wrt feedback values

!         fbval stores initial feedback values
!         fbfun stores corresponding function values
!               x - g(x)

!         for broyden method:
!            dx stores current scaled step of fb variables
!            df stores current scaled change in function values
!            both are used in the broyden update of the jacobian

    matlst = .false.

    do i = 1, mdl%nfb
        if (method == "B") then
            ypfbi = yp(mdl%numfb(i))
            dx(i) = (ypfbi - fbval(i)) / scale(i)
            df(i) = (ypfbi - curvars(mdl%numfb(i)) - fbfun(i)) / scale(i)
            fbval(i) = ypfbi
            fbfun(i) = ypfbi - curvars(mdl%numfb(i))
        else
            ! Newton / Newton QR
            fbval(i) = yp(mdl%numfb(i))
            fbfun(i) = fbval(i) - curvars(mdl%numfb(i))
        endif
    end do

! store result after one pass in yp

    yp = curvars

    Fbmaxp = Fbmax
    Fquotp = Fquot

!         if convergence speed is too slow and a new jacobian
!         may be computed then
!              indicate that a new jacobian should be computed
!         else
!             newton/newtqr: if relaxation is allowed then
!             execute a relaxation
!         else
!              execute broyden update
!              if result is singular then
!                  indicate that a new jacobian should be computed
!                 (if allowed)
!              else
!                  Cannot continue !!
!         endif

    if (Fcrit > opts%cnmtrx .and. njcmat < opts%maxmat) then
        matrix = .false.
    elseif (method == "B") then
        call msbrup(mdl%nfb, jacob, jacob(:, mdl%nfb + 1: 2 * mdl%nfb), &
&                 jacdim, fbfun, la_work, ier)
        if (ier /= 0) then
            if (njcmat < opts%maxmat) then
                 matrix = .false.
            else
                 tcod = 3
            endif
        endif
    else
        ! Newton / Newton QR
        if (Fcrit > opts%crelax .and. relax >= opts%rlxmin / opts%rlxspeed) then
            relax  = opts%rlxspeed * relax
            call solotc(itr, relax)
       endif
    endif
endif

if (tcod == 0) goto 100

500 continue
itrtot = itrtot + itr

if (tcod > 1) then
    ! NO CONVERGENCE
    convgd = .false.
    ndiver = ndiver + 1
    call solotd(itr)
else
    ! CONVERGENCE
    convgd = .true.
    call solotf(itr)
endif

! recover feedback variables (no implicit GS step at end)
! this guarantees that if this solution is used as starting
! point for a subsequent simulation convergence will be
! achieved in 0 iterations
! also setup fbval and fbfun in case someone needs them
! (fit procedure)

do i = 1, mdl%nfb
    fbval(i)        = yp(mdl%numfb(i))
    fbfun(i)        = fbval(i) - curvars(mdl%numfb(i))
    curvars(mdl%numfb(i)) = fbval(i)
end do

! call epilogue

call mseplg(xcod)
if (xcod .ne. 0) then
    retcod = 2
    return
endif

! find out if simulation has to be stopped

if (.not. convgd) then
    retcod = 2
endif
    
    return
    end subroutine solnw
    
    ! ---------------------------------------------------------------------
    
        subroutine solve_gauss_seidel(retcod, ndiver)
        use msvars
        use msutil
        use msfbvl
        use nucnst
        use msolot
         
        ! solves the model for the current period jc
        ! using the gauss-seidel method
         
        ! at call, curvars contains starting values
        ! the result is stored in curvars
         
        ! retcod 1 for missing/invalid exo's or starting values
        ! retcod 2 for numerical problems or non-convergence
        ! retcod 3 for write error
         
        integer, intent(out) :: retcod
        integer, intent(inout) :: ndiver
        logical ::   convgd
        
        integer ::  xcod,itr,i
        logical ::  quit
        
        retcod = 0
        
        ! check exo's and ca's for validity
        ! check for invalid numbers in feedback variables
        
        call chkxaf(quit)
        if (quit) then
            retcod = 1
            return
        endif
        
        ! store starting values ( yp(*) = curvars(*) )
        ! call prologue
        ! store result
        
        yp = curvars
        
        call msprlg(xcod)
        if (xcod /= 0 ) then
            retcod = 2
            return
        endif
        
        yp = curvars
        
        ! start iteration loop
        
        convgd = .false.
        
        do itr = 0, opts%maxit
        
            if (opts%priter) then
                call solot7(itr)
            endif
        
            ! call simultaneous block
        
            call msloop(xcod)
            if (xcod /= 0 ) then
                retcod = 2
                return
            endif
        
            ! test for invalid numbers in feedback variables
        
            if (.not. tstvfb()) then
                !  invalid values in feedback set
                !  cannot continue with gauss-seidel
                call solot8(itr)
                retcod = 1
                return
            endif
        
           ! test for convergence
        
            if (tstcvg()) then
                convgd = .true.
                exit
            else if (itr < opts%maxit) then
                !  Gauss-Seidel step (with relaxation factor opts%grelax)
                if (opts%grelax /= Rone) then
                    do i = 1, mdl%nrv
                        curvars(i)  = yp(i) + opts%grelax * (curvars(i) - yp(i))
                    end do
                endif
                yp = curvars
            endif
        
        end do ! end iteration loop
        
        itrtot = itrtot + itr
            
        if (.not. convgd) then
            ! NO CONVERGENCE
            ndiver = ndiver + 1
            call solotd(itr)
        else
            ! CONVERGENCE
            call solotf(itr)
        endif
        
        ! recover feedback variables (no implicit GS step at end)
        ! this guarantees that if this solution is used as starting
        ! point for a subsequent simulation convergence will be
        ! achieved in 0 iterations
        
        do i = 1, mdl%nfb
            curvars(mdl%numfb(i)) = yp(mdl%numfb(i))
        end do
        
        ! call epilogue
        call mseplg(xcod)
        if (xcod /= 0) then
            retcod = 2
            return
        endif
        
        ! find out if simulation has to be stopped
        if (.not. convgd) then
            quit = .true.
            if (quit) then
                retcod = 1
            endif
        endif
        
        return
    end subroutine solve_gauss_seidel

end module mssolve
