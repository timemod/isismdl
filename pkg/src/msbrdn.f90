module msbrdn
use kinds
use msvars
use nucnst

contains

subroutine solve_brdn(retcod, itr)
    integer, intent(out) :: retcod
    integer, intent(in) :: itr

    ! the values of the feedback variables are updated according to the
    ! newton scheme : curvars = fbval - inv(I-H)*fbfun

    ! retcod  1 for numerical problems
    ! retcod 10 when there is not enoug memory available


    ! Msbjac needs 2 work arrays
    ! !!! using work and fbstep
    retcod = 0

    if (.not. matrix) then
        call msbjac(retcod, itr)
    endif

    if (retcod == 0) then
        call msbstp
    endif

    return
end subroutine solve_brdn

!-----------------------------------------------------------------------

subroutine msbstp

    ! calculate newton/broyden step
    
    integer(kind = LAPACK_IKIND) :: i, info
    
    ! scale function values
    
    do i = 1, mdl%nfb
        fbstep(i) = - fbfun(i) / scale(i)
    end do
    
    ! solve Jac * s = -f
    !       Q*R * s = -f
    ! solve R * s = - trans(Q)*f
    
    ! fbwork = - trans(Q) * f
    call dgemm('T', 'N', mdl%nfb, 1_LAPACK_IKIND, mdl%nfb, Rone, &
    &         jacob, jacdim, fbstep, mdl%nfb, Rzero, fbwork, mdl%nfb)
    ! **** solve R * fbstep = fbwork
    call dcopy(mdl%nfb, fbwork, 1_LAPACK_IKIND, fbstep, &
    &          1_LAPACK_IKIND)
    call dtrtrs('U', 'N', 'N', mdl%nfb, 1_LAPACK_IKIND, &
    &           jacob(1, mdl%nfb + 1), jacdim, fbstep, mdl%nfb, info)
    
    ! unscale stepsize + calculate new fb values
    
    do i = 1, mdl%nfb
        fbstep(i)   = fbstep(i) * scale(i)
        if (mdl%lik(mdl%numfb(i))) &
    &        curvars(mdl%numfb(i)) = fbval(i) + relax * fbstep(i)
    end do
    
    return
end subroutine msbstp

!-----------------------------------------------------------------------

subroutine msbjac(retcod, itr)
use msnwut
use msjcot
use liqrco
!use svd_anal
integer, intent(out) :: retcod
integer, intent(in) :: itr

!     generate jacobian by calling appropriate routines
!     perform a QR decomposition and prepare for Broyden
!     retcod = 0  : o.k.
!     retcod = 1  : Jacobian is (nearly) singular or
!                   other problems
!     retcod = 10 : not enough memory

real(kind = ISIS_RKIND) :: rcond

integer ::   matitr, ier
integer(kind = LAPACK_IKIND) :: j, info
logical ::   quit

real(kind = ISIS_RKIND), dimension(:,:), allocatable :: jac
integer ::  i, stat, svd_err

!
! allocate jacobian if required
!
if (.not. allocated(jacob)) then
    jacdim = mdl%nfb
    allocate(jacob(jacdim, 2 * mdl%nfb), stat = stat)
    if (stat /= 0) then
        call jacot7
        retcod = 10
        return
    endif
    jacob = 0
endif


! generate jacobian for feedback variables
! print if required
! scale if required

call msjac(retcod, itr, matitr)
if (retcod /= 0) return

if (opts%svdtest_tol >= 0) then
    !  save Jacobian for svd analysis
    allocate(jac(mdl%nfb, mdl%nfb), stat = stat)
    if (stat /= 0) then
        !call svdot1
        retcod = 10
        return
    endif
    do i = 1, mdl%nfb
        do j = 1, mdl%nfb
            jac(i,j) = jacob(i, j)
        enddo
     enddo
endif


! QR decomposition of scaled jacobian
! estimate inverse condition of R ==> inverse condition of jacobian
call qrco(jacob, jacdim, mdl%nfb, mdl%nfb, fbwork, rcond, la_work, la_lwork)

if( Rone + rcond == Rone) then
   ier = 1
else
   ier = 0
endif

call jacot2(matitr, itr, rcond)

if (rcond <= opts%svdtest_tol) then
!    call svd_analysis(jac, mdl%nfb, mdl%nfb, mdl%numfb, &
!&                     mdl%numfb, .false., opts%svdtest_tol, svd_err)
    svd_err = 0
else
    svd_err = 0
endif
if (opts%svdtest_tol >= 0) then
    deallocate(jac, stat = stat)
endif
if (svd_err == 1) then
    retcod = 10
    return
endif

if (ier /= 0) then
    call jacot5(ier)
    if (quit) then
        retcod = 1
        return
    endif
endif

!     copy the upper triangular part of a QR decomposition
!     contained in Rjac into Rjac(,n+1..)
!     generate full Q

do j = 1, mdl%nfb
    call dcopy(j, jacob(1,j), 1_LAPACK_IKIND, jacob(1,j + mdl%nfb), &
&              1_LAPACK_IKIND)
enddo
call dorgqr(mdl%nfb, mdl%nfb, mdl%nfb, jacob, jacdim, fbwork, &
&           la_work, la_lwork, info)

!     jacob(1..n,1  ..n ) contains expanded Q
!     jacob(1..n,n+1..2n) contains R

matrix = .true.

!     with fresh Jacobian always try maximum Newton step
!     so reset relaxation to initial value
!     !! must do here because of common

relax = opts%rlxmax

return
end subroutine msbjac

!-----------------------------------------------------------------------

subroutine msbrup(n,q,r,ldr,fp,wa,ier)
use liqrup
use msjcot
use nuv
integer(kind = LI_IKIND), intent(in) :: n, ldr
integer, intent(out) :: ier
real(kind = ISIS_RKIND) :: q(ldr,*),r(ldr,*)
real(kind = ISIS_RKIND) :: fp(*),wa(*)

!***********************************************************************

!     Calculate new Q and R from rank-1 update with dx and df
!     using Broyden method

!     Arguments

!     In       n       Integer         size of xc() etc.
!     Inout    Q       Real(ldr,n)     orthogonal matrix Q from QR
!                                      On output updated Q
!     Inout    R       Real(ldr,n)     upper triangular R from QR
!                                      On output updated R
!     In       ldr     Integer         leading dimension of Q and R
!     In       fp      Real(*)         new f-values

!     Out      wa      Real(*)         workspace, size >= 4 * n
!     Out      ier     Integer         0 all ok
!                                      1 if Q*R appears to be
!                                        singular or ill-conditioned

!-----------------------------------------------------------------------

integer ::  i, info
logical ::  doupdt
real(kind = SOLVE_RKIND) :: eta,sts,rcond

!     fudge factor for determining whether update is to actually done

eta    = Rhund * Rtwo * Rmeps
doupdt = .false.

! clear lower triangle (liqrup does not do this)
do i = 1, n - 1
    r(i + 1 : n, i) = Rzero
enddo

!     calculate df - B*dx = df - Q*R*dx
!     wa = R*dx
!     df = df - Q*(R*dx)
!     do not update with noise

call dcopy(n, dx, 1_LAPACK_IKIND, wa, 1_LAPACK_IKIND)
call dtrmv('U', 'N', 'N', n, r, ldr, wa, 1_LAPACK_IKIND)
call dgemm('N', 'N', n, 1_LAPACK_IKIND, n, -Rone, q, ldr, wa, n, Rone, df, n)

!     if changes in f(unction values) too small then no update

do  i=1,n
   if( abs(df(i)) .gt. eta*abs(fp(i)) ) then
      doupdt = .true.
   else
      df(i)  = Rzero
   endif
enddo

if( doupdt ) then
   sts = dnrm2(int(n, ISIS_IKIND), dx, 1)
   call dscal(n, Rone/sts, dx, 1_LAPACK_IKIND)
   call dscal(n, Rone/sts, df, 1_LAPACK_IKIND)
   call qrup(q,ldr,n,r,ldr,df,dx,wa)
endif


!      Estimate condition

call dtrcon('1', 'U', 'N', n, r, ldr, rcond, wa, wa(3 * n + 1), info)

if( Rone + rcond .eq. Rone) then
   ier = 1
else
   ier = 0
endif

if (ier /= 0) then
   call jacot6(rcond)
endif
return
end subroutine msbrup

end module msbrdn
