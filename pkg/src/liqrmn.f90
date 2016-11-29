module liqrmn

contains

! *NOTE Linear algebra QR solving FORTRAN routine
! Solves the underdetermined linear system trans(A)*b = y using the
! QR decomposition of trans(A).
subroutine qrmn(a, lda, m, n, tau, b, work, lwork)
use kinds
use nucnst
integer(kind = LI_IKIND), intent(in) :: lda, m, n, lwork
real(kind = ISIS_RKIND), intent(in) ::  a(lda,*), tau(*)
real(kind = ISIS_RKIND), intent(inout) :: b(*)
real(kind = ISIS_RKIND), intent(out) :: work(*)

integer(kind = LAPACK_IKIND) :: info

!  Subroutine Liqrmn

!  Format:

!      Call Liqrmn(a,lda,m,n,tau,y,b,work,lwork)

!  Parameters:

!      In    A        Real*8(lda,n)    Transformed matrix.
!                                      (Output of Liqrco)

!      In    lda      Integer          Leading dimension of A
!      In    m        Integer          number of rows A
!      In    n        Integer          number of cols A (n <= m)

!      In    tau      Real*8(n)        Constants from Householder
!                                      transformation (from Liqrpv/fa)

!      Inout b        Real*8(m)        On input: right hand side.
!                                      On output: solution
!      Out   work     Real*8           workspace.
!                                      On exit, if INFO = 0, WORK(1)
!                                      returns the optimal LWORK.
!      In    lwork    integer          the size of the workspace
!                                      If LWORK = -1, then a workspace query
!                                      is assumed; the routine only calculates
!                                      the optimal size of the WORK array,
!                                      and returns this value as the first
!                                      entry of the WORK array,



!  Description:

!      Subroutine Liqrmn applies the output of Liqrco to compute
!      a solution to the underdetermined linear system trans(A)*b = y.
!      This gives a vector b with minimal norm satisfying y = A'b.

!  Modified oct 1995 B.H.Hasselman
!  Modified jan 2013 Rob van Harrevelt: LAPACK / BLAS routines used.

if (m < n) return

!  **** underdetermined

if  (lwork == -1) then
    !  Determine optimal workspace for dormqr
    call dormqr('L', 'T', m, 1_LAPACK_IKIND, n, a, lda, tau, b, m, &
                work, -1_LAPACK_IKIND, info)
    return
endif

!     **** solve trans(R)*w=y
call dtrtrs('U', 'T', 'N', n, 1_LAPACK_IKIND, a, lda, b, n, info)
!     **** set elements > n in b to zero
if (m > n) b(n + 1 : m) = Rzero
!     **** compute b = Q*w
call dormqr('L', 'N', m, 1_LAPACK_IKIND, n, a, lda, tau, b, m, work, &
&           lwork, info)
return
end subroutine qrmn

end module liqrmn


