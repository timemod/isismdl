module liqrsq

contains

!*NOTE Linear algebra QR solving FORTRAN routine
! Solve Q * R * x = b, where A = Q * R is a square matrix.
subroutine qrsq(a, lda, n, tau, b, work, lwork)
use kinds
integer(kind = LI_IKIND), intent(in) ::  lda, n, lwork
real(kind = ISIS_RKIND), intent(in) :: a(lda,*), tau(*)
real(kind = ISIS_RKIND), intent(inout) :: b(*)
real(kind = ISIS_RKIND), intent(out) :: work(*)

integer(kind = LAPACK_IKIND) :: info

!  Subroutine Liqrsq

!  Format:

!      Call qrsq(a,lda,n,tau,b,work,lwork)

!  Parameters:

!      In    A        Real*8(Lda,n)    Transformed matrix.

!      In    Lda      Integer          Leading dimension of A
!      In    n        Integer          number of rows A
!      In    tau      Real*8(n)        Householder constants

!      Inout b        Real*8(n)        On entry the right hand side vector b.
!                                      On return the solution vector x.
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

!      Subroutine Liqrsq solves the linear system A * x = b
!      from the QR decomposition of a square matrix A.
!      The solution is returned in b (overwrite)

!  Modified oct 1995 B.H.Hasselman
!  Modified jan 2013 R. van Harrevelt: use LAPACK routine dtrtrs

if  (lwork == -1) then
!   Determine optimal workspace for dormqr
    call dormqr('L', 'T', n, 1_LAPACK_IKIND, n, a, lda, tau, b, n, &
&               work, -1_LAPACK_IKIND, info)
    return
endif

!     **** Solve Q * R * x = b

!  **** compute trans(Q) * b
call dormqr('L', 'T', n, 1_LAPACK_IKIND, n, a, lda, tau, b, n, &
&           work, lwork, info)
!  **** solve R * x = trans(Q) * b
call dtrtrs('U', 'N', 'N', n, 1_LAPACK_IKIND, a, lda, b, n, info)

return
end subroutine qrsq

end module liqrsq
