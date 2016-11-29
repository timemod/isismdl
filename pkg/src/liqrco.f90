module liqrco

contains

! Subroutine liqrco performs a A = QR factorisation and estimates
! the inverse condition of matrix A using the factorisation.
subroutine qrco(a, lda, m, n, tau, rcond, work, lwork)
use kinds
real(kind = ISIS_RKIND), intent(inout) :: a(lda,*)
integer(kind = LI_IKIND), intent(in) :: lda, m, n, lwork
real(kind = ISIS_RKIND), intent(out) :: tau(*), rcond, work(*)

integer(kind = LAPACK_IKIND) :: info

!  Subroutine Liqrco

!  Format:

!      call liqrco(A,Lda,m,n,tau,rcond,work,lwork)

!  Parameters:

!      Inout A        Real*8(Lda,n)    Matrix to transform.

!      In    lda      Integer          Leading dimension of A
!      In    m        Integer          number of rows A
!      In    n        Integer          number of cols A

!      Out   tau      Real*8(n)        Information for recovering
!                                      orthogonal part of decomposition
!      Out   Rcond    Real*8           estimated inverse condition of A
!      Out   work     Real*8           workspace.
!                                      On exit, if INFO = 0, WORK(1)
!                                      returns the optimal LWORK.
!      In    lwork    integer          the size of the workspace
!                                      If LWORK = -1, then a workspace query
!                                      is assumed; the routine only calculates
!                                      the optimal size of the WORK array,
!                                      and returns this value as the first
!                                      entry of the WORK array,

!   Description: !
!   Liqrco reduces A to upper triangular form (QR-decomposing) using
!   Housholder transformations, so that A = Q*R.

!   On return A contains the decomposition. The upper triangle
!   contains the upper triangular matrix R of the QR factorization.
!   Below the diagonal, it contains information on the Householder
!   transformations from which the Householder vectors can be
!   recovered.

!   Tau contains constants from the Householder reflectors.

!    on exit, the elements on and above the diagonal of the array
!    contain the min(m,n) by n upper trapezoidal matrix R (R is
!    upper triangular if m >= n); the elements below the diagonal,
!    with the array tau, represent the orthogonal matrix Q as a
!    product of elementary reflectors (see further details).

!  Further details

!  The matrix Q is represented as a product of elementary reflectors

!     Q = H(1) H(2) . . . H(k), where k = min(m,n).

!  each H(i) has the form

!     H(i) = I - tau * v * trans(v)

!  where tau is a real scalar, and v is a real vector with
!  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
!  and tau in tau(i).


if  (lwork == -1) then

!         Determine optimal workspace for dqeqrf / dtrcon.
!         dtrcon requires a workspace 4 * n.
    call dgeqrf(m, n, a, lda, tau, work, -1_LAPACK_IKIND, info)
    work(1) = max(work(1), 4.0 * n)
    return
endif

!
!     Perform QR factorisation
!
call dgeqrf(m,  n, a, lda, tau, work, lwork, info)
!
!    Estimate inverse condition
!
call dtrcon('1', 'U', 'N', n, a, lda, rcond, work, work(3 * n + 1), info)

return
end subroutine qrco

end module liqrco
