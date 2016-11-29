module liqrup

contains

subroutine qrup(q,ldq,n,r,ldr,u,v,wk)
use kinds
use nuv
integer(kind = LI_IKIND), intent(in) :: ldq, n, ldr
real(kind = ISIS_RKIND), intent(inout) :: q(ldq,*), r(ldr,*)
real(kind = ISIS_RKIND), intent(in) :: u(*), v(*)
real(kind = ISIS_RKIND), intent(out) :: wk(*)

!     Format

!     qrup(q,ldq,n,r,ldr,u,v,wk)


!     Parameters

!     Inout  Q       Real*8(ldq,n)    orthogonal matrix from QR
!     In     ldq     Integer          leading dimension of Q
!     In     n       Integer          order of Q and R
!     Inout  R       Real*8(ldr,n)    upper triangular matrix R from QR
!     In     ldr     Integer          leading dimension of R
!     In     u       Real*8(*)        vector u of size n
!     In     v       Real*8(*)        vector v of size n
!     Out    wk      Real*8(*)        workspace of size n

!     on return

!        Q       Q is the matrix with orthonormal columns in a QR
!                decomposition of the matrix B = A + u*v'

!        R       R is the upper triangular matrix in a QR
!                decomposition of the matrix B = A + u*v'

!     Description

!     The matrices Q and R are a QR decomposition of a square matrix
!     A = Q*R.
!     Given Q and R, qrupdt computes a QR decomposition of the rank one
!     modification B = A + u*trans(v) of A. Here u and v are vectors.

!     Source : Algorithm 686
!              Fortran subroutines for updating the QR decomposition
!              ACM Transactions on Mathematical software, dec. 1990

!     Local variables and functions

integer(kind = LAPACK_IKIND) :: k, i
real(kind = ISIS_RKIND) :: c,s,tmp

!     calculate wk = trans(Q)*u

do i = 1, n
   wk(i) = ddot(int(n, ISIS_IKIND), q(1,i), 1, u, 1)
end do

!     zero components wk(n),wk(n-1)...wk(2)
!     and apply rotators to R and Q.

do k = n - 1, 1, -1
   call nuvgiv(wk(k),wk(k+1),c,s)
   call drot(n - k + 1_LAPACK_IKIND, r(k,k), ldr, &
&            r(k+1,k), ldr, c,s)
   call drot(n, q(1,k), 1_LAPACK_IKIND, &
&            q(1, k+1), 1_LAPACK_IKIND,c, s)
end do

tmp = wk(1)
do  k=1,n
  r(1,k) = r(1,k) + tmp*v(k)
enddo

!     R is of upper hessenberg form. Triangularize R.

do k = 1, n - 1
   call nuvgiv(r(k,k),r(k+1,k),c,s)
   call drot(n - k, r(k,k+1), ldr, r(k+1,k+1), ldr, c, s)
   call drot(n, q(1,k), 1_LAPACK_IKIND, q(1,k+1), &
&            1_LAPACK_IKIND,c, s)
end do

return
end subroutine qrup

end module liqrup
