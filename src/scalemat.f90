module scalemat

use kinds
use nuv

private :: scale_normbal

integer, parameter :: NO_SCALING = -1
integer, parameter :: NORMBAL    =  0
integer, parameter :: LIMBAL     =  1

contains

  subroutine scale_matrix(a, lda, n, scale, method, ier)
      real(kind = SOLVE_RKIND), intent(inout) :: a(lda, *)
      integer(kind = SOLVE_IKIND), intent(in) :: lda, n
      real(kind = SOLVE_RKIND), intent(out) :: scale(*)
      integer, intent(in) :: method
      integer, intent(out) :: ier

      integer(kind = LAPACK_IKIND) :: idum, info

      if (method == NORMBAL) then

          call scale_normbal(a, lda, n, scale, ier)

      else if (method == LIMBAL) then

          call dgebal('S', n, a, lda, idum, idum, scale, info)

      endif
      return

  end subroutine scale_matrix

!-----------------------------------------------------------------------

  subroutine scale_normbal(a, lda, n, scale, ier)
      use nucnst
      use nuna

      ! generate or update scale factors for the feedback variables
      ! such that the rows and columns of the scaled aian are commensurate

      ! return status in ier

      ! ier =  0   all went ok
      ! ier = -1   10 iterations not enough to update scale factors

      real(kind = SOLVE_RKIND), intent(inout) :: a(lda,*)
      integer(kind = SOLVE_IKIND), intent(in) :: n, lda
      real(kind = SOLVE_RKIND), intent(out) :: scale(*)
      integer, intent(out) :: ier

      real(kind = ISIS_RKIND) :: srow, scol, fac, rmax, cmax
      real(kind = ISIS_RKIND) :: sfmin, sfmax, sh
      integer(kind = LI_IKIND) :: i, j, m, itt, ridx, cidx
      logical :: presca

      ier = 0

      sfmin = Rtiny / (Rtwo * Rmeps)
      sfmax = Rone / sfmin

      ! determine if we can use current scale factors

      ridx = idamax(n, scale, 1)
      sh = scale(ridx)

      presca = .true.
      do i=1,n
!       ---     if largest column entry * largest scale / scale(i) is NA ==> quit
          ridx = idamax(n, a(i,1), lda)
          rmax = abs(a(i,ridx))
          if(nuifna(rmax*sh/scale(i))) then
             presca = .false.
             exit
          endif
      end do

      if (presca) then
          do i=1, n
               do j= 1, n
                   a(i,j) = a(i,j) * scale(j) / scale(i)
               end do
           end do
      else
           scale(1:n) = Rone
      endif

      do m = 1, 10
         itt = 0
         do i = 1, n

!         -----   srow/scol Euclidian norm of row / column

            srow = dnrm2(n, a(i,1), lda)
            scol = dnrm2(n, a(1,i), 1)

!         -----   determine largest entry of column i

            cidx = idamax(n, a(1,i), 1)
            cmax = abs(a(cidx,i))

!         -----   determine largest entry of row i

            ridx = idamax(n, a(i,1), lda)
            rmax = abs(a(i,ridx))

            fac = sqrt(srow/scol)

!         -----   test for legal fac values

            if(nuifna(fac)) cycle

!         -----   ensure legal and safe values for new scale factor

            sh = scale(i) * fac
            if(nuifna(sh)) cycle
            if( sh .le. Sfmin .or. sh .gt. Sfmax ) cycle

!         -----   ensure that scaling result will be legal
!         -----   should correspond to approximately fac*cmax <= sfmax and rmax/fac <= sfmax
!         -----   ensure that no zero due to underflow occur

            if(nuifna(fac*cmax)) cycle
            if(nuifna(rmax/fac)) cycle
            if( fac .le. sfmin ) cycle
            if( fac*cmax .lt. sfmin .or. rmax/fac .lt. sfmin ) cycle

            if (fac  > Rtwo .or. fac < Rhalf) then
                itt = itt + 1
                call dscal(n, Rone / fac, a(i,1), lda)
                call dscal(n, fac, a(1,i), 1)
                scale(i) = sh
            endif

         end do
         if (itt .eq. 0) then
             ier = -1
             exit
         endif
      end do

      return
  end subroutine scale_normbal

  subroutine DGEEQU_COL(M, N, A, LDA, C, COLCND, AMAX, INFO )

!       a variant of LAPACK dgeequ routine with only column scaling,
!       used in the fit procedure
!
!  -- LAPACK computational routine (version 3.4.0) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
!     .. Scalar Arguments ..
INTEGER(KIND = LAPACK_IKIND) :: INFO, LDA, M, N
DOUBLE PRECISION   AMAX, COLCND
!     ..
!     .. Array Arguments ..
DOUBLE PRECISION   A( LDA, * ), C( * )
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
DOUBLE PRECISION   ONE, ZERO
PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
INTEGER(KIND = LAPACK_IKIND) :: I, J
DOUBLE PRECISION   BIGNUM, RCMAX, RCMIN, SMLNUM
!     ..
!     .. External Functions ..
DOUBLE PRECISION   DLAMCH
EXTERNAL           DLAMCH
!     ..
!     .. External Subroutines ..
EXTERNAL           XERBLA
!     ..
!     .. Intrinsic Functions ..
INTRINSIC          ABS, MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
INFO = 0
IF( M.LT.0 ) THEN
   INFO = -1
ELSE IF( N.LT.0 ) THEN
   INFO = -2
ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
   INFO = -4
END IF
IF( INFO.NE.0 ) THEN
   CALL XERBLA( 'DGEEQU', -INFO )
   RETURN
END IF
!
!     Quick return if possible
!
IF( M.EQ.0 .OR. N.EQ.0 ) THEN
   COLCND = ONE
   AMAX = ZERO
   RETURN
END IF
!
!     Get machine constants.
!
SMLNUM = DLAMCH( 'S' )
BIGNUM = ONE / SMLNUM
!
!     Compute column scale factors
!
do  J = 1, N
   C( J ) = ZERO
enddo
!
!     Find the maximum element in each column,
!     assuming the row scaling computed above.
!
do  J = 1, N
do  I = 1, M
      C( J ) = MAX( C( J ), ABS( A( I, J ) ))
enddo
enddo
!
!     Find the maximum and minimum scale factors.
!
RCMIN = BIGNUM
RCMAX = ZERO
do  J = 1, N
   RCMIN = MIN( RCMIN, C( J ) )
   RCMAX = MAX( RCMAX, C( J ) )
enddo
!
IF( RCMIN.EQ.ZERO ) THEN
!
!        Find the first zero scale factor and return an error code.
!
do  J = 1, N
      IF( C( J ).EQ.ZERO ) THEN
         INFO = M + J
         RETURN
      END IF
enddo
ELSE
!
!        Invert the scale factors.
!
do  J = 1, N
      C( J ) = ONE / MIN( MAX( C( J ), SMLNUM ), BIGNUM )
enddo
!
!        Compute COLCND = min(C(J)) / max(C(J))
!
   COLCND = MAX( RCMIN, SMLNUM ) / MIN( RCMAX, BIGNUM )
END IF
!
RETURN
!
!     End of DGEEQU_COL
!
end subroutine dgeequ_col

end module scalemat
