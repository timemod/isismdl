 module svd_anal

 private :: svdout_problemc, svdout_vector, svdout

 contains
 
!     Use the SVD of matrix mat to determine which columns or rows cause
!     rank loss. This routine should only be called when the matrix
!     is (nearly) ill-conditioned.
 
subroutine svd_analysis(mat, ldm, m, n, num_row, num_col, fit, svd_tol, error)
 
    !     Input:
    !       mat matrix with dimensions m x n
    !       ldm : leading dimension of mat.
    !       m and n : row and column dimension of mat
    !       num_row an array with the model variable indices for the
    !               rows of mat
    !       num_col an array with the model variable indices for the
    !               columns of  mat
    !       fit     true if matrix is transpose of the fit jacobian, false if the
    !               matrix is the Newton jacobian
    !       svd_tol tolerance for singular values
    !     Outout:
    !       error   0 ok
    !               1 not enough memory

    use model_params
    use mdl_name_utils
    use kinds
    use nuv
    use nuna
    use nucnst
    
    integer(kind = MC_IKIND), intent(in) :: m, n, ldm
    integer(kind = MC_IKIND), intent(in) :: num_col(*), num_row(*)
    logical, intent(in) :: fit
    real(kind = ISIS_RKIND), intent(in) :: mat(ldm, *), svd_tol
    integer, intent(out) :: error
    
    real(kind = ISIS_RKIND), dimension(:,:), allocatable :: u, vt
    real(kind = ISIS_RKIND), dimension(:), allocatable :: sv, work, vec
    real(kind = ISIS_RKIND) :: rlwork
    real(kind = ISIS_RKIND), dimension(:), allocatable :: minc, minr
    integer :: info, lwork, i, j, min_m_n, max_m_n, stat
    
    error = 0
    
    min_m_n = min(m, n)
    max_m_n = max(m, n)
    
    allocate(u(m, m), stat = stat)
    if (stat == 0) allocate(vt(n, n), stat = stat)
    if (stat == 0) allocate(sv(min_m_n), stat = stat)
    if (stat == 0) allocate(minc(n), stat = stat)
    if (stat == 0) allocate(minr(m), stat = stat)
    if (stat == 0) allocate(vec(max_m_n), stat = stat)
    if (stat /= 0) then
        call svdot1
        error = 1
        return
    endif
    
    ! get 1-norm of the columns of mat
    do i = 1, n
        minc(i) = dasum(int(m, ISIS_IKIND), mat(1, i), 1)
        if (nuifna(minc(i))) then
            call svdout(7, fit)
            goto 999
        endif
    enddo

    ! get 1-norm of the rows of mat
    do i = 1, m
        minr(i) = dasum(int(n, ISIS_IKIND), mat(i, 1), int(m, ISIS_IKIND))
    enddo
    
    call dgesvd('A', 'A', m, n, mat, m, sv, u, m, vt, n, rlwork, -1, info)
    lwork = nint(rlwork)
    allocate(work(lwork), stat = stat)
    if (stat /= 0) then
        call svdot1
        error = 1
        return
    endif
    call dgesvd('A', 'A', m, n, mat, m, sv, u, m, vt, n, work, lwork, info)
    
    if (sv(n)/sv(1) > svd_tol) then
        goto 999
    endif
    
    call svdout(1, fit)
    
    ! print left singular vectors
    call svdout(2, fit)
    do i = 1, min_m_n
        if ((sv(i) / sv(1)) <= svd_tol) then
            do j = 1, m
                vec(j) = u(j, i)
            end do
            call svdout_vector(m, sv(i)/sv(1), vec, num_row)
        endif
    end do

    call svdout(100, fit)
    
    ! print right  singular vectors
    call svdout(3, fit)
    do i = 1, min_m_n
        if ((sv(i) / sv(1)) <= svd_tol) then
            do j = 1, n
                vec(j) = vt(i, j)
            end do
            call svdout_vector(n, sv(i)/sv(1), vec, num_col)
        endif
    end do
    call svdout(100, fit)

    ! print the L1-norm of "problem rows"
    call svdout(4, fit)
    do i = 1, m
        do j= 1, min_m_n
            if  (( (sv(j)/sv(1)) <= svd_tol) &
                  .and. (abs(u(i, j)) >= sqrt(Rmeps))) then
               call svdout_problemc(num_row(i), minr(i))
               exit
            endif
        enddo
    enddo
    
    ! print the L1-norm of "problem columns"
    call svdout(5, fit)
    do i = 1, n
        do j = 1, min_m_n
            if  (( (sv(j)/sv(1)) <= svd_tol) &
                  .and. (abs(vt(j, i)) >= sqrt(Rmeps))) then
                call svdout_problemc(num_col(i), minc(i))
               exit
            endif
        end do
    enddo

    call svdout(6, fit)
    
    999 continue
    
    deallocate(work, stat = stat)
    deallocate(u, stat = stat)
    deallocate(vt, stat = stat)
    deallocate(sv, stat = stat)
    deallocate(minc, stat = stat)
    deallocate(minr, stat = stat)
    deallocate(vec, stat = stat)

    return
end subroutine svd_analysis

!-----------------------------------------------------------------------

subroutine svdout(imsg, fit)
    use msimot
    use nucnst
integer ::  imsg
logical ::  fit

   if (imsg == 1) then

       str = ''
       call strout(O_OUTN)
       str = '*** SVD analysis ***'
       call strout(O_OUTN)
       str = '======================='
       call strout(O_OUTN)
       str = "The purpose of the SVD analysis is to find linearly dependent rows or columns"
       call strout(O_OUTN)
       if (fit) then
           str = "of the fit jacobian."
       else
           str = "in the jacobian"
       endif
       call strout(O_OUTN)
       str = 'A left singular vector is a linear combination of the rows of the jacobian'
       call strout(O_OUTN)
       str = 'that is (almost) zero.'
       call strout(O_OUTN)
       str = 'A right singular vector is a linear combination of the columns of the jacobian'
       call strout(O_OUTN)
       str = 'that is (almost) zero.'
       call strout(O_OUTN)
       write(str, '(A19, g10.2, A12)', round = 'compatible') "Only components >= ", sqrt(Rmeps), ' are shown.'
       call strout(O_OUTN)
       str = ''
       call strout(O_OUTN)

   elseif (imsg == 2) then

       str = 'Left Singular vectors:'
       call strout(O_OUTN)
       str = '----------------------'
       call strout(O_OUTN)

   elseif (imsg == 3) then

       str = 'Right Singular vectors:'
       call strout(O_OUTN)
       str = '-----------------------'
       call strout(O_OUTN)

   elseif (imsg == 4) then

       str = ''
       call strout(O_OUTN)
       str = 'The singularity may also be caused by (almost) zero rows or columns.'
       call strout(O_OUTN)
       str = 'Therefore we print the norm of "problem rows" and "problem columns"'
       call strout(O_OUTN)
       str = '(rows and columns with significant components in left and right singular vectors resp.)'
       call strout(O_OUTN)
       str = ''
       call strout(O_OUTN)
       str = 'Problem rows:'
       call strout(O_OUTN)
       str = '-------------'
       call strout(O_OUTN)
       write(str, '(A32, A19)') 'Variable', 'L1 norm of row'
       call strout(O_OUTN)

   elseif (imsg == 5) then

       str = ''
       call strout(O_OUTN)
       str = 'Problem columns:'
       call strout(O_OUTN)
       str = '----------------'
       call strout(O_OUTN)
       write(str, '(A32, A19)') 'Variable', 'L1 norm of column'
       call strout(O_OUTN)

   elseif (imsg == 6) then

       str = ''
       call strout(O_OUTN)
       str = '*** END SVD ANALYSIS *** '
       call strout(O_OUTN)
       str = ''
       call strout(O_OUTN)

   elseif (imsg == 7) then

       str = ''
       call strout(O_OUTN)
       str = 'Matrix contains invalid numbers'
       call strout(O_OUTN)
       str = 'Not possible to perform SVD analysis'
       call strout(O_OUTN)
       str = ''
       call strout(O_OUTN)

   else

       str = ''
       call strout(O_OUTN)

   endif

return
end subroutine svdout

!-----------------------------------------------------------------------

subroutine svdout_problemc(iv, column_sum)
use msvars
use msimot
use mdl_name_utils

!     print message about problem column of derivatives in fit jacobian

integer(kind = MC_IKIND) ::  iv
real(kind = ISIS_RKIND) :: column_sum

call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
write(str, '(A32, g10.2)', round = 'compatible' ) name(:nlen), column_sum
call strout(O_OUTN)
return
end subroutine svdout_problemc

!-----------------------------------------------------------------------

subroutine svdout_vector(n, sv, vector, num_col)
    use msvars
    use msimot
    use mdl_name_utils
    use nucnst
    integer(kind = ISIS_IKIND) ::  n, num_col(*)
    real(kind = ISIS_RKIND) :: sv, vector(n)
    real(kind = 8) :: tol
    
    integer ::  i

    write(str, '(A15, g10.2)', round = 'compatible') "Singular value ", sv
    call strout(O_OUTN)


    tol = sqrt(Rmeps)

    do i = 1, n
        if (abs(vector(i)) >= tol) then
            call mcf7ex(name, nlen, mdl%ivnames(num_col(i)), mdl%vnames)
            write(str, '(A32, g10.2)', round = 'compatible') &
&               name(:nlen), vector(i)
            call strout(O_OUTN)
        endif
   end do

end subroutine svdout_vector

!-----------------------------------------------------------------------

subroutine svdot1
   use output_utils

   ! print message about insufficient memory for svd analysis

   call isismdl_error('Not enough memory for the svd analysis')


end subroutine svdot1

end module svd_anal
