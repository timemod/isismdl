module mcfbor

contains

subroutine gen_fb_order(fbotp, mdl, ier)
    use model_type
    use mccedp
    integer, intent(in)  :: fbotp
    type(model), intent(inout) :: mdl
    integer, intent(out) :: ier

!     Description
 
!     Generate order (solution sequence) for each feedback variable
!     in arrays Fboptr and Fbordr. This order is used for the efficient
!     calculation of the Jacobian.
 
!     For feedback variable <k> the equations to solve in order are in
 
!        Fbordr[ Fboptr[k] .. Fboptr[k+1]-1 ]
 
!     where Fboptr[1] = 1
 
!     Arrays Edptr and Eqdep are the output of procedure <mcgedp>
!     Scalar Eqvtot is the output of procedure <mcfbct> or <mcjsbl>
 
!     Calling sequence for getting feedback ordering to work
 
!        1. mcgedp/mccedp to fill Edptr and Eqdep
!        2. mcfbct and allocate Fbptr/Fbordr
!        3. mcfbor
 
!     Specify fbotp = 0 when an ordering is required for numerical calculation
!             fbotp = 1 when an ordering is required for symbolic jacobian
 
!     When fbotp = 0 then usually slightly less memory is used for Fbordr
!     !! call mcfbct with same fbotyp argument as mcfbor
 
!     Arguments
 
!        In     fbotp        Integer          type of feedback ordering
!                                             0  to exclude feedback equations
!                                             1  to include feedback equations
!        Out    Ier          ier              Error flag.
!                                             0  feedback ordering
!                                                generated
!                                             1  no feedback variables
!                                             2  feedback jacobian too
!                                                dense
!                                             3  not enough memory

integer ::  stat, equ
integer ::  i,j,k,vn,fbp,jb,je, idum
    integer(kind = MC_IKIND), dimension(:), allocatable :: work

    mdl%fbomem = 0
    deallocate(mdl%fboptr, stat = idum)
    deallocate(mdl%fbordr, stat = idum)

    if (mdl%nfb == 0) then
        ier = 1
        return
    endif

    jb = mdl%loops
    if (fbotp .eq. 0) then
       je = mdl%loope - mdl%nfb
    else
       je = mdl%loope
    endif

    allocate(work(mdl%nrv), stat = stat)
    if (stat > 0) then
        ier = 3
        goto 999
    endif

    mdl%fbomem = get_fbomem()

    if (mdl%fbomem <= 0) then
          ier = 2
          goto 999
    else if ((mdl%fbomem + mdl%nfb * mdl%nfb) / &
&       (mdl%loope - mdl%loops+1) .gt. (9 * mdl%nfb) / 10) then
!               do not use feedback ordering if implied number of jacobian steps
!               does not lead to a reduction of at least 10% wrt full method
          mdl%fbomem = 0
          ier = 2
          goto 999
    else
          allocate(mdl%fboptr(mdl%nfb + 1), stat = stat)
          if (stat == 0) then
              allocate(mdl%fbordr(mdl%fbomem), stat = stat)
          endif
          if (stat /= 0) then
!                   not enough memory for calculating fb ordering
              deallocate(mdl%fboptr, stat = idum)
              deallocate(mdl%fbordr, stat = idum)
              mdl%fbomem = 0
              ier = 3
              goto 999
          else
              ier  = 0
          endif
    endif

    if (mdl%fbomem <= 0) goto 999


!         for each feedback variable
!         go through model by ordering
!         if current lhs var depends on changed variable then mark it
!         store equation visited

    fbp = 0
    mdl%fboptr(1) = 1


    do i = 1, mdl%nfb

       work(:mdl%nrv) = 0
       work(mdl%numfb(i))  = 1

       do j = jb, je

          equ = mdl%order(j)
          do k = edptr(equ), edptr(equ + 1) - 1
             vn = eqdep(k)
             if (work(vn) .gt. 0 ) then
                 work(mdl%lhsnum(equ)) = 1
                 fbp = fbp + 1
                 mdl%fbordr(fbp) = equ
                 exit
             endif
          end do

       end do
       mdl%fboptr(i + 1) = fbp + 1

    end do


999 if (allocated(work)) deallocate(work)

    return

    contains
        integer function get_fbomem()

            get_fbomem = 0

            do i = 1, mdl%nfb

                work(:mdl%nrv) = 0
                work(mdl%numfb(i))  = 1

                do j = jb, je
                   equ = mdl%order(j)
                   do k = edptr(equ), edptr(equ + 1) - 1
                       vn = eqdep(k)
                       if (work(vn) .gt. 0 ) then
                           work(mdl%lhsnum(equ)) = 1
                           get_fbomem = get_fbomem + 1
                           exit
                       endif
                   end do
                end do
            end do
            return
        end function get_fbomem

end subroutine gen_fb_order

end module mcfbor
