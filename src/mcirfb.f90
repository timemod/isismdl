module mcirfb

contains

subroutine rm_redundant_fb(mdl, orderr, errpar, usinac, fixepi, dofbrd, logunt)
use model_type
use kinds
use mcordr

!     Model has been ordered and has feedback variables
!     Check for redundant fbvars ansd iterate until no more redundancies

type(model), intent(inout) :: mdl
integer, intent(out) :: orderr, errpar
logical, intent(in), optional :: usinac, fixepi, dofbrd
integer, intent(in) :: logunt
logical :: mem_error

!     Locals

integer ::  fbrc, xpass, fixcnt, nfbp, stat

integer, dimension(:), allocatable :: fbred, fbrpos, fbrfix, fbtypfix
integer(kind = SOLVE_IKIND), dimension(:), allocatable :: owork
logical :: dofbrd_tmp

if (present(dofbrd)) then
    dofbrd_tmp = dofbrd
else
    dofbrd_tmp = .true.
endif

call alloc_fb_work(mem_error)
if (.not. mem_error) then
    allocate(owork(mdl%nrv), stat = stat)
    if (stat /= 0) mem_error = .true.
endif
if (mem_error) then
    orderr = 20
    goto 200
endif

!     Determine if feedback set + order has redundant feedbacks

call mcimsg(9,0,logunt)

xpass = 1

call mcfbrd

call mcimsg(10,fbrc, logunt)

if (fbrc .gt. 0 ) then
!        redundant feedback variables detected ==> print

   call mcfrms

   if( .not. dofbrd_tmp)  then
!           don't remove redundant feedback variables so quit
      call mcimsg(11, 1, logunt)
      goto 200
   endif

   call mcimsg(11,0   , logunt)

!        iterate until no more redundant feedback variables are detected

100 continue

!           remove redundancies and fix remaining feedback variables

      call mcfrfx

!           record number of feedbacks after removing the redundants

      nfbp = mdl%nfb - fbrc
      call order_equations(orderr, errpar, mdl, usinac = usinac, &
&                          fixepi = fixepi, numfix = fixcnt, &
&                          fbfix = fbrfix, fbtypfix = fbtypfix)

      if (orderr .ne. 0 ) goto 200

      if (mdl%nfb .gt. nfbp ) then
!              message that feedback sized has increased

         call mcimsg(16, mdl%nfb - nfbp, logunt)

!              increase size of work arrays
         if (mdl%nfb > size(fbred)) then
             call alloc_fb_work(mem_error)
             if (mem_error) then
                 orderr = 20
                 goto 200
             endif
         endif

!              check for redundancies again

         call mcfbrd

         if (fbrc .ne. 0 ) then
             call mcimsg(10,fbrc, logunt)
             if( xpass .lt. 5) then
                call mcfrms
                xpass = xpass + 1
                goto 100
             else
                call mcimsg(18,0 ,logunt)
             endif
         else
         endif
      else
!               No additional feedbacks created ==> done
           call mcimsg(16, mdl%nfb - nfbp, logunt)
      endif

      call mcimsg(17,xpass,logunt)

endif

200 continue
deallocate(fbred, stat = stat)
deallocate(fbrfix, stat = stat)
deallocate(fbrpos, stat = stat)
deallocate(fbtypfix, stat = stat)
deallocate(owork, stat = stat)

return

contains

    subroutine mcfbrd
    use mccedp

!         Investigate if ordering has redundant feedback variables
!         i.e. not influencing self
!           Fbrc will hold count of redundant fbvars
!           or 0 if none.
!           Fbred[1..fbrc] will hold numbers of redundant fbvars.
!           Fbrpos[1..fbrc] will hold position in original numfb[]
!           array.

integer ::  equ
integer ::  i,j,k,vn,fbnum
logical ::  chkfb

!         for each feedback variable
!            mark feedback variable as changed
!            go through model following the ordering (exclude feedback equations)
!               if current lhs var depends on changed variable then mark it !
!                changed
!            if equation for current feedback variable depends on
!              a changed variable then
!                 Feedback variable NOT REDUNDANT

    if (mdl%nfb .gt. 0 ) fbred(:mdl%nfb) = 0
    fbrc = 0

    chkfb = .true.

    fb_loop: do i = 1, mdl%nfb

!            heuristically chosen ==> check for redundancy
       chkfb = mdl%fbtype(i) .eq. 3

       if (chkfb) then

          owork(:mdl%nrv) = 0
          fbnum = mdl%numfb(i)
          owork(fbnum)  = 1

          do j = mdl%loops, mdl%loope - mdl%nfb
              equ = mdl%order(j)
              do k = edptr(equ), edptr(equ + 1) - 1
                 vn = eqdep(k)
                 if (owork(vn) .gt. 0 ) then
                     owork(mdl%lhsnum(equ)) = 1
                     exit
                 endif
              end do
          end do

          equ = equno(fbnum)
          do k = edptr(equ), edptr(equ + 1) - 1
             vn = eqdep(k)
             if (owork(vn) .gt. 0 ) then
!                      feedback variable depends on itself ==> not redundant
                 cycle fb_loop
             endif
          end do

!               this feedback variables does not depend on itself
!               it is therefore redundant

          fbrc = fbrc + 1
          fbred(fbrc) = fbnum
          fbrpos(fbrc)= i

       endif
    end do fb_loop

    return
    end subroutine mcfbrd

! ---------------------------------------------------------------------

    subroutine mcfrms
    use mdl_name_utils

!         print overview of redundant feedback variables


    character*80 str
integer ::  lenstr
integer ::  i
integer ::  nlen1,nlen2

    character*(mcmxnm) fbrnam, fbpnam

    do i = 1, fbrc
      call mcf7ex(fbrnam, nlen1, mdl%ivnames(fbred(i)), mdl%vnames)
      if (fbrpos(i) .gt. 1) then
          call mcf7ex(fbpnam, nlen2, &
&           mdl%ivnames(mdl%numfb(fbrpos(i)-1)), mdl%vnames)
          write(str,'(5a)') 'B12*>Redundant fb variable ', &
&           fbrnam(1:nlen1),' after ',fbpnam(1:nlen2),'*EX'
      else
          write(str,'(5a)') 'B12*>Redundant fb variable ', &
&           fbrnam(1:nlen1),' (first)*EX'
      endif

      !call fmtlog(str, logunt)
    end do

    return
    end subroutine mcfrms

! ---------------------------------------------------------------------

    subroutine mcfrfx

!         generate a list of fixed feedback variables from
!         current feedback list by removing redundant ones
!         specified in fbred[1..fbrc]

integer ::  i,rp

        fixcnt = 0
        rp     = 1
        do i = 1, mdl%nfb
           if( rp .gt. fbrc ) then
               fixcnt = fixcnt + 1
               fbrfix(fixcnt) = mdl%numfb(i)
               fbtypfix(fixcnt) = mdl%fbtype(i)
           elseif (mdl%numfb(i) .ne. fbred(rp)) then
               fixcnt = fixcnt + 1
               fbrfix(fixcnt) = mdl%numfb(i)
               fbtypfix(fixcnt) = mdl%fbtype(i)
           else
               rp = rp + 1
           endif
        end do

        return
    end subroutine mcfrfx

! ---------------------------------------------------------------------

    subroutine alloc_fb_work(error)
        logical, intent(out) :: error

        integer :: stat

        deallocate(fbred, stat = stat)
        deallocate(fbrfix, stat = stat)
        deallocate(fbrpos, stat = stat)
        deallocate(fbtypfix, stat = stat)

        allocate(fbred(mdl%nfb), stat = stat)
        if (stat == 0) allocate(fbrfix(mdl%nfb),   stat = stat)
        if (stat == 0) allocate(fbrpos(mdl%nfb),   stat = stat)
        if (stat == 0) allocate(fbtypfix(mdl%nfb), stat = stat)

        error = stat /= 0
    end subroutine alloc_fb_work

end subroutine rm_redundant_fb

end module mcirfb
