module mcjsbl

contains

subroutine gen_sjac(mdl, sjac, lds)
    use model_type
    use mccedp
    type(model), intent(in) :: mdl
    integer(kind = MC_IKIND), intent(in) :: lds
    integer(kind = MC_IKIND), intent(out)    :: sjac(lds,*)
 
!         Generate structure of feedback jacobian
!         Sjac[r,c] <> 0 if feedback c has effect on feedback r
 
!         Arguments
 
!            In     Mdl          type(model)      the model
!            In     Edptr        Integer(neq+1)   start pointers in Eqdep
!            In     Eqdep        Integer(maxarc)  dependency structure
!            Out    Sjac         Integer(lds,*)   structure of jacobian
!            In     Lds          Integer          leading dimension sjac
 

integer ::  equ
integer ::  i,j,k,vn
    integer(kind = MC_IKIND), dimension(:), allocatable :: work

    allocate (work(mdl%nrv))

!         for each feedback variable
!         go through model by ordering
!         if current lhs var depends on changed variable then mark it
!         at end for each feedbk variable fill in column of jacobian

    do i = 1, mdl%nfb

       work(:mdl%nrv) = 0
       work(mdl%numfb(i))  = 1

       do j = mdl%loops, mdl%loope

          equ = mdl%order(j)
          do k = edptr(equ), edptr(equ + 1) - 1
             vn = eqdep(k)
             if (work(vn) .gt. 0 ) then
                 work(mdl%lhsnum(equ)) = 2
                 exit
             endif
          end do

       end do

       do j = 1, mdl%nfb
          if (work(mdl%numfb(j)) .gt. 1 ) then
              sjac(j,i) = 1
          else
              sjac(j,i) = 0
          endif
       end do

    end do

    deallocate(work)
    return
end subroutine gen_sjac

!-----------------------------------------------------------------------

subroutine mcjzct(mdl, nzrcnt, work)
    use model_type
    use mccedp
    type(model) :: mdl
    integer(kind = MC_IKIND), intent(out) :: work(*)
    integer, intent(out) ::  nzrcnt
 
!         Calculate number of structural nonzero elements of feedback jacobian
 
!         Arguments
 
!            In     Mdl          type(model)      the model
!            In     Edptr        Integer(neq+1)   start pointers in Eqdep
!            In     Eqdep        Integer(maxarc)  dependency structure
!            Out    nzrcnt       Integer          # of structural nonzero in Jacobian
!            Inout  Work         Integer(nrv)     workspace
 

integer ::  equ
integer ::  i,j,k,vn,nz

!         for each feedback variable
!         go through model by ordering
!         if current lhs var depends on changed variable then mark it
!         at end  add non zero elements

    nz = 0

    do i = 1, mdl%nfb

       work(:mdl%nrv) = 0
       work(mdl%numfb(i))  = 1

       do j = mdl%loops, mdl%loope

          equ = mdl%order(j)
          do k = edptr(equ), edptr(equ + 1) - 1
             vn = eqdep(k)
             if (work(vn) .gt. 0 ) then
                 work(mdl%lhsnum(equ)) = 2
                 exit
             endif
          end do
       end do

       do j = 1, mdl%nfb
          if (work(mdl%numfb(j)) .gt. 1) then
              nz = nz + 1
          endif
       end do

    end do

    nzrcnt = nz

    return
end subroutine mcjzct

end module mcjsbl
