module mccedp
    use kinds
    use model_type
    integer, save :: maxarc
    integer(kind = SOLVE_IKIND), dimension(:), allocatable, save &
&              :: equno, eqdep, edptr

    integer, private, save :: dep_alloc_stat
    private :: init_dep

    contains

!-----------------------------------------------------------------------


subroutine mcrddp(mdl, ier, errpar)
use mcdep
use mcpars

type(model), intent(in) :: mdl
integer, intent(out) :: ier, errpar

!     Read dependency structure from file depunt.
!     This procedure is used for a model that has just
!     been compiled and is in model compiler memory.

!     dependencies are given as variable numbers

!     dependencies for equation <k> are in

!           Eqdep[ edptr[k] .. edptr[k+1]-1 ]

!     where Edptr[1] = 1

!     Arguments

!        Out    Ier          Integer          error flag
!                                             0 : o.k.
!                                             1 : not enough memory
!                                             8/9/6/7 : ???

!        Out    Errpar       Integer          extra error parameter


integer(kind = MC_IKIND) :: i, k, curep,vn, ii
integer :: ios

ier = 0

call init_dep(mdl)
if (dep_alloc_stat > 0) then
    ier = 1
    return
endif

rewind depunt

curep = 0
edptr(1) = 1
do i = 1, mdl%neq
   read(depunt,err=900,iostat=ios,end=910) ii,nsimv,isimv(1:min(nsimv,MAXRHS))
   if( nsimv .gt. MAXRHS ) goto 930
   if( mdl%lhsnum(i) .ne. ii ) goto 920

   do k=1,nsimv
      vn = isimv(k)
      if( equno(vn) .ne. 0 ) then
          curep = curep + 1
          eqdep(curep) = vn
      endif
   end do
   edptr(i+1) = curep + 1
end do

goto 1000

!     error returns

900 ier = 8
goto 1000
910 ier = 9
errpar = ios
goto 1000
920 ier = 6
errpar = ii
goto 1000

!     this one shouldn't happen
930 ier = 7

1000 return
end subroutine mcrddp

!-----------------------------------------------------------------------

subroutine mcgedp(mdl, useleads, ier)
use mcdep
use mcodep
type(model), intent(in) :: mdl
logical, intent(in) :: useleads
integer, intent(out) :: ier

!     Generate endogenous dependency structure for a model in solve
!     memory.
!     Dependencies are given as variable numbers
!     May only be called after a model has been read with succes

!     dependencies for equation <k> are in

!           Eqdep[ edptr[k] .. edptr[k+1]-1 ]

!     where Edptr[1] = 1

!     Arguments

!        In     useleads     Logical          .true. if leads must be
!                                             as current
!        Out    Ier          Integer          error flag
!                                             0  : o.k.
!                                             10 : not enough memory
!                                             other: see mcodep.rf7


integer ::  i,k, curep,vn

ier = 0

maxarc = get_maxarc(useleads, ier)
if (ier /= 0) return

call init_dep(mdl)
if (dep_alloc_stat > 0) then
    ier = 10
    return
endif

curep = 0
edptr(1) = 1
do i = 1, mdl%neq
    call mcvdep_eq(mdl, i, ORDMDL, useleads, ier)
    if (ier.ne. 0) exit
    do k = 1, nsimv
       vn = isimv(k)
       if (equno(vn) .ne. 0 ) then
           curep = curep + 1
           eqdep(curep) = vn
       endif
    enddo
    edptr(i+1) = curep + 1
 enddo

return

contains
    integer function get_maxarc(useleads, ier)
        logical, intent(in) :: useleads
        integer, intent(out) :: ier

        integer ::  larc, i

        get_maxarc = 1

        larc = 0
        do i = 1, mdl%neq
            call mcvdep_eq(mdl, i, ORDMDL, useleads, ier)
            if (ier /= 0) return
            larc = larc + nsimv
        end do

        get_maxarc = max(larc, 1)

    end function get_maxarc

end subroutine mcgedp

!-----------------------------------------------------------------------

subroutine mc_analyse_model(ier)
use mcvars
use invpol
use mcdep
use mcpars
use mcodep

integer, intent(out) :: ier

!     Analyse dependcies of model just compiled with procedure
!     compile_model and write dependencies to scratch file depunt.
!     The function and equation code is read from scratch files
!     ufnunt and polunt.

!     Arguments

!        Out    Ier          Integer          error flag
!                                             0  : o.k.
!                                             10 : not enough memory
!                                             other: see mcodep.rf7


integer ::  i, j, ios, fstat, idum
integer(kind = MC_IKIND) :: ipol

ier = 0
maxlag(:mdl%nrv) = 0
mxlead(:mdl%nrv) = 0
maxarc = 0
maxref = 0
ipol = 0

! equat should contain the code of all user functions
! plus the code for one equation (max. size is maxpol)
deallocate(mdl%nrecuf, mdl%equat, stat = idum)
allocate(mdl%nrecuf(mdl%nuf + 1), stat = dep_alloc_stat)
if (dep_alloc_stat == 0) then
    allocate(mdl%equat(mdl%ufblen + maxpol), stat = dep_alloc_stat)
endif
if (dep_alloc_stat > 0) then
    ier = 10
    return
endif

!     read polish code user functions from file ufnunt

rewind ufnunt
do i = 1, mdl%nuf
    mdl%nrecuf(i) = ipol
    read(ufnunt, err=895, iostat = fstat) npol, mdl%equat(ipol + 1 : ipol + npol)
    ipol = ipol + npol
enddo
mdl%nrecuf(mdl%nuf + 1) = ipol

rewind depunt
rewind polunt
do i = 1, mdl%neq
   read(polunt, err=895, iostat=fstat) npol, mdl%equat(ipol + 1 : ipol + npol)
   call mcvdep(mdl, ipol + 1, CMPMDL, .false., ier)
   write(depunt, err = 910, iostat = ios) &
&        mdl%lhsnum(i), nsimv, isimv(1:min(nsimv,MAXRHS)), &
&                     nv,    iv(1:min(nv,MAXRHS)), &
&                     nparv, iparv(1:min(nparv,MAXRHS))
   do j = 1, nv
       maxlag(iv(j)) = max(maxlag(iv(j)), -iminld(j))
       mxlead(iv(j)) = max(mxlead(iv(j)),  imaxld(j))
   end do


!        nsimv: number of simultaneous variables for normal solve
   maxarc = maxarc + nsimv
   maxref = maxref + nv + nparv
   if (ier.ne. 0 ) exit

enddo

goto 920

895 ier = 2
goto 920

910 ier = 1

920 continue

deallocate(mdl%nrecuf, mdl%equat)

return
end subroutine mc_analyse_model

!-----------------------------------------------------------------------

subroutine init_dep(mdl)
    type(model), intent(in) :: mdl

    integer :: i

    call clear_edep

    allocate(equno(mdl%nrv), stat = dep_alloc_stat)
    if (dep_alloc_stat == 0) allocate(edptr(mdl%neq + 1), stat = dep_alloc_stat)
    if (dep_alloc_stat == 0) allocate(eqdep(maxarc), stat = dep_alloc_stat)
    if (dep_alloc_stat > 0) return

    equno = 0
    do i = 1, mdl%neq
        equno(mdl%lhsnum(i)) = i
    end do

end subroutine init_dep

!-----------------------------------------------------------------------

subroutine clear_edep
    integer :: stat
    deallocate(equno, edptr, eqdep, stat = stat)
end subroutine clear_edep

!-----------------------------------------------------------------------

end module mccedp
