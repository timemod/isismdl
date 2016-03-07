subroutine read_mif_file(mdl, modelnmlen, modelnm, ier, fstat)
use model_type
use mif_file
use invpol

type(model), intent(inout)        :: mdl
integer, intent(in)               :: modelnmlen
integer, dimension(*), intent(in) :: modelnm
integer, intent(out)     :: ier, fstat

!     error status in ier and fstat
!       0  all ok
!       1  can't open mif file
!       2  read error mif file (iostat in fstat)
!       3  invalid identification string on file
!       4  file is corrupted
!       10 not enough memory for reading the model

integer, intrinsic :: max
integer(kind = MC_IKIND) :: maxeqt, fbosiz, stat, idum

integer(kind = MC_IKIND) ::  eqCharCount, varCharCount, &
&               parCharCount, funCharCount, ulFunCharCount


  ! clear current model
  call deallocate_model(mdl)

  call rdmifh(mdl, ier, fstat, modelnmlen, modelnm, fbosiz)
  if (ier /= 0) return

  eqCharCount    = mdl%ielast / 64
  varCharCount   = mdl%ivlast / 64
  parCharCount   = mdl%iplast / 64
  funCharCount   = mdl%iflast / 64
  ulFunCharCount = mdl%iulflast / 64

  call allocate_model(mdl, mdl%neq, mdl%nrv, mdl%nrp, mdl%nrc, &
&                     mdl%nuf, mdl%nulf, eqCharCount, &
&                     varCharCount, parCharCount, funCharCount, &
&                     ulFunCharCount, mdl%nca, mdl%nendex)
  allocate(mdl%numfb(mdl%nfb), stat = stat)
  if (mdl_alloc_stat > 0 .or. stat > 0) then
      call deallocate_model(mdl)
      ier = 10
      return
  endif

  if (mdl%fboflg > 0) then
      ! fbosiz is the total size of mdl%fboptr (mdl%nfb + 1)
      ! and mdl%fbordr(mdl%fbomem)
      mdl%fbomem = max(fbosiz - mdl%nfb - 1, 0)
      if (mdl%fboflg == 1 .and. mdl%fbomem > 0) then
          ! feedback ordering on file
          allocate(mdl%fboptr(mdl%nfb + 1), stat = stat)
          if (stat == 0) then
              allocate(mdl%fbordr(mdl%fbomem), stat = stat)
          endif
          if (stat > 0) then
              call deallocate_model(mdl)
              ier = 10
              return
          endif
      endif
  endif

  ! mdl%ufblen total length of function code
  ! mdl%eqblen total length of equation code
  ! maxeqt (in /mpglob/) for memory allocation of code (mdl%equat array)

  if (mifvernum == 3) then
      ! There was a bug in mcisis.rf7 for Isis versions
      ! 22.0 - 22.2.1. ufblen was not calculated correctly,
      ! and always had the value zero. Consequently, for mif
      ! version 3 (this mif version was used by these Isis
      ! versions), we need special treatment. See also subroutine
      ! read_polish. For the time being we assume that mdl%ufblen
      ! was equal to maxpol
      mdl%ufblen = max(mdl%ufblen, maxpol)
  endif
  maxeqt = mdl%ufblen + mdl%eqblen
  allocate(mdl%nrecuf(mdl%nuf + 1), stat = stat)
  if (stat == 0) allocate(mdl%nrecp(mdl%neq + 1), stat = stat)
  if (stat == 0) allocate(mdl%equat(maxeqt), stat = stat)
  if (stat > 0) then
      call deallocate_model(mdl)
      ier = 10
      return
  endif

  call rdmifc(mdl, ier, fstat)

end subroutine read_mif_file

!-----------------------------------------------------------------------

subroutine rdmifh(mdl, ier, fstat, modelnmlen, modelnm, fbosiz)
use model_type
use mif_file

!     reads header of model code file
!     error status in ier and fstat
!       0  all ok
!       1  can't open mif file
!       2  read error mif file (iostat in fstat)
!       3  invalid identification string on file
!       4  file is corrupted

type(model), intent(inout)        :: mdl
integer ::  ier, fstat, modelnmlen, modelnm(*)

logical(kind = MC_LKIND) :: ldum1
integer(kind = MC_IKIND) :: idum1, ufargl, fbosiz

ier = 1

call byasf7(modelnm, 1, modelnmlen, pathnm)
open(MIFUNT, file = pathnm, status = 'OLD', access = 'STREAM', &
&    action = 'READ', err=894)

!   MIF version has the form MIFISIS.Xn where X = A..Z and num = 01..99.
!   The MIF file should have the same X as the current version defined
!   in mif_file.f90.

read(MIFUNT,err=895,end=895,iostat=fstat) fcode
if( fcode(:9) .ne. MIFVER(:9)) goto 896
read(fcode(10:11),*) mifvernum
if (mifvernum .ge. 2) then
  read(MIFUNT,err=895,end=895,iostat=fstat) mdl%cnamelen
  if (mdl%cnamelen .gt. MAXMOLEN) goto 898
  read(MIFUNT,err=895,end=895,iostat=fstat) &
&      mdl%cname(:mdl%cnamelen), mdl%date, mdl%time
else ! old mif version
  rewind(MIFUNT)
  read(MIFUNT,err=895,end=895,iostat=fstat) fcode,mdl%cname(:8), &
&           mdl%date, mdl%time
  mdl%cnamelen = len_trim(mdl%cname(:8))
endif

mdl%nulf = 0
if (mifvernum > 2) then
    read(MIFUNT,err=895,end=895,iostat=fstat) mdl%neq,mdl%loops, &
&          mdl%loope, ldum1,idum1,mdl%nuf,mdl%ielast, mdl%ufblen, &
&          mdl%eqblen,mdl%nrp,mdl%nrc,mdl%nfb,mdl%nrv,mdl%nca, &
&          mdl%nd,mdl%nd1,mdl%nendex,mdl%ivlast,mdl%iplast, mdl%iflast
    if (mifvernum > 3) read(MIFUNT, err = 895, end = 895, iostat = fstat) &
&                  mdl%nulf, mdl%iulflast
else
    read(MIFUNT,err=895,end=895,iostat=fstat) mdl%neq,mdl%loops, &
&          mdl%loope, ldum1, idum1,mdl%nuf,mdl%ielast, mdl%ufblen, &
&          mdl%eqblen, ufargl, &
&          mdl%nrp,mdl%nrc,mdl%nfb,mdl%nrv,mdl%nca,mdl%nd,mdl%nd1, &
&          mdl%nendex,mdl%ivlast,mdl%iplast
!          estimate mdl%iflast for ancient mif version
     mdl%iflast = mdl%nuf * OLD_MAX_FUNC_NAME * 64
endif
if (fstat /= 0) goto 895

!     read flag word
!     if 0 then no additional info on file
!     Feedback ordering flag is first 2 bits of extra
!         0  then no feedback ordering for this model
!         1  then feedback ordering info on file
!         2  then feedback ordering must be generated after reading
!            model

!     mdl%fbomem is length of feedback ordering array

read(MIFUNT,err=895,end=895,iostat=fstat) mdl%fboflg
if (mdl%fboflg > 0) then
!        feedback ordering on file or after reading model.
!        fbosiz is the total size of mdl%fboptr (mdl%nfb + 1)
!        and mdl%fbordr(mdl%fbomem).
   read(MIFUNT,err=895,end=895,iostat=fstat) fbosiz
endif

ier = 0
return

894 ier = 1
goto 900
895 ier = 2
goto 900
896 ier = 3
goto 900
897 ier = 4
goto 900
898 ier = 5
goto 900

900 close(MIFUNT,iostat=fstat)
return
end

!-----------------------------------------------------------------------

subroutine rdmifc(mdl, ier,fstat)
use model_type
use mif_file
use isischar_utils

!     reads index arrays from model code file
!     reads equation code from model code file

!     error status in ier
!       0    all ok
!       2    read  error on mif file (iostat in fstat)

type(model), intent(inout) :: mdl
integer ::  ier,fstat
integer ::    i, j
integer ::    ufargl,smax, idumar(2)
character(len = 6), allocatable :: userf(:)
integer(kind = MC_IKIND), allocatable :: dummyar(:)

ier = 0

!     read string memory and pointer arrays

smax = (mdl%ielast/64 - 1) / MCNYI4 + 1
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%enames(1:smax)
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%ienames(1:mdl%neq)
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%etype(1:(mdl%neq/MCNYI4+1))
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%lhsnum(1:mdl%neq)
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%order(1:mdl%neq)
if (mifvernum <= 2) then
    read(MIFUNT,err=895,end=895,iostat=fstat) mdl%nrecp(1:mdl%neq)
endif
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%indexe(1:mdl%neq)

smax = (mdl%ivlast/64 - 1) / MCNYI4 + 1
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%vnames(1:smax)

read(MIFUNT,err=895,end=895,iostat=fstat) mdl%numfb(1:mdl%nfb)
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%iendex(1:mdl%nendex)
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%ica(1:mdl%nca)

read(MIFUNT,err=895,end=895,iostat=fstat) mdl%aci(1:mdl%nrv)

read(MIFUNT,err=895,end=895,iostat=fstat) mdl%ivnames(1:mdl%nrv)
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%ibx1(1:mdl%nrv)
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%ibx2(1:mdl%nrv)
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%indexv(1:mdl%nrv)
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%lik(1:mdl%nrv)


if( mdl%nrp .gt. 0 ) then
   smax = (mdl%iplast/64 - 1) / MCNYI4 + 1
   read(MIFUNT,err=895,end=895,iostat=fstat) mdl%pnames(1:smax)
   read(MIFUNT,err=895,end=895,iostat=fstat) mdl%ipnames(1:mdl%nrp)
   read(MIFUNT,err=895,end=895,iostat=fstat) mdl%indexp(1:mdl%nrp)
   read(MIFUNT,err=895,end=895,iostat=fstat) mdl%nvalp(1:mdl%nrp)
   read(MIFUNT,err=895,end=895,iostat=fstat) mdl%cfptr(1:mdl%nrp)
   read(MIFUNT,err=895,end=895,iostat=fstat) mdl%coef(1:mdl%nrc)
endif

mdl%ibx1(mdl%nrv+1) = mdl%nd1 + 1
mdl%ibx2(mdl%nrv+1) = mdl%nd  + 1

if (mdl%nulf > 0) then
   ! read the names of user language functions
   smax = (mdl%iulflast/64 - 1) / MCNYI4 + 1
   read(MIFUNT,err=895,end=895,iostat=fstat) mdl%ulfnames(1:smax)
   read(MIFUNT,err=895,end=895,iostat=fstat) mdl%iulfnames(1:mdl%nulf)
endif

!     read user function info
if(mdl%nuf .gt. 0) then
   if (mifvernum > 2) then
       smax = (mdl%iflast/64 - 1) / MCNYI4 + 1
       read(MIFUNT,err=895,end=895,iostat=fstat) mdl%fnames(:smax)
       read(MIFUNT,err=895,end=895,iostat=fstat) mdl%ifnames(:mdl%nuf)
       read(MIFUNT,err=895,end=895,iostat=fstat) mdl%narguf(:mdl%nuf)
   else
       allocate(userf(mdl%nuf))
       read(MIFUNT,err=895,end=895,iostat=fstat) userf
       read(MIFUNT,err=895,end=895,iostat=fstat) mdl%narguf(:mdl%nuf)
       read(MIFUNT,err=895,end=895,iostat=fstat) mdl%nrecuf(:mdl%nuf)
!             llowrp, llowrv and lupper are not used any more
!             read(MIFUNT,err=895,end=895,iostat=fstat) llowrp(:ufargl)
!             read(MIFUNT,err=895,end=895,iostat=fstat) llowrv(:ufargl)
!             read(MIFUNT,err=895,end=895,iostat=fstat) lupper(:ufargl)
       ufargl = mdl%narguf(mdl%nuf)
       allocate(dummyar(ufargl))
       read(MIFUNT,err=895,end=895,iostat=fstat) dummyar(:ufargl)
       read(MIFUNT,err=895,end=895,iostat=fstat) dummyar(:ufargl)
       read(MIFUNT,err=895,end=895,iostat=fstat) dummyar(:ufargl)
       call store_function_names
       deallocate(userf, dummyar)

!            in old mif versions, mdl%narguf was cumulative number of
!            arguments for each user functions. Therefore, decumulate
!            mdl%narguf
       do i = mdl%nuf, 2, -1
           mdl%narguf(i) = mdl%narguf(i)  - mdl%narguf(i - 1)
       end do
   endif

endif

! calculate maximum variable lag and lead from ibx1 and ibx2
mdl%mxlag  = 0
mdl%mxlead = 0
do i = 1, mdl%nrv
    mdl%mxlag  = max(mdl%ibx1(i+1) - mdl%ibx1(i), mdl%mxlag )
    mdl%mxlead = max(mdl%ibx2(i+1) - mdl%ibx2(i), mdl%mxlead)
end do


!
! convert all names from internal to ASCII characters
!
call convert_names(mdl%neq, mdl%ienames, mdl%enames, mdl%indexe, .true.)
call convert_names(mdl%nrv, mdl%ivnames, mdl%vnames, mdl%indexv, .true.)
call convert_names(mdl%nrp, mdl%ipnames, mdl%pnames, mdl%indexp, .true.)
call convert_names(mdl%nuf, mdl%ifnames, mdl%fnames, idumar, .false.)

!     read function and equation code

if (mifvernum >= 3) then
    call read_polish
else
    call read_polish_mif_1_2
endif
if (ier /= 0) goto 900

!     read feedback ordering if available

if (mdl%fboflg == 1 .and. mdl%fbomem > 0) then
    read(MIFUNT,err=895,iostat=fstat) mdl%fboptr(1 : mdl%nfb + 1), &
&                                     mdl%fbordr(1 : mdl%fbomem)
endif

goto 900

895 ier = 1

900 close(MIFUNT)
return


contains


    subroutine read_polish

!   read polish code for the mif versions > 2

    integer(kind = MC_IKIND) :: ipol, npol

    ipol = 0

    !
    ! read polish code for the user functions
    !
    if (mifvernum == 4) then
        do i = 1, mdl%nuf
            mdl%nrecuf(i) = ipol
            read(MIFUNT, err=1001, end=1001, iostat = fstat) &
&               npol, mdl%equat(ipol + 1 : ipol + npol)
            ipol = ipol + npol
        end do
        mdl%nrecuf(mdl%nuf + 1) = ipol
    else if (mdl%nuf > 0) then
        call read_ufpol_mif_3
        if (ier /= 0) return
        ipol = mdl%ufblen
     endif

     do i = 1, mdl%neq
         mdl%nrecp(i) = ipol
         read(MIFUNT, err=1001, end=1001, iostat = fstat) &
&                npol, mdl%equat(ipol + 1 : ipol + npol)
         ipol = ipol + npol
     end do
     mdl%nrecp(mdl%neq + 1) = ipol

1001 continue
     if (fstat /= 0) ier = 2
     return
     end subroutine read_polish

     subroutine read_ufpol_mif_3
     use invpol

     ! Read polish code for user functions for mif version 3.
     ! The polish code is written in the same form as for mif
     ! version 4. However, for mif version 3 parameter ufblen
     ! (the total size of the polish code for user functions)
     ! is not known, because it was calculated incorrectly
     ! in mcisis.rf7 for Isis versions 22.0 - 22.2.1.

     integer(kind = MC_IKIND), dimension(maxpol) :: pol
     integer(kind = MC_IKIND) :: ipol, np, maxeqt, alloc_stat
     integer(kind = MC_IKIND), dimension(:), allocatable :: tmp

     ipol = 0

     maxeqt = size(mdl%equat)

     do i = 1, mdl%nuf
         mdl%nrecuf(i) = ipol
         read(MIFUNT, err=1001, end=1001, iostat = fstat) np, pol(:np)
         if (ipol + np > maxeqt) then
             maxeqt = maxeqt + 10 * maxpol
             allocate(tmp(maxeqt), stat = alloc_stat)
             if (alloc_stat /= 0) then
                 ier = 10
                 return
             endif
             tmp(1: ipol) = mdl%equat(1 : ipol)
             call move_alloc(tmp, mdl%equat)
         endif
         mdl%equat(ipol + 1 : ipol + np) = pol(:np)
         ipol = ipol + np
     end do
     mdl%nrecuf(mdl%nuf + 1) = ipol

     mdl%ufblen = ipol

     ! update the size of mdl%equat if necessary
     if (mdl%ufblen + mdl%eqblen /= maxeqt) then
         maxeqt = mdl%ufblen + mdl%eqblen
         allocate(tmp(maxeqt), stat = alloc_stat)
         if (alloc_stat /= 0) then
             ier = 10
             return
         endif
         tmp(1: mdl%ufblen) = mdl%equat(1 : mdl%ufblen)
         call move_alloc(tmp, mdl%equat)
     endif

1001 continue
     if (fstat /= 0) ier = 2
     return

     end subroutine read_ufpol_mif_3

!    read polish code for mif versions 1 and 2

     subroutine read_polish_mif_1_2

!    read polish code for mif versions 1 and 2
     integer ::  urec, erec, i

     urec = 0
     do i = 1, mdl%nuf
        urec = urec + mdl%nrecuf(i)
     end do

     mdl%nrecuf(mdl%nuf + 1) = urec
     do i = mdl%nuf, 1, -1
        mdl%nrecuf(i) = mdl%nrecuf(i+1) - mdl%nrecuf(i)
     end do

     erec = 0
     do i = 1, mdl%neq
        erec = erec + mdl%nrecp(i)
     end do

     mdl%nrecp(mdl%neq + 1) = erec + urec
     do i = mdl%neq, 1, -1
        mdl%nrecp(i) = mdl%nrecp(i+1) - mdl%nrecp(i)
     end do

!          read function and equation code

     do j = 1, mdl%nuf
         read(MIFUNT, err = 1002, end = 1002, iostat = fstat) &
&           mdl%equat((mdl%nrecuf(j) + 1) : mdl%nrecuf(j + 1))
     end do

     do j = 1, mdl%neq
         read(MIFUNT, err = 1002, end = 1002, iostat = fstat) &
&           mdl%equat((mdl%nrecp(j) + 1) : mdl%nrecp(j + 1))
     end do

1002 continue
     if (fstat /= 0) ier = 2
     return

     end subroutine read_polish_mif_1_2


!    store function names of the old model compiler in mdl%fnames

     subroutine store_function_names
     use mdl_name_utils
     use kinds
     integer(kind = MC_IKIND) :: k, cnt, length
     integer ::  i

     integer(kind = MC_IKIND), dimension((len(userf(1)) - 1) / MCNYI4 + 1) :: tmp

     mdl%iflast = 0
     cnt = 0
     do k = 1, mdl%nuf
        length = len_trim(userf(k))
        call byf7as(userf(k), tmp, 1, int(length, ISIS_IKIND))
        i = add_name(tmp, length, k, mdl%fnames, mdl%ifnames, cnt, &
&                    size(mdl%fnames), mdl%iflast)
     end do

     end subroutine store_function_names

end subroutine rdmifc
