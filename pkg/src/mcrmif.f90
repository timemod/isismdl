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
!       5  file is corrupt
!       10 not enough memory for reading the model

integer(kind = MC_IKIND) :: maxeqt, fbosiz, stat

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
integer(kind = MC_IKIND) :: idum1, fbosiz

ier = 1

call byasf7(modelnm, 1, modelnmlen, pathnm)
open(MIFUNT, file = pathnm, status = 'OLD', access = 'STREAM', &
&    action = 'READ', err=894)

!   MIF version has the form MIFISIS.Xn where X = A..Z and num = 01..99.
!   The MIF file should have the same X as the current version defined
!   in mif_file.f90.

read(MIFUNT,err=895,end=895,iostat=fstat) fcode
if (fcode(:9) /= MIFVER(:9)) goto 896
read(fcode(10:11),*) mifvernum
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%cnamelen
if (mdl%cnamelen .gt. MAXMOLEN) goto 898
read(MIFUNT,err=895,end=895,iostat=fstat) &
!&    mdl%cname(:mdl%cnamelen), mdl%date, mdl%time
&    mdl%cname(:mdl%cnamelen)

mdl%nulf = 0
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%neq,mdl%loops, &
&    mdl%loope, ldum1,idum1,mdl%nuf,mdl%ielast, mdl%ufblen, &
&    mdl%eqblen,mdl%nrp,mdl%nrc,mdl%nfb,mdl%nrv,mdl%nca, &
&    mdl%nd,mdl%nd1,mdl%nendex,mdl%ivlast,mdl%iplast, mdl%iflast
read(MIFUNT, err = 895, end = 895, iostat = fstat) &
&                  mdl%nulf, mdl%iulflast

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
898 ier = 5
goto 900

900 close(MIFUNT,iostat=fstat)
return
end

!-----------------------------------------------------------------------

subroutine rdmifc(mdl, ier,fstat)
use model_type
use mif_file

!     reads index arrays from model code file
!     reads equation code from model code file

!     error status in ier
!       0    all ok
!       2    read  error on mif file (iostat in fstat)

type(model), intent(inout) :: mdl
integer ::  ier,fstat
integer ::    i
integer ::    smax

ier = 0

!     read string memory and pointer arrays

smax = (mdl%ielast/64 - 1) / MCNYI4 + 1
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%enames(1:smax)
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%ienames(1:mdl%neq)
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%etype(1:(mdl%neq/MCNYI4+1))
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%lhsnum(1:mdl%neq)
read(MIFUNT,err=895,end=895,iostat=fstat) mdl%order(1:mdl%neq)
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
if (mdl%nuf >  0) then
    smax = (mdl%iflast/64 - 1) / MCNYI4 + 1
    read(MIFUNT,err=895,end=895,iostat=fstat) mdl%fnames(:smax)
    read(MIFUNT,err=895,end=895,iostat=fstat) mdl%ifnames(:mdl%nuf)
    read(MIFUNT,err=895,end=895,iostat=fstat) mdl%narguf(:mdl%nuf)
endif

! calculate maximum variable lag and lead from ibx1 and ibx2
mdl%mxlag  = 0
mdl%mxlead = 0
do i = 1, mdl%nrv
    mdl%mxlag  = max(mdl%ibx1(i+1) - mdl%ibx1(i), mdl%mxlag )
    mdl%mxlead = max(mdl%ibx2(i+1) - mdl%ibx2(i), mdl%mxlead)
end do

! read function and equation code
call read_polish

if (ier /= 0) goto 900
    ! read feedback ordering if available
    if (mdl%fboflg == 1 .and. mdl%fbomem > 0) then
        read(MIFUNT,err=895,iostat=fstat) mdl%fboptr(1 : mdl%nfb + 1), &
&                                         mdl%fbordr(1 : mdl%fbomem)
   
endif

goto 900

895 ier = 1

900 close(MIFUNT)
return

contains


    subroutine read_polish

        ! read polish code for the mif versions > 2

        integer(kind = MC_IKIND) :: ipol, npol

        ipol = 0

        !
        ! read polish code for the user functions
        !
        do i = 1, mdl%nuf
            mdl%nrecuf(i) = ipol
            read(MIFUNT, err=1001, end=1001, iostat = fstat) &
               npol, mdl%equat(ipol + 1 : ipol + npol)
            ipol = ipol + npol
        end do
        mdl%nrecuf(mdl%nuf + 1) = ipol

         do i = 1, mdl%neq
             mdl%nrecp(i) = ipol
             read(MIFUNT, err=1001, end=1001, iostat = fstat) &
                      npol, mdl%equat(ipol + 1 : ipol + npol)
             ipol = ipol + npol
         end do
         mdl%nrecp(mdl%neq + 1) = ipol

1001 continue
         if (fstat /= 0) ier = 2
         return
     end subroutine read_polish

end subroutine rdmifc
