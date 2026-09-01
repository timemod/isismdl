module mcisis_utils

 use mcvars

 interface
     function mcip(mfname, ppfname) bind(c)
         use iso_c_binding, only : c_int, c_char
         integer(c_int) :: mcip
         character(kind = c_char), dimension(*), intent(in) :: mfname, ppfname
     end function mcip
 
 end interface

contains

!  Performs filename administration for the model compiler

!  Return code: 
!   0 OK
!  40 Empty model name.
!   7 Model name too long

!  sets global error string, but does not produce error message
integer function mcfileadmin(modelnaml, modelnams, pathnm)
use filenameutils
integer, intent(in) :: modelnaml, modelnams(*)
character(len = *), intent(out) :: pathnm

character(len = MAXFLEN) :: basename, tmpstr
character(len = 1)       :: dirsep

mcfileadmin = 0

call byasf7(modelnams, 1, modelnaml, pathnm)
if (len_trim(pathnm) == 0) then
    mcfileadmin = 40
    errstr = "Empty model name"
    return
endif

call split_path(pathnm, mdlpath, basename, dirsep)

! mdlpath holds directory path of modelfile (including trailing \ or /)
mdlpath = mdlpath(:len_trim(mdlpath)) // dirsep

call remove_extension(basename, tmpstr)
mdl%cnamelen = len_trim(tmpstr)
mdl%cname = tmpstr(:mdl%cnamelen)
if (mdl%cnamelen > len(mdl%cname)) then
    mcfileadmin = 7
    write(errstr,'(A21, I3, A11)') &
             'Model name longer than ', len(mdl%cname), ' characters'
endif

return
end function mcfileadmin

!-----------------------------------------------------------------------

subroutine mkfnam(fnam, ext)
use mcvars
character*(*) fnam, ext

!     make a file name from
!           mdlpath, mdl%cname and the extension supplied in <ext>
!     return  in <fnam>

integer ::  cnmlen, mdplen

cnmlen = len_trim(mdl%cname)
if( mdlpath .eq. '' ) then
  fnam = mdl%cname(1:cnmlen) // ext
else
  mdplen = len_trim(mdlpath)
  fnam = mdlpath(1:mdplen) // mdl%cname(1:cnmlen) // ext
endif

return
end subroutine mkfnam

!-----------------------------------------------------------------------

subroutine mcimsg(msgnum, mpar)
use output_utils
integer, intent(in) ::  msgnum, mpar

! print progress .. messages

character(len = 80) :: str

select case (msgnum)

case (1)
    str = 'Isis Model Compiler 3.00'

case (3) 
    str = 'Compiling model ...'

case (4)
    str = 'Ordering equations ...'

case (6)
    str = 'Writing cross-reference file ...'

case (7)
    str = 'End compilation'

case (8) 
    str = '         equations processed'
    write(str(4:8),'(i5)' ) mpar

case (12)
    str = 'Generating feedback variable ordering'

case (13)
    str = 'Feedback jacobian too dense (< 10% step reduction)'

case (14)
    str = 'Out of memory for feedback ordering'

case (15)
    str = 'NO feedback ordering generated and installed'

case default
return

end select

call isismdl_out(str)

return
end subroutine mcimsg

!-----------------------------------------------------------------------

subroutine mcszer(names, lastp, kmax)
    use model_params

    ! set unused bytes to 0 in names

    integer(kind = MC_IKIND), intent(inout) :: names(*)
    integer(kind = MC_IKIND), intent(in) :: lastp, kmax

    integer :: end

    ! last used byte
    end = lastp / 64

    call byszer(names, end + 1, kmax * MCNYI4 - (end + 1) + 1)

    return
end subroutine mcszer

!--------------------------------------------------------------------------

subroutine read_coef
  use mcpars

!     read parameters from parunt and set mdl%nvalp and mdl%cfptr arrays
!     mdl%nvalp(i)  number of values for parameter i
!     mdl%cfptr(i)  start in mdl%coef for parameter i

integer ::  k, icoef, nrval

  rewind parunt

  icoef = 1
  do k =1, mdl%nrp
      mdl%cfptr(k) = icoef
      read(parunt) nrval,  mdl%coef(icoef : icoef + nrval - 1)
      mdl%nvalp(k) = nrval
      icoef = icoef + nrval
      mdl%nrc = mdl%nrc + nrval
  end do

end subroutine read_coef

! ------------------------------------------------------------

! Writes the compiled model to a mif file.
! Return code:
! 0    OK
! /= 0 Error, see comments in mcwmif.f90
! 
! sets global error string, but does not produce error message
integer function mc_write_mif() 

  integer :: k, i, nd2, ier
  integer, external :: bysget

  ! first use some lag/lead administration

  mdl%nd1    = 0
  nd2        = 0
  mdl%nendex = 0
  mdl%nca    = 0

  do k =1, mdl%nrv

      mdl%ibx1(k) = mdl%nd1 + 1
      mdl%nd1     = mdl%nd1 + maxlag(k)
      mdl%ibx2(k) = nd2 + 1
      nd2     = nd2 + mxlead(k)
      mdl%aci(k)  = 0

      if (char(bysget(vtype,k)) .eq. 'E') then
           ! exogenous variable
           mdl%lik(k) = .false.
      else
          ! endogenous variable
          mdl%lik(k) = .true.
          if (char(bysget(vtype,k)) .eq. 'B') then
              ! frml variable
              mdl%nca = mdl%nca + 1
              mdl%ica(mdl%nca) = k
              mdl%aci(k) = mdl%nca
          endif
      endif

      if (mxlead(k) > 0 .and. mdl%lik(k)) then
          mdl%nendex = mdl%nendex + 1
          mdl%iendex(mdl%nendex) = k
      endif

  end do

  mdl%ibx1(mdl%nrv+1) = mdl%nd1 + 1
  mdl%ibx2(mdl%nrv+1) = nd2 + 1

  do i = 1, mdl%nrv + 1
      mdl%ibx2(i) = mdl%nd1 + mdl%ibx2(i)
  end do
  mdl%nd = mdl%nd1 + nd2

  ! now write mig file

  call mcwmif(mdl, mifnam, ier)

  ! error message

  if (ier .ne. 0) then
      select case (ier)
          case (5)
              errstr = 'Cannot open MIF file for writing'
          case (6)
              errstr = 'Write error'
          case (7)
               errstr = 'Read error on scratch file'
          case default
               errstr = 'Unknown error'
       end select
  endif

  mc_write_mif = ier

end function mc_write_mif

! ------------------------------------------------------------

! install the dependency structure 
integer function mc_gen_dep()
  use mccedp

  integer :: ier, errpar

  ! analyse dependency structure and write dependencies to scratch
  ! file depunt

  call mcimsg(4, 0)

!*IF TIMER
!      call xtime(told)
!*ENDIF
  call mc_analyse_model(ier)
!*IF TIMER
!      call xtime(tnew)
!      call mctimer('Analysing dependencies        ', told, tnew)
!*ENDIF
  if (ier /= 0) then
     mc_gen_dep = ier
     return
  endif

! read dependency information from scratch file depunt

!*IF TIMER
!      call xtime(told)
!*ENDIF
  call mcrddp(mdl, ier, errpar)
!*IF TIMER
!      call xtime(tnew)
!      call mctimer('mcrddp                        ',told,tnew)
!*ENDIF
  if (ier /= 0) then
      errstr = '(Internal) Error reading the dependency structure from scratch file'
  endif
 
  mc_gen_dep = ier

  return

end function mc_gen_dep

! ------------------------------------------------------------

! Determines the model ordering 

! Return code:
! 0    OK
! /= 0 Error, see comments in mcwmif.f90

integer function mc_order_mdl(dofbrd, genfbo) 
  use mdordr
  logical, intent(in) :: dofbrd, genfbo

  integer:: orderr, errpar, fbor_err

!*IF TIMER
!      call xtime(told)
!*ENDIF
      call order_mdl(mdl, dofbrd = dofbrd, errflg = orderr, fbor_err = fbor_err)
!*IF TIMER
!      call xtime(tnew)
!      call mctimer('Total time ordering model            ',told,tnew)
!*ENDIF
  if (orderr /= 0) goto 100

  if (genfbo .and. mdl%nfb > 1) then
    call mcimsg(12,0)
  endif

  if (genfbo .and. mdl%nfb > 1 .and. fbor_err /= 0) then
    if (fbor_err == 2) then
        call mcimsg(13, 0)
    else if (fbor_err == 3) then
        call mcimsg(14, mdl%fbomem)
    endif
    call mcimsg(15,0)
  endif

100 continue

  mc_order_mdl = orderr

  return

end function mc_order_mdl

! Writes the current time or date to string c.
! String c should contain at least 10 characters.
! Input parameter i determines if the time (i = 1)
! or date (i = 2) is written to c.
subroutine datim(c,i)
character*(*) c
integer ::  i

integer ::  values(8)

call date_and_time(VALUES = values)

if (i.eq.1) then
     write(c,'(I2.2,A1,I2.2,A1,I2.2)') values(5),':', values(6),':', values(7)
else
     write(c,'(I4.4,A1,I2.2,A1,I2.2)') values(1),'-', values(2),'-', values(3)
endif

return
end subroutine datim

end module mcisis_utils
