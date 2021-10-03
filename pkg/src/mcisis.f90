subroutine mcisis(modelnaml,modelnams, &
&                 mifnaml, mifnams, &
&                 idofbrd, igenfbo, ifbomif, iprifbi, iprisjc, &
&                 mrfopt, fbcopt, igen_dep_file, mcstat)
use mcvars
use mccedp
use mdordr
use mcpars
use mcxref
use iso_c_binding, only : C_NULL_CHAR

interface
    function mcip(mfname, strict, gen_dep_file) bind(c)
        use iso_c_binding, only : c_int, c_char
        integer(c_int) :: mcip
        character(kind = c_char), dimension(*), intent(in) :: mfname
        integer(c_int), intent(in) :: strict, gen_dep_file
    end function mcip
end interface

!     Isis model compiler
!      input is mdl file
!      converted to MIF file
!      cross reference info written to MRF file
!      errors written to ERR file

!     Arguments

!         In    modelnaml   Integer     length of modelname (bytes)
!         In    modelnams   Integer(*)  modelname
!         In    idofbrd     Integer     1 for    remove redundant fb vars
!                                       0 to NOT remove redundant fb vars

!         In    igenfbo     Integer     1 for generate feedback ordering
!                                       0 to NOT generate fb ordering

!         In    ifbomif     Integer     1  for write feedback ordering on mif
!                                       0 to  NOT write feedback ordering on mif

!         In    iprifbi     Integer     0 to print in crossreference info
!                                             on how each fb var was chosen
!                                       1 to not print this info

!         In    iprisjc     Integer     0 to print rhs symbolic jacobian
!                                            + statistics
!                                       1 to not print

!         In    mrfopt      Integer(2)  options/flags for cross reference

!         In    fbcopt      Integer(2)  options for printing feedback cycle
!                                       passed to mcxref

!         In    igen_dep_file Integer    0 to generate a dependency file
!                                        1 to not generate a dependency file
!         Out   mcstat      Integer     status flag
!                                         0 all ok
!                                         1 model file does not exist
!                                         2 error in model compilation
!                                         3 other errors
!                                         (model ordering, writing mif
!                                          or mrf)
!                                         4 not enough memory to compile
!                                           the model

!     adapted from Lahey source of tomci (Don/E)
!     many changes
!     more restrictive in syntax (no $ as replacement for ;)
!                                (no redundant , )
!     different error handling
!     different intermediate code and output files

integer, intent(out) ::  mcstat
integer, intent(in) ::  modelnaml, modelnams(*)
integer, intent(in) ::  mifnaml, mifnams(*)
integer, intent(in) ::  idofbrd, igenfbo, ifbomif, iprifbi, iprisjc,  &
                                   igen_dep_file
integer, intent(in) ::  mrfopt(*), fbcopt(*)


logical ::  dofbrd, genfbo, fbomif, prifbi, prisjc, gen_dep_file

!     for copy of fbcopt

integer ::  fbcpar(2)

! parameters that determine dimensions

!   maxnuf = maximum no of functions (6 standard + rest user defined)
!   maxnre = maximum no of equations in tsp input
!   maxnrv = maximum no of variables in tsp input
!   maxnrp = maximum no of names of parameters + constants in tsp input
!   maxnrc = ditto, counting numbers rather than names

!   maxnfb = maximum no of feedback variables in ordering


!     Dynamic arrays for Pass 2 of model compiler

logical ::  davail
external davail

!     name of model file with path
character*(MAXFLEN + 1) pathnm

integer ::   ios
integer ::   ier,errpar,mcrcod,orderr

integer ::  istrict, i_gen_dep_file

!*IF TIMER
!     for time measurements
!
!      real told,tnew
!*ENDIF
integer ::  remove_xrffile, remove_miffile, xrf_err, mif_err
integer ::  fbor_err

! convert integers (passed from C) to logicals
dofbrd = idofbrd > 0
genfbo = igenfbo > 0
fbomif = ifbomif > 0
prifbi = iprifbi > 0
prisjc = iprisjc > 0
gen_dep_file = igen_dep_file > 0

mcstat = 0
faterr = .false.
nerror = 0
filerr = 0
filios = 0
mcrcod = 0
ier    = 0
mc_alloc_stat = 0

call mcimsg(1, 0)

call mcifig

! initialise model compiler, determine pathname modelfile
call mcfileadmin(modelnaml, modelnams, pathnm)

! generate names of the mif and mrf file
call byasf7(mifnams, 1, mifnaml, mifnam)
call mkfnam(xrfnam,xrfext)

!  delete the mif and mrf file if they already exist
mif_err = remove_miffile()
xrf_err = remove_xrffile()

if (mif_err /= 0 .or. xrf_err /= 0) then
    mcstat = 3
    goto 2000
endif

! open files

! unit parunt first for parameter list,
! later for model structure (ordering information)
open(parunt,status='scratch',form='unformatted',iostat=ios)
if(ios.ne.0) goto 980

! unit polunt for equation code
open(polunt,form='UNFORMATTED',status='scratch',iostat=ios)
if(ios.ne.0) goto 980

! unit ufnunt for function code
open(ufnunt,form='UNFORMATTED',status='scratch',iostat=ios)
if(ios.ne.0) goto 980

!     initialise variables for name administration
mdl%ivlast = 0
mdl%iplast = 0
mdl%ielast = 0
mdl%iflast = 0
mdl%iulflast = 0
mdl%neq = 0
nre = 0
mdl%nrv = 0
mdl%nrp = 0
mdl%nrc = 0
mdl%nuf = 0
mdl%nulf = 0
mdl%ufblen = 0
mdl%eqblen = 0
mdl%fboflg = 0
mdl%fbomem = 0

if (genfbo) then
    if (fbomif) then
        mdl%fboflg = 1
    else
        mdl%fboflg = 2
    endif
endif

!*IF TIMER
!      call xtime(told)
!*ENDIF


! call the xpc model compiler

istrict = 1
if (gen_dep_file) then
   i_gen_dep_file = 1
else
   i_gen_dep_file = 0
endif

mcstat = mcip(trim(pathnm) // C_NULL_CHAR, istrict, i_gen_dep_file)

call mcimsg(3, 0)

!*IF TIMER
!      call xtime(tnew)
!      call mctimer('Model compilation             ', told, tnew)
!*ENDIF
if (mcstat /= 0) goto 2000
if (mdl_alloc_stat > 0 .or. mc_alloc_stat > 0) then
    mcstat = 4
    goto 2000
endif

mdl%neq = nre

!   sort names
!*IF TIMER
!      call xtime(told)
!*ENDIF
call sort_names(mdl)

!*IF TIMER
!      call xtime(tnew)
!      call mctimer('Sorting names                 ', told, tnew)
!*ENDIF

!     read coefficients from file
call read_coef

call mcimsg(8, int(mdl%neq, ISIS_IKIND))

if (mdl%neq .le. 0) goto 850

call mcimsg(4, 0)

! set unused bytes in string memory to zero
call mcszer(mdl%enames, mdl%ielast, size(mdl%enames))
call mcszer(mdl%vnames, mdl%ivlast, size(mdl%vnames))
call mcszer(mdl%pnames, mdl%iplast, size(mdl%pnames))
call mcszer(mdl%fnames, mdl%iflast, size(mdl%fnames))
if (mdl%nulf > 0) call mcszer(mdl%ulfnames, mdl%iulflast, size(mdl%ulfnames))

!*IF TIMER
!      call xtime(told)
!*ENDIF

!     analyse dependency structure and write dependencies to scratch
!     file depunt

call mc_analyse_model(ier)

!*IF TIMER
!      call xtime(tnew)
!      call mctimer('Analysing dependencies        ', told, tnew)
!*ENDIF
if (ier /= 0) then
    if (ier == 10) then
        mcstat = 4
    else
        call mcdeperr(ier)
        mcstat = 3
    endif
    goto 2000
endif


! generate dependency data structure for ordering

!*IF TIMER
!      call xtime(told)
!*ENDIF
call mcrddp(mdl, orderr, errpar)
!*IF TIMER
!      call xtime(tnew)
!      call mctimer('mcrddp                        ',told,tnew)
!*ENDIF
if (orderr .ne. 0) then
    if (orderr == 1) then
        mcstat = 4
        goto 2000
    else
        goto 975
    endif
endif

!*IF TIMER
!      call xtime(told)
!*ENDIF
call order_mdl(mdl, dofbrd = dofbrd, errflg = orderr, fbor_err = fbor_err)
!*IF TIMER
!      call xtime(tnew)
!      call mctimer('Total time ordering model            ',told,tnew)
!*ENDIF
if (orderr .ne. 0 ) then
    if (orderr == 20) then
        mcstat = 4
        goto 2000
    else
        goto 975
    endif
endif
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

call mcimsg(5, 0)

call datim(mdl%time, 1)
call datim(mdl%date, 2)

!*IF TIMER
!      call xtime(told)
!*ENDIF
call check_variables
call mcwmif(mdl, mifnam, ier)
!*IF TIMER
!      call xtime(tnew)
!      call mctimer('Mcwmif                        ',told,tnew)
!*ENDIF
if (ier .ne. 0 ) then
    if (ier == 5) then
        errstr = 'Cannot open MIF file for write'
    elseif (ier == 6) then
        errstr = 'Write error on MIF file'
    elseif (ier == 7) then
        errstr = 'Read error on scratch file'
    else
        errstr = 'Unknown error while writing the mif file.'
    endif
    goto 976
endif

!     write cross-reference file
call mcimsg(6, 0)


!  TEMPORARY
!      turn print feedback chain on
!      set max print length to   something reasonable

if( mdl%fbomem .gt. 0 ) then
    fbcpar(1) = fbcopt(1)
    fbcpar(2) = fbcopt(2)
else
    fbcpar(1) = 0
    fbcpar(2) = 0
endif

!     release memory not needed for writing the cross reference

deallocate(mdl%indexe, mdl%coef)
!     if fbcpar(1) .eq. 0, the cross reference does not need full information
!     on feedback ordering.
!     for very large models and a long cross reference there may not
!     be enough memory..
!     if there is a feedback ordering remember only the pointers (length)
if (mdl%fboflg > 0 .and. fbcpar(1) .eq. 0 ) then
    if (allocated(mdl%fbordr)) deallocate(mdl%fbordr)
endif

!*IF TIMER
!      call xtime(told)
!*ENDIF
call write_xref(ier, mrfopt, fbcpar, prifbi, prisjc)
!*IF TIMER
!      call xtime(tnew)
!      call mctimer('Mcxref                        ',told,tnew)
!*ENDIF

if( ier .ne. 0 ) then
!        an error occurred in mcxref
   call mcxerr
   call mcgerr(4)
   mcstat = 3
endif

call mcimsg(7, 0)


!     FINISHED
goto 2000

850 continue
!     no equations
mcrcod = 1
goto 999

975 continue
!     error in ordering model
mcrcod = 101
goto 999

976 continue
!     errors in mcwmif
call mcxerr
mcrcod = 3
goto 999

980 continue
filerr = 2
filios = ios
goto 999

!     FALL THROUGH

999 continue
if( filerr .ne. 0 ) then
   call mcferr
elseif( mcrcod .gt. 100 ) then
   call mcoerr(orderr)
else
   call mcgerr(mcrcod)
endif
mcstat = 3

2000 continue

close(parunt,iostat=ios)
close(polunt,iostat=ios)
close(ufnunt,iostat=ios)

call deallocate_model(mdl)
call clear_edep
call clear_mcvars
return
end

!-----------------------------------------------------------------------

subroutine mcifig
use mcvars

!     default configuration

!     currently length of line in mrf file only

lenxrf = 128

return
end

!-----------------------------------------------------------------------

subroutine mcfileadmin(modelnaml, modelnams, pathnm)
use mcvars
use filenameutils
integer, intent(in) :: modelnaml, modelnams(*)
character(len = *), intent(out) :: pathnm

!     performs filename administration for the model compiler

!     errors are recorded in global
!       faterr
!       filerr/filios

character(len = MAXFLEN) :: basename, tmpstr
character(len = 1)       :: dirsep

call byasf7(modelnams, 1, modelnaml, pathnm)
if (len_trim(pathnm) == 0) goto 900

call split_path(pathnm, mdlpath, basename, dirsep)

! mdlpath holds directory path of modelfile (including trailing \ or /)
mdlpath = mdlpath(:len_trim(mdlpath)) // dirsep

call remove_extension(basename, tmpstr)
mdl%cnamelen = len_trim(tmpstr)
mdl%cname = tmpstr(:mdl%cnamelen)
if (mdl%cnamelen > len(mdl%cname)) goto 920

return

900 continue
filerr = 40
goto 999

920 continue
filerr = 7

999 faterr = .true.

return
end

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
end

!-----------------------------------------------------------------------

subroutine mcoerr(errcod)

!     ordering error message

!     In    errcod   Integer    type error

integer, intent(in) ::  errcod
character(len = 80) :: errmsg

select case (errcod)
case (1)
    errmsg = 'No diagonal match'
case (2)
    errmsg = 'No ordering found'
case (3)
    errmsg = 'Diagonal element not found'
case (4)
    errmsg = 'Equations still unordered'
case (5)
    errmsg = 'cannot mdl%order feedbacks'
case (6)
    errmsg = 'duplicate lhs var'
case (7)
    errmsg = 'too many arcs in model'
case (8)
    errmsg = 'Error reading scratch file'
case (9)
    errmsg = 'End of file on scratch file'
case (10)
    errmsg = 'prepos not finished'
case (11)
    errmsg = 'too many feedbacks'
case (12)
    errmsg = 'non zero element in single'
case (13)
    errmsg = 'cannot choose feedback'
case default
    errmsg = 'Unknown error'
end select

return
end subroutine mcoerr

!-----------------------------------------------------------------------

subroutine mcdeperr(errcod)
use msufstack
use mcdep

integer ::  errcod

!           error message in mcodep

!     In    errcod   Integer    type error


  character(len = 50) :: errmsg

  select case (errcod)
  case (1)
      write(errmsg, '(a, i5, a)') &
&      'Too many variables in equation (max =', MAXRHS, ')'
  case (2)
      errmsg = 'Internal error: problem in equation code'
  case (3)
      errmsg = 'Internal error: invalid operator code'
  case (4)
      errmsg = 'Equation too long: ran out of stack'
  case (5)
      write(errmsg, '(a, i3, a)') 'Too many nested user functions (max =', &
&        MAX_USERF_STACK, ')'
  case default
      errmsg = 'Unknown error in mcodep'

  end select

  !call fmtlog('B5*' // errmsg(:len_trim(errmsg)) // '*EX')

  return

end subroutine mcdeperr

!-----------------------------------------------------------------------

subroutine mcgerr( errcod)
    use output_utils

    ! general error message for a variety of errors

    !  In    errcod   Integer    type error

    integer, intent(in) ::  errcod
    character(len = 50) :: errmsg

    select case(errcod)
    case (1)
        errmsg = 'Model has no equations'
    case (2)
        errmsg = 'Error ordering model'
    case (3)
        errmsg = 'Problem writing MIF file'
    case (4)
        errmsg = 'Problem generating cross reference'
    case default
        errmsg = 'Unknown error in mcisis'
    end select
    
    call isismdl_error(errmsg)

    return
end subroutine mcgerr

!-----------------------------------------------------------------------

subroutine mcferr
use mcvars
use output_utils

! error message for a file (read/write) error

character(len = 80) :: errmsg

if( filerr .eq. 1 ) then
    errmsg = 'Cannot open error file'

elseif( filerr .eq. 2 ) then
    errmsg = 'Cannot open scratch file'

elseif( filerr .eq. 3 ) then
    errmsg = 'Cannot open MDL file'

elseif( filerr .eq. 4 ) then
    errmsg = 'Cannot open MIF file for write'

elseif( filerr .eq. 6 ) then
    errmsg = 'Cannot open MRF file for write'

elseif( filerr .eq. 7) then
              write(errmsg,'(A21, I3, A11)') &
& 'Model name longer than ', len(mdl%cname), ' characters'

elseif( filerr .eq. 10) then
    errmsg = 'Write error on ERR file'

elseif( filerr .eq. 12) then
    errmsg = 'Write error on MIF file'

elseif( filerr .eq. 13) then
    errmsg = 'Write error on MRF file'

elseif( filerr .eq. 14) then
    errmsg = 'Write error on scratch file'

elseif( filerr .eq. 20) then
    errmsg = 'Read error on MDL file'

elseif( filerr .eq. 21) then
    errmsg = 'Read error on scratch file'

elseif( filerr .eq. 30) then
    errmsg = 'Unexpected eof on MDL file'

elseif( filerr .eq. 40) then
    errmsg = '(Internal) empty model name'

else
    errmsg = '(Internal) unknown file error'

endif

call isismdl_warn(errmsg)

return
end

!-----------------------------------------------------------------------

subroutine mcxerr
use mcvars
use output_utils

    ! error message during scanning/compiling model
    ! message is in common variable errstr
    ! write is now written to the Isis log file and to the .err file
    ! errtyp == 1 implies a continuation message

    call isismdl_warn(errstr)

    return
end subroutine mcxerr

!-----------------------------------------------------------------------

subroutine mcimsg(msgnum, mpar)
use output_utils
integer, intent(in) ::  msgnum, mpar

! print progress .. messages

character(len = 80) :: str

select case (msgnum)

case (1)
    str = 'Isis Model Compiler 3.00'

case (2) 
    str = 'Scanning model ...'

case (3) 
    str = 'Compiling model ...'

case (4)
    str = 'Ordering equations ...'

case(5)
    str = 'Writing MIF file ...'

case (6)
    str = 'Writing cross-reference file ...'

case (7)
    str = 'End compilation'

case (8) 
    str = '         equations processed'
    write(str(4:8),'(i5)' ) mpar

case (9) 
    str = 'Checking redundant feedback variables'

case (10)
    str = '         redundant feedback variables detected'
    write(str(4:8),'(i5)' ) mpar

case (11)
    if (mpar == 0) then
        str = 'Removing redundant feedback variables'
    else
        str = 'Not removing redundant feedback variables'
    endif

case (12)
    str = 'Generating feedback variable ordering'

case (13)
    str = 'Feedback jacobian too dense (< 10% step reduction)'

case (14)
    str = 'Out of memory for feedback ordering'

case (15)
    str = 'NO feedback ordering generated and installed'

case (16)
    str = '         feedback variables added'
    write(str(3:8),'(i6)' ) mpar

case (17)
    str = 'Redundancies resolved in    passes'
    write(str(26:27),'(i2)' ) mpar

case (18)
    str = 'More than 5 passes for redundant feedbacks'

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

    integer(kind = MC_IKIND) :: names(*), lastp, kmax

    integer ::  end

    ! last used byte
    end = lastp / 64

    call byszer(names, end+1, kmax*MCNYI4 - (end+1) + 1)

    return
end subroutine mcszer

!--------------------------------------------------------------------------

subroutine read_coef
  use mcvars
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


!******************************************************************************
!      The following subroutines are called by the xpc model compiler
!******************************************************************************

integer function save_equation(np, pol)
    use mcvars
    use mcpars
    use invpol
    integer(kind = MC_IKIND), intent(in) :: np
    integer(kind = MC_IKIND), dimension(*), intent(in) :: pol
    ! Write polish code of an equation to scratch file polunt.
    ! The function returns 0 if no errors are detected.
    ! It returns maxpol when the lenght of the polish code (np)
    ! is longer than maxpol (in other subroutines the polish
    ! code has to fit in array polish).

    if (np > maxpol) then
        save_equation = maxpol
        return
    endif

    write(polunt) np, pol(:np)
    mdl%eqblen = mdl%eqblen + np

#ifdef DEBUG_COMPILER
    print *,'polish code equation written to polunt ', pol(:np)
#endif

    save_equation = 0
    return

end function save_equation

! ---------------------------------------------------------------------

integer function save_userfunction(np, pol)
   use mcvars
   use invpol
   use mcpars
   integer(kind = MC_IKIND), intent(in) :: np
   integer(kind = MC_IKIND), dimension(*), intent(in) :: pol(*)

   ! Write polish code of a user function to scratch file ufnunt.
   ! The function returns 0 if no errors are detected.
   ! It returns maxpol when the length of the polish code (np)
   ! is longer than maxpol (in other subroutines the polish
   ! code has to fit in array polish).

   if (np > maxpol) then
       save_userfunction = maxpol
       return
   endif

   write(ufnunt) np, pol(:np)
   mdl%ufblen = mdl%ufblen + np

#ifdef DEBUG_COMPILER
   print *,'polish code userfunction written to polunt ',  pol(:np)
#endif

   save_userfunction = 0
   return

end function save_userfunction

!--------------------------------------------------------------------------

subroutine save_parameter(n, values)
    use mcpars

!     write parameter values to a scratch file parunt

integer ::  n
   real(kind = 8) values(n)

integer ::  ios

#ifdef DEBUG_COMPILER
    print *,'writing parameter values ', values(:n)
#endif
    write(parunt, iostat = ios) n, values(:n)

end subroutine save_parameter

!------------------------------------------------------------------------

subroutine mc_alloc(eqCount, varCount, parCount, coefCount, &
&                   funCount, ulFunCount, eqCharCount, &
&                   varCharCount, parCharCount, &
&                   funCharCount, ulFunCharCount, caCount, leadVarCount, stat)
   use mcvars

   ! subroutine mc_alloc is called by the xpc model compiler to allocate
   ! the arrays for the name registration

   integer(kind = MC_IKIND), intent(in) :: eqCount, varCount, &
&       parCount, coefCount, funCount, ulFunCount, &
&       eqCharCount, varCharCount, parCharCount, funCharCount, &
&       ulFunCharCount, caCount, leadVarCount
   integer(kind = MC_IKIND), intent(out) :: stat

#ifdef DEBUG_COMPILER
         print *
         print *,'mc_alloc, eqCount = ', eqCount
         print *,'mc_alloc, eqoCharCount = ', eqCharCount
         print *,'mc_alloc, varCount = ', varCount
         print *,'mc_alloc, varCharCount = ', varCharCount
         print *,'mc_alloc, parCount = ', parCount
         print *,'mc_alloc, parCharCount = ', parCharCount
         print *,'mc_alloc, coefCount = ', coefCount
         print *,'mc_alloc, funCount = ', funCount
         print *,'mc_alloc, ulFunCount = ', ulFunCount
         print *,'mc_alloc, funCharCount = ', funCharCount
         print *,'mc_alloc, ulFunCharCount = ', ulFunCharCount
         print *,'mc_alloc, caCount = ', caCount
         print *,'mc_alloc, leadVarCount = ', leadVarCount
         print *
#endif

   call allocate_model(mdl, eqCount, varCount, parCount, &
&                      coefCount, funCount, ulFunCount, &
&                      eqCharCount, varCharCount, parCharCount, &
&                      funCharCount, ulFunCharCount, caCount, leadVarCount)
   stat = mdl_alloc_stat
   if (mdl_alloc_stat ==  0) then
       allocate(vtype((varCount - 1) / MCNYI4 + 1), stat = mc_alloc_stat)
       if (mc_alloc_stat == 0) then
           allocate(maxlag(varCount), stat = mc_alloc_stat)
       endif
       if (mc_alloc_stat == 0) then
           allocate(mxlead(varCount), stat = mc_alloc_stat)
       endif
   endif

end subroutine mc_alloc

!------------------------------------------------------------------------------

subroutine add_eqname(k, nmlen, name, lhs_num, type)
   use mcvars
   use kinds
   use mdl_name_utils
   integer(kind = MC_IKIND) :: k, nmlen, name(*), lhs_num
   character type

   integer :: i

#ifdef DEBUG_COMPILER
   character(len = 100) :: eqnam

   call byasf7(name, 1, nmlen, eqnam)
   print *,'add_eqname: k, type, lhs_num, nmlen, naam' , &
                k, type, lhs_num, nmlen, eqnam(:nmlen)
#endif
   i = add_name(name, nmlen, k, mdl%enames, mdl%ienames, &
&               nre, size(mdl%enames), mdl%ielast)
   mdl%lhsnum(k) =  lhs_num
   call bysset(mdl%etype, int(k, ISIS_IKIND), ichar(type))

end subroutine add_eqname

! -----------------------------------------------------------

subroutine add_varname(k, nmlen, name, type)
    use mcvars
    use kinds
    use mdl_name_utils
    integer(kind = MC_IKIND) ::  k, nmlen, name(*)
    character type

   integer :: i

#ifdef DEBUG_COMPILER
         character(len = 100) :: varnam
         call byasf7(name, 1, nmlen, varnam)
         print *,'add_varname: k, type, nmlen, naam' ,  &
             k, type, nmlen, varnam(:nmlen)
#endif

   i = add_name(name, nmlen, k, mdl%vnames, mdl%ivnames, &
&               mdl%nrv, size(mdl%vnames), mdl%ivlast)
   call bysset(vtype, int(k, ISIS_IKIND), ichar(type))

end subroutine add_varname

! -----------------------------------------------------------

subroutine add_parname(k, nmlen, name)
   use mcvars
   use mdl_name_utils
   integer(kind = MC_IKIND) :: k, nmlen, name(*)
   integer :: i

#ifdef DEBUG_COMPILER
   print *,'add_parname: k, nmlen', k, nmlen, name(1)
#endif

   i = add_name(name, nmlen, k, mdl%pnames, mdl%ipnames, &
&               mdl%nrp, size(mdl%pnames), mdl%iplast)
end subroutine add_parname

! -----------------------------------------------------------

subroutine add_funcname(k, nmlen, name, argCount)
   use mcvars
   use mdl_name_utils
   integer(kind = MC_IKIND) ::  k, name(*), nmlen, argCount
   integer                  :: i

#ifdef DEBUG_COMPILER
   print *,'add_funname: k, argCount, nmlen', k, argCount, nmlen, name(1)
#endif

   i = add_name(name, nmlen, k, mdl%fnames, mdl%ifnames, &
&               mdl%nuf, size(mdl%fnames), mdl%iflast)
   mdl%narguf(k) = argCount

end subroutine add_funcname

! -----------------------------------------------------------

subroutine add_ulfuncname(k, nmlen, name)
   use mcvars
   use mdl_name_utils
   integer(kind = MC_IKIND) ::  k, name(*), nmlen
   integer                  :: i
   !
   ! register user language function name
   !

#ifdef DEBUG_COMPILER
   print *,'add_ulfunname: k, argCount, nmlen', k, argCount, &
                 nmlen, name(1)
#endif

   i = add_name(name, nmlen, k, mdl%ulfnames, mdl%iulfnames, &
&               mdl%nulf, size(mdl%ulfnames), mdl%iulflast)

end subroutine add_ulfuncname

! -----------------------------------------------------------

integer function remove_xrffile()
  use mcvars

  integer :: delete_file

  remove_xrffile = delete_file(xrfnam)

  if (remove_xrffile /= 0) then
      errstr = 'Cannot open mrf file for write'
      call mcxerr
      call mcgerr(4)
  endif

end function remove_xrffile

! -----------------------------------------------------------

integer function remove_miffile()
  use mcvars

  integer :: delete_file

  remove_miffile = delete_file(mifnam)

  if (remove_miffile /= 0) then
      errstr = 'Cannot open mif file for write'
      call mcxerr
      call mcgerr(3)
  endif

end function remove_miffile

! -----------------------------------------------------------

subroutine check_variables
  use mcvars

  integer :: k, i, nd2
  integer, external :: bysget

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

end subroutine check_variables

