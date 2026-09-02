subroutine mcisis(modelnaml,modelnams, &
&                 mifnaml, mifnams, ppfnaml, ppfnams, &
&                 idofbrd, igenfbo, iprifbi, iprisjc, &
&                 mrfopt, fbcopt)
use mcvars
use mcpars
use mcxref
use mccedp
use output_utils
use mcisis_utils
use mcordr
use file_utils
use iso_c_binding, only : C_NULL_CHAR

!     Isis model compiler
!      input is mdl file
!      converted to MIF file
!      cross reference info written to MRF file

!     Arguments

!         In    modelnaml   Integer     length of modelname (bytes)
!         In    modelnams   Integer(*)  modelname
!         In    mifnaml     Integer     length of the name of the mif file (bytes)
!         In    mifnams     Integer(*)  name of the mif file
!         In    ppfnaml     Integer     length of the name of the post processor file (bytes)
!         In    ppfnams     Integer(*)  name of the post processor file
!         In    idofbrd     Integer     1 for    remove redundant fb vars
!                                       0 to NOT remove redundant fb vars
!         In    igenfbo     Integer     1 for generate feedback ordering
!                                       0 to NOT generate fb ordering
!         In    iprifbi     Integer     0 to print in crossreference info
!                                             on how each fb var was chosen
!                                       1 to not print this info
!         In    iprisjc     Integer     0 to print rhs symbolic jacobian
!                                            + statistics
!                                       1 to not print

!         In    mrfopt      Integer(2)  options/flags for cross reference

!         In    fbcopt      Integer(2)  options for printing feedback cycle
!                                       passed to mcxref

integer, intent(in) ::  modelnaml, modelnams(*)
integer, intent(in) ::  mifnaml, mifnams(*)
integer, intent(in) ::  ppfnaml, ppfnams(*)
integer, intent(in) ::  idofbrd, igenfbo, iprifbi, iprisjc
integer, intent(in) ::  mrfopt(*), fbcopt(*)

logical :: dofbrd, genfbo, fbomif, prifbi, prisjc
integer :: mcstat, orderr = 0, errpar
integer :: fbcpar(2)

! parameters that determine dimensions

!   maxnuf = maximum no of functions (6 standard + rest user defined)
!   maxnre = maximum no of equations in tsp input
!   maxnrv = maximum no of variables in tsp input
!   maxnrp = maximum no of names of parameters + constants in tsp input
!   maxnrc = ditto, counting numbers rather than names

!   maxnfb = maximum no of feedback variables in ordering


! name of model file with path
character(len = MAXFLEN + 1) :: pathnm, ppfname

integer :: ios
integer :: ier
logical :: has_error

has_error = .false.

! convert integers (passed from C) to logicals
dofbrd = idofbrd > 0
genfbo = igenfbo > 0
prifbi = iprifbi > 0
prisjc = iprisjc > 0


call init_mcvars(genfbo)

call mcimsg(1, 0)

!
! ------------------------ file name admin ------------------------
!

! check length of filenames
if (modelnaml > MAXFLEN) then
    errstr = "Length of model filename too long"
    has_error = .true.
else if (mifnaml > MAXFLEN) then
    errstr = "Length of filename for mif file too long"
    has_error = .true.
else if (ppfnaml > MAXFLEN) then
    errstr = "Length of filename for preproc file too long"
    has_error = .true.
endif

if (has_error) goto 2000

! initialise model compiler, determine pathname modelfile
if (mcfileadmin(modelnaml, modelnams, pathnm) /= 0) then
   has_error = .true.
   goto 2000
endif

! generate names of the mif, mrf and ppf file
call byasf7(mifnams, 1, mifnaml, mifnam)
call mkfnam(xrfnam, xrfext)
call byasf7(ppfnams, 1, ppfnaml, ppfname)

!  delete the mif and mrf file if they already exist
if (delete_file(mifnam) /= 0) then
    has_error = .true.
    errstr = 'Cannot remove existing mif file'
    goto 2000
endif
if (delete_file(xrfnam) /= 0) then
    has_error = .true.
    errstr = 'Cannot remove existing mrf file'
    goto 2000
endif
if (delete_file(ppfname) /= 0) then
    has_error = .true.
    errstr = 'Cannot remove existing ppdf file'
    goto 2000
endif

!
! -------------- open scratch files ---------------
!

! unit parunt first for parameter list,
! later for model structure (ordering information)
open(parunt, status = 'scratch', form = 'unformatted', iostat = ios)
if (ios .ne. 0) then
    errstr = 'Internal - Cannot open scratch file'
    has_error = .true.
    goto 2000
endif

! unit polunt for equation code
open(polunt, form = 'UNFORMATTED', status = 'scratch', iostat = ios)
if (ios .ne. 0) then
    errstr = 'Internal - Cannot open scratch file'
    has_error = .true.
    goto 2000
endif

! unit ufnunt for function code
open(ufnunt, form = 'UNFORMATTED', status = 'scratch', iostat = ios)
if (ios .ne. 0) then
    errstr = 'Internal - Cannot open scratch file'
    has_error = .true.
    goto 2000
endif

!
! -------------------call the xpc model compiler -----------------------
!

! the compiled code is written to temporary files 
! (the scratch files opened before)

call mcimsg(3, 0)

mcstat = mcip(trim(pathnm) // C_NULL_CHAR, trim(ppfname) // C_NULL_CHAR)

if (mcstat /= 0) then
    select case (mcstat)
        case (1)
            errstr = 'The model file ' // trim(pathnm) // ' does not exist' // achar(10)
        case (2)
            errstr = 'Error detected in compilation of model ' // trim(pathnm) // &
                      achar(10) // 'Check the .err file'
        case (4)
             errstr = 'Not enough memory to compile the model ' // trim(pathnm)
        case default
             errstr = 'Error detected in compilation of model ' // trim(pathnm)
    end select
    has_error = .true.
    goto 2000
endif

mdl%neq = nre

! sort names
call sort_names(mdl)

! read coefficients from file
call read_coef

call mcimsg(8, int(mdl%neq, MC_IKIND))

if (mdl%neq .le. 0) then
    has_error = .true.
    errstr = 'Model has no equations'
    goto 2000
endif

! set unused bytes in string memory to zero
call mcszer(mdl%enames, mdl%ielast, size(mdl%enames))
call mcszer(mdl%vnames, mdl%ivlast, size(mdl%vnames))
call mcszer(mdl%pnames, mdl%iplast, size(mdl%pnames))
call mcszer(mdl%fnames, mdl%iflast, size(mdl%fnames))
if (mdl%nulf > 0) call mcszer(mdl%ulfnames, mdl%iulflast, size(mdl%ulfnames))

!
! ------------------- determine dependency structure ---------------------
!

ier = mc_gen_dep()
if (ier /= 0) then
    has_error = .true.
    goto 2000
endif

! 
! ---------------- Order the model --------------------------------------
!

orderr = mc_order_mdl(dofbrd, genfbo)
if (orderr .ne. 0) then
    has_error = .true.
    goto 2000
endif

!
! -----------------set compilation time and date in mdl ----------
!

call datim(mdl%time, 1)
call datim(mdl%date, 2)

!        
! ------------------- write mif file ---------------------------
!

ier = mc_write_mif()
if (ier /= 0) then
    errstr = 'Error writing the mif file (' // trim(errstr) // ")"
    has_error = .true.
    goto 2000
endif

!
! ----------------- write xrf file ------------------------
!

call mcimsg(6, 0)

! turn print feedback chain on
! set max print length to something reasonable

if (mdl%fbomem .gt. 0) then
    fbcpar(1) = fbcopt(1)
    fbcpar(2) = fbcopt(2)
else
    fbcpar(1) = 0
    fbcpar(2) = 0
endif

! release memory not needed for writing the cross reference

if (allocated(mdl%indexe)) deallocate(mdl%indexe)
if (allocated(mdl%coef)) deallocate(mdl%coef)

! if fbcpar(1) .eq. 0, the cross reference does not need full information
! on feedback ordering.
! for very large models and a long cross reference there may not
! be enough memory..
! if there is a feedback ordering remember only the pointers (length)
if (mdl%fboflg > 0 .and. fbcpar(1) .eq. 0 ) then
    if (allocated(mdl%fbordr)) deallocate(mdl%fbordr)
endif

call write_xref(ier, mrfopt, fbcpar, prifbi, prisjc)
if (ier .ne. 0) then
    ! an error occurred in mcxref, errstr contains error message
    errstr = 'Error generating cross reference (' // trim(errstr) // ")"
    has_error = .true.
    goto 2000
endif

!
! ------------------------ Finished -----------------------
!

call mcimsg(7, 0)


2000 continue

close(parunt, iostat = ios)
close(polunt, iostat = ios)
close(ufnunt, iostat = ios)

call deallocate_model(mdl)
call clear_edep
call clear_mcvars

if (has_error) then
    if (orderr /= 0) then
        call mcoerr(orderr)
    else 
       call isismdl_error(errstr)
    endif
endif

return
end subroutine mcisis

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
   integer, intent(out) :: stat

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
&                      funCharCount, ulFunCharCount, caCount, leadVarCount, stat)

   if (stat == 0) call allocate_mcvars(varCount, stat = stat)

   if (stat /= 0) call deallocate_model(mdl)

end subroutine mc_alloc

!------------------------------------------------------------------------------

subroutine add_eqname(k, nmlen, name, lhs_num, type_int)
   use mcvars
   use kinds
   use mdl_name_utils
   integer(kind = MC_IKIND), intent(in) :: k, nmlen, name(*), lhs_num, type_int

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
   call bysset(mdl%etype, int(k, ISIS_IKIND), type_int)

end subroutine add_eqname

! -----------------------------------------------------------

subroutine add_varname(k, nmlen, name, type_int)
    use mcvars
    use kinds
    use mdl_name_utils
    integer(kind = MC_IKIND), intent(in) ::  k, nmlen, name(*), type_int

   integer :: i

#ifdef DEBUG_COMPILER
         character(len = 100) :: varnam
         call byasf7(name, 1, nmlen, varnam)
         print *,'add_varname: k, type, nmlen, naam' ,  &
             k, type, nmlen, varnam(:nmlen)
#endif

   i = add_name(name, nmlen, k, mdl%vnames, mdl%ivnames, &
&               mdl%nrv, size(mdl%vnames), mdl%ivlast)
   call bysset(vtype, int(k, ISIS_IKIND), type_int)

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
   print *,'add_ulfunname: k, nmlen, name(1)', k, nmlen, name(1)
#endif

   i = add_name(name, nmlen, k, mdl%ulfnames, mdl%iulfnames, &
&               mdl%nulf, size(mdl%ulfnames), mdl%iulflast)

end subroutine add_ulfuncname
