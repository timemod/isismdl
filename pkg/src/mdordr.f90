   module mdordr

   contains

   subroutine order_mdl(mdl, fixcnt, fbfix, usinac, fixepi, &
&                       checkfbrd, dofbrd, fbrdcnt, errflg, fbor_err)

    !
    ! full ordering of the model:
    !    - ordering of the equations
    !    - optionally remove redundant feedback variables
    !    - optionally generate 'feedback ordering' for the
    !      efficient calculation of the Jacobian
    !
    use mdl_name_utils
    use mccedp
    use model_type
    use kinds
    use mcordr
    use mcirfb
    use mcfbor
    type(model), intent(inout) :: mdl
    integer, intent(in), optional  :: fixcnt
    integer, intent(in), optional  :: fbfix(*)
    logical, intent(in), optional  :: usinac, fixepi, checkfbrd, dofbrd
    integer, intent(out), optional :: fbrdcnt, errflg, fbor_err

!    in  Fixcnt : integer   * 0 for no fixed feedback variables
!                               *>0 for some
!                               *)
!
!      in  Fbfix  : integer(*)  fixed feedback variables
!                               *)
!
!      in  usinac : logical   (* true  if deactivated equations should
!                               *       be treated as exogenous
!                               * false if deactivated equations should
!                               *       be treated as endogenous
!                               *       i.e. ignore equation status
!                               *)

!      in fixepi  : logical   (* true if fixed feedback variables may be put in
!                               * in epilogue of ordering
!                               * false if a fixed feedback variable
!                               * must be regarded as feedback variable (see xpratx)
!                               *)

!      in checkfbrd : logical   (* true  to check for redundant fbvars
!                               * false to NOT check for redundant fbvars
!                               *)
!      in dofbrd : logical    (* true  to     remove redundant fbvars
!                               * false to NOT remove redundant fbvars
!                               *)
!
!      out   fbrdcnt: integer    (* # of redundant fbvars detected *)
!
!      out   errflg : integer    (*   0 for ok
!                                 * <>0 mcordr error code
!                                 * Warning: on error
!                                 *          variables concerning ordering
!                                 *          in model common are BAD
!                                 *
!                                 * Error message printed here
!                                 *)
!      out   fbor_err : integer    (*  error flag feedback ordering
!                                   0  o.k.
!                                   1  no feedback variables
!                                   2  feedback jacobian too
!                                      dense
!                                   3  not enough memory


integer :: ier, errval, nfbprev
integer :: namelen
integer, dimension(:), allocatable :: fbtypfix
character(len = 20) :: rname = "MDORDR"
!*IF TIMER
!      real :: told, tnew
!*ENDIF
logical :: checkfbrd_tmp
integer :: fbrdcnt_tmp, alloc_stat

!
! handle some optional arguments
!
if (present(checkfbrd)) then
    checkfbrd_tmp = checkfbrd
else
    checkfbrd_tmp = .true.
endif
if (present(fixcnt)) then
    allocate(fbtypfix(fixcnt), stat = alloc_stat)
    if (alloc_stat > 0) then
        ier = 20
        goto 999
    endif
    fbtypfix = 1
endif

!*IF TIMER
!    call xtime(told)
!*ENDIF
    call order_equations(ier, errval, mdl, usinac = usinac, &
&                        fixepi = fixepi, numfix = fixcnt, &
&                        fbfix = fbfix, fbtypfix = fbtypfix)
!*IF TIMER
!    call xtime(tnew)
!    call mctimer('Ordering equations              ',told, tnew)
!*ENDIF

    errflg = ier
    if (ier /= 0)  goto 999
    fbrdcnt_tmp = 0
    nfbprev = mdl%nfb
    if (checkfbrd_tmp .and. mdl%nfb > 1) then
        !
        ! check for redundant feedback variables
        !
!*IF TIMER
!        call xtime(told)
!*ENDIF
        call rm_redundant_fb(mdl, ier, errval, usinac = usinac, &
&                            fixepi = fixepi, dofbrd = dofbrd)
!*IF TIMER
!        call xtime(tnew)
!        call mctimer('Checking redundant fb variables', told, tnew)
!*ENDIF
        errflg = ier
        if (ier /= 0) goto 999
        fbrdcnt_tmp = nfbprev - mdl%nfb
    endif

    if (mdl%fboflg > 0) then
!*IF TIMER
!        call xtime(told)
!*ENDIF
        call gen_fb_order(0, mdl, fbor_err)
!*IF TIMER
!        call xtime(tnew)
!        call mctimer('Generating fb ordering          ',  told, tnew)
!*ENDIF
    else
        fbor_err = 0
        mdl%fbomem = 0
        deallocate(mdl%fboptr, mdl%fbordr, stat = alloc_stat)
    endif

999 continue

!if (ier /= 0) then
!    select case (ier)
!    case (6)
!        ! duplicate lhs
!        call mcbyin(tmp, namelen, mdl%ivnames(errval), mdl%vnames)
!        call erfset(rname, errec, 643418, 1, namelen, tmp(1))
!    case (7)
!        !too many arc's
!        call erfset(rname, errec, 643419, 1, 1, maxarc)
!    case (11)
!         ! too many feedbacks
!        call nrfset(rname, errec, 643420, 1, 1, int(mdl%nfb, ISIS_IKIND))
!    case(20)
!         ! not enough memory
!        call erfset(rname, errec, 643440, 0)
!    case default
!        !internal ordering error *)
!        call erfset(rname, errec, 643421, 1, 1, ier)
!    end select
!endif
!
!if (fbor_err == 3) then
!    ! not enough memory to generate feedback ordering
!    call erfset(rname, erinf, 643441, 0)
!endif


if (present(fbrdcnt)) fbrdcnt = fbrdcnt_tmp

call deallocate_mcordr_work
if (allocated(fbtypfix)) deallocate(fbtypfix)
return

end subroutine order_mdl

end module mdordr
