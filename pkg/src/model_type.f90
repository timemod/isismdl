module model_type

    use model_params

    !
    ! Model paramters
    !    fboflg feedback ordering flag
    !             0 : do not generate feedback ordering
    !             1 : generate feedback ordering but do not write
    !                 to mif
    !             2 : generate fb ordering and write to mif
    !             WARNING: if fboflg > 0, the model does not
    !             always have a feedback ordering. See argument
    !             fbomem.
    !   fbomem  size that array fbordr should have to hold
    !           the feedback ordering information. If equal to 0,
    !           the model does not have a feedback ordering.
    !           This is possible if the model has less than 2
    !           feedback variables or if the Jacobian is so
    !           dense that it is not advantageous to
    !           use feedback ordering (see module mcfbor).


    type model
        integer(kind = MC_IKIND)  :: cnamelen
        character(len = MAXMOLEN_2) :: cname
        character(len = 10) :: date, time
        integer(kind = MC_IKIND)  :: &
&           neq,loops,loope,nuf,nulf,ielast,ufblen,eqblen, &
&           nrp,nrc,nfb,nrv,nca,nendex,nd,nd1,ivlast,iplast, &
&           iflast,iulflast, fboflg,fbomem, &
&           mxlead, mxlag
        integer(kind = MC_IKIND), dimension(:), allocatable :: &
&           enames,vnames,pnames,fnames,ulfnames,ienames,ivnames, &
&           ipnames,ifnames,iulfnames,indexe,lhsnum,indexv, &
&           narguf,indexp,nvalp,cfptr,etype, &
&           numfb,fbtype,iendex,ica,aci,ibx1,ibx2, &
&           order,fboptr,fbordr,nrecp,nrecuf,equat
         logical(kind = MC_IKIND), dimension(:), allocatable :: lik
         real(kind = MC_RKIND), dimension(:), allocatable :: coef
    end type model

    ! if mdl_alloc_stat > 0, then there was not enough memory to
    ! allocate all model variables
    integer, save :: mdl_alloc_stat

    integer, parameter, private :: ACTIVATE = 1
    integer, parameter, private :: DEACTIVATE = 2

    private :: activate_deactivate_eq

contains

    subroutine allocate_model(mdl, eqCount, varCount, parCount, &
&       coefCount, funCount, ulFunCount, eqCharCount, &
&       varCharCount, parCharCount, funCharCount, ulFunCharCount, &
&       caCount, leadVarCount)

       type(model), intent(inout) :: mdl
       integer(kind = MC_IKIND), intent(in) :: eqCount, varCount, &
&                  parCount, coefCount, funCount, ulFunCount
       integer(kind = MC_IKIND), intent(in) ::  eqCharCount, &
&               varCharCount, parCharCount, funCharCount, &
&               ulFunCharCount, caCount, leadVarCount

       integer(kind = MC_IKIND) ::  kmaxe, kmaxv, kmaxp, kmaxf, kmaxulf
       integer :: stat

       call deallocate_model(mdl)

       allocate(mdl%ienames(eqCount), stat = stat)
       if (stat == 0) allocate(mdl%ivnames(varCount), stat = stat)
       if (stat == 0) allocate(mdl%ipnames(parCount), stat = stat)
       if (stat == 0) allocate(mdl%ifnames(funCount), stat = stat)
       if (stat == 0) allocate(mdl%indexe(eqCount), stat = stat)
       if (stat == 0) allocate(mdl%indexv(varCount), stat = stat)
       if (stat == 0) allocate(mdl%indexp(parCount), stat = stat)
       if (stat == 0) allocate(mdl%lhsnum(eqCount), stat = stat)
       if (stat == 0) allocate(mdl%etype(eqCount / MCNYI4 + 1), stat = stat)
       if (stat == 0) allocate(mdl%narguf(funCount), stat = stat)
       if (stat == 0) allocate(mdl%coef(coefCount), stat = stat)
       if (stat == 0) allocate(mdl%nvalp(parCount), stat = stat)
       if (stat == 0) allocate(mdl%cfptr(parCount), stat = stat)
       if (stat == 0) allocate(mdl%order(eqCount), stat = stat)
       if (stat == 0) allocate(mdl%ibx1(varCount + 1), stat = stat)
       if (stat == 0) allocate(mdl%ibx2(varCount + 1), stat = stat)
       if (stat == 0) allocate(mdl%lik(varCount), stat = stat)
       if (stat == 0) allocate(mdl%ica(caCount), stat = stat)
       if (stat == 0) allocate(mdl%aci(varCount), stat = stat)

       mdl_alloc_stat = stat

       if (mdl_alloc_stat > 0) return

       kmaxp  =  (parCharCount - 1)   / MCNYI4 + 1
       kmaxe  =  (eqCharCount  - 1)   / MCNYI4 + 1
       kmaxv  =  (varCharCount - 1)   / MCNYI4 + 1
       kmaxf  =  (funCharCount - 1)   / MCNYI4 + 1

       allocate(mdl%enames(kmaxe), stat = stat)
       if (stat == 0) allocate(mdl%vnames(kmaxv), stat = stat)
       if (stat == 0) allocate(mdl%pnames(kmaxp), stat = stat)
       if (stat == 0) allocate(mdl%fnames(kmaxf), stat = stat)
       mdl_alloc_stat = stat
       if (mdl_alloc_stat > 0) return

       if (leadVarCount > 0) then
           allocate(mdl%iendex(leadVarCount), stat = mdl_alloc_stat)
           if (mdl_alloc_stat > 0) return
       endif

       if (ulFunCount > 0) then
           kmaxulf = (ulFunCharCount - 1) / MCNYI4 + 1
           allocate(mdl%iulfnames(ulFunCount), stat = stat)
           if (stat == 0) allocate(mdl%ulfnames(kmaxulf), stat= stat)
           mdl_alloc_stat = stat
       endif


    end subroutine allocate_model

    subroutine deallocate_model(mdl)
        type(model), intent(inout) :: mdl

        integer :: stat

        deallocate(mdl%enames, stat = stat)
        deallocate(mdl%vnames, stat = stat)
        deallocate(mdl%pnames, stat = stat)
        deallocate(mdl%fnames, stat = stat)
        deallocate(mdl%ulfnames,stat = stat)
        deallocate(mdl%ienames, stat = stat)
        deallocate(mdl%ivnames, stat = stat)
        deallocate(mdl%ipnames, stat = stat)
        deallocate(mdl%ifnames, stat = stat)
        deallocate(mdl%iulfnames, stat = stat)
        deallocate(mdl%indexe, stat = stat)
        deallocate(mdl%indexv, stat = stat)
        deallocate(mdl%indexp, stat = stat)
        deallocate(mdl%lhsnum, stat = stat)
        deallocate(mdl%narguf, stat = stat)
        deallocate(mdl%nvalp, stat = stat)
        deallocate(mdl%cfptr, stat = stat)
        deallocate(mdl%etype, stat = stat)
        deallocate(mdl%numfb, stat = stat)
        deallocate(mdl%fbtype, stat = stat)
        deallocate(mdl%iendex, stat = stat)
        deallocate(mdl%ica, stat = stat)
        deallocate(mdl%aci, stat = stat)
        deallocate(mdl%ibx1, stat = stat)
        deallocate(mdl%ibx2, stat = stat)
        deallocate(mdl%order, stat = stat)
        deallocate(mdl%fboptr, stat = stat)
        deallocate(mdl%fbordr, stat = stat)
        deallocate(mdl%nrecp, stat = stat)
        deallocate(mdl%nrecuf, stat = stat)
        deallocate(mdl%equat, stat = stat)
        deallocate(mdl%lik, stat = stat)
        deallocate(mdl%coef, stat = stat)

    end subroutine deallocate_model

    function has_endo_lead(mdl)
        logical :: has_endo_lead
        type(model), intent(in) :: mdl

        !
        !returns true if the model has an endogenous lead

        integer :: i

        has_endo_lead = .false.
        do i = 1, mdl%nendex
            if (mdl%iendex(i) > 0 .and. mdl%lik(mdl%iendex(i))) then
                has_endo_lead = .true.
                return
            endif
         end do
         return
    end function has_endo_lead
    
    ! This subroutine is used to extract the name of model
    ! variable i. The name is put as a byte string in nam,
    ! and nlen is set to its length. If a name does not
    ! exist is nlen is set to -1.
    subroutine get_var_name(mdl, i, alphabetical, nam, nlen)
        use mdl_name_utils
        type(model), intent(in) :: mdl
        integer, intent(in) :: i
        logical, intent(in) :: alphabetical
        integer, intent(out) :: nlen  ! length of name
        integer, dimension(*), intent(out) :: nam ! name (ascii char.)

        integer :: iv

        if (i <= 0 .or. i > mdl%nrv) then
            nlen = -1
            return
        endif

        if (alphabetical) then
            iv = mdl%indexv(i)
        else
            iv = i
        endif
        call mcgetnam(nam, nlen, mdl%ivnames(iv), mdl%vnames)

    end subroutine get_var_name

    ! This subroutine is used to extract the name of model
    ! parameter i. The name is put as a byte string in nam,
    ! and nlen is set to its length. If a name does not
    ! exist is nlen is set to -1.
    subroutine get_par_name(mdl, i, alphabetical, nam, nlen)
        use mdl_name_utils
        type(model), intent(in) :: mdl
        integer, intent(in) :: i
        logical, intent(in) :: alphabetical
        integer, intent(out) :: nlen  ! length of name
        integer, dimension(*), intent(out) :: nam ! name (ascii char.)

        integer :: ip

        if (i <= 0 .or. i > mdl%nrp) then
            nlen = -1
            return
        endif

        if (alphabetical) then
            ip = mdl%indexp(i)
        else
            ip = i
        endif
        call mcgetnam(nam, nlen, mdl%ipnames(ip), mdl%pnames)

    end subroutine get_par_name

    ! This subroutine is used to extract the name of model
    ! variable i. The name is put as a byte string in nam,
    ! and nlen is set to its length. If a name does not
    ! exist is nlen is set to -1.
    subroutine get_eq_name(mdl, i, alphabetical, nam, nlen)
        use mdl_name_utils
        type(model), intent(in) :: mdl
        integer, intent(in) :: i
        logical, intent(in) :: alphabetical
        integer, intent(out) :: nlen  ! length of name
        integer, dimension(*), intent(out) :: nam ! name (ascii char.)

        integer :: ieq

        if (i <= 0 .or. i > mdl%neq) then
            nlen = -1
            return
        endif

        if (alphabetical) then
            ieq = mdl%indexe(i)
        else
            ieq = i
        endif
        call mcgetnam(nam, nlen, mdl%ienames(ieq), mdl%enames)

    end subroutine get_eq_name

    ! This subroutine is used to extract the name of model
    ! parameter i. The name is put as a byte string in nam,
   
    logical function is_frml(mdl, iv) 
        ! returns true if variable iv is a frml equation
        type(model), intent(in) :: mdl
        integer, intent(in) :: iv
        is_frml = mdl%aci(iv) > 0
    end function is_frml

    logical function is_active(mdl, eqnum, alphabetical)
        ! returns true if equation eqnum is active
        type(model), intent(in) :: mdl
        integer, intent(in) :: eqnum
        logical, intent(in) :: alphabetical

        integer :: ieq, eqtyp
        integer, external :: bysget

        if (alphabetical) then
            ieq = mdl%indexe(eqnum)
        else
            ieq = eqnum
        endif

        eqtyp = bysget(mdl%etype, ieq)
        ! eqtyp is lowercase for active eqaution and uppercase for an 
        ! inactive equaiton
        is_active = eqtyp <= 96
    end function is_active

    subroutine activate_eq(mdl, eqnum)
        type(model), intent(inout) :: mdl
        integer, intent(in) :: eqnum
        call activate_deactivate_eq(mdl, eqnum, ACTIVATE)
    end subroutine activate_eq

    subroutine deactivate_eq(mdl, eqnum)
        type(model), intent(inout) :: mdl
        integer, intent(in) :: eqnum
        call activate_deactivate_eq(mdl, eqnum, DEACTIVATE)
    end subroutine deactivate_eq

    subroutine activate_deactivate_eq(mdl, eqnum, action)
        use utils, only : idxlzuk
        type(model), intent(inout) :: mdl
        integer, intent(in) :: eqnum, action

        integer :: eqtyp, lhsvar, jen, canum, lhsvar_new
        logical :: is_act
        integer, external :: bysget

        is_act = is_active(mdl, eqnum, .false.)

        if ((action == ACTIVATE .and. is_act) .or. &
            (action == DEACTIVATE  .and. .not. is_act)) return

        eqtyp  = bysget(mdl%etype, eqnum)
        lhsvar = mdl%lhsnum(eqnum)

        ! CA number (if <> 0 index into ica() corresponding to lhs variable)
        canum = mdl%aci(lhsvar)

        if (mdl%ibx2(lhsvar+1) > mdl%ibx2(lhsvar)) then
            ! have leads: lookup index in iendex (to make exogenous)
            jen = idxlzuk(mdl%iendex, mdl%nendex, lhsvar)
            if (jen == 0 ) then
                call rexit("Internal error in mcadeq: iendex array error")
            endif
        else 
            jen = 0
        endif
    
        ! eqtyp is lowercase for active eqaution and uppercase for an 
        ! inactive equaiton
        if (action == ACTIVATE) then
            call bysset(mdl%etype, eqnum, eqtyp - 32)
            lhsvar_new = lhsvar
        else 
            call bysset(mdl%etype, eqnum, eqtyp + 32)
            lhsvar_new = -lhsvar
        endif
        if (jen   > 0) mdl%iendex(jen) = lhsvar_new
        if (canum > 0) mdl%ica(canum)  = lhsvar_new
        mdl%lik(lhsvar) = action == ACTIVATE
    end subroutine activate_deactivate_eq

    logical function has_fb_order(mdl)
        type(model), intent(inout) :: mdl
        has_fb_order = mdl%fboflg > 0 .and. mdl%fbomem > 0
    end function has_fb_order

end module model_type
