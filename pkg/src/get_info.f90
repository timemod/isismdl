!
! This file contains several fortran functions and subroutines
! to extract information about the models 
! These fortran functions and subroutines can be called directly in C.
!
function get_param_count(model_index)
    use modelworkspaces
    use iso_c_binding
    integer(c_int) :: get_param_count
    integer(c_int), intent(in) :: model_index
    get_param_count = mws_array(model_index)%mdl%nrp
end function get_param_count

function get_variable_count(model_index)
    use modelworkspaces
    use iso_c_binding
    integer(c_int) :: get_variable_count
    integer(c_int), intent(in) :: model_index
    get_variable_count = mws_array(model_index)%mdl%nrv
end function get_variable_count

function get_ca_count(model_index)
    use modelworkspaces
    use iso_c_binding
    integer(c_int) :: get_ca_count
    integer(c_int), intent(in) :: model_index
    get_ca_count = mws_array(model_index)%mdl%nca
end function get_ca_count

function get_endex_count(model_index)
    use modelworkspaces
    use iso_c_binding
    integer(c_int) :: get_endex_count
    integer(c_int), intent(in) :: model_index
    get_endex_count = mws_array(model_index)%mdl%nendex
end function get_endex_count

function get_fb_count(model_index)
    use modelworkspaces
    use iso_c_binding
    integer(c_int) :: get_fb_count
    integer(c_int), intent(in) :: model_index
    get_fb_count = mws_array(model_index)%mdl%nfb
end function get_fb_count

function get_eq_count(model_index)
    use modelworkspaces
    use iso_c_binding
    integer(c_int) :: get_eq_count
    integer(c_int), intent(in) :: model_index
    get_eq_count = mws_array(model_index)%mdl%neq
end function get_eq_count

subroutine get_param_name(model_index, i, nam, nlen, alpha)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in)   :: model_index, i, alpha
    integer(c_int), intent(out)  :: nlen
    integer, dimension(*), intent(out) :: nam
    call get_par_name(mws_array(model_index)%mdl, i, alpha /= 0, nam, nlen);
end subroutine get_param_name

subroutine get_variable_name(model_index, ivar, nam, nlen, alpha)
    ! return the variable name for variable ivar 
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in)   :: model_index, ivar, alpha
    integer(c_int), intent(out)  :: nlen
    integer, dimension(*), intent(out) :: nam
    call get_var_name(mws_array(model_index)%mdl, ivar, alpha /= 0, nam, nlen);
end subroutine get_variable_name

subroutine get_ca_name(model_index, i, nam, nlen)
    ! returns the name of the i'th constant adjustment 
    ! (NOT in alphabetical order!!!), both active and inactive
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in)   :: model_index, i
    integer(c_int), intent(out)  :: nlen
    integer, dimension(*), intent(out) :: nam

    integer :: ivar

    ivar = mws_array(model_index)%mdl%ica(i)
    ! ivar is negative for deactived equations
    ivar = abs(ivar)
    call get_var_name(mws_array(model_index)%mdl, ivar, .false., nam, nlen);
end subroutine get_ca_name

subroutine get_endex_name(model_index, i, nam, nlen)
    ! returns the name of the i'th endogenous lead (active and inactive) 
    ! (NOT in alphabetical order!!!)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in)   :: model_index, i
    integer(c_int), intent(out)  :: nlen
    integer, dimension(*), intent(out) :: nam

    integer :: ivar

    ! iendex(i) is negative for the lhs of inactive equations
    ivar = abs(mws_array(model_index)%mdl%iendex(i))
    call get_var_name(mws_array(model_index)%mdl, ivar, .false., nam, nlen);
end subroutine get_endex_name

subroutine get_fb_name(model_index, i, nam, nlen)
    ! returns the name of the i'th feedback variable (active and inactive) 
    ! (NOT in alphabetical order!!!)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in)   :: model_index, i
    integer(c_int), intent(out)  :: nlen
    integer, dimension(*), intent(out) :: nam

    integer :: ivar

    ivar = mws_array(model_index)%mdl%numfb(i)
    call get_var_name(mws_array(model_index)%mdl, ivar, .false., nam, nlen);
end subroutine get_fb_name


subroutine get_equation_name(model_index, ieq, nam, nlen, alpha)
    ! returns the name of the i'th equation
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in)   :: model_index, ieq, alpha
    integer(c_int), intent(out)  :: nlen
    integer, dimension(*), intent(out) :: nam

    call get_eq_name(mws_array(model_index)%mdl, ieq, alpha /= 0, nam, nlen)
end subroutine get_equation_name

integer(c_int) function get_par_index(model_index, name, namelen)
    use iso_c_binding
    use modelworkspaces
    use mdl_name_utils
    integer(c_int), intent(in) :: model_index, name(*), namelen
    
    type(model), pointer :: mdl
    mdl => mws_array(model_index)%mdl
    get_par_index = find_name(name, namelen, mdl%ipnames, mdl%indexp, &
&                             mdl%pnames, mdl%nrp)
end function get_par_index


integer(c_int) function get_var_index(model_index, name, namelen)
    use iso_c_binding
    use modelworkspaces
    use mdl_name_utils
    integer(c_int), intent(in) :: model_index, name(*), namelen
    
    type(model), pointer :: mdl
    mdl => mws_array(model_index)%mdl
    get_var_index = find_name(name, namelen, mdl%ivnames, mdl%indexv, &
&                             mdl%vnames, mdl%nrv)
end function get_var_index

integer(c_int) function get_ca_index(model_index, name, namelen)
    use iso_c_binding
    use modelworkspaces
    use mdl_name_utils
    integer(c_int), intent(in) :: model_index, name(*), namelen

    integer :: ivar
    
    type(model), pointer :: mdl
    mdl => mws_array(model_index)%mdl
    ivar = find_name(name, namelen, mdl%ivnames, mdl%indexv, mdl%vnames, &
                     mdl%nrv) 
    if (ivar >= 0) then
        get_ca_index = mdl%aci(ivar)
    else 
        get_ca_index =  0
    endif
end function get_ca_index

integer(c_int) function get_iendex(model_index, name, namelen)
    ! returns the index of an endogenous lead (both active
    ! and inactive)
    use iso_c_binding
    use modelworkspaces
    use mdl_name_utils
    use utils, only : idxlzuk
    integer(c_int), intent(in) :: model_index, name(*), namelen

    integer :: ivar
    
    type(model), pointer :: mdl
    mdl => mws_array(model_index)%mdl
    ivar = find_name(name, namelen, mdl%ivnames, mdl%indexv, mdl%vnames, &
                     mdl%nrv) 
    if (ivar >= 0) then
        get_iendex = idxlzuk(mdl%iendex, mdl%nendex, ivar)
        !mdl%iendex is negative if  the equation is deactivated
        if (get_iendex == 0) get_iendex = idxlzuk(mdl%iendex, mdl%nendex, -ivar)
    else 
        get_iendex =  0
    endif
end function get_iendex

integer(c_int) function get_eq_index(model_index, name, namelen)
    ! return the equation index of an equation
    use iso_c_binding
    use modelworkspaces
    use mdl_name_utils
    integer(c_int), intent(in) :: model_index, name(*), namelen
    
    type(model), pointer :: mdl
    mdl => mws_array(model_index)%mdl
    get_eq_index = find_name(name, namelen, mdl%ienames, mdl%indexe, &
&                             mdl%enames, mdl%neq)
end function get_eq_index

subroutine get_fix_info(model_index, nfix,  jtb, jte)
    use iso_c_binding
    use modelworkspaces
    integer(c_int), intent(in)  :: model_index
    integer(c_int), intent(out) :: nfix, jtb, jte
    
    integer :: jt
    type(modelworkspace), pointer :: mws

    mws => mws_array(model_index)

    nfix = mws%fix_vars%var_count

    jtb = - mws%mdl%mxlag
    jte = - mws%mdl%mxlag
    do jt = 1 - mws%mdl%mxlag, mws%perlen
        if (isfixp(mws, jt)) then
            jtb = jt
            exit
        endif
    end do
    if (jtb > -mws%mdl%mxlag) then
        do jt = mws%perlen + mws%mdl%mxlead, jtb, -1
            if (isfixp(mws, jt)) then
                jte = jt
                exit
            endif
        end do
    else 
        nfix = 0
    endif
end subroutine get_fix_info

subroutine get_fit_info(model_index, nfit,  jtb, jte)
    use iso_c_binding
    use modelworkspaces
    integer(c_int), intent(in)  :: model_index
    integer(c_int), intent(out) :: nfit, jtb, jte
    
    integer :: jt
    type(modelworkspace), pointer :: mws

    mws => mws_array(model_index)

    nfit = mws%fit_targets%var_count

    jtb = - mws%mdl%mxlag
    jte = - mws%mdl%mxlag
    do jt = 1 - mws%mdl%mxlag, mws%perlen + mws%mdl%mxlead
        if (isfitp(mws, jt)) then
            jtb = jt
            exit
        endif
    end do
    if (jtb > -mws%mdl%mxlag) then
        do jt = mws%perlen + mws%mdl%mxlead, jtb, -1
            if (isfitp(mws, jt)) then
                jte = jt
                exit
            endif
        end do
    else 
        nfit = 0
    endif
end subroutine get_fit_info

function get_param_length(model_index, i)
    use modelworkspaces
    use iso_c_binding
    integer(c_int) :: get_param_length
    integer(c_int), intent(in) :: model_index, i
    get_param_length = mws_array(model_index)%mdl%nvalp(i)
end function get_param_length

function equation_is_active(model_index, ieq)
    use modelworkspaces
    use iso_c_binding
    integer(c_int) :: equation_is_active
    integer(c_int), intent(in) :: model_index, ieq

    if (is_active(mws_array(model_index)%mdl, ieq)) then
        equation_is_active = 1
    else 
        equation_is_active = 0
    endif
end function equation_is_active

function get_lhsnum(model_index, ieq)
    use modelworkspaces
    use iso_c_binding
    integer(c_int) :: get_lhsnum
    integer(c_int), intent(in) :: model_index, ieq

    get_lhsnum = mws_array(model_index)%mdl%lhsnum(ieq)
end function get_lhsnum

integer function get_eq_order(model_index, ieq)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: model_index, ieq
    get_eq_order = mws_array(model_index)%mdl%order(ieq)
end function get_eq_order

function get_simerr(model_index)
    use modelworkspaces
    use iso_c_binding
    integer(c_int) :: get_simerr
    integer(c_int), intent(in) :: model_index
    get_simerr = mws_array(model_index)%simerr
end function get_simerr

function has_lag(model_index, iv)
    use modelworkspaces
    use iso_c_binding
    integer(c_int) :: has_lag
    integer(c_int), intent(in) :: model_index, iv
    if (mws_array(model_index)%mdl%ibx1(iv + 1) > &
        mws_array(model_index)%mdl%ibx1(iv)) then
        has_lag = 1
    else 
        has_lag = 0
    endif
end function has_lag

function has_lead(model_index, iv)
    use modelworkspaces
    use iso_c_binding
    integer(c_int) :: has_lead
    integer(c_int), intent(in) :: model_index, iv
    if (mws_array(model_index)%mdl%ibx2(iv + 1) > &
        mws_array(model_index)%mdl%ibx2(iv)) then
        has_lead = 1
    else 
        has_lead = 0
    endif
end function has_lead

! returns the number of simulatenous variables
function get_simul_count(model_index)
    use modelworkspaces
    use iso_c_binding
    integer(c_int) :: get_simul_count
    integer(c_int), intent(in) :: model_index
    integer :: loops, loope  
    loops = mws_array(model_index)%mdl%loops
    loope = mws_array(model_index)%mdl%loope
    get_simul_count = loope - loops + 1
end function get_simul_count

subroutine get_simul_name(model_index, isimul, nam, nlen)
    ! return the name of the isimul'th simulatneous variable
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in)   :: model_index, isimul
    integer(c_int), intent(out)  :: nlen
    integer, dimension(*), intent(out) :: nam
    integer ieq_ordr, ieq, ivar

    ieq_ordr = mws_array(model_index)%mdl%loops + isimul - 1
    ieq = mws_array(model_index)%mdl%order(ieq_ordr)
    if (ieq <= 0) then
       ! the equation is deactivated
       nlen = 0
       return
    endif
    ivar = mws_array(model_index)%mdl%lhsnum(ieq)
    if (.not. mws_array(model_index)%mdl%lik(ivar)) then
        nlen = 0
        return
    endif
    call get_var_name(mws_array(model_index)%mdl, ivar, .false., nam, nlen);
end subroutine get_simul_name
