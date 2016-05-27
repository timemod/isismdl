!
! This file contains several fortran functions and subroutines
! to extract information about the models that are in common blocks.
! These fortran functions and subroutines can be called directly in C.
!

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

subroutine get_variable_name(model_index, i, nam, nlen)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in)   :: model_index, i
    integer(c_int), intent(out)  :: nlen
    integer, dimension(*), intent(out) :: nam
    call get_var_name(mws_array(model_index)%mdl, i, .false., nam, nlen);
end subroutine get_variable_name

subroutine get_ca_name(model_index, i, nam, nlen)
    ! returns the name of the i'th constant adjustment
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in)   :: model_index, i
    integer(c_int), intent(out)  :: nlen
    integer, dimension(*), intent(out) :: nam

    integer :: ivar

    ivar = mws_array(model_index)%mdl%ica(i)
    call get_var_name(mws_array(model_index)%mdl, ivar, .false., nam, nlen);
end subroutine get_ca_name

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

subroutine get_fix_info(model_index, nfix,  jtb, jte)
    use iso_c_binding
    use modelworkspaces
    integer(c_int), intent(in)  :: model_index
    integer(c_int), intent(out) :: nfix, jtb, jte
    
    integer :: jt
    type(modelworkspace), pointer :: mws

    mws => mws_array(model_index)

    nfix = mws%fix_vars%var_count

    jtb = -1
    do jt = 1, mws%perlen
        if (isfixp(mws, jt)) then
            jtb = jt
            exit
        endif
    end do
    if (jtb > 0) then
        do jt = mws%perlen, jtb, -1
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

    jtb = -1
    do jt = 1, mws%perlen
        if (isfitp(mws, jt)) then
            jtb = jt
            exit
        endif
    end do
    if (jtb > 0) then
        do jt = mws%perlen, jtb, -1
            if (isfitp(mws, jt)) then
                jte = jt
                exit
            endif
        end do
    else 
        nfit = 0
    endif
end subroutine get_fit_info

