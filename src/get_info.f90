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

subroutine get_period_info(model_index, per_len, max_lag, max_lead)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in) :: model_index
    integer(c_int), intent(out) :: per_len, max_lag, max_lead
    per_len = mws_array(model_index)%perlen
    max_lag = mws_array(model_index)%mdl%mxlag
    max_lead = mws_array(model_index)%mdl%mxlead
end subroutine get_period_info

subroutine get_variable_name(model_index, i, nam, nlen)
    use modelworkspaces
    use iso_c_binding
    integer(c_int), intent(in)   :: model_index, i
    integer(c_int), intent(out)  :: nlen
    integer, dimension(*), intent(out) :: nam
    call get_var_name(mws_array(model_index)%mdl, i, .false., nam, nlen);
end subroutine get_variable_name

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
