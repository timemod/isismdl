subroutine init_set_options(model_index, use_mws)
    use modelworkspaces
    use set_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: model_index, use_mws

    if (use_mws /= 0) then
        ! directly modify the options in the mws
        options_set => mws_array(model_index)%solve_opts
    else 
        ! create of copy of the options of the mws, and
        ! use these option. This is used when the user has 
        ! supplied options in the call of solve.
        tmp_options = mws_array(model_index)%solve_opts
        options_set => tmp_options
    endif
end subroutine init_set_options

