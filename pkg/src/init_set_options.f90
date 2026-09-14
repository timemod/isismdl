subroutine init_set_options(model_index, use_mws)
    use modelworkspaces
    use set_options
    use iso_c_binding, only : c_int
    use msvars, only : opts
    integer(c_int), intent(in) :: model_index, use_mws

    if (use_mws /= 0) then
        ! modify the options in the mws
        options_set => mws_array(model_index)%solve_opts
    else 
        ! modify solve option in msvars
        options_set => opts
    endif
end subroutine init_set_options

