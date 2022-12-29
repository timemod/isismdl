subroutine init_get_options(model_index)
    use modelworkspaces
    use get_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(in) :: model_index

    options => mws_array(model_index)%solve_opts
end subroutine init_get_options
