module set_options
    use mws_type
    use kinds
    use solve_options_type
    type(solve_options), pointer, save :: options_set
    type(solve_options), target, save :: tmp_options
end module set_options
