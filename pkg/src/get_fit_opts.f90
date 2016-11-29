subroutine get_fit_dbgopts(prica, prijac, supsot)
    use get_options
    use iso_c_binding, only : c_int
    integer(c_int), intent(out) :: prica, prijac, supsot
    prica = logical2int(options%fit%prica)
    prijac = logical2int(options%fit%prijac)
    supsot = logical2int(options%fit%supsot)
end subroutine get_fit_dbgopts
