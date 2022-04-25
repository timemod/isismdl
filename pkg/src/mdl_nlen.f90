module mdl_nlen 
    !  define name parameters
    !  MCMXNM  maximum name length (in bytes)
    !  MCMXCT  maximum length in bytes of Isis CA tag (_ca)
    ! NOTE: MCMXNM should agree with the maximum name lengths in xpcdef.h
    integer, parameter :: MCMXNM = 60, MCMXCT = 3
end module mdl_nlen
