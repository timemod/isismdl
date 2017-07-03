module mws_params
    integer, parameter :: MWS_RKIND = 8

    !
    ! constant for several update modes.
    ! the constants should be in agreement with ...
    !
    integer, parameter :: REPLACE = 0
    integer, parameter :: UPD     = 1
    integer, parameter :: UPD_NA  = 2
    integer, parameter :: UPD_VAL = 3
    
end module mws_params
