module mws_params
    integer, parameter :: MWS_RKIND = 8
    real(kind = MWS_RKIND) :: TSTDFL = 1.0e-6

    !
    ! constant for several update modes.
    ! the constants should be in agreement with src/incl/tsupd.tsl
    !
    integer, parameter :: REPLACE = 1
    integer, parameter :: UPD     = 2
    integer, parameter :: UPD_NA  = 3
    integer, parameter :: UPD_VAL = 4
end module mws_params
