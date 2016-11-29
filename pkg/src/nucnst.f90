module nucnst
    use kinds
    real(kind = ISIS_RKIND), parameter :: Rzero = 0.0_ISIS_RKIND
    real(kind = ISIS_RKIND), parameter :: Rone  = 1.0_ISIS_RKIND
    real(kind = ISIS_RKIND), parameter :: Rtwo  = 2.0_ISIS_RKIND
    real(kind = ISIS_RKIND), parameter :: Rten  = 10.0_ISIS_RKIND
    real(kind = ISIS_RKIND), parameter :: Rhalf = 0.5_ISIS_RKIND
    real(kind = ISIS_RKIND), parameter :: Rhund = 100_ISIS_RKIND
    real(kind = ISIS_RKIND), parameter :: Rmeps = epsilon(1.0_ISIS_RKIND)
    real(kind = ISIS_RKIND), parameter :: Rtiny = tiny(1.0_ISIS_RKIND)
end module nucnst
