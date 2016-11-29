module kinds
    ! kind of all integers variables used by LAPACK (and BLAS)
    integer, parameter :: LAPACK_IKIND = 4
    ! kind of all several integers variables used by linear
    ! algebra routines
    integer, parameter :: LI_IKIND = LAPACK_IKIND
    ! kind of all real variables used by solving models
    integer, parameter :: SOLVE_RKIND = 8
    ! integer kind of all integer arrays and some integer
    ! variables for solving models
    integer, parameter :: SOLVE_IKIND = 4
    integer, parameter :: ISIS_RKIND = 8
    integer, parameter :: ISIS_IKIND = kind(1)
end module kinds

