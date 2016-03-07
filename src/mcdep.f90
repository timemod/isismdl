module mcdep
!     Module for variables, simultaneous variables
!     and parameters in an equation

!     maximum number of variables or parameters on the rhs of an equation

integer, parameter ::  MAXRHS = 1000

!     options for procedure mcodep:
integer, parameter :: CMPMDL = 1  ! register all variables and parameters
integer, parameter :: ORDMDL = 2  ! register only simultaneous variables
integer, parameter :: LINEQN = 3  ! register only variables

!     iv(:nv)       : all variables in rhs of equation
!     isimv(:nsimv) : all simultaneous variables in rhs of eqaution
!     iparv(:nparv) : all parameters in equation
!     iminld(:nv)   : minimum shift of variable in equation
!     imaxld(:nv)   : maximum shift of variable in equation

integer, dimension(MAXRHS), save :: iv, isimv, iparv
integer, save                    :: nv, nsimv, nparv
integer, dimension(MAXRHS), save :: iminld, imaxld

end module mcdep
