module mdl_flen

! maximum filename including path and extension.
integer, parameter :: MAXFLEN = 4096

! maximum size of extension (including a . or _ ):
integer, parameter :: MAXEXTLEN = 5

! maximum length of the modelname (excluding extension):
integer, parameter :: MAXMOLEN = 1024

end module mdl_flen
