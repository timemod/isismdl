module mdl_flen

! maximum filename including path and extension.
integer, parameter :: MAXFLEN = 256

! maximum size of extension (including a . or _ ):
integer, parameter :: MAXEXTLEN = 5

! maximum length of the modelname (excluding extension):
integer, parameter :: MAXMOLEN = 128

end module mdl_flen
