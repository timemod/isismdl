module mcpars

!     defines file units to be used by model compiler
!             filename extensions
!             version identification strings
!     xrfunt    .xrf file   (cross reference)
!     polunt    scratch file for temporary storage of model code
!     ufnunt    scratch file for temporary storage of user funcs
!     mciunt    for .mif modelcode output
!     depunt    scratch file for temporary storage of xref info
!     parunt    scratch file for temporary storage of param data
integer, parameter :: xrfunt = 4 , mciunt = 4,  polunt = 10, &
                      ufnunt = 11, depunt = 8 , parunt = 8

! current .mif version identification string
character(len = 11), parameter :: mifver = 'MIFISIS.A04'

end module mcpars
