module mcpars

!     defines file units to be used by model compiler
!             filename extensions
!             version identification strings
!     outunt    output file
!     xrfunt    .xrf file   (cross reference)
!     polunt    scratch file for temporary storage of model code
!     ufnunt    scratch file for temporary storage of user funcs
!     mciunt    for .mif modelcode output
!     depunt    scratch file for temporary storage of xref info
!     parunt    scratch file for temporary storage of param data
integer ::  outunt, xrfunt, polunt, mciunt, depunt, parunt, ufnunt
parameter( outunt = 6 , xrfunt = 4 , mciunt = 4 , polunt = 10, ufnunt = 11, &
&          depunt = 8 , parunt = 8 )
!     filename extensions
character(len = 4) :: mdlext, xrfext, mifext
character(len = 1) :: extsep
parameter( extsep = '.' )
parameter( mdlext = extsep // 'mdl' , xrfext = extsep // 'mrf' , &
&          mifext = extsep // 'mif' )

!     current .mif version identification string
character(len = 11) :: mifver
parameter( mifver = 'MIFISIS.A04' )

integer, parameter :: MC_INT_KIND = 4
integer, parameter :: MC_NYI4 = 4

end module mcpars
