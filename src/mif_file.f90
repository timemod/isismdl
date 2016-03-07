! Information about mif file
module mif_file
    use mdl_flen

    ! parameters
    character(len = 11), parameter :: MIFVER = 'MIFISIS.A04' ! current version
    character(len = 11), parameter :: MIFVR0 = 'MIFISIS.A03' ! old version
    integer, parameter :: MIFUNT = 8

    !     parameters for reading old mif version (< MWSISIS.A03)
    integer, parameter :: OLD_MAX_FUNC_NAME = 6

    ! auxiliarry variables (is save really necessary here?)
    integer, save ::  cnamelen, mifvernum
    character(len = MAXFLEN), save  :: pathnm
    character(len = MAXMOLEN), save :: cname
    character(len = 11), save :: fcode

end module mif_file
