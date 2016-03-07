module mcvars
    use model_type
    use mdl_flen

    type(model), save :: mdl
    integer(kind = MC_IKIND), dimension(:), allocatable, save :: &
&       vtype, maxlag, mxlead
    character(len = MAXFLEN), save :: mdlpath, mifnam, xrfnam
    character(len = 256), save ::  errstr
    logical, save ::  faterr
    integer, save :: nerror, filerr, filios, lenxrf
    integer, save :: maxref
    integer(kind = MC_IKIND), save :: nre
    integer, save :: mc_alloc_stat

    contains

        subroutine clear_mcvars
            integer :: stat
            deallocate(vtype,  stat = stat)
            deallocate(maxlag, stat = stat)
            deallocate(mxlead, stat = stat)
        end subroutine clear_mcvars

end module mcvars
