!
! module with variables used during model compilation
!
module mcvars
    use model_type
    use mdl_flen

    type(model), save :: mdl
    integer(kind = MC_IKIND), dimension(:), allocatable, save :: &
&       vtype, maxlag, mxlead
    character(len = MAXFLEN), save :: mdlpath, mifnam, xrfnam
    character(len = 256), save :: errstr
    integer, save :: nerror, lenxrf
    integer, save :: maxref
    integer(kind = MC_IKIND), save :: nre

    contains

        subroutine allocate_mcvars(varCount, stat)
            integer(kind = MC_IKIND), intent(in) :: varCount
            integer, intent(out) :: stat

            allocate(vtype((varCount - 1) / MCNYI4 + 1), stat = stat)
            if (stat == 0) allocate(maxlag(varCount), stat = stat)
            if (stat == 0) allocate(mxlead(varCount), stat = stat)
             
            if (stat /= 0) call clear_mcvars
        end subroutine allocate_mcvars

        subroutine clear_mcvars
            if (allocated(vtype))  deallocate(vtype)
            if (allocated(maxlag)) deallocate(maxlag)
            if (allocated(mxlead)) deallocate(mxlead)
        end subroutine clear_mcvars

end module mcvars
