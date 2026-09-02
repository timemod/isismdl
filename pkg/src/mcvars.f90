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
    integer, save :: nerror
    integer, save :: maxref
    integer(kind = MC_IKIND), save :: nre

    contains

        ! initialize all saved variables for the model compilation
        subroutine init_mcvars(genfbo)
            logical, intent(in) :: genfbo

            nerror = 0
            errstr = 'Internal error: error string not set'
            nre = 0

            ! initialise mdl
            mdl%ivlast = 0
            mdl%iplast = 0
            mdl%ielast = 0
            mdl%iflast = 0
            mdl%iulflast = 0
            mdl%neq = 0
            mdl%nrv = 0
            mdl%nrp = 0
            mdl%nrc = 0
            mdl%nuf = 0
            mdl%nulf = 0
            mdl%ufblen = 0
            mdl%eqblen = 0
            mdl%fbomem = 0
            if (genfbo) then
                mdl%fboflg = 2
            else
                mdl%fboflg = 0
            endif
        end subroutine init_mcvars

        subroutine allocate_mcvars(varCount, stat)
            integer(kind = MC_IKIND), intent(in) :: varCount
            integer, intent(out) :: stat

            call clear_mcvars

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
