subroutine write_model_fortran(mifnmlen, mifnm, model_index, ier)
    ! 
    ! read the model from the file
    !
    use modelworkspaces
    use mdl_flen

    integer, intent(in)               :: mifnmlen
    integer, dimension(*), intent(in) :: mifnm
    integer, intent(in)               :: model_index
    integer, intent(out)              :: ier

    character(len = MAXFLEN)               :: mifnam

    integer :: fstat

    call byasf7(mifnm, 1, mifnmlen, mifnam)

    call mcwmif(mws_array(model_index)%mdl, mifnam, ier)
    
    ! TODO: correct error handling
    ier = 0
end subroutine write_model_fortran
