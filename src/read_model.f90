subroutine read_model_fortran(modelnmlen, modelnm, model_index, ier)
    ! 
    ! read the model from the file
    !
    use modelworkspaces
    use msvars
    integer, intent(in)               :: modelnmlen
    integer, dimension(*), intent(in) :: modelnm
    integer, intent(out)              :: model_index, ier

    integer :: fstat

    model_index = create_mws()
    if (model_index < 0) return

    call read_mif_file(mws_array(model_index)%mdl, modelnmlen, modelnm, ier, &
&                      fstat)

    if (ier == 0) call mwsinit(mws_array(model_index), ier)

    ! TODO: if ier <> 0 then delete the current mws from the list
    !       of used mwses module modelworkspaces
    if (ier /= 0) return
    
    if (mws_array(model_index)%mdl%fboflg == 2) then
        call mdgfbo(mws_array(model_index)%mdl, ier)
    endif

end subroutine read_model_fortran
