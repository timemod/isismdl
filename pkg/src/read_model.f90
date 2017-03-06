subroutine read_model_fortran(modelnmlen, modelnm, model_index, ier)
    ! 
    ! read the model from the file
    !
    use modelworkspaces
    use msvars
    use output_utils

    integer, intent(in)               :: modelnmlen
    integer, dimension(*), intent(in) :: modelnm
    integer, intent(out)              :: model_index, ier
    character(len = 80)               :: str

    integer :: fstat

    model_index = create_mws()
    if (model_index < 0) return

    call read_mif_file(mws_array(model_index)%mdl, modelnmlen, modelnm, ier, &
&                      fstat)

    ! TODO: error handling if fstat != 0
    
    str = "Model with       equations read"
    write(str(12:16), '(I5)') mws_array(model_index)%mdl%neq
    call isismdl_out(str)

    if (ier == 0) call mwsinit(mws_array(model_index), ier)

    ! TODO: if ier <> 0 then delete the current mws from the list
    !       of used mwses module modelworkspaces
    if (ier /= 0) return
        
    if (.not. has_fb_order(mws_array(model_index)%mdl)) then
        call isismdl_out("No feedback ordering for this model...")
    elseif (mws_array(model_index)%mdl%fboflg == 2) then
            call mdgfbo(mws_array(model_index)%mdl, ier)
            if (ier == 0) then
                call isismdl_out("Feedback ordering generated ...")
            else 
                call isismdl_out("Feedback ordering NOT generated ...")
            endif
    elseif (mws_array(model_index)%mdl%fboflg == 1) then
        call isismdl_out("Feedback ordering read from mif file...")
    endif

end subroutine read_model_fortran
