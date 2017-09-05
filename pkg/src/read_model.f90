subroutine read_model_fortran(modelnmlen, modelnm, model_index, ier)
    ! 
    ! read the model from the file
    !
    ! ier: error flag
    !  0   ok
    !  1   error opening mif file
    !  2   several errors reading mif file
    !  3   error detected in model
    !  4   out of memory

    use modelworkspaces
    use msvars
    use output_utils
    use mcheck

    integer, intent(in)               :: modelnmlen
    integer, dimension(*), intent(in) :: modelnm
    integer, intent(out)              :: model_index, ier
    character(len = 80)               :: str

    integer :: fstat, nerr, fbo_err

    model_index = create_mws()
    if (model_index < 0) return

    call read_mif_file(mws_array(model_index)%mdl, modelnmlen, modelnm, ier, &
&                      fstat)
    if (ier /= 0) then
        if (ier == 1) then
            ier = 1
        else 
            ier = 2
        endif
        goto 999
    endif
    
    str = "Model with       equations read"
    write(str(12:16), '(I5)') mws_array(model_index)%mdl%neq
    call isismdl_out(str)

    !
    ! check model for syntactic erros
    !
    call isismdl_out("Checking Model-code...")
    call chkmdl(mws_array(model_index)%mdl, nerr)
    if (nerr == 0) then
        call isismdl_out("Model is ok...")
    else
        ier = 3
        call isismdl_out("Errors encountered in model...")
        goto 999
    endif

    if (.not. has_fb_order(mws_array(model_index)%mdl)) then
        call isismdl_out("No feedback ordering for this model...")
    else if (mws_array(model_index)%mdl%fboflg == 2) then
        call mdgfbo(mws_array(model_index)%mdl, fbo_err)
        if (fbo_err == 0) then
             call isismdl_out("Feedback ordering generated ...")
        else 
            call isismdl_out("Feedback ordering NOT generated ...")
        endif
    elseif (mws_array(model_index)%mdl%fboflg == 1) then
        call isismdl_out("Feedback ordering read from mif file...")
    endif

    call mwsinit(mws_array(model_index), ier)
    if (ier /= 0) ier = 4

999 if (ier /= 0) then
        call clear_mws(mws_array(model_index))
        call remove_mws(model_index)
        model_index = -1
    endif

end subroutine read_model_fortran
