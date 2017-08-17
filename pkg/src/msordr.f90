subroutine msordr(model_index, genfbo_specified, genfbo)
    !
    ! reorder the model in solve memory, directly callable from C
    !
    use iso_c_binding, only : c_int 
    use mccedp
    use mdordr
    use mws_type
    use output_utils
    use modelworkspaces
    integer(c_int), intent(in) :: model_index, genfbo_specified, genfbo

    ! in genfbo : 1  (* 1  to     generate feedback ordering
    !                 * 0 to NOT generate feedback ordering
    !                 *)

    ! in genfbo_specified : 1 argument genfbo specified by user
    !                       0 argument genfbo not specified by user

    integer :: ier, errflg
    integer :: fbrdcnt, fbor_err
    character(len = 40) :: msg
    type(model), pointer :: mdl

    mdl => mws_array(model_index)%mdl

    !
    ! generate dependency structure
    !
    call mcgedp(mdl, .false., ier)
    if (ier == 10) then
        ! not enough memory to generate the dependency structure
        call isismdl_error("Not enough memory for ordering the model" // &
                           "equations.")
        return
    else if (ier /= 0) then
        ! internal error in mcgedp
        write(msg, '(a, i5, a)') "Internal error ", ier, " in mcgedp"
        call isismdl_error(msg)
        goto 999
    endif
    
    if (genfbo_specified == 1) then
        if (genfbo == 1 .and. mdl%fboflg == 0) then
            mdl%fboflg = 2
        elseif (genfbo == 0) then
            mdl%fboflg = 0
        endif
    endif

    !
    ! order the model
    !
    call order_mdl(mdl, fbrdcnt = fbrdcnt, errflg = errflg, fbor_err = fbor_err)

    if (errflg /= 0) then
        call isismdl_error("Error ordering model")
        goto 999
    endif

    call isismdl_out("Summary ordering information")
    write(msg, '(a, i5, a)') "       ", mdl%loops - 1, " prologue equations"
    call isismdl_out(msg)
    write(msg, '(a, i5, a)') "       ", mdl%loope - mdl%loops +1, &
                             " simultaneous equations"
    call isismdl_out(msg)
    write(msg, '(a, i5, a)') "       ", mdl%neq - mdl%loope," epilogue equations"
    call isismdl_out(msg)
    if (fbrdcnt > 0) then
        write(msg, '(a, i5, a)') "       ", fbrdcnt, &
                                 "redundant feedback variables removed"
    endif
    if (mdl%fboflg > 0 .and. fbor_err == 0) then
        call isismdl_out("Feedback ordering installed")
    else
        call isismdl_out("No feedback ordering determined")
    endif


999 continue

    call clear_edep

    return
end subroutine msordr
