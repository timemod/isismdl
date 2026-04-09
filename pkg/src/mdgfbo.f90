subroutine mdgfbo(mdl, errflg)
    use mccedp
    use mcfbor
    use model_type
    use output_utils
    type(model), intent(inout) :: mdl
    integer, intent(out) :: errflg

    !
    ! generate feedback ordering for the efficient
    ! calculation of the Jacobian for an the model
    ! in solve memory
    !  errflg : 0 ok
    !           > 0: some error has occurred
    !

    !
    ! generate dependency structure
    !
    call mcgedp(mdl, .false., errflg)
    if (errflg /= 0) then
        call isismdl_out("ERROR: mcgedp generated dependency structure failed.")
        goto 999
    endif

    !
    ! generate feedback ordering
    !
    call gen_fb_order(0, mdl, errflg)

    if (errflg == 3) then
         call isismdl_out("ERROR: out of memory generating feedback ordering in mcfbor.")
    else if (errflg == 4) then
         call isismdl_out("ERROR: array boundaries exceeded in gen_fb_order.")
    else if (errflg /= 0 .and. errflg /= 1 .and. errflg /= 2) then
         call isismdl_out("ERROR: unknown error originating from gen_fb_order.")
    endif

999 continue

    ! clear dependency structure data
    call  clear_edep

end subroutine mdgfbo
