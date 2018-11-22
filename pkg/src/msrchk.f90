subroutine reschk(retcod)
    use msutil
    use msfbvl
    use msolot
    integer, intent(out) :: retcod
 
    ! solves the model for the current period targetc (jc)
    ! by performing a residual check
 
    ! at call, curvars contains starting values
    ! the result is stored in curvars.

    logical :: quit

    retcod = 0

    ! check exo's and ca's for validity
    ! no real need to check feedback values (will not propagate here)
    if (opts%erropt /= ERROPT_SILENT) then    
        call chkxa(quit)
        if (quit) then
            retcod = 1
            goto 100
        endif
    endif

    ! store starting values ( yp(*) = curvars(*) )
    ! call prologue
    ! call simultaneous block
    ! call epilogue

    ! Note 1st three args in call of const, solve and postr
    ! to avoid lhs results moving to rhs of equations

    yp = curvars

    call msprlg(retcod)
    call msloop(retcod)
    call mseplg(retcod)

100 continue

    call solotf(0)
    return
end subroutine reschk
