module msfitm
    !
    ! work variables for the fit procedure
    !   fix_active:  true if fix variables are active
    !   nu_max : maximum number of fit CAs for entire solution
    !            period
    !   nw_max : maximum number of fit targets for entire solution
    !           period
    !   nu     : number of fit CAs for current period
    !            may be less than nu_max if fix procedure is used
    !   nw     : number of fit targets for current period
    !
    use kinds
    use msvars
    use nuna
    use nucnst

    ! NOTE: the values of the following parameters should agree with
    ! the order in which the corresponding scalemethod
    ! options are defined in bootproc/defns/setfopt.xpd.
    integer, parameter :: SCALE_NONE  = 1
    integer, parameter :: SCALE_ROW   = 2
    integer, parameter :: SCALE_BOTH  = 3

    logical, save :: fix_active, is_square, scale_w, scale_u
    integer(kind = SOLVE_IKIND), private, save :: nu_max = 0, &
&                                     nw_max = 0, nu = 0, nw = 0
    integer(kind = LI_IKIND), private, save :: lwork_fit
    integer(kind = SOLVE_IKIND), dimension(:), allocatable, &
&          save, private ::  numu, numr, numw, numu_prev
    real(kind = SOLVE_RKIND), dimension(:), allocatable, private, &
&           save :: w, delw, dddelw, b, du0, djtau, delu, resold, &
&                   zsav, work_fit, u_scale, w_scale
    real(kind = SOLVE_RKIND), dimension(:, :), allocatable, private, save :: dj

    private allocate_fit_work, fitone, mkdelw, mkdelu, &
&              mkb, mkdu0, mkdjac, mkdqr, mdlpas, msfuck, wfinit, msgstp, msgjac


contains

    subroutine allocate_fit_work(alloc_stat)
        integer, intent(out) :: alloc_stat

        integer :: stat
        integer(kind = SOLVE_IKIND), allocatable :: temp(:)

        if (nu_max < mdl%nca) then
            ! decrease the size of the numu and numr arrays
            allocate(temp(nu_max), stat = stat)
            if (stat /= 0) then
                alloc_stat = stat
                return
            endif
            temp(:nu_max) = numu(:nu_max)
            call move_alloc(temp, numu)
            allocate(temp(nu_max), stat = stat)
            if (stat /= 0) then
                alloc_stat = stat
                return
            endif
            temp(:nu_max) = numr(:nu_max)
            call move_alloc(temp, numr)
        endif

        allocate(numw(nw_max), stat = stat)
        if (stat == 0) allocate(w(nw_max), stat = stat)
        if (stat == 0) allocate(delw(nw_max) , stat = stat)
        if (stat == 0) allocate(dddelw(nw_max), stat = stat)
        if (stat == 0) allocate(b(nw_max) , stat = stat)
        if (stat == 0) allocate(du0(nw_max) , stat = stat)
        if (stat == 0) allocate(djtau(nw_max), stat = stat)
        if (stat == 0) allocate(delu(nu_max), stat = stat)
        if (stat == 0) allocate(resold(nu_max), stat = stat)
        if (stat == 0) allocate(zsav(mdl%nrv), stat = stat)
        if (stat == 0) allocate(work_fit(lwork_fit), stat = stat)
        alloc_stat = stat

    end subroutine allocate_fit_work

    subroutine init_fit_work(do_fit, error)
        use mdlvars
        use msfito

        logical, intent(out) :: do_fit
        integer, intent(out) :: error
        ! error = 0  ok
        ! error = 1  not enough memory

        integer :: jt, alloc_stat

        error = 0

        !
        ! initialise fit work data
        !

        call clear_fit_work

        do_fit = mws%fit_targets%var_count > 0 .and. mdl%nca > 0
        if (.not. do_fit) return

        lwork_fit = get_lwork_fit(mdl%nca, mws%fit_targets%var_count)

        ! check if in current solution period (jf .. jl)
        ! fit will be called
        do_fit = .false.
        do jt = jf, jl
            if (isfitp(mws, jt)) then
                do_fit = .true.
                exit
            endif
        end do

        if (.not. do_fit) then
            return
        endif

        allocate(numu(mdl%nca), stat = alloc_stat)
        if (alloc_stat == 0) allocate(numr(mdl%nca), stat = alloc_stat)
        if (alloc_stat /= 0) then
            error = 1
            call fitot11
            return
        endif

        call mknumu(mws, numu, numr, nu)
        nu_max = nu
        if (nu > 0) then
            call fitonu(nu, numu)
        endif

        nw_max = mws%fit_targets%var_count

        call allocate_fit_work(alloc_stat)
        if (alloc_stat /= 0) then
            error = 1
            call fitot11
            return
        endif

        fix_active = mws%fix_vars%var_count > 0
        if (fix_active) then
            allocate(numu_prev(nu), stat = alloc_stat)
            if (alloc_stat /= 0) then
                error = 1
                call fitot11
                return
            endif
            numu_prev = numu(:nu)
        endif

    end subroutine init_fit_work

    subroutine clear_fit_work

        integer :: stat

        deallocate(numu, stat = stat)
        deallocate(numr, stat = stat)
        deallocate(numw, stat = stat)
        deallocate(numu_prev, stat = stat)
        deallocate(w, stat = stat)
        deallocate(delw, stat = stat)
        deallocate(dddelw, stat = stat)
        deallocate(b, stat = stat)
        deallocate(du0, stat = stat)
        deallocate(djtau, stat = stat)
        deallocate(delu, stat = stat)
        deallocate(resold, stat = stat)
        deallocate(zsav, stat = stat)
        deallocate(work_fit, stat = stat)
        deallocate(u_scale, stat = stat)
        deallocate(w_scale, stat = stat)
        deallocate(dj, stat = stat)
        nu = 0
        nw = 0
        lwork_fit = 0
    end subroutine clear_fit_work

!-----------------------------------------------------------------------

    subroutine solfit(retcod, ndiver, jt)
        use msfito
        integer ::    retcod,ndiver,jt
        
        integer ::        fiterr
        logical ::        quit
        
        ! To understand what is going on in this code you
        ! must read the separate paper detailing the mathematics of the
        ! Fit procedure
        
        ! initialize arrays for period jt (with checks)
        call wfinit(jt, fiterr, quit)
        if (quit) then
            retcod = 1
            return
        endif
        
        if (fiterr /= 0 ) then
            call fitot3(fiterr)
            if (fiterr /= 2 )  then
                ! code 2 implies nothing to be done
                ! other codes imply cannot continue with fit/simulation
                retcod = 2
            endif
        elseif (.not. quit ) then
            call fitot0(1)
            call fitone(retcod, ndiver)
            call fitot0(2)
        endif
        
        return
    end subroutine solfit

    !-----------------------------------------------------------------------
    
    subroutine fitone(retcod,ndiver)
        use msvars
        use msutil
        use msnwut
        use mssolve
        use msfito
        !use svd_anal
        
        !     Solves the model for current period jc with constraints
        !     on some endogenous variables.
        !     Uses Constant Adjustments to achieve targeted variables.
        !     Currently only possible with method .eq. 'N' (Newton) (nfb<>0)
        !     Note: experiments with Gauss-Seidel total failure.
        
        !     vector curvars(*) contains current unconstrained solution
        
        !     Called after solone if one or several "fit" variables
        !     have been given values.
        !     Minimum residuals to that effect are stored in CA.
        !     The solution is stored in Z.
        
        !     retcod <> 0 impies an error has occurred
        !     retcod 1 : simulation not possible
        !     retcod 2 : simulation stopped
        
        integer, intent(inout) :: ndiver
        integer, intent(inout) ::  retcod
        
        !     constants for relative conergence
        !                   absolute convergence
        !                   residual threshold
        
        real(kind = ISIS_RKIND)  :: CVGREL, RESTHD
        real(kind = SOLVE_RKIND) ::    Qzero
        parameter(CVGREL = 0.95_SOLVE_RKIND)
        parameter(RESTHD = 2.0_SOLVE_RKIND )
        parameter(Qzero = 0.0_SOLVE_RKIND)
        
        logical ::  deval,fcvgd,prihdr
        integer ::  i
        integer ::  fiscod,fiter,wmxidx,wmxtyp,djcnt
        integer ::  xcod
        real(kind = ISIS_RKIND) :: delwmx, dlwmxp, dcond !, svd_tol
        real(kind = ISIS_RKIND), dimension(:,:), allocatable :: dj_copy
        ! integer :: svd_err
        logical :: memory_error

        !     fiscod is fit iteration status
        !        0    continue
        !        1    absolute convergence of fit targets
        !        2    too many iterations
        !        3    cannot locate better point than previous
        
        fiscod = 0
        fiter  = 0
        fcvgd  = .false.
        prihdr = opts%fit%prica .or. (.not. opts%fit%supsot)
        djcnt  = 0
        
        if (allocated(dj_copy)) deallocate(dj_copy)
        
        ! compute discrepancies delw, and largest delw
        call mkdelw(delwmx, curvars, wmxidx, wmxtyp)
        if (opts%fit%repopt == FITREP_FULLREP) then
            call fitot4(fiter,.false.,Qzero,delwmx,Qzero,wmxidx, wmxtyp,.true.)
        endif
        
        if (delwmx <= opts%fit%cvgabs .and. is_square) then
            fiscod = 1
            goto 900
        endif
        
        if (mdl%nfb /= 0 .and. .not. matrix ) then
            ! simultaneous model had no newton jacobian available
            ! generate jacobian
            ! may happen if start is a solution
            ! fbval,fbfun have already been prepared
            call msgjac(xcod, 0)
            if( xcod .ne. 0 ) goto 9000
            matrix = .true.
        endif
        
        deval  = .true.
        
        do
            fiter = fiter + 1
        
            if (deval ) then
        
               !  generate jacobian of fit targets wrt residuals
               !  !!!!      Warning
               !  The jacobian is the transposed matrix of what is documented
               !  in the separate mathematical documentation
               !  It is easier to program the whole thing that way.
       
               !  I have tried to put in correct comments about
               !  what is happening
               !  The array Dj should be the transpose of D
               !  in the mathematical paper.
        
               call mkdjac(xcod, fiter, memory_error)
               if (memory_error) then
                   retcod = 2
                   goto 9999
               endif
        
               if( xcod .ne. 0 ) goto 9000
        
               djcnt = djcnt + 1
        
               ! compute D*u = trans(Dj)*u
               if (.not. is_square) call mkdu0(ca, delu)
        
        !       if (svdtest_tol_fit >= 0 .or. svdtest) then
        !            Save a copy of matrix dj for the svdtest in cause of problems
        !           if (.not. allocated(dj_copy)) then
        !               allocate(dj_copy(nu, nw), stat = stat)
        !               if (stat /= 0) then
        !                   call fitot15
        !                   retcod = 2
        !                   goto 9999
        !               endif
        !           endif
        !           dj_copy = dj(:nu, :nw)
        !       endif
        
               ! QR factorize Dj
               call mkdqr(dcond, opts%fit%nochkjac, xcod)
        
        !       if ((dcond <= svdtest_tol_fit) .or. (xcod .ne. 0 .and. svdtest)) then
        !               svd_tol = max(svdtest_tol_fit, sqrt(Rmeps))
        !               call svd_analysis(dj_copy, nu,  nw, numw, numu, &
        !&                               .true., svd_tol, svd_err)
        !               if (svd_err /= 0) then
        !                   retcod = 2
        !                   goto 9999
        !               endif
        !       endif
               if (xcod /= 0) goto 9000
            endif
        
            ! retain largest delw
        
            dlwmxp = delwmx
        
            ! calculate the rhs vector (use delu as work space)
            call mkb(deval)
        
            ! estimate required (descaled) addition to residuals.
            call mkdelu
        
            ! Save old residuals and solution
            do i = 1, nu
               resold(i) = ca(numu(i))
            end do
            zsav(:mdl%nrv) = curvars(:mdl%nrv)
        
            ! Update residuals with de-scaled addition.
            if (deval .and. .not. is_square) then
                ! delu contains new residuals
                do i = 1, nu
                    ca(numu(i)) = delu(i)
                end do
            else
                do i = 1, nu
                    ca(numu(i)) = ca(numu(i)) + delu(i)
                end do
            endif
        
            if (opts%fit%prica) then
                call fitoca(delu,numu,nu,fiter)
            endif
        
            ! new solution and new discrepancies.
        
            call solone(xcod, ndiver)
            if(xcod .ne. 0) goto 9000
        
            ! save delw in case needed for reset
            dddelw(:nw) = delw(:nw)
        
            call mkdelw(delwmx, curvars, wmxidx, wmxtyp)
            if (opts%fit%repopt == FITREP_FULLREP) then
                call fitot4(fiter,deval,dcond,delwmx,dlwmxp,wmxidx, &
                            wmxtyp, prihdr .or. (deval .and. opts%fit%prijac))
            endif
        
            if (delwmx <= opts%fit%cvgabs) then
                ! absolute convergence
                fiscod = 1
            elseif ((delwmx > CVGREL * dlwmxp) .and. &
        &           (is_square .or. fiter > 1)) then
        
                ! if relative convergence is poor undo step.
                ! for non square jacobian then at least 1 
                ! iteration should be performed before the step is undone
        
                do i = 1, nu
                   ca(numu(i)) = resold(i)
                end do
                curvars(:mdl%nrv) = zsav(:mdl%nrv)
                delw(:nw) = dddelw(:nw)
        
                delwmx = dlwmxp
                if (deval .or. fiter >= opts%fit%maxiter) then
                    ! cannot locate a better point
                   fiscod = 3
                else 
                   ! try with fresh jacobian
                   deval = .true.
                endif
            elseif (fiter >= opts%fit%maxiter) then
                !  too many iterations : no convergence
                fiscod = 2
            else 
                ! if slow convergence then generate new jacobian
                deval = delwmx > opts%fit%mkdcrt * dlwmxp
            endif
        
            if (fiscod /= 0 ) exit
        
        end do
        
        900 continue
        
        if (fiscod == 3) then
            call fitot5(CVGREL)
        elseif (fiscod == 1) then
           fcvgd = .true.
        endif
        if (opts%fit%warnca) then
            call msfuck(ca)
        endif
        call fitot9(fiter,fcvgd,djcnt,opts%fit%maxiter)
        if (.not. fcvgd) then
            retcod = 2
            goto 9999
        endif
        
        goto 9999
        
        ! error returns
        
        9000 retcod = xcod
        
        9999 if (allocated(dj_copy)) deallocate(dj_copy)
        return
    
    end subroutine fitone
    
    !-----------------------------------------------------------------------
    
    subroutine mkdelw(delwmx, z, wmxidx, wmxtyp)
    
    !     computes discrepancies delw in "fit"variables and
    !     largest discrepancy delwmx.
    !     corresponding variable number in wmxidx
    !     type of discrepancy in wmxtyp : 1 for absolute 2 for relative
    !     0 for wmx... implies all zero !!
    
    
    real(kind = SOLVE_RKIND), parameter :: Qone = 1.0_SOLVE_RKIND
    real(kind = SOLVE_RKIND) :: maxdif, dif
    
    real(kind = SOLVE_RKIND), intent(out) :: delwmx
    real(kind = SOLVE_RKIND), intent(in)  :: z(*)
    integer, intent(out) ::  wmxidx,wmxtyp
    
    integer :: i, wtyp
    
    maxdif = 0.
    wmxtyp = 0
    wmxidx = 0
    
    do i = 1, nw
       delw(i) = w(i) - z(numw(i))
       dif     = abs(delw(i))
       wtyp    = 1
       if (abs(w(i)) > Qone) then
          dif = dif / abs(w(i))
          wtyp = 2
       endif
       if (dif > maxdif ) then
          maxdif = dif
          wmxtyp = wtyp
          wmxidx = numw(i)
       endif
    end do
    
    delwmx = maxdif
    
    return
    end subroutine mkdelw
    
    !-----------------------------------------------------------------------
    
    subroutine mkdelu
        ! solve Dj * delu = b  for delu
        ! delu determined as minimum norm solution
        use msvars
        use liqrmn
     
        integer :: i
    
        delu(:nw) = b(:nw)
    
        call qrmn(dj, nu_max, nu, nw, djtau, delu, work_fit, lwork_fit)
    
        ! delu is scaled ==> unscale
        do i = 1, nu
            delu(i) = delu(i) * mws%rmsu(numr(i))
        enddo
    
        if (scale_u) delu(:nu) = delu(:nu) * u_scale(:nu)
    
        return
    end subroutine mkdelu
    
    !-----------------------------------------------------------------------
    
    subroutine mkdu0(ca, work)
        use nuv
        real(kind = SOLVE_RKIND), intent(in)  :: ca(*)
        real(kind = SOLVE_RKIND), intent(out) :: work(*)
    
        ! calculate D u_0 = trans(Dj) * scaled residuals
    
        integer :: i
    
        ! calculate scaled residuals
        do i = 1, nu
            work(i) = ca(numu(i)) / mws%rmsu(numr(i))
        enddo
    
        ! calculate D *u. (N.B. dj = trans(D))
        do i = 1, nw
            du0(i) = ddot(int(nu, ISIS_IKIND), dj(1,i), 1, work(1), 1)
        enddo
    
    end subroutine mkdu0
    
    !-----------------------------------------------------------------------
    
    subroutine mkb(deval)
        use nuv
        logical, intent(in)  :: deval
    
        ! Computes the rhs vector b for the underdetermined system
        ! of the form D * x = b.
        ! If deval = .true., then D is a fresh jacobian calculated for
        ! u = u0.
        ! Depending on deval, the equation D * x = b is given by:
        !   deval      equations
        !  .true.      D * u         = D * u_0 + delw
        !  .false.     D * (u - u_0) =           delw
        !
        ! For square systems (nu = nw), equation D * u = D * u_0 +
        ! delw can be simplified by D * (u - u_0) = delw.
        !
    
        ! If scale_w, then delw should be scaled with w_scale.
        if (scale_w) then
            b(:nw) = delw(:nw) * w_scale(:nw)
        else
            b(:nw) = delw(:nw)
        endif
    
        if (.not. is_square .and. deval) then
            ! add D * u_0
            b(:nw) = b(:nw) + du0(:nw)
        endif
    
        return
    end subroutine mkb
    
    !-----------------------------------------------------------------------
    
    subroutine mkdjac(xcod, fiter, memory_error)
    use msvars
    use msutil
    use nuv
    use mssolve
    use msnwut
    use scalemat
    use msfito
    
    ! Estimates the jacobian dj = trans(D) of the "fit"variables wrt. the
    ! residuals by means of single newton steps for small,
    ! scaled differences in the residuals.
    
    
    integer, intent(out) :: xcod
    integer, intent(in)  :: fiter
    logical, intent(out) :: memory_error
    
    real(kind = ISIS_RKIND) :: oldca
    real(kind = ISIS_RKIND), parameter :: RMSDEL = 0.1_SOLVE_RKIND
    real(kind = ISIS_RKIND) :: t, mat_norm, rowcnd, colcnd, amax
    integer(kind = LAPACK_IKIND) :: info
    
    integer ::  i, j, ires, idum, itr0, matitr, stat

    itr0 = 0
    
    
    !     RMSDEL is constant for calculation of derivative
    
    memory_error = .false.
    
    !     allocate fit matrix Dj.
    if (.not. allocated(dj)) then
        allocate(dj(nu_max, mws%fit_targets%var_count), stat = stat)
        if (opts%fit%scale_method /= SCALE_NONE .and. stat == 0) then
            allocate(w_scale(mws%fit_targets%var_count), stat = stat)
        endif
        if (opts%fit%scale_method == SCALE_BOTH .and. stat == 0) then
            allocate(u_scale(nu_max), stat = stat)
        endif
        memory_error = stat /= 0
        if (memory_error) then
            call fitot14
            return
        endif
    endif
    
    zsav(:mdl%nrv) = curvars(:mdl%nrv)
    
    if (opts%fit%accurate_jac) itr0 = itrtot
    
    do i = 1, nu
    
        ires     = numu(i)
        oldca    = ca(ires)
    
        ca(ires) = oldca + RMSDEL * mws%rmsu(numr(i))
    
        if (opts%fit%accurate_jac) then
            call solone(xcod, idum)
        else
            ! make a single newton pass through model
            call mdlpas(xcod)
        endif
        if (xcod /= 0) then
            call fitot13
        endif
    
        ! store changes in "fit"variables (descaled) in dj, and
        ! restore curvars and ca.
    
        ! dj[i,j] is derivative of target numw(j) wrt to residual numu(i)
        ! Warning:  this the transposed of the matrix D in mathematical paper.
    
        do j = 1, nw
            dj(i,j) = (curvars(numw(j)) - zsav(numw(j))) / RMSDEL
        enddo
    
        curvars(:mdl%nrv) = zsav(:mdl%nrv)
    
        ca(ires) = oldca
    
    end do
    
    if (opts%fit%accurate_jac) then
        matitr = itrtot - itr0
        call fitot12(matitr, nu)
        itrtot = itrtot - matitr
    endif
    
    ! output the transpose of the dj matrix
    if (opts%fit%prijac) then
        call fitodj(dj, fiter, numw, numu, nw, nu, nu_max)
    endif
    
    ! scale the matrix
    if (opts%fit%scale_method == SCALE_BOTH .and. is_square) then
        call dgeequ(nu, nw, dj, nu_max, u_scale, w_scale, rowcnd, colcnd,  &
             amax, info)
        scale_w = colcnd < 0.1 .or. rowcnd < 0.1
        scale_u = scale_w
    else if (opts%fit%scale_method /= SCALE_NONE) then
        call dgeequ_col(nu, nw, dj, nu_max, w_scale, colcnd, amax, info)
        scale_w = colcnd < 0.1
        scale_u = .false.
    else
        scale_w = .false.
        scale_u = .false.
    endif
    
    if (scale_w) then
        do j = 1, nw
            do i = 1, nu
                dj(i, j) = dj(i, j) * w_scale(j)
            end do
        end do
    endif
    if (scale_u) then
        do j = 1, nw
            do i = 1, nu
                dj(i, j) = dj(i, j) * u_scale(i)
            end do
        end do
    endif
    
    !     check for (almost) zero columns in dj matrix
    
    !     calculate 1-norm of matrix
    mat_norm = 0
    do j = 1, nw
        mat_norm = max(dasum(int(nu, ISIS_IKIND), dj(:, j), 1), mat_norm)
    enddo
    
    if (nuifna(mat_norm)) then
        ! matrix has invalid numbers
        call fitot10(fiter)
        do j = 1, nw
            t = dasum(int(nu, ISIS_IKIND), dj(:, j), 1)
            if (nuifna(t)) call fitotc_invalid(numw(j))
         enddo
    else
        ! determine which columns are (almost) zero
        do j = 1, nw
            t = dasum(int(nu, ISIS_IKIND), dj(:, j), 1)
            if (t <= mat_norm * sqrt(Rmeps) ) then
                call fitotc(numw(j))
            endif
        enddo
    endif
    
    
    return
    end subroutine mkdjac
    
    !-----------------------------------------------------------------------
    
    subroutine mkdqr(dcond, nochkjac, xcod)
    use liqrco
    use msfito
     
    !     compute QR factorization of Dj
    !     == QR of trans(D) where D is the jacobian defined in mathematical paper.
     
    use msvars
    real(kind = SOLVE_RKIND), intent(out) :: dcond
    logical, intent(in) :: nochkjac
    integer, intent(out) :: xcod
    
    integer ::  ier
    logical ::  quit
    
    xcod = 0
    
    !     QR decomposition of Fit jacobian
    !     estimate inverse condition of R ==> inverse condition of jacobian
    call qrco(dj, nu_max, nu, nw, djtau, dcond, work_fit, lwork_fit)
    
    if (nochkjac) then
    
        !  no strict and correct checking of square jacobian
        !  same test as in msnwqr (Newton with QR)
        if (Rone + dcond .eq. Rone) then
           ier = 2
        else
           ier = 0
        endif
    
    else
    
         !! if comparison with sqrt(Rmeps) changes then CHANGE fitot8 !!!
         if (dcond < sqrt(Rmeps) ) then
             ier = 1
         else
             ier = 0
         endif
    
    endif
    
    if (ier /= 0) then
        call fitot8(ier, dcond)
        quit = .true.
        if (quit) then
            xcod = 1
        endif
    endif
    
    return
    end subroutine mkdqr
    
    !-----------------------------------------------------------------------
    
    subroutine mdlpas(xcod)
    use msvars
    use msnwut
    use msfbvl
    
    integer, intent(out) ::  xcod
    
    integer :: j
    
    !     make 1 complete newton pass through model (nfb <> 0)
    
    !     prologue.
    
    call msprlg(xcod)
    if( xcod .ne. 0 ) return
    
    if (mdl%nfb .ne. 0 ) then
    
    !        first solve
    !        initial feedback values in fbval
    
       do j = 1, mdl%nfb
          fbval(j) = curvars(mdl%numfb(j))
       enddo
    
       call msloop(xcod)
       if( xcod .ne. 0 ) return
    
    !        prepare for newton step
    !        compute function values
    !        and execute a single newton step.
    
       do j = 1, mdl%nfb
          fbfun(j) = fbval(j) - curvars(mdl%numfb(j))
       enddo
    
       call msgstp
    
    !        second solve.
    
       call msloop(xcod)
       if( xcod .ne. 0 ) return
    
    !        epilogue
    
       call mseplg(xcod)
       if( xcod .ne. 0 ) return
    
    endif
    
    return
    end subroutine mdlpas
    
    !-----------------------------------------------------------------------
    
    subroutine msfuck(u)
    use msfito
    
    ! check relative size of residuals (in u) and
    ! if required give warning message for too big CA's
    
    
    real(kind = SOLVE_RKIND), intent(in) :: u(*)
    
    real(kind = SOLVE_RKIND) :: ucrit
    real(kind = SOLVE_RKIND), parameter :: utbig = 2_SOLVE_RKIND
    
    integer :: i, nbig
    
    nbig = 0
    do i = 1, nu
       ucrit = abs(u(numu(i))) / mws%rmsu(numr(i))
       if (ucrit > utbig) then
           nbig = nbig + 1
           call fitot6(numu(i), ucrit)
       endif
    end do
    if (nbig > 0) call fitot7(nbig)
    
    return
    end subroutine msfuck
    
    !-----------------------------------------------------------------------
    
    subroutine wfinit(jt , fiterr, quit)
    use msvars
    use msfito
    integer, intent(in)  :: jt
    integer, intent(out) :: fiterr
    logical, intent(out) :: quit
    
    !     initialize fit targets in array w from fitvars
    !     and also check status of fit CA/targets
    !     if fix procedure is active then also numu and numr will be
    !     updated, because the current period may contain less
    !     endogenous CAs.
    
    !     fiterr return codes
    !      0    all ok Fit may be done
    !      1    inconsistent/missing/invalid fit targets/CA detected
    !      2    nothing to fit in this period
    !      3    no fit CA's available (nu = 0)
    !      4    not enough CA's (nu < nw)
    !      5    only Newton method allowed for Fit
    
    !     fiterr <> 0 implies NO Fit
    
    !     quit is set to true if the simulation should be stopped
    
    integer :: nonval, i, nu_prev
    
    type(mdl_variable), pointer :: fit_tar
    real(kind = MWS_RKIND) :: value
    logical :: changed
    
    quit = .false.
    fiterr = 0
    nonval = 0
    
    if (opts%method /= 'N' .and. opts%method /= 'B' .and. opts%method /= 'Q') then
       if (mdl%nfb /= 0 ) then
           fiterr = 5
           return
       endif
    endif
    
    if (fix_active) then
        ! Reset arrays for exogenous fit CAs.
        ! If the fix procedure is active, then some fit CAs
        ! can be endogenous for the current period.
        nu_prev = nu
        call mknumu(mws, numu, numr, nu)
        if (nu /= nu_prev) then
             changed = .true.
        else
             changed = .false.
             do i = 1, nu
                if (numu_prev(i) /= numu(i)) then
                    changed = .true.
                    exit
                endif
             end do
        endif
        if (changed .and. nu > 0) then
            numu_prev(:nu) = numu(:nu)
            call fitonu_fix(nu, numu)
        endif
    endif
    
    ! setup numw array and check that all fit targets are endogenous
    
    nw = 0
    fit_tar => mws%fit_targets%first
    do
       value = get_mdl_var_value(fit_tar, jt)
       if (.not. nuifna(value)) then
           nw = nw + 1
           numw(nw) = fit_tar%var_index
           w(nw) = value
           if (.not. mdl%lik(fit_tar%var_index)) then
               call fitotb(fit_tar%var_index)
               nonval = nonval + 1
            endif
        endif
        fit_tar => fit_tar%next
        if (.not. associated(fit_tar)) exit
    end do
    
    if (nonval > 0) then
       call fitot2(nonval)
       quit = opts%erropt == ERROPT_STOP
       fiterr = 1
    else if( nw .eq. 0 ) then
       fiterr = 2
    else if( nu .eq. 0 ) then
       fiterr = 3
    else if( nu .lt. nw ) then
       fiterr = 4
    endif
    
    is_square = nu == nw
    
    return
    end subroutine wfinit
    
    !-----------------------------------------------------------------------
    
    subroutine prefit
    
    ! called just before execution of a solve model in period jt
    ! and period jt has fit targets (fitone will be called after solve)
    ! set any global for solve model
    
    integer ::  i
    
    if (opts%fit%newjac) then
       njcmat = 0
       matrix = .false.
    endif
    
    if (opts%fit%zeroca) then
       do i = 1, nu
          ca(numu(i)) = Rzero
       end do
    endif
    
    return
    end subroutine prefit
    
    !-----------------------------------------------------------------------
    
    subroutine fitini
        nu     = 0
        nw     = 0
    return
    end subroutine fitini
    
    !-----------------------------------------------------------------------
    
    subroutine msgjac(retcod, itr)
     
    !     general jacobian routine
    !     for use by procedures wich should not know what decomposition is used
     
    use msbrdn
    !use msnwto
    !use msnwqr
    integer :: retcod,itr
    
    if (opts%method == 'N' ) then
        !call msnjac(retcod, itr)
        continue
    elseif (opts%method == 'Q') then
        !call msqjac(retcod, itr)
        continue
    elseif (opts%method == 'B') then
       call msbjac(retcod, itr)
    endif
    
    return
    end subroutine msgjac
    
    !-----------------------------------------------------------------------
    
    subroutine msgstp
    use msbrdn
    !use msnwto
    !use msnwqr
    
    ! general calculate newton or broyden step
    
    if (opts%method == 'N' ) then
        ! pivoted LU factorization
        !call msnstp
        continue
    elseif (opts%method == 'Q') then
        ! QR decomposition
        !call msqstp
    elseif (opts%method == 'B') then
        !  expanded QR factorization (no pivots)
        call msbstp
    endif
    
    return
    end subroutine msgstp
    
    !-----------------------------------------------------------------------
    
    integer function get_lwork_fit(nu, nw)
        use liqrco
        use liqrmn
        integer(kind = LI_IKIND),  intent(in) :: nu, nw
    
        ! Function get_lwork_fit returns the optimal workspace for the
        ! linear algebra routines for fit procedure.
    
        real(kind = SOLVE_RKIND) ::  rdum(1,1), rwork(1)
        integer(kind = SOLVE_IKIND) lwrk1, lwrk2
    
        call qrco(rdum, nu, nu, nw, rdum(:, 1), rdum(1, 1), rwork, -1_LI_IKIND)
        lwrk1 = nint(rwork(1))
        call qrmn(rdum, nu, nu, nw, rdum(:, 1), rdum(:, 1), rwork, -1_LI_IKIND)
        lwrk2 = nint(rwork(1))
        get_lwork_fit = max(lwrk1, lwrk2)
        return
    end function get_lwork_fit

end module msfitm
