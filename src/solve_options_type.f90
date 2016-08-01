module solve_options_type
    use kinds

    ! type of ratex report
    integer, parameter :: RATREP_MINIMAL      = 1
    integer, parameter :: RATREP_ITER         = 2
    integer, parameter :: RATREP_FULLREP      = 3
    integer, parameter :: RATREP_ITERSCRN     = 4
    integer, parameter :: RATREP_FULLREPSCRN  = 5

    ! scale method for the fit procedures
    integer, parameter :: FIT_SCALE_NONE  = 1
    integer, parameter :: FIT_SCALE_ROW   = 2
    integer, parameter :: FIT_SCALE_BOTH  = 3
    
    ! NOTE: the ordering of MODES and START_OPTIONS should agree with the
    ! ordering of the options in solve_options.c!!!! */
    character(len = 1), dimension(5), parameter :: MODES = &
                             (/ 'D', 'X', 'R', 'B', 'S' /)
    character(len = 1), dimension(4), parameter :: START_OPTIONS = &
                             (/ 'P', 'C', 'Q', 'D' /)


    ! options for the fit procedfure
    type fit_options
        real(kind = ISIS_RKIND)    :: cvgabs
        real(kind = ISIS_RKIND)    :: mkdcrt
        integer(kind = ISIS_IKIND) :: fitrmx
        integer(kind = ISIS_IKIND) :: repopt
        integer(kind = ISIS_IKIND) :: scale_method
        logical(kind = ISIS_IKIND) :: zeroca
        logical(kind = ISIS_IKIND) :: warnca
        logical(kind = ISIS_IKIND) :: accurate_jac
        logical(kind = ISIS_IKIND) :: newjac
        logical(kind = ISIS_IKIND) :: prijac
        logical(kind = ISIS_IKIND) :: nochkjac
    end type fit_options

    type solve_options
        logical :: dbgeqn
        character(len = 1) :: mode
        character(len = 1) :: method
        character(len = 1) :: start
        integer(kind = ISIS_IKIND) :: bktmax
        real(kind = ISIS_RKIND) :: cstpbk
        real(kind = ISIS_RKIND) :: cnmtrx
        real(kind = ISIS_RKIND) :: crelax
        real(kind = ISIS_RKIND) :: grelax
        integer(kind = ISIS_IKIND) :: maxit
        integer(kind = ISIS_IKIND) :: maxmat
        logical(kind = ISIS_IKIND) :: arith
        real(kind = ISIS_RKIND) :: rlxmin
        real(kind = ISIS_RKIND) :: rlxmax
        real(kind = ISIS_RKIND) :: rlxspeed
        integer(kind = ISIS_IKIND) :: scalmd
        real(kind = ISIS_RKIND) :: svdtest_tol
        
        ! output options
        integer(kind = ISIS_IKIND) :: ioutsm
        integer(kind = ISIS_IKIND) :: iendsm
        logical(kind = ISIS_IKIND) :: suptst

        ! ratex options
        logical(kind = ISIS_IKIND) :: uplead
        integer(kind = ISIS_IKIND) :: njacpd
        integer(kind = ISIS_IKIND) :: ratrep_type
        integer(kind = ISIS_IKIND) :: ratrep
        integer(kind = ISIS_IKIND) :: ratrepfull
        real(kind = ISIS_RKIND)    :: xrelax
        integer(kind = ISIS_IKIND) :: mratex
        real(kind = ISIS_RKIND)    :: xtfac

        type(fit_options) :: fit
    end type solve_options

contains
        
    subroutine set_default_options(mdl, options)
        use model_type
        use scalemat
        type(model), intent(in) :: mdl
        type(solve_options), intent(out) :: options

        options%dbgeqn = .false.
        if (has_endo_lead(mdl)) then
            options%mode = "X"
        else 
            options%mode = "D"
        endif
        if (mdl%nfb == 0) then
            options%method = 'G' ! Seidel
        else
            options%method = 'B' ! Broyden
        endif
        options%start = 'C'
        options%uplead = .false.
        options%bktmax = 5
        options%cstpbk = 1.3_ISIS_RKIND
        options%cnmtrx = 0.9_ISIS_RKIND
        options%crelax = 2.0_ISIS_RKIND
        options%grelax = 1.0_ISIS_RKIND
        options%maxit  = 50
        options%maxmat = 10
        options%arith = .false.
        options%rlxmin = 0.05_ISIS_RKIND
        options%rlxmax = 1.0_ISIS_RKIND
        options%rlxspeed = 0.5_ISIS_RKIND
        options%njacpd = 0
        options%scalmd = NORMBAL
    
        ! output options
        options%ioutsm = 2
        options%iendsm = 1
        options%suptst = .true.

        ! ratex (Fair-Taylor) options
        options%ratrep_type = RATREP_ITER
        options%ratrep = 1
        options%ratrepfull = -1
        options%xrelax = 1.0_ISIS_RKIND
        options%mratex = 10
        options%xtfac = 10.0_ISIS_RKIND

        ! fit options
        options%fit%cvgabs = 1e-7_ISIS_RKIND
        options%fit%mkdcrt =  0.5_ISIS_RKIND
        options%fit%scale_method = FIT_SCALE_ROW
        options%fit%accurate_jac = .false.
        options%fit%prijac = .false.
        options%fit%nochkjac = .false.
        options%fit%newjac = .true.
        options%fit%zeroca = .false.
        options%fit%warnca = .true.
        options%fit%fitrmx = 10
        options%fit%repopt = 2

    end subroutine set_default_options

    function get_mode_text(mode) 
        character(len = 21) :: get_mode_text
        character, intent(in) :: mode
        select case (mode) 
        case ('D')
            get_mode_text = "dynamic"
        case ('X')
            get_mode_text = "rational expectations"
        case ('R')
            get_mode_text = "residual check"
        case ('B')
            get_mode_text = "backward"
        case ('S')
            get_mode_text = "static"
        end select
    end function get_mode_text

    function get_start_text(start) 
        character(len = 24) :: get_start_text
        character, intent(in) :: start
        select case (start) 
        case ('P')
            get_start_text = "previous period"
        case ('C')
            get_start_text = "current period"
        case ('Q')
            get_start_text = "previous period if valid"
        case ('D')
            get_start_text = "current period if valid"
        end select
    end function get_start_text

    function get_arith_text(arith) 
        character(len = 10) :: get_arith_text
        logical, intent(in) :: arith
        
        if (arith) then
            get_arith_text = "arithmetic"
        else
            get_arith_text = "geometric"
        endif
    end function get_arith_text

end module solve_options_type
