module solve_options_type
    use kinds

    ! erropt (these values should agree with the ordering of
    ! the ERROPT_OPTIONS in solve_options.c (first option should be 1)!
    integer, parameter :: ERROPT_STOP  = 1
    integer, parameter :: ERROPT_CONT  = 2
    integer, parameter :: ERROPT_SILENT = 3

    ! reopt option
    integer, parameter :: REP_NONE    = 1    ! no report at all
    integer, parameter :: REP_MINIMAL = 2
    integer, parameter :: REP_PERIOD  = 3
    integer, parameter :: REP_FULLREP = 4

    ! type of fit report
    integer, parameter :: FITREP_MINIMAL   = 1
    integer, parameter :: FITREP_FULLREP   = 2

    ! type of ratex report
    integer, parameter :: RATREP_MINIMAL      = 1
    integer, parameter :: RATREP_ITER         = 2
    integer, parameter :: RATREP_FULLREP      = 3

    ! scale method for the fit procedures
    ! NOTE: the FIT_SCALE-parameters should agree with the ordering of
    ! in array fit_scale_method in fit_options.c
    integer, parameter :: FIT_SCALE_NONE  = 1
    integer, parameter :: FIT_SCALE_ROW   = 2
    integer, parameter :: FIT_SCALE_BOTH  = 3

    ! NOTE: the ordering of MODES and START_OPTIONS should agree with the
    ! ordering of the options in solve_options.c!!!! */
    character(len = 1), dimension(6), parameter :: MODES = &
                             (/ 'D', 'X', 'R', 'B', 'S', '?' /)
    character(len = 1), dimension(4), parameter :: START_OPTIONS = &
                             (/ 'P', 'C', 'Q', 'D'/)

    ! options for the fit procedfure
    type fit_options
        integer(kind = ISIS_IKIND) :: maxiter
        real(kind = ISIS_RKIND)    :: cvgabs
        real(kind = ISIS_RKIND)    :: mkdcrt
        real(kind = ISIS_RKIND)    :: cvgrel
        integer(kind = ISIS_IKIND) :: scale_method
        logical(kind = ISIS_IKIND) :: prica
        logical(kind = ISIS_IKIND) :: prijac
        logical(kind = ISIS_IKIND) :: zeroca
        logical(kind = ISIS_IKIND) :: warnca
        logical(kind = ISIS_IKIND) :: newjac
        logical(kind = ISIS_IKIND) :: supsot
        logical(kind = ISIS_IKIND) :: chkjac
        logical(kind = ISIS_IKIND) :: accurate_jac
        integer(kind = ISIS_IKIND) :: repopt
        real(kind = ISIS_RKIND)    :: svdtest_tol
        logical(kind = ISIS_IKIND) :: zealous
        logical(kind = ISIS_IKIND) :: warn_zero_row
        logical(kind = ISIS_IKIND) :: warn_zero_col
    end type fit_options

    type solve_options
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

        ! debug options
        logical(kind = ISIS_IKIND) :: priter
        logical(kind = ISIS_IKIND) :: prexen
        logical(kind = ISIS_IKIND) :: jacprt
        logical(kind = ISIS_IKIND) :: suptst
        logical(kind = ISIS_IKIND) :: xsuptt
        logical(kind = ISIS_IKIND) :: prscal

        ! error options
        integer(kind = ISIS_IKIND) :: erropt

        ! output options
        integer(kind = ISIS_IKIND) :: repopt

        ! ratex options
        logical(kind = ISIS_IKIND) :: uplead
        integer(kind = ISIS_IKIND) :: njacpd
        integer(kind = ISIS_IKIND) :: ratrepopt
        integer(kind = ISIS_IKIND) :: ratreport_rep
        integer(kind = ISIS_IKIND) :: ratfullreport_rep
        real(kind = ISIS_RKIND)    :: xrelax
        integer(kind = ISIS_IKIND) :: mratex
        real(kind = ISIS_RKIND)    :: xtfac

        type(fit_options) :: fit
    end type solve_options

contains

    subroutine set_default_options(options)
        use model_type
        use scalemat
        use nucnst, only : Rmeps
        use nuna, only : NA_INTEGER
        type(solve_options), intent(out) :: options
        options%mode = "?"
        options%method = "?"
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
        options%svdtest_tol = -1.0_ISIS_RKIND;

        ! debug options
        options%priter = .false.
        options%prexen = .false.
        options%jacprt = .false.
        options%suptst = .true.
        options%xsuptt = .true.
        options%prscal = .false.

        ! error options
        options%erropt = ERROPT_STOP

        ! output options
        options%repopt = REP_PERIOD

        ! ratex (Fair-Taylor) options
        options%ratrepopt = RATREP_ITER
        options%ratreport_rep = 1
        options%ratfullreport_rep = NA_INTEGER
        options%xrelax = 1.0_ISIS_RKIND
        options%mratex = 10
        options%xtfac = 10.0_ISIS_RKIND

        ! fit options
        options%fit%maxiter = 10
        options%fit%cvgabs = 100 * sqrt(Rmeps)
        options%fit%mkdcrt =  0.5_ISIS_RKIND
        options%fit%cvgrel =  0.95_ISIS_RKIND
        options%fit%scale_method = FIT_SCALE_ROW
        options%fit%accurate_jac = .true.
        options%fit%prica  = .false.
        options%fit%prijac = .false.
        options%fit%zeroca = .false.
        options%fit%warnca = .true.
        options%fit%newjac = .true.
        options%fit%supsot = .true.
        options%fit%chkjac = .true.
        options%fit%repopt = FITREP_FULLREP
        options%fit%svdtest_tol = -1.0_ISIS_RKIND
        options%fit%zealous = .true.
        options%fit%warn_zero_row = .false.
        options%fit%warn_zero_col = .false.

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
        case ('?')
            get_mode_text = "model dependent"
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
