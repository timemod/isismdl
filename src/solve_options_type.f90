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


    ! options for the Fair-Taylor rational expectations algorithm
    type ratex_options
        integer(kind = ISIS_IKIND) :: njacpd
        integer(kind = ISIS_IKIND) :: ratrep_type
        integer(kind = ISIS_IKIND) :: ratrep
        integer(kind = ISIS_IKIND) :: ratrepfull
        real(kind = ISIS_RKIND)    :: xrelax
        integer(kind = ISIS_IKIND) :: mratex
        real(kind = ISIS_RKIND)    :: xtfac
    end type ratex_options

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
        logical            :: uplead
        real(kind = ISIS_RKIND) :: bktmax
        real(kind = ISIS_RKIND) :: cstpbk
        real(kind = ISIS_RKIND) :: cnmtrx
        real(kind = ISIS_RKIND) :: crelax
        real(kind = ISIS_RKIND) :: grelax
        integer(kind = ISIS_IKIND) :: maxit
        integer(kind = ISIS_IKIND) :: maxmat
        logical(kind = ISIS_IKIND) :: arithmetic_crit
        real(kind = ISIS_RKIND) :: rlxmin
        real(kind = ISIS_RKIND) :: rlxmax
        real(kind = ISIS_RKIND) :: rlxspeed
        integer(kind = ISIS_IKIND) :: scalmd
        real(kind = ISIS_RKIND) :: svdtest_tol
        
        ! output options
        integer(kind = ISIS_IKIND) :: ioutsm
        integer(kind = ISIS_IKIND) :: iendsm
        logical(kind = ISIS_IKIND) :: suptst

        type(ratex_options) :: ratex
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
        !options%maxit  = 2
        options%maxmat = 10
        options%arithmetic_crit = .false.
        options%rlxmin = 0.05_ISIS_RKIND
        options%rlxmax = 1.0_ISIS_RKIND
        options%rlxspeed = 0.5_ISIS_RKIND
        options%scalmd = NORMBAL
    
        ! output options
        options%ioutsm = 2
        options%iendsm = 1
        options%suptst = .true.

        ! ratex (Fair-Taylor) options
        options%ratex%njacpd = 0
        options%ratex%ratrep_type = RATREP_ITER
        options%ratex%ratrep = 1
        options%ratex%ratrepfull = -1
        options%ratex%xrelax = 1.0_ISIS_RKIND
        options%ratex%mratex = 10
        options%ratex%xtfac = 10.0_ISIS_RKIND

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

end module solve_options_type
