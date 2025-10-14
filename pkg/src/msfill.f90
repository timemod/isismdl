module msfill

integer, parameter, private :: NO_REP     = 1  ! no report (silent)
integer, parameter, private :: MIN_REP    = 2  ! minimal report
integer, parameter, private :: PERIOD_REP = 3  ! report per period

contains
subroutine fill_mdl_data(jt1, jt2, report_type, idents_only)
    use msvars
    use msimot
    integer, intent(in) :: jt1, jt2, report_type
    logical, intent(in) :: idents_only

    ! fills missing values by running identities for period jt1 .. jt2
    ! skips inactive and behavioural equations

    ! walks through model in solution order
    ! and runs identities if the left hand side variable is missing

    integer ::  jtime, ngain, tgain, itnum
    logical ::  didfb

    ! initialize simulation output system
    call simot1

    tgain = 0

    do jtime = jt1, jt2
       itnum = 0
       do
          itnum = itnum + 1
          ngain = 0
          call msfilp(jtime, ngain, didfb, idents_only)
          if (report_type == PERIOD_REP) call filot1(ngain, jtime, itnum)
          tgain = tgain + ngain
          if (ngain == 0 .or. mdl%nfb == 0 .or. .not. didfb) exit
        end do

     end do
     if (report_type /= NO_REP) call filot2(tgain, idents_only)

     return
     end subroutine fill_mdl_data

     subroutine msfilp(jtime, ngain, didfb, idents_only)
        use mssneq
        integer, intent(in) :: jtime
        integer, intent(inout) :: ngain
        logical, intent(out) :: didfb
        logical, intent(in) :: idents_only

        ! fills missing values by running identities for period jtime
        ! skips inactive and behavioural equations

        ! walks through model in solution order
        ! and runs identities if the left hand side variable is missing

        integer, external ::  bysget
        character(len = 1) :: eqtype
        logical ::     error

        real(kind = SOLVE_RKIND) :: x, xresult, xca
        integer ::  i, iequ, lhsvar, ier, ca_index

        didfb = .false.
        
        do i = 1, mdl%neq
           iequ   = mdl%order(i)
           ! iequ is 0 if the equation has been deactived AND if the model has been reordered.
           if (iequ == 0) cycle  
           lhsvar = mdl%lhsnum(iequ)

           eqtype = char(bysget(mdl%etype, iequ))

           ! Equation type: uppercase for active equations and lowercase for inactive equations.
           ! ASCI with integer values > 96 are lowercase.
           if (ichar(eqtype) .gt. 96) cycle

           if (idents_only .and. eqtype /= 'I' .and. eqtype /= 'N') cycle

           ! possibly something to do
           ! calculate index in mws%mdl_data array
           ! if available entry is not missing do nothing
           call get_var_value(mws, lhsvar, jtime, x, error)

           if (.not. nuifna(x) .or. error) cycle

           if (eqtype == 'I') then
               call msisng(xresult, iequ, jtime, ier)
           elseif (eqtype == "N") then
               ! implicit identity (solve with newton)
               call msinwt(xresult, lhsvar, 0, iequ, jtime, ier)
           elseif (eqtype == 'B' .or. eqtype == "M") then
               ca_index = mdl%aci(lhsvar)
               xca = mws%constant_adjustments(ca_index, jtime)
               if (nuifna(xca)) then
                   xresult = xca
               elseif (eqtype == "B") then
                   ! normal behavourial equation
                   call msisng(xresult, iequ, jtime, ier)
                   xresult = xresult + xca
               else
                   ! implicit equation
                   call msinwt(xresult, lhsvar, ca_index, iequ, jtime, ier)
               endif
           endif

           if (ier /= 0 .and. ier /= 3 .and. ier /= 7) then
              ! no message for any kind of missing values
              call msverr(ier, jtime, iequ)
           endif

           ! store result; if not missing then
           ! a missing value has been replaced and therefore
           ! progress has been made

           if (.not. nuifna(xresult) ) then
               ngain = ngain + 1
               call set_var_value(mws, lhsvar, jtime, xresult, error)
               ! check for fill of feedback variable
               if ((i .ge. mdl%loope - mdl%nfb + 1) .and. &
                   (i .le. mdl%loope) ) then
                   didfb = .true.
               endif
           endif
        end do

        return
    end subroutine msfilp

    subroutine filot1(ngain, jtime, itnum)
        use msimot
        integer, intent(in) :: ngain, jtime, itnum

        call sjcstr(jtime)

        write(str,'(I10,2A,A,I4,A)') &
              ngain , ' Missing/invalid values replaced in period ', &
              perstr, '(iteration',itnum,')'

        call strout(O_OUTB)

        return
    end subroutine filot1

    subroutine filot2(tgain, idents_only)
        use msimot
        integer, intent(in) :: tgain
        logical, intent(in) :: idents_only

        integer, parameter :: MAX_INT_STR_LEN = 10
        character(len = MAX_INT_STR_LEN) :: istr
        character(len = 15) :: last_words

        if (idents_only) then
           last_words = " in identities"
        else
           last_words = ""
        endif

        if (tgain /= 0) then
            write(istr, '(I10)') tgain
            istr = adjustl(istr)
            write(str, '(4A)') 'Replaced a total of ', trim(istr), &
                               ' missing/invalid values', last_words
        else
            write(str, '(2A)') 'No missing/invalid values replaced', last_words
        endif

        call strout(O_OUTB)

        return
    end subroutine filot2

end module msfill
