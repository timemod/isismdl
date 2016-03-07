module period_type
    use model_params

    type period 
        integer(kind = MC_IKIND) :: year
        integer(kind = MC_IKIND) :: subperiod
        integer(kind = MC_IKIND) :: frequency
    end type period

    contains

        !
        ! this function returns the number of (sub)periods between
        ! two periods
        ! 
        integer function get_period_count(period1, period2) 
            type(period), intent(in) :: period1, period2
            integer :: subperiods1, subperiods2
            subperiods1 = period1%year * period1%frequency + period1%subperiod
            subperiods2 = period2%year * period2%frequency + period2%subperiod
            get_period_count = subperiods2 - subperiods1 + 1
        end function get_period_count

end module period_type
