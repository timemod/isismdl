! initialise the modules
! this should be called only once, to initialise the modules

subroutine init_modules
    use nuna
    use mcedef
    
    ! initialis NA
    call init_na
    
    ! initialise operator definition
    call mceini
    
end subroutine init_modules
