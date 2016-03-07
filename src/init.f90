module init

    logical, save :: initialised = .false.
    
    contains

        subroutine init_modules
            use nuna
            use mcedef
    
            if (initialised) return
        
            ! initialis NA
            call init_na
    
            ! initialise operator definition
            call mceini
    
            initialised = .true.
    
        end subroutine init_modules

end module init
