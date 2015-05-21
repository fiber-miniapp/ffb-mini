      program main
        implicit none
        character(9) FILEIN
        parameter(FILEIN = 'PARMLES3X')
        call LES3X(FILEIN)
      end program
