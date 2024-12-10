module vars

    implicit none

    private
    public :: vars_zonal

    contains


    subroutine vars_zonal(unit, znum)
        integer, intent(in) :: unit
        integer, intent(in) :: znum

        write(unit,'(A)')      'VARS 5'
        write(unit,'(A,I0,A)') 'u  ', znum, ' 99 Zonal Wind [m/s]'
        write(unit,'(A,I0,A)') 'v  ', znum, ' 99 Meridional Wind [m/s]'
        write(unit,'(A,I0,A)') 'pt ', znum, ' 99 Potential Temperature [K]'
        write(unit,'(A,I0,A)') 't  ', znum, ' 99 Temperature Dagger derived from pt [K]'

        write(unit,'(A)')      'ENDVARS'
        write(unit,*)

    end subroutine vars_zonal


end module vars

