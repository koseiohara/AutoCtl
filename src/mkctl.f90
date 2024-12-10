module mkctl

    implicit none

    private
    public :: auto_ctl

    contains


    subroutine auto_ctl(bin, title, options, xnum, xmin, xstep, ynum, ymin, ystep, znum, levels, tnum, tini, tstep, write_vars)
        character(*), intent(in) :: bin
        character(*), intent(in) :: title
        character(*), intent(in), optional :: options
        integer     , intent(in) :: xnum
        class(*)    , intent(in) :: xmin
        class(*)    , intent(in) :: xstep
        integer     , intent(in) :: ynum
        class(*)    , intent(in) :: ymin
        class(*)    , intent(in) :: ystep
        integer     , intent(in) :: znum
        class(*)    , intent(in) :: levels(znum)
        integer     , intent(in) :: tnum
        character(*), intent(in) :: tini
        character(*), intent(in) :: tstep

        integer :: ctl
        character(256) :: ctlname
        character(256) :: dset

        interface
            subroutine write_vars(unit, znum)
                integer, intent(in) :: unit
                integer, intent(in) :: znum
            end subroutine write_vars
        end interface


        call get_ctl_name(bin    , &  !! IN
                        & ctlname  )  !! OUT

        call get_dset(bin , &  !! IN
                    & dset  )  !! OUT

        open(newunit=ctl   , &
           & file=ctlname  , &
           & action='WRITE'  )

        write(ctl,'(A)') 'DSET ^' // trim(dset)
        write(ctl,'(A,x,A)') 'TITLE', trim(title)
        if (present(options)) then
            write(ctl,'(A,x,A,x)') 'OPTIONS', trim(options)
        endif
        write(ctl,'(A)') 'UNDEF 9.999E+20'
        write(ctl,*)

        !-----XDEF STATEMENT-----!
        write(ctl,'(A,x,I0,x,A,x)',ADVANCE='NO') 'XDEF', xnum, 'LINEAR'
        select type(xmin)
            type is (integer(4))
                write(ctl,'(I0,x)',ADVANCE='NO') xmin
            type is (integer(8))
                write(ctl,'(I0,x)',ADVANCE='NO') xmin
            type is (real(4))
                write(ctl,'(F0.5,x)',ADVANCE='NO') xmin
            type is (real(8))
                write(ctl,'(F0.5,x)',ADVANCE='NO') xmin
            type is (real(16))
                write(ctl,'(F0.5,x)',ADVANCE='NO') xmin
            class default
                write(*,'(A)') 'ERROR STOP'
                write(*,'(A)') 'Unsupported Data Type : xmin'
                ERROR STOP
        end select

        select type(xstep)
            type is (integer(4))
                write(ctl,'(I0)') xstep
            type is (integer(8))
                write(ctl,'(I0)') xstep
            type is (real(4))
                write(ctl,'(F0.5)') xstep
            type is (real(8))
                write(ctl,'(F0.5)') xstep
            type is (real(16))
                write(ctl,'(F0.5)') xstep
            class default
                write(*,'(A)') 'ERROR STOP'
                write(*,'(A)') 'Unsupported Data Type : xstep'
                ERROR STOP
        end select

        !-----YDEF STATEMENT-----!
        write(ctl,'(A,x,I0,x,A,x)',ADVANCE='NO') 'YDEF', ynum, 'LINEAR'
        select type(ymin)
            type is (integer(4))
                write(ctl,'(I0,x)',ADVANCE='NO') ymin
            type is (integer(8))
                write(ctl,'(I0,x)',ADVANCE='NO') ymin
            type is (real(4))
                write(ctl,'(F0.5,x)',ADVANCE='NO') ymin
            type is (real(8))
                write(ctl,'(F0.5,x)',ADVANCE='NO') ymin
            type is (real(16))
                write(ctl,'(F0.5,x)',ADVANCE='NO') ymin
            class default
                write(*,'(A)') 'ERROR STOP'
                write(*,'(A)') 'Unsupported Data Type : ymin'
                ERROR STOP
        end select

        select type(ystep)
            type is (integer(4))
                write(ctl,'(I0)') ystep
            type is (integer(8))
                write(ctl,'(I0)') ystep
            type is (real(4))
                write(ctl,'(F0.5)') ystep
            type is (real(8))
                write(ctl,'(F0.5)') ystep
            type is (real(16))
                write(ctl,'(F0.5)') ystep
            class default
                write(*,'(A)') 'ERROR STOP'
                write(*,'(A)') 'Unsupported Data Type : ystep'
                ERROR STOP
        end select

        !-----ZDEF STATEMENT-----!
        write(ctl,'(A,x,I0,x,A)') 'ZDEF', znum, 'LEVELS'
        select type(levels)
            type is (integer(4))
                write(ctl,'(*(I0,:,", "))') levels(1:znum)
            type is (integer(8))
                write(ctl,'(*(I0,:,", "))') levels(1:znum)
            type is (real(4))
                write(ctl,'(*(F0.3,:,", "))') levels(1:znum)
            type is (real(8))
                write(ctl,'(*(F0.3,:,", "))') levels(1:znum)
            type is (real(16))
                write(ctl,'(*(F0.3,:,", "))') levels(1:znum)
            class default
                write(*,'(A)') 'ERROR STOP'
                write(*,'(A)') 'Unsupported Data Type : levels'
                ERROR STOP
        end select

        !-----TDEF STATEMENT-----!
        write(ctl,'(A,x,I0,x,A,x,A)') 'TDEF', tnum, trim(tini), trim(tstep)
        write(ctl,*)

        call write_vars(ctl , &  !! IN
                      & znum  )  !! IN

        close(ctl)

    end subroutine auto_ctl


    subroutine get_ctl_name(binname, ctlname)
        character(*), intent(in)  :: binname
        character(*), intent(out) :: ctlname

        character(8), allocatable :: extension_list(:)
        character(8) :: extension
        integer      :: extension_kinds
        integer      :: where_period
        integer      :: i

        extension_list=['grd', 'bin', 'dat', 'raw']
        extension_kinds=size(extension_list)

        where_period = index(binname, '.', back=.TRUE.)
        extension    = trim(binname(where_period+1:))

        do i = 1, extension_kinds
            if (trim(extension) == trim(extension_list(i))) then
                ctlname = trim(binname(1:where_period-1)) // '.ctl'
                return
            endif
        enddo

        ctlname = trim(binname) // '.ctl'

    end subroutine get_ctl_name


    subroutine get_dset(binname, dset)
        character(*), intent(in)  :: binname
        character(*), intent(out) :: dset

        integer :: dir_range

        dir_range = index(binname, '/', back=.TRUE.)
        dset = trim(binname(dir_range+1:))

    end subroutine get_dset



end module mkctl

