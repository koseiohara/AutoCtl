program main
    use mkctl
    use vars
    implicit none

    call auto_ctl(bin='hoge/piyo/fuga.2024'   , &
                & title='fuga'          , &
                & options='yrev'        , &
                & xnum=145              , &
                & xmin=0                , &
                & xstep=1            , &
                & ynum=145              , &
                & ymin=-90             , &
                & ystep=1.25            , &
                & znum=3                , &
                & levels=[1000., 900., 800.], &
                & tnum=120              , &
                & tini='00Z01JAN2020'   , &
                & tstep='6hr'           , &
                & write_vars=vars_zonal   )

end program main

