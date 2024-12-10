# Control File Generator
Developer : Kosei Ohara  

## Test Environment
CentOS 7  
ifort 19.1.3.304  
Fortran 2003 or later will be needed.  

## About This Module
This module will be called to generate a GrADS Control file automatically.
By specifying a no-header binary file from an argument, a control file will be made in the same directory as the binary file.
Its file name will be the same as the binary file, but only extension is different.

# Usage
## Preparation
Users need to make a subroutine to write `VARS` lines before.  
Here is an example :
<a id='vars_example'>
```Fortran:vars.f90
module vars
    implicit none
    contains
    subroutine vars_example(unit, znum)
        integer, intent(in) :: unit         ! Unit number for a control file
        integer, intent(in) :: znum         ! Number of grids in z-direction
        write(unit,'(A)')          'VARS 1'
        write(unit,'(A,x,I0,x,A)') 'u', znum, '99 zonal wind'
        write(unit,'(A)')          'ENDVARS'
    end subroutine vars_example
end module vars
```
</a>
Two arguments are needed, the first one is the unit number of a control file, the second one is the number of grids in z-direction.
This subroutine will be the last argument of the main subroutine of this tool.

## Call the Main Module
Users will call `auto_ctl()`.
The other subroutines are Encapsulated.
The arguments for `auto_ctl()` are below:  
- bin  
  `character(*)`  
  File name of the binary file.
  Control file name will be determined from this file name.  
- title  
  `character(*)`  
  Title written in the control file.  
- options  
  `character(*)` `optional`  
  Options of the binary file, such as `'little_endian'` or `'yrev'`.
  If this argument is not provided, no options will be written.  
- xnum  
  `integer(4)`  
  Number of grids in x-direction.  
- xmin  
  `integer(4)`, `integer(8)`, `real(4)`, `real(8)`, or `real(16)`  
  The longitude of the first grid.  
- xstep  
  `integer(4)`, `integer(8)`, `real(4)`, `real(8)`, or `real(16)`  
  Grid size in x-direction (degree).  
- ynum  
  `integer(4)`
  Number of grids in y-direction.  
- ymin  
  `integer(4)`, `integer(8)`, `real(4)`, `real(8)`, or `real(16)`  
  The latitude of the first grid.  
- ystep  
  `integer(4)`, `integer(8)`, `real(4)`, `real(8)`, or `real(16)`  
  Grid size in y-direction (degree).  
- znum  
  `integer(4)`  
  Number of grids in z-direction.  
- levels  
  `integer(4)`, `integer(8)`, `real(4)`, `real(8)`, or `real(16)` `dimension(znum)`  
  Surfaces.  
- tnum  
  `integer(4)`  
  Number of time steps.  
- tini  
  `character(*)`  
  Date and time of the first step.
  This argument should be provided in `hhZddMthyyyy` format, such as `00Z01JAN2000` (Errors will not be returned even if the input does not follow this format).  
  Any other formats will be acceptable if GrADS can understand the string.
- tstep  
  `character(*)`  
  Temporal resolution.  
  This argument must be provided in `xmn`, `xhr`, `xdy`, `xmo`, or `xyr` format, where x is any integers, such as `6hr` (Errors will not be returned even if the input does not follow all of these formats).  
- write\_vars  
  `subroutine`  
  Arguments of this subroutine must follow the [example](#vars_example).
  The first argument is the unit number of the control file, the second is number of grids in z-direction.  
  





