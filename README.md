### fpm-tools : [prep](https://urbanjost.github.io/prep/)

## A Fortran code pre-processor written in Fortran

## Documentation   ![docs](docs/images/docs.gif)
 + [man-pages](https://urbanjost.github.io/prep/prep.1.html) reformatted as HTML.
 + the [demos/](https://github.com/urbanjost/prep/tree/main/demos/) directory has example input files for a variety of cases
 + The [CHANGELOG](https://github.com/urbanjost/prep/blob/main/docs/CHANGELOG.md)
 + The [CI/CD](https://github.com/urbanjost/prep/blob/main/docs/STATUS.md) results
 + Developer [ford(1) output](https://urbanjost.github.io/prep/fpm-ford/index.html)

## Description
`prep(1)` is a streamlined pre-processor primarily designed for use with
Fortran. It does not support procedural macros but does support variable
substitution and reusable free-format text blocks which allows for basic
templating and easy construction of multi-line CHARACTER variables;
and is quite capable of supporting traditional conditional compilation.

It is written in standard Fortran so those in the Fortran community will
find it easy to modify and to use portably.

`prep(1)` is intentionally simple enough to be well described with
a one-page crib sheet, and should take no more than an hour to
master. Fortran-like (and some POSIX shell-like) syntax is used to
leverage familiarity with Fortran.

In particular, `prep(1)` allows for maintaining documentation in the body
of the source code in a variety of formats via the __$BLOCK__ directive.

## Example Showing Templating

The most distinct feature of **prep(1)** compared to basic preprocessors
is the ability to define a block of text and apply special processing
to it to simplify maintaining documentation but also to repeat the code
with different string expansions, allowing for a form of templating a
generic routine. A relatively advanced example:

```text
$import USER
$! write the routine generically with ${NAME} variables
$parcel ex1
   ! created by ${USER} on ${DATE} at ${TIME}
   subroutine testit_${KIND}(value)
   real(kind=${kind}) :: value
      write(*,*)'big subroutine with type ${kind} and value=',value
   end subroutine testit_${KIND}
$endparcel
$!
module M_testit
use, intrinsic :: iso_fortran_env, only : real32, real64, real128
implicit none
private
public testit
interface testit
   module procedure testit_real32
   module procedure testit_real64
   module procedure testit_real128
end interface testit
contains
$! now just $POST the routine multiple times changing the kind ...
$set kind real32
$post ex1
$set kind real64
$post ex1
$set kind real128
$post ex1
end module M_testit
```
The output looks like
```fortran
module M_testit
use, intrinsic :: iso_fortran_env, only : real32, real64, real128
implicit none
private
public testit
interface testit
   module procedure testit_real32
   module procedure testit_real64
   module procedure testit_real128
end interface testit
contains
   ! created by urbanjs on Jun 19 2021 at 11:55:43
   subroutine testit_real32(value)
   real(kind=real32) :: value
      write(*,*)'big subroutine with type real32 and value=',value
   end subroutine testit_real32
   ! created by urbanjs on Jun 19 2021 at 11:55:43
   subroutine testit_real64(value)
   real(kind=real64) :: value
      write(*,*)'big subroutine with type real64 and value=',value
   end subroutine testit_real64
   ! created by urbanjs on Jun 19 2021 at 11:55:43
   subroutine testit_real128(value)
   real(kind=real128) :: value
      write(*,*)'big subroutine with type real128 and value=',value
   end subroutine testit_real128
end module M_testit
```
## Building
To build it requires `git`, `fpm`(Fortran Package Manager), a modern
Fortran compiler and WWW access or you will need to compile the single-file
[bootstrap version](https://raw.githubusercontent.com/urbanjost/prep/main/standalone/prep.f90).
It was tested with

   + GNU Fortran (GCC) 8.3.1         on 20191121
   + GNU Fortran 9.3.0               on 20210612
   + GNU Fortran 10.3.0              on 20220305
   + ifort (IFORT) 19.1.3.304        on 20200925
   + ifort (IFORT) 2021.3.0 20210609 on 20220305

In addition, the standalone version was tested with

   + nvfortran 21.5-0                on 20220329

## Accessing
```bash
   # go to where you want to create the `prep` directory. For example:
   mkdir github
   cd github
   # get a clone of the repository
   git clone https://github.com/urbanjost/prep.git
   # enter the repository directory
   cd prep
```
## Building and Installing
```bash
   # build and install (in the default location) using fpm(1)
   fpm install
```
or
```bash
   # examples of building standalone version
   gfortran standalone/prep.f90 -o prep 
   ifort standalone/prep.f90 -o prep 
   nvfortran -Mbackslash standalone/prep.f90 -o prep 
   # example of moving to a location in your path
   mv prep $HOME/.local/bin/
```
## Try It
```bash
   # if you placed the program in a directory in your command path you are ready to go!
   prep --help
```
## Homepage
https://github.com/urbanjost/prep.git

Feedback is welcome.
[**leave a comment!**](https://github.com/urbanjost/prep/wiki/Fortran-pre-processing)


<!--
https://github.com/fortran-lang/fpm/issues/78
-->
