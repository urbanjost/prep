# prep(1)
## A Fortran code pre-processor

## DOCUMENTATION
 + [manpages](https://urbanjost.github.io/prep/prep.1.html) reformatted as HTML.
 + the [demos/](demos/) directory has example input files for a variety of cases
 + The [CHANGELOG](docs/CHANGELOG.md)

## DESCRIPTION
`prep(1)` is a streamlined pre-processor primarily designed for use
with Fortran.  It does not support procedural macros but does support
variable substitution and reusable free-format text blocks which allows for
basic templating as well as easy construction of multi-line CHARACTER
variables; and is quite capable of supporting traditional conditional
compilation.

It is written in standard Fortran so those in the Fortran community
will find it easy to modify and to use portably wherever modern Fortran
compilers are found.

It is intentionally simple enough to be well described with a one-page
crib sheet, and should take no more than an hour to master. Fortran-like
(and some POSIX shell-like) syntax is used to leverage familiarity
with Fortran.

In particular, prep(1) allows for maintaining documentation in the body
of the source code in a variety of formats via the __$BLOCK__ directive.

Feedback is welcome. 
[**leave a comment!**](https://github.com/urbanjost/prep/wiki/Fortran-pre-processing)

## EXAMPLE SHOWING TEMPLATING

The most distinct feature of **prep(1)** compared to basic preprocessors is the
ability to define a block of text and apply special processing to it to simplify
maintaining documentation but also to repeat the code with different string
expansions, allowing for a form of templating a generic routine. A relatively
advanced example:

```text
$import USER
$! write the routine generically with ${NAME} variables
$parcel ex1
   ! created by ${USER} on ${DATE} at ${TIME}
   subroutine testit_${KIND}(value)
   real(kind=${kind}) :: value
      write(*,*)'big subroutine with type ${kind} and value=',value
   end subroutine testit_${KIND}
$parcel
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

$! a POST can be done within a $BLOCK to apply special processing
$block comment  ! convert parcel to comments
  any text placed here in free form will be converted to
  comments, as well as anything from a $POST. For example:

$post ex1

  $BLOCK can do other special processing, such as converting a block 
  to a CHARACTER variable,
  writing the lines to an external file for generating documentation,
  and so on.

$block 
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

!   any text placed here in free form will be converted to
!   comments, as well as anything from a $POST. For example:
! 
!    ! created by urbanjs on Jun 19 2021 at 11:55:43
!    subroutine testit_real128(value)
!    real(kind=real128) :: value
!       write(*,*)'big subroutine with type real128 and value=',value
!    end subroutine testit_real128
!
!  $BLOCK can do other special processing, such as converting a block 
!  to a CHARACTER variable,
!  writing the lines to an external file for generating documentation,
!  and so on.
!
```

## BUILDING
To build it requires `git`, `fpm`(Fortran Package Manager), a modern
Fortran compiler and WWW access or you will need to compile the single-file
[bootstrap version](https://github.com/urbanjost/index/tree/main/bootstrap). It was tested with

   + GNU Fortran (GCC) 8.3.1  on 20191121 
   + GNU Fortran 9.3.0        on 20210612
   + ifort (IFORT) 19.1.3.304 on 20200925

## ACCESSING
```bash
   # go to where you want to create the `prep` directory. For example:
   mkdir github
   cd github
   # get a clone of the repository
   git clone https://github.com/urbanjost/prep.git
   # enter the repository directory
   cd prep
```
## BUILDING AND INSTALLING
```bash
   # install (in the default location)
   fpm install 
```
## TRY IT
```bash
   # if you placed the program in a directory in your command path you are ready to go!
   prep --help
```

## HOMEPAGE
https://github.com/urbanjost/prep.git

<!--
https://github.com/fortran-lang/fpm/issues/78
-->
