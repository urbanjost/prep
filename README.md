# prep(1)
## A Fortran code pre-processor

## DOCUMENTATION
 + [manpages](https://urbanjost.github.io/prep/prep.1.html) reformatted as HTML.
 + the [demos/](demos/) directory has example input files for a variety of cases
 + The [CHANGELOG](CHANGELOG.md)

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

Please 
[**leave a comment**](https://github.com/urbanjost/prep/wiki/Fortran-pre-processing)
if you find this enticing. Feedback is welcome, even if it is a comment
on why you are not interested even though you read this far!

## BUILDING
To build it requires `git`, `fpm`(Fortran Package Manager), a modern
Fortran compiler and WWW access. It was tested with

   + GNU Fortran (GCC) 8.3.1  on 20191121 
   + GNU Fortran 9.3.0        on 20210612
   + ifort (IFORT) 19.1.3.304 on 20200925

Some use in remote packages of REAL128 variables and an issue
with an unused section of the remote dependencies caused
a few issues with nvfortran, but the single-file [bootstrap
version](bootstrap/prep_20210617.f90) builds with nvfortran 21.5-0 LLVM
without any issues.

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
