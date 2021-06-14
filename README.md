# prep(1)
## A Fortran pre-processor written in Fortran

## DOCUMENTATION
 + [manpages](https://urbanjost.github.io/prep/prep.1.html) reformatted as HTML.
 + the [demos/](demos/) directory has example input files for a variety of cases
 + The `prep`(1) [CHANGELOG](CHANGELOG.md)

## DESCRIPTION
In general pre-processing should be minimalized but is still generally a
"necessary evil".

`prep(1)` is a basic pre-processor primarily designed for use with
Fortran. It does not support macros but is quite capable of supporting
conditional compilation for general platform-specific issues.

It is written in Fortran so those in the Fortran community may find it
easy to modify as needed and to use portably wherever modern Fortran
compilers are found (and in particular where other utilities from `m4`
to `cpp` are not).

`prep`(1) is not a drop-in replacement for `cpp` or `fpp` but serves
many of the same purposes.

A major use beyond conditional compilation is support for maintaining
documentation in the body of the source code in a variety of formats
via the __$BLOCK__ directive.

Note that `prep(1)` is a simplified but much more portable version of
`ufpp(1)` provided primarily to see what interest is garnered and whether
further development is warranted.

So please 
[**leave a comment**](https://github.com/urbanjost/prep/wiki/Fortran-pre-processing)
if you find this enticing. Feedback is welcome, even if it is a comment
on why you are not interested even though you read this far!

## BUILDING
To build it requires `git`, `fpm`(Fortran Package Manager), a modern
Fortran compiler and WWW access. It was tested with

   + GNU Fortran (GCC) 8.3.1  on 20191121 
   + GNU Fortran 9.3.0        on 20210612
   + ifort (IFORT) 19.1.3.304 on 20200925

Some use in remote packages of REAL128 variables and an issue with an
unused section of the remote dependencies caused a few issues with 
nvfortran, but the single-file [bootstrap version](bootstrap/prep_20210613.f90)
built with nvfortran 21.5-0 LLVM 64-bit target on x86-64 Linux without any
issues on 20210613.

## ACCESSING
```bash
   # go to where you want to create the `prep` directory
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
