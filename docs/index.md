## prep(1)

- [man-page](prep.1.html)
- Ford-generated [developer documentation](fpm-ford/index.html)
- [github homepage](https://github.com/urbanjost/prep)

### There are tons of pre-processors for Fortran. What are the pros and cons of prep(1) over others?

#### Sustainable

   prep(1) is written in Fortran, making it very easily maintained
   or modified in a Fortran programming environment. The file
   standalone/prep.f90 is a single Fortran file that intentionally avoids
   platform-specific code.
   
#### Basic Templating

   prep(1)  allows for templating via variable expansion using a POSIX-shell
   syntax which many are familiar with

#### Text-based documentation can easily be maintained in the source file

   It allows for plain text blocks to be written to a file, allowing
   markdown, HTML, and Latex to be easily incorporated straight in the
   code file.
   
   The plain text blocks can be filtered into a CHARACTER array
   definition, WRITE() statements and comments, including a basic flag
   (preliminary) that allows the same text block to be formatted for use
   with Ford or Doxygen as well as as-is comments. Support of in-line
   documentation is not a common Fortran-compatible preprocessor feature.

#### No collisions with cpp(1)

   It intentionally does not use a pound prefix by default so that files
   (especially C) that already contain cpp(1) directives can be mixed in
   without collisions.
   
#### Easy access to environment variables

   prep(1) also allows environment variables to be imported, and by default
   $IFNDEF|$IFDEF will test for environment variables as well as variables
   declared with $DEFINE. This allows for easier communication with the
   programming environment than fpp(1) or cpp(1) which require that kind of
   information to be passed in on the command line.

### On the con side ...

prep(1) allow reuse of blocks but not true looping or a “for”
statement.

cpp(1) and fpp(1) are often tightly coupled with the compiler, allowing
for compiler-specific pre-defined variables. Supporting importing of
environment variables addresses this to some extent.

Currently, prep(1) does not insert "# NNN “filename” lines for the
debugger, but line counts and input files are tracked so that would be
trivial to add.

There is not a warning about line length being exceeded nor support for
automatic continuation lines. Again, easy to add but there should at
least be a warning, as the changes to Fortran to eliminate the 132 column
default line length limit will not be available on all compilers for a
while although a lot of compilers already allow that.
