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

#### Fortran-like syntax

   Conditional expressions may be written very close to Fortran syntax,
   allowing for a low learning curve for Fortran programmers.

#### Text-based documentation can easily be maintained in the source file

   It allows for plain text blocks to be written to a file, allowing
   markdown, HTML, and Latex to be easily incorporated straight in the
   code file, including an option to literally keep code in a Markdown
   file with the code delimited between the lines  "\`\`\`fortran" and
   "\`\`\`".
   
   The plain text blocks can additionally be filtered to become
   a CHARACTER array definition, WRITE() statements or comments.

   It is a lot easier to maintain a large amount of text as simple lines
   than to maintain them as properly formatted variable definitions, for
   example.

   A command-line flag (preliminary) allows the same text block to be
   formatted for use with Ford or Doxygen or as as-is comments. Support of
   in-line documentation is not a common Fortran-compatible preprocessor
   feature.

#### No collisions with cpp(1)

   prep(1) intentionally does not use a pound prefix by default so that
   files (especially C) that already contain cpp(1) directives can be
   mixed in without collisions.
   
#### Easy access to environment variables

   prep(1) also allows environment variables to be imported, and by default
   $IFNDEF|$IFDEF will test for environment variables as well as variables
   declared with $DEFINE. This allows for easier communication with the
   programming environment than fpp(1) or cpp(1) which require that kind of
   information to be passed in on the command line.

### On the con side ...

prep(1) allows reuse of blocks but not true looping or a “for”
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

## Crib Sheet
```text
EXPRESSIONS
  numeric operators are +,-,*,/,**, (). Logical operators are
   >  | .EQ.| .NE.| .GE.| .GT.| .LE.| .LT.|.NOT.|.AND.| .OR.| .EQV.|.NEQV.|
   >  |  == |  /= |  >= |  >  |  <= |  <  |  !  |  && |  || |  ==  |  !=  |
  $DEFINE variable_name[=expression][;...]
   > Predefined values are "OS", which is set to a guess of the system type, and
   > UNKNOWN=0 LINUX=1 MACOS=2 WINDOWS=3 CYGWIN=4 SOLARIS=5 FREEBSD=6 OPENBSD=7.
   > SYSTEMON is .TRUE. if --system is present on the command line, else .FALSE.
  $UNDEFINE|$UNDEF variable_name[;...]
CONDITIONAL CODE SELECTION:
  $IF logical_integer-based_expression| [.NOT.] DEFINED(varname[,...])
  $IFDEF|$IFNDEF variable_or_envname
  $ELSEIF|$ELIF logical_integer-based_expression
  $ELSE
  $ENDIF
MACRO STRING EXPANSION AND TEXT REPLAY:
   > Unless at least one variable name is defined no ${NAME} expansion occurs.
  $SET varname string
  $$UNSET variable_name[;...]
  $IMPORT envname[;...]
   > $set author  William Shakespeare
   > $import HOME
   > write(*,*)'${AUTHOR} ${DATE} ${TIME} File ${FILE} Line ${LINE} HOME ${HOME}
  $PARCEL blockname ... $ENDPARCEL ! a reuseable parcel of expandable text
  $POST   blockname(s)  ! insert a defined parcel of text
EXTERNAL FILES (see $BLOCK ... --file also)
  $OUTPUT filename [--append]
  $INCLUDE filename
TEXT BLOCK FILTERS (--file writes to $PREP_DOCUMENT_DIR/doc/NAME)
  $BLOCK [comment|null|write|variable [--varname NAME]|set|system|message|
         define|help|version][--file NAME [--append]] ... $ENDBLOCK
INFORMATION
  $MESSAGE message_to_stderr
  $SHOW [defined_variable_name][;...]
SYSTEM COMMANDS (see also: $BLOCK SYSTEM)
  $SYSTEM command
  $STOP [stop_value[ "message"]] | $QUIT ["message"]| $ERROR ["message"]
```
