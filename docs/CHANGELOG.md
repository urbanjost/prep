## prep Changelog

The intent of this changelog is to keep everyone in the loop about
what's new in the `prep` project. It is a curated, chronologically ordered
list of notable changes including records of change such as bug fixes,
new features, changes, and relevant notifications.

  - [x] git repository on WWW (github)
  - [x] annotated source files 
  - [x] an open license
  - [x] fpm(1) build
  - [x] user manual (on-line)
  - [x] man-page
  - [x] app program
  - [x] unit test 
  - [ ] make(1) build
  - [x] single-file build
  - [x] developer documents (ford(1),doxygen(1), ...)
  - [ ] CI/CD(Continious Integration/Development) verification (github actions)
  - [ ] registered in fpm(1) repository

---
**2022-05-10**  John S. Urban <https://github.com/urbanjost>

### :green_circle: ADD:
  + The POST command can take multiple parcel names
---
**2022-05-07**  John S. Urban <https://github.com/urbanjost>

### :green_circle: ADD:
  + added $UNSET directive
---
**2022-04-11**  John S. Urban <https://github.com/urbanjost>

### :orange_circle: DIFF:
  + No longer produce warning message if variable is redefined, to be more
    compatible with fpp(1) and cpp(1)
  + deprecating $REDEFINE. Removed from documentation.
---
**2022-04-02**  John S. Urban <https://github.com/urbanjost>

### :orange_circle: DIFF:
  + Ending "$BLOCK" with "$BLOCK END" or "$BLOCK" and "$PARCEL NAME "
    with "$PARCEL" is now deprecated,
### :green_circle: ADD:
  + on $IF and $ELSEIF  DEFINED() function can take list of names
  + add $ENDBLOCK 
  + add $ENDPARCEL                                             
  + add predefined SYSTEMON variable for conditional execution of lines
    when --system is on the command line
---
**2022-04-01**  John S. Urban <https://github.com/urbanjost>

### :orange_circle: DIFF:
  + $MESSAGE no longer automatically prefixs message with "message"

### :green_circle: ADD:
  + add $BLOCK SYSTEM        
  + add $BLOCK SET                                             
  + add $BLOCK MESSAGE                                         
---
**2022-04-01**  John S. Urban <https://github.com/urbanjost>

### :red_circle: FIX:
  + Negative values were not correctly tested by logical operators

### :orange_circle: DIFF:
  + --debug mode requires --verbose as well to show all debug messages

### :green_circle: ADD:

  + version 7.0.0 is released
  + added $ERROR for compatibility with some versions of cpp(1)
  + $IF,$ELSEIF,and $ELIF can use the syntax (expression)THEN
---
**2022-03-28**  John S. Urban <https://github.com/urbanjost>

### :green_circle: ADD:

  + $STOP can have a user-defined message                   
---
**2022-03-28**  John S. Urban <https://github.com/urbanjost>

### :green_circle: ADD:

  + $SHOW and $UNDEF allow basic globbing with "*" and "?"
---
**2022-03-27**  John S. Urban <https://github.com/urbanjost>

### :green_circle: ADD:

  + $REDEFINE was documented
  + $UNDEFINE,$DEFINE,$REDEFINE can take a list of semi-colon
    delimited list of variable names and expressions
  + $STOP was modernized now that a modern STOP is available on the
    majority of compilers.
  + allow filenames on $INCLUDE to be double-quoted or have \< prefix
    and \> suffix.
---
**2022-03-26**  John S. Urban <https://github.com/urbanjost>

### :red_circle: FIX:
  + Negative values were unneccesarily not allowed in expressions except
    as a simple value.

### :orange_circle: DIFF:
  + A comment is explicitly an exclamation _followed by a space_
    to allow C-style operators in expressions such as !,!=, ...
    which must _not_ be followed by a space.
  + A $DEFINE of a defined value is a warning instead of an error

### :green_circle: ADD:

  + $SHOW can take a list of variable names and only print those
---
**2022-03-13**  John S. Urban <https://github.com/urbanjost>

### :green_circle: ADD:

  +  Added --type, --start, and --stop options primarily for allowing for
     extracting Fortran from extended markdown files.
---
**2021-10-19**  John S. Urban <https://github.com/urbanjost>

### :green_circle: ADD:

  + Added algebraic operators (==,>=,<=,/=,>,<) to logical expressions
    in lieue of just dot operators (.eq.,.ge.,.le.,.ne.,.gt.,.lt.).
---
**2021-06-21**  John S. Urban <https://github.com/urbanjost>

### :green_circle: ADD:

  + Added $BLOCK SHELL option using a simpler system-dependent approach
    with scratch files rather than with calls to popen(3c) in order to be
    more portable. The documentation is commented out at this time. This
    is an experimental feature undergoing evaluation and should not be
    depended on at this time as a standard feature. It is clearly highly
    system dependent and should be used with restraint.

---
**2021-06-16**  John S. Urban <https://github.com/urbanjost>

### :green_circle: ADD:

  + Added $IMPORT so environment variables can be used as if they had been
    used to declare $SET names and values.                                

### :orange_circle: DIFF:
  + deprecated the $PRINTENV directive, as it is superceded by $IMPORT in
    a much more consistent way.
---
**2021-06-13**  John S. Urban <https://github.com/urbanjost>

### :green_circle: ADD:

  + Try to guess value for predefined variable OS, and if a $SET directive
    has been entered create string values for "here document" expansion so
    that ${__FILE__}, ${__LINE__}, ${__DATE__}, ${__TIME__} strings will
    be predefined.
---
**2021-06-12**  John S. Urban <https://github.com/urbanjost>

### :green_circle: ADD:

  + Added $PARCEL, $POST, and $SET which allows for basic templating
    as a prototype for allowing expansion using POSIX shell syntax
    like a "here document".
---
**2021-06-08**  John S. Urban <https://github.com/urbanjost>

### :green_circle: ADD:

Intialized github repository

---

<!--
-->
