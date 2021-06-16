## GPF Changelog

The intent of this changelog is to keep everyone in the loop about
what's new in the `prep` project. It is a curated, chronologically ordered
list of notable changes including`records of change such as bug fixes,
new features, changes, and relevant notifications.

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

   - [x] manpage
   - [x] program
   - [ ] unit test
### :orange_circle: DIFF:
### :red_circle: FIX:
---

<!--
-->
