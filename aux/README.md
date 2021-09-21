
Some files to use on plain txt for showing examples
of the $BLOCK --file option

txt2man - converts a basic markup language to a *roff file for use with man(1)
          or for further conversion via man(1) to HTML, PostScript, ...

md      - run the markdown script adding a CSS style sheet


For example, using the demos/block_help.ff file and txt2man

   cd demos/
   export PREP_DOCUMENT_DIR=`pwd`
   prep --system -i block_help.ff

and then look in doc/ and man/man/1


