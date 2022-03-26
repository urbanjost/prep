program test_prep
use M_io, only : filewrite, filedelete, gulp
implicit none
character(len=:),allocatable :: data(:)
character(len=:),allocatable :: expected(:)
character(len=:),allocatable :: result(:)
integer                      :: i
integer                      :: ierr
logical,allocatable          :: tally(:)
allocate(tally(0))
   call conditionals()
   call set()
   call block()
   call expressions()
   call define()
   call block_2()
   call conditionals_2()
   if(all(tally))then
      write(*,'(a)')'ALL PREP TESTS PASSED'
   else
      write(*,'(a,*(l2))')'PREP TESTS FAILED',tally
      stop 1
   endif
contains
subroutine conditionals()

data=[ character(len=132) :: &
'$! set value of variable "a" if it is not specified on the prep(1) command. ', &
'$if .not.defined(a)                                                         ', &
'$   define a=1  ! so only define the following first version of sub(3f)     ', &
'$endif                                                                      ', &
'unfiltered                                                                  ', &
'$! select a line depending on the value of variable "a"                     ', &
'$if a .eq. 1                                                                ', &
'   a is 1                                                                   ', &
'$elseif a .eq. 2                                                            ', &
'   a is 2                                                                   ', &
'$else                                                                       ', &
'   a is not 1 or 2                                                          ', &
'$endif                                                                      ', &
'last line']

expected=[ character(len=132) :: &
'unfiltered                                                                  ', &
'   a is 1                                                                   ', &
'last line']

call teardown('CONDITIONALS')
end subroutine conditionals

subroutine set()
data=[ character(len=132) :: &
"$set author  William Shakespeare", &
"write(*,*)'By ${AUTHOR}'        ", &
"write(*,*)'File ${FILE}'        ", &
"write(*,*)'Line ${LINE}'        ", &
"last line"]

expected=[ character(len=132) :: &
"write(*,*)'By  William Shakespeare'", &
"write(*,*)'File _scratch.txt'      ", &
"write(*,*)'Line 4'                 ", &
'last line']

call teardown('SET')

end subroutine set

subroutine block()
data=[ character(len=132) :: &
"$!", &
"$! basic $block usage", &
"$!", &
"$BLOCK NULL", &
"", &
"  The $BLOCK directive allows for several treatments of blocks of", &
"  free-format text, facilitating easier maintenance of comments,", &
"  single-file maintenance of code and documentation, and easy definition", &
"  of large CHARACTER variable arrays.", &
"", &
"  This block has the NULL keyword specified so these lines are ignored", &
"  when generating the output file.", &
"", &
"$BLOCK", &
"", &
"$BLOCK COMMENT ", &
"  These lines will be converted to Fortran comments.", &
"  It is easier to reformat comments this way instead of having", &
"  to prefix each line with exclamations.", &
"$BLOCK", &
"", &
"$BLOCK COMMENT --file doc.html", &
"<html> <body>", &
"<p>", &
"  These lines will also be converted to Fortran comments but if ", &
"  the environment variable $PREP_DOCUMENT_DIR is set it will be", &
"  also be written as-is into $PREP_DOCUMENT_DIR/doc/doc.html.", &
"", &
"  The --file option also works with other options such as NULL", &
"  so no output has to appear in the output file if desired.", &
"</p>", &
"</body> </html>", &
"$BLOCK", &
"", &
"block", &
"integer :: io=6", &
"$BLOCK WRITE", &
"  These lines are converted to a series of WRITE() statements", &
"  where the LUN value ""IO"" is assumed to have been declared.", &
"$BLOCK", &
"endblock", &
"", &
"block ", &
"character(len=:),allocatable :: HELP_TEXT", &
"$BLOCK VARIABLE -varname HELP_TEXT", &
"  These lines are converted to a declaration of a CHARACTER", &
"  variable.", &
"$BLOCK", &
"endblock", &
'last line']

expected=[ character(len=132) :: &
"", &
"!   These lines will be converted to Fortran comments.", &
"!   It is easier to reformat comments this way instead of having", &
"!   to prefix each line with exclamations.", &
"", &
"! <html> <body>", &
"! <p>", &
"!   These lines will also be converted to Fortran comments but if", &
"!   the environment variable $PREP_DOCUMENT_DIR is set it will be", &
"!   also be written as-is into $PREP_DOCUMENT_DIR/doc/doc.html.", &
"! ", &
"!   The --file option also works with other options such as NULL", &
"!   so no output has to appear in the output file if desired.", &
"! </p>", &
"! </body> </html>", &
"", &
"block", &
"integer :: io=6", &
"write(io,'(a)')'  These lines are converted to a series of WRITE() statements'", &
"write(io,'(a)')'  where the LUN value ""IO"" is assumed to have been declared.'", &
"endblock", &
"", &
"block", &
"character(len=:),allocatable :: HELP_TEXT", &
"HELP_TEXT=[ CHARACTER(LEN=128) :: &", &
"'  These lines are converted to a declaration of a CHARACTER',&", &
"'  variable.',&", &
"'']", &
"", &
"endblock", &
'last line']

call teardown('BLOCK')

end subroutine block
subroutine blank()
data=[ character(len=132) :: &
'                             ', &
'last line']

expected=[ character(len=132) :: &
'                             ', &
'last line']

call teardown('BLANK')

end subroutine blank

subroutine teardown(name)
character(len=*),intent(in) :: name
   ierr=filewrite('_scratch.txt',data,status='replace')
   call execute_command_line ('fpm run prep -- -i _scratch.txt -o _out.txt')
   call gulp('_out.txt',result)
   CHECK : block
   if(size(expected).eq.size(result))then
      if( all(expected.eq.result) )then
         write(*,'(*(g0,1x))')name// ' PASSED'
         tally=[tally,.true.]
         exit CHECK
      endif
   endif
   tally=[tally,.false.]
   write(*,'(*(g0,1x))')name// ' FAILED'
   write(*,'(/,a)')'RESULT'
   write(*,'(i3.3,1x,a)')(i,trim(result(i)),i=1,size(result))
   write(*,'(/,a)')'EXPECTED'
   write(*,'(i3.3,1x,a)')(i,trim(expected(i)),i=1,size(expected))
   endblock CHECK
   ierr=filedelete('_scratch.txt')
   ierr=filedelete('_out.txt')
end subroutine teardown

subroutine expressions()
data=[ character(len=132) :: &
'$DEFINE A=10', &
'$IF .NOT.a.EQ.5*2 ! Note space after exclamation ', &
'$define A=A+1', &
'bad 1', &
'$else', &
'good ', &
'$endif', &
' ', &
'$if !10==5*2', &
'bad 2', &
'$define A=A+10', &
'$else', &
'good ', &
'$endif', &
' ', &
'$if 2*(4+2-5*2)/(-4)==2', &
'good ', &
'$else', &
'bad 3', &
'$define A=A+100', &
'$endif', &
' ', &
'$if A.ne.10', &
'$   stop', &
'$endif', &
'last line']

expected=[ character(len=132) :: &
'good', &
' ', &
'good', &
' ', &
'good', &
' ', &
'last line']

call teardown('EXPRESSIONS')

end subroutine expressions

subroutine define()
data=[ character(len=132) :: &
' ', &
'$DEFINE A=10', &
'$DEFINE A=A+1', &
'$ifndef A', &
'$   stop 1', &
'$endif', &
'$UNDEFINE A', &
'$ifdef A', &
'$   stop 2', &
'$endif', &
'$DEFINE A=10+2', &
'$show A', &
'last line']

expected=[ character(len=132) :: &
' ', &
'!  A  =  12', &
'last line']

call teardown('define')

end subroutine define

subroutine block_2()
data=[ character(len=132) :: &
'$BLOCK comment', &
'  This is a block of text that should be', &
'  converted to standard Fortran comments', &
'$BLOCK end', &
'$!------------------------------------------------', &
'$BLOCK null', &
'  #===================================#', &
'  | These lines should be ignored and |', &
'  | produce no output                 |', &
'  #===================================#', &
'$BLOCK ', &
'$!------------------------------------------------', &
'$BLOCK write', &
'  Convert this paragraph of text describing', &
'  sundry input options into a series of', &
'  WRITE statements', &
'$BLOCK end', &
'$!------------------------------------------------', &
'character(len=:),allocatable :: textblock(:)', &
'$BLOCK VARIABLE --varname textblock', &
'', &
' It is a lot easier to maintain a large amount of', &
' text as simple lines than to maintain them as', &
' properly formatted variable definitions', &
'', &
'$BLOCK end', &
'$!------------------------------------------------', &
'last line']

expected=[ character(len=132) :: &
"!   This is a block of text that should be", &
"!   converted to standard Fortran comments", &
"write(io,'(a)')'  Convert this paragraph of text describing'", &
"write(io,'(a)')'  sundry input options into a series of'", &
"write(io,'(a)')'  WRITE statements'", &
"character(len=:),allocatable :: textblock(:)", &
"textblock=[ CHARACTER(LEN=128) :: &", &
"'',&", &
"' It is a lot easier to maintain a large amount of',&", &
"' text as simple lines than to maintain them as',&", &
"' properly formatted variable definitions',&", &
"'',&", &
"'']", &
"", &
'last line']

call teardown('block_2')

end subroutine block_2

subroutine conditionals_2()

data=[ character(len=132) :: &
'$! set variable "a" if not specified on the prep(1) command.', &
'$IF .NOT.DEFINED(A)', &
'$   DEFINE a=1  ! so only define the first version of SUB(3f) below', &
'$ENDIF', &
'   program conditional_compile', &
'      call sub()', &
'   end program conditional_compile', &
'$! select a version of SUB depending on the value of variable "a"', &
'$IF a .EQ. 1', &
'   subroutine sub', &
'      print*, "This is the first SUB"', &
'   end subroutine sub', &
'$ELSEIF a .eq. 2', &
'   subroutine sub', &
'     print*, "This is the second SUB"', &
'  end subroutine sub', &
'$ELSE', &
'   subroutine sub', &
'      print*, "This is the third SUB"', &
'   end subroutine sub', &
'$ENDIF', &
'last line']

expected=[ character(len=132) :: &
'   program conditional_compile', &
'      call sub()', &
'   end program conditional_compile', &
'   subroutine sub', &
'      print*, "This is the first SUB"', &
'   end subroutine sub', &
'last line']

call teardown('CONDITIONALS_2')
end subroutine conditionals_2

end program test_prep
