program test_prep
USE ISO_FORTRAN_ENV, ONLY : STDERR=>ERROR_UNIT, STDOUT=>OUTPUT_UNIT,STDIN=>INPUT_UNIT
use M_io, only : filewrite, filedelete, gulp
use M_strings, only : upper
implicit none
character(len=:),allocatable :: data(:)
character(len=:),allocatable :: expected(:)
character(len=:),allocatable :: result(:)
integer                      :: i
integer                      :: ierr
logical,allocatable          :: tally(:)
allocate(tally(0))
!>>    > numeric operators are +,-,*,/,**, () are supported, logical operators are
!>>    >  | .EQ.| .NE.| .GE.| .GT.| .LE.| .LT.|.NOT.|.AND.| .OR.| .EQV.|.NEQV.|
!>>    >  |  == |  /= |  >= |  >  |  <= |  <  |  !  |  && |  || |  ==  |  !=  |
   call expressions()
   call expressions_2()

!>>   $DEFINE|$REDEFINE variable_name[=expression][;...]
!>>    > Predefined values are
!>>    > UNKNOWN=0 LINUX=1 MACOS=2 WINDOWS=3 CYGWIN=4 SOLARIS=5 FREEBSD=6 OPENBSD=7
!>>    > In addition OS is set to what the program guesses the system type is.
!>>    > SYSTEMON is 1 if --system is used on the command line, else it is 0.
!>>   $UNDEFINE|$UNDEF variable_name[;...]
   call define()

!>> CONDITIONAL CODE SELECTION
!>>   $IF logical_integer-based_expression | $IFDEF|$IFNDEF variable_name
!>>   $IF DEFINED(varname) | $IF .NOT. DEFINED(varname) |
!>>   $ELSEIF|$ELIF logical_integer-based_expression
!>>   $ELSE
!>>   $ENDIF
   call conditionals()
   call conditionals_2()
   call conditionals_3()
   call logics()

!>> MACRO STRING EXPANSION AND TEXT REPLAY
!>>   $SET varname string
!>>   $IMPORT envname[;...]
!>>    > Unless at least one variable name is defined no ${NAME} expansion occurs.
!>>    > $set author  William Shakespeare
!>>    > $import HOME
!>>    > write(*,*)'${AUTHOR} ${DATE} ${TIME} File ${FILE} Line ${LINE} HOME ${HOME}
   call set()

!>>   $PARCEL [blockname]  ! create a reuseable parcel of text that can be expanded
!>>   $POST   blockname  ! insert a defined parcel of text
   call parcel()

!>> EXTERNAL FILES (see $BLOCK ... --file also)
!>>   $OUTPUT filename [-append]
!>>   $INCLUDE filename
   call output()

!>> TEXT BLOCK FILTERS
!>>   $BLOCK [comment|null|write|variable [-varname NAME]]|help|version
!>>          [-file NAME [-append]]
   call block()
   call block_2()
   call block_3()

!>> SYSTEM COMMANDS
!>>   $SYSTEM command
   !TODO!call system()

   call env()

   call misc()

!>> INFORMATION
!>>   $MESSAGE message_to_stderr
!>>   $SHOW [defined_variable_name][;...]
   call message()
!>> METADATA
!>>   $IDENT metadata
!>>   $@(#)  metadata
   call ident()

!>>   $STOP [stop_value[ "message"]] | $QUIT ["message"]
   call stop()
   call quit()

   if(all(tally))then
      write(*,'(a)')'ALL PREP TESTS PASSED'
   else
      write(*,'(a,*(l2))')'PREP TESTS FAILED',tally
      stop 1
   endif
contains
!===============================================================================
subroutine sample()
data=[ character(len=132) :: &
"                                                                        ", &
"                                                                        ", &
"                                                                        ", &
"                                                                        ", &
"                                                                        ", &
"last line"]

expected=[ character(len=132) :: &
"                                                                        ", &
"                                                                        ", &
"                                                                        ", &
"                                                                        ", &
"                                                                        ", &
'last line']

call teardown('sample')

end subroutine sample
!===============================================================================
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
'$if SYSTEMON                                                                ', &
'BAD                                                                         ', &
'$endif                                                                      ', &
'last line']

expected=[ character(len=132) :: &
'unfiltered                                                                  ', &
'   a is 1                                                                   ', &
'last line']

call teardown('CONDITIONALS')
end subroutine conditionals
!===============================================================================
subroutine set()
data=[ character(len=132) :: &
"$set author  William Shakespeare", &
"write(*,*)'By ${AUTHOR}'        ", &
"write(*,*)'File ${FILE}'        ", &
"write(*,*)'Line ${LINE}'        ", &
"$unset author                   ", &
"write(*,*)'By ${AUTHOR}'        ", &
"last line"]

expected=[ character(len=132) :: &
"write(*,*)'By  William Shakespeare'", &
"write(*,*)'File _scratch.txt'      ", &
"write(*,*)'Line 4'                 ", &
"write(*,*)'By ${AUTHOR}'           ", &
'last line']

call teardown('SET')

end subroutine set
!===============================================================================
subroutine block()
data=[ character(len=132) :: &
"$!                                                                      ", &
"$! basic $block usage                                                   ", &
"$!                                                                      ", &
"$BLOCK NULL                                                             ", &
"                                                                        ", &
"  The $BLOCK directive allows for several treatments of blocks of       ", &
"  free-format text, facilitating easier maintenance of comments,        ", &
"  single-file maintenance of code and documentation, and easy definition", &
"  of large CHARACTER variable arrays.                                   ", &
"                                                                        ", &
"  This block has the NULL keyword specified so these lines are ignored  ", &
"  when generating the output file.                                      ", &
"                                                                        ", &
"$ENDBLOCK                                                               ", &
"                                                                        ", &
"$BLOCK COMMENT                                                          ", &
"  These lines will be converted to Fortran comments.                    ", &
"  It is easier to reformat comments this way instead of having          ", &
"  to prefix each line with exclamations.                                ", &
"$ENDBLOCK                                                               ", &
"                                                                        ", &
"$BLOCK COMMENT --file doc.html                                          ", &
"<html> <body>                                                           ", &
"<p>                                                                     ", &
"  These lines will also be converted to Fortran comments but if         ", &
"  the environment variable $PREP_DOCUMENT_DIR is set it will be         ", &
"  also be written as-is into $PREP_DOCUMENT_DIR/doc/doc.html.           ", &
"                                                                        ", &
"  The --file option also works with other options such as NULL          ", &
"  so no output has to appear in the output file if desired.             ", &
"</p>                                                                    ", &
"</body> </html>                                                         ", &
"$ENDBLOCK                                                               ", &
"                                                                        ", &
"block                                                                   ", &
"integer :: io=6                                                         ", &
"$BLOCK WRITE                                                            ", &
"  These lines are converted to a series of WRITE() statements           ", &
"  where the LUN value ""IO"" is assumed to have been declared.          ", &
"$BLOCK                                                                  ", &
"endblock                                                                ", &
"                                                                        ", &
"block                                                                   ", &
"character(len=:),allocatable :: HELP_TEXT                               ", &
"$BLOCK VARIABLE -varname HELP_TEXT                                      ", &
"  These lines are converted to a declaration of a CHARACTER             ", &
"  variable.                                                             ", &
"$BLOCK END                                                              ", &
"endblock                                                                ", &
'last line                                                               ']

expected=[ character(len=132) :: &
"                                                                                ", &
"!   These lines will be converted to Fortran comments.                          ", &
"!   It is easier to reformat comments this way instead of having                ", &
"!   to prefix each line with exclamations.                                      ", &
"                                                                                ", &
"! <html> <body>                                                                 ", &
"! <p>                                                                           ", &
"!   These lines will also be converted to Fortran comments but if               ", &
"!   the environment variable $PREP_DOCUMENT_DIR is set it will be               ", &
"!   also be written as-is into $PREP_DOCUMENT_DIR/doc/doc.html.                 ", &
"!                                                                               ", &
"!   The --file option also works with other options such as NULL                ", &
"!   so no output has to appear in the output file if desired.                   ", &
"! </p>                                                                          ", &
"! </body> </html>                                                               ", &
"                                                                                ", &
"block                                                                           ", &
"integer :: io=6                                                                 ", &
"write(io,'(a)')'  These lines are converted to a series of WRITE() statements'  ", &
"write(io,'(a)')'  where the LUN value ""IO"" is assumed to have been declared.' ", &
"endblock                                                                        ", &
"                                                                                ", &
"block                                                                           ", &
"character(len=:),allocatable :: HELP_TEXT                                       ", &
"HELP_TEXT=[ CHARACTER(LEN=128) :: &                                             ", &
"'  These lines are converted to a declaration of a CHARACTER',&                 ", &
"'  variable.                                                ',&                 ", &
"'']                                                                             ", &
"endblock                                                                        ", &
'last line                                                                       ']

call teardown('BLOCK')

end subroutine block
!===============================================================================
subroutine teardown(name,expected_exitstat)
character(len=*),intent(in) :: name
integer,optional    :: expected_exitstat
character(len=1)    :: paws
integer             :: iostat
character(len=256)  :: cmdmsg
integer             :: exitstat
integer             :: cmdstat
integer             :: estat
   if(present(expected_exitstat))then
      estat=expected_exitstat
   else
      estat=0
   endif
   ierr=filewrite('_scratch.txt',data,status='replace')
   !call execute_command_line ('fpm run prep -- --verbose --debug -i _scratch.txt -o _out.txt')
   exitstat=0
   cmdstat=0
   call execute_command_line ('fpm run prep -- F90 TESTPRG90 CMD=30/2 -i _scratch.txt -o _out.txt', &
   & exitstat=exitstat,cmdstat=cmdstat,cmdmsg=cmdmsg)
   write(*,*)'exitstat=',exitstat,'cmdstat=',cmdstat
   if(cmdstat.ne.0)write(stderr,*)trim(cmdmsg)
   call gulp('_out.txt',result)
   CHECK : block
      if(size(expected).eq.size(result).and.exitstat.eq.estat)then
         if( all(expected.eq.result) )then
            write(*,'("....................",T1,(a,T21,a))')trim(upper(name)),'PASSED'
            tally=[tally,.true.]
            exit CHECK
         endif
      endif
      tally=[tally,.false.]
      write(*,'("....................",T1,*(a,T21,a))')upper(name),'FAILED'
      write(*,'(/,a)')'RESULT'
      if(allocated(result))write(*,'(i3.3,1x,a)')(i,trim(result(i)),i=1,size(result))
      write(*,'(/,a)')'EXPECTED'
      if(allocated(expected))write(*,'(i3.3,1x,a)')(i,trim(expected(i)),i=1,size(expected))
   endblock CHECK
   ierr=filedelete('_scratch.txt')
   ierr=filedelete('_out.txt')
   call flushit()
   !write(*,'(a)',advance='no')'Use RETURN to continue'
   !read(*,'(a)',iostat=iostat)paws
end subroutine teardown
!===============================================================================
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
'$   define A=A+100', &
'$endif', &
' ', &
'$if A.ne.10', &
'$   STOP', &
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
!===============================================================================
subroutine expressions_2()
data=[ character(len=132) :: &
'$!> numeric operators are +,-,*,/,**, () are supported, logical operators are ',&
'$!>  | .EQ.| .NE.| .GE.| .GT.| .LE.| .LT.|.NOT.|.AND.| .OR.| .EQV.|.NEQV.|    ',&
'$!>  |  == |  /= |  >= |  >  |  <= |  <  |  !  |  && |  || |  ==  |  !=  |    ',&
'$DEFINE A=3.eq.3                                                              ',&
'$DEFINE A=A.and.100.ne.200                                                    ',&
'$DEFINE A=A.AND.300.ge.300                                                    ',&
'$DEFINE A=A.and.300.GE.299                                                    ',&
'$DEFINE A=A.and.300.gt.3                                                      ',&
'$DEFINE A=A.AND.3.LE.3                                                        ',&
'$DEFINE A=A.and.3.le.4                                                        ',&
'$DEFINE A=A.AND.3.LT.300                                                      ',&
'$!                                                                            ',&
'$DEFINE A = 3 == 3                                                            ',&
'$DEFINE A = A && 100 /= 200                                                   ',&
'$DEFINE A = A && 300 >= 300                                                   ',&
'$DEFINE A = A && 300> = 299                                                   ',&
'$DEFINE A = A && 300 > 3                                                      ',&
'$DEFINE A = A && 3 <= 3                                                       ',&
'$DEFINE A = A && 3 <= 4                                                       ',&
'$DEFINE A = A && 3 < 300                                                      ',&
'$!                                                                            ',&
'$show A                                                                       ',&
'last line']

expected=[ character(len=132) :: &
'! VARIABLE:  A  =  .TRUE.                                                     ',&
'last line']

call teardown('expressions_2')

end subroutine expressions_2
!===============================================================================
subroutine define()
data=[ character(len=132) :: &
'                                                    ', &
'$DEFINE A=10                                        ', &
'$DEFINE A=A+1                                       ', &
'$REDEFINE A=A+10                                    ', &
'$ifndef A                                           ', &
'$   stop 1                                          ', &
'$endif                                              ', &
'$UNDEFINE A                                         ', &
'$ifdef A                                            ', &
'$   stop 2                                          ', &
'$endif                                              ', &
'$DEFINE A=10+2                                      ', &
'$ifndef A                                           ', &
'$   stop 5                                          ', &
'$endif                                              ', &
'$ifdef A                                            ', &
'$else                                               ', &
'$   stop 6                                          ', &
'$endif                                              ', &
'$show A                                             ', &
'$define AB ; A_B                                    ', &
'$define AB_                                         ', &
'$define SUM=AB+A_B+AB_                              ', &
'$show SUM AB A_B AB_                                ', &
'$if .not.(AB+A_B+AB_==3)                            ', &
'    unexpected sum of the variables                 ', &
'$   show                                            ', &
'$   stop 3                                          ', &
'$endif                                              ', &
'$undefine ab; a_b; ab_                              ', &
'$if defined(AB).or.defined(A_B).or.defined(AB_)     ', &
'    variables are defined                           ', &
'$   show                                            ', &
'$   stop 4                                          ', &
'$endif                                              ', &
'last line                                           ']

expected=[ character(len=132) :: &
' ', &
'! VARIABLE:  A  =  12', &
'! VARIABLE:  SUM  =  3', &
'! VARIABLE:  AB  =  1', &
'! VARIABLE:  A_B  =  1', &
'! VARIABLE:  AB_  =  1', &
'last line']

call teardown('define')

end subroutine define
!===============================================================================
subroutine block_2()
data=[ character(len=132) :: &
'$BLOCK comment', &
'  This is a block of text that should be                       ', &
'  converted to standard Fortran comments                       ', &
'$BLOCK end                                                     ', &
'$!------------------------------------------------             ', &
'$BLOCK null                                                    ', &
'  #===================================#                        ', &
'  | These lines should be ignored and |                        ', &
'  | produce no output                 |                        ', &
'  #===================================#                        ', &
'$ENDBLOCK                                                      ', &
'$!------------------------------------------------             ', &
'$BLOCK write                                                   ', &
'  Convert this paragraph of text describing                    ', &
'  sundry input options into a series of                        ', &
'  WRITE statements                                             ', &
'$ENDBLOCK                                                      ', &
'$!------------------------------------------------             ', &
'character(len=:),allocatable :: textblock(:)                   ', &
'$BLOCK VARIABLE --varname textblock                            ', &
'                                                               ', &
' It is a lot easier to maintain a large amount of              ', &
' text as simple lines than to maintain them as                 ', &
' properly formatted variable definitions                       ', &
'                                                               ', &
'$ENDBLOCK                                                      ', &
'$!------------------------------------------------             ', &
'last line']

expected=[ character(len=132) :: &
"!   This is a block of text that should be",&
"!   converted to standard Fortran comments",&
"write(io,'(a)')'  Convert this paragraph of text describing'",&
"write(io,'(a)')'  sundry input options into a series of'",&
"write(io,'(a)')'  WRITE statements'",&
"character(len=:),allocatable :: textblock(:)",&
"textblock=[ CHARACTER(LEN=128) :: &",&
"'',&",&
"' It is a lot easier to maintain a large amount of',&",&
"' text as simple lines than to maintain them as   ',&",&
"' properly formatted variable definitions         ',&",&
"'                                                 ',&",&
"'']",&
"last line"]

call teardown('block_2')

end subroutine block_2
!===============================================================================
subroutine block_3()
data=[ character(len=132) :: &
'$!-------------------------------',&
'$BLOCK set                       ',&
'one   This is the one            ',&
'    two   two plus two is four   ',&
'    three pennies                ',&
'Four  calling birds              ',&
'FIVE  1 +1+ 1+4-2                ',&
'$ENDBLOCK                        ',&
'$!-------------------------------',&
'$BLOCK DEFINE                    ',&
'A=10;b = 20 ;                    ',&
'VAR = 3+3-2/2*(3**2)             ',&
'$ENDBLOCK                        ',&
'$!-------------------------------',&
'$if VAR==-3                      ',&
' ${one}                          ',&
'${two}                           ',&
'$endif                           ',&
'$!-------------------------------',&
'${three}                         ',&
'      ${three}                   ',&
'$!-------------------------------',&
'$if(A+B.eq.30)then               ',&
'${four}                          ',&
'${five}                          ',&
'${one}${three}  ${three}         ',&
'$endif                           ',&
'$!-------------------------------',&
'last line']

expected=[ character(len=132) :: &
'   This is the one               ',&
"  two plus two is four           ",&
"pennies                          ",&
"      pennies                    ",&
" calling birds                   ",&
" 1 +1+ 1+4-2                     ",&
"  This is the onepennies  pennies",&
"last line"]

call teardown('block_3')

end subroutine block_3
!===============================================================================
subroutine logics()
data=[ character(len=132) :: &
'$!-------------------------------',&
'$define a=.true.                 ',&
'$define b=.true.                 ',&
'$define c=.true.                 ',&
'$define d=.true.                 ',&
'$!-------------------------------',&
'$IF  a.and.b .eqv. .not.d        ',&
'BAD EQV                          ',&
'$ELSE                            ',&
'GOOD EQV                         ',&
'$ENDIF                           ',&
'$!-------------------------------',&
'$IF  a.and.b .neqv. .not.d       ',&
'GOOD NEQV                        ',&
'$ELSE                            ',&
'BAD NEQV                         ',&
'$ENDIF                           ',&
'$!-------------------------------',&
'$if 3.eq.3 .neqv. 4.eq.5         ',&
'GOOD A                           ',&
'$ENDIF                           ',&
'$!-------------------------------',&
'last line']

expected=[ character(len=132) :: &
"GOOD EQV                         ",&
"GOOD NEQV                        ",&
"GOOD A                           ",&
"last line"]

call teardown('logics')

end subroutine logics
!===============================================================================
subroutine conditionals_2()

data=[ character(len=132) :: &
'$! set variable "a" if not specified on the prep(1) command.         ', &
'$IF .NOT.DEFINED(A)                                                  ', &
'$   DEFINE a=1  ! so only define the first version of SUB(3f) below  ', &
'$ENDIF                                                               ', &
'   program conditional_compile                                       ', &
'      call sub()                                                     ', &
'   end program conditional_compile                                   ', &
'$! select a version of SUB depending on the value of variable "a"    ', &
'$IF a .EQ. 1                                                         ', &
'   subroutine sub                                                    ', &
'      print*, "This is the first SUB"                                ', &
'   end subroutine sub                                                ', &
'$ELSEIF a .eq. 2                                                     ', &
'   subroutine sub                                                    ', &
'     print*, "This is the second SUB"                                ', &
'  end subroutine sub                                                 ', &
'$ELSE                                                                ', &
'   subroutine sub                                                    ', &
'      print*, "This is the third SUB"                                ', &
'   end subroutine sub                                                ', &
'$ENDIF                                                               ', &
'last line']

expected=[ character(len=132) :: &
'   program conditional_compile                                       ', &
'      call sub()                                                     ', &
'   end program conditional_compile                                   ', &
'   subroutine sub                                                    ', &
'      print*, "This is the first SUB"                                ', &
'   end subroutine sub                                                ', &
'last line']

call teardown('CONDITIONALS_2')
end subroutine conditionals_2
!===============================================================================
subroutine parcel()
data=[ character(len=132) :: &
'$! write a generic function ".                   ',&
'$!==============================                 ',&
'$PARCEL SWAP                                     ',&
'elemental subroutine ${PREFIX}_swap(x,y)         ',&
'!> swap two ${TYPE} variables                    ',&
'${TYPE}, intent(inout) :: x,y                    ',&
'${TYPE}                :: temp                   ',&
'   temp = x; x = y; y = temp                     ',&
'end subroutine ${PREFIX}_swap                    ',&
'$ENDPARCEL                                       ',&
'$!==============================                 ',&
'module M_swap                                    ',&
'implicit none                                    ',&
'private                                          ',&
'public :: swap                                   ',&
'integer,parameter :: dp=kind(0.0d0)              ',&
'integer,parameter :: cd=kind(0.0d0)              ',&
'interface swap                                   ',&
'   module procedure r_swap, i_swap, c_swap       ',&
'   module procedure d_swap, l_swap, cd_swap      ',&
'end interface                                    ',&
'contains                                         ',&
'$!==============================                 ',&
'$SET TYPE doubleprecision                        ',&
'$SET PREFIX d                                    ',&
'$POST SWAP                                       ',&
'$!==============================                 ',&
'$SET TYPE real                                   ',&
'$SET PREFIX r                                    ',&
'$POST SWAP                                       ',&
'$!==============================                 ',&
'$SET TYPE integer                                ',&
'$SET PREFIX i                                    ',&
'$POST SWAP                                       ',&
'$!==============================                 ',&
'$SET TYPE logical                                ',&
'$SET PREFIX l                                    ',&
'$POST SWAP                                       ',&
'$!==============================                 ',&
'$SET TYPE complex                                ',&
'$SET PREFIX c                                    ',&
'$POST SWAP                                       ',&
'$!==============================                 ',&
'$SET TYPE complex(kind=cd)                       ',&
'$SET PREFIX cd                                   ',&
'$POST SWAP                                       ',&
'$!==============================                 ',&
'end module M_swap                                ',&
""]

expected=[ character(len=132) :: &
'module M_swap                                    ',&
'implicit none                                    ',&
'private                                          ',&
'public :: swap                                   ',&
'integer,parameter :: dp=kind(0.0d0)              ',&
'integer,parameter :: cd=kind(0.0d0)              ',&
'interface swap                                   ',&
'   module procedure r_swap, i_swap, c_swap       ',&
'   module procedure d_swap, l_swap, cd_swap      ',&
'end interface                                    ',&
'contains                                         ',&
'elemental subroutine d_swap(x,y)                 ',&
'!> swap two doubleprecision variables            ',&
'doubleprecision, intent(inout) :: x,y            ',&
'doubleprecision                :: temp           ',&
'   temp = x; x = y; y = temp                     ',&
'end subroutine d_swap                            ',&
'elemental subroutine r_swap(x,y)                 ',&
'!> swap two real variables                       ',&
'real, intent(inout) :: x,y                       ',&
'real                :: temp                      ',&
'   temp = x; x = y; y = temp                     ',&
'end subroutine r_swap                            ',&
'elemental subroutine i_swap(x,y)                 ',&
'!> swap two integer variables                    ',&
'integer, intent(inout) :: x,y                    ',&
'integer                :: temp                   ',&
'   temp = x; x = y; y = temp                     ',&
'end subroutine i_swap                            ',&
'elemental subroutine l_swap(x,y)                 ',&
'!> swap two logical variables                    ',&
'logical, intent(inout) :: x,y                    ',&
'logical                :: temp                   ',&
'   temp = x; x = y; y = temp                     ',&
'end subroutine l_swap                            ',&
'elemental subroutine c_swap(x,y)                 ',&
'!> swap two complex variables                    ',&
'complex, intent(inout) :: x,y                    ',&
'complex                :: temp                   ',&
'   temp = x; x = y; y = temp                     ',&
'end subroutine c_swap                            ',&
'elemental subroutine cd_swap(x,y)                ',&
'!> swap two complex(kind=cd) variables           ',&
'complex(kind=cd), intent(inout) :: x,y           ',&
'complex(kind=cd)                :: temp          ',&
'   temp = x; x = y; y = temp                     ',&
'end subroutine cd_swap                           ',&
'end module M_swap                                ',&
'']

call teardown('parcel')

end subroutine parcel
!===============================================================================
subroutine stop()
data=[ character(len=132) :: &
"PRINT THIS               ", &
"$stop 10 Exit Here !     ", &
"NOT THIS                 ", &
"last line"]

expected=[ character(len=132) :: &
'PRINT THIS']

call teardown('stop',1)

end subroutine stop
!===============================================================================
subroutine quit()
data=[ character(len=132) :: &
"PRINT THIS               ", &
"$QUIT                    ", &
"NOT THIS                 ", &
"last line"]

expected=[ character(len=132) :: &
'PRINT THIS']

call teardown('quit')

end subroutine quit
!===============================================================================
subroutine message()
data=[ character(len=132) :: &
"$show *E*                           ", &
"$IMPORT USER                        ", &
"$import HOME                        ", &
"$message ${USER} ${DATE} ${TIME}    ", &
"$set author  William Shakespeare    ", &
"$MESSAGE 'By ${AUTHOR}'             ", &
"$MESSAGE 'File ${FILE}'             ", &
"$MESSAGE 'Line ${LINE}'             ", &
"$MESSAGE 'Date ${DATE}'             ", &
"$MESSAGE 'Time ${TIME}'             ", &
"$MESSAGE 'HOME ${HOME}'             ", &
"$block message                      ", &
"this is a block of text             ", &
"     to display                     ", &
"     on stderr.                     ", &
"                                    ", &
"${date} ${time} ${file}             ", &
"$endblock                           ", &
"last line"]

expected=[ character(len=132) :: &
'! VARIABLE:  TESTPRG90  =  1        ', &
'! VARIABLE:  SYSTEMON  =  .FALSE.   ', &
'! VARIABLE:  OPENBSD  =  7          ', &
'! VARIABLE:  FREEBSD  =  6          ', &
'last line']

call teardown('message')

end subroutine message
!===============================================================================
subroutine ident()
data=[ character(len=132) :: &
"$@(#) M_module::procedure: my procedure", &
"$IDENT M_module::procedure:    my  procedure   ", &
"last line"]

expected=[ character(len=132) :: &
'! ident_1="@(#) M_module procedure my procedure"', &
'! ident_2="@(#) M_module procedure my procedure"', &
'last line']

call teardown('message')

end subroutine ident
!===============================================================================
subroutine output()
integer :: ios, lun
data=[ character(len=132) :: &
"$OUTPUT _scratch_output                                                 ", &
"This should be placed in an external file                               ", &
"that is subsequently read back in                                       ", &
"$OUTPUT                                                                 ", &
"$INCLUDE _scratch_output                                                ", &
"last line"]

expected=[ character(len=132) :: &
"This should be placed in an external file                               ", &
"that is subsequently read back in                                       ", &
'last line']

call teardown('output')

open(file='_scratch_output',newunit=lun,iostat=ios)
close(unit=lun,iostat=ios,status='delete')

end subroutine output
!===============================================================================
subroutine env()
character(len=4096)       :: home_value
integer                   :: istatus

data=[ character(len=132) :: &
"$IMPORT HOME             ", &
"${HOME}"]

home_value=''
call get_environment_variable('HOME',home_value,status=istatus)
expected=[ character(len=132) :: home_value]

call teardown('env')

end subroutine env
!===============================================================================
subroutine misc()
character(len=4096)       :: home_value
integer                   :: istatus

data=[ character(len=132) :: &
'$if ( 3 .eq. 2+1 ) then                                                         ',&
'OK A                                                                            ',&
'$else                                                                           ',&
'$error "test  A"                                                                ',&
'$endif                                                                          ',&
'$!======================                                                        ',&
'$if( 3 .lt. 4 )then                                                             ',&
'OK B                                                                            ',&
'$else                                                                           ',&
'$error "test  B"                                                                ',&
'$endif                                                                          ',&
'$!======================                                                        ',&
'$if( 3 .lt. 4 )                                                                 ',&
'OK C                                                                            ',&
'$else                                                                           ',&
'$error "test  C"                                                                ',&
'$endif                                                                          ',&
'$!======================                                                        ',&
'$if( 3-5 .lt. 4 )                                                               ',&
'OK D                                                                            ',&
'$else                                                                           ',&
'$SHOW                                                                           ',&
'$error "test  D"                                                                ',&
'$endif                                                                          ',&
'$!======================                                                        ',&
'$if( 3+5 .lt. 4 )                                                               ',&
'$elseif( -5 .gt. 1 )then                                                        ',&
'$elseif( -5 .lt. 1 )                                                            ',&
'OK E                                                                            ',&
'$endif                                                                          ',&
'$!======================                                                        ',&
'$if  2 .ge. (-5  ) .and. 1==1&&2<3&&2<=2&&!4==5&&6>=6&&7>-1&&10**3>999          ',&
'OK F                                                                            ',&
'$else                                                                           ',&
'$error "test  F"                                                                ',&
'$endif                                                                          ',&
'$!======================                                                        ',&
'$define VAL1=20                                                                 ',&
'$define VAL2=VAL1                                                               ',&
'$if  VAL2 == 20                                                                 ',&
'OK G                                                                            ',&
'$else                                                                           ',&
'$error "test  G"                                                                ',&
'$endif                                                                          ',&
'$!======================                                                        ',&
'$if  CMD == 15                                                                  ',&
'GOOD: CMD                                                                       ',&
'$else                                                                           ',&
'BAD: CMD                                                                        ',&
'$endif                                                                          ',&
'$!======================                                                        ',&
"last line"]

expected=[ character(len=132) :: &
'OK A',&
'OK B',&
'OK C',&
'OK D',&
'OK E',&
'OK F',&
'OK G',&
'GOOD: CMD                                                                       ',&
"last line"]

call teardown('misc')

end subroutine misc
!===============================================================================
subroutine conditionals_3()

data=[ character(len=132) :: &
'$define a;b;c;d;e;f;g;h                                                      ',&
'$                                                                            ',&
'$if defined(a,b,c,d,e,f,g).and.defined(h,g,f,e,d,c,b,a).and..not.defined(i,j)',&
'GOOD 1                                                                       ',&
'$else                                                                        ',&
'BAD 1                                                                        ',&
'$endif                                                                       ',&
'$                                                                            ',&
'$if defined(a,b,c,i,d,e,f,g).and.defined(h,g,f,e,d,c,b,a)                    ',&
'BAD 2                                                                        ',&
'$else                                                                        ',&
'GOOD 2                                                                       ',&
'$endif                                                                       ',&
'$                                                                            ',&
'$if defined(a,b,c,i,d,e,f,g).and.defined(h,g,f,e,j,d,c,b,a)                  ',&
'BAD 3                                                                        ',&
'$else                                                                        ',&
'GOOD 3                                                                       ',&
'$endif                                                                       ',&
'$                                                                            ',&
'$if defined(a,b,c,d,e,f,g).and.defined(h,g,f,e,j,d,c,b,a)                    ',&
'BAD 4                                                                        ',&
'$else                                                                        ',&
'GOOD 4                                                                       ',&
'$endif                                                                       ',&
'$                                                                            ',&
'$if defined(a).or.defined(i)                                                 ',&
'GOOD 5                                                                       ',&
'$else                                                                        ',&
'BAD 5                                                                        ',&
'$endif                                                                       ',&
'$                                                                            ',&
'$if defined(j).or.defined(i)                                                 ',&
'BAD 6                                                                        ',&
'$else                                                                        ',&
'GOOD 6                                                                       ',&
'$endif                                                                       ',&
'$                                                                            ',&
'last line']

expected=[ character(len=132) :: &
'GOOD 1                                                                  ', &
'GOOD 2                                                                  ', &
'GOOD 3                                                                  ', &
'GOOD 4                                                                  ', &
'GOOD 5                                                                  ', &
'GOOD 6                                                                  ', &
'last line']

call teardown('CONDITIONALS_3')
end subroutine conditionals_3
!===============================================================================
subroutine flushit()
integer :: ios
      flush(unit=stdout,iostat=ios)
      flush(unit=stderr,iostat=ios)
end subroutine flushit
end program test_prep
