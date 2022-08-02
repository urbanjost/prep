program prep                                               !@(#)prep(1f): preprocessor for Fortran/FORTRAN source code
USE M_HISTORY, ONLY : REDO
use M_expr,    only : expr, G_line_length, G_var_len, G_verbose, G_debug
implicit none
character(len=G_line_length) :: line                       ! working copy of input line
integer                      :: ios
logical                      :: tdef=.false.
   READLINE: do                                            ! read loop to read input file
      write(*,'(a)',advance='no')'>'
      read(*,'(a)',iostat=ios) line
      select case(line)
      case('.verbose'); G_verbose=.not.G_verbose;call printstate()
      case('.debug');   G_debug=.not.G_debug;call printstate()
      case('.define');  tdef=.not.tdef;call printstate()
      case('.show');    call printme();call printstate()
      case('.stop');    stop
      case('.help');    write(*,*)'verbose,debug,define,show,stop,help'
      case default
       if(ios.ne.0)exit READLINE
       CALL REDO(LINE,r='.')      ! pass line to REDO(3f). This is a no-op except
       call printme()
      end select
   enddo READLINE
   stop
   !call printme('--100')
   !call printme('.and.')
   !call printme('.and.3')
   !call printme('0.and.3')
contains
subroutine printstate()
   write(*,*)'Verbose',G_verbose,' Debug',G_debug,'Def',tdef
end subroutine printstate
subroutine printme()
character(len=G_var_len)    :: value                     ! returned variable value
integer                     :: ierr
   if(tdef)then
      call expr(line,value,ierr,def=.true.)
   else
      call expr(line,value,ierr)
   endif
   if(ierr.ne.0)then
      write(*,*)'VALUE=',trim(value),' IERR=',ierr
   else
      write(*,*)'VALUE=',trim(value)
   endif
end subroutine printme
end program prep
