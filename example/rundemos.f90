program rundemos
use M_io, only : slurp
implicit none
character(len=*),parameter            :: g='(*(g0))'
character(len=*),parameter   :: commands(*)=[ character(len=132) :: &
'prep -i block.ff                   ',&
'prep -i block_help.ff              ',&
'prep --type md -i color_wheel.md   ',&
'prep -i conditionals.ff            ',&
'prep -i m_cli_demo.ff              ',&
'prep -i os.ff                      ',&
'prep -i qsort_mod.ff               ',&
'prep -i set.ff                     ',&
'prep -i shell.ff --system          ',&
'prep -i show.ff                    ',&
'prep -i template.ff                ',&
'prep -i zen.ff                     ',&
'prep -i copyright.ff               ',&
'prep -i try_kracken.ff             ',&
'prep --help']
logical                               :: wait=.false.
integer                               :: exitstat
integer                               :: cmdstat
character(len=256)                    :: cmdmsg
integer                               :: i
   write(*,g)'ASSUMING prep(1) is in your search path'
   do i=1, size(commands)
      write(*,g)'RUNNING: ',commands(i)
      call execute_command_line('cd demos;'//commands(i), exitstat=exitstat,cmdmsg=cmdmsg,cmdstat=cmdstat,wait=wait)
      print *, i, "Exit status was ", exitstat,'Command status was ',cmdstat
      if(exitstat.ne.0)write(*,g)trim(cmdmsg)
   enddo
end program rundemos
