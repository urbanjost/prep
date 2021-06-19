program test_conditionals
implicit none
character(len=:),allocatable :: data(:)
integer                      :: i
data=[ character(len=132) :: &
'$! set value of variable "a" if it is not specified on the prep(1) command. ', &
'$IF .NOT.DEFINED(A)                                                         ', &
'$   DEFINE a=1  ! so only define the following first version of SUB(3f)     ', &
'$ENDIF                                                                      ', &
'   unfiltered                                                               ', &
'$! select a line depending on the value of variable "a"                     ', &
'$IF a .EQ. 1                                                                ', &
'   A is 1                                                                   ', &
'$ELSEIF a .eq. 2                                                            ', &
'   A is 2                                                                   ', &
'$ELSE                                                                       ', &
'   A is not 1 or 2                                                          ', &
'$ENDIF                                                                      ', &
'']

   write(*,'(a)')(trim(data(i)),i=1,size(data))

end program test_conditionals
