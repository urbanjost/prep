 
!>>>>> build/dependencies/M_kracken95/src/M_kracken95.f90
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! The M_KRACKEN95(3f) interface file is free and unencumbered software released into the public domain.
! For further details see the file UNLICENSE.txt or refer to <http;//unlicense.org/>
!
! Even so, I ask that you send me interesting alterations that are available for public use; and
! that you include a note in the source acknowledging the original author (1989,1996,2013 John S. Urban).
!
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_kracken95
implicit none
! "@(#)M_kracken95(3f,module):parse command line options of Fortran programs using Unix-like syntax"
!===================================================================================================================================
   private
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: kracken                ! define command and default parameter values from command arguments
   public :: setprompts             ! define prompts for commands in interactive mode
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: rget                   ! fetch real    value of name VERB_NAME from the language dictionary
   public :: dget                   ! fetch double  value of name VERB_NAME from the language dictionary
   public :: iget                   ! fetch integer value of name VERB_NAME from the language dictionary
   public :: lget                   ! fetch logical value of name VERB_NAME from the language dictionary
   public :: sget                   ! fetch string  value of name VERB_NAME from the language dictionary.
   public :: sgetl                  ! fetch string  value of name VERB_NAME from the language dictionary.
   public :: retrev                 ! retrieve token value as string from Language Dictionary when given NAME
!-!   public :: retrev_string_variable_length
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: delim                  ! parse a string and store tokens into an array
   public :: string_to_real         ! returns real value from numeric character string NOT USING CALCULATOR
   public :: string_to_dble         ! returns double precision value from numeric character string NOT USING CALCULATOR
!-----------------------------------------------------------------------------------------------------------------------------------
   public  :: dissect               ! for user-defined commands: define defaults, then process user input
   private :: parse                 ! parse user command and store tokens into Language Dictionary
   private :: store                 ! replace dictionary name's value (if allow=add add name if necessary)
   private :: bounce                ! find location (index) in Language Dictionary where VARNAM can be found
   private :: add_string            ! Add new string name to Language Library dictionary
   private :: send_message
   private :: get_command_arguments ! get_command_arguments: return all command arguments as a string
   private :: igets                 ! return the subscript value of a string when given it's name
   private :: uppers                ! return copy of string converted to uppercase
   private :: menu                  ! generate an interactive menu when -? option is used
!-----------------------------------------------------------------------------------------------------------------------------------
   public  :: kracken_comment
!-----------------------------------------------------------------------------------------------------------------------------------
! length of verbs and entries in Language dictionary
! NOTE:   many parameters were reduced in size so as to just accommodate being used as a command line parser.
!         In particular, some might want to change:
   integer, parameter,public :: IPic=100                           ! number of entries in language dictionary
   integer, parameter,public :: IPvalue=4096                       ! length of keyword value
   integer, parameter,public :: IPcmd=32768                        ! length of command
   integer, parameter,public :: IPverb=20                          ! length of verb
!-----------------------------------------------------------------------------------------------------------------------------------
   integer, parameter        :: dp = kind(0.d0)
   integer, parameter        :: k_int = SELECTED_INT_KIND(9)       ! integer*4
   integer, parameter        :: k_dbl = SELECTED_REAL_KIND(15,300) ! real*8
!-----------------------------------------------------------------------------------------------------------------------------------
   ! dictionary for Language routines
   character(len=IPvalue),dimension(IPic)     :: values=" "        ! contains the values of string variables
   character(len=IPverb),dimension(IPic)      :: dict_verbs=" "    ! string variable names
   integer(kind=k_int),dimension(IPic)        :: ivalue=0          ! significant lengths of string variable values
   character(len=1),save                      :: kracken_comment='#'
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine retrev(name,val,llen,ier)
! "@(#)retrev(3f): retrieve token value from Language Dictionary when given NAME"
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)  :: name        ! name of variable to retrieve value for in form VERB_NAME
      character(len=*),intent(out) :: val         ! value for requested variable
      integer,intent(out)          :: llen        ! position of last non-blank character in requested variable
      integer,intent(out)          :: ier         ! error flag 0=found requested variable; -1=entry not found
!-----------------------------------------------------------------------------------------------------------------------------------
      integer                      :: isub        ! subscript in dictionary where requested entry and corresponding value are found
!-----------------------------------------------------------------------------------------------------------------------------------
      isub=igets(name)                            ! get index entry is stored at
!-----------------------------------------------------------------------------------------------------------------------------------
      if(isub > 0)then                            ! entry was in dictionary
         val=values(isub)                         ! retrieve corresponding value for requested entry
         llen=ivalue(isub)                        ! get significant length of value
         ier=0                                    ! indicate requested entry name was successfully found
      else                                        ! entry was not in dictionary
         val=" "                                  ! set value to blank
         llen=0                                   ! set length to zero
         ier=-1                                   ! set error flag to indicate requested entry was not found
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine retrev
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine string_to_dble(chars,value8,ierr)
! "@(#)string_to_dble(3f): returns double precision value from numeric character string"
                                                  !      works with any g-format input, including integer, real, and exponential.
      character(len=*),intent(in)  :: chars       ! string assumed to represent a numeric value
      real(kind=k_dbl),intent(out) :: value8      ! double precision value to return; set to zero on error.
      integer,intent(out)          :: ierr        ! if an error occurs in the read, a non-zero value is returned.
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=13)            :: frmt        ! FORMAT to use to read the value from the string
      integer                      :: ios         ! error flag returned from internal READ
!-----------------------------------------------------------------------------------------------------------------------------------
      ierr=0                                                     ! initialize the error flag
!-----------------------------------------------------------------------------------------------------------------------------------
      write(unit=frmt,fmt="( ""(bn,g"",i5,"".0)"" )")len(chars)  ! build FORMAT to read the value based on length of input string
      read(unit=chars,fmt=frmt,iostat=ios)value8                 ! read the value from the string using an internal read
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ios /= 0 )then                                          ! if an error occurred in reading from the string report it
         value8=0.0_k_dbl                                        ! set the returned value to zero on error
         call send_message("*string_to_dble* - cannot produce number from this string["//trim(chars)//"]")
         ierr=ios
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine string_to_dble
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine string_to_real(chars,valu,ierr)
! "@(#)string_to_real(3f): returns real value from numeric character string"
      character(len=*),intent(in)  :: chars
      real,intent(out)             :: valu
      integer,intent(out)          :: ierr
      real(kind=k_dbl)             :: valu8
!-----------------------------------------------------------------------------------------------------------------------------------
      call string_to_dble(chars,valu8,ierr)         ! get value as double precision and stuff into a real variable
      valu=real(valu8)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine string_to_real
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function dget(keyword)
! "@(#)dget(3f): given keyword fetch value from Language Dictionary as a dble (zero on error)"
   real(kind=dp)               :: dget              ! function type
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in) :: keyword           ! keyword to retrieve value for from dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=IPvalue)      :: value             ! value returned
   integer                     :: llen              ! length of value found
   integer                     :: ier               ! error flag on call to retrieve value
   real(kind=dp)               :: a8                ! number to return
!-----------------------------------------------------------------------------------------------------------------------------------
   value=" "                                        ! initialize value found for keyword in case an error occurs
   call retrev(keyword, value, llen, ier)           ! find value associated with keyword
   call string_to_dble(value(:llen), a8, ier)       ! convert the string to a numeric value
   dget = a8
!-----------------------------------------------------------------------------------------------------------------------------------
end function dget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function rget(keyword)
! "@(#)rget(3f): given keyword, fetch single real value from language dictionary (zero on error)"
!-----------------------------------------------------------------------------------------------------------------------------------
   real                        :: rget             ! function type
   character(len=*),intent(in) :: keyword          ! keyword to retrieve value for from dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
   rget=real(dget(keyword))                        ! just call DGET(3f) but change returned value to type REAL
!-----------------------------------------------------------------------------------------------------------------------------------
end function rget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function iget(keyword)
! "@(#)iget(3f): given keyword, fetch integer value from language dictionary (zero on error)"
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: iget            ! function type
   character(len=*),intent(in)  :: keyword         ! keyword to retrieve value for from dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
   iget = int(dget(keyword))                       ! just call DGET(3f) but change returned value to type INTEGER
!-----------------------------------------------------------------------------------------------------------------------------------
end function iget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function lget(keyword)
! "@(#)lget(3f): given keyword, fetch logical value from language dictionary (zero on error)"
!-----------------------------------------------------------------------------------------------------------------------------------
   logical                      :: lget            ! procedure type
   character(len=*),intent(in)  :: keyword         ! the dictionary keyword (in form VERB_KEYWORD) to retrieve
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=IPvalue)       :: value           ! value corresponding to the requested keyword
   integer                      :: llen            ! length of VALUE returned by RETREV(3f)
   integer                      :: ier             ! flag returned by RETREV(3f) indicating if an error occurred in retrieving value
!-----------------------------------------------------------------------------------------------------------------------------------
   lget=.false.                                    ! initialize return value to .false.
   call retrev(keyword, value, llen, ier)          ! get value for corresponding keyword from language dictionary
                                                   ! report on error ????
   value=adjustl(uppers(value,llen))               ! convert value to uppercase, left spaces trimmed
!-----------------------------------------------------------------------------------------------------------------------------------
   if(value(:llen).ne."#N#")then
      select case(value(1:1))                      ! check first letter
      case('T','Y',' ')                            ! anything starting with "T" or "Y" or a blank is TRUE (true,t,yes,y,...)
         lget=.true.
      case('F','N')                                ! assume this is false or no
         lget=.false.
      case('.')                                    ! looking for fortran logical syntax .STRING.
         select case(value(2:2))
         case('T')                                 ! assume this is .t. or .true.
            lget=.true.
         case('F')                                 ! assume this is .f. or .false.
            lget=.false.
         case default
            call send_message("*lget* bad logical expression for "//keyword(:len_trim(keyword))//'='//value(:llen))
         end select
      case default
            call send_message("*lget* bad logical expression for "//keyword(:len_trim(keyword))//'='//value(:llen))
      end select
   else                                            ! special value "#N#" is assumed FALSE
      lget=.false.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function lget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
character(len=IPvalue) function sget(name,iilen) result(string)
! "@(#)sget(3f): Fetch string value and length of specified NAME the language dictionary"
!     This routine trusts that the desired name exists. A blank is returned if the name is not in the dictionary
      character(len=*),intent(in)   :: name     !  name to look up in dictionary
      integer,intent(out),optional  :: iilen    !  length of returned output string
!-----------------------------------------------------------------------------------------------------------------------------------
      integer                       :: isub     ! index where verb_oo is stored or -1 if this is an unknown name
!-----------------------------------------------------------------------------------------------------------------------------------
      isub=igets(name)                          ! given name return index name is stored at
!-----------------------------------------------------------------------------------------------------------------------------------
      if(isub > 0)then                          ! if index is valid return string
         string=values(isub)
      else                                      ! if index is not valid return blank string
         string(:)=" "
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(present(iilen))then                    ! if iilen is present on call, return the value
         iilen=ivalue(isub)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function sget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function sgetl(name,iilen) result(string)
! "@(#)sgetl(3f): Fetch string value for NAME from language dictionary up to length iilen"
!     This routine trusts that the desired name exists. A blank is returned if the name is not in the dictionary
      character(len=*),intent(in)  :: name        ! name to look up in dictionary
      integer,intent(in)           :: iilen       ! length of returned output string
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=iilen)          :: string
      integer                      :: isub
!-----------------------------------------------------------------------------------------------------------------------------------
      isub=igets(name)                            ! given name return index name is stored at
!-----------------------------------------------------------------------------------------------------------------------------------
      if(isub > 0)then                            ! if index is valid return string
         string=values(isub)
      else                                        ! if index is not valid return blank string
         string(:)=" "
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function sgetl
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine kracken(verb,string,error_return)
! "@(#)kracken(3f): define and parse command line options"
!     get the entire command line argument list and pass it and the
!     prototype to dissect()
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)    :: string
      character(len=*),intent(in)    :: verb
      integer,intent(out),optional   :: error_return
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=IPcmd)           :: command
      integer                        :: iilen
      integer                        :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
      if(present(error_return))error_return=0
!-----------------------------------------------------------------------------------------------------------------------------------
      call get_command_arguments(command,iilen,ier)
      if(ier.ne.0)then
         call send_message("*kracken* could not get command line arguments")
         if(present(error_return))error_return=ier
      else
         call dissect(verb,string,command(:iilen),iilen,ier)
         ! if calling procedure is not testing error flag stop program on error
         if(.not.present(error_return).and.ier.ne.0)then
            call send_message("*kracken* (V 20151212) STOPPING: error parsing arguments")
            stop
         endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine kracken
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine setprompts(verb,init)
! "@(#)setprompts(3f): set explicit prompts for keywords in interactive mode"
      character(len=*),intent(in)  :: verb   ! verb name to define prompts for
      character(len=*),intent(in)  :: init   ! string to define prompts instead of values
      call parse('?'//trim(verb),init,"add") ! initialize command, prefixing verb with question mark character to designate prompts
end subroutine setprompts
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine dissect(verb,init,pars,ipars,error_return)
! "@(#)dissect(3f): convenient call to parse() -- define defaults, then process"
!
      character(len=*),intent(in)  :: verb            ! the name of the command to be reset/defined  and then set
      character(len=*),intent(in)  :: init            ! used to define or reset command options; usually hard-set in the program.
      character(len=*),intent(in)  :: pars            ! defines the command options to be set, usually from a user input file
      integer,intent(in)           :: ipars           ! length of the user-input string pars.
      integer,intent(out),optional :: error_return
!-----------------------------------------------------------------------------------------------------------------------------------
      integer                      :: ier
      character(len=IPvalue)       :: varvalue        ! value of environment variable
      integer                      :: ipars2
!-----------------------------------------------------------------------------------------------------------------------------------
      call store(trim(verb)//'_?','.false.',"add",ier)  ! all commands have the option -? to invoke prompt mode
      call parse(trim(verb),init,"add") ! initialize command
!-----------------------------------------------------------------------------------------------------------------------------------
      ! if environment variable DEFAULT_verbname is set apply it as defaults to define _verb values
      ! for programs that want to determine the values set by the command definition and the variable
      ! before user selections are applied
      call parse('_'//trim(verb),init,"add") ! initialize _command
      call get_environment_variable('DEFAULT_'//trim(verb),varvalue)
      call parse('_'//trim(verb),trim(varvalue),"no_add") ! process and store as _CMD_VERB for appending
!-----------------------------------------------------------------------------------------------------------------------------------
      if(varvalue.ne.' ')then
         call parse(verb,trim(varvalue),"no_add")            ! process environment variable
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ipars <= 0)then
         ipars2=len(pars(:ipars))
      else
         ipars2=ipars
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call parse(verb,pars(:ipars2),"no_add",ier) ! process user command options
      if(lget(trim(verb)//'_?'))then    ! if -? option was present prompt for values
         call menu(verb)
      endif
      if(present(error_return))error_return=ier
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine dissect
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine parse(verb,string,allow,error_return)
! "@(#)parse(3f,private): parse user command and store tokens into Language Dictionary"
!!!   set up odd for future expansion
!!!   need to handle a minus followed by a blank character
!-----------------------------------------------------------------------------------------------------------------------------------
!     given a string of form
!         verb  -keyword1 value1 -keyword2 value2 ...
!     define three arrays of the form
!     verb_keyword(i) : value(i)  : len_trim(value(i))
!     -keyword(i) will become verb__keyword(i)
!
!     values may be in double quotes.
!     if tokens contain alphameric characters an unquoted # signifies the rest of the line is a comment.
!     adjacent double quotes put one double quote into value
!     processing ends when an end of string is encountered
!     the variable name for the first value is verb_oo
!     call it once to give defaults
!     leading and trailing blanks are removed from values
!
!-----------------------------------------------------------------------------------------------------------------------------------
! @(#)parse+ for left-over command string for Language routines
!     optionally needed if you are going to allow multiple commands on a line
      ! number of characters left over,
      ! number of non-blank characters in actual parameter list
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)          :: verb
      character(len=*),intent(in)          :: string
      character(len=*),intent(in)          :: allow
      character(len=IPvalue+2)             :: dummy
      character(len=IPvalue),dimension(2)  :: var
      character(len=3)                     :: delmt
      character(len=2)                     :: init
      character(len=1)                     :: currnt
      character(len=1)                     :: prev
      character(len=1)                     :: forwrd
      character(len=IPvalue)               :: val
      character(len=IPverb)                :: name
      integer,dimension(2)                 :: ipnt
      integer                              :: ilist
      integer                              :: ier
      integer,optional,intent(out)         :: error_return
      integer                              :: islen
      integer                              :: ipln
      integer                              :: ipoint
      integer                              :: itype
      integer                              :: ifwd
      integer                              :: ibegin
      integer                              :: iend
!-----------------------------------------------------------------------------------------------------------------------------------
      ilist=1
      init="oo"
      ier=0
      if(present(error_return)) error_return=0
      islen=len_trim(string)   ! find number of characters in input string
      ! if input string is blank, even default variable will not be changed
      if(islen  ==  0)then
         return
      endif
      dummy=string             ! working mutable copy of STRING
      ipln=len_trim(verb)      ! find number of characters in verb prefix string
!-----------------------------------------------------------------------------------------------------------------------------------
      var(2)=init         ! initial variable name
      var(1)=" "          ! initial value of a string
      ipoint=0            ! ipoint is the current character pointer for (dummy)
      ipnt(2)=2           ! pointer to position in parameter name
      ipnt(1)=1           ! pointer to position in parameter value
      itype=1             ! itype=1 for value, itype=2 for variable
!-----------------------------------------------------------------------------------------------------------------------------------
      delmt="off"
      prev=" "
!-----------------------------------------------------------------------------------------------------------------------------------
      do
      ipoint=ipoint+1               ! move current character pointer forward
      currnt=dummy(ipoint:ipoint)   ! store current character into currnt
      ifwd=min(ipoint+1,islen)
      forwrd=dummy(ifwd:ifwd)       ! next character (or duplicate if last)
!-----------------------------------------------------------------------------------------------------------------------------------
      if((currnt=="-".and.prev==" ".and.delmt == "off".and.index("0123456789.",forwrd) == 0).or.ipoint > islen)then
      ! beginning of a parameter name
         if(forwrd.eq.'-')then         ! change --var to -var so "long" syntax is supported
            dummy(ifwd:ifwd)='_'
            ipoint=ipoint+1            ! ignore second - instead
         endif
         if(ipnt(1)-1 >= 1)then
            ibegin=1
            iend=len_trim(var(1)(:ipnt(1)-1))
            do
               if(iend  ==  0)then   !len_trim returned 0, parameter value is blank
                  iend=ibegin
                  exit
               else if(var(1)(ibegin:ibegin) == " ")then
                  ibegin=ibegin+1
               else
                  exit
               endif
            enddo
            name=verb(:ipln)//"_"//var(2)(:ipnt(2))
            val=var(1)(ibegin:iend)
            if(var(2)(:ipnt(2)).eq.'oo'.and.allow.ne.'add'.and.val.eq.'')then
               ! do not allow a blank value to override initial value so can have default
            else
               call store(name,val,allow,ier)       ! store name and it's value
            endif
            if(present(error_return).and.ier.ne.0)error_return=ier
         else
            name=verb(:ipln)//"_"//var(2)(:ipnt(2))
            val=" "                                 ! store name and null value
            call store(name,val,allow,ier)
            if(present(error_return).and.ier.ne.0)error_return=ier
         endif
         ilist=ilist+ipln+1+ipnt(2)
         ilist=ilist+1
         itype=2                          ! change to filling a variable name
         var(1)=" "                       ! clear value for this variable
         var(2)=" "                       ! clear variable name
         ipnt(1)=1                        ! restart variable value
         ipnt(2)=1                        ! restart variable name
!-----------------------------------------------------------------------------------------------------------------------------------
      elseif(currnt == kracken_comment.and.delmt == "off")then   ! rest of line is comment
         islen=ipoint
         dummy=" "
         prev=" "
         cycle
!-----------------------------------------------------------------------------------------------------------------------------------
      ! rest of line is another command(s)
         islen=ipoint
         dummy=" "
         prev=" "
         cycle
!-----------------------------------------------------------------------------------------------------------------------------------
      else       ! currnt is not one of the special characters
         ! the space after a keyword before the value
         if(currnt == " ".and.itype  ==  2)then
            ! switch from building a keyword string to building a value string
            itype=1
         ! beginning of a delimited parameter value
         elseif(currnt  ==  """".and.itype  ==  1)then
            ! second of a double quote, put quote in
            if(prev  ==  """")then
                var(itype)(ipnt(itype):ipnt(itype))=currnt
                ipnt(itype)=ipnt(itype)+1
                delmt="on"
            elseif(delmt  ==  "on")then     ! first quote of a delimited string
                delmt="off"
            else
                delmt="on"
            endif
         else     ! add character to current parameter name or parameter value
            var(itype)(ipnt(itype):ipnt(itype))=currnt
            ipnt(itype)=ipnt(itype)+1
            if(currnt /= " ")then
            endif
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      prev=currnt
      if(ipoint <= islen)then
         cycle
      endif
      exit
      enddo
end subroutine parse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine store(name1,value1,allow1,ier)
! "@(#)store(3f,private): replace dictionary name's value (if allow='add' add name if necessary)"
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)        :: name1       ! name in dictionary of from VERB_KEYWORD
      character(len=*),intent(in)        :: value1      ! value to be associated to NAME1
      character(len=*),intent(in)        :: allow1      ! flag to allow new VERB_KEYWORD name being added
      integer,intent(out)                :: ier         ! flag if error occurs in adding or setting value
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=IPverb)              :: name
      integer                            :: indx
      character(len=10)                  :: allow
      character(len=IPvalue)             :: value
      character(len=IPvalue)             :: mssge       !  the  message/error/string  value
      integer                            :: nnlen
      integer                            :: new
      integer                            :: ii
      integer                            :: i10
!-----------------------------------------------------------------------------------------------------------------------------------
      value=" "
      name=" "
      allow=" "
      name=name1                                        ! store into a standard size variable for this type
      value=value1                                      ! store into a standard size variable for this type
      allow=allow1                                      ! store into a standard size variable for this type
      nnlen=len(name1)
!-----------------------------------------------------------------------------------------------------------------------------------
      call bounce(name,indx,dict_verbs,ier,mssge)       ! determine storage placement of the variable and whether it is new
      if(ier  ==  -1)then                               ! an error occurred in determining the storage location
         call send_message("error occurred in *store*")
         call send_message(mssge)
         return
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(indx > 0)then                                  ! found the variable name
         new=1
      else if(indx <= 0.and.allow  ==  "add")then       ! check if the name needs added
         call add_string(name,nnlen,indx,ier)           ! adding the new variable name in the variable name array
         if(ier  ==  -1)then
            call send_message("*store* could not add "//name(:nnlen))
            call send_message(mssge)
            return
         endif
         new=0
!-----------------------------------------------------------------------------------------------------------------------------------
      else                                              ! did not find variable name but not allowed to add it
         ii=index(name,"_")
         call send_message("########################################################")
         call send_message("error: UNKNOWN OPTION -"//name(ii+1:))
         if(ii > 0)then
            call send_message(name(:ii-1)//" parameters are")
            do i10=1,IPic
               if(name(:ii)  ==  dict_verbs(i10)(:ii))then
                  if(dict_verbs(i10)(ii:ii+1).eq.'__')then
                     call send_message(" --"//dict_verbs(i10)(ii+2:len_trim(dict_verbs(i10)))//" "//values(i10)(:ivalue(i10)))
                  else
                     call send_message(" -"//dict_verbs(i10)(ii+1:len_trim(dict_verbs(i10)))//" "//values(i10)(:ivalue(i10)))
                  endif
               endif
            enddo
         endif
         call send_message("########################################################")
         ier=-10
         return
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      values(iabs(indx))=value               ! store a defined variable's value
      ivalue(iabs(indx))=len_trim(value)     ! store length of string
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine store
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine bounce(varnam,indx,dictionary,ier,mssge)
! "@(#)bounce(3f,private): find index in Language Dictionary where VARNAM can be found"
!
!     If VARNAM is not found report where it should be placed as a NEGATIVE index number.
!     Assuming DICTIONARY is an alphabetized array
!     Assuming all variable names are lexically greater than a blank string.
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)                :: varnam      ! variable name to look up in dictionary
      integer,intent(out)                        :: indx        ! location where variable is or should be
      character(len=*),dimension(:),intent(in)   :: dictionary  ! sorted dictionary array to find varnam in
      integer,intent(out)                        :: ier
      character(len=*),intent(out)               :: mssge
!-----------------------------------------------------------------------------------------------------------------------------------
      integer                                    :: maxtry      ! maximum number of tries that should be required
      integer                                    :: imin
      integer                                    :: imax
      integer                                    :: i10
!-----------------------------------------------------------------------------------------------------------------------------------
      maxtry=int(log(float(IPic))/log(2.0)+1.0)                 ! calculate max number of tries required to find a conforming name
      indx=(IPic+1)/2
      imin=1
      imax=IPic
!-----------------------------------------------------------------------------------------------------------------------------------
      do i10=1,maxtry
         if(varnam  ==  dictionary(indx))then
            return
         else if(varnam > dictionary(indx))then
            imax=indx-1
         else
            imin=indx+1
         endif
         if(imin > imax)then
            indx=-imin
            if(iabs(indx) > IPic)then
               mssge="error 03 in bounce"
               ier=-1
               return
            endif
            return
         endif
         indx=(imax+imin)/2
         if(indx > IPic.or.indx <= 0)then
            mssge="error 01 in bounce"
            ier=-1
            return
         endif
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      mssge="error 02 in bounce"
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine bounce
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine add_string(newnam,nchars,indx,ier)
! "@(#)add_string(3f,private): Add new string name to Language Library dictionary"
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      character(len=*),intent(in)       :: newnam     ! new variable name to add to dictionary
      integer,intent(in)                :: nchars     ! number of characters in NEWNAM
      integer,intent(in)                :: indx
      integer,intent(out)               :: ier
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      integer                           :: istart
      integer                           :: i10
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      istart=iabs(indx)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!     if last position in the name array has already been used, then report no room is left and set error flag and error message.
      if(dict_verbs(IPic) /= " ")then                 ! check if dictionary full
         call send_message("*add_string* no room left to add more string variable names")
         ier=-1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      elseif(istart.gt.IPic)then
         call send_message("*add_string* dictionary size exceeded")
         ier=-1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      else
         do i10=IPic-1,istart,-1
!           pull down the array to make room for new value
            values(i10+1)=values(i10)
            ivalue(i10+1)=ivalue(i10)
            dict_verbs(i10+1)=dict_verbs(i10)
         enddo
         values(istart)=" "
         ivalue(istart)= 0
         dict_verbs(istart)=newnam(1:nchars)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
end subroutine add_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function igets(chars0)
! "@(#)igets(3f,private): return the subscript value of a string when given it's name"
!     WARNING: only request value of names known to exist
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)        :: chars0
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=IPverb)              :: chars
      character(len=IPvalue)             :: mssge
      integer                            :: ierr
      integer                            :: indx
      integer                            :: igets
!-----------------------------------------------------------------------------------------------------------------------------------
      chars=chars0
      ierr=0
      indx=0
      call bounce(chars,indx,dict_verbs,ierr,mssge)             ! look up position
!-----------------------------------------------------------------------------------------------------------------------------------
      if((ierr  ==  -1).or.(indx <= 0))then
         call send_message("*igets* variable "//trim(chars)//" undefined")
         igets=-1                                                ! very unfriendly subscript value
      else
         igets=indx
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function igets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine delim(line0,array,n,iicount,ibegin,iterm,iilen,dlim)
! "@(#)delim(3f): parse a string and store tokens into an array"
!
!     given a line of structure " par1 par2 par3 ... parn "
!     store each par(n) into a separate variable in array.
!
!     IF ARRAY(1) = '#N#' do not store into string array  (KLUDGE))
!
!     also icount number of elements of array initialized, and
!     return beginning and ending positions for each element.
!     also return position of last non-blank character (even if more
!     than n elements were found).
!
!     no quoting of delimiter is allowed
!     no checking for more than n parameters, if any more they are ignored
!
      character(len=*),intent(in)                 :: line0
      integer,intent(in)                          :: n
      !character(len=*),dimension(n),intent(out)  :: array
      character(len=*),dimension(:),intent(out)   :: array
      integer,intent(out)                         :: iicount
      !integer,dimension(n),intent(out)           :: ibegin
      integer,dimension(:),intent(out)            :: ibegin
      !integer,dimension(n),intent(out)           :: iterm
      integer,dimension(:),intent(out)            :: iterm
      integer,intent(out)                         :: iilen
      character(len=*),intent(in)                 :: dlim
      character(len=IPcmd)                        :: line
      logical                                     :: lstore
      integer                                     :: idlim
      integer                                     :: icol
      integer                                     :: iarray
      integer                                     :: istart
      integer                                     :: iend
      integer                                     :: i10
      integer                                     :: ifound
      iicount=0
      iilen=len_trim(line0)
      if(iilen > IPcmd)then
         call send_message("*delim* input line too long")
      endif
      line=line0
      idlim=len(dlim)
      if(idlim > 5)then
         idlim=len_trim(dlim)                                ! dlim a lot of blanks on some machines if dlim is a big string
         if(idlim  ==  0)then
            idlim=1  ! blank string
         endif
      endif
!     command was totally blank
      if(iilen  ==  0)then
         return
      endif
!     there is at least one non-blank character in the command
!     iilen is the column position of the last non-blank character
!     find next non-delimiter
      icol=1
      if(array(1)  ==  "#N#")then                            ! special flag to not store into character array
         lstore=.false.
      else
         lstore=.true.
      endif
      do iarray=1,n,1                                        ! store into each array element until done or too many words
         if(index(dlim(1:idlim),line(icol:icol))  ==  0)then ! if current character is not a delimiter
           istart=icol                                       ! start new token on the non-delimiter character
           ibegin(iarray)=icol
           iend=iilen-istart+1+1                             ! assume no delimiters so put past end of line
           do i10=1,idlim
              ifound=index(line(istart:iilen),dlim(i10:i10))
              if(ifound > 0)then
                iend=min(iend,ifound)
              endif
           enddo
            if(iend <= 0)then                                ! no remaining delimiters
              iterm(iarray)=iilen
              if(lstore)then
                 array(iarray)=line(istart:iilen)
              endif
              iicount=iarray
              return
            else
              iend=iend+istart-2
              iterm(iarray)=iend
              if(lstore)then
                 array(iarray)=line(istart:iend)
              endif
            endif
           icol=iend+2
         else
           icol=icol+1
           cycle
         endif
   !     last character in line was a delimiter, so no text left
   !     (should not happen where blank=delimiter)
         if(icol > iilen)then
           iicount=iarray
           return
         endif
      enddo
!     more than n elements
      iicount=n
end subroutine delim
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine send_message(msg)
! "@(#)send_message(3f,private): general message routine"
      character(len=*),intent(in) :: msg                      ! message to display
      print "(""# "",a)", msg(:len_trim(msg))               ! write message
end subroutine send_message
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine get_command_arguments(string,istring_len,istatus)
! "@(#)get_command_arguments(3f,private): return all command arguments as a string"
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(out) :: string                     ! string of all arguments to create
   integer,intent(out)          :: istring_len                ! last character position set in output string
   integer,intent(out)          :: istatus                    ! status (non-zero means error)
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: istring_len_new            ! length of output if append next argument
   integer                      :: string_len                 ! allowed length of output string
   integer                      :: ilength                    ! length of individual arguments
   integer                      :: i                          ! loop count
   character(len=IPvalue)       :: value                      ! store individual arguments one at a time
   integer                      :: ifoundspace
!-----------------------------------------------------------------------------------------------------------------------------------
   string=""                                                  ! initialize returned output string
   string_len=len(string)                                     ! find out how big the output string can be
   istring_len=0                                              ! initialize returned output string length
   istatus=0                                                  ! initialize returned error code
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,command_argument_count()                            ! append any arguments together
      call get_command_argument(i,value,ilength,istatus)      ! get next argument
      istring_len_new=istring_len+ilength+1                   ! calculate total string length plus one for a separator
      !---------------------
      ! BEGIN GUESS AT RE-QUOTING STRING
      !---------------------
      ! if argument contains a space and does not contain a double-quote and is short enough to have double quotes added
      ! assume this argument was quoted but that the shell stripped the quotes and add double quotes. This is an optional
      ! behavior and assumes an operating system that strips the quotes from quoted strings on the command line. If the
      ! operating system is smarter than that remove this section
      if(ilength.gt.0)then
         ifoundspace=index(value(:ilength),' ')
         if(index(value(:ilength),' ').ne.0.and.index(value(:ilength),'"').eq.0)then
            ilength=ilength+2
            if(ilength.le.len(value))then
               value='"'//value(:ilength)//'"'
            endif
         endif
      endif
      !---------------------
      ! END GUESS AT RE-QUOTING STRING
      !---------------------
      if(ilength.gt.len(value))then
         call send_message('*get_command_arguments* argument too big')
         stop
      elseif(istatus /= 0) then                               ! stop appending on error
         call send_message('*get_command_arguments* error obtaining argument')
         stop
      elseif(istring_len_new.gt.string_len)then               ! not enough room to store argument
         call send_message('*get_command_arguments* output too long, command trimmed')
         stop
      endif
      string=string(:istring_len)//value(:ilength)            ! append strings together
      istring_len=istring_len_new
   enddo
   istring_len=len_trim(string)                               ! keep track of length and so do not need to use len_trim
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine get_command_arguments
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function uppers(input_string,output_size) result (output_string)
! "@(#)uppers(3f,private): return copy of input string converted to uppercase"
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in) :: input_string         ! input string to convert to uppercase
      integer,intent(in)          :: output_size          ! size of output string
      character(len=output_size)  :: output_string        ! output string converted to uppercase
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=1)            :: letter               ! current letter
      integer                     :: ilet                 ! ADE (ASCII Decimal Equivalent) of current letter
      integer                     :: icount               ! counter used to increment thru the input string
!-----------------------------------------------------------------------------------------------------------------------------------
      if(len_trim(input_string).gt.output_size)then       ! warn that length of input longer than length of output
         call send_message("*uppers* - input string longer than output string")
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      output_string=" "                                   ! initialize output string to all blanks
!-----------------------------------------------------------------------------------------------------------------------------------
      do icount=1,min(output_size,len_trim(input_string)) ! loop thru input one character at a time up to length of output string
         letter=input_string(icount:icount)               ! extract next letter
         ilet=ichar(letter)                               ! get integer ADE (ASCII Decimal Equivalent) of letter
                                                          ! NOTE: lowercase a-z in ASCII is an ADE of 97 to 122
                                                          !       uppercase A-Z in ASCII is an ADE of 65 to 90
         if((ilet >= 97) .and.(ilet <= 122))then          ! find if current letter is a lowercase letter
            output_string(icount:icount)=char(ilet-32)    ! convert lowercase a-z to uppercase A-Z and store into output string
         else                                             ! character is not a lowercase letter, just put it in output
            output_string(icount:icount)=letter           ! store character as-is
         endif
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end function uppers
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine menu(verb)
! "@(#)menu(3f,private): prompt for values using a menu interface"
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in)    :: verb
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=IPvalue)         :: reply
   character(len=IPvalue)         :: prompt
   integer                        :: ii
   integer                        :: icount
   integer                        :: ios
   integer                        :: i10
   integer                        :: i20
   integer                        :: istart
   integer                        :: iend
   integer                        :: ifound
   integer                        :: ireply
   real                           :: valu
   integer                        :: ierr
   integer                        :: indx
   character(len=IPvalue)         :: mssge   !  the message/error/string  value returned by BOUNCE(3f)
!-----------------------------------------------------------------------------------------------------------------------------------
   ii=len_trim(verb)
   write(*,*)verb(:ii)//" parameters are"
   istart=1
!-----------------------------------------------------------------------------------------------------------------------------------
   INFINITE: do
      icount=0                                                ! how many entries in the dictionary belong to this command
      iend=IPic                                               ! last dictionary entry to search for current command
      MAKEMENU: do i10=istart,iend                            ! search dictionary for keywords for current command
         if(verb(:ii)//'_'  ==  dict_verbs(i10)(:ii+1))then   ! found part of the desired command
            if(istart.eq.0)istart=i10                         ! store index to the beginning of this command
            icount=icount+1                                   ! count keywords that start with VERB_
            if(dict_verbs(i10).eq.verb(:ii)//'_?')then        ! do not show the keyword VERB_?
               cycle MAKEMENU
            endif
            call bounce('?'//dict_verbs(i10),indx,dict_verbs,ierr,mssge) ! if ?VERB is defined assume it is a prompt
            if(indx.gt.0)then
               prompt=values(indx)
            else
               prompt=' '
            endif
            if(prompt.eq.'')then
               write(*,'(i4,")",a,a)') i10,dict_verbs(i10)(ii+2:),trim(values(i10)(:ivalue(i10)))
            elseif(prompt.eq.'#N#')then                       ! special prompt value which means to skip prompting
            else
               write(*,'(i4,")",a,":[",a,"]")') i10,trim(prompt),trim(values(i10))
            endif
         endif
      enddo MAKEMENU
      iend=icount+istart-1                                 ! no need to go thru entire dictionary on subsequent passes
!-----------------------------------------------------------------------------------------------------------------------------------
      write(*,'(a)',advance='no')'Enter number of parameter to change(0 to finish):'
      read(*,'(a)',iostat=ios)reply
      reply=adjustl(reply)
      valu=-1
!-----------------------------------------------------------------------------------------------------------------------------------
      if(reply(1:1).eq.'-')then            ! assume this is the beginning of a respecification of options using -keyword value ...
         call parse(verb,trim(reply)//' -? .false.',"no_add")
         cycle INFINITE
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      select case(REPLY)
!-----------------------------------------------------------------------------------------------------------------------------------
      case('@DUMP')                                   ! debug option to dump dictionary
         do i20=1,IPic
            if(dict_verbs(i20).ne.' ')then
                 write(*,'(a,a,a)')i20,dict_verbs(i20),':',trim(values(i20)(:ivalue(i20)))
            endif
         enddo
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('.','q')
         stop
         !exit INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('?')
         write(*,*)'--------------------------------------------------------------------------------'
         write(*,*)' Enter '
         write(*,*)'   o  NNN                   the number of the option to change the value for'
         write(*,*)'   o  "-keyword value ..."  to respecify values'
         write(*,*)'   o  ?                     display this help'
         write(*,*)'   o  .                     stop the program'
         write(*,*)''
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case default
         call string_to_real(reply,valu,ierr)              ! try to convert to a number
      end select
!-----------------------------------------------------------------------------------------------------------------------------------
      ireply=int(valu)
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ireply.eq.0)then
         exit INFINITE
      elseif((valu.lt.istart).or.(valu.gt.iend))then
         write(*,*)'illegal menu choice ',istart,'<=',valu,'<=',iend
!-----------------------------------------------------------------------------------------------------------------------------------
      else
         ifound=ireply                                     ! index into dictionary for requested keyword and value
         if(dict_verbs(ifound).eq.verb(:ii)//'_?')then     ! replaced this with FINISHED so exit
            exit INFINITE
         endif
         call bounce('?'//dict_verbs(ifound),indx,dict_verbs,ierr,mssge) ! if ?VERB is defined assume it is a prompt
         if(indx.gt.0)then
            prompt=values(indx)
         else
            prompt=' '
         endif
         if(prompt.eq.'')then
            write(*,'("Enter value for ",a,":")',advance='no') trim(dict_verbs(ifound)(ii+2:))
         elseif(prompt.eq.'#N#')then                       ! special prompt value
         else
            write(*,'(a,":")',advance='no') trim(prompt)
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
         read(*,'(a)',iostat=ios)reply
         call store(dict_verbs(ifound),reply,"no_add",ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
      endif
   enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine menu
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! YOUR COMPILER MUST SUPPORT ALLOCATABLE SCALAR CHARACTER VARIABLES BEFORE
! ACTIVATING THIS CODE.
!-----------------------------------------------------------------------------------------------------------------------------------
! From: "Felix Becker" <felix.becker@zih.tu-dresden.de>
! Subject: Variable-length string interface for "retrev".
! Date: Tuesday, May 28, 2013 11:51 AM
! 
! Hello John Urban,
! 
! using your Fortran "M_kracken95" module, I wrote a small wrapper that
! allows for "val" to be of unknown length, and that allows just getting
! the length of val without getting val and vice versa.
! 
! (using allocatable strings, and optional arguments).
! 
! Tested with gfortran version '4.8.0 20130502 (prerelease)'.
! 
! I am aware that M_kracken95 itself uses fixed length strings, and I did
! not fiddle with that; just providing a more flexible user interface on
! top of that.
! 
! Find my quick hack attached, use it if you want.
! 
!-----------------------------------------------------------------------------------------------------------------------------------
!-!subroutine retrev_string_variable_length(name,val,len,ier)
!-!  !!! @(#)retrev_string_variable_length: A wrapper for "retrev" from the module "M_kracken95":
!-!  !!!   - allows for the length of 'val' not being known before
!-!  !!!   - allows for 'val', 'len' and 'ier' being optional.
!-!  !!!
!-!  !!! If present: 'val' must be allocatable (i.e. not an "ordinary"
!-!  !!!   'character(len=[...])'-variable).
!-!  !!! If present and allocated: 'val' gets deallocated and then re-allocated.
!-!  !!!
!-!  !!! Tested with gfortran version '4.8.0 20130502 (prerelease)'.
!-!  character(len=*),intent(in)                       ::  name
!-!  character(len=:),intent(out),allocatable,OPTIONAL ::  val
!-!  integer,intent(out),OPTIONAL                      ::  llen
!-!  integer,intent(out),OPTIONAL                      ::  ier
!-!  
!-!  integer                                           ::  len_internal
!-!  integer                                           ::  ier_internal
!-!  character(llen=0)                                 ::  dummystring
!-!  
!-!  call retrev(name,dummystring,len_internal,ier_internal)
!-!  
!-!  if (present(val)) then
!-!    if (allocated(val)) then
!-!      deallocate(val)
!-!    end if
!-!    call allocate_string(int(len_internal,kind=8),val)
!-!    call retrev(name,val,len_internal,ier_internal)
!-!  end if
!-!  
!-!  if (present(llen)) then
!-!    llen = len_internal
!-!  end if
!-!  
!-!  if (present(ier)) then
!-!    ier = ier_internal
!-!  end if
!-!end subroutine retrev_string_variable_length
!-----------------------------------------------------------------------------------------------------------------------------------
!-!subroutine allocate_string(stringlength,stringvariable)
!-!  !!! For various reasons, we need a subroutine to allocate a character
!-!  !!! of llen=stringlength where stringlength is determined at runtime.
!-!  !!! @(#)allocate_string: allocate string
!-!  integer(kind=8),intent(in)                  :: stringlength
!-!  character(len=:),allocatable,intent(out)    :: stringvariable
!-!  
!-!  allocate(character(len=stringlength) :: stringvariable)
!-!
!-!end subroutine allocate_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_kracken95
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! HISTORY:
! updated 20151212
! allow cmd_oo to have default value
! requote token from the command line if it starts with - and has a space in it to make is possible to pass "-[^0-9-] values
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131224
! minor cleanup
! updated 20131214
! added preliminary setprompts and menu as routines to explore prompting modes.
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131206
! added optional error flag to KRACKEN(3f). If the error flag is not present, an error will cause the program to stop instead of
! always returning to the calling procedure.
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131201
! create name CMD__NAME if --NAME is specified; so --version and --help
! are more easily used
! add dget(3f) function for returning doubleprecision values
! rename parse_two(3f) to dissect(3f) and make it public so input from sources other than
! command line arguments can be parsed easily.
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131029
! read environment variable DEFAULT_CMD
!-----------------------------------------------------------------------------------------------------------------------------------
 
 
!>>>>> build/dependencies/M_io/src/M_io.f90
!===================================================================================================================================
MODULE M_io
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT        ! access computing environment
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit !!, stdout=>output_unit, stderr=>error_unit
implicit none
private
 public uniq
 public print_inquire
 public notopen
 public slurp
 public swallow
 public dirname
 public splitpath
 public fileopen
 public fileclose
 public filedelete
 public get_tmp
 public read_line
 public getline
 public read_table
 public rd
 public separator
 public which
 public getname
 public filewrite
 public basename
public joinpath

!character(len=*),parameter::ident_1="@(#)M_io::read_table(3f): read file containing a table of numeric values"

interface read_table
   module procedure read_table_real, read_table_doubleprecision
end interface

interface string_to_value
   module procedure a2d, a2i
end interface

interface v2s
   module procedure i2s
end interface

!$@(#) M_io::rd(3f): ask for string or number from standard input with user-definable prompt
interface rd
   module procedure rd_character
   module procedure rd_integer
   module procedure rd_real
   module procedure rd_doubleprecision
end interface
CONTAINS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      uniq(3f) - [M_io] append a number to the end of filename to make a unique name if name exists
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!      Usage
!!
!!       character(len=:),allocatable function uniq(name,istart,verbose,create)
!!       character(len=*),intent(in) :: name
!!       integer,intent(in),optional :: istart
!!       logical,intent(in),optional :: verbose
!!       logical,intent(in),optional :: create
!!
!!##DESCRIPTION
!!    Given a filename test if it is in use or exists. If it is, or if it
!!    ends in a period add a number to the end of the name and
!!    test if the new name exists. If necessary, increment the number and
!!    try again up to the value 9999999. By default an empty file is created
!!    if an unused name is found.
!!
!!    o relatively non-generic;
!!    o does not try to detect io errors
!!
!!##OPTIONS
!!    name     base input name used to create output filename
!!             If name ends in "." a numeric suffix is always added.
!!    istart   number to start with as a suffix. Default is 1. Must be a
!!             positive integer less than 9999999.
!!    verbose  writes extra messages to stdout. Defaults to .false.
!!    create   create file if new name is successfully found. Defaults
!!             to .true. .
!!
!!##RETURNS
!!    uniq     A unique filename that is the same as the NAME input parameter
!!             except with a number appended at the end if needed. If could
!!             not find a unique name a blank is returned.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!       program demo_uniq
!!       use M_io, only : uniq
!!       implicit none
!!       character(len=4096) :: myname
!!       integer             :: i
!!          myname=uniq('does_not_exist')
!!          open(unit=10,file='does_exist')
!!          write(*,*)'name stays the same ',trim(myname)
!!          myname=uniq('does_exist')
!!          write(*,*)'name has suffix added ',trim(myname)
!!          do i=1,10
!!             myname=uniq('does_exist')
!!             write(*,*) 'FILENAME:',trim(myname)
!!             open(unit=20+i,file=myname)
!!          enddo
!!       end program demo_uniq
!!
!!    Expected output
!!
!!     name stays the same does_not_exist
!!     name has suffix added does_exist0001
!!     FILENAME:does_exist0002
!!     FILENAME:does_exist0003
!!     FILENAME:does_exist0004
!!     FILENAME:does_exist0005
!!     FILENAME:does_exist0006
!!     FILENAME:does_exist0007
!!     FILENAME:does_exist0008
!!     FILENAME:does_exist0009
!!     FILENAME:does_exist0010
!!     FILENAME:does_exist0011
!!
!!##AUTHOR
!!    John S. Urban, 1993
!!##LICENSE
!! Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
function uniq(name,istart,verbose,create)
implicit none

!character(len=*),parameter::ident_3="&
!&@(#)M_io::uniq(3f): append a number to the end of filename to make a unique name if name exists"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: name
character(len=:),allocatable :: uniq
integer,intent(in),optional  :: istart
logical,intent(in),optional  :: verbose
logical,intent(in),optional  :: create
!-----------------------------------------------------------------------------------------------------------------------------------
logical                     :: around
integer,save                :: icount=1           ! counter to generate suffix from
character(len=4096),save    :: lastname=' '       ! name called with last time the routine was called
integer                     :: ilen
integer                     :: itimes
integer                     :: iscr
integer                     :: ios
logical                     :: verbose_local
logical                     :: create_local
!-----------------------------------------------------------------------------------------------------------------------------------
   uniq=trim(name)                                   ! the input name will be returned if it passes all the tests
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lastname.ne.name)then                          ! if a different input name than last time called reset icount
      lastname=name                                  ! a new name to keep for subsequent calls
      icount=1                                       ! icount is used to make a suffix to add to make the file unique
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(create))then
      create_local=create
   else
      create_local=.true.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(istart))then
      icount=istart                                  ! icount is used to make a suffix to add to make the file unique
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len_trim(name)                               ! find last non-blank character in file name
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ilen.ne.0)then                                 ! a blank input name so name will just be a suffix
      if(name(ilen:ilen).ne.'.')then                 ! always append a number to a file ending in .
         inquire(file=name(:ilen),exist=around)      ! check filename as-is
         if(.not.around)then                         ! file name does not exist, can use it as-is
            uniq=trim(name)
            if(create_local)then
               open(newunit=iscr,file=uniq,iostat=ios,status='new')
               close(unit=iscr,iostat=ios)
            endif
            return
         endif
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   itimes=0                                           ! count number of times tried to get a uniq name
   deallocate(uniq)
   allocate(character(len=ilen+8) :: uniq)            ! make it useable with an internal WRITE(3f) with room for a numeric suffix
   uniq(:)=name
   INFINITE: do                                       ! top of loop trying for a unique name
      if(itimes.ge.9999999)then                       ! if too many tries to be reasonable give up
         write(*,*) '*uniq* unable to find a unique filename. Too many tries'
         uniq=''
         return
      endif
      if(icount.gt.9999999) icount=1                  ! reset ICOUNT when it hits arbitrary maximum value
      if(icount.le.9999)then
         write(uniq(ilen+1:),'(i4.4)')icount          ! create name by adding a numeric string to end
      else
         write(uniq(ilen+1:),'(i7.7)')icount          ! create name by adding a numeric string to end
      endif
      icount=icount+1                                 ! increment counter used to come up with suffix
      inquire(file=uniq,exist=around)                 ! see if this filename already exists
      if(.not.around)then                             ! found an unused name
         if(verbose_local)then
            write(*,*)trim('*uniq* name='//trim(uniq)) ! write out message reporting name used
         endif
         if(create_local)then
            open(newunit=iscr,file=uniq,iostat=ios,status='new')
            close(unit=iscr,iostat=ios)
         endif
         uniq=trim(uniq)
         return                                       ! return successfully
      endif
      itimes=itimes+1                                 ! haven't found a unique name, try again
   enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
end function uniq
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    print_inquire(3f) - [M_io] Do INQUIRE on file by name/number and print results
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   Definition:
!!
!!    subroutine print_inquire(lun)
!!      or
!!    subroutine print_inquire(name)
!!    integer,intent(in),optional          :: lun
!!    character(len=*),intent(in),optional :: name
!!
!!##DESCRIPTION
!!    Given either a Fortran file-unit-number or filename, call the INQUIRE(3f)
!!    intrinsic and print typical status information.
!!
!!##OPTIONS
!!    lun    if lun is not equal to -1 then query by number and ignore
!!           filename even if present
!!    name   if lun = -1  or is not present then query by this filename
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_print_inquire
!!    use M_io, only : print_inquire, fileopen
!!    implicit none
!!    character(len=4096)  :: filename
!!    character(len=20)    :: mode
!!    integer              :: ios
!!    character(len=256)   :: message
!!    integer              :: lun
!!       do
!!          write(*,'(a)',advance='no')'enter filename>'
!!          read(*,'(a)',iostat=ios)filename
!!          if(ios.ne.0)exit
!!          write(*,'(a)',advance='no')'enter mode ([rwa][bt][+]>'
!!          read(*,'(a)',iostat=ios)mode
!!          if(ios.ne.0)exit
!!          lun=fileopen(filename,mode,ios)
!!          if(ios.eq.0)then
!!             write(*,*)'OPENED'
!!          else
!!             write(*,*)'ERROR: IOS=',ios
!!          endif
!!          if(lun.ne.-1)then
!!             call print_inquire(lun,'')
!!             close(lun,iostat=ios,iomsg=message)
!!             if(ios.ne.0)then
!!                write(*,'(a)')trim(message)
!!             endif
!!          endif
!!       enddo
!!    end program demo_print_inquire
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine print_inquire(lun_in,namein_in) ! Version: JSU-1997-12-31, 2020-01-11

!character(len=*),parameter::ident_4="@(#)M_io::print_inquire(3f): Do INQUIRE on file by name/number and print results"

integer,intent(in),optional             :: lun_in        ! if unit >= 0 then query by unit number, else by name
character(len=*),intent(in),optional    :: namein_in
integer                        :: ios
character(len=256)             :: message
character(len=:),allocatable   :: namein
integer                        :: lun
!==============================================================================================
!  ACCESS    =  SEQUENTIAL  |  DIRECT       |  STREAM
!  ACTION    =  READ        | WRITE         |  READWRITE
!  FORM      =  FORMATTED   |  UNFORMATTED
!  POSITION  =  ASIS        |  REWIND       |  APPEND
!  STATUS    =  NEW         |  REPLACE      |  OLD     |  SCRATCH   | UNKNOWN
character(len=20)              :: access         ; namelist/inquire/access
character(len=20)              :: action         ; namelist/inquire/action
character(len=20)              :: asynchronous   ; namelist/inquire/asynchronous
character(len=20)              :: blank          ; namelist/inquire/blank
character(len=20)              :: decimal        ; namelist/inquire/decimal
character(len=20)              :: delim          ; namelist/inquire/delim
character(len=20)              :: direct         ; namelist/inquire/direct
character(len=20)              :: encoding       ; namelist/inquire/encoding
logical                        :: exist          ; namelist/inquire/exist
character(len=20)              :: form           ; namelist/inquire/form
character(len=20)              :: formatted      ; namelist/inquire/formatted
integer                        :: id             ; namelist/inquire/id
character(len=20)              :: name           ; namelist/inquire/name
logical                        :: named          ; namelist/inquire/named
integer                        :: nextrec        ; namelist/inquire/nextrec
integer                        :: number         ; namelist/inquire/number
logical                        :: opened         ; namelist/inquire/opened
character(len=20)              :: pad            ; namelist/inquire/pad
logical                        :: pending        ; namelist/inquire/pending
integer                        :: pos            ; namelist/inquire/pos
character(len=20)              :: position       ; namelist/inquire/position
character(len=20)              :: read           ; namelist/inquire/read
character(len=20)              :: readwrite      ; namelist/inquire/readwrite
integer                        :: recl           ; namelist/inquire/recl
!bug!character(len=20)              :: round          ; !BUG!namelist/inquire/round
character(len=20)              :: sequential     ; namelist/inquire/sequential
!bug!character(len=20)              :: sign           ; !BUG!namelist/inquire/sign
integer                        :: size           ; namelist/inquire/size
character(len=20)              :: stream         ; namelist/inquire/stream
character(len=20)              :: unformatted    ; namelist/inquire/unformatted
character(len=20)              :: write          ; namelist/inquire/write
!==============================================================================================
   namein=merge_str(namein_in,'',present(namein_in))
   lun=merge(lun_in,-1,present(lun_in))
   ! exist, opened, and named always become defined unless an error condition occurs.
   !!write(*,*)'LUN=',lun,' FILENAME=',namein
   !-----------------------------------------------------------------------------------------------------------------------------------
   name=''
   if(namein.eq.''.and.lun.ne.-1)then
         write(*,*)'*print_inquire* checking unit',lun
         inquire(unit=lun,                                                                               &
     &   recl=recl,nextrec=nextrec,pos=pos,size=size,                                                    &
     &   position=position,                                                                              &
     &   name=name,                                                                                      &
     &   form=form,formatted=formatted,unformatted=unformatted,                                          &
     &   access=access,sequential=sequential,direct=direct,stream=stream,                                &
     &   action=action,read=read,write=write,readwrite=readwrite,                                        &
     !BUG!&   sign=sign,                                                                                      &
     !BUG!&   round=round,                                                                                    &
     &   blank=blank,decimal=decimal,delim=delim,encoding=encoding,pad=pad,                              &
     &   named=named,opened=opened,exist=exist,number=number,pending=pending,asynchronous=asynchronous,  &
     &   iostat=ios,err=999,iomsg=message)
    elseif(namein.ne.'')then
         write(*,*)'*print_inquire* checking file:'//namein
         inquire(file=namein,                                                                            &
     &   recl=recl,nextrec=nextrec,pos=pos,size=size,                                                    &
     &   position=position,                                                                              &
     &   name=name,                                                                                      &
     &   form=form,formatted=formatted,unformatted=unformatted,                                          &
     &   access=access,sequential=sequential,direct=direct,stream=stream,                                &
     &   action=action,read=read,write=write,readwrite=readwrite,                                        &
     !BUG!&   sign=sign,                                                                                      &
     !BUG!&   round=round,                                                                                    &
     &   blank=blank,decimal=decimal,delim=delim,encoding=encoding,pad=pad,                              &
     &   named=named,opened=opened,exist=exist,number=number,pending=pending,asynchronous=asynchronous,  &
     &   iostat=ios,err=999,iomsg=message)
     if(name.eq.'')name=namein
    else
       write(*,*)'*print_inquire* must specify either filename or unit number'
    endif
!-----------------------------------------------------------------------------------------------------------------------------------
   write(*,nml=inquire,delim='none')
   return
!-----------------------------------------------------------------------------------------------------------------------------------
999   continue
   write(*,*)'*print_inquire* bad inquire'
!  If an error condition occurs during execution of an INQUIRE  statement,
!  all of the inquiry identifiers except ios become undefined.
   write(*,*)'*print_inquire* inquire call failed,iostat=',ios,'message=',message
end subroutine print_inquire
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    read_table(3f) - [M_io] read file containing a table of numeric values
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine read_table(filename,array,ierr)
!!
!!    character(len=*),intent(in)             :: filename
!!
!!    doubleprecision,allocatable,intent(out) :: array(:,:)
!!    ! or
!!    real           ,allocatable,intent(out) :: array(:,:)
!!
!!    integer,intent(out)                     :: ierr
!!
!!##DESCRIPTION
!!    Read a table from a file that is assumed to be columns of
!!    space-delimited numbers, with each row containing the same
!!    number of values
!!
!!##OPTIONS
!!    filename   filename to read
!!    array      array to create
!!    ierr       zero if no error occurred
!!##EXAMPLES
!!
!!    Sample program, assuming the input file "inputfile" exists:
!!
!!     program demo_read_table
!!     use M_io, only : read_table
!!     doubleprecision,allocatable :: array(:,:)
!!
!!     ! create test file
!!     open(file='inputfile',unit=10)
!!     write(10,'(a)') '1 10  45'
!!     write(10,'(a)') '10 10  45'
!!     write(10,'(a)') '  2 20  15'
!!     write(10,'(a)') ' 20.345 20  15'
!!     write(10,'(a)') '  3 30.111   0'
!!     write(10,'(a)') '30 30e3   0'
!!     write(10,'(a)') '  4 300.444e-1 -10'
!!     write(10,'(a)') '40 30.5555d0 -10'
!!     write(10,'(a)') '  4 300.444E-1 -10'
!!     write(10,'(a)') '40 30.5555D0 -10'
!!     close(unit=10)
!!
!!     ! read file as a table
!!     call read_table('inputfile',array,ierr)
!!
!!     ! print values
!!     write(*,*)'size=       ',size(array)
!!     write(*,*)'size(dim=1)=',size(array,dim=1)
!!     write(*,*)'size=(dim=2)',size(array,dim=2)
!!     do i=1,size(array,dim=1)
!!        write(*,*)array(i,:)
!!     enddo
!!
!!     ! remove sample file
!!     open(file='inputfile',unit=10)
!!     close(unit=10,status='delete')
!!
!!     end program demo_read_table
!!
!!   Results:
!!
!!     size=          30
!!     size(dim=1)=   10
!!     size(dim=2)=    3
!!       1.0000000000000000        10.000000000000000        45.000000000000000
!!       10.000000000000000        10.000000000000000        45.000000000000000
!!       2.0000000000000000        20.000000000000000        15.000000000000000
!!       20.344999999999999        20.000000000000000        15.000000000000000
!!       3.0000000000000000        30.111000000000001        0.0000000000000000
!!       30.000000000000000        30000.000000000000        0.0000000000000000
!!       4.0000000000000000        30.044400000000000       -10.000000000000000
!!       40.000000000000000        30.555499999999999       -10.000000000000000
!!       4.0000000000000000        30.044400000000000       -10.000000000000000
!!       40.000000000000000        30.555499999999999       -10.000000000000000
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine read_table_doubleprecision(filename,array,ierr)
implicit none

character(len=*),intent(in)             :: FILENAME
doubleprecision,allocatable,intent(out) :: array(:,:)
integer,intent(out)                     :: ierr

character(len=1),allocatable :: text(:) ! array to hold file in memory
integer                      :: length
integer                      :: irows
integer                      :: icols
integer                      :: nchars
integer                      :: i
integer                      :: j
integer                      :: k
integer                      :: istart
character(len=:),allocatable :: line

    call slurp(FILENAME,text,lines=irows,length=length) ! allocate character array and copy file into it
    nchars=size(text)
    ierr=0

    if(.not.allocated(text))then
       write(*,*)'*read_table_doubleprecision* failed to load file '//FILENAME
       ierr=-1
    else
       allocate(character(len=length) :: line)
       ! find number of values on first line and assume this is constant
       line(:)=''
       do i=1,nchars
          if(text(i).eq.NEW_LINE('A'))then
             exit
          endif
          if(text(i).eq.char(9))then
             line(i:i)=' '
          else
             line(i:i)=text(i)
          endif
       enddo
       icols=size(s2vs(line))
       allocate(array(irows,icols))

       array=0.0d0
       istart=1
       do j=1,irows
          k=0
          line(:)=''
          do i=istart,nchars
             if(text(i).eq.NEW_LINE('A').or.i.eq.nchars)then
                exit
             endif
             k=k+1
             if(text(i).eq.char(9))then
                line(k:k)=' '
             else
                line(k:k)=text(i)
             endif
          enddo
          istart=i+1
          array(j,:)=s2vs(line)
       enddo

       deallocate(text)  ! release memory
    endif

end subroutine read_table_doubleprecision
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine read_table_real(filename,array,ierr)
implicit none

character(len=*),intent(in)             :: FILENAME
real,allocatable,intent(out) :: array(:,:)
integer,intent(out)                     :: ierr

character(len=1),allocatable :: text(:) ! array to hold file in memory
integer                      :: length
integer                      :: irows
integer                      :: icols
integer                      :: nchars
integer                      :: i
integer                      :: j
integer                      :: k
integer                      :: istart
character(len=:),allocatable :: line

    call slurp(FILENAME,text,lines=irows,length=length) ! allocate character array and copy file into it
    nchars=size(text)
    ierr=0

    if(.not.allocated(text))then
       write(*,*)'*read_table_real* failed to load file '//FILENAME
       ierr=-1
    else
       allocate(character(len=length) :: line)
       ! find number of values on first line and assume this is constant
       line(:)=''
       do i=1,nchars
          if(text(i).eq.NEW_LINE('A'))then
             exit
          endif
          if(text(i).eq.char(9))then
             line(i:i)=' '
          else
             line(i:i)=text(i)
          endif
       enddo
       icols=size(s2vs(line))
       allocate(array(irows,icols))

       array=0.0
       istart=1
       do j=1,irows
          k=0
          line(:)=''
          do i=istart,nchars
             if(text(i).eq.NEW_LINE('A').or.i.eq.nchars)then
                exit
             endif
             k=k+1
             if(text(i).eq.char(9))then
                line(k:k)=' '
             else
                line(k:k)=text(i)
             endif
          enddo
          istart=i+1
          array(j,:)=s2vs(line)
       enddo

       deallocate(text)  ! release memory
    endif

end subroutine read_table_real
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    swallow(3f) - [M_io] read a file into a character array line by line
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine swallow(filename,pageout)
!!
!!    character(len=*),intent(in) :: filename
!!      or
!!    integer,intent(in)          :: io
!!
!!    character(len=1),allocatable,intent(out) :: pageout(:)
!!##DESCRIPTION
!!    Read an entire file into memory as a character array, one character
!!    variable per line.
!!
!!    NOTE:
!!
!!    Never casually read an entire file into memory if you can process it
!!    per line or in smaller units; as large files can consume unreasonable
!!    amounts of memory.
!!
!!##OPTIONS
!!       filename   filename to read into memory, or LUN (Fortran Logical Unit Number).
!!                  If filename is a LUN, file must be opened with
!!
!!                     form='unformatted',access='stream'
!!
!!                  as in
!!
!!                    open(unit=igetunit, file=filename,     &
!!                    & action="read", iomsg=message,        &
!!                    & form="unformatted", access="stream", &
!!                    & status='old',iostat=ios)
!!
!!       pageout    array of characters to hold file
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!    program demo_swallow
!!    use M_io,      only : swallow
!!    implicit none
!!    character(len=4096)          :: FILENAME   ! file to read
!!    character(len=:),allocatable :: pageout(:) ! array to hold file in memory
!!    integer                      :: longest, lines, i, ilen
!!    character(len=:),allocatable :: line
!!       ! get a filename
!!       call get_command_argument(1, FILENAME)
!!       ! allocate character array and copy file into it
!!       call swallow(FILENAME,pageout)
!!       if(.not.allocated(pageout))then
!!          write(*,*)'*demo_swallow* failed to load file '//FILENAME
!!       else
!!          ! write file from last line to first line
!!          longest=len(pageout)
!!          lines=size(pageout)
!!          allocate(character(len=longest)::line)
!!          write(*,*)'number of lines is ',lines
!!          write(*,*)'and length of lines is ',longest
!!          write(*,'(a)')repeat('%',longest+2)
!!          do i=lines,1,-1
!!             write(*,'("%",a,"%")')pageout(i)
!!          enddo
!!          write(*,'(a)')repeat('%',longest+2)
!!          deallocate(pageout)  ! release memory
!!       endif
!!    end program demo_swallow
!!
!!   Given
!!
!!    first line
!!    second line
!!    third line
!!
!!   Expected output
!!
!!     number of lines is 3
!!     and length of lines is 11
!!    %%%%%%%%%%%%%
!!    %third line %
!!    %second line%
!!    %first line %
!!    %%%%%%%%%%%%%
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine swallow(FILENAME,pageout)
implicit none
class(*),intent(in)                      :: FILENAME   ! file to read
character(len=:),allocatable,intent(out) :: pageout(:) ! page to hold file in memory
character(len=1),allocatable             :: text(:)    ! array to hold file in memory

   call slurp(FILENAME,text) ! allocate character array and copy file into it

   if(.not.allocated(text))then
      select type(FILENAME)
       type is (character(len=*)); write(*,*)'*swallow* failed to load file '//FILENAME
       type is (integer);          write(*,'(a,i0)')'*swallow* failed to load file unit ',FILENAME
      end select
   else  ! convert array of characters to array of lines
      pageout=page(text)
      deallocate(text)     ! release memory
   endif

contains
function page(array)  result (table)

!character(len=*),parameter::ident_5="@(#)M_strings::page(3fp): function to copy char array to page of text"

character(len=1),intent(in)  :: array(:)
character(len=:),allocatable :: table(:)
integer                      :: i
integer                      :: linelength
integer                      :: length
integer                      :: lines
integer                      :: linecount
integer                      :: position
integer                      :: sz
!!character(len=1),parameter   :: nl=new_line('A')
character(len=1),parameter   :: nl=char(10)
   lines=0
   linelength=0
   length=0
   sz=size(array)
   do i=1,sz
      if(array(i).eq.nl)then
         linelength=max(linelength,length)
         lines=lines+1
         length=0
      else
         length=length+1
      endif
   enddo
   if(sz.gt.0)then
      if(array(sz).ne.nl)then
         lines=lines+1
      endif
   endif

   allocate(character(len=linelength) :: table(lines))
   table=' '

   linecount=1
   position=1
   do i=1,sz
      if(array(i).eq.nl)then
         linecount=linecount+1
         position=1
      elseif(linelength.ne.0)then
         table(linecount)(position:position)=array(i)
         position=position+1
      endif
   enddo
end function page
end subroutine swallow
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    SLURP(3f) - [M_io] read a file into a character array
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine slurp(filename,text)
!!
!!    character(len=*),intent(in) :: filename
!!     or
!!    integer,intent(in)          :: filenumber
!!
!!    character(len=1),allocatable,intent(out) :: text(:)
!!    integer,intent(out),optional :: length
!!    integer,intent(out),optional :: lines
!!##DESCRIPTION
!!    Read an entire file as a stream into memory as an array of single
!!    characters, retaining line end terminators.
!!
!!    NOTE:
!!
!!    Never casually read an entire file into memory if you can process it
!!    per line or in smaller units; as large files can consume unreasonable
!!    amounts of memory.
!!
!!##OPTIONS
!!       filename   filename to read into memory or LUN (Fortran Logical
!!                  Unit Number) If a LUN, file must be opened with
!!
!!                     form='unformatted',access='stream'
!!
!!                  as in
!!
!!                    open(unit=igetunit, file=filename,     &
!!                    & action="read", iomsg=message,        &
!!                    & form="unformatted", access="stream", &
!!                    & status='old',iostat=ios)
!!
!!       text       array of characters to hold file
!!       length     length of longest line read(Optional).
!!       lines      number of lines read(Optional).
!!
!!##EXAMPLES
!!
!!    Sample program, which  creates test input file "inputfile":
!!
!!     program demo_slurp
!!     use M_io, only      : slurp
!!     implicit none
!!     character(len=1),allocatable :: text(:) ! array to hold file in memory
!!     character(len=*),parameter :: FILENAME='inputfile' ! file to read
!!
!!     ! create test file
!!     open(file=FILENAME,unit=10)
!!     write(10,'(a)') new_line('A')//'esrever lliw'
!!     write(10,'(a)') 'margorp elpmas eht taht'
!!     write(10,'(a)') 'elif elpmas a si sihT'
!!     close(unit=10)
!!
!!     call slurp(FILENAME,text) ! allocate character array and copy file into it
!!
!!     if(.not.allocated(text))then
!!        write(*,*)'*rever* failed to load file '//FILENAME
!!     else
!!        ! write file reversed to stdout
!!        write(*,'(*(a:))',advance='no')text(size(text):1:-1)
!!        deallocate(text)  ! release memory
!!     endif
!!
!!     end program demo_slurp
!!
!!    Expected output:
!!
!!     >This is a sample file
!!     >that the sample program
!!     >will reverse
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine slurp(filename,text,length,lines)
!-----------------------------------------------------------------------------------------------------------------------------------
implicit none

!character(len=*),parameter::ident_6="@(#)M_io::slurp(3f): allocate text array and read file filename into it"

class(*),intent(in)                      :: filename    ! filename to shlep
character(len=1),allocatable,intent(out) :: text(:)     ! array to hold file
integer,intent(out),optional             :: length      ! length of longest line
integer,intent(out),optional             :: lines       ! number of lines
!-----------------------------------------------------------------------------------------------------------------------------------
integer :: nchars=0             ! holds size of file
integer :: igetunit             ! use newunit=igetunit in f08
integer :: ios=0                ! used for I/O error status
integer :: length_local
integer :: lines_local
integer :: i
integer :: icount
character(len=256)  :: message
character(len=4096) :: local_filename
!-----------------------------------------------------------------------------------------------------------------------------------
   length_local=0
   lines_local=0
   igetunit=notopen(10,99)         ! find unused file unit number (hopefully)
   if(igetunit.lt.0)then
      call stderr_local('*slurp* could not find unused file unit number')
      return
   endif
!-------------------------------------------
   message=''
      select type(FILENAME)
       type is (character(len=*))
          open(unit=igetunit, file=trim(filename), action="read", iomsg=message,&
           &form="unformatted", access="stream",status='old',iostat=ios)
          local_filename=filename
       type is (integer)
          rewind(unit=filename,iostat=ios,iomsg=message)
          write(local_filename,'("unit ",i0)')filename
          igetunit=filename
      end select
!-------------------------------------------
   if(ios.eq.0)then  ! if file was successfully opened
!-------------------------------------------
      inquire(unit=igetunit, size=nchars)
!-------------------------------------------
      if(nchars.le.0)then
         call stderr_local( '*slurp* empty file '//trim(local_filename) )
         return
      endif
      ! read file into text array
      !
      if(allocated(text))deallocate(text) ! make sure text array not allocated
      allocate ( text(nchars) )           ! make enough storage to hold file
      read(igetunit,iostat=ios,iomsg=message) text      ! load input file -> text array
      if(ios.ne.0)then
         call stderr_local( '*slurp* bad read of '//trim(local_filename)//':'//trim(message) )
      endif
   else
      call stderr_local('*slurp* '//message)
      allocate ( text(0) )           ! make enough storage to hold file
   endif

   close(iostat=ios,unit=igetunit)            ! close if opened successfully or not

   if(present(lines).or.present(length))then  ! get length of longest line and number of lines
      icount=0
      do i=1,nchars
         if(text(i).eq.NEW_LINE('A'))then
            lines_local=lines_local+1
            length_local=max(length_local,icount)
            icount=0
         endif
         icount=icount+1
      enddo
      if(nchars.ne.0)then
         if(text(nchars).ne.NEW_LINE('A'))then
            lines_local=lines_local+1
            length_local=max(length_local,icount)
         endif
      endif
      if(present(lines))lines=lines_local
      if(present(length))length=length_local
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine stderr_local(message)
use, intrinsic :: iso_fortran_env, only : error_unit
character(len=*) :: message
   write(error_unit,'(a)')trim(message)    ! write message to standard error
end subroutine stderr_local
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine slurp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    notopen(3f) - [M_io] Find a FUN/LUN (Fortran-unit-number) that is not in use
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    Usage
!!
!!       integer function notopen(start,end,err)
!!       integer,optional,intent(in)  :: start
!!       integer,optional,intent(in)  :: end
!!       integer,optional,intent(out) :: err
!!##DESCRIPTION
!!    A free FORTRAN unit number is needed to OPEN a file. NOTOPEN() returns
!!    a FORTRAN unit number from START to END not currently associated with
!!    an I/O unit. START and END are expected to be positive integers where
!!    END .ge. START.
!!
!!    If NOTOPEN() returns -1, then no free FORTRAN unit could be found in
!!    the specified range.
!!
!!    Otherwise, NOTOPEN() returns an integer representing a free FORTRAN
!!    logical unit number. Note that NOTOPEN() assumes the following unit
!!    numbers defined by the Fortran 2008 ISO_FORTRAN_ENV module
!!
!!       ERROR_UNIT,INPUT_UNIT,OUTPUT_UNIT
!!
!!    are special, and will never return those values.
!!
!!##OPTIONS
!!       start  optional logical unit number to start scan at, defaults to 10.
!!       end    optional logical unit number to stop scan at, defaults to 99.
!!       err    optional error flag returned. ERR will be non-zero if no errors.
!!              If not present and an error occurs the program will stop instead
!!              of returning.
!!
!!##NOTES
!!
!!    Why are the default START and END limits from 10 to 99? the Fortran 77
!!    standard did not specify a specific limit on the upper range limit, but
!!    the LUN range of 1 to 99 was almost always supported in conventional
!!    programming environments. Additionally, units in the range 0-10 have
!!    often been the units used for pre-assigned files. Occasionally 100,
!!    101 and 102 are reserved (for files such as standard input, standard
!!    output, standard error, ...). Therefore, the defaults for START and
!!    END were selected to be 10 and 99. And most programs do not need
!!    more than 90 files simultaneously open, so the defaults work well in
!!    practice with many versions/vintages of Fortran.
!!
!!    Note that an environment may impose a limit on the number of
!!    simultaneously open files (which some compilers work around).
!!
!!    Beginning with f2008, you can probably use OPEN(NEWUNIT=...) instead
!!    of an open unit locator.
!!
!!##EXAMPLE
!!
!!
!!    Sample program:
!!
!!     program demo_notopen ! test the NOTOPEN(3f) function
!!     use m_io, only: notopen
!!     implicit none
!!     integer :: ii, ierr, igot
!!
!!     write(*,*)'check for preassigned files from unit 0 to unit 1000'
!!     write(*,*)'(5 and 6 always return -1)'
!!
!!     do ii=0,1000
!!        if(notopen(ii,ii,ierr) .ne. ii)then
!!           write(*,*)'INUSE:',ii, notopen(ii,ii,ierr)
!!        endif
!!     enddo
!!
!!     ! open all files from UNIT=10 to UNIT=30 so have used units
!!     do ii=10,30,1
!!       open(unit=ii,status="scratch")
!!     enddo
!!     ! close UNIT=25
!!     close(25)
!!
!!     ! find open file in range 10 to 30
!!     write(*,*)'Should get 25 for this ..',notopen(10,30,ierr)
!!
!!     close(18)
!!     do ii=10,32
!!       igot=notopen(ii,ii,ierr)
!!       write(*,*)'For unit ',ii,' I got ',igot,' with ERR=',ierr
!!     enddo
!!
!!     end program demo_notopen
!!
!!    Expected output(can vary with each programming environment):
!!
!!       check for preassigned files from unit 0 to unit 1000
!!       (5 and 6 always return -1)
!!       INUSE:    0    -1
!!       INUSE:    5    -1
!!       INUSE:    6    -1
!!       Should get 25 for this .. 25
!!       For  unit  10  I  got  -1  with  ERR=  -1
!!       For  unit  11  I  got  -1  with  ERR=  -1
!!       For  unit  12  I  got  -1  with  ERR=  -1
!!       For  unit  13  I  got  -1  with  ERR=  -1
!!       For  unit  14  I  got  -1  with  ERR=  -1
!!       For  unit  15  I  got  -1  with  ERR=  -1
!!       For  unit  16  I  got  -1  with  ERR=  -1
!!       For  unit  17  I  got  -1  with  ERR=  -1
!!       For  unit  18  I  got  18  with  ERR=   0
!!       For  unit  19  I  got  -1  with  ERR=  -1
!!       For  unit  20  I  got  -1  with  ERR=  -1
!!       For  unit  21  I  got  -1  with  ERR=  -1
!!       For  unit  22  I  got  -1  with  ERR=  -1
!!       For  unit  23  I  got  -1  with  ERR=  -1
!!       For  unit  24  I  got  -1  with  ERR=  -1
!!       For  unit  25  I  got  25  with  ERR=   0
!!       For  unit  26  I  got  -1  with  ERR=  -1
!!       For  unit  27  I  got  -1  with  ERR=  -1
!!       For  unit  28  I  got  -1  with  ERR=  -1
!!       For  unit  29  I  got  -1  with  ERR=  -1
!!       For  unit  30  I  got  -1  with  ERR=  -1
!!       For  unit  31  I  got  31  with  ERR=   0
!!       For  unit  32  I  got  32  with  ERR=   0
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
integer function notopen(start,end,err)
use, intrinsic :: iso_fortran_env, only : error_unit,input_unit,output_unit     ! access computing environment
implicit none

!character(len=*),parameter::ident_7="@(#)M_io::notopen(3f): find free FORTRAN unit number to OPEN() a file"

integer,optional,intent(in)    :: start                           ! unit number to start looking at
integer,optional,intent(in)    :: end                             ! last unit number to look at
integer,optional,intent(out)   :: err                             ! error flag returned
integer                        :: istart
integer                        :: iend
integer                        :: ierr

integer         :: i10                                            ! counter from start to end
integer         :: ios                                            ! iostatus from INQUIRE
logical         :: lopen                                          ! returned from INQUIRE
logical         :: lexist                                         ! returned from INQUIRE
!-----------------------------------------------------------------------------------------------------------------------------------
   !! IEND=MERGE( END, 99, PRESENT(END)) do not use merge, as TSOURCE must be evaluated before the call
   if(present(start))then; istart=start; else; istart=10; endif
   if(present(end  ))then; iend  =end  ; else; iend  =99; endif
   ierr=0
   notopen=(-1)                                                   ! result if no units are available
!-----------------------------------------------------------------------------------------------------------------------------------
   do i10=istart,iend                                             ! check units over selected range
      select case (i10)                                           ! always skip these predefined units
      case(error_unit,input_unit,output_unit)
          cycle
      end select
      inquire( unit=i10, opened=lopen, exist=lexist, iostat=ios )
      if( ios == 0 )then                                          ! no error on inquire
         if(.not. lopen .and. lexist)then                         ! if unit number not in use, return it
            notopen = i10
            exit                                                  ! only need to find one, so return
         endif
      else
         write(error_unit,*)'*notopen*:error on unit ',i10,'=',ios
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   if (notopen .lt. 0 )then                                       ! no valid unit was found in given range
      ierr=-1
   else                                                           ! valid value being returned
      ierr=0
   endif
   if(present(err))then                                           ! if error flag is present set it
      err=ierr
   elseif(ierr.ne.0)then                                          ! if error occurred and error flag not present stop program
      stop 1
   endif
end function notopen
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dirname(3f) - [M_io] strip last component from filename
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function dirname(FILENAME) result (DIRECTORY)
!!
!!      character(len=*),intent(in)  :: FILENAME
!!      character(len=:),allocatable :: DIRECTORY
!!
!!##DESCRIPTION
!!    Output FILENAME with its last non-slash component and trailing
!!    slashes removed. If FILENAME contains no '/' character, output
!!    '.' (meaning the current directory).
!!
!!    Assumes leaf separator is a slash ('/') and that filename does not
!!    contain trailing spaces.
!!
!!##OPTIONS
!!      FILENAME   pathname to remove the last leaf from
!!
!!##RETURNS
!!      DIRECTORY  directory name for pathname
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_dirname
!!    use M_io, only : dirname
!!    implicit none
!!    character(len=:),allocatable :: filename
!!    integer                      :: filename_length
!!    integer                      :: i
!!    ! get pathname from command line arguments
!!    do i = 1 , command_argument_count()
!!       call get_command_argument (i , length=filename_length)
!!       allocate(character(len=filename_length) :: filename)
!!       call get_command_argument (i , value=filename)
!!       write(*,'(a)')dirname(filename)
!!       deallocate(filename)
!!    enddo
!!    end program demo_dirname
!!
!!   Sample program executions:
!!
!!      demo_dirname /usr/bin/          -> "/usr"
!!      demo_dirname dir1/str dir2/str  -> "dir1" followed by "dir2"
!!      demo_dirname stdio.h            -> "."
!!
!!##SEE ALSO
!!    dirname(3c), basename(3c), readlink(3c), realpath(3c)
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        dirname(3f)
!! DESCRIPTION:    strip last component from filename
!!##VERSION:        1.0.0
!!##DATE:           2015-06-26
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
function dirname(filename) result (directory)
implicit none

!character(len=*),parameter::ident_8="@(#)M_io::dirname(3f): strip last component from filename"

character(len=*),intent(in)      :: filename
character(len=:),allocatable     :: directory
integer                          :: iend
!-----------------------------------------------------------------------------------------------------------------------------------
   directory=trim(filename)
   call removetail()                         ! trim trailing slashes even if duplicates
   iend=index(directory,'/',back=.true.)     ! find last slash if any
   if(iend.eq.0)then                         ! filename is a leaf
      directory='.'                          ! special case
   else
      directory=directory(:iend-1)           ! remove leaf
      call removetail()                      ! trim off trailing slashes in case duplicates
   endif
   directory=trim(directory)                 ! clean up return value
contains
   subroutine removetail()              ! replace trailing slashes with spaces even if duplicates
   integer :: right
   do right=len(directory),1,-1
      if(directory(right:right).eq.'/'.or.directory(right:right).eq.' ')then
         directory(right:right)=' '
      else
         exit
      endif
   enddo
   end subroutine removetail

end function dirname
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fileopen(3f) - [M_io] A simple open of a sequential file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function fileopen(filename,mode,ios) result(lun)
!!
!!    character(len=*),intent(in)           :: filename
!!    character(len=*),intent(in),optional  :: mode
!!    integer,intent(out),optional          :: ios
!!    integer                               :: lun
!!
!!##DESCRIPTION
!!    fileopen(3f) is a convenience routine that allows you to open a file
!!    for sequential reading and writing as a text file in a form commonly
!!    found in C and interpreted languages such as shells. See the OPEN(3f)
!!    statement for more demanding I/O specifications (asynchronous, direct,
!!    unformatted, ... ). The documentation for the flexible and powerful
!!    OPEN(3f) statement can be a bit overwhelming; this routine cuts it
!!    down to the just the simple basic functions typically available in
!!    a scripting language such as bash, tcsh, sh, ...
!!
!!    Specify the file's name as the string FILENAME with a shell-like prefix
!!    specifying the access mode, or alternatively specify a plain FILENAME
!!    and the kind of access you need to the file with the string MODE.
!!
!!    Three fundamental kinds of access are available: read, write,
!!    and append.
!!
!!##OPTION
!!   FILENAME  The filename to open. If the beginning of the filename is
!!
!!             <   open for read. File must exist
!!             >   open for write. Will overwrite current file
!!             >>  open for append. Will append to current file
!!
!!             If no prefix exists to specify a file access mode, it
!!             will depend on the values of the MODE argument (meaning
!!             the default will be "readwrite").
!!
!!             A blank filename causes a unit number for a scratch file
!!             to be returned.
!!
!!   MODE     [rwa][tb][+]
!!            An alternate way to specify the file access mode is to specify
!!            a MODE value. It should begin with one of the three characters
!!            "r", "w", or "a". It defaults to 'rw'. It is case-insensitive.
!!
!!
!!        READING PREFIX
!!        r,<   Open the file for reading; the operation will fail if the
!!              file does not exist, or if the host system does not permit
!!              you to read it.
!!
!!        WRITING PREFIXES
!!        w,>   Open a file for writing from the beginning of the file.
!!              If the file whose name you specified already existed,
!!              the call fails.
!!
!!        o     Open the file for writing from the beginning of the file:
!!              effectively, this always creates a new file. If the file
!!              whose name you specified already existed, its old contents
!!              are discarded.
!!
!!        a,<<  Initially open the file for appending data (ie. writing
!!              at the end of file).
!!
!!        SUFFIX
!!
!!        b   Append a "b" to any of the three modes above to specify that
!!            you are opening the file as a "binary file" (the default is
!!            to open the file as a sequential formatted text file. This
!!            switch changes to to an unformatted stream).
!!
!!                   open( ... access='stream';form='unformatted')
!!
!!        t   Append a "t" to any of the three modes (rwa) to specify a
!!            formatted stream
!!
!!                   open( ... access='stream';form='formatted')
!!
!!        +   Finally, you might need to both read and write from the same
!!            file. You can specify "rw" or you can append a `+' to any of
!!            the three primary modes ("rwa") to permit "readwrite" access
!!
!!        v   Additionally, "v" selects verbose mode, which prints the
!!            OPEN(3f) options explicitly selected
!!
!!        NOTES
!!
!!            If you want to append both `b' and `+', you can do it in
!!            either order: for example, "rb+" means the same thing as
!!            "r+b" when used as a mode string.)
!!
!!    IOS    The error code returned by the OPEN(3f) statement ultimately
!!           executed by this function. If not present the program stops on
!!           an error.
!!##RETURNS
!!        FILEOPEN(3f) returns a Fortran unit number which you can use for other file
!!        operations, unless the file you requested could not be opened;
!!        in that situation, the result is -1 (a reserved value that cannot be returned
!!        as a NEWUNIT value on an OPEN(3f)) and IOS will be non-zero.
!!
!!##EXAMPLE
!!
!!  Common usage
!!
!!   READ
!!     R=fileopen('<in.txt')
!!     or
!!     R=fileopen('in.txt','r')
!!
!!   WRITE
!!     W=fileopen('>out.txt')
!!     or
!!     W=fileopen('out.txt','W')
!!
!!   READWRITE
!!     RW=fileopen('inout.txt')
!!
!!   APPEND
!!     A=fileopen('>>inout.txt')
!!     or
!!     A=fileopen('inout.txt','a')
!!
!!   Sample program
!!
!!       program demo_fileopen
!!       use M_io, only : fileopen, fileclose, print_inquire
!!       integer :: lun
!!       lun=fileopen('fred.txt')
!!       call print_inquire(lun)
!!       end program demo_fileopen
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function fileopen(filename,mode,ios) result(lun)
character(len=*),intent(in)           :: filename
character(len=*),intent(in),optional  :: mode
integer,intent(out),optional          :: ios
integer                               :: lun, i, ios_local,ifound,gts
character(len=:),allocatable          :: local_mode
character(len=256)                    :: message
character(len=:),allocatable          :: action, position, access, form, status, file
logical                               :: verbose
   local_mode=lower(merge_str(mode,'',present(mode)))
   file=trim(adjustl(filename))//'   '
   ifound=index(file,'>>')
   if(ifound.ne.0)then
      file(ifound:ifound+1)='  '
      local_mode=local_mode//'a'
   endif
   ifound=index(file,'>')
   if(ifound.ne.0)then
      file(ifound:ifound)=' '
      local_mode=local_mode//'w'
   endif
   ifound=index(file,'<')
   if(ifound.ne.0)then
      file(ifound:ifound)=' '
      local_mode=local_mode//'r'
   endif
   file=adjustl(file)
   local_mode=merge_str('rw',local_mode,local_mode.eq.'')
   file=trim(file)

   gts=0
   action=''
   position='asis'
   form='formatted'
   access='sequential'
   status='unknown'
   verbose=.false.
   do i=1,len(local_mode) ! create order independence
      select case(local_mode(i:i))
       case('r','<'); if(action.ne.'readwrite'.and.action.ne.'read')action='read'//action
                      if(status.eq.'unknown')status='old'
       case('w','>'); if(action.ne.'readwrite'.and.action.ne.'write')action=action//'write'
                      if(status=='unknown')status='new'
                      if(gts.gt.0)then
                         position='append'
                      endif
                      gts=gts+1
       case('o');     if(action.ne.'readwrite'.and.action.ne.'write')action=action//'write'
                      if(status=='unknown')then
                         status='replace'
                      endif
       case('a');     position='append'
                      if(action.ne.'readwrite'.and.action.ne.'write')action=action//'write'
                      if(status.eq.'old')status='unknown'
       case('b');     access='stream';form='unformatted'
       case('t');     access='stream';form='formatted'
       case('+');     action='readwrite'
                      status='unknown'
       case('v');     verbose=.true.
       case default
         write(*,'(*(g0))',advance='no')'*fileopen* unknown mode key ',local_mode(i:i)
         write(*,'(*(:,"[",g0,"=",g0,"]"))',advance='no') &
         & ' INPUTNAME=',trim(file), &
         & ' MODE=',trim(local_mode)
      end select
   enddo
   if(action.eq.'')action='readwrite'

   if(verbose)then
      write(*,'(*(:,"[",g0,"=",g0,"]"))',advance='no') &
         & 'INPUTNAME=',trim(file), &
         & 'MODE=',trim(local_mode)
      write(*,'(a)',advance='no')'==>'
      write(*,'(*(:,"[",g0,"=",g0,"]"))') &
         & 'FILE=',trim(file), &
         & 'FORM=',trim(form), &
         & 'ACCESS=',trim(access), &
         & 'ACTION=',trim(action), &
         & 'POSITION=',trim(position), &
         & 'STATUS=',trim(status)
   endif
   if(file.ne.' ')then
    open(file=file,newunit=lun,form=form,access=access,action=action,position=position,status=status,iostat=ios_local,iomsg=message)
   else
    open(newunit=lun,form=form,access=access,action=action,status='scratch',iostat=ios_local,iomsg=message)
   endif
   !  ACCESS    =  SEQUENTIAL  |  DIRECT       |  STREAM
   !  ACTION    =  READ|WRITE  |  READWRITE
   !  FORM      =  FORMATTED   |  UNFORMATTED
   !  POSITION  =  ASIS        |  REWIND       |  APPEND
   !  STATUS    =  NEW         |  REPLACE      |  OLD     |  SCRATCH   | UNKNOWN
   if(ios_local.ne.0)then
      write(*,*)'*fileopen* ',message
      lun=-1
   endif
   if(present(ios))then        ! caller has asked for status so let caller process any error
      ios=ios_local
   elseif(ios_local.ne.0)then  ! caller did not ask for status so stop program on error
      stop 1
   endif
end function fileopen
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fileclose(3f) - [M_io] A simple close of a sequential file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     function fileclose(lun) result(ios)
!!
!!      integer,intent(in)       :: lun
!!      integer                  :: ios
!!##DESCRIPTION
!!   A convenience command for closing a file that leaves an
!!   error message in the current journal file if active.
!!##OPTION
!!   LUN unit number to close
!!##RETURNS
!!   IOS status value from CLOSE
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_fileclose
!!     use M_io, only : fileclose, fileopen
!!     implicit none
!!     integer :: lun
!!     integer :: ios
!!        lun=fileopen('<input.txt')
!!        ios=fileclose(lun)
!!     end program demo_fileclose
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function fileclose(lun) result(ios)
integer,intent(in)       :: lun
integer                  :: ios
character(len=256)       :: message
   close(unit=lun,iostat=ios,iomsg=message)
   if(ios.ne.0)then
      write(*,*)'*fileclose* ',trim(message)
      stop
   endif
end function fileclose
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    filedelete(3f) - [M_io] A simple close of an open file with STATUS='DELETE'
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function filedelete(lun) result(ios)
!!
!!     integer,intent(in)    :: lun
!!     integer               :: ios
!!
!!##DESCRIPTION
!!   A convenience command for deleting an OPEN(3f) file that leaves an
!!   error message in the current journal file if active.
!!##OPTION
!!   LUN  unit number of open file to delete
!!##RETURNS
!!   IOS  status returned by CLOSE().
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_filedelete
!!     use M_io, only : filedelete, fileopen
!!     implicit none
!!     integer :: lun
!!     integer :: ios
!!        lun=fileopen('<input.txt')
!!        ios=filedelete(lun)
!!     end program demo_filedelete
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function filedelete(lun) result(ios)
integer,intent(in)    :: lun
integer               :: ios
character(len=256)    :: message
   close(unit=lun,iostat=ios,status='delete',iomsg=message)
   if(ios.ne.0)then
      write(*,*)'*filedelete* ',trim(message)
   endif
end function filedelete
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     splitpath(3f) - [M_io] split a Unix pathname into components
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   splitpath(path,dir,name,basename,ext)
!!
!!    integer,parameter :: maxlen=4096
!!    character(len=maxlen),intent(in)  :: path
!!    character(len=maxlen),intent(out),optional :: dir
!!    character(len=maxlen),intent(out),optional :: name
!!    character(len=maxlen),intent(out),optional :: basename
!!    character(len=maxlen),intent(out),optional :: ext
!!
!!##DESCRIPTION
!!    splitpath(3f) splits given pathname assuming a forward slash separates
!!    filename components and that the right-most period in the last leaf
!!    of the pathname is considered the beginning of an extension. If
!!    an extension is found it is left present in NAME but removed from
!!    BASENAME.
!!
!!    This routine does not check the system for the existence or type of
!!    the filename components; it merely parses a string.
!!
!!    Assumes leaf separator is a slash ('/') and that filename does not
!!    contain trailing spaces.
!!
!!##OPTIONS
!!    path      Path to be broken into components. It is assumed
!!
!!              o Forward slashes (/) separate pathname components.
!!              o the name '.' means "current directory"
!!              o the name '..' means "up one directory"
!!              o a pathname ending in a slash is a directory name
!!              o a slash starting the pathname represents the root
!!                directory.
!!              o trailing spaces are insignificant.
!!
!!    Using these rules helps to reduce incorrect parsing, but the
!!    routine is only intended for simple parsing of names of the form
!!    "[dir/]name[.extension].
!!
!!##RESULTS
!!    dir       Path of directories, including the trailing slash.
!!    name      Name of file leaf or, if no file is specified in path,
!!              name of the lowest directory.
!!    basename  NAME with any extension removed
!!    ext       File name extension, if any, including the leading period (.).
!!
!!    The path parameter can be a complete or partial file specification. The
!!    special name "." is assumed to mean the current directory, and the
!!    special name ".." is assumed to mean one directory above the current
!!    directory.
!!
!!##EXAMPLE
!!
!!   program demo_splitpath
!!
!!    use m_io, only : splitpath
!!    implicit none
!!    integer,parameter :: maxlen=4096
!!    character(len=maxlen),parameter   :: file(*)=[&
!!       & 'dirs/name.ext  ', &
!!       & 'xx/IO/zz/NN.FF ', &
!!       & 'xx/IO/zz/NN    ', &
!!       & '/xx/IO/zz/NN   ', &
!!       & '/xx/IO/zz/     ', &
!!       & '/xx/IO/zz.A/   ', &
!!       & '/xx/IO/zz/.    ', &
!!       & '               ', &
!!       & './             ', &
!!       & '/              ', &
!!       & '/..            ', &
!!       & './..           ', &
!!       & 'name.          ', &
!!       & '.name          ', &
!!       & '.name.         ', &
!!       & '.              ', &
!!       & '..             ', &
!!       & '...            ']
!!
!!    character(len=maxlen)  :: dir
!!    character(len=maxlen)  :: name
!!    character(len=maxlen)  :: basename
!!    character(len=maxlen)  :: ext
!!    integer                :: i
!!    integer                :: longest
!!    longest=maxval(len_trim(file)) ! find longest filename
!!
!!    do i=1,size(file)
!!       call splitpath(file(i), dir, name, basename, ext)
!!       write(*,'(*("| ",a:))')  &
!!       & file(i)(:longest),     &
!!       & dir(:longest),         &
!!       & name(:longest),        &
!!       & basename(:longest),    &
!!       & ext(:longest)
!!    enddo
!!   end program demo_splitpath
!!
!!   Output
!!
!!    | dirs/name.ext | dirs          | name.ext      | name          | .ext
!!    | xx/IO/zz/NN.FF| xx/IO/zz      | NN.FF         | NN            | .FF
!!    | xx/IO/zz/NN   | xx/IO/zz      | NN            | NN            |
!!    | /xx/IO/zz/NN  | /xx/IO/zz     | NN            | NN            |
!!    | /xx/IO/zz/    | /xx/IO/zz     |               |               |
!!    | /xx/IO/zz.A/  | /xx/IO/zz.A   |               |               |
!!    | /xx/IO/zz/.   | /xx/IO/zz/.   |               |               |
!!    |               | .             |               |               |
!!    | ./            | .             |               |               |
!!    | /             | /             |               |               |
!!    | /..           | /             |               |               |
!!    | ./..          | ./..          |               |               |
!!    | name.         |               | name.         | name          | .
!!    | .name         |               | .name         | .name         |
!!    | .name.        |               | .name.        | .name         | .
!!    | .             | .             |               |               |
!!    | ..            |               |               |               |
!!    | ...           |               | ...           | ..            | .
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine splitpath(path,dir,name,basename,ext)
implicit none

!character(len=*),parameter::ident_9="@(#)M_io::splitpath(3f): split Unix pathname into components (dir,name,basename,extension)"

!===================================================================================================================================
character(len=*),intent(in)           :: path
character(len=*),intent(out),optional :: dir
character(len=*),intent(out),optional :: name
character(len=*),intent(out),optional :: basename
character(len=*),intent(out),optional :: ext
integer,parameter                     :: maxlen=4096
character(len=maxlen)                 :: dir_local
character(len=maxlen)                 :: name_local
character(len=maxlen)                 :: basename_local
character(len=maxlen)                 :: ext_local
character(len=len(path)+1)            :: path_local
integer                               :: where
integer                               :: i
integer                               :: iend
!===================================================================================================================================
   path_local=path                           ! initialize variables
   dir_local=''
   name_local=''
   basename_local=''
   ext_local=''
   iend=len_trim(path_local)
   LOCAL : block
!===================================================================================================================================
   if(iend.eq.0)then                         ! blank input path
      dir_local='.'
      exit LOCAL
   endif
!===================================================================================================================================
   if(path_local(iend:iend).eq.'/')then      ! assume entire name is a directory if it ends in a slash
      if(iend.gt.1)then
         dir_local=path_local(:iend-1)
      else                                   ! if just a slash it means root directory so leave it as slash
         dir_local=path_local
      endif
      exit LOCAL
   endif
!===================================================================================================================================
   TRIMSLASHES: do i=iend,1,-1               ! trim off trailing slashes even if duplicates
      if(path_local(i:i).eq.'/')then
         path_local(i:i)=' '
         iend=i-1
      else
         iend=i
         exit TRIMSLASHES
      endif
   enddo TRIMSLASHES

   if(iend.eq.0)then                         ! path composed entirely of slashes.
      dir_local='/'
      exit LOCAL
   endif
!===================================================================================================================================
   where=INDEX(path_local,'/',BACK=.true.)   ! find any right-most slash in remaining non-null name_local after trimming trailing slashes
   if(where.le.0)then                        ! no slash in path so everything left is name_local
      name_local=path_local(:iend)                 ! this is name_local unless '.' or '..'
   else                                      ! last slash found
      dir_local=path_local(:where-1)               ! split into directory
      name_local=path_local(where+1:iend)          ! this is name_local unless '.' or '..'
   endif
!===================================================================================================================================
   select case (name_local(1:3))                   ! special cases where name_local is a relative directory name_local '.' or '..'
   case('.  ')
      dir_local=path_local
      name_local=''
   case('.. ')
      if(dir_local.eq.'')then
         if(path_local(1:1).eq.'/')then
            dir_local='/'
         endif
      else
         dir_local=path_local
      endif
      name_local=''
   case default
   end select
!===================================================================================================================================
   if(name_local.eq.'.')then
      name_local=''
   endif
!===================================================================================================================================
   iend=len_trim(name_local)
   where=INDEX(name_local,'.',BACK=.true.)         ! find any extension
   if(where.gt.0.and.where.ne.1)then         ! only consider a non-blank extension name_local
      ext_local=name_local(where:)
      basename_local=name_local(:where-1)
   else
      basename_local=name_local
   endif
!===================================================================================================================================
   endblock LOCAL
   if(present(dir))dir=dir_local
   if(present(name))name=name_local
   if(present(basename))basename=basename_local
   if(present(ext))ext=ext_local
end subroutine splitpath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     getline(3f) - [M_io] read a line from specified LUN into allocatable string up to line length limit
!!     (LICENSE:PD)
!!
!!##SYNTAX
!!   function getline(line,lun) result(ier)
!!
!!    character(len=:),allocatable,intent(out) :: line
!!    integer,intent(in),optional              :: lun
!!    integer,intent(out)                      :: ier
!!
!!##DESCRIPTION
!!
!!    Read a line of any length up to programming environment's maximum
!!    line length. Requires Fortran 2003+.
!!
!!    It is primarily expected to be used when reading input which will
!!    then be parsed.
!!
!!    The simple use of a loop that repeatedly re-allocates a character
!!    variable in addition to reading the input file one buffer at a
!!    time could (depending on the programming environment used) be
!!    inefficient, as it could reallocate and allocate memory used for
!!    the output string with each buffer read.
!!
!!##OPTIONS
!!
!!    LINE   line read
!!    LUN    optional LUN (Fortran logical I/O unit) number. Defaults
!!           to stdin.
!!##RETURNS
!!
!!    IER    zero unless an error occurred. If not zero, LINE returns the
!!           I/O error message.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_getline
!!    use M_io, only : getline
!!    implicit none
!!    character(len=:),allocatable :: line
!!       INFINITE: do while (getline(line)==0)
!!          write(*,'(a)')'['//line//']'
!!       enddo INFINITE
!!    end program demo_getline
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function getline(line,lun) result(ier)
use, intrinsic :: iso_fortran_env, only : INPUT_UNIT
implicit none

!character(len=*),parameter::ident_10="&
!&@(#)M_io::getline(3f): read a line from specified LUN into allocatable string up to line length limit"

character(len=:),allocatable,intent(out) :: line
integer,intent(in),optional              :: lun
integer                                  :: ier
character(len=4096)                      :: message

integer,parameter                        :: buflen=1024
character(len=:),allocatable             :: line_local
character(len=buflen)                    :: buffer
integer                                  :: isize
integer                                  :: lun_local

   line_local=''
   ier=0
   if(present(lun))then
      lun_local=lun
   else
      lun_local=INPUT_UNIT
   endif

   INFINITE: do                                                      ! read characters from line and append to result
      read(lun_local,iostat=ier,fmt='(a)',advance='no',size=isize,iomsg=message) buffer ! read next buffer (might use stream I/O for files
                                                                     ! other than stdin so system line limit is not limiting
      if(isize.gt.0)line_local=line_local//buffer(:isize)            ! append what was read to result
      if(is_iostat_eor(ier))then                                     ! if hit EOR reading is complete unless backslash ends the line
         ier=0                                                       ! hitting end of record is not an error for this routine
         exit INFINITE                                               ! end of reading line
     elseif(ier.ne.0)then                                            ! end of file or error
        line=trim(message)
        exit INFINITE
     endif
   enddo INFINITE
   line=line_local                                                   ! trim line
end function getline
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     read_line(3f) - [M_io] read a line from specified LUN into allocatable string up to line length limit cleaning up input line
!!     (LICENSE:PD)
!!
!!##SYNTAX
!!   function read_line(line,lun) result(ier)
!!
!!    character(len=:),allocatable,intent(out) :: line
!!    integer,intent(in),optional              :: lun
!!    integer                                  :: ier
!!
!!##DESCRIPTION
!!
!!    Read a line of any length up to programming environment's maximum
!!    line length. Requires Fortran 2003+.
!!
!!    It is primarily expected to be used when reading input which will
!!    then be parsed.
!!
!!    o Append lines that end in a backslash with next line
!!    o Expand tabs
!!    o Replace unprintable characters with spaces
!!    o Remove trailing carriage return characters and white space
!!
!!    The simple use of a loop that repeatedly re-allocates a character
!!    variable in addition to reading the input file one buffer at a
!!    time could (depending on the programming environment used) be
!!    inefficient, as it could reallocate and allocate memory used for
!!    the output string with each buffer read.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program simple_read_line
!!    use M_io, only : read_line
!!    implicit none
!!    character(len=:),allocatable :: line
!!       INFINITE: do while (read_line(line)==0)
!!          write(*,'(a)')'['//line//']'
!!       enddo INFINITE
!!    end program simple_read_line
!!
!!   Checking the error message and counting lines:
!!
!!     program demo_read_line
!!     use,intrinsic :: iso_fortran_env, only : stdin  => input_unit
!!     use,intrinsic :: iso_fortran_env, only : stderr => error_unit
!!     use,intrinsic :: iso_fortran_env, only : iostat_end, iostat_eor
!!     use M_io, only : read_line
!!     implicit none
!!     character (len =: ), allocatable :: line
!!     integer  ::  ios, icount=0
!!        INFINITE: do while (isave (read_line (line), ios) == 0)
!!           write (*, '(*(g0))') icount,' [',line,']'
!!        enddo INFINITE
!!        if ( .not.is_iostat_end(ios) ) then
!!           write (stderr, '(*(g0))') 'error: line ',icount,'==>',trim (line)
!!        endif
!!     contains
!!        integer function isave (iin, iout)
!!        integer, intent (in) :: iin
!!        integer, intent (out) :: iout
!!           iout = iin
!!           isave = iin
!!           icount=icount+1
!!        end function isave
!!     end program demo_read_line
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function read_line(line,lun) result(ier)
use, intrinsic :: iso_fortran_env, only : INPUT_UNIT
implicit none

!character(len=*),parameter::ident_11="&
!&@(#)M_io::read_line(3f): read a line from specified LUN into allocatable string up to line length limit"

character(len=:),allocatable,intent(out) :: line
integer,intent(in),optional              :: lun
integer                                  :: ier

integer,parameter                        :: buflen=1024
character(len=:),allocatable             :: line_local
character(len=256)                       :: message
integer                                  :: biggest
character(len=buflen)                    :: buffer
integer                                  :: last
integer                                  :: isize
integer                                  :: lun_local

   line_local=''
   ier=0
   lun_local=merge(lun,INPUT_UNIT,present(lun))

   INFINITE: do                                                           ! read characters from line and append to result
      read(lun_local,iostat=ier,fmt='(a)',advance='no',size=isize,iomsg=message) buffer ! read next buffer (might use stream I/O for
                                                                          ! files other than stdin so system line limit
                                                                          ! is not limiting
      if(isize.gt.0)line_local=line_local//buffer(:isize)   ! append what was read to result
      if(is_iostat_eor(ier))then                            ! if hit EOR reading is complete unless backslash ends the line
         last=len(line_local)
         if(last.ne.0)then
            if(line_local(last:last).eq.'\')then            ! if line ends in backslash it is assumed a continued line
               line_local=line_local(:last-1)               ! remove backslash
               cycle INFINITE                               ! continue on and read next line and append to result
            endif
         endif
         ier=0                                              ! hitting end of record is not an error for this routine
         exit INFINITE                                      ! end of reading line
     elseif(ier.ne.0)then                                   ! end of file or error
        line_local=trim(message)
        exit INFINITE
     endif
   enddo INFINITE
   biggest=8*len(line_local)                                ! worst case is raw line is all tab characters
   allocate(character(len=biggest) :: line)
   call notabs(line_local,line,last)                        ! expand tabs, trim carriage returns, remove unprintable characters
   line=trim(line(:last))                                   ! trim line
end function read_line
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      get_tmp(3f) - [M_io] Return the name of the scratch directory
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!     function get_tmp() result(tname)
!!
!!      character(len=:),allocatable :: tname
!!##DESCRIPTION
!!
!!    Return the name of the scratch directory set by the most common
!!    environment variables used to designate a scratch directory.
!!    $TMPDIR is the canonical environment variable in Unix and POSIX[1]
!!    to use to specify a temporary directory for scratch space. If $TMPDIR
!!    is not set, $TEMP, $TEMPDIR, and $TMP are examined in that order. If
!!    nothing is set "/tmp/" is returned. The returned value always ends in
!!    "/". No test is made that the directory exists or is writable.
!!
!!##EXAMPLE
!!
!!
!!   Sample:
!!
!!     program demo_get_tmp
!!     use M_io, only : get_tmp, uniq
!!     implicit none
!!     character(len=:),allocatable :: answer
!!        answer=get_tmp()
!!        write(*,*)'result is ',answer
!!        answer=get_tmp()//uniq('_scratch',create=.false.)
!!        write(*,*)'the file ',answer,' was a good scratch file name, at least a moment ago'
!!     end program demo_get_tmp
!!
!!   Sample Results:
!!
!!     result is /cygdrive/c/Users/JSU/AppData/Local/Temp/
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function get_tmp() result(tname)

!character(len=*),parameter::ident_12="@(#)M_io::get_tmp(3f): Return the name of the scratch directory"

character(len=:),allocatable :: tname
integer                      :: lngth
character(len=10),parameter  :: names(4)=["TMPDIR    ","TEMP      ","TEMPDIR   ","TMP       "]
integer                      :: i
   tname=''
   do i=1,size(names)
      call get_environment_variable(name=names(i), length=lngth)
      if(lngth.ne.0)then
         deallocate(tname)
         allocate(character(len=lngth) :: tname)
         call get_environment_variable(name=names(i), value=tname)
         exit
      endif
   enddo
   if(lngth.eq.0)then
      tname='/tmp'
      lngth=len_trim(tname)
   endif
   if(scan(tname(lngth:lngth),'/\').eq.0)then
      tname=tname//'/'
   endif
end function get_tmp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! COPIES FROM OTHER MODULES
!===================================================================================================================================
function merge_str(str1,str2,expr) result(strout)
! for some reason the MERGE(3f) intrinsic requires the strings it compares to be of equal length
! make an alias for MERGE(3f) that makes the lengths the same before doing the comparison by padding the shorter one with spaces

!character(len=*),parameter::ident_37="@(#)M_strings::merge_str(3f): pads first and second arguments to MERGE(3f) to same length"

character(len=*),intent(in)     :: str1
character(len=*),intent(in)     :: str2
logical,intent(in)              :: expr
character(len=:),allocatable    :: strout
integer                         :: big
   big=max(len(str1),len(str2))
   strout=trim(merge(lenset(str1,big),lenset(str2,big),expr))
end function merge_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine split(input_line,array,delimiters,order,nulls)
!-----------------------------------------------------------------------------------------------------------------------------------

!character(len=*),parameter::ident_7="&
!&@(#)M_strings::split(3f): parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
character(len=:),allocatable  :: ordr                   ! string containing order keyword
character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
integer                       :: ii,iiii                ! loop parameters used to control print order
integer                       :: icount                 ! number of tokens found
integer                       :: ilen                   ! length of input string with trailing spaces trimmed
integer                       :: i10,i20,i30            ! loop counters
integer                       :: icol                   ! pointer into input string as it is being parsed
integer                       :: idlim                  ! number of delimiter characters
integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
integer                       :: inotnull               ! count strings not composed of delimiters
integer                       :: ireturn                ! number of tokens returned
integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters.ne.'')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
   if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len(input_line)                                           ! ILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ilen)
!-----------------------------------------------------------------------------------------------------------------------------------
   case (:0)                                                      ! command was totally blank
!-----------------------------------------------------------------------------------------------------------------------------------
   case default                                                   ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,ilen,1                                   ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)).eq.0)then  ! if current character is not a delimiter
            iterm(i30)=ilen                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):ilen),dlim(i10:i10))
               IF(ifound.gt.0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol.gt.ilen)then                                     ! no text left
            exit INFINITE
         endif
      enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(nlls)))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
   !allocate(array(ireturn))                                       ! allocate the array to turn
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
   case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
   case default             ; ii=1       ; iiii=1                 ! first to last
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20).lt.ibegin(i20))then
         select case (trim(adjustl(nlls)))
         case ('ignore','','ignoreend')
         case default
            array(ii)=' '
            ii=ii+iiii
         end select
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+iiii
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   end subroutine split
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental pure function upper(str,begin,end) result (string)

!character(len=*),parameter::ident_21="@(#)M_strings::upper(3f): Changes a string to uppercase"

character(*), intent(In)      :: str                 ! inpout string to convert to all uppercase
integer, intent(in), optional :: begin,end
character(len(str))           :: string              ! output string that contains no miniscule letters
integer                       :: i                   ! loop counter
integer                       :: ibegin,iend
   string = str                                      ! initialize output string to input string

   ibegin = 1
   if (present(begin))then
      ibegin = max(ibegin,begin)
   endif

   iend = len_trim(str)
   if (present(end))then
      iend= min(iend,end)
   endif

   do i = ibegin, iend                               ! step thru each letter in the string in specified range
       select case (str(i:i))
       case ('a':'z')                                ! located miniscule letter
          string(i:i) = char(iachar(str(i:i))-32)    ! change miniscule letter to uppercase
       end select
   end do

end function upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental pure function lower(str,begin,end) result (string)

!character(len=*),parameter::ident_22="@(#)M_strings::lower(3f): Changes a string to lowercase over specified range"

character(*), intent(In)     :: str
character(len(str))          :: string
integer,intent(in),optional  :: begin, end
integer                      :: i
integer                      :: ibegin, iend
   string = str

   ibegin = 1
   if (present(begin))then
      ibegin = max(ibegin,begin)
   endif

   iend = len_trim(str)
   if (present(end))then
      iend= min(iend,end)
   endif

   do i = ibegin, iend                               ! step thru each letter in the string in specified range
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32)     ! change letter to miniscule
      case default
      end select
   end do

end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function s2a(string)  RESULT (array)

!character(len=*),parameter::ident_24="@(#)M_strings::s2a(3fp): function to copy string(1:Clen(string)) to char array"

character(len=*),intent(in) :: string
character(len=1)            :: array(len(string))
integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall(i=1:len(string)) array(i) = string(i:i)
! ----------------------------------------------------------------------------------------------------------------------------------
end function s2a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine notabs(INSTR,OUTSTR,ILEN)

!character(len=*),parameter::ident_31="&
!&@(#)M_strings::notabs(3f): convert tabs to spaces while maintaining columns, remove CRLF chars"

CHARACTER(LEN=*),INTENT(IN)   :: instr        ! input line to scan for tab characters
CHARACTER(LEN=*),INTENT(OUT)  :: outstr       ! tab-expanded version of INSTR produced
INTEGER,INTENT(OUT)           :: ilen         ! column position of last character put into output string
                                              ! that is, ILEN holds the position of the last non-blank character in OUTSTR
!===================================================================================================================================
INTEGER,PARAMETER             :: tabsize=8    ! assume a tab stop is set every 8th column
INTEGER                       :: ipos         ! position in OUTSTR to put next character of INSTR
INTEGER                       :: lenin        ! length of input string trimmed of trailing spaces
INTEGER                       :: lenout       ! number of characters output string can hold
INTEGER                       :: istep        ! counter that advances thru input string INSTR one character at a time
CHARACTER(LEN=1)              :: c            ! character in input line being processed
INTEGER                       :: iade         ! ADE (ASCII Decimal Equivalent) of character being tested
!===================================================================================================================================
   IPOS=1                                     ! where to put next character in output string OUTSTR
   lenin=LEN(instr)                           ! length of character variable INSTR
   lenin=LEN_TRIM(instr(1:lenin))             ! length of INSTR trimmed of trailing spaces
   lenout=LEN(outstr)                         ! number of characters output string OUTSTR can hold
   OUTSTR=" "                                 ! this SHOULD blank-fill string, a buggy machine required a loop to set all characters
!===================================================================================================================================
      SCAN_LINE: DO istep=1,lenin             ! look through input string one character at a time
         c=instr(istep:istep)                 ! get next character
         iade=ICHAR(c)                        ! get ADE of the character
         expand_tabs : SELECT CASE (iade)     ! take different actions depending on which character was found
         CASE(9)                              ! test if character is a tab and move pointer out to appropriate column
            ipos = ipos + (tabsize - (MOD(ipos-1,tabsize)))
         CASE(10,13)                          ! convert carriage-return and new-line to space ,typically to handle DOS-format files
            ipos=ipos+1
         CASE DEFAULT                         ! c is anything else other than a tab,newline,or return  insert it in output string
            IF(ipos > lenout)THEN
               write(*,*)"*notabs* output string overflow"
               EXIT
            ELSE
               outstr(ipos:ipos)=c
               ipos=ipos+1
            ENDIF
         END SELECT expand_tabs
      enddo SCAN_LINE
!===================================================================================================================================
      ipos=MIN(ipos,lenout)                   ! tabs or newline or return characters or last character might have gone too far
      ilen=LEN_TRIM(outstr(:ipos))            ! trim trailing spaces
!===================================================================================================================================
END SUBROUTINE notabs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!    basename(3f) - [M_io] return last component from filename
!    (LICENSE:PD)
! 
! SYNOPSIS
!    function basename(FILENAME,SUFFIX) result (LEAF)
! 
!      character(len=:),allocatable :: FILENAME
!      character(len=*),intent(in),optional :: SUFFIX
!      character(len=*),intent(in) :: LEAF
! 
! DESCRIPTION
!    Output LEAF of filename with directory paths removed.
! 
!    Assumes leaf separator is a slash or backslash as determined by
!    separator(3f) and that filename does not contain trailing spaces.
! 
! OPTIONS
!      FILENAME   pathname to extract the last leaf from
!      SUFFIX     suffix to remove. If not present
!                 the rightmost ".string" string is removed.
!                 If present the LEAF is returned with any matching
!                 suffix removed.
! 
! RETURNS
!      LEAF  returned leaf name
! 
! EXAMPLES
!   Sample program:
! 
!    program demo_basename
!    use M_io, only : basename
!    implicit none
!    character(len=:),allocatable :: fn
!    integer                      :: filename_length
!    integer                      :: i
!    ! get pathname from command line arguments
!    do i = 1, command_argument_count()
!       call get_command_argument (i, length=filename_length)
!       if(allocated(fn))deallocate(fn)
!       allocate(character(len=filename_length) :: fn)
!       call get_command_argument (i, value=fn)
!       ! leaf with any suffix removed
!       ! leaf with suffix retained
!       ! with suffix unless it is ".f90"
!       write(*,'(*(a,1x))') basename(fn), basename(fn,''), basename(fn,'.f90')
!       deallocate(fn)
!    enddo
!    end program demo_basename
! 
!   Sample program executions:
! 
!     $demo_basename /usr/bin/
!     bin bin bin
!     $demo_basename dir1/fred.x dir2/.y
!     fred fred.x fred.x
!     .y .y .y
!     $demo_basename stdio.h
!     stdio stdio.h stdio.h
!     $demo_basename /name.f90
!     name name.f90 name
! SEE ALSO
!    basename(3c), basename(3c), readlink(3c), realpath(3c)
! AUTHOR
!    John S. Urban
! LICENSE
!    Public Domain
! PRODUCT:        CLI library utilities and examples
! PROGRAM:        basename(3f)
! DESCRIPTION:    strip last component from filename
! VERSION:        1.0.0
! DATE:           2015-06-26
! AUTHOR:         John S. Urban
! REPORTING BUGS: http://www.urbanjost.altervista.org/
! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
function basename(filename,suffix) result (leaf)
implicit none

! ident_9="@(#)M_io::basename(3f): strip last component from filename"

character(len=*),intent(in)          :: filename
character(len=*),intent(in),optional :: suffix
character(len=:),allocatable         :: leaf
integer                              :: iend
integer                              :: i
integer,parameter                    :: maxlen=4096
character(len=maxlen)                :: name
character(len=maxlen)                :: bname
character(len=maxlen)                :: extension
character(len=1)                 :: sep
   sep=separator()
   iend=len_trim(filename)
   do i=iend,1,-1
      if(filename(i:i).ne.sep)exit
      iend=iend-1
   enddo
   call splitpath(filename(:iend),name=name,basename=bname,ext=extension)
   if(present(suffix))then
      leaf=merge(bname,name,suffix.eq.extension)
   else
      leaf=bname
   endif
   if(leaf.eq.'')leaf=name
   leaf=trim(leaf)
end function basename
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!    filewrite(3f) - [M_io] A simple write of a CHARACTER array to a file
!    (LICENSE:PD)
! 
! SYNOPSIS
!     function filewrite(filename,data,status) result(ierr)
! 
!      character(len=*),intent(in) :: filename
!      character(len=*),intent(in) :: data(:)
!      character(len=*),intent(in),optional :: status
!      character(len=*),intent(in),optional :: position
!      integer                     :: ierr
! DESCRIPTION
!   A convenience procedure for writing a CHARACTER array as
!   a new file.
! OPTION
!   FILENAME   file to create or write. If the name ends
!              in ">" the default for STATUS changes to
!              "REPLACE". If it ends ">>" STATUS changes to
!              "UNKNOWN" and the default POSTION changes to "APPEND".
!   DATA       CHARACTER array to write to file
!   STATUS     STATUS to use on OPEN(7f). Defaults to "NEW"
!              allowed values are  NEW|REPLACE|OLD|SCRATCH|UNKNOWN
!   POSITION   POSITION to use of OPEN(7f). Defaults to "REWIND".
!              allowed values are  ASIS|REWIND|APPEND
! RETURNS
!   IERR       status value. Zero indicates no error occurred
! EXAMPLE
!  Sample program:
! 
!     program demo_filewrite
!     use M_io, only : filewrite
!     implicit none
!     integer :: ierr
!     character(len=:),allocatable :: data(:)
!        data=[ character(len=80) :: &
!             &'This is the text to write  ', &
!             &'into the file. It will be  ', &
!             &'trimmed on the right side. ', &
!             &' ', &
!             &'     That is all Folks!    ', &
!             &'']
!        ierr=filewrite('_scratch.txt',data)
!     end program demo_filewrite
! 
! AUTHOR
!    John S. Urban
! LICENSE
!    Public Domain
function filewrite(filename,filedata,status,position) result (ierr)
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
! write filedata to file filename
character(len=*),intent(in)           :: filename
character(len=*),intent(in)           :: filedata(:)
character(len=*),intent(in),optional  :: status
character(len=*),intent(in),optional  :: position
integer                               :: ierr
integer                               :: lun, i, ios, ilen
character(len=256)                    :: message
character(len=:),allocatable          :: file
character(len=:),allocatable          :: local_status
character(len=:),allocatable          :: local_position
character(len=:),allocatable          :: default_status
character(len=:),allocatable          :: default_position
   ierr=0
   default_status='NEW'
   default_position='REWIND'
   file=trim(adjustl(filename))//'  '
   ilen=max(len_trim(file),2)
   if(file(ilen-1:ilen).eq.'>>')then
      ilen=ilen-2
      file=file(:ilen)
      default_status='UNKNOWN'
      default_position='APPEND'
   elseif(file(ilen:ilen).eq.'>')then
      ilen=ilen-1
      file=file(:ilen)
      default_status='REPLACE'
   else
      file=trim(file)
   endif
   if(present(position))then; local_position=position; else; local_position=default_position; endif
   if(present(status))then;   local_status=status;     else; local_status=default_status;     endif
   if(file.ne.' ')then
      open(file=file, &
      & newunit=lun, &
      & form='formatted', &         !  FORM      =  FORMATTED   |  UNFORMATTED
      & access='sequential', &      !  ACCESS    =  SEQUENTIAL  |  DIRECT       |  STREAM
      & action='write', &           !  ACTION    =  READ|WRITE  |  READWRITE
      & position=local_position, &  !  POSITION  =  ASIS        |  REWIND       |  APPEND
      & status=local_status, &      !  STATUS    =  NEW         |  REPLACE      |  OLD     |  SCRATCH   | UNKNOWN
      & iostat=ios, &
      & iomsg=message)
   else
      lun=stdout
      ios=0
   endif
   if(ios.ne.0)then
      write(stderr,'(*(a,1x))')'*filewrite* error:',file,trim(message)
      ierr=ios
   else
      do i=1,size(filedata)                                                    ! write file
         write(lun,'(a)',iostat=ios,iomsg=message)trim(filedata(i))
         if(ios.ne.0)then
            write(stderr,'(*(a,1x))')'*filewrite* error:',file,trim(message)
            ierr=ios
            exit
         endif
      enddo
   endif
   close(unit=lun,iostat=ios,iomsg=message)                                 ! close file
   if(ios.ne.0)then
      write(stderr,'(*(a,1x))')'*filewrite* error:',trim(message)
      ierr=ios
   endif
end function filewrite
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!    joinpath(3f) - [M_io] join parts of a pathname together
!    (LICENSE:PD)
! 
! SYNOPSIS
!    function joinpath(a1,a2,a3,a4,a5,a6,a7,a8,a9)  result(path)
! 
!     character(len=*), intent(in)           :: a1, a2
!     character(len=*), intent(in), optional :: a3, a4, a5, a6, a7, a8, a9
!     character(len=:), allocatable          :: path
! DESCRIPTION
! OPTIONS
!     a1,a2  the first two pathname sections to join. Required
!     a3-a9  additional optional sections to join
! RETURNS
!     pathname sections joined together with trailing spaces removed from the ends
!     of sections and a separator (as returned by separator(3f) placed between
!     them, and duplicate adjacent separators removed accept for one beginning the
!     joined pathname.
! EXAMPLE
!   Sample program
! 
!      program demo_joinpath
!      use M_io, only : joinpath
!      implicit none
!         write(*,*)joinpath('/share/user','/man/','man3','joinpath.3m_io'//'.gz')
!      end program demo_joinpath
! AUTHOR
!    John S. Urban
! LICENSE
!    Public Domain
!===================================================================================================================================
function joinpath(a1,a2,a3,a4,a5,a6,a7,a8,a9) result(path)
   ! Construct path by joining strings with os file separator
   !
   character(len=*), intent(in)           :: a1, a2
   character(len=*), intent(in), optional :: a3, a4, a5, a6, a7, a8, a9
   character(len=:), allocatable          :: path
   character(len=1)                       :: filesep

   filesep = separator()
   if(a1.ne.'')then
      path = trim(a1) // filesep // trim(a2)
   else
      path = trim(a2)
   endif
   if (present(a3)) path = path // filesep // trim(a3)
   if (present(a4)) path = path // filesep // trim(a4)
   if (present(a5)) path = path // filesep // trim(a5)
   if (present(a6)) path = path // filesep // trim(a6)
   if (present(a7)) path = path // filesep // trim(a7)
   if (present(a8)) path = path // filesep // trim(a8)
   if (present(a9)) path = path // filesep // trim(a9)
   path=adjustl(path//'  ')
   call substitute(path,filesep//filesep,filesep,start=2) ! some systems allow names starting with '//' or '\\'
   path=trim(path)
end function joinpath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!    separator(3f) - [M_io] try to determine pathname directory separator character
!    (LICENSE:PD)
! 
! SYNOPSIS
!    function separator() result(sep)
! 
!     character(len=1) :: sep
! 
! DESCRIPTION
!    First using the name the program was invoked with, then the name
!    returned by an INQUIRE(3f) of that name, then ".\NAME" and "./NAME"
!    try to determine the seperator character used to separate directory
!    names from file basenames.  Can be very system dependent. If the
!    queries fail the default returned is "/".
! 
! EXAMPLE
!   sample usage
! 
!    program demo_separator
!    use M_io, separator
!       write(*,*)'separator=',separator()
!    end program demo_separator
function separator() result(sep)
! use the pathname returned as arg0 to determine pathname separator
implicit none
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: istat
logical                      :: existing
character(len=1)             :: sep
!*ifort_bug*!character(len=1),save        :: sep_cache=' '
character(len=4096)          :: name
character(len=:),allocatable :: fname

   !*ifort_bug*!   if(sep_cache.ne.' ')then  ! use cached value. NOTE:  A parallel code might theoretically use multiple OS
   !*ifort_bug*!      sep=sep_cache
   !*ifort_bug*!      return
   !*ifort_bug*!   endif

   arg0_length=0
   name=' '
   call get_command_argument(0,length=arg0_length,status=istat)
   if(allocated(arg0))deallocate(arg0)
   allocate(character(len=arg0_length) :: arg0)
   call get_command_argument(0,arg0,status=istat)
   ! check argument name
   if(index(arg0,'\').ne.0)then
      sep='\'
   elseif(index(arg0,'/').ne.0)then
      sep='/'
   else
      ! try name returned by INQUIRE(3f)
      existing=.false.
      name=' '
      inquire(file=arg0,iostat=istat,exist=existing,name=name)
      if(index(name,'\').ne.0)then
         sep='\'
      elseif(index(name,'/').ne.0)then
         sep='/'
      else
         ! well, try some common syntax and assume in current directory
         fname='.\'//arg0
         inquire(file=fname,iostat=istat,exist=existing)
         if(existing)then
            sep='/'
         else
            fname='./'//arg0
            inquire(file=fname,iostat=istat,exist=existing)
            if(existing)then
               sep='/'
            else
               !*!write(*,*)'<WARNING>unknown system directory path separator'
               sep='/'
            endif
         endif
      endif
   endif
   !*ifort_bug*!sep_cache=sep
end function separator
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!    getname(3f) - [M_system:ENVIRONMENT] get environment variable
!                   from Fortran by calling get_environment_variable(3f)
!    (LICENSE:PD)
! 
! SYNOPSIS
!    function getname() result(name)
! 
!     character(len=:),allocatable         :: getname
! 
! DESCRIPTION
!    The getname() returns the name of the current executable using
!    get_command_argument(3f) and inquire(3f).
! 
! EXAMPLE
!    Sample getting a pathname of current executable:
! 
!      program demo_getname
!      use M_system, only : getname
!      implicit none
!         write(*,*)'Running',getname()
!      end program demo_getname
! 
! AUTHOR
!        John S. Urban
! 
! LICENSE
!        Public Domain
function getname() result(name)
! get the pathname of arg0
implicit none
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: istat
character(len=4096)          :: long_name
character(len=:),allocatable :: name
   arg0_length=0
   name=''
   long_name=''
   call get_command_argument(0,length=arg0_length,status=istat)
   if(istat.eq.0)then
      if(allocated(arg0))deallocate(arg0)
      allocate(character(len=arg0_length) :: arg0)
      call get_command_argument(0,arg0,status=istat)
      if(istat.eq.0)then
         inquire(file=arg0,iostat=istat,name=long_name)
         name=trim(long_name)
      else
         name=arg0
      endif
   endif
end function getname
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!     which(3f) - [M_io] given a command name find the pathname by searching
!                 the directories in the environment variable $PATH
!     (LICENSE:PD)
! 
! SYNTAX
!   function which(command) result(pathname)
! 
!    character(len=*),intent(in)  :: command
!    character(len=:),allocatable :: pathname
! 
! DESCRIPTION
! 
!    Given a command name find the first file with that name in the directories
!    specified by the environment variable $PATH.
! 
! 
! OPTIONS
!    COMMAND   the command to search for
! RETURNS
!    PATHNAME  the first pathname found in the current user path. Returns blank
!              if the command is not found.
! 
! EXAMPLE
!   Sample program:
! 
!   Checking the error message and counting lines:
! 
!     program demo_which
!     use M_io, only : which
!     implicit none
!        write(*,*)'ls is',which('ls')
!        write(*,*)'dir is',which('dir')
!        write(*,*)'install is',which('install')
!     end program demo_which
! 
! AUTHOR
!    John S. Urban
! LICENSE
!    Public Domain
function which(command) result(pathname)
character(len=*),intent(in)         :: command
character(len=:),allocatable        :: pathname
character(len=:),allocatable        :: checkon

character(len=:),allocatable        :: path
character(len=:),allocatable        :: paths(:)
character(len=256)                  :: message
character(len=1)                    :: sep
integer                             :: howbig, stat, i
logical                             :: existing
   pathname=''
   ! get length required to hold value
   call get_environment_variable('PATH', length=howbig,status=stat)
   select case (stat)
   case (1)
      print *, "<WARNING>*which*:PATH is not defined in the environment. Strange..."
   case (2)
      print *, "<WARNING>*which*:This processor doesn't support environment variables. Boooh!"
   case default
      allocate(character(len=howbig) :: path)  ! make string to hold value of sufficient size
      call get_environment_variable('PATH', path) ! get value
      sep=separator()
      sep=merge('%',':',sep.eq.'\')
      call split(path,paths,delimiters=sep)
      existing=.false.
      do i=1,size(paths)
         checkon=joinpath(paths(i),command)
         inquire(file=checkon,exist=existing,iostat=stat,iomsg=message)
         if(stat.ne.0)then
            write(*,'(a,a)')'<WARNING>*which*',trim(message)
         endif
         if(existing)then
            pathname=checkon
            exit
         endif
      enddo
   end select
end function which
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
! rd(3f) - [M_io] ask for string from standard input with user-definable prompt
! (LICENSE:PD)
! 
!   function rd(prompt,default) result(strout)
! 
!    character(len=*),intent(in)              :: prompt
! 
!    character(len=*),intent(in)              :: default
!          or
!    integer,intent(in)                       :: default
!          or
!    real,intent(in)                          :: default
!          or
!    doubleprecision,intent(in)               :: default
! 
!    character(len=:),allocatable,intent(out) :: strout
! 
! DESCRIPTION
!    Ask for string or value from standard input with user-definable prompt
!    up to 20 times.
! 
!    Do not use the function in an I/O statement as not all versions of
!    Fortran support this form of recursion. Numeric values may be input
!    in standard INTEGER, REAL, and DOUBLEPRECISION formats or as whole
!    numbers in base 2 to 36 in the format BASE#VALUE.
! 
! OPTIONS
!    prompt    Prompt string; displayed on same line as input is read from
!    default   default answer on carriage-return. The type of the default
!              determines the type of the output.
! RETURNS
!    strout    returned string or value. If an end-of-file or system error
!              is encountered the string "EOF" is returned, or a "Nan"
!              numeric value.
! EXAMPLE
!   Sample program:
! 
!    program demo_rd
!    use M_io, only : rd
!    character(len=:),allocatable :: mystring
!    doubleprecision              :: d
!    real                         :: r
!    integer                      :: i
! 
!    INFINITE: do
!       mystring=rd('Enter string or "STOP":',default='Today')
!       if(mystring.eq.'STOP')stop
!       i=rd('Enter integer:',default=huge(0))
!       r=rd('Enter real:',default=huge(0.0))
!       d=rd('Enter double:',default=huge(0.0d0))
! 
!       write(*,*)'I=', i, 'R=', r, 'D=',d,  'MYSTRING=', mystring
!    enddo INFINITE
! 
!    end program demo_rd
! 
! AUTHOR
!    John S. Urban, 1993
! LICENSE
!    Public Domain
function rd_character(prompt,default) result(strout)
! 1995 John S. Urban
!
implicit none

! ident_15="@(#)M_io::rd_character(3fp): ask for string from standard input with user-definable prompt"

character(len=*),intent(in)  :: prompt
character(len=*),intent(in)  :: default
character(len=:),allocatable :: strout

integer                      :: len_default
integer                      :: igot
integer                      :: ierr
integer                      :: icount
!===================================================================================================================================
   len_default=len(prompt)
!===================================================================================================================================
   do icount=1,20                                                  ! prevent infinite loop on error or end-of-file
      if(len_default.gt.0)write(*,'(a,'' '')',advance='no')prompt  ! write prompt
      ierr=getline(strout,stdin)                                  ! get back string
      igot=len(strout)
      if(ierr.ne.0)then
         strout='EOF'
         cycle
      elseif(igot.eq.0.and.len_default.gt.0)then
         strout=default
         exit
      elseif(igot.le.0)then
         write(*,*)'*rd* blank string not allowed'
         cycle
      else
         exit
      endif
   enddo
end function rd_character
!===================================================================================================================================
function rd_doubleprecision(prompt,default) result(dvalue)
implicit none

! ident_16="@(#)M_io::rd_doubleprecision(3fp): ask for number from standard input with user-definable prompt"

doubleprecision              :: dvalue
integer                      :: ivalue
character(len=*),intent(in)  :: prompt
doubleprecision,intent(in)   :: default
character(len=:),allocatable :: strout
character(len=:),allocatable :: message
integer                      :: itest

   dvalue=default
   strout=adjustl(rd_character(prompt,'NaN'))

   ! 1 for an integer [-+]NNNNN
   ! 2 for a whole number [-+]NNNNN.
   ! 3 for a real value [-+]NNNNN.MMMM
   ! 4 for a exponential value [-+]NNNNN.MMMM[-+]LLLL [-+]NNNNN.MMMM[ed][-+]LLLL
   ! values less than 1 represent an error
   if(strout.eq.'NaN')then
      dvalue=default
   elseif(index(strout,'#').ne.0)then
      if( decodebase(strout,0,ivalue))then
         dvalue=ivalue
      else
         write(*,*)'ERROR> could not convert ',strout
      endif
   else
      itest=isnumber(strout,message)
      if(itest.gt.0)then
         dvalue=s2v(strout)
      else
         write(*,*)' ERROR> for ',strout,' ',itest,':',trim(message)
      endif
   endif
end function rd_doubleprecision
!===================================================================================================================================
function rd_real(prompt,default) result(rvalue)
implicit none

! ident_17="@(#)M_io::rd_real(3fp): ask for number from standard input with user-definable prompt"

real                         :: rvalue
character(len=*),intent(in)  :: prompt
real,intent(in)              :: default
   rvalue=real(rd_doubleprecision(prompt,dble(default)))
end function rd_real
!===================================================================================================================================
function rd_integer(prompt,default) result(ivalue)
implicit none

! ident_18="@(#)M_io::rd_integer(3fp): ask for number from standard input with user-definable prompt"

integer                      :: ivalue
character(len=*),intent(in)  :: prompt
integer,intent(in)           :: default
   ivalue=nint(rd_doubleprecision(prompt,dble(default)))
end function rd_integer
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function lenset(line,length) result(strout)

!character(len=*),parameter::ident_36="@(#)M_strings::lenset(3f): return string trimmed or padded to specified length"

character(len=*),intent(in)  ::  line
integer,intent(in)           ::  length
character(len=length)        ::  strout
   strout=line
end function lenset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine a2i(chars,valu,ierr)

!character(len=*),parameter::ident_41="@(#)M_strings::a2i(3fp): subroutine returns integer value from string"

character(len=*),intent(in) :: chars                      ! input string
integer,intent(out)         :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(valu8.le.huge(valu))then
      if(valu8.le.huge(valu))then
         valu=int(valu8)
      else
         write(*,*)'sc','*a2i*','- value too large',valu8,'>',huge(valu)
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2i
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2d(chars,valu,ierr,onerr)

!character(len=*),parameter::ident_42="@(#)M_strings::a2d(3fp): subroutine returns double value from string"

!     1989,2016 John S. Urban.
!
!  o  works with any g-format input, including integer, real, and exponential.
!  o  if an error occurs in the read, iostat is returned in ierr and value is set to zero.  if no error occurs, ierr=0.
!  o  if the string happens to be 'eod' no error message is produced so this string may be used to act as an end-of-data.
!     IERR will still be non-zero in this case.
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: chars                        ! input string
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu                         ! value read from input string
integer,intent(out)          :: ierr                         ! error flag (0 == no error)
class(*),optional,intent(in) :: onerr
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"     ! format used to build frmt
character(len=15)            :: frmt                         ! holds format built to read input string
character(len=256)           :: msg                          ! hold message from I/O errors
integer                      :: intg
integer                      :: pnd
integer                      :: basevalue, ivalu
character(len=3),save        :: nan_string='NaN'
!----------------------------------------------------------------------------------------------------------------------------------
   ierr=0                                                       ! initialize error flag to zero
   local_chars=chars
   msg=''
   if(len(local_chars).eq.0)local_chars=' '
   call substitute(local_chars,',','')                          ! remove any comma characters
   pnd=scan(local_chars,'#:')
   if(pnd.ne.0)then
      write(frmt,fmt)pnd-1                                      ! build format of form '(BN,Gn.0)'
      read(local_chars(:pnd-1),fmt=frmt,iostat=ierr,iomsg=msg)basevalue   ! try to read value from string
      if(decodebase(local_chars(pnd+1:),basevalue,ivalu))then
         valu=real(ivalu,kind=kind(0.0d0))
      else
         valu=0.0d0
         ierr=-1
      endif
   else
      select case(local_chars(1:1))
      case('z','Z','h','H')                                     ! assume hexadecimal
         frmt='(Z'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('b','B')                                             ! assume binary (base 2)
         frmt='(B'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('o','O')                                             ! assume octal
         frmt='(O'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case default
         write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
         read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
      end select
   endif
   if(ierr.ne.0)then                                            ! if an error occurred ierr will be non-zero.
      if(present(onerr))then
         select type(onerr)
         type is (integer)
            valu=onerr
         type is (real)
            valu=onerr
         type is (doubleprecision)
            valu=onerr
         end select
      else                                                      ! set return value to NaN
         read(nan_string,'(g3.3)')valu
      endif
      if(local_chars.ne.'eod')then                           ! print warning message except for special value "eod"
         write(*,*)'sc','*a2d* - cannot produce number from string ['//trim(chars)//']'
         if(msg.ne.'')then
            write(*,*)'*a2d* - ['//trim(msg)//']'
         endif
      endif
   endif
end subroutine a2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
doubleprecision function s2v(chars,ierr,onerr)
!  1989 John S. Urban

!character(len=*),parameter::ident_43="@(#)M_strings::s2v(3f): returns doubleprecision number from string"


character(len=*),intent(in)  :: chars
integer,optional             :: ierr
doubleprecision              :: valu
integer                      :: ierr_local
class(*),intent(in),optional :: onerr

   ierr_local=0
   if(present(onerr))then
      call a2d(chars,valu,ierr_local,onerr)
   else
      call a2d(chars,valu,ierr_local)
   endif
   if(present(ierr))then ! if error is not returned stop program on error
      ierr=ierr_local
      s2v=valu
   elseif(ierr_local.ne.0)then
      write(*,*)'*s2v* stopped while reading '//trim(chars)
      stop 1
   else
      s2v=valu
   endif
end function s2v
!===================================================================================================================================
! calls to s2v(3f) for extending intrinsics int(3f), real(3f), dble(3f)
!===================================================================================================================================
doubleprecision function dble_s2v(chars)
character(len=*),intent(in) :: chars
   dble_s2v=s2v(chars)
end function dble_s2v
!===================================================================================================================================
real function real_s2v(chars)
character(len=*),intent(in) :: chars
   real_s2v=real(s2v(chars))
end function real_s2v
!===================================================================================================================================
integer function int_s2v(chars)
character(len=*),intent(in) :: chars
   int_s2v=int(s2v(chars))
end function int_s2v
!===================================================================================================================================
function ints_s2v(chars)
integer,allocatable         :: ints_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(ints_s2v(isize))
   do i=1,isize
      ints_s2v(i)=int(s2v(chars(i)))
   enddo
end function ints_s2v
!===================================================================================================================================
function reals_s2v(chars)
real,allocatable            :: reals_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(reals_s2v(isize))
   do i=1,isize
      reals_s2v(i)=real(s2v(chars(i)))
   enddo
end function reals_s2v
!===================================================================================================================================
function dbles_s2v(chars)
doubleprecision,allocatable :: dbles_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(dbles_s2v(isize))
   do i=1,isize
      dbles_s2v(i)=s2v(chars(i))
   enddo
end function dbles_s2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()())()()()()()()()()()()()()!
!===================================================================================================================================
function s2vs(string,delim) result(darray)

!character(len=*),parameter::ident_55="@(#)M_strings::s2vs(3f): function returns array of values from a string"

character(len=*),intent(in)        :: string                       ! keyword to retrieve value for from dictionary
character(len=*),optional          :: delim                        ! delimiter characters
character(len=:),allocatable       :: delim_local
doubleprecision,allocatable        :: darray(:)                    ! function type

character(len=:),allocatable       :: carray(:)                    ! convert value to an array using split(3f)
integer                            :: i
integer                            :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(delim))then
      delim_local=delim
   else
      delim_local=' ;,'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call split(string,carray,delimiters=delim_local)         ! split string into an array
   allocate(darray(size(carray)))                           ! create the output array
   do i=1,size(carray)
      call string_to_value(carray(i), darray(i), ier)       ! convert the string to a numeric value
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end function s2vs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function i2s(ivalue,fmt) result(outstr)

!character(len=*),parameter::ident_47="@(#)M_strings::i2s(3fp): private function returns string given integer value"

integer,intent(in)           :: ivalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(ivalue,string,fmt=fmt)
   else
      call value_to_string(ivalue,string)
   endif
   outstr=trim(string)
end function i2s
!===================================================================================================================================
subroutine value_to_string(gval,chars,length,err,fmt,trimz)

!character(len=*),parameter::ident_40="@(#)M_strings::value_to_string(3fp): subroutine returns a string from a value"

class(*),intent(in)                      :: gval
character(len=*),intent(out)             :: chars
integer,intent(out),optional             :: length
integer,optional                         :: err
integer                                  :: err_local
character(len=*),optional,intent(in)     :: fmt         ! format to write value with
logical,intent(in),optional              :: trimz
character(len=:),allocatable             :: fmt_local
character(len=1024)                      :: msg

!  Notice that the value GVAL can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,LOGICAL)

   if (present(fmt)) then
      select type(gval)
      type is (integer)
         fmt_local='(i0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (real)
         fmt_local='(bz,g23.10e3)'
         fmt_local='(bz,g0.8)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         fmt_local='(bz,g0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (logical)
         fmt_local='(l1)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      class default
         write(*,*)'*value_to_string* UNKNOWN TYPE'
         chars=' '
      end select
      if(fmt.eq.'') then
         chars=adjustl(chars)
         call trimzeros(chars)
      endif
   else                                                  ! no explicit format option present
      err_local=-1
      select type(gval)
      type is (integer)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (real)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (logical)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      class default
         chars=''
      end select
      chars=adjustl(chars)
      if(index(chars,'.').ne.0) call trimzeros(chars)
   endif
   if(present(trimz))then
      if(trimz)then
         chars=adjustl(chars)
         call trimzeros(chars)
      endif
   endif

   if(present(length)) then
      length=len_trim(chars)
   endif

   if(present(err)) then
      err=err_local
   elseif(err_local.ne.0)then
      !! cannot currently do I/O from a function being called from I/O
      !!write(ERROR_UNIT,'(a)')'*value_to_string* WARNING:['//trim(msg)//']'
      chars=chars//' *value_to_string* WARNING:['//trim(msg)//']'
   endif

end subroutine value_to_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
logical function decodebase(string,basein,out_baseten)
implicit none

!character(len=*),parameter::ident_72="@(#)M_strings::decodebase(3f): convert whole number string in base [2-36] to base 10 number"

character(len=*),intent(in)  :: string
integer,intent(in)           :: basein
integer,intent(out)          :: out_baseten

character(len=len(string))   :: string_local
integer           :: long, i, j, k
real              :: y
real              :: mult
character(len=1)  :: ch
real,parameter    :: XMAXREAL=real(huge(1))
integer           :: out_sign
integer           :: basein_local
integer           :: ipound
integer           :: ierr

  string_local=upper(trim(adjustl(string)))
  decodebase=.false.

  ipound=index(string_local,'#')                                       ! determine if in form [-]base#whole
  if(basein.eq.0.and.ipound.gt.1)then                                  ! split string into two values
     call string_to_value(string_local(:ipound-1),basein_local,ierr)   ! get the decimal value of the base
     string_local=string_local(ipound+1:)                              ! now that base is known make string just the value
     if(basein_local.ge.0)then                                         ! allow for a negative sign prefix
        out_sign=1
     else
        out_sign=-1
     endif
     basein_local=abs(basein_local)
  else                                                                 ! assume string is a simple positive value
     basein_local=abs(basein)
     out_sign=1
  endif

  out_baseten=0
  y=0.0
  ALL: if(basein_local<2.or.basein_local>36) then
    print *,'(*decodebase* ERROR: Base must be between 2 and 36. base=',basein_local
  else ALL
     out_baseten=0;y=0.0; mult=1.0
     long=LEN_TRIM(string_local)
     do i=1, long
        k=long+1-i
        ch=string_local(k:k)
        if(ch.eq.'-'.and.k.eq.1)then
           out_sign=-1
           cycle
        endif
        if(ch<'0'.or.ch>'Z'.or.(ch>'9'.and.ch<'A'))then
           write(*,*)'*decodebase* ERROR: invalid character ',ch
           exit ALL
        endif
        if(ch<='9') then
              j=IACHAR(ch)-IACHAR('0')
        else
              j=IACHAR(ch)-IACHAR('A')+10
        endif
        if(j>=basein_local)then
           exit ALL
        endif
        y=y+mult*j
        if(mult>XMAXREAL/basein_local)then
           exit ALL
        endif
        mult=mult*basein_local
     enddo
     decodebase=.true.
     out_baseten=nint(out_sign*y)*sign(1,basein)
  endif ALL
end function decodebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine trimzeros(string)

!character(len=*),parameter::ident_50="@(#)M_strings::trimzeros(3fp): Delete trailing zeros from numeric decimal string"

! if zero needs added at end assumes input string has room
character(len=*)             :: string
character(len=len(string)+2) :: str
character(len=len(string))   :: exp          ! the exponent string if present
integer                      :: ipos         ! where exponent letter appears if present
integer                      :: i, ii
   str=string                                ! working copy of string
   ipos=scan(str,'eEdD')                     ! find end of real number if string uses exponent notation
   if(ipos>0) then                           ! letter was found
      exp=str(ipos:)                         ! keep exponent string so it can be added back as a suffix
      str=str(1:ipos-1)                      ! just the real part, exponent removed will not have trailing zeros removed
   endif
   if(index(str,'.').eq.0)then               ! if no decimal character in original string add one to end of string
      ii=len_trim(str)
      str(ii+1:ii+1)='.'                     ! add decimal to end of string
   endif
   do i=len_trim(str),1,-1                   ! scanning from end find a non-zero character
      select case(str(i:i))
      case('0')                              ! found a trailing zero so keep trimming
         cycle
      case('.')                              ! found a decimal character at end of remaining string
         if(i.le.1)then
            str='0'
         else
            str=str(1:i-1)
         endif
         exit
      case default
         str=str(1:i)                        ! found a non-zero character so trim string and exit
         exit
      end select
   end do
   if(ipos>0)then                            ! if originally had an exponent place it back on
      string=trim(str)//trim(exp)
   else
      string=str
   endif
end subroutine trimzeros
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine substitute(targetline,old,new,ierr,start,end)

!character(len=*),parameter::ident_11="@(#)M_strings::substitute(3f): Globally substitute one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*)               :: targetline         ! input line to be changed
character(len=*),intent(in)    :: old                ! old substring to replace
character(len=*),intent(in)    :: new                ! new substring
integer,intent(out),optional   :: ierr               ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
integer,intent(in),optional    :: start              ! start sets the left margin
integer,intent(in),optional    :: end                ! end sets the right margin
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(targetline)) :: dum1               ! scratch string buffers
integer                        :: ml, mr, ier1
integer                        :: maxlengthout       ! MAXIMUM LENGTH ALLOWED FOR NEW STRING
integer                        :: original_input_length
integer                        :: len_old, len_new
integer                        :: ladd
integer                        :: ir
integer                        :: ind
integer                        :: il
integer                        :: id
integer                        :: ic
integer                        :: ichar
!-----------------------------------------------------------------------------------------------------------------------------------
   if (present(start)) then                            ! optional starting column
      ml=start
   else
      ml=1
   endif
   if (present(end)) then                              ! optional ending column
      mr=end
   else
      mr=len(targetline)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ier1=0                                              ! initialize error flag/change count
   maxlengthout=len(targetline)                        ! max length of output string
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   dum1(:)=' '                                         ! initialize string to build output in
   id=mr-ml                                            ! check for window option !! change to optional parameter(s)
!-----------------------------------------------------------------------------------------------------------------------------------
   len_old=len(old)                                    ! length of old substring to be replaced
   len_new=len(new)                                    ! length of new substring to replace old substring
   if(id.le.0)then                                     ! no window so change entire input string
      il=1                                             ! il is left margin of window to change
      ir=maxlengthout                                  ! ir is right margin of window to change
      dum1(:)=' '                                      ! begin with a blank line
   else                                                ! if window is set
      il=ml                                            ! use left margin
      ir=min0(mr,maxlengthout)                         ! use right margin or rightmost
      dum1=targetline(:il-1)                           ! begin with what's below margin
   endif                                               ! end of window settings
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old.eq.0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichar=len_new + original_input_length
      if(ichar.gt.maxlengthout)then
         write(*,*)'*substitute* new line will be too long'
         ier1=-1
         if (present(ierr))ierr=ier1
         return
      endif
      if(len_new.gt.0)then
         dum1(il:)=new(:len_new)//targetline(il:original_input_length)
      else
         dum1(il:)=targetline(il:original_input_length)
      endif
      targetline(1:maxlengthout)=dum1(:maxlengthout)
      ier1=1                                           ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ier1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ichar=il                                            ! place to put characters into output string
   ic=il                                               ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1    ! try to find start of old string in remaining part of input in change window
      if(ind.eq.ic-1.or.ind.gt.ir)then                 ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      ier1=ier1+1                                      ! found an old string to change, so increment count of changes
      if(ind.gt.ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         if(ichar-1+ladd.gt.maxlengthout)then
            ier1=-1
            exit loop
         endif
         dum1(ichar:)=targetline(ic:ind-1)
         ichar=ichar+ladd
      endif
      if(ichar-1+len_new.gt.maxlengthout)then
         ier1=-2
         exit loop
      endif
      if(len_new.ne.0)then
         dum1(ichar:)=new(:len_new)
         ichar=ichar+len_new
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ier1)
   case (:-1)
      write(*,*)'*substitute* new line will be too long'
   case (0)                                                ! there were no changes made to the window
   case default
      ladd=original_input_length-ic
      if(ichar+ladd.gt.maxlengthout)then
         write(*,*)'*substitute* new line will be too long'
         ier1=-1
         if(present(ierr))ierr=ier1
         return
      endif
      if(ic.lt.len(targetline))then
         dum1(ichar:)=targetline(ic:max(ic,original_input_length))
      endif
      targetline=dum1(:maxlengthout)
   end select
   if(present(ierr))ierr=ier1
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine substitute
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!    isnumber(3f) - [M_strings:NUMERIC] determine if a string represents a number
!    (LICENSE:PD)
! 
! SYNOPSIS
!    function isnumber(str,msg)
! 
!     character(len=*),intent(in)  :: str
!     character(len=:),intent(out),allocatable,optional  :: msg
! 
! DESCRIPTION
!     ISNUMBER(3f) returns a value greater than zero if the string represents
!     a number, and a number less than or equal to zero if it is a bad number.
!     Blank characters are ignored.
! 
! OPTIONS
!     str  the string to evaluate as to whether it represents a numeric value
!          or not
!     msg  An optional message describing the string
! 
! RETURNS
!     isnumber  the following values are returned
! 
!                1 for an integer             [-+]NNNNN
!                2 for a whole number         [-+]NNNNN.
!                3 for a real value           [-+]NNNNN.MMMM
!                4 for a exponential value    [-+]NNNNN.MMMM[-+]LLLL
!                                             [-+]NNNNN.MMMM[ed][-+]LLLL
! 
!               values less than 1 represent an error
! 
! EXAMPLES
!   As the example shows, you can use an internal READ(3f) along with the
!   IOSTAT= parameter to check (and read) a string as well.
! 
!     program demo_isnumber
!     use M_strings, only : isnumber
!     implicit none
!     character(len=256) :: line
!     real               :: value
!     integer            :: ios
!     integer            :: answer
!     character(len=256) :: message
!     character(len=:),allocatable :: description
!        write(*,*)'Begin entering values, one per line'
!        do
!           read(*,'(a)',iostat=ios)line
!           !
!           ! try string as number using list-directed input
!           line=''
!           read(line,*,iostat=ios,iomsg=message) value
!           if(ios.eq.0)then
!              write(*,*)'VALUE=',value
!           elseif( is_iostat_end(ios) ) then
!              stop 'end of file'
!           else
!              write(*,*)'ERROR:',ios,trim(message)
!           endif
!           !
!           ! try string using isnumber(3f)
!           answer=isnumber(line,msg=description)
!           if(answer.gt.0)then
!              write(*,*) &
!              & ' for ',trim(line),' ',answer,':',description
!           else
!              write(*,*) &
!              & ' ERROR for ',trim(line),' ',answer,':',description
!           endif
!           !
!        enddo
!     end program demo_isnumber
! 
!  Example run
! 
!    > Begin entering values
!    > ERROR:          -1 End of file
!    >  ERROR for            -1 :null string
!    >10
!    > VALUE=   10.0000000
!    >  for 10            1 :integer
!    >20
!    > VALUE=   20.0000000
!    >  for 20            1 :integer
!    >20.
!    > VALUE=   20.0000000
!    >  for 20.            2 :whole number
!    >30.1
!    > VALUE=   30.1000004
!    >  for 30.1            3 :real number
!    >3e1
!    > VALUE=   30.0000000
!    >  for 3e1            4 :value with exponent
!    >1-2
!    > VALUE=   9.99999978E-03
!    >  for 1-2            4 :value with exponent
!    >100.22d-4
!    > VALUE=   1.00220004E-02
!    >  for 100.22d-4            4 :value with exponent
!    >1--2
!    > ERROR:        5010 Bad real number in item 1 of list input
!    >  ERROR for 1--2           -5 :bad number
!    >e
!    > ERROR:        5010 Bad real number in item 1 of list input
!    >  ERROR for e           -6 :missing leading value before exponent
!    >e1
!    > ERROR:        5010 Bad real number in item 1 of list input
!    >  ERROR for e1           -6 :missing leading value before exponent
!    >1e
!    > ERROR:        5010 Bad real number in item 1 of list input
!    >  ERROR for 1e           -3 :missing exponent
!    >1e+
!    > ERROR:        5010 Bad real number in item 1 of list input
!    >  ERROR for 1e+           -4 :missing exponent after sign
!    >1e+2.0
!    > ERROR:        5010 Bad real number in item 1 of list input
!    >  ERROR for 1e+2.0           -5 :bad number
! 
! AUTHOR
!    John S. Urban
! 
! LICENSE
!    Public Domain
function isNumber(string,msg,verbose)
implicit none

! ident_1="@(#)M_strings::isnumber(3f): Determines if a string is a number of not."

character(len=*),intent(in)    :: string
character(len=:),intent(out),allocatable,optional :: msg
logical,intent(in),optional                      :: verbose
integer                      :: isnumber

integer             :: i,iend
character(len=1),allocatable :: z(:)
character(len=:),allocatable :: message
logical                      :: founddigit
logical                      :: verbose_local

   i=1
   founddigit=.false.
   isnumber=0
   z=s2a(trim(nospace(string)))
   iend=size(z)
   message='not a number'
   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif
   DONE : block
      if(iend.eq.0)then
         isnumber=-1                   ! string is null
         message='null string'
         exit DONE
      endif

      if(index('+-',z(i)).ne.0) i=i+1  ! skip optional leading sign
      if(i.gt.iend)then
         isnumber=-2                   ! string was just a sign
         message='just a sign'
         exit DONE
      endif

      call next()                      ! position I to next non-digit or end of string+1

      if(i.gt.iend)then
         isnumber=1                    ! [+-]NNNNNN
         message='integer'
         exit DONE
      endif
      if(z(i).eq.'.')then              ! a period would be OK at this point
         i=i+1
      endif

      if(i.gt.iend)then                ! [+-]NNNNNN.
         isnumber=2
         message='whole number'
         exit DONE
      endif

      call next()                      ! position I to next non-digit or end of string+1
      if(i.gt.iend)then
         isnumber=3                    ! [+-]NNNNNN.MMMM
         message='real number'
         exit DONE
      endif

      if(index('eEdD',z(i)).ne.0)then
         i=i+1
         if(i.eq.2)then
            isnumber=-6                   ! [+-]NNNNNN[.[MMMM]]e but a value must follow
            message='missing leading value before exponent'
            exit DONE
         endif
      endif
      if(i.gt.iend)then
         isnumber=-3                   ! [+-]NNNNNN[.[MMMM]]e but a value must follow
         message='missing exponent'
         exit DONE
      endif
      if(.not.founddigit)then
         isnumber=-7
         message='missing value before exponent'
         exit DONE
      endif
      if(index('+-',z(i)).ne.0) i=i+1
      if(i.gt.iend)then
         isnumber=-4                   ! [+-]NNNNNN[.[MMMM]]e[+-] but a value must follow
         message='missing exponent after sign'
         exit DONE
      endif
      call next()                      ! position I to next non-digit or end of string+1
      if(i.gt.iend)then
         isnumber=4                    ! [+-]NNNNNN.MMMMe[+-]LL
         message='value with exponent'
         exit DONE
      endif
      isnumber=-5
      message='bad number'
   endblock DONE
   if(verbose_local)then
      write(*,*)trim(string)//' is '//message
   endif
   if(present(msg))then
      msg=message
   endif

contains
   subroutine next() ! move to next non-digit or end of string+1
      integer :: j
      do j=i,iend
         if(.not.isdigit(z(j)))then
            exit
         endif
         founddigit=.true.
         if(verbose_local) write(*,*)'I=',i,' J=',j,' Z(j)=',z(j)
      enddo
      i=j
      if(verbose_local)then
         write(*,*)'I and J=',i
         if(i.le.iend) then
            write(*,*)'Z(I)=',z(i)
         else
            write(*,*)'====>'
         endif
      endif
   end subroutine next
end function isNumber
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isdigit(ch) result(res)

! ident_2="@(#)M_strings::isdigit(3f): Returns .true. if ch is a digit (0-9) and .false. otherwise"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isdigit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! NAME
!    nospace(3f) - [M_strings:WHITESPACE] remove all whitespace from
!    input string
!    (LICENSE:PD)
! 
! SYNOPSIS
!    function nospace(str) - remove all whitespace from input string
! 
!     character(len=*),intent(in)          :: str
!     character(len=:),allocatable         :: nospace
! 
! DESCRIPTION
!    nospace(3f) removes space, tab, carriage return, new line, vertical
!    tab, formfeed and null characters (called "whitespace"). The output
!    is returned trimmed.
! 
! EXAMPLES
!   Sample program:
! 
!     program demo_nospace
!     use M_strings, only: nospace
!     implicit none
!     character(len=:),allocatable  :: s
!        s='  This     is      a     test  '
!        write(*,*) 'original input string is ....',s
!        write(*,*) 'processed output string is ...',nospace(s)
!        if(nospace(s).eq.'Thisisatest')then
!           write(*,*)'nospace test passed'
!        else
!           write(*,*)'nospace test error'
!        endif
!     end program demo_nospace
! 
!   Expected output
! 
!     original input string is ....  This     is      a     test
!     processed output string is ...Thisisatest
!     nospace test passed
! 
! AUTHOR
!    John S. Urban
! 
! LICENSE
!    Public Domain
function nospace(line)

! ident_3="@(#)M_strings::nospace(3f): remove all whitespace from input string"

character(len=*),intent(in)    ::  line             ! remove whitespace from this string and return it
character(len=:),allocatable   ::  nospace          ! returned string
integer                        ::  ipos             ! position to place next output character at
integer                        ::  i                ! counter to increment from beginning to end of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   allocate(nospace,mold=line)                      ! initially make output line length of input line
   nospace(:len_trim(nospace))=' '
   ipos=0
   do i=1,len_trim(line)                            ! increment from first to last character of the input line
      if ( isspace( line(i:i) ) ) cycle             ! if a blank is encountered skip it
      ipos=ipos+1                                   ! increment count of non-blank characters found
      nospace(ipos:ipos)=line(i:i)                  ! store non-blank character in output
   enddo
   nospace=trim(nospace)                            ! blank out unpacked part of line
end function nospace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isspace(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     null, space, tab, carriage return, new line, vertical tab, or formfeed
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isspace(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isspace
!!
!!##DESCRIPTION
!!     isspace(3f) returns .true. if character is a null, space, tab,
!!     carriage return, new line, vertical tab, or formfeed
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isspace  returns true if character is ASCII white space
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_isspace
!!     use M_strings, only : isspace
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(20(g0,1x))')'ISSPACE: ', &
!!        & ichar(pack( string, isspace(string) ))
!!     end program demo_isspace
!!
!!   Results:
!!
!!    ISSPACE:  0 9 10 11 12 13 32
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isspace(ch) result(res)

! ident_63="@(#)M_strings::isspace(3f): true if null,space,tab,return,new line,vertical tab, or formfeed"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(' ')                 ! space(32)
     res=.true.
   case(char(0))             ! null(0)
     res=.true.
   case(char(9):char(13))    ! tab(9), new line(10), vertical tab(11), formfeed(12), carriage return(13),
     res=.true.
   case default
     res=.false.
   end select
end function isspace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module m_io
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
 
 
!>>>>> build/dependencies/M_strings/src/M_strings.f90
!>
!!##NAME
!!    M_strings(3f) - [M_strings:INTRO] Fortran string module
!!
!!##DESCRIPTION
!!    The M_strings(3fm) module is a collection of Fortran procedures
!!    that supplement the built-in intrinsic string routines. Routines
!!    for parsing, tokenizing, changing case, substituting new strings for
!!    substrings, locating strings with simple wildcard expressions, removing
!!    tabs and line terminators and other string manipulations are included.
!!
!!    M_strings_oop(3fm) is a companion module that provides an OOP interface
!!    to the M_strings module.
!!
!!##SYNOPSIS
!!
!!  public entities:
!!
!!      use M_strings, only : split,sep,delim,chomp,strtok
!!      use M_strings, only : substitute,change,modif,transliterate,reverse
!!      use M_strings, only : replace,join
!!      use M_strings, only : upper,lower,upper_quoted
!!      use M_strings, only : rotate13
!!      use M_strings, only : adjustc,compact,nospace,indent
!!      use M_strings, only : crop,unquote,quote
!!      use M_strings, only : len_white,atleast,stretch,lenset,merge_str
!!      use M_strings, only : switch,s2c,c2s
!!      use M_strings, only : noesc,notabs,dilate,expand,visible
!!      !!use M_strings, only : uc
!!      use M_strings, only : string_to_value,string_to_values,s2v,s2vs
!!      use M_strings, only : value_to_string,v2s,msg
!!      use M_strings, only : listout,getvals
!!      use M_strings, only : glob, ends_with
!!      use M_strings, only : fmt
!!      use M_strings, only : base, decodebase, codebase, base2
!!      use M_strings, only : isalnum, isalpha, iscntrl, isdigit
!!      use M_strings, only : isgraph, islower, isprint, ispunct
!!      use M_strings, only : isspace, isupper, isascii, isblank, isxdigit
!!      use M_strings, only : isnumber
!!      use M_strings, only : fortran_name
!!      use M_strings, only : describe
!!
!!   TOKENS
!!       split  subroutine parses string using specified delimiter characters
!!              and stores tokens into an array
!!       sep    function interface to split(3f)
!!       delim  subroutine parses string using specified delimiter characters
!!              and store tokens into an array
!!       chomp  function consumes input line as it returns next token in a
!!              string using specified delimiters
!!       fmt    convert a string into a paragraph
!!       strtok tokenize a string like C strtok(3c) routine
!!
!!   EDITING
!!       substitute     subroutine non-recursively globally replaces old
!!                      substring with new substring
!!       replace        function non-recursively globally replaces old
!!                      substring with new substring using allocatable string
!!                      (version of substitute(3f) without limitation on
!!                      length of output string)
!!       change         subroutine non-recursively globally replaces old
!!                      substring with new substring with a directive like
!!                      line editor
!!       modif          subroutine modifies a string with a directive like the
!!                      XEDIT line editor MODIFY command
!!       transliterate  replace characters found in set one with characters
!!                      from set two
!!       reverse        reverse character order in a string
!!       join           join an array of CHARACTER variables with specified
!!                      separator
!!       rotate13       apply trivial encryption algorithm ROT13 to a string
!!
!!   CASE
!!       upper          function converts string to uppercase
!!       lower          function converts string to miniscule
!!       upper_quoted   function converts string to uppercase skipping strings
!!                      quoted per Fortran rules
!!
!!   WHITE SPACE
!!       adjustc  elemental function centers text within the length of the
!!                input string
!!       compact  left justify string and replace duplicate whitespace with
!!                single characters or nothing
!!       nospace  function replaces whitespace with nothing
!!       indent   find number of leading spaces
!!       crop     function trims leading and trailing spaces
!!
!!   QUOTES
!!       unquote  remove quotes from string as if read with list-directed input
!!       quote    add quotes to string as if written with list-directed input
!!
!!   STRING LENGTH
!!       len_white  find location of last non-whitespace character
!!       lenset     return a string of specified length
!!       atleast    return a string of at least specified length
!!       stretch    return a string of at least specified length with suffix
!!       merge_str  make strings of equal length and then call MERGE(3f)
!!                  intrinsic
!!
!!   CHARACTER ARRAY VERSUS STRING
!!       switch  switch between a string and an array of single characters
!!       s2c     convert string to array of single characters and add null
!!               terminator for passing to C
!!       c2s     convert null-terminated array of single characters to
!!               string for converting strings returned from C
!!
!!   NONALPHA
!!       noesc    convert non-printable ASCII8 characters to a space
!!       notabs   convert tabs to spaces while maintaining columns,
!!                assuming tabs are set every 8 characters
!!       dilate   function to convert tabs to spaces assuming tabs are set
!!                every 8 characters
!!       expand   expand escape sequences in a string
!!       visible  expand escape sequences in a string to "control" and
!!                meta-control representations
!!
!!   NUMERIC STRINGS
!!       string_to_value   generic subroutine returns numeric value (REAL,
!!                         DOUBLEPRECISION, INTEGER) from string
!!       string_to_values  subroutine reads an array of numbers from a string
!!       getvals           subroutine reads a relatively arbitrary number
!!                         of values from a string using list-directed read
!!       s2v               function returns DOUBLEPRECISION numeric value
!!                         from string
!!       s2vs              function returns a DOUBLEPRECISION array of numbers
!!                         from a string
!!       msg               append the values of up to nine values into a string
!!
!!       value_to_string   generic subroutine returns string given numeric value
!!                         (REAL, DOUBLEPRECISION, INTEGER, LOGICAL )
!!       v2s               generic function returns string from numeric value
!!                         (REAL, DOUBLEPRECISION, INTEGER )
!!       listout           expand a list of numbers where negative numbers
!!                         denote range ends (1 -10 means 1 thru 10)
!!       isnumber          determine if string represents a number
!!
!!   CHARACTER TESTS
!!       glob        compares given string for match to pattern which may
!!                   contain wildcard characters
!!       ends_with   test whether strings ends with one of the specified suffixs
!!
!!       o isalnum   returns .true. if character is a letter or digit
!!       o isalpha   returns .true. if character is a letter and
!!                   .false. otherwise
!!       o iscntrl   returns .true. if character is a delete character or
!!                   ordinary control character
!!       o isdigit   returns .true. if character is a digit (0,1,...,9)
!!                   and .false. otherwise
!!       o isgraph   returns .true. if character is a printable character
!!                   except a space is considered non-printable
!!       o islower   returns .true. if character is a miniscule letter (a-z)
!!       o isprint   returns .true. if character is an ASCII printable
!!                   character
!!       o ispunct   returns .true. if character is a printable punctuation
!!                   character
!!       o isspace   returns .true. if character is a null, space, tab,
!!                   carriage return, new line, vertical tab, or formfeed
!!       o isupper   returns .true. if character is an uppercase letter (A-Z)
!!       o isascii   returns .true. if the character is in the range char(0)
!!                   to char(127)
!!       o isblank   returns .true. if character is a blank character
!!                   (space or horizontal tab.
!!       o isxdigit  returns .true. if character is a hexadecimal digit
!!                   (0-9, a-f, or A-F).
!!
!!       fortran_name   returns .true. if input string is a valid Fortran name
!!
!!   BASE CONVERSION
!!       base       convert whole number string in base [2-36] to string
!!                  in alternate base [2-36]
!!       base2      convert INTEGER to a string representing a binary value
!!       codebase   convert whole number string in base [2-36] to base
!!                  10 number
!!       decodebase convert whole number in base 10 to string in base [2-36]
!!
!!   MISCELLANEOUS
!!       describe   returns a string describing the name of a single character
!!
!!   INTRINSICS
!!    The M_strings(3fm) module supplements and works in combination with
!!    the Fortran built-in intrinsics. Stand-alone Fortran lets you access
!!    the characters in a string using ranges much like they are character
!!    arrays, assignment, comparisons with standard operators, supports
!!    dynamically allocatable strings and supports concatenation using the //
!!    operator, as well as a number of intrinsic string routines:
!!
!!        adjustl             Left adjust a string
!!        adjustr             Right adjust a string
!!        index               Position of a substring within a string
!!        repeat              Repeated string concatenation
!!        scan                Scan a string for the presence of a set
!!                            of characters
!!        trim                Remove trailing blank characters of a string
!!        verify              Scan a string for the absence of a set of
!!                            characters
!!        len                 It returns the length of a character string
!!        achar               converts an integer into a character
!!        iachar              converts a character into an integer
!!        len_trim            finds length of string with trailing spaces
!!                            ignored
!!        new_line            Newline character
!!        selected_char_kind  Choose character kind
!!        lge                 Lexical greater than or equal
!!        lgt                 Lexical greater than
!!        lle                 Lexical less than or equal
!!        llt                 Lexical less than
!!
!!   OOPS INTERFACE
!!    The M_strings_oop(3fm) module (included with the M_strings(3fm)
!!    module) provides an OOP (Object-Oriented Programming) interface to
!!    the M_strings(3fm) module.
!!
!!##SEE ALSO
!!    There are additional routines in other GPF modules for working with
!!    expressions (M_calculator), time strings (M_time), random strings
!!    (M_random, M_uuid), lists (M_list), and interfacing with the C regular
!!    expression library (M_regex).
!!
!!##EXAMPLES
!!
!!    Each of the procedural functions includes an example program in the
!!    corresponding man(1) page for the function. The object-oriented
!!    interface does not have individual man(1) pages, but is instead
!!    demonstrated using the following example program:
!!
!!
!!     program demo_M_strings
!!     use M_strings, only : split, delim, chomp, sep
!!     use M_strings, only : substitute, change, modif
!!     use M_strings, only : transliterate, reverse
!!     use M_strings, only : replace, join
!!     use M_strings, only : upper, lower, upper_quoted
!!     use M_strings, only : rotate13
!!     use M_strings, only : adjustc, compact, nospace, indent, crop
!!     use M_strings, only : unquote, quote
!!     use M_strings, only : len_white, atleast, stretch, lenset, merge_str
!!     use M_strings, only : switch, s2c, c2s
!!     use M_strings, only : noesc, notabs, dilate, expand, visible
!!     !!use M_strings, only : uc
!!     use M_strings, only : string_to_value, string_to_values, s2v, s2vs
!!     use M_strings, only : value_to_string, v2s, msg
!!     use M_strings, only : listout, getvals
!!     use M_strings, only : glob, ends_with
!!     use M_strings, only : fmt
!!     use M_strings, only : base, decodebase, codebase, base2
!!     use M_strings, only : isalnum, isalpha, iscntrl, isdigit, isgraph
!!     use M_strings, only : islower, isprint, ispunct, isspace, isupper
!!     use M_strings, only : isascii, isblank, isxdigit
!!     use M_strings, only : fortran_name
!!     end program demo_M_strings
!!
!!   Expected output
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
MODULE M_strings !
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT        ! access computing environment
use, intrinsic :: iso_fortran_env, only : output_unit, stderr=>error_unit
implicit none    ! change default for every procedure contained in the module

! ident_1="@(#)M_strings(3f): Fortran module containing routines that deal with character strings"

!-----------------------------------------------------------------------------------------------------------------------------------
PRIVATE

!----------------------# TOKENS
PUBLIC split           !  subroutine parses a string using specified delimiter characters and store tokens into an allocatable array
PUBLIC sep             !  function interface to split
PUBLIC chomp           !  function consumes input line as it returns next token in a string using specified delimiters
PUBLIC delim           !  subroutine parses a string using specified delimiter characters and store tokens into an array
PUBLIC strtok          !  gets next token. Used by change(3f)
PUBLIC fmt             !  convert a long string into a paragraph
!----------------------# EDITING
PUBLIC substitute      !  subroutine non-recursively globally replaces old substring with new substring in string
PUBLIC replace         !  function non-recursively globally replaces old substring with new substring in string
PUBLIC change          !  replaces old substring with new substring in string with a directive like a line editor
PUBLIC modif           !  change string using a directive using rules similar to XEDIT line editor MODIFY command
PUBLIC transliterate   !  when characters in set one are found replace them with characters from set two
PUBLIC reverse         !  elemental function reverses character order in a string
PUBLIC join            !  append an array of character variables with specified separator into a single CHARACTER variable
PUBLIC rotate13        !  apply trivial encryption algorithm ROT13 to string
!----------------------# CHARACTER ARRAY VERSUS STRING
PUBLIC switch          !  generic switch between a string and an array of single characters (a2s,s2a)
PRIVATE a2s            !  function to copy char array to string
PRIVATE s2a            !  function to copy string(1:Clen(string)) to char array
PUBLIC s2c             !  convert character variable to array of character(len=1) with null terminator for C compatibility
PUBLIC c2s             !  convert null-terminated array of character(len=1) to string for strings returned by C
!----------------------# CASE
PUBLIC upper           !  elemental function converts string to uppercase
PUBLIC lower           !  elemental function converts string to miniscule
PUBLIC upper_quoted          !  elemental function converts string to miniscule skipping strings quoted per Fortran syntax rules
!----------------------# WHITE SPACE
PUBLIC adjustc         !  elemental function centers string within the length of the input string
PUBLIC compact         !  left justify string and replace duplicate whitespace with single characters or nothing
PUBLIC nospace         !  function replaces whitespace with nothing
PUBLIC indent          !  count number of leading spaces
PUBLIC crop            !  function trims leading and trailing spaces
!----------------------# QUOTES
PUBLIC unquote         !  remove quotes from string as if read with list-directed input
PUBLIC quote           !  add quotes to string as if written with list-directed input
!----------------------# STRING LENGTH
PUBLIC lenset          !  return a string as specified length
PUBLIC atleast         !  return a string of at least specified length
PUBLIC stretch         !  return a string of at least specified length with suffix
PUBLIC merge_str       !  make strings of equal length and then call MERGE(3f) intrinsic
PUBLIC len_white       !  find location of last non-whitespace character
!----------------------# NONALPHA
PUBLIC noesc           !  elemental function converts non-printable ASCII8 characters to a space
PUBLIC notabs          !  convert tabs to spaces in output while maintaining columns, assuming a tab is set every 8 characters
PUBLIC dilate          !  convert tabs to spaces in output while maintaining columns, assuming a tab is set every 8 characters
PUBLIC expand          !  expand escape sequences in a string
PUBLIC visible         !  expand escape sequences in a string to control and meta-control representations
!----------------------# NUMERIC STRINGS
PUBLIC string_to_value !  generic subroutine returns REAL|DOUBLEPRECISION|INTEGER value from string (a2d,a2r,a2i)
 PRIVATE a2d           !  subroutine returns double value from string
 PRIVATE a2r           !  subroutine returns real value from string
 PRIVATE a2i           !  subroutine returns integer value from string
PUBLIC string_to_values!  subroutine returns values from a string
PUBLIC getvals         !  subroutine returns values from a string
PUBLIC s2v             !  function returns doubleprecision value from string
PUBLIC s2vs            !  function returns a doubleprecision array of numbers from a string
                       !------------------------------------------------------------------------------------------------------------
PUBLIC msg             !  function returns a string representing up to nine scalar intrinsic values
PUBLIC value_to_string !  generic subroutine returns string given numeric REAL|DOUBLEPRECISION|INTEGER|LOGICAL value
PUBLIC v2s             !  generic function returns string given numeric REAL|DOUBLEPRECISION|INTEGER|LOGICAL value
 PRIVATE d2s           !  function returns string from doubleprecision value
 PRIVATE r2s           !  function returns string from real value
 PRIVATE i2s           !  function returns string from integer value
 PRIVATE l2s           !  function returns string from logical value
PUBLIC v2s_bug         !  generic function returns string given numeric REAL|DOUBLEPRECISION|INTEGER value
PUBLIC isnumber        !  determine if string represents a number
 PRIVATE trimzeros_    !  Delete trailing zeros from numeric decimal string
PUBLIC listout         !  expand a list of numbers where  negative numbers denote range ends (1 -10 means 1 thru 10)
!-----------------------------------------------------------------------------------------------------------------------------------
!
! extend intrinsics to accept CHARACTER values
!
PUBLIC int, real, dble

interface int;     module procedure int_s2v;           end interface
interface real;    module procedure real_s2v;          end interface
interface dble;    module procedure dble_s2v;          end interface

interface int;     module procedure ints_s2v;          end interface
interface real;    module procedure reals_s2v;         end interface
interface dble;    module procedure dbles_s2v;         end interface

!-----------------------------------------------------------------------------------------------------------------------------------
!----------------------# BIT ROUTINES
PUBLIC setbits8        !  use a string representing a positive binary value to fill the bits of an INTEGER value
PUBLIC setbits16       !  use a string representing a positive binary value to fill the bits of an INTEGER value
PUBLIC setbits32       !  use a string representing a positive binary value to fill the bits of an INTEGER value
PUBLIC setbits64       !  use a string representing a positive binary value to fill the bits of an INTEGER value
!----------------------# BASE CONVERSION
PUBLIC base            !  convert whole number string in base [2-36] to string in alternate base [2-36]
PUBLIC codebase        !  convert whole number string in base [2-36] to base 10 number
PUBLIC decodebase      !  convert whole number in base 10 to string in base [2-36]
PUBLIC base2           !  convert INTEGER to a string representing a binary value
!----------------------# LOGICAL TESTS
PUBLIC glob            !  compares given string for match to pattern which may contain wildcard characters
PUBLIC matchw          !  clone of glob -- for backward compatibiity
PUBLIC ends_with       !  test whether strings ends with one of the specified suffix
PUBLIC isalnum         !  elemental function returns .true. if CHR is a letter or digit
PUBLIC isalpha         !  elemental function returns .true. if CHR is a letter and .false. otherwise
PUBLIC isascii         !  elemental function returns .true. if the low order byte of c is in the range char(0) to char(127)
PUBLIC isblank         !  elemental function returns .true. if CHR is a blank character (space or horizontal tab.
PUBLIC iscntrl         !  elemental function returns .true. if CHR is a delete character or ordinary control character
PUBLIC isdigit         !  elemental function returns .true. if CHR is a digit (0,1,...,9) and .false. otherwise
PUBLIC isgraph         !  elemental function true if CHR is an ASCII printable character except considers a space non-printable
PUBLIC islower         !  elemental function returns .true. if CHR is a miniscule letter (a-z)
PUBLIC isprint         !  elemental function determines if CHR is an ASCII printable character
PUBLIC ispunct         !  elemental function returns .true. if CHR is a printable punctuation character
PUBLIC isspace         !  elemental function true if CHR is a null, space, tab, carriage return, new line, vertical tab, or formfeed
PUBLIC isupper         !  elemental function returns .true. if CHR is an uppercase letter (A-Z)
PUBLIC isxdigit        !  elemental function returns .true. if CHR is a hexadecimal digit (0-9, a-f, or A-F).
!----------------------#
PUBLIC fortran_name    !  elemental function returns .true. if LINE is a valid Fortran name
!----------------------#
PUBLIC describe        !  returns a string describing character
!----------------------#

!-----------------------------------------------------------------------------------------------------------------------------------

! ident_2="@(#)M_strings::switch(3f): toggle between string and array of characters"

interface switch
   module procedure a2s, s2a
end interface switch
! note how returned result is "created" by the function
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_3="@(#)M_strings::string_to_value(3f): Generic subroutine converts numeric string to a number (a2d,a2r,a2i)"

interface string_to_value
   module procedure a2d, a2r, a2i
end interface
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_4="@(#)M_strings::v2s(3f): Generic function returns string given REAL|INTEGER|DOUBLEPRECISION value(d2s,r2s,i2s)"

interface v2s
   module procedure d2s, r2s, i2s, l2s
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
!-!interface setbits !! boz
!-!        module procedure setbits8, setbits16, setbits32, setbits64
!-!end interface
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_5="@(#)M_strings::msg(3f): convert up to nine scalar values to a string. Alternatively can also handle one-dimensional arrays"

interface msg
   module procedure msg_scalar, msg_one
end interface msg
!-----------------------------------------------------------------------------------------------------------------------------------
! ASCII character constants
character, public, parameter :: ascii_nul = char(0)   ! null
character, public, parameter :: ascii_bel = char(7)   ! bell
character, public, parameter :: ascii_bs  = char(8)   ! backspace
character, public, parameter :: ascii_ht  = char(9)   ! horizontal tab
character, public, parameter :: ascii_lf  = char(10)  ! line feed or newline
character, public, parameter :: ascii_ff  = char(12)  ! form feed or newpage
character, public, parameter :: ascii_cr  = char(13)  ! carriage return
character, public, parameter :: ascii_esc = char(27)  ! escape
!-----------------------------------------------------------------------------------------------------------------------------------
interface ends_with
    procedure :: ends_with_str
    procedure :: ends_with_any
end interface ends_with
!-----------------------------------------------------------------------------------------------------------------------------------
public :: split2020, string_tokens

interface split2020
   module procedure :: split_tokens, split_first_last, split_pos
end interface split2020
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
!This contains a conditionally built mini-version of M_journal which allows the M_strings.f90 module
!to be built using make as a stand-alone distribution but still have make.shell built a true version
!
!This is so when built with make.shell(1) or fpm(1) it will use the
!real M_journal.f90 file but that fpm(1) will not auto-discover the mini
!M_journal.f90 file and built it and cause duplicates.

interface journal
   module procedure flush_trail               ! journal()                ! no options
   module procedure write_message_only        ! journal(c)               ! must have one string
   module procedure where_write_message_all   ! journal(where,[g1-g9])   ! must have two strings
   module procedure set_stdout_lun            ! journal(i)               ! first is not a string
end interface journal

interface str
   module procedure str_scalar, str_one
end interface str

!$@(#) M_journal::journal(3fg): provides public message routine, no paging or graphic mode change

! global variables

integer,save,private       :: stdout=OUTPUT_UNIT
logical,save               :: debug=.false.
integer,save               :: last_int=0
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
! for compatibility allow old name for renamed procedures
interface matchw; module procedure glob ;  end interface
!-----------------------------------------------------------------------------------------------------------------------------------
CONTAINS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    glob(3f) - [M_strings:COMPARE] compare given string for match to
!!    a pattern which may contain globbing wildcard characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    logical function glob(string, pattern )
!!
!!     character(len=*),intent(in) :: string
!!     character(len=*),intent(in) :: pattern
!!
!!##DESCRIPTION
!!    glob(3f) compares given STRING for match to PATTERN which may
!!    contain basic wildcard "globbing" characters.
!!
!!    In this version to get a match the entire string must be described
!!    by PATTERN. Trailing whitespace is significant, so trim the input
!!    string to have trailing whitespace ignored.
!!
!!##OPTIONS
!!    string   the input string to test to see if it contains the pattern.
!!    pattern  the following simple globbing options are available
!!
!!             o "?" matching any one character
!!             o "*" matching zero or more characters.
!!               Do NOT use adjacent asterisks.
!!             o Both strings may have trailing spaces which
!!               are ignored.
!!             o There is no escape character, so matching strings with
!!               literal question mark and asterisk is problematic.
!!
!!##EXAMPLES
!!
!!   Example program
!!
!!    program demo_glob
!!    implicit none
!!    ! This main() routine passes a bunch of test strings
!!    ! into the above code.  In performance comparison mode,
!!    ! it does that over and over. Otherwise, it does it just
!!    ! once. Either way, it outputs a passed/failed result.
!!    !
!!    integer :: nReps
!!    logical :: allpassed
!!    integer :: i
!!     allpassed = .true.
!!
!!     nReps = 10000
!!     ! Can choose as many repetitions as you're expecting
!!     ! in the real world.
!!     nReps = 1
!!
!!     do i=1,nReps
!!      ! Cases with repeating character sequences.
!!      allpassed=allpassed .and. test("a*abab", "a*b", .true.)
!!      !!cycle
!!      allpassed=allpassed .and. test("ab", "*?", .true.)
!!      allpassed=allpassed .and. test("abc", "*?", .true.)
!!      allpassed=allpassed .and. test("abcccd", "*ccd", .true.)
!!      allpassed=allpassed .and. test("bLah", "bLaH", .false.)
!!      allpassed=allpassed .and. test("mississippi", "*sip*", .true.)
!!      allpassed=allpassed .and. &
!!       & test("xxxx*zzzzzzzzy*f", "xxx*zzy*f", .true.)
!!      allpassed=allpassed .and. &
!!       & test("xxxx*zzzzzzzzy*f", "xxxx*zzy*fffff", .false.)
!!      allpassed=allpassed .and. &
!!       & test("mississipissippi", "*issip*ss*", .true.)
!!      allpassed=allpassed .and. &
!!       & test("xxxxzzzzzzzzyf", "xxxx*zzy*fffff", .false.)
!!      allpassed=allpassed .and. &
!!       & test("xxxxzzzzzzzzyf", "xxxx*zzy*f", .true.)
!!      allpassed=allpassed .and. test("xyxyxyzyxyz", "xy*z*xyz", .true.)
!!      allpassed=allpassed .and. test("xyxyxyxyz", "xy*xyz", .true.)
!!      allpassed=allpassed .and. test("mississippi", "mi*sip*", .true.)
!!      allpassed=allpassed .and. test("ababac", "*abac*", .true.)
!!      allpassed=allpassed .and. test("aaazz", "a*zz*", .true.)
!!      allpassed=allpassed .and. test("a12b12", "*12*23", .false.)
!!      allpassed=allpassed .and. test("a12b12", "a12b", .false.)
!!      allpassed=allpassed .and. test("a12b12", "*12*12*", .true.)
!!
!!      ! Additional cases where the '*' char appears in the tame string.
!!      allpassed=allpassed .and. test("*", "*", .true.)
!!      allpassed=allpassed .and. test("a*r", "a*", .true.)
!!      allpassed=allpassed .and. test("a*ar", "a*aar", .false.)
!!
!!      ! More double wildcard scenarios.
!!      allpassed=allpassed .and. test("XYXYXYZYXYz", "XY*Z*XYz", .true.)
!!      allpassed=allpassed .and. test("missisSIPpi", "*SIP*", .true.)
!!      allpassed=allpassed .and. test("mississipPI", "*issip*PI", .true.)
!!      allpassed=allpassed .and. test("xyxyxyxyz", "xy*xyz", .true.)
!!      allpassed=allpassed .and. test("miSsissippi", "mi*sip*", .true.)
!!      allpassed=allpassed .and. test("miSsissippi", "mi*Sip*", .false.)
!!      allpassed=allpassed .and. test("abAbac", "*Abac*", .true.)
!!      allpassed=allpassed .and. test("aAazz", "a*zz*", .true.)
!!      allpassed=allpassed .and. test("A12b12", "*12*23", .false.)
!!      allpassed=allpassed .and. test("a12B12", "*12*12*", .true.)
!!      allpassed=allpassed .and. test("oWn", "*oWn*", .true.)
!!
!!      ! Completely tame (no wildcards) cases.
!!      allpassed=allpassed .and. test("bLah", "bLah", .true.)
!!
!!      ! Simple mixed wildcard tests suggested by IBMer Marlin Deckert.
!!      allpassed=allpassed .and. test("a", "*?", .true.)
!!
!!      ! More mixed wildcard tests including coverage for false positives.
!!      allpassed=allpassed .and. test("a", "??", .false.)
!!      allpassed=allpassed .and. test("ab", "?*?", .true.)
!!      allpassed=allpassed .and. test("ab", "*?*?*", .true.)
!!      allpassed=allpassed .and. test("abc", "?**?*?", .true.)
!!      allpassed=allpassed .and. test("abc", "?**?*&?", .false.)
!!      allpassed=allpassed .and. test("abcd", "?b*??", .true.)
!!      allpassed=allpassed .and. test("abcd", "?a*??", .false.)
!!      allpassed=allpassed .and. test("abcd", "?**?c?", .true.)
!!      allpassed=allpassed .and. test("abcd", "?**?d?", .false.)
!!      allpassed=allpassed .and. test("abcde", "?*b*?*d*?", .true.)
!!
!!      ! Single-character-match cases.
!!      allpassed=allpassed .and. test("bLah", "bL?h", .true.)
!!      allpassed=allpassed .and. test("bLaaa", "bLa?", .false.)
!!      allpassed=allpassed .and. test("bLah", "bLa?", .true.)
!!      allpassed=allpassed .and. test("bLaH", "?Lah", .false.)
!!      allpassed=allpassed .and. test("bLaH", "?LaH", .true.)
!!
!!      ! Many-wildcard scenarios.
!!      allpassed=allpassed .and. test(&
!!      &"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!!      &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",&
!!      &"a*a*a*a*a*a*aa*aaa*a*a*b",&
!!      &.true.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacac&
!!      &adaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*a*aa*aaa*fa*ga*b*",&
!!      &.true.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacaca&
!!      &cadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*a*x*aaa*fa*ga*b*",&
!!      &.false.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacacad&
!!      &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*aaaa*fa*ga*gggg*b*",&
!!      &.false.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacacad&
!!      &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*aaaa*fa*ga*ggg*b*",&
!!      &.true.)
!!      allpassed=allpassed .and. test("aaabbaabbaab", "*aabbaa*a*", .true.)
!!      allpassed=allpassed .and. &
!!      test("a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*",&
!!      &"a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.)
!!      allpassed=allpassed .and. test("aaaaaaaaaaaaaaaaa",&
!!      &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.)
!!      allpassed=allpassed .and. test("aaaaaaaaaaaaaaaa",&
!!      &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .false.)
!!      allpassed=allpassed .and. test(&
!!      &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
!!      &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
!!      & "abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc&
!!      &*abc*abc*abc*",&
!!      &.false.)
!!      allpassed=allpassed .and. test(&
!!      &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
!!      &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
!!      &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*",&
!!      &.true.)
!!      allpassed=allpassed .and. test("abc*abcd*abcd*abc*abcd",&
!!      &"abc*abc*abc*abc*abc", .false.)
!!      allpassed=allpassed .and. test( "abc*abcd*abcd*abc*abcd*abcd&
!!      &*abc*abcd*abc*abc*abcd", &
!!      &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abcd",&
!!      &.true.)
!!      allpassed=allpassed .and. test("abc",&
!!      &"********a********b********c********", .true.)
!!      allpassed=allpassed .and.&
!!      &test("********a********b********c********", "abc", .false.)
!!      allpassed=allpassed .and. &
!!      &test("abc", "********a********b********b********", .false.)
!!      allpassed=allpassed .and. test("*abc*", "***a*b*c***", .true.)
!!
!!      ! A case-insensitive algorithm test.
!!      ! allpassed=allpassed .and. test("mississippi", "*issip*PI", .true.)
!!     enddo
!!
!!     if (allpassed)then
!!        write(*,'(a)')"Passed",nReps
!!     else
!!        write(*,'(a)')"Failed"
!!     endif
!!    contains
!!    ! This is a test program for wildcard matching routines.
!!    ! It can be used either to test a single routine for correctness,
!!    ! or to compare the timings of two (or more) different wildcard
!!    ! matching routines.
!!    !
!!    function test(tame, wild, bExpectedResult) result(bpassed)
!!    use M_strings, only : glob
!!       character(len=*) :: tame
!!       character(len=*) :: wild
!!       logical          :: bExpectedResult
!!       logical          :: bResult
!!       logical          :: bPassed
!!       bResult = .true.    ! We'll do "&=" cumulative checking.
!!       bPassed = .false.   ! Assume the worst.
!!       write(*,*)repeat('=',79)
!!       bResult = glob(tame, wild) ! Call a wildcard matching routine.
!!
!!       ! To assist correctness checking, output the two strings in any
!!       ! failing scenarios.
!!       if (bExpectedResult .eqv. bResult) then
!!          bPassed = .true.
!!          if(nReps == 1) write(*,*)"Passed match on ",tame," vs. ", wild
!!       else
!!          if(nReps == 1) write(*,*)"Failed match on ",tame," vs. ", wild
!!       endif
!!
!!    end function test
!!    end program demo_glob
!!
!!   Expected output
!!
!!##AUTHOR
!!   John S. Urban
!!
!!##REFERENCE
!!   The article "Matching Wildcards: An Empirical Way to Tame an Algorithm"
!!   in Dr Dobb's Journal, By Kirk J. Krauss, October 07, 2014
!!
!!##LICENSE
!!   Public Domain
function glob(tame,wild)

! ident_6="@(#)M_strings::glob(3f): function compares text strings, one of which can have wildcards ('*' or '?')."

logical                    :: glob
character(len=*)           :: tame       ! A string without wildcards
character(len=*)           :: wild       ! A (potentially) corresponding string with wildcards
character(len=len(tame)+1) :: tametext
character(len=len(wild)+1) :: wildtext
character(len=1),parameter :: NULL=char(0)
integer                    :: wlen
integer                    :: ti, wi
integer                    :: i
character(len=:),allocatable :: tbookmark, wbookmark
! These two values are set when we observe a wildcard character. They
! represent the locations, in the two strings, from which we start once we've observed it.
   tametext=tame//NULL
   wildtext=wild//NULL
   tbookmark = NULL
   wbookmark = NULL
   wlen=len(wild)
   wi=1
   ti=1
   do                                            ! Walk the text strings one character at a time.
      if(wildtext(wi:wi) == '*')then             ! How do you match a unique text string?
         do i=wi,wlen                            ! Easy: unique up on it!
            if(wildtext(wi:wi).eq.'*')then
               wi=wi+1
            else
               exit
            endif
         enddo
         if(wildtext(wi:wi).eq.NULL) then        ! "x" matches "*"
            glob=.true.
            return
         endif
         if(wildtext(wi:wi) .ne. '?') then
            ! Fast-forward to next possible match.
            do while (tametext(ti:ti) .ne. wildtext(wi:wi))
               ti=ti+1
               if (tametext(ti:ti).eq.NULL)then
                  glob=.false.
                  return                         ! "x" doesn't match "*y*"
               endif
            enddo
         endif
         wbookmark = wildtext(wi:)
         tbookmark = tametext(ti:)
      elseif(tametext(ti:ti) .ne. wildtext(wi:wi) .and. wildtext(wi:wi) .ne. '?') then
         ! Got a non-match. If we've set our bookmarks, back up to one or both of them and retry.
         if(wbookmark.ne.NULL) then
            if(wildtext(wi:).ne. wbookmark) then
               wildtext = wbookmark;
               wlen=len_trim(wbookmark)
               wi=1
               ! Don't go this far back again.
               if (tametext(ti:ti) .ne. wildtext(wi:wi)) then
                  tbookmark=tbookmark(2:)
                  tametext = tbookmark
                  ti=1
                  cycle                          ! "xy" matches "*y"
               else
                  wi=wi+1
               endif
            endif
            if (tametext(ti:ti).ne.NULL) then
               ti=ti+1
               cycle                             ! "mississippi" matches "*sip*"
            endif
         endif
         glob=.false.
         return                                  ! "xy" doesn't match "x"
      endif
      ti=ti+1
      wi=wi+1
      if (tametext(ti:ti).eq.NULL) then          ! How do you match a tame text string?
         if(wildtext(wi:wi).ne.NULL)then
            do while (wildtext(wi:wi) == '*')    ! The tame way: unique up on it!
               wi=wi+1                           ! "x" matches "x*"
               if(wildtext(wi:wi).eq.NULL)exit
            enddo
         endif
         if (wildtext(wi:wi).eq.NULL)then
            glob=.true.
            return                               ! "x" matches "x"
         endif
         glob=.false.
         return                                  ! "x" doesn't match "xy"
      endif
   enddo
end function glob
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    ends_with(3f) - [M_strings:MATCH] test if string ends with specified suffix(es)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function ends_with(source_string,suffix)
!!
!!     or
!!
!!    function ends_with(source_string,[suffixs])
!!
!!     character(len=*),intent(in)          :: source_string
!!     character(len=*),intent(in)          :: suffix
!!     logical                              :: ends_with
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!     SOURCE_STRING  string to tokenize
!!     SUFFIX         list of separator characters. May be scalar or an array.
!!
!!##RETURNS
!!     ENDS_WITH      returns .TRUE. if one of the suffix match the end
!!                    of SOURCE_STRING.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_ends_with
!!    use M_strings, only : ends_with
!!    implicit none
!!       write(*,*)ends_with('prog.a',['.o','.i','.s'])
!!       write(*,*)ends_with('prog.f90',['.F90','.f90'])
!!       write(*,*)ends_with('prog.pdf','.pdf')
!!       write(*,*)ends_with('prog.doc','.txt')
!!    end program demo_ends_with
!!
!!   Results:
!!
!!     F
!!     T
!!     T
!!     F
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function ends_with_str(string, ending) result(matched)
character(*), intent(in) :: string, ending
integer                  :: n1, n2
logical                  :: matched
   n1 = len(string) - len(ending) + 1
   n2 = len(string)
   if (n1 < 1) then
       matched = .false.
   else
       matched = (string(n1:n2) == ending)
   endif
end function ends_with_str
!-----------------------------------------------------------------------------------------------------------------------------------
pure function ends_with_any(string, endings) result(matched)
character(*), intent(in) :: string
character(*), intent(in) :: endings(:)
logical                  :: matched
integer                  :: i
   matched = .true.
   FINDIT: block
   do i=1, size(endings)
       if(ends_with_str(string,trim(endings(i)))) exit FINDIT
   enddo
   matched = .false.
   endblock FINDIT
end function ends_with_any
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sep(3f) - [M_strings:TOKENS] function to parse string into an array using
!!    specified delimiters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function sep(input_line,delimiters,nulls)
!!
!!     character(len=*),intent(in)              :: input_line
!!     character(len=*),optional,intent(in)     :: delimiters
!!     character(len=*),optional,intent(in)     :: nulls
!!     character(len=:),allocatable             :: sep(:)
!!
!!##DESCRIPTION
!!     sep(3f) parses a string using specified delimiter characters and
!!     store tokens into an allocatable array
!!
!!##OPTIONS
!!    INPUT_LINE  Input string to tokenize
!!
!!    DELIMITERS  List of delimiter characters.
!!                The default delimiters are the "whitespace" characters
!!                (space, tab,new line, vertical tab, formfeed, carriage
!!                return, and null). You may specify an alternate set of
!!                delimiter characters.
!!
!!                Multi-character delimiters are not supported (Each
!!                character in the DELIMITERS list is considered to be
!!                a delimiter).
!!
!!                Quoting of delimiter characters is not supported.
!!
!!    NULLS=IGNORE|RETURN|IGNOREEND  Treatment of null fields.
!!                By default adjacent delimiters in the input string
!!                do not create an empty string in the output array. if
!!                NULLS='return' adjacent delimiters create an empty element
!!                in the output ARRAY. If NULLS='ignoreend' then only
!!                trailing delimiters at the right of the string are ignored.
!!##RETURNS
!!    SEP       Output array of tokens
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_sep
!!    use M_strings, only: sep
!!    character(len=*),parameter :: fo='(/,a,*(/,"[",g0,"]":,","))'
!!    character(len=*),parameter :: line=&
!!    '  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
!!       write(*,'(a)') 'INPUT LINE:['//LINE//']'
!!       write(*,fo) 'typical call:',sep(line)
!!       write(*,fo) 'delimiters ":|":',sep(line,':|')
!!       write(*,fo) 'count null fields ":|":',sep(line,':|','return')
!!    end program demo_sep
!!
!!  Output
!!
!!    INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!
!!    typical call:
!!    [cc        ],
!!    [B         ],
!!    [a         ],
!!    [333|333   ],
!!    [1:|:2     ],
!!    [qrstuvwxyz],
!!    [ghijklmnop],
!!    [aBcdef    ]
!!
!!    delimiters ":|":
!!    [333 a B cc                         ],
!!    [2     333                          ],
!!    [  aBcdef   ghijklmnop qrstuvwxyz  1]
!!
!!    count null fields ":|":
!!    [333 a B cc                         ],
!!    [2     333                          ],
!!    [                                   ],
!!    [                                   ],
!!    [  aBcdef   ghijklmnop qrstuvwxyz  1]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function sep(input_line,delimiters,nulls)
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_7="@(#)M_strings::sep(3f): parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=:),allocatable             :: sep(:)      ! output array of tokens
   call split(input_line,sep,delimiters,'right',nulls)
!-----------------------------------------------------------------------------------------------------------------------------------
end function sep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    split(3f) - [M_strings:TOKENS] parse string into an array using
!!    specified delimiters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine split(input_line,array,delimiters,order,nulls)
!!
!!     character(len=*),intent(in)              :: input_line
!!     character(len=:),allocatable,intent(out) :: array(:)
!!     character(len=*),optional,intent(in)     :: delimiters
!!     character(len=*),optional,intent(in)     :: order
!!     character(len=*),optional,intent(in)     :: nulls
!!
!!##DESCRIPTION
!!     SPLIT(3f) parses a string using specified delimiter characters and
!!     store tokens into an allocatable array
!!
!!##OPTIONS
!!    INPUT_LINE  Input string to tokenize
!!
!!    ARRAY       Output array of tokens
!!
!!    DELIMITERS  List of delimiter characters.
!!                The default delimiters are the "whitespace" characters
!!                (space, tab,new line, vertical tab, formfeed, carriage
!!                return, and null). You may specify an alternate set of
!!                delimiter characters.
!!
!!                Multi-character delimiters are not supported (Each
!!                character in the DELIMITERS list is considered to be
!!                a delimiter).
!!
!!                Quoting of delimiter characters is not supported.
!!
!!    ORDER SEQUENTIAL|REVERSE|RIGHT  Order of output array.
!!                By default ARRAY contains the tokens having parsed
!!                the INPUT_LINE from left to right. If ORDER='RIGHT'
!!                or ORDER='REVERSE' the parsing goes from right to left.
!!                (This can be accomplished with array syntax in modern
!!                Fortran, but was more useful pre-fortran90).
!!
!!    NULLS=IGNORE|RETURN|IGNOREEND  Treatment of null fields.
!!                By default adjacent delimiters in the input string
!!                do not create an empty string in the output array. if
!!                NULLS='return' adjacent delimiters create an empty element
!!                in the output ARRAY. If NULLS='ignoreend' then only
!!                trailing delimiters at the right of the string are ignored.
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_split
!!    use M_strings, only: split
!!    implicit none
!!    integer :: i
!!    character(len=*),parameter     :: line=&
!!    '  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
!!    character(len=:),allocatable :: array(:) ! output array of tokens
!!       write(*,*)'INPUT LINE:['//LINE//']'
!!       write(*,'(70("="))')
!!       write(*,*)'typical call:'
!!       CALL split(line,array)
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',SIZE(array)
!!       write(*,'(70("-"))')
!!       write(*,*)'custom list of delimiters (colon and vertical line):'
!!       CALL split(line,array,delimiters=':|',&
!!       & order='sequential',nulls='ignore')
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',SIZE(array)
!!       write(*,'(70("-"))')
!!       write(*,*) 'custom list of delimiters, &
!!       &reverse array order and count null fields:'
!!       CALL split(line,array,delimiters=':|',&
!!       &order='reverse',nulls='return')
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',SIZE(array)
!!       write(*,'(70("-"))')
!!       write(*,*)'INPUT LINE:['//LINE//']'
!!       write(*,*) 'default delimiters and reverse array order &
!!       &and return null fields:'
!!       CALL split(line,array,delimiters='',order='reverse',nulls='return')
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',SIZE(array)
!!    end program demo_split
!!
!!  Output
!!
!!   >INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|
!!   333 a B cc    ]
!!   >=================================================================
!!   > typical call:
!!   >1 ==> aBcdef
!!   >2 ==> ghijklmnop
!!   >3 ==> qrstuvwxyz
!!   >4 ==> 1:|:2
!!   >5 ==> 333|333
!!   >6 ==> a
!!   >7 ==> B
!!   >8 ==> cc
!!   > SIZE:           8
!!   >----------------------------------------------------------------
!!   > custom list of delimiters (colon and vertical line):
!!   >1 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!   >2 ==> 2     333
!!   >3 ==> 333 a B cc
!!   > SIZE:           3
!!   >----------------------------------------------------------------
!!   > custom list of delimiters, reverse array order and
!!   return null fields:
!!   >1 ==> 333 a B cc
!!   >2 ==> 2     333
!!   >3 ==>
!!   >4 ==>
!!   >5 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!   > SIZE:           5
!!   >----------------------------------------------------------------
!!   > INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|
!!   333 a B cc    ]
!!   > default delimiters and reverse array order and count null fields:
!!   >1 ==>
!!   >2 ==>
!!   >3 ==>
!!   >4 ==> cc
!!   >5 ==> B
!!   >6 ==> a
!!   >7 ==> 333|333
!!   >8 ==>
!!   >9 ==>
!!   >10 ==>
!!   >11 ==>
!!   >12 ==> 1:|:2
!!   >13 ==>
!!   >14 ==> qrstuvwxyz
!!   >15 ==> ghijklmnop
!!   >16 ==>
!!   >17 ==>
!!   >18 ==> aBcdef
!!   >19 ==>
!!   >20 ==>
!!   > SIZE:          20
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine split(input_line,array,delimiters,order,nulls)
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_8="@(#)M_strings::split(3f): parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
character(len=:),allocatable  :: ordr                   ! string containing order keyword
character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
integer                       :: ii,iiii                ! loop parameters used to control print order
integer                       :: icount                 ! number of tokens found
integer                       :: ilen                   ! length of input string with trailing spaces trimmed
integer                       :: i10,i20,i30            ! loop counters
integer                       :: icol                   ! pointer into input string as it is being parsed
integer                       :: idlim                  ! number of delimiter characters
integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
integer                       :: inotnull               ! count strings not composed of delimiters
integer                       :: ireturn                ! number of tokens returned
integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters.ne.'')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
   if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   if(allocated(ibegin))deallocate(ibegin)    !*! intel compiler says allocated already ?
   if(allocated(iterm))deallocate(iterm)      !*! intel compiler says allocated already ?
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len(input_line)                                           ! ILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ilen.gt.0)then                                              ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,ilen,1                                   ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)).eq.0)then  ! if current character is not a delimiter
            iterm(i30)=ilen                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):ilen),dlim(i10:i10))
               IF(ifound.gt.0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol.gt.ilen)then                                     ! no text left
            exit INFINITE
         endif
      enddo INFINITE
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(nlls)))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
   !allocate(array(ireturn))                                       ! allocate the array to return
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
   case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
   case default             ; ii=1       ; iiii=1                 ! first to last
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20).lt.ibegin(i20))then
         select case (trim(adjustl(nlls)))
         case ('ignore','','ignoreend')
         case default
            array(ii)=' '
            ii=ii+iiii
         end select
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+iiii
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   end subroutine split
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    chomp(3f) - [M_strings:TOKENS] Tokenize a string, consuming it one
!!    token per call
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function chomp(source_string,token[,delimiters])
!!
!!     character(len=*)                     :: source_string
!!     character(len=:),intent(out)         :: token
!!     character(len=:),intent(in),optional :: delimiters
!!     integer                              :: chomp
!!
!!##DESCRIPTION
!!    The CHOMP(3f) function is used to isolate sequential tokens in a
!!    string, SOURCE_STRING. These tokens are delimited in the string by at
!!    least one of the characters in DELIMITERS. This routine consumes the
!!    source_string one token per call. It returns -1 when complete. The
!!    default delimiter list is "space,tab,carriage return,newline".
!!
!!##OPTIONS
!!     SOURCE_STRING  string to tokenize
!!     DELIMITERS     list of separator characters
!!
!!##RETURNS
!!     TOKEN          returned token
!!     CHOMP          status flag. 0 = success, -1 = no tokens remain
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_chomp
!!
!!    use M_strings, only : chomp
!!    implicit none
!!    character(len=100)            :: inline
!!    character(len=:),allocatable  :: token
!!    character(len=*),parameter    :: delimiters=' ;,'
!!    integer                       :: ios
!!    integer                       :: icount
!!    integer                       :: itoken
!!       icount=0
!!       do        ! read lines from stdin until end-of-file or error
!!          read (unit=*,fmt="(a)",iostat=ios) inline
!!          if(ios.ne.0)stop
!!          icount=icount+1
!!          itoken=0
!!          write(*,*)'INLINE ',trim(inline)
!!          do while ( chomp(inline,token,delimiters).ge. 0)
!!             itoken=itoken+1
!!             print *, itoken,'TOKEN=['//trim(token)//']'
!!          enddo
!!       enddo
!!
!!    end program demo_chomp
!!
!!   sample input file
!!
!!     this is a test of chomp; A:B :;,C;;
!!
!!   sample output file
!!
!!     > INLINE     this is a test of chomp; A:B :;,C;;
!!     >           1 TOKEN=[this]
!!     >           2 TOKEN=[is]
!!     >           3 TOKEN=[a]
!!     >           4 TOKEN=[test]
!!     >           5 TOKEN=[of]
!!     >           6 TOKEN=[chomp]
!!     >           7 TOKEN=[A:B]
!!     >           8 TOKEN=[:]
!!     >           9 TOKEN=[C]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
FUNCTION chomp(source_string,token,delimiters)

! ident_9="@(#)M_strings::chomp(3f): Tokenize a string : JSU- 20151030"

character(len=*)                         :: source_string    ! string to tokenize
character(len=:),allocatable,intent(out) :: token            ! returned token
character(len=*),intent(in),optional     :: delimiters       ! list of separator characters
integer                                  :: chomp            ! returns copy of shifted source_string
character(len=:),allocatable             :: delimiters_local
integer                                  :: token_start      ! beginning of token found if function result is .true.
integer                                  :: token_end        ! end of token found if function result is .true.
integer                                  :: isource_len
!-----------------------------------------------------------------------------------------------------------------------------------
!  calculate where token_start should start for this pass
   if(present(delimiters))then
      delimiters_local=delimiters
   else                                          ! increment start to previous end + 1
      delimiters_local=char(32)//char(09)//char(10)//char(13) ! space,horizontal tab, newline, carriage return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   isource_len=len(source_string)                ! length of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   ! find beginning of token
   token_start=1
   do while (token_start .le. isource_len)       ! step thru each character to find next delimiter, if any
      if(index(delimiters_local,source_string(token_start:token_start)) .ne. 0) then
         token_start = token_start + 1
      else
         exit
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   token_end=token_start
   do while (token_end .le. isource_len-1)                         ! step thru each character to find next delimiter, if any
      if(index(delimiters_local,source_string(token_end+1:token_end+1)) .ne. 0) then  ! found a delimiter in next character
         exit
      endif
      token_end = token_end + 1
   enddo
   !write(*,*)'TOKEN_START ',token_start
   !write(*,*)'TOKEN_END   ',token_end
   chomp=isource_len-token_end
   if(chomp.ge.0)then
      token=source_string(token_start:token_end)
      source_string=source_string(token_end+1:)
   else
      token=''
      source_string=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function chomp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      delim(3f) - [M_strings:TOKENS] parse a string and store tokens into
!!      an array
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine delim(line,array,n,icount,ibegin,iterm,ilen,dlim)
!!
!!     character(len=*),intent(in)  :: line
!!     integer,integer(in)          :: n
!!     integer,intent(out)          :: icount
!!     character(len=*)             :: array(n)
!!     integer,intent(out)          :: ibegin(n)
!!     integer,intent(out)          :: iterm(n)
!!     integer,intent(out)          :: ilen
!!     character(len=*)             :: dlim
!!
!!##DESCRIPTION
!!      Given a LINE of structure " par1 par2 par3 ... parn "
!!      store each par(n) into a separate variable in ARRAY (UNLESS
!!      ARRAY(1).eq.'#N#')
!!
!!      Also set ICOUNT to number of elements of array initialized, and
!!      return beginning and ending positions for each element in IBEGIN(N)
!!      and ITERM(N).
!!
!!      Return position of last non-blank character (even if more
!!      than N elements were found) in ILEN
!!
!!      No quoting or escaping of delimiter is allowed, so the delimiter
!!      character can not be placed in a token.
!!
!!      No checking for more than N parameters; If any more they are ignored.
!!
!!##OPTIONS
!!    LINE      input string to parse into tokens
!!    ARRAY(N)  array that receives tokens
!!    N         size of arrays ARRAY, IBEGIN, ITERM
!!    ICOUNT    number of tokens found
!!    IBEGIN(N) starting columns of tokens found
!!    ITERM(N)  ending columns of tokens found
!!    ILEN      position of last non-blank character in input string LINE
!!    DLIM      delimiter characters
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!     program demo_delim
!!
!!     use M_strings, only: delim
!!     implicit none
!!     character(len=80) :: line
!!     character(len=80) :: dlm
!!     integer,parameter :: n=10
!!     character(len=20) :: array(n)=' '
!!     integer           :: ibegin(n),iterm(n)
!!     integer           :: i20, icount, ilen, i10
!!     line=' first  second 10.3 words_of_stuff  '
!!     do i20=1,4
!!        ! change delimiter list and what is calculated or parsed
!!        if(i20.eq.1)dlm=' '
!!        if(i20.eq.2)dlm='o'
!!        if(i20.eq.3)dlm=' aeiou'    ! NOTE SPACE IS FIRST
!!        if(i20.eq.3)ARRAY(1)='#N#'  ! QUIT RETURNING STRING ARRAY
!!        if(i20.eq.4)line='AAAaBBBBBBbIIIIIi  J K L'
!!
!!        ! write out a break line composed of =========== ..
!!        write(*,'(57("="))')
!!        ! show line being parsed
!!        write(*,'(a)')'PARSING=['//trim(line)//'] on '//trim(dlm)
!!        ! call parsing procedure
!!        call delim(line,array,n,icount,ibegin,iterm,ilen,dlm)
!!        write(*,*)'number of tokens found=',icount
!!        write(*,*)'last character in column ',ilen
!!        if(icount.gt.0)then
!!           if(ilen.ne.iterm(icount))then
!!              write(*,*)'ignored from column ',iterm(icount)+1,' to ',ilen
!!           endif
!!           do i10=1,icount
!!              ! check flag to see if ARRAY() was set
!!              if(array(1).ne.'#N#')then
!!                 ! from returned array
!!                 write(*,'(a,a,a)',advance='no')&
!!                 &'[',array(i10)(:iterm(i10)-ibegin(i10)+1),']'
!!              endif
!!           enddo
!!           ! using start and end positions in IBEGIN() and ITERM()
!!           write(*,*)
!!           do i10=1,icount
!!              ! from positions in original line
!!              write(*,'(a,a,a)',advance='no')&
!!              &'[',line(ibegin(i10):iterm(i10)),']'
!!           enddo
!!           write(*,*)
!!        endif
!!     enddo
!!     end program demo_delim
!!
!!  Expected output
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine delim(line,array,n,icount,ibegin,iterm,ilen,dlim)

! ident_10="@(#)M_strings::delim(3f): parse a string and store tokens into an array"

!
!     given a line of structure " par1 par2 par3 ... parn "
!     store each par(n) into a separate variable in array.
!
!     IF ARRAY(1) == '#N#' do not store into string array  (KLUDGE))
!
!     also count number of elements of array initialized, and
!     return beginning and ending positions for each element.
!     also return position of last non-blank character (even if more
!     than n elements were found).
!
!     no quoting of delimiter is allowed
!     no checking for more than n parameters, if any more they are ignored
!
character(len=*),intent(in)    :: line
integer,intent(in)             :: n
character(len=*)               :: array(n)
integer,intent(out)            :: icount
integer,intent(out)            :: ibegin(n)
integer,intent(out)            :: iterm(n)
integer,intent(out)            :: ilen
character(len=*),intent(in)    :: dlim
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(line)):: line_local
logical             :: lstore
integer             :: i10
integer             :: iarray
integer             :: icol
integer             :: idlim
integer             :: iend
integer             :: ifound
integer             :: istart
!-----------------------------------------------------------------------------------------------------------------------------------
      icount=0
      ilen=len_trim(line)
      line_local=line

      idlim=len(dlim)
      if(idlim > 5)then
         idlim=len_trim(dlim)      ! dlim a lot of blanks on some machines if dlim is a big string
         if(idlim == 0)then
            idlim=1     ! blank string
         endif
      endif

      if(ilen == 0)then                                        ! command was totally blank
         return
      endif
!
!     there is at least one non-blank character in the command
!     ilen is the column position of the last non-blank character
!     find next non-delimiter
      icol=1

      if(array(1) == '#N#')then                                ! special flag to not store into character array
         lstore=.false.
      else
         lstore=.true.
      endif

      do iarray=1,n,1                                          ! store into each array element until done or too many words
         NOINCREMENT: do
            if(index(dlim(1:idlim),line_local(icol:icol)) == 0)then  ! if current character is not a delimiter
               istart=icol                                     ! start new token on the non-delimiter character
               ibegin(iarray)=icol
               iend=ilen-istart+1+1                            ! assume no delimiters so put past end of line
               do i10=1,idlim
                  ifound=index(line_local(istart:ilen),dlim(i10:i10))
                  if(ifound > 0)then
                     iend=min(iend,ifound)
                  endif
               enddo
               if(iend <= 0)then                               ! no remaining delimiters
                 iterm(iarray)=ilen
                 if(lstore)then
                    array(iarray)=line_local(istart:ilen)
                 endif
                 icount=iarray
                 return
               else
                 iend=iend+istart-2
                 iterm(iarray)=iend
                 if(lstore)then
                    array(iarray)=line_local(istart:iend)
                 endif
               endif
               icol=iend+2
               exit NOINCREMENT
            endif
            icol=icol+1
         enddo NOINCREMENT
!        last character in line was a delimiter, so no text left
!        (should not happen where blank=delimiter)
         if(icol > ilen)then
           icount=iarray
           if( (iterm(icount)-ibegin(icount)) < 0)then         ! last token was all delimiters
              icount=icount-1
           endif
           return
         endif
      enddo
      icount=n  ! more than n elements
end subroutine delim
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    replace(3f) - [M_strings:EDITING] function replaces one
!!    substring for another in string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!! syntax:
!!
!!      function replace(targetline,old,new,cmd,&
!!       & occurrence, &
!!       & repeat, &
!!       & ignorecase, &
!!       & ierr) result (newline)
!!      character(len=*)                       :: targetline
!!      character(len=*),intent(in),optional   :: old
!!      character(len=*),intent(in),optional   :: new
!!      character(len=*),intent(in),optional   :: cmd
!!      integer,intent(in),optional            :: occurrence
!!      integer,intent(in),optional            :: repeat
!!      logical,intent(in),optional            :: ignorecase
!!      integer,intent(out),optional           :: ierr
!!      character(len=:),allocatable           :: newline
!!
!!##DESCRIPTION
!!    Replace one substring for another in string.
!!    Either CMD or OLD and NEW must be specified.
!!
!!##OPTIONS
!!     targetline  input line to be changed
!!     old         old substring to replace
!!     new         new substring
!!     cmd         alternate way to specify old and new string, in
!!                 the form c/old/new/; where "/" can be any character
!!                 not in "old" or "new"
!!     occurrence  if present, start changing at the Nth occurrence of the
!!                 OLD string. If negative start replacing from the left
!!                 end of the string.
!!     repeat      number of replacements to perform. Defaults to a global
!!                 replacement.
!!     ignorecase  whether to ignore ASCII case or not. Defaults
!!                 to .false.
!!##RETURNS
!!     newline     allocatable string returned
!!     ierr        error code. iF ier = -1 bad directive, >= 0 then
!!                 count of changes made
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_replace
!!    use M_strings, only : replace
!!    implicit none
!!    character(len=:),allocatable :: line
!!
!!    write(*,*)replace('Xis is Xe string','X','th')
!!    write(*,*)replace('Xis is xe string','x','th',ignorecase=.true.)
!!    write(*,*)replace('Xis is xe string','X','th',ignorecase=.false.)
!!
!!    ! a null old substring means "at beginning of line"
!!    write(*,*) replace('my line of text','','BEFORE:')
!!
!!    ! a null new string deletes occurrences of the old substring
!!    write(*,*) replace('I wonder i ii iii','i','')
!!
!!    ! Examples of the use of RANGE
!!
!!    line=replace('aaaaaaaaa','a','A',occurrence=1,repeat=1)
!!    write(*,*)'replace first a with A ['//line//']'
!!
!!    line=replace('aaaaaaaaa','a','A',occurrence=3,repeat=3)
!!    write(*,*)'replace a with A for 3rd to 5th occurrence ['//line//']'
!!
!!    line=replace('ababababa','a','',occurrence=3,repeat=3)
!!    write(*,*)'replace a with null instances 3 to 5 ['//line//']'
!!
!!    line=replace( &
!!     & 'a b ab baaa aaaa aa aa a a a aa aaaaaa',&
!!     & 'aa','CCCC',occurrence=-1,repeat=1)
!!    write(*,*)'replace lastaa with CCCC ['//line//']'
!!
!!    write(*,*)replace('myf90stuff.f90.f90','f90','for',occurrence=-1,repeat=1)
!!    write(*,*)replace('myf90stuff.f90.f90','f90','for',occurrence=-2,repeat=2)
!!
!!    end program demo_replace
!!
!!   Results:
!!
!!     this is the string
!!     this is the string
!!     this is xe string
!!     BEFORE:my line of text
!!     I wonder
!!     replace first a with A [Aaaaaaaaa]
!!     replace a with A for 3rd to 5th occurrence [aaAAAaaaa]
!!     replace a with null instances 3 to 5 [ababbb]
!!     replace lastaa with CCCC [a b ab baaa aaaa aa aa a a a aa aaaaCCCC]
!!     myf90stuff.f90.for
!!     myforstuff.for.f90
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine crack_cmd(cmd,old,new,ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)              :: cmd
character(len=:),allocatable,intent(out) :: old,new                ! scratch string buffers
integer                                  :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=1)                         :: delimiters
integer                                  :: itoken
integer,parameter                        :: id=2                   ! expected location of delimiter
logical                                  :: ifok
integer                                  :: lmax                   ! length of target string
integer                                  :: start_token,end_token
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   old=''
   new=''
   lmax=len_trim(cmd)                       ! significant length of change directive

   if(lmax.ge.4)then                      ! strtok ignores blank tokens so look for special case where first token is really null
      delimiters=cmd(id:id)               ! find delimiter in expected location
      itoken=0                            ! initialize strtok(3f) procedure

      if(strtok(cmd(id:),itoken,start_token,end_token,delimiters)) then        ! find OLD string
         old=cmd(start_token+id-1:end_token+id-1)
      else
         old=''
      endif

      if(cmd(id:id).eq.cmd(id+1:id+1))then
         new=old
         old=''
      else                                                                     ! normal case
         ifok=strtok(cmd(id:),itoken,start_token,end_token,delimiters)         ! find NEW string
         if(end_token .eq. (len(cmd)-id+1) )end_token=len_trim(cmd(id:))       ! if missing ending delimiter
         new=cmd(start_token+id-1:min(end_token+id-1,lmax))
      endif
   else                                                                        ! command was two or less characters
      ierr=-1
      call journal('sc','*crack_cmd* incorrect change directive -too short')
   endif

end subroutine crack_cmd
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function replace(targetline,old,new,cmd,occurrence,repeat,ignorecase,ierr) result (newline)

! ident_11="@(#)M_strings::replace(3f): replace one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
! parameters
character(len=*),intent(in)            :: targetline   ! input line to be changed
character(len=*),intent(in),optional   :: old          ! old substring to replace
character(len=*),intent(in),optional   :: new          ! new substring
character(len=*),intent(in),optional   :: cmd          ! contains the instructions changing the string
integer,intent(in),optional            :: occurrence   ! Nth occurrence of OLD string to start replacement at
integer,intent(in),optional            :: repeat       ! how many replacements
logical,intent(in),optional            :: ignorecase
integer,intent(out),optional           :: ierr         ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
!-----------------------------------------------------------------------------------------------------------------------------------
! returns
character(len=:),allocatable  :: newline               ! output string buffer
!-----------------------------------------------------------------------------------------------------------------------------------
! local
character(len=:),allocatable  :: new_local, old_local, old_local_for_comparison
integer                       :: icount,ichange,ier2
integer                       :: original_input_length
integer                       :: len_old, len_new
integer                       :: ladd
integer                       :: left_margin, right_margin
integer                       :: ind
integer                       :: ic
integer                       :: ichar
integer                       :: range_local(2)
character(len=:),allocatable  :: targetline_for_comparison   ! input line to be changed
logical                       :: ignorecase_local
logical                       :: flip
character(len=:),allocatable  :: targetline_local   ! input line to be changed
!-----------------------------------------------------------------------------------------------------------------------------------
   flip=.false.
   ignorecase_local=.false.
   original_input_length=len_trim(targetline)          ! get non-blank length of input line

!  get old_local and new_local from cmd or old and new
   if(present(cmd))then
      call crack_cmd(cmd,old_local,new_local,ier2)
      if(ier2.ne.0)then
         newline=targetline  ! if no changes are made return original string on error
         if(present(ierr))ierr=ier2
         return
      endif
   elseif(present(old).and.present(new))then
      old_local=old
      new_local=new
   else
      newline=targetline  ! if no changes are made return original string on error
      call journal('sc','*replace* must specify OLD and NEW or CMD')
      return
   endif
   if(present(ignorecase))then
      ignorecase_local=ignorecase
   else
      ignorecase_local=.false.
   endif
   if(present(occurrence))then
      range_local(1)=abs(occurrence)
   else
      range_local(1)=1
   endif
   if(present(repeat))then
      range_local(2)=range_local(1)+repeat-1
   else
      range_local(2)=original_input_length
   endif
   if(ignorecase_local)then
      targetline_for_comparison=lower(targetline)
      old_local_for_comparison=lower(old_local)
   else
      targetline_for_comparison=targetline
      old_local_for_comparison=old_local
   endif
   if(present(occurrence))then
      if(occurrence.lt.0)then
         flip=.true.
         targetline_for_comparison=reverse(targetline_for_comparison)
         targetline_local=reverse(targetline)
         old_local_for_comparison=reverse(old_local_for_comparison)
         old_local=reverse(old_local)
         new_local=reverse(new_local)
      else
         targetline_local=targetline
      endif
   else
      targetline_local=targetline
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   icount=0                                            ! initialize error flag/change count
   ichange=0                                           ! initialize error flag/change count
   len_old=len(old_local)                              ! length of old substring to be replaced
   len_new=len(new_local)                              ! length of new substring to replace old substring
   left_margin=1                                       ! left_margin is left margin of window to change
   right_margin=len(targetline)                        ! right_margin is right margin of window to change
   newline=''                                          ! begin with a blank line as output string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old.eq.0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichar=len_new + original_input_length
      if(len_new.gt.0)then
         newline=new_local(:len_new)//targetline_local(left_margin:original_input_length)
      else
         newline=targetline_local(left_margin:original_input_length)
      endif
      ichange=1                                        ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ichange
      if(flip) newline=reverse(newline)
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ichar=left_margin                                   ! place to put characters into output string
   ic=left_margin                                      ! place looking at in input string
   loop: do
                                                       ! try finding start of OLD in remaining part of input in change window
      ind=index(targetline_for_comparison(ic:),old_local_for_comparison(:len_old))+ic-1
      if(ind.eq.ic-1.or.ind.gt.right_margin)then       ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      icount=icount+1                                  ! found an old string to change, so increment count of change candidates
      if(ind.gt.ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         newline=newline(:ichar-1)//targetline_local(ic:ind-1)
         ichar=ichar+ladd
      endif
      if(icount.ge.range_local(1).and.icount.le.range_local(2))then    ! check if this is an instance to change or keep
         ichange=ichange+1
         if(len_new.ne.0)then                                          ! put in new string
            newline=newline(:ichar-1)//new_local(:len_new)
            ichar=ichar+len_new
         endif
      else
         if(len_old.ne.0)then                                          ! put in copy of old string
            newline=newline(:ichar-1)//old_local(:len_old)
            ichar=ichar+len_old
         endif
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ichange)
   case (0)                                            ! there were no changes made to the window
      newline=targetline_local                         ! if no changes made output should be input
   case default
      if(ic.le.len(targetline))then                    ! if there is more after last change on original line add it
         newline=newline(:ichar-1)//targetline_local(ic:max(ic,original_input_length))
      endif
   end select
   if(present(ierr))ierr=ichange
   if(flip) newline=reverse(newline)
!-----------------------------------------------------------------------------------------------------------------------------------
end function replace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    substitute(3f) - [M_strings:EDITING] subroutine globally substitutes
!!    one substring for another in string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine substitute(targetline,old,new,ierr,start,end)
!!
!!     character(len=*)              :: targetline
!!     character(len=*),intent(in)   :: old
!!     character(len=*),intent(in)   :: new
!!     integer,intent(out),optional  :: ierr
!!     integer,intent(in),optional   :: start
!!     integer,intent(in),optional   :: end
!!
!!##DESCRIPTION
!!    Globally substitute one substring for another in string.
!!
!!##OPTIONS
!!     TARGETLINE  input line to be changed. Must be long enough to
!!                 hold altered output.
!!     OLD         substring to find and replace
!!     NEW         replacement for OLD substring
!!     IERR        error code. If IER = -1 bad directive, >= 0 then
!!                 count of changes made.
!!     START       sets the left margin to be scanned for OLD in
!!                 TARGETLINE.
!!     END         sets the right margin to be scanned for OLD in
!!                 TARGETLINE.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_substitute
!!    use M_strings, only : substitute
!!    implicit none
!!    ! must be long enough to hold changed line
!!    character(len=80) :: targetline
!!
!!    targetline='this is the input string'
!!    write(*,*)'ORIGINAL    : '//trim(targetline)
!!
!!    ! changes the input to 'THis is THe input string'
!!    call substitute(targetline,'th','TH')
!!    write(*,*)'th => TH    : '//trim(targetline)
!!
!!    ! a null old substring means "at beginning of line"
!!    ! changes the input to 'BEFORE:this is the input string'
!!    call substitute(targetline,'','BEFORE:')
!!    write(*,*)'"" => BEFORE: '//trim(targetline)
!!
!!    ! a null new string deletes occurrences of the old substring
!!    ! changes the input to 'ths s the nput strng'
!!    call substitute(targetline,'i','')
!!    write(*,*)'i => ""     : '//trim(targetline)
!!
!!    end program demo_substitute
!!
!!   Expected output
!!
!!     ORIGINAL    : this is the input string
!!     th => TH    : THis is THe input string
!!     "" => BEFORE: BEFORE:THis is THe input string
!!     i => ""     : BEFORE:THs s THe nput strng
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine substitute(targetline,old,new,ierr,start,end)

! ident_12="@(#)M_strings::substitute(3f): Globally substitute one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*)               :: targetline         ! input line to be changed
character(len=*),intent(in)    :: old                ! old substring to replace
character(len=*),intent(in)    :: new                ! new substring
integer,intent(out),optional   :: ierr               ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
integer,intent(in),optional    :: start              ! start sets the left margin
integer,intent(in),optional    :: end                ! end sets the right margin
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(targetline)) :: dum1               ! scratch string buffers
integer                        :: ml, mr, ier1
integer                        :: maxlengthout       ! MAXIMUM LENGTH ALLOWED FOR NEW STRING
integer                        :: original_input_length
integer                        :: len_old, len_new
integer                        :: ladd
integer                        :: ir
integer                        :: ind
integer                        :: il
integer                        :: id
integer                        :: ic
integer                        :: ichar
!-----------------------------------------------------------------------------------------------------------------------------------
   if (present(start)) then                            ! optional starting column
      ml=start
   else
      ml=1
   endif
   if (present(end)) then                              ! optional ending column
      mr=end
   else
      mr=len(targetline)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ier1=0                                              ! initialize error flag/change count
   maxlengthout=len(targetline)                        ! max length of output string
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   dum1(:)=' '                                         ! initialize string to build output in
   id=mr-ml                                            ! check for window option !! change to optional parameter(s)
!-----------------------------------------------------------------------------------------------------------------------------------
   len_old=len(old)                                    ! length of old substring to be replaced
   len_new=len(new)                                    ! length of new substring to replace old substring
   if(id.le.0)then                                     ! no window so change entire input string
      il=1                                             ! il is left margin of window to change
      ir=maxlengthout                                  ! ir is right margin of window to change
      dum1(:)=' '                                      ! begin with a blank line
   else                                                ! if window is set
      il=ml                                            ! use left margin
      ir=min0(mr,maxlengthout)                         ! use right margin or rightmost
      dum1=targetline(:il-1)                           ! begin with what's below margin
   endif                                               ! end of window settings
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old.eq.0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichar=len_new + original_input_length
      if(ichar.gt.maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if (present(ierr))ierr=ier1
         return
      endif
      if(len_new.gt.0)then
         dum1(il:)=new(:len_new)//targetline(il:original_input_length)
      else
         dum1(il:)=targetline(il:original_input_length)
      endif
      targetline(1:maxlengthout)=dum1(:maxlengthout)
      ier1=1                                           ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ier1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ichar=il                                            ! place to put characters into output string
   ic=il                                               ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1    ! try to find start of old string in remaining part of input in change window
      if(ind.eq.ic-1.or.ind.gt.ir)then                 ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      ier1=ier1+1                                      ! found an old string to change, so increment count of changes
      if(ind.gt.ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         if(ichar-1+ladd.gt.maxlengthout)then
            ier1=-1
            exit loop
         endif
         dum1(ichar:)=targetline(ic:ind-1)
         ichar=ichar+ladd
      endif
      if(ichar-1+len_new.gt.maxlengthout)then
         ier1=-2
         exit loop
      endif
      if(len_new.ne.0)then
         dum1(ichar:)=new(:len_new)
         ichar=ichar+len_new
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ier1)
   case (:-1)
      call journal('sc','*substitute* new line will be too long')
   case (0)                                                ! there were no changes made to the window
   case default
      ladd=original_input_length-ic
      if(ichar+ladd.gt.maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if(present(ierr))ierr=ier1
         return
      endif
      if(ic.lt.len(targetline))then
         dum1(ichar:)=targetline(ic:max(ic,original_input_length))
      endif
      targetline=dum1(:maxlengthout)
   end select
   if(present(ierr))ierr=ier1
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine substitute
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    change(3f) - [M_strings:EDITING] change old string to new string with
!!    a directive like a line editor
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine change(target_string,cmd,ierr)
!!
!!     character(len=*),intent(inout) :: target_string
!!     character(len=*),intent(in)    :: cmd
!!     integer                        :: ierr
!!
!!##DESCRIPTION
!!    change an old substring into a new substring in a character variable
!!    like a line editor. Primarily used to create interactive utilities
!!    such as input history editors for interactive line-mode programs. The
!!    output string is assumed long enough to accommodate the change.
!!    a directive resembles a line editor directive of the form
!!
!!       C/old_string/new_string/
!!
!!    where / may be any character which is not included in old_string
!!    or new_string.
!!
!!    a null old_string implies "beginning of string".
!!
!!##OPTIONS
!!    target_string  line to be changed
!!    cmd            contains instructions to change the string
!!    ierr           error code.
!!
!!       o =-1 bad directive
!!       o =0 no changes made
!!       o >0 count of changes made
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_change
!!
!!     use M_strings, only : change
!!     implicit none
!!     character(len=132) :: line='This is a test string to change'
!!     integer            :: ierr
!!        write(*,*)trim(line)
!!        ! change miniscule a to uppercase A
!!        call change(line,'c/a/A/',ierr)
!!        write(*,*)trim(line)
!!        ! put string at beginning of line
!!        call change(line,'c//prefix: /',ierr)
!!        write(*,*)trim(line)
!!        ! remove blanks
!!        call change(line,'c/ //',ierr)
!!        write(*,*)trim(line)
!!    end program demo_change
!!
!!   Expected output
!!
!!     This is a test string to change
!!     This is A test string to chAnge
!!     prefix: This is A test string to chAnge
!!     prefix:ThisisAteststringtochAnge
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine change(target_string,cmd,ierr)
! Change a string assumed long enough to accommodate the change, with a directive that resembles a line editor directive of the form
!    C/old_string/new_string/
! where / may be any character which is not included in old_string or new_string.
! a null old_string implies "beginning of string"
!===================================================================================================================================

! ident_13="@(#)M_strings::change(3f): change a character string like a line editor"

character(len=*),intent(inout)   :: target_string          ! line to be changed
character(len=*),intent(in)      :: cmd                    ! contains the instructions changing the string
character(len=1)                 :: delimiters
integer                          :: ierr                   ! error code. ier=-1 bad directive;=0 no changes made;>0 ier changes made
integer                          :: itoken
integer,parameter                :: id=2                   ! expected location of delimiter
character(len=:),allocatable     :: old,new                ! scratch string buffers
logical                          :: ifok
integer                          :: lmax                   ! length of target string
integer                          :: start_token,end_token
!-----------------------------------------------------------------------------------------------------------------------------------
   lmax=len_trim(cmd)                                                          ! significant length of change directive
   if(lmax.ge.4)then                         ! strtok ignores blank tokens so look for special case where first token is really null
      delimiters=cmd(id:id)                                                    ! find delimiter in expected location
      itoken=0                                                                 ! initialize strtok(3f) procedure

      if(strtok(cmd(id:),itoken,start_token,end_token,delimiters)) then        ! find OLD string
         old=cmd(start_token+id-1:end_token+id-1)
      else
         old=''
      endif

      if(cmd(id:id).eq.cmd(id+1:id+1))then
         new=old
         old=''
      else                                                                     ! normal case
         ifok=strtok(cmd(id:),itoken,start_token,end_token,delimiters)         ! find NEW string
         if(end_token .eq. (len(cmd)-id+1) )end_token=len_trim(cmd(id:))       ! if missing ending delimiter
         new=cmd(start_token+id-1:min(end_token+id-1,lmax))
      endif

      call substitute(target_string,old,new,ierr,1,len_trim(target_string))    ! change old substrings to new substrings
   else                                                                        ! command was two or less characters
      ierr=-1
      call journal('sc','*change* incorrect change directive -too short')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine change
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     strtok(3f) - [M_strings:TOKENS] Tokenize a string
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  function strtok(source_string,itoken,token_start,token_end,delimiters)
!!  result(strtok_status)
!!
!!   ! returned value
!!   logical                      :: strtok_status
!!   ! string to tokenize
!!   character(len=*),intent(in)  :: source_string
!!   ! token count since started
!!   integer,intent(inout)        :: itoken
!!   ! beginning of token
!!   integer,intent(out)          :: token_start
!!   ! end of token
!!   integer,intent(inout)        :: token_end
!!   ! list of separator characters
!!   character(len=*),intent(in)  :: delimiters
!!
!!##DESCRIPTION
!!     The STRTOK(3f) function is used to isolate sequential tokens in a
!!     string, SOURCE_STRING. These tokens are delimited in the string by
!!     at least one of the characters in DELIMITERS. The first time that
!!     STRTOK(3f) is called, ITOKEN should be specified as zero. Subsequent
!!     calls, wishing to obtain further tokens from the same string,
!!     should pass back in TOKEN_END  and ITOKEN until the function result
!!     returns .false.
!!
!!     This routine assumes no other calls are made to it using any other
!!     input string while it is processing an input line.
!!
!!##OPTIONS
!!     source_string  input string to parse
!!     itoken         token count should be set to zero for a new string
!!     delimiters     characters used to determine the end of tokens
!!
!!##RETURN
!!     token_start    beginning position in SOURCE_STRING where token was found
!!     token_end      ending position in SOURCE_STRING where token was found
!!     strtok_status
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_strtok
!!     use M_strings, only : strtok
!!     implicit none
!!     character(len=264)          :: inline
!!     character(len=*),parameter  :: delimiters=' ;,'
!!     integer                     :: ios, itoken, istart, iend
!!        do ! read lines from stdin until end-of-file or error
!!           read (unit=*,fmt="(a)",iostat=ios) inline
!!           if(ios.ne.0)stop
!!           ! must set ITOKEN=0 before looping on strtok(3f)
!!           ! on a new string.
!!           itoken=0
!!           do while &
!!           &( strtok(inline,itoken,istart,iend,delimiters) )
!!              print *, itoken,&
!!              & 'TOKEN=['//(inline(istart:iend))//']',istart,iend
!!           enddo
!!        enddo
!!     end program demo_strtok
!!
!!     sample input file
!!
!!      this is a test of strtok; A:B :;,C;;
!!
!!     sample output file
!!
!!     1  TOKEN=[this]    2   5
!!     2  TOKEN=[is]      7   8
!!     3  TOKEN=[a]       10  10
!!     4  TOKEN=[test]    12  15
!!     5  TOKEN=[of]      17  18
!!     6  TOKEN=[strtok]  20  25
!!     7  TOKEN=[A:B]     28  30
!!     8  TOKEN=[:]       32  32
!!     9  TOKEN=[C]       35  35
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
FUNCTION strtok(source_string,itoken,token_start,token_end,delimiters) result(strtok_status)
! JSU- 20151030

! ident_14="@(#)M_strings::strtok(3f): Tokenize a string"

character(len=*),intent(in)  :: source_string    ! Source string to tokenize.
character(len=*),intent(in)  :: delimiters       ! list of separator characters. May change between calls
integer,intent(inout)        :: itoken           ! token count since started
logical                      :: strtok_status    ! returned value
integer,intent(out)          :: token_start      ! beginning of token found if function result is .true.
integer,intent(inout)        :: token_end        ! end of token found if function result is .true.
integer,save                 :: isource_len
!----------------------------------------------------------------------------------------------------------------------------
!  calculate where token_start should start for this pass
   if(itoken.le.0)then                           ! this is assumed to be the first call
      token_start=1
   else                                          ! increment start to previous end + 1
      token_start=token_end+1
   endif
!----------------------------------------------------------------------------------------------------------------------------
   isource_len=len(source_string)                ! length of input string
!----------------------------------------------------------------------------------------------------------------------------
   if(token_start.gt.isource_len)then            ! user input error or at end of string
      token_end=isource_len                      ! assume end of token is end of string until proven otherwise so it is set
      strtok_status=.false.
      return
   endif
!----------------------------------------------------------------------------------------------------------------------------
   ! find beginning of token
   do while (token_start .le. isource_len)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_start:token_start)) .ne. 0) then
         token_start = token_start + 1
      else
         exit
      endif
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   token_end=token_start
   do while (token_end .le. isource_len-1)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_end+1:token_end+1)) .ne. 0) then  ! found a delimiter in next character
         exit
      endif
      token_end = token_end + 1
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   if (token_start .gt. isource_len) then        ! determine if finished
      strtok_status=.false.                      ! flag that input string has been completely processed
   else
      itoken=itoken+1                            ! increment count of tokens found
      strtok_status=.true.                       ! flag more tokens may remain
   endif
!----------------------------------------------------------------------------------------------------------------------------
end function strtok
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    modif(3f) - [M_strings:EDITING] emulate the MODIFY command from the
!!    line editor XEDIT
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine modif(cline,cmod)
!!
!!     character(len=*) :: cline ! input string to change
!!     ! directive provides directions on changing string
!!     character(len=*) :: cmod
!!
!!##DESCRIPTION
!!   MODIF(3f) Modifies the line currently pointed at using a directive
!!   that acts much like a line editor directive.
!!   Primarily used to create interactive utilities such as input history
!!   editors for interactive line-mode programs.
!!
!!   the modify directives are as follows-
!!
!!    DIRECTIVE EXPLANATION
!!
!!    ^STRING#   Causes the string of characters between the ^ and the
!!               next # to be inserted before the characters pointed to
!!               by the ^. an ^ or & within the string is treated as a
!!               regular character. If the closing # is not specified,
!!               MODIF(3f) inserts the remainder of the line as if a # was
!!               specified after the last nonblank character.
!!
!!               There are two exceptions. the combination ^# causes a #
!!               to be inserted before the character pointed to by the
!!               ^, and an ^ as the last character of the directives
!!               causes a blank to be inserted.
!!
!!    #          (When not the first # after an ^) causes the character
!!               above it to be deleted.
!!
!!    &          Replaces the character above it with a space.
!!
!!    (SPACE)    A space below a character leaves it unchanged.
!!
!!    Any other character replaces the character above it.
!!
!!##EXAMPLES
!!
!!   Example input/output:
!!
!!    THE INPUT LINE........ 10 THIS STRING  TO BE MORTIFD
!!    THE DIRECTIVES LINE...        ^ IS THE#        D#  ^IE
!!    ALTERED INPUT LINE.... 10 THIS IS THE STRING  TO BE MODIFIED
!!
!!   Sample program:
!!
!!    program demo_modif
!!    use M_strings, only : modif
!!    implicit none
!!    character(len=256)           :: line
!!    integer                      :: ios
!!    integer                      :: count
!!    integer                      :: COMMAND_LINE_LENGTH
!!    character(len=:),allocatable :: COMMAND_LINE
!!       ! get command name length
!!       call get_command_argument(0,length=count)
!!       ! get command line length
!!       call get_command(length=COMMAND_LINE_LENGTH)
!!       ! allocate string big enough to hold command line
!!       allocate(character(len=COMMAND_LINE_LENGTH+200) :: COMMAND_LINE)
!!       ! get command line as a string
!!       call get_command(command=COMMAND_LINE)
!!       ! trim leading spaces just in case
!!       COMMAND_LINE=adjustl(COMMAND_LINE)
!!       ! remove command name
!!       COMMAND_LINE=adjustl(COMMAND_LINE(COUNT+2:))
!!       INFINITE: do
!!          read(*,'(a)',iostat=ios)line
!!          if(ios.ne.0)exit
!!          call modif(line,COMMAND_LINE)
!!          write(*,'(a)')trim(line)
!!       enddo INFINITE
!!    end program demo_modif
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE MODIF(CLINE,MOD)

!$@(#) M_strings::modif(3f): Emulate the MODIFY command from the line editor XEDIT

!
! MODIF
! =====
! ACTION- MODIFIES THE LINE CURRENTLY POINTED AT. THE INPUT STRING CLINE IS ASSUMED TO BE LONG ENOUGH TO ACCOMMODATE THE CHANGES
!         THE MODIFY DIRECTIVES ARE AS FOLLOWS-
!
!   DIRECTIVE                       EXPLANATION
!   ---------                       ------------
!   ^STRING#   CAUSES THE STRING OF CHARACTERS BETWEEN THE ^ AND THE
!              NEXT  # TO BE INSERTED BEFORE THE CHARACTERS POINTED TO
!              BY THE ^. AN ^ OR & WITHIN THE STRING IS TREATED AS A
!              REGULAR CHARACTER. IF THE CLOSING # IS NOT SPECIFIED,
!              MODIF(3f) INSERTS THE REMAINDER OFTHELINE AS IF A # WAS
!              SPECIFIED AFTER THE LAST NONBLANK CHARACTER.
!
!              THERE ARE TWO EXCEPTIONS. THE COMBINATION ^# CAUSES A #
!              TO BE INSERTED BEFORE THE CHARACTER POINTED TO BY THE
!              ^,  AND AN ^ AS THE LAST CHARACTER OF THE DIRECTIVES
!              CAUSES A BLANK TO BE INSERTED.
!
!   #          (WHEN NOT THE FIRST # AFTER AN ^) CAUSES THE CHARACTER
!              ABOVE IT TO BE DELETED.
!
!   &          REPLACES THE CHARACTER ABOVE IT WITH A SPACE.
!
!   (SPACE)    A SPACE BELOW A CHARACTER LEAVES IT UNCHANGED.
!
!   ANY OTHER CHARACTER REPLACES THE CHARACTER ABOVE IT.
!
! EXAMPLE-
! THE INPUT LINE........ 10 THIS STRING  TO BE MORTIFD
! THE DIRECTIVES LINE...        ^ IS THE#        D#  ^IE
! ALTERED INPUT LINE.... 10 THIS IS THE STRING  TO BE MODIFIED
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
character(len=*)            :: cline        !STRING TO BE MODIFIED
character(len=*),intent(in) :: mod          !STRING TO DIRECT MODIFICATION
character(len=len(cline))   :: cmod
character(len=3),parameter  :: c='#&^'      !ASSIGN DEFAULT EDIT CHARACTERS
integer                     :: maxscra      !LENGTH OF SCRATCH BUFFER
character(len=len(cline))   :: dum2         !SCRATCH CHARACTER BUFFER
logical                     :: linsrt       !FLAG FOR INSERTING DATA ON LINE
integer :: i, j, ic, ichar, iend, lmax, lmx1
maxscra=len(cline)
   CMOD=TRIM(MOD)
   LMAX=MIN0(LEN(CLINE),MAXSCRA)         !DETERMINE MAXIMUM LINE LENGTH
   LMX1=LMAX-1                           !MAX LINE LENGTH -1
   DUM2=' '                              !INITIALIZE NEW LINE
   LINSRT=.FALSE.                        !INITIALIZE INSERT MODE
   IEND=len_trim(CMOD)                   !DETERMINE END OF MODS
   I=0                                   !CHAR COUNTER FOR MOD LINE CMOD
   IC=0                                  !CHAR COUNTER FOR CURRENT LINE CLINE
   ICHAR=0                               !CHAR COUNTER NEW LINE DUM2
11 CONTINUE
   I=I+1                                 !NEXT CHAR IN MOD LINE
   IF(ICHAR.GT.LMX1)GOTO 999             !IF TOO MANY CHARS IN NEW LINE
   IF(LINSRT) THEN                       !IF INSERTING NEW CHARS
      IF(I.GT.IEND) CMOD(I:I)=C(1:1)     !FORCE END OF INSERT MODE
      IF(CMOD(I:I).EQ.C(1:1))THEN        !IF END OF INSERT MODE
         LINSRT=.FALSE.                  !RESET INSERT MODE FLAG
         IF(IC+1.EQ.I)THEN               !NULL INSERT STRING
            ICHAR=ICHAR+1                !INCREMENT COUNTER FOR NEW LINE
            DUM2(ICHAR:ICHAR)=C(1:1)     !INSERT INSERT MODE TERMINATOR
         ENDIF
         DO J=IC,I                       !LOOP OF NUMBER OF CHARS INSERTED
            ICHAR=ICHAR+1                !INCREMENT COUNTER FOR NEW LINE
            IF(ICHAR.GT.LMAX)GOTO 999    !IF AT BUFFER LIMIT, QUIT
            DUM2(ICHAR:ICHAR)=CLINE(J:J) !APPEND CHARS FROM ORIG LINE
         ENDDO                           !...WHICH ALIGN WITH INSERTED CHARS
         IC=I                            !RESET CHAR COUNT TO END OF INSERT
         GOTO 1                          !CHECK NEW LINE LENGTH AND CYCLE
      ENDIF                              !END OF TERMINATED INSERT LOGIC
      ICHAR=ICHAR+1                      !INCREMENT NEW LINE COUNT
      DUM2(ICHAR:ICHAR)=CMOD(I:I)        !SET NEWLINE CHAR TO INSERTED CHAR
   ELSE                                  !IF NOT INSERTING CHARACTERS
      IC=IC+1                            !INCREMENT ORIGINAL LINE COUNTER
      IF(CMOD(I:I).EQ.C(1:1))GOTO 1      !IF DELETE CHAR. NO COPY AND CYCLE
      IF(CMOD(I:I).EQ.C(3:3))THEN        !IF BEGIN INSERT MODE
         LINSRT=.TRUE.                   !SET INSERT FLAG TRUE
         GOTO 1                          !CHECK LINE LENGTH AND CONTINUE
      ENDIF                              !IF NOT BEGINNING INSERT MODE
      ICHAR=ICHAR+1                      !INCREMENT NEW LINE COUNTER
      IF(CMOD(I:I).EQ.C(2:2))THEN        !IF REPLACE WITH BLANK
         DUM2(ICHAR:ICHAR)=' '           !SET NEWLINE CHAR TO BLANK
         GOTO 1                          !CHECK LINE LENGTH AND CYCLE
      ENDIF                              !IF NOT REPLACE WITH BLANK
      IF(CMOD(I:I).EQ.' ')THEN           !IF BLANK, KEEP ORIGINAL CHARACTER
         DUM2(ICHAR:ICHAR)=CLINE(IC:IC)  !SET NEW CHAR TO ORIGINAL CHAR
      ELSE                               !IF NOT KEEPING OLD CHAR
         DUM2(ICHAR:ICHAR)=CMOD(I:I)     !REPLACE ORIGINAL CHAR WITH NEW
      ENDIF                              !END CHAR KEEP OR REPLACE
   ENDIF                                 !END INSERT OR NO-INSERT
1  CONTINUE
   IF(I.LT.LMAX)GOTO 11                  !CHECK FOR END OF LINE REACHED
                                         !AND CYCLE IF OK
999   CONTINUE
   CLINE=DUM2                            !SET ORIGINAL CHARS TO NEW CHARS
END SUBROUTINE MODIF                     !RETURN
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      len_white(3f) - [M_strings:LENGTH] get length of string trimmed
!!      of whitespace.
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    integer function len_white(string)
!!
!!     character(len=*) :: string
!!
!!##DESCRIPTION
!!      len_white(3f) returns the position of the last character in
!!      string that is not a whitespace character. The Fortran90 intrinsic
!!      LEN_TRIM() should be used when trailing whitespace can be assumed
!!      to always be spaces.
!!
!!      This procedure was heavily used in the past because ANSI FORTRAN
!!      77 character objects are fixed length and blank padded and the
!!      LEN_TRIM() intrinsic did not exist. It should now be used only when
!!      whitespace characters other than blanks are likely.
!!
!!##OPTIONS
!!      string     input string whose trimmed length is being calculated
!!                 ignoring all trailing whitespace characters.
!!##RETURNS
!!      len_white  the number of characters in the trimmed string
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_len_white
!!
!!      use M_strings, only : len_white
!!      implicit none
!!      character(len=80) ::  s
!!      integer           :: ilen, lastnb
!!      intrinsic len
!!
!!      s=' ABCDEFG abcdefg '
!!      ilen = len(s)
!!      lastnb = len_white(s)
!!
!!      write(*,*) 'total length of variable is ',ilen
!!      write(*,*) 'trimmed length of variable is ',lastnb
!!      write(*,*) 'trimmed string=[',s(:lastnb),']'
!!
!!     end program demo_len_white
!!
!!##NOTES
!!
!! o len_white
!!
!!      is a resource-intensive routine. Once the end of
!!      the string is found, it is probably best to keep track of it in
!!      order to avoid repeated calls to len_white. Because they
!!      might be more efficient, consider looking for vendor-supplied or
!!      system-optimized equivalents. For example:
!!
!!         o lnblnk - Solaris f77
!!         o len_trim - FORTRAN 90
!!
!! o Some compilers seem to have trouble passing a string of variable
!!   length properly. To be safe, use something like this:
!!
!!       subroutine message(s)
!!        character(len=*) :: s ! s is of variable length
!!           ilen=len(s)        ! get total length of variable
!!           ! explicitly specify a substring instead of just variable name
!!           lastnb = len_white(s(:ilen))
!!           write(*,*)'error:[',s(:lastnb),']'
!!       end subroutine messages
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental integer function len_white(string)
!  DEPRECATED. Use len_trim(3f),trim(3f) unless you might have trailing nulls (common when interacting with C procedures)"
!  John S. Urban, 1984, 1997-12-31
!  Note that if the string is blank, a length of 0 is returned; which is not a legal string length in Fortran77.
!  this routine used to return one instead of zero.
!   - mod 1:     1994
!                added null (char(0)) because HP and some Suns not padding
!                strings with blank, but with null characters; 1994 JSU
!   - mod 2:     1999
!                update syntax with INTENT(), ENDDO, no RETURN
!                still need instead of LEN_TRIM() because some systems stil pad CHARACTER with NULL
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_15="@(#)M_strings::len_white(3f): return position of last non-blank/non-null character in string"

character(len=*),intent(in):: string ! input string to determine length of
integer                    :: i10
intrinsic len
   len_white=0
   do i10=len(string),1,-1
      select case(string(i10:i10))
      case(' ')                 ! space(32)
      case(char(0))             ! null(0)
      case(char(9):char(13))    ! tab(9), new line(10), vertical tab(11), formfeed(12), carriage return(13)
      case default
         len_white=i10
         exit
      end select
   enddo
end function len_white
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    crop(3f) - [M_strings:WHITESPACE] trim leading and trailing blanks and control characters from a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function crop(strin) result (strout)
!!
!!     character(len=*),intent(in)  :: strin
!!     character(len=:),allocatable :: strout
!!
!!##DESCRIPTION
!!    All control characters throughout the string are replaced with spaces
!!    and leading and trailing spaces are trimmed from the resulting string.
!!    Tabs are expanded assuming a stop every eight characters.
!!
!!
!!##OPTIONS
!!    strin   input string to trim leading and trailing space and control
!!    characters
!!            from
!!
!!##RETURNS
!!    strout  cropped version of input string
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_crop
!!    use M_strings, only: crop
!!    implicit none
!!    character(len=20) ::  untrimmed = '   ABCDEFG abcdefg  '
!!       write(*,*) 'untrimmed string=[',untrimmed,']'
!!       write(*,*) 'cropped string=[',crop(untrimmed),']'
!!    end program demo_crop
!!
!!   Expected output
!!
!!      untrimmed string=[   ABCDEFG abcdefg                      ]
!!      cropped string=[ABCDEFG abcdefg]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function crop(strin) result (strout)

! ident_16="@(#)M_strings::crop(3f): replace control characters with whitespace and trim leading and trailings spaces from resulting string"

character(len=*),intent(in)  :: strin
character(len=:),allocatable :: strout
   strout=trim(adjustl(noesc(dilate(strin))))
end function crop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    transliterate(3f) - [M_strings:EDITING] replace characters from old set with new set
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure function transliterate(instr,old_set,new_set) result(outstr)
!!
!!     character(len=*),intent(in)  :: instr
!!     character(len=*),intent(in)  :: old_set
!!     character(len=*),intent(in)  :: new_set
!!     character(len=len(instr))    :: outstr
!!
!!##DESCRIPTION
!!    Translate, squeeze, and/or delete characters from the input string.
!!
!!##OPTIONS
!!    instr    input string to change
!!    old_set  list of letters to change in INSTR if found
!!
!!             Each character in the input string that matches a character
!!             in the old set is replaced.
!!
!!    new_set  list of letters to replace letters in OLD_SET with.
!!
!!             If the new_set is the empty set the matched characters
!!             are deleted.
!!
!!             If the new_set is shorter than the old set the last character
!!             in the new set is used to replace the remaining characters
!!             in the new set.
!!
!!##RETURNS
!!    outstr   instr with substitutions applied
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_transliterate
!!
!!     use M_strings, only : transliterate
!!     implicit none
!!     character(len=80)   :: STRING
!!
!!     STRING='aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ'
!!     write(*,'(a)') STRING
!!
!!     ! convert a string to uppercase:
!!     write(*,*) TRANSLITERATE(STRING, &
!!     & 'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')
!!
!!     ! change all miniscule letters to a colon (":"):
!!     write(*,*) TRANSLITERATE(STRING, &
!!     & 'abcdefghijklmnopqrstuvwxyz',':')
!!
!!     ! delete all miniscule letters
!!     write(*,*) TRANSLITERATE(STRING, &
!!     & 'abcdefghijklmnopqrstuvwxyz','')
!!
!!    end program demo_transliterate
!!
!!    Expected output
!!
!!     > aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ
!!     > AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ
!!     > :A:B:C:D:E:F:G:H:I:J:K:L:M:N:O:P:Q:R:S:T:U:V:W:X:Y:Z
!!     > ABCDEFGHIJKLMNOPQRSTUVWXYZ
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
PURE FUNCTION transliterate(instr,old_set,new_set) RESULT(outstr)

! ident_17="@(#)M_strings::transliterate(3f): replace characters from old set with new set"

!-----------------------------------------------------------------------------------------------------------------------------------
CHARACTER(LEN=*),INTENT(IN)  :: instr                             ! input string to change
CHARACTER(LEN=*),intent(in)  :: old_set
CHARACTER(LEN=*),intent(in)  :: new_set
!-----------------------------------------------------------------------------------------------------------------------------------
CHARACTER(LEN=LEN(instr))    :: outstr                            ! output string to generate
!-----------------------------------------------------------------------------------------------------------------------------------
INTEGER                      :: i10                               ! loop counter for stepping thru string
INTEGER                      :: ii,jj
!-----------------------------------------------------------------------------------------------------------------------------------
   jj=LEN(new_set)
   IF(jj.NE.0)THEN
      outstr=instr                                                ! initially assume output string equals input string
      stepthru: DO i10 = 1, LEN(instr)
         ii=iNDEX(old_set,instr(i10:i10))                         ! see if current character is in old_set
         IF (ii.NE.0)THEN
            if(ii.le.jj)then                                      ! use corresponding character in new_set
               outstr(i10:i10) = new_set(ii:ii)
            else
               outstr(i10:i10) = new_set(jj:jj)                   ! new_set not as long as old_set; use last character in new_set
            endif
         ENDIF
      ENDDO stepthru
   else                                                           ! new_set is null string so delete characters in old_set
      outstr=' '
      hopthru: DO i10 = 1, LEN(instr)
         ii=iNDEX(old_set,instr(i10:i10))                         ! see if current character is in old_set
         IF (ii.EQ.0)THEN                                         ! only keep characters not in old_set
            jj=jj+1
            outstr(jj:jj) = instr(i10:i10)
         ENDIF
      ENDDO hopthru
   endif
END FUNCTION transliterate
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    rotate13(3f) - [M_strings] apply trivial ROT13 encryption to a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    rotate13(input) result(output)
!!
!!     character(len=*),intent(in) :: input
!!     character(len=len(input))   :: output
!!
!!##DESCRIPTION
!!    ROT13 ("rotate by 13 places", sometimes hyphenated ROT-13) is a simple
!!    letter substitution cipher that replaces a letter with the 13th letter
!!    after it in the alphabet; wrapping around if necessary.
!!
!!    The transformation can be done using a lookup table, such as the
!!    following:
!!
!!       Input  ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
!!       Output NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm
!!
!!    ROT13 is used in online forums as a means of hiding spoilers,
!!    punchlines, puzzle solutions, and offensive materials from the casual
!!    glance. ROT13 has inspired a variety of letter and word games on-line,
!!    and is frequently mentioned in newsgroup conversations.
!!
!!    The algorithm provides virtually no cryptographic security, and is
!!    often cited as a canonical example of weak encryption.
!!
!!    ROT13 is a special case of the Caesar cipher which was developed in
!!    ancient Rome.
!!
!!    ALGORITHM
!!
!!    Applying ROT13 to a piece of text merely requires examining its
!!    alphabetic characters and replacing each one by the letter 13 places
!!    further along in the alphabet, wrapping back to the beginning if
!!    necessary. A becomes N, B becomes O, and so on up to M, which becomes
!!    Z, then the sequence continues at the beginning of the alphabet: N
!!    becomes A, O becomes B, and so on to Z, which becomes M. Only those
!!    letters which occur in the English alphabet are affected; numbers,
!!    symbols, whitespace, and all other characters are left unchanged.
!!
!!    SAME ALGORITHM FOR ENCODING AND DECODING
!!
!!    Because there are 26 letters in the English alphabet and 26 = 2 x 13,
!!    the ROT13 function is its own inverse: so the same action can be used
!!    for encoding and decoding. In other words, two successive applications
!!    of ROT13 restore the original text (in mathematics, this is sometimes
!!    called an involution; in cryptography, a reciprocal cipher).
!!
!!    TRIVIAL SECURITY
!!
!!    The use of a constant shift means that the encryption effectively
!!    has no key, and decryption requires no more knowledge than the fact
!!    that ROT13 is in use. Even without this knowledge, the algorithm is
!!    easily broken through frequency analysis.
!!
!!    In encrypted normal English-language text of any significant size,
!!    ROT13 is recognizable from some letter/word patterns. The words "n",
!!    "V" (capitalized only), and "gur" (ROT13 for "a", "I", and "the"),
!!    and words ending in "yl" ("ly") are examples.
!!
!!##REFERENCES
!!    Wikipedia, the free encyclopedia
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_rotate13
!!    use M_strings, only : rotate13
!!    implicit none
!!    character(len=256) :: line
!!    integer            :: ios
!!    do
!!       read(*,'(a)',iostat=ios)line
!!       if(ios.ne.0)exit
!!       write(*,'(a)')rotate13(line)
!!    enddo
!!    end program demo_rotate13
!!
!!  Sample usage:
!!
!!    demo_rotate13
!!    United we stand, divided we fall.
!!    Havgrq jr fgnaq, qvivqrq jr snyy.
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function rotate13 (input)
implicit none

! ident_18="@(#)M_strings::rotate13(3f): converts a character to its ROT13 equivalent, which is a trivial encryption."

character(len=*),intent(in) :: input
character(len=len(input))   :: rotate13
integer                     :: itemp
integer                     :: i
   rotate13=' '
   do i=1,len_trim(input)
      itemp = ichar (input(i:i))
      select case(itemp)
       case(65:77,97:109)
         itemp = itemp + 13
       case(78:90,110:122)
         itemp = itemp - 13
      end select
      rotate13(i:i) = char ( itemp )
   enddo

end function rotate13
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    join(3f) - [M_strings:EDITING] append CHARACTER variable array into
!!    a single CHARACTER variable with specified separator
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure function join(str,sep,trm,left,right,start,end) result (string)
!!
!!     character(len=*),intent(in)          :: str(:)
!!     character(len=*),intent(in),optional :: sep
!!     logical,intent(in),optional          :: trm
!!     character(len=*),intent(in),optional :: right
!!     character(len=*),intent(in),optional :: left
!!     character(len=*),intent(in),optional :: start
!!     character(len=*),intent(in),optional :: end
!!     character(len=:),allocatable         :: string
!!
!!##DESCRIPTION
!!   JOIN(3f) appends the elements of a CHARACTER array into a single
!!   CHARACTER variable, with elements 1 to N joined from left to right.
!!   By default each element is trimmed of trailing spaces and the
!!   default separator is a null string.
!!
!!##OPTIONS
!!      STR(:)  array of CHARACTER variables to be joined
!!      SEP     separator string to place between each variable. defaults
!!              to a null string.
!!      LEFT    string to place at left of each element
!!      RIGHT   string to place at right of each element
!!      START   prefix string
!!      END     suffix string
!!      TRM     option to trim each element of STR of trailing
!!              spaces. Defaults to .TRUE.
!!
!!##RESULT
!!      STRING  CHARACTER variable composed of all of the elements of STR()
!!              appended together with the optional separator SEP placed
!!              between the elements.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!   program demo_join
!!   use M_strings, only: join
!!   implicit none
!!   character(len=:),allocatable  :: s(:)
!!   character(len=:),allocatable  :: out
!!   integer                       :: i
!!     s=[character(len=10) :: 'United',' we',' stand,', &
!!     & ' divided',' we fall.']
!!     out=join(s)
!!     write(*,'(a)') out
!!     write(*,'(a)') join(s,trm=.false.)
!!     write(*,'(a)') (join(s,trm=.false.,sep='|'),i=1,3)
!!     write(*,'(a)') join(s,sep='<>')
!!     write(*,'(a)') join(s,sep=';',left='[',right=']')
!!     write(*,'(a)') join(s,left='[',right=']')
!!     write(*,'(a)') join(s,left='>>')
!!   end program demo_join
!!
!!  Expected output:
!!
!!   United we stand, divided we fall.
!!   United     we        stand,    divided   we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United<> we<> stand,<> divided<> we fall.
!!   [United];[ we];[ stand,];[ divided];[ we fall.]
!!   [United][ we][ stand,][ divided][ we fall.]
!!   >>United>> we>> stand,>> divided>> we fall.
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function join(str,sep,trm,left,right,start,end) result (string)

! ident_19="@(#)M_strings::join(3f): merge string array into a single CHARACTER value adding specified separators, caps, prefix and suffix"

character(len=*),intent(in)          :: str(:)
character(len=*),intent(in),optional :: sep, right, left, start, end
logical,intent(in),optional          :: trm
character(len=:),allocatable         :: sep_local, left_local, right_local
character(len=:),allocatable         :: string
logical                              :: trm_local
integer                              :: i
   if(present(sep))then   ; sep_local=sep     ; else ; sep_local=''     ; endif
   if(present(trm))then   ; trm_local=trm     ; else ; trm_local=.true. ; endif
   if(present(left))then  ; left_local=left   ; else ; left_local=''    ; endif
   if(present(right))then ; right_local=right ; else ; right_local=''   ; endif
   string=''
   if(size(str).eq.0)then
      string=string//left_local//right_local
   else
      do i = 1,size(str)-1
         if(trm_local)then
            string=string//left_local//trim(str(i))//right_local//sep_local
         else
            string=string//left_local//str(i)//right_local//sep_local
         endif
      enddo
      if(trm_local)then
         string=string//left_local//trim(str(i))//right_local
      else
         string=string//left_local//str(i)//right_local
      endif
   endif
   if(present(start))string=start//string
   if(present(end))string=string//end
end function join
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      reverse(3f) - [M_strings:EDITING] Return a string reversed
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function reverse(str) result (string)
!!
!!     character(*), intent(in) :: str
!!     character(len(str))      :: string
!!
!!##DESCRIPTION
!!      reverse(string) returns a copy of the input string with
!!      all characters reversed from right to left.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_reverse
!!       use M_strings, only: reverse
!!       implicit none
!!       character(len=:),allocatable  :: s
!!          write(*,*)'REVERSE STRINGS:',reverse('Madam, I''m Adam')
!!          s='abcdefghijklmnopqrstuvwxyz'
!!          write(*,*) 'original input string is ....',s
!!          write(*,*) 'reversed output string is ...',reverse(s)
!!       end program demo_reverse
!!
!!    Expected output
!!
!!      original input string is ....abcdefghijklmnopqrstuvwxyz
!!      reversed output string is ...zyxwvutsrqponmlkjihgfedcba
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental function reverse(string ) result (rev)

! ident_20="@(#)M_strings::reverse(3f): Return a string reversed"

character(len=*),intent(in)    :: string   ! string to reverse
character(len=len(string))     :: rev      ! return value (reversed string)
integer                        :: length
integer                        :: i
   length = len(string)
   do i = 1,length
      rev(i:i)=string(length-i+1:length-i+1)
   enddo
end function reverse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! upper_quoted(3f) - [M_strings:CASE] elemental function converts string to miniscule skipping strings quoted per Fortran syntax rules
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function upper_quoted(str) result (string)
!!
!!     character(*), intent(in)    :: str
!!     character(len(str))         :: string  ! output string
!!
!!##DESCRIPTION
!!    upper_quoted(string) returns a copy of the input string with all not-quoted
!!    characters converted to uppercase, assuming ASCII character sets
!!    are being used. The quoting rules are the same as for Fortran source.
!!    Either a single or double quote starts a quoted string, and a quote
!!    character of the same type is doubled when it appears internally in
!!    the quoted string. If a double quote quotes the string single quotes
!!    may appear in the quoted string as single characters, and vice-versa
!!    for single quotes.
!!
!!##OPTIONS
!!    str    string to convert to uppercase
!!
!!##RESULTS
!!    upper  copy of the input string with all unquoted characters converted
!!           to uppercase
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_upper_quoted
!!     use M_strings, only: upper_quoted
!!     implicit none
!!     character(len=:),allocatable  :: s
!!     s=' ABCDEFG abcdefg "Double-Quoted" ''Single-Quoted'' "with ""&
!!        & Quote" everything else'
!!        write(*,*) 'mixed-case input string is ....',s
!!        write(*,*) 'upper-case output string is ...',upper_quoted(s)
!!        write(*,'(1x,a,*(a:,"+"))') 'upper_quoted(3f) is elemental ==>', &
!!        & upper_quoted(["abc","def","ghi"])
!!     end program demo_upper_quoted
!!
!!    Expected output:
!!
!!     mixed-case input string is .... ABCDEFG abcdefg "Double-Quoted"
!!     'Single-Quoted' "with "" Quote" everything else
!!     upper-case output string is ... ABCDEFG ABCDEFG "Double-Quoted"
!!     'Single-Quoted' "with "" Quote" EVERYTHING ELSE
!!     upper_quoted(3f) is elemental ==>ABC+DEF+GHI
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental pure function upper_quoted(str) result (string)

! ident_21="@(#)M_strings::upper_quoted(3f): elemental function converts string to miniscule skipping strings quoted per Fortran syntax rules"

character(len=*), intent(in)   :: str     ! The input string
character(len=len(str))        :: string  ! The output string
logical                        :: toggle
character(len=1)               :: togglechar
integer                        :: irnk
integer                        :: i
character(len=26), parameter   :: large="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
character(len=26), parameter   :: small="abcdefghijklmnopqrstuvwxyz"

   string=str
   toggle = .TRUE.
   do i = 1, len_trim(string)
      if(toggle) then
         if(string(i:i) == '"' .or. string(i:i) == "'") then
            toggle = .not. toggle
            togglechar = string(i:i)
         endif
         irnk = index(small, string(i:i))
         if(irnk > 0) then
            string(i:i) = large(irnk:irnk)
         endif
      else
         if(string(i:i) == togglechar) toggle = .not. toggle
      endif
   enddo
end function upper_quoted
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! upper(3f) - [M_strings:CASE] changes a string to uppercase
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function upper(str,begin,end) result (string)
!!
!!     character(*), intent(in)    :: str
!!     integer,optional,intent(in) :: begin,end
!!     character(len(str))         :: string  ! output string
!!
!!##DESCRIPTION
!!      upper(string) returns a copy of the input string with all characters
!!      converted in the optionally specified range to uppercase, assuming
!!      ASCII character sets are being used. If no range is specified the
!!      entire string is converted to uppercase.
!!
!!##OPTIONS
!!    str    string to convert to uppercase
!!    begin  optional starting position in "str" to begin converting to
!!           uppercase
!!    end    optional ending position in "str" to stop converting to
!!           uppercase
!!
!!##RESULTS
!!    upper  copy of the input string with all characters converted to
!!           uppercase over optionally specified range.
!!
!!##TRIVIA
!!    The terms "uppercase" and "lowercase" date back to the early days of
!!    the mechanical printing press. Individual metal alloy casts of each
!!    needed letter, or punctuation symbol, were meticulously added to a
!!    press block, by hand, before rolling out copies of a page. These
!!    metal casts were stored and organized in wooden cases. The more
!!    often needed miniscule letters were placed closer to hand, in the
!!    lower cases of the work bench. The less often needed, capitalized,
!!    majuscule letters, ended up in the harder to reach upper cases.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_upper
!!     use M_strings, only: upper
!!     implicit none
!!     character(len=:),allocatable  :: s
!!        s=' ABCDEFG abcdefg '
!!        write(*,*) 'mixed-case input string is ....',s
!!        write(*,*) 'upper-case output string is ...',upper(s)
!!        write(*,*) 'make first character uppercase  ... ',&
!!        & upper('this is a sentence.',1,1)
!!        write(*,'(1x,a,*(a:,"+"))') 'UPPER(3f) is elemental ==>',&
!!        & upper(["abc","def","ghi"])
!!     end program demo_upper
!!
!!    Expected output
!!
!!     mixed-case input string is .... ABCDEFG abcdefg
!!     upper-case output string is ... ABCDEFG ABCDEFG
!!     make first character uppercase  ... This is a sentence.
!!     UPPER(3f) is elemental ==>ABC+DEF+GHI
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
! Timing
!
!    Several different methods have been proposed for changing case.
!    A simple program that copies a large file and converts it to
!    uppercase was timed and compared to a simple copy. This was used
!    to select the default function.
!
! NULL:    83.41user  9.25system 1:37.94elapsed 94%CPU
! upper:  101.44user 10.89system 1:58.36elapsed 94%CPU
! upper2: 105.04user 10.69system 2:04.17elapsed 93%CPU
! upper3: 267.21user 11.69system 4:49.21elapsed 96%CPU
elemental pure function upper(str,begin,end) result (string)

! ident_22="@(#)M_strings::upper(3f): Changes a string to uppercase"

character(*), intent(in)      :: str                 ! inpout string to convert to all uppercase
integer, intent(in), optional :: begin,end
character(len(str))           :: string              ! output string that contains no miniscule letters
integer                       :: i                   ! loop counter
integer                       :: ibegin,iend
integer,parameter             :: diff = iachar('A')-iachar('a')
   string = str                                      ! initialize output string to input string
   ibegin=1
   iend=len_trim(str)

   if (present(begin))then
      ibegin = min(max(ibegin,begin),iend)
   endif

   if (present(end))then
      iend= max(1,min(iend,end))
   endif

   do concurrent (i = ibegin:iend)                   ! step thru each letter in the string in specified range
       select case (str(i:i))
       case ('a':'z')                                ! located miniscule letter
          string(i:i) = char(iachar(str(i:i))+diff)  ! change miniscule letter to uppercase
       end select
   enddo

end function upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    lower(3f) - [M_strings:CASE] changes a string to lowercase over
!!    specified range
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function lower(str,begin,end) result (string)
!!
!!     character(*), intent(in) :: str
!!     integer,optional         :: begin, end
!!     character(len(str))      :: string  ! output string
!!
!!##DESCRIPTION
!!      lower(string) returns a copy of the input string with all characters
!!      converted to miniscule over the specified range, assuming ASCII
!!      character sets are being used. If no range is specified the entire
!!      string is converted to miniscule.
!!
!!##OPTIONS
!!    str    string to convert to miniscule
!!    begin  optional starting position in "str" to begin converting to
!!           miniscule
!!    end    optional ending position in "str" to stop converting to
!!           miniscule
!!
!!##RESULTS
!!    lower  copy of the input string with all characters converted to
!!           miniscule over optionally specified range.
!!
!!##TRIVIA
!!    The terms "uppercase" and "lowercase" date back to the early days of
!!    the mechanical printing press. Individual metal alloy casts of each
!!    needed letter, or punctuation symbol, were meticulously added to a
!!    press block, by hand, before rolling out copies of a page. These
!!    metal casts were stored and organized in wooden cases. The more
!!    often needed miniscule letters were placed closer to hand, in the
!!    lower cases of the work bench. The less often needed, capitalized,
!!    majuscule letters, ended up in the harder to reach upper cases.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_lower
!!       use M_strings, only: lower
!!       implicit none
!!       character(len=:),allocatable  :: s
!!          s=' ABCDEFG abcdefg '
!!          write(*,*) 'mixed-case input string is ....',s
!!          write(*,*) 'lower-case output string is ...',lower(s)
!!       end program demo_lower
!!
!!    Expected output
!!
!!       mixed-case input string is .... ABCDEFG abcdefg
!!       lower-case output string is ... abcdefg abcdefg
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental pure function lower(str,begin,end) result (string)

! ident_23="@(#)M_strings::lower(3f): Changes a string to lowercase over specified range"

character(*), intent(in)     :: str
character(len(str))          :: string
integer,intent(in),optional  :: begin, end
integer                      :: i
integer                      :: ibegin, iend
integer,parameter             :: diff = iachar('A')-iachar('a')
   string = str
   ibegin=1
   iend=len_trim(str)

   if (present(begin))then
      ibegin = min(max(1,begin),iend)
   endif

   if (present(end))then
      iend= max(1,min(iend,end))
   endif

   do concurrent (i = ibegin:iend)                   ! step thru each letter in the string in specified range
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))-diff)   ! change letter to miniscule
      case default
      end select
   enddo

end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    switch(3f) - [M_strings:ARRAY] converts between CHARACTER scalar and
!!    array of single characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure function switch(array) result (string)
!!
!!     character(len=1),intent(in) :: array(:)
!!     character(len=SIZE(array))  :: string
!!
!!      or
!!
!!    pure function switch(string) result (array)
!!
!!     character(len=1),intent(in) :: array(:)
!!     character(len=SIZE(array))  :: string
!!
!!##DESCRIPTION
!!    SWITCH(3f): generic function that switches CHARACTER string to an array
!!    of single characters or an array of single characters to a CHARACTER
!!    string. Useful in passing strings to C. New Fortran features may
!!    supersede these routines.
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_switch
!!    use M_strings, only : switch, isalpha, islower, nospace
!!    character(len=*),parameter :: &
!!    & dashes='-----------------------------------'
!!    character(len=*),parameter :: string='This is a string'
!!    character(len=1024)        :: line
!!
!!    ! First, examples of standard Fortran features
!!    ! returns array [F,T,T,T,T,T]
!!    write(*,*)['A','=','=','=','=','='].eq.'='
!!    ! this would return T
!!    write(*,*)all(['=','=','=','=','=','='].eq.'=')
!!    ! this would return F
!!    write(*,*)all(['A','=','=','=','=','='].eq.'=')
!!
!!    ! so to test if the string DASHES is all dashes
!!    ! using SWITCH(3f) is
!!    if(all(switch(dashes).eq.'-'))then
!!       write(*,*)'DASHES is all dashes'
!!    endif
!!
!!    ! so to test is a string is all letters
!!    ! isalpha(3f) returns .true. only if character is a letter
!!    ! false because dashes are not a letter
!!    write(*,*) all(isalpha(switch(dashes)))
!!    ! false because of spaces
!!    write(*,*) all(isalpha(switch(string)))
!!    ! true because removed whitespace
!!    write(*,*) all(isalpha(switch(nospace(string))))
!!
!!    ! to see if a string is all uppercase
!!    ! show the string
!!    write(*,*) string
!!    ! converted to character array
!!    write(*,'(1x,*("[",a,"]":))') switch(string)
!!    write(*,'(*(l3))') islower(switch(string))
!!
!!    ! we need a string that is all letters
!!    line=nospace(string)
!!    write(*,*)'LINE=',trim(line)
!!    ! all true except first character
!!    write(*,*) islower(switch(nospace(string)))
!!    ! should be false
!!    write(*,*) all(islower(switch(nospace(string))))
!!    ! should be true
!!    write(*,*) all(islower(switch(nospace(string(2:)))))
!!
!!    end program demo_switch
!!
!!  Expected output
!!
!!     F T T T T T
!!     T
!!     F
!!     DASHES is all dashes
!!     F
!!     F
!!     T
!!     This is a string
!!     [T][h][i][s][ ][i][s][ ][a][ ][s][t][r][i][n][g]
!!      F  T  T  T  F  T  T  F  T  F  T  T  T  T  T  T
!!     LINE=Thisisastring
!!     F T T T T T T T T T T T T
!!     F
!!     T
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function a2s(array)  result (string)

! ident_24="@(#)M_strings::a2s(3fp): function to copy char array to string"

character(len=1),intent(in) :: array(:)
character(len=SIZE(array))  :: string
integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall( i = 1:size(array)) string(i:i) = array(i)
! ----------------------------------------------------------------------------------------------------------------------------------
end function a2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function s2a(string)  RESULT (array)

! ident_25="@(#)M_strings::s2a(3fp): function to copy string(1:Clen(string)) to char array"

character(len=*),intent(in) :: string
character(len=1)            :: array(len(string))
integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall(i=1:len(string)) array(i) = string(i:i)
! ----------------------------------------------------------------------------------------------------------------------------------
end function s2a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      s2c(3f) - [M_strings:ARRAY] convert character variable to array of
!!      characters with last element set to null
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function s2c(string)
!!
!!     character(len=*),intent=(in)  :: string
!!     character(len=1),allocatable  :: s2c(:)
!!
!!##DESCRIPTION
!!    Given a character variable convert it to an array of single-character
!!    character variables with the last element set to a null character.
!!    This is generally used to pass character variables to C procedures.
!!
!!##EXAMPLES
!!
!!    Sample Program:
!!
!!     program demo_s2c
!!     use M_strings, only : s2c
!!     implicit none
!!     character(len=*),parameter   :: string="single string"
!!     character(len=3),allocatable :: array(:)
!!        write(*,*)'INPUT STRING ',trim(string)
!!        ! put one character into each 3-character element of array
!!        array=s2c(string)
!!        ! write array with ASCII Decimal Equivalent below it except show
!!        ! unprintable characters like NULL as "XXX"
!!        write(*,'(1x,*("[",a3,"]":))')&
!!             & merge('XXX',array,ichar(array(:)(1:1)).lt.32)
!!        write(*,'(1x,*("[",i3,"]":))')&
!!             & ichar(array(:)(1:1))
!!     end program demo_s2c
!!
!!   Expected output:
!!
!!    INPUT STRING single string
!!    [s  ][i  ][n  ][g  ][l  ][e  ][   ][s  ][t  ][r  ][i  ][n  ][g  ][XXX]
!!    [115][105][110][103][108][101][ 32][115][116][114][105][110][103][  0]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function s2c(string)  RESULT (array)
use,intrinsic :: ISO_C_BINDING, only : C_CHAR

! ident_26="@(#)M_strings::s2c(3f): copy string(1:Clen(string)) to char array with null terminator"

character(len=*),intent(in)     :: string

! This is changing, but currently the most portable way to pass a CHARACTER variable to C is to convert it to an array of
! character variables with length one and add a null character to the end of the array. The s2c(3f) function helps do this.
character(kind=C_CHAR,len=1)    :: array(len_trim(string)+1)
integer                         :: i
   do i = 1,size(array)-1
      array(i) = string(i:i)
   enddo
   array(size(array):)=achar(0)
end function s2c
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      c2s(3f) - [M_strings:ARRAY] convert C string pointer to Fortran
!!      character string
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function c2s(c_string_pointer) result(f_string)
!!
!!     type(c_ptr), intent(in)       :: c_string_pointer
!!     character(len=:), allocatable :: f_string
!!
!!##DESCRIPTION
!!    Given a C pointer to a character string return a Fortran character
!!    string.
!!
!!##OPTIONS
!!    c_string_pointer  C pointer to convert
!!
!!##RETURNS
!!    f_string          Fortran character variable to return
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function c2s(c_string_pointer) result(f_string)
! gets a C string (pointer), and returns the corresponding Fortran string;
! If the C string is null, it returns "NULL", similar to C's "(null)" printed in similar cases:
use, intrinsic :: iso_c_binding, only: c_ptr,c_f_pointer,c_char,c_null_char

! ident_27="@(#)M_strings::c2s(3f): copy pointer to C char array till a null is encountered to a Fortran string up to 4096 characters"

integer,parameter                             :: max_length=4096
type(c_ptr), intent(in)                       :: c_string_pointer
character(len=:), allocatable                 :: f_string
character(kind=c_char), dimension(:), pointer :: char_array_pointer => null()
character(len=max_length)                            :: aux_string
integer                                       :: i,length=0

   call c_f_pointer(c_string_pointer,char_array_pointer,[max_length])
   if (.not.associated(char_array_pointer)) then
     allocate(character(len=4)::f_string)
     f_string="NULL"
     return
   endif
   aux_string=" "
   do i=1,max_length
     if (char_array_pointer(i)==c_null_char) then
       length=i-1
       exit
     endif
     aux_string(i:i)=char_array_pointer(i)
   enddo
   allocate(character(len=length)::f_string)
   f_string=aux_string(1:length)

end function c2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      indent(3f) - [M_strings:WHITESPACE] count number of leading spaces
!!      in a string
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function indent(line)
!!
!!     integer                        :: indent
!!     character(len=*),intent(in)    :: line
!!
!!##DESCRIPTION
!!    Count number of leading spaces in a CHARACTER variable.
!!
!!##EXAMPLES
!!
!!  Sample Program:
!!
!!    program demo_indent
!!    !  test filter to count leading spaces in a character variable
!!    !  might want to call notabs(3f) to expand tab characters
!!    use M_strings, only : indent
!!    implicit none
!!    character(len=1024) :: in
!!    integer             :: ios
!!       READFILE: do
!!          read(*,'(A)',iostat=ios)in
!!          if(ios /= 0) exit READFILE
!!          write(*,'(i3,"",a)')indent(in),trim(in)
!!       enddo READFILE
!!    end program demo_indent
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function indent(line)
implicit none

! ident_28="@(#)M_strings::indent(3f): find number of leading spaces in a string"

integer                        :: indent
character(len=*),intent(in)    :: line
integer                        :: i
   indent=0
   NOTSPACE: block
      SCAN: do i=1,len(line)
         if(line(i:i).ne.' ')then
            indent=i-1
            exit NOTSPACE
         endif
      enddo SCAN
      indent=len(line)
   endblock NOTSPACE
end function indent
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    visible(3f) - [M_strings:NONALPHA] expand a string to control and
!!    meta-control representations
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function visible(input) result(output)
!!
!!     character(len=*),intent(in)           :: input
!!     character(len=:),allocatable          :: output
!!
!!##DESCRIPTION
!!     visible(3f) expands characters to commonly used sequences used
!!     to represent the characters as control sequences or meta-control
!!     sequences.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!     program demo_visible
!!     use M_strings, only : visible
!!     integer :: i
!!        do i=0,255
!!           write(*,'(i0,1x,a)')i,visible(char(i))
!!        enddo
!!     end program demo_visible
!!##BUGS
!!     The expansion is not reversible, as input sequences such as "M-" or
!!     "^a" will look like expanded sequences.
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function visible(input) result(output)
character(len=*),intent(in)  :: input
character(len=:),allocatable :: output

! ident_29="@(#)M_strings::visible(3f) expand escape sequences in a string to control and meta-control representations"

integer                      :: i
character(len=1)             :: c

character(len=*),parameter :: chars(0:255)= [ &
'^@  ', '^A  ', '^B  ', '^C  ', '^D  ', '^E  ', '^F  ', '^G  ', '^H  ', '^I  ', &
'^J  ', '^K  ', '^L  ', '^M  ', '^N  ', '^O  ', '^P  ', '^Q  ', '^R  ', '^S  ', &
'^T  ', '^U  ', '^V  ', '^W  ', '^X  ', '^Y  ', '^Z  ', '^[  ', '^\  ', '^]  ', &
'^^  ', '^_  ', '    ', '!   ', '"   ', '#   ', '$   ', '%   ', '&   ', '''   ', &
'(   ', ')   ', '*   ', '+   ', ',   ', '-   ', '.   ', '/   ', '0   ', '1   ', &
'2   ', '3   ', '4   ', '5   ', '6   ', '7   ', '8   ', '9   ', ':   ', ';   ', &
'<   ', '=   ', '>   ', '?   ', '@   ', 'A   ', 'B   ', 'C   ', 'D   ', 'E   ', &
'F   ', 'G   ', 'H   ', 'I   ', 'J   ', 'K   ', 'L   ', 'M   ', 'N   ', 'O   ', &
'P   ', 'Q   ', 'R   ', 'S   ', 'T   ', 'U   ', 'V   ', 'W   ', 'X   ', 'Y   ', &
'Z   ', '[   ', '\   ', ']   ', '^   ', '_   ', '`   ', 'a   ', 'b   ', 'c   ', &
'd   ', 'e   ', 'f   ', 'g   ', 'h   ', 'i   ', 'j   ', 'k   ', 'l   ', 'm   ', &
'n   ', 'o   ', 'p   ', 'q   ', 'r   ', 's   ', 't   ', 'u   ', 'v   ', 'w   ', &
'x   ', 'y   ', 'z   ', '{   ', '|   ', '}   ', '~   ', '^?  ', 'M-^@', 'M-^A', &
'M-^B', 'M-^C', 'M-^D', 'M-^E', 'M-^F', 'M-^G', 'M-^H', 'M-^I', 'M-^J', 'M-^K', &
'M-^L', 'M-^M', 'M-^N', 'M-^O', 'M-^P', 'M-^Q', 'M-^R', 'M-^S', 'M-^T', 'M-^U', &
'M-^V', 'M-^W', 'M-^X', 'M-^Y', 'M-^Z', 'M-^[', 'M-^\', 'M-^]', 'M-^^', 'M-^_', &
'M-  ', 'M-! ', 'M-" ', 'M-# ', 'M-$ ', 'M-% ', 'M-& ', 'M-'' ', 'M-( ', 'M-) ', &
'M-* ', 'M-+ ', 'M-, ', 'M-- ', 'M-. ', 'M-/ ', 'M-0 ', 'M-1 ', 'M-2 ', 'M-3 ', &
'M-4 ', 'M-5 ', 'M-6 ', 'M-7 ', 'M-8 ', 'M-9 ', 'M-: ', 'M-; ', 'M-< ', 'M-= ', &
'M-> ', 'M-? ', 'M-@ ', 'M-A ', 'M-B ', 'M-C ', 'M-D ', 'M-E ', 'M-F ', 'M-G ', &
'M-H ', 'M-I ', 'M-J ', 'M-K ', 'M-L ', 'M-M ', 'M-N ', 'M-O ', 'M-P ', 'M-Q ', &
'M-R ', 'M-S ', 'M-T ', 'M-U ', 'M-V ', 'M-W ', 'M-X ', 'M-Y ', 'M-Z ', 'M-[ ', &
'M-\ ', 'M-] ', 'M-^ ', 'M-_ ', 'M-` ', 'M-a ', 'M-b ', 'M-c ', 'M-d ', 'M-e ', &
'M-f ', 'M-g ', 'M-h ', 'M-i ', 'M-j ', 'M-k ', 'M-l ', 'M-m ', 'M-n ', 'M-o ', &
'M-p ', 'M-q ', 'M-r ', 'M-s ', 'M-t ', 'M-u ', 'M-v ', 'M-w ', 'M-x ', 'M-y ', &
'M-z ', 'M-{ ', 'M-| ', 'M-} ', 'M-~ ', 'M-^?']
output=''
do i=1,len(input)
   c=input(i:i)
   if(c.eq.' ')then
      output=output//' '
   else
      output=output//trim(chars(ichar(c)))
   endif
enddo
end function visible
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    expand(3f) - [M_strings:NONALPHA] expand C-like escape sequences
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function expand(line,escape) result(lineout)
!!
!!    character(len=*)                      :: line
!!    character(len=1),intent(in),optional  :: escape
!!    character(len=:),allocatable          :: lineout
!!
!!##DESCRIPTION
!!     EXPAND() expands sequences used to represent commonly used escape
!!     sequences or control characters. By default ...
!!
!!     Escape sequences
!!       \      backslash
!!       a      alert (BEL) -- g is an alias for a
!!       b      backspace
!!       c      suppress further output
!!       e      escape
!!       f      form feed
!!       n      new line
!!       r      carriage return
!!       t      horizontal tab
!!       v      vertical tab
!!       oNNN   byte with octal value NNN (3 digits)
!!       dNNN   byte with decimal value NNN (3 digits)
!!       xHH    byte with hexadecimal value HH (2 digits) -- h is an alias for x
!!
!!     The default escape character is the backslash, but this may be
!!     changed using the optional parameter ESCAPE.
!!
!!##EXAMPLES
!!
!!    Sample Program:
!!
!!     program demo_expand
!!     !  test filter to expand escape sequences in input lines
!!     use M_strings, only : expand
!!     character(len=1024) :: line
!!     integer             :: ios
!!        READFILE: block
!!           do
!!              read(*,'(A)',iostat=ios)line
!!              if(ios /= 0) exit READFILE
!!              write(*,'(a)')trim(expand(line))
!!           enddo
!!        endblock READFILE
!!     end program demo_expand
!!
!!    Sample input:
!!
!!      \e[2J
!!      \tABC\tabc
!!      \tA\a
!!      \nONE\nTWO\nTHREE
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function expand(line,escape) result(lineout)
!*!USE ISO_C_BINDING ,ONLY: c_horizontal_tab
implicit none

! ident_30="@(#)M_strings::expand(3f): return string with escape sequences expanded"

character(len=*),parameter            :: c_horizontal_tab=char(9)
character(len=*),intent(in)           :: line
character(len=1),intent(in),optional  :: escape ! escape character. Default is backslash
! expand escape sequences found in input string
! Escape sequences
!    %%      escape character           %a     alert (BEL) -- gi is an alias for a
!    %b      backspace                  %c     suppress further output
!    %e      escape                     %E     escape
!    %f      form feed                  %n     new line
!    %r      carriage return            %t     horizontal tab
!    %v      vertical tab
!    %oNNN   byte with octal value NNN (3 digits)
!    %dNNN   byte with decimal value NNN (3 digits)
!    %xHH    byte with hexadecimal value HH (2 digits) -- h is an alias for x
character(len=1)                      :: esc    ! escape character. Default is %
character(len=:),allocatable          :: lineout
integer                               :: i
integer                               :: ilen
character(len=3)                      :: thr
integer                               :: xxx
integer                               :: ios
   i=0 ! pointer into input

   ilen=len_trim(line)
   lineout=''

   if(ilen.eq.0)return

   if (present(escape))then
      esc=escape
   else
      esc=char(92)
   endif

   EXP: do
      i=i+1
      if(i.gt.ilen)exit
      if(line(i:i).eq.esc)then
         i=i+1
         if(i.gt.ilen)exit
         if(line(i:i).ne.esc)then
            BACKSLASH: select case(line(i:i))
            case('a','A','g','G');lineout=lineout//char(  7) ! %a     alert (BEL)
            case('b','B');lineout=lineout//char(  8)         ! %b     backspace
            case('c','C');exit EXP                           ! %c     suppress further output
            case('d','D')                                    ! %d     Dnnn decimal value
                      thr=line(i+1:)
                   read(thr,'(i3)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+3
            case('e','E');lineout=lineout//char( 27)         ! %e     escape
            case('f','F');lineout=lineout//char( 12)         ! %f     form feed
            case('n','N');lineout=lineout//char( 10)         ! %n     new line
          !!case('n','N');lineout=lineout//new_line('A')     ! %n     new line
            case('o','O')
                      thr=line(i+1:)
                   read(thr,'(o3)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+3
            case('r','R');lineout=lineout//char( 13)         ! %r     carriage return
            case('t','T');lineout=lineout//c_horizontal_tab  ! %t     horizontal tab
            case('v','V');lineout=lineout//char( 11)         ! %v     vertical tab
            case('x','X','h','H')                            ! %x     xHH  byte with hexadecimal value HH (1 to 2 digits)
                      thr=line(i+1:)
                   read(thr,'(z2)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+2
            end select BACKSLASH
         else
            lineout=lineout//esc                             ! escape character, defaults to backslash
         endif
      else
         lineout=lineout//line(i:i)
      endif
      if(i.ge.ilen)exit EXP
   enddo EXP

end function expand
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    notabs(3f) - [M_strings:NONALPHA] expand tab characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine notabs(INSTR,OUTSTR,ILEN)
!!
!!     character(len=*),intent=(in)  :: INSTR
!!     character(len=*),intent=(out) :: OUTSTR
!!     integer,intent=(out)          :: ILEN
!!
!!##DESCRIPTION
!!     NOTABS() converts tabs in INSTR to spaces in OUTSTR while maintaining
!!     columns. It assumes a tab is set every 8 characters. Trailing spaces
!!     are removed.
!!
!!     In addition, trailing carriage returns and line feeds are removed
!!     (they are usually a problem created by going to and from MSWindows).
!!
!!     What are some reasons for removing tab characters from an input line?
!!     Some Fortran compilers have problems with tabs, as tabs are not
!!     part of the Fortran character set. Some editors and printers will
!!     have problems with tabs. It is often useful to expand tabs in input
!!     files to simplify further processing such as tokenizing an input line.
!!
!!##OPTIONS
!!     instr     Input line to remove tabs from
!!
!!##RESULTS
!!     outstr    Output string with tabs expanded.
!!     ilen      Significant length of returned string
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_notabs
!!
!!    !  test filter to remove tabs and trailing white space from input
!!    !  on files up to 1024 characters wide
!!    use M_strings, only : notabs
!!    character(len=1024) :: in,out
!!    integer             :: ios,iout
!!       do
!!          read(*,'(A)',iostat=ios)in
!!          if(ios /= 0) exit
!!          call notabs(in,out,iout)
!!          write(*,'(a)')out(:iout)
!!       enddo
!!    end program demo_notabs
!!
!!##SEE ALSO
!!     GNU/Unix commands expand(1) and unexpand(1)
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine notabs(INSTR,OUTSTR,ILEN)

! ident_31="@(#)M_strings::notabs(3f): convert tabs to spaces while maintaining columns, remove CRLF chars"

CHARACTER(LEN=*),INTENT(IN)   :: instr        ! input line to scan for tab characters
CHARACTER(LEN=*),INTENT(OUT)  :: outstr       ! tab-expanded version of INSTR produced
INTEGER,INTENT(OUT)           :: ilen         ! column position of last character put into output string
                                              ! that is, ILEN holds the position of the last non-blank character in OUTSTR
!===================================================================================================================================
INTEGER,PARAMETER             :: tabsize=8    ! assume a tab stop is set every 8th column
INTEGER                       :: ipos         ! position in OUTSTR to put next character of INSTR
INTEGER                       :: lenin        ! length of input string trimmed of trailing spaces
INTEGER                       :: lenout       ! number of characters output string can hold
INTEGER                       :: istep        ! counter that advances thru input string INSTR one character at a time
CHARACTER(LEN=1)              :: c            ! character in input line being processed
INTEGER                       :: iade         ! ADE (ASCII Decimal Equivalent) of character being tested
!===================================================================================================================================
   IPOS=1                                     ! where to put next character in output string OUTSTR
   lenin=LEN(instr)                           ! length of character variable INSTR
   lenin=LEN_TRIM(instr(1:lenin))             ! length of INSTR trimmed of trailing spaces
   lenout=LEN(outstr)                         ! number of characters output string OUTSTR can hold
   OUTSTR=" "                                 ! this SHOULD blank-fill string, a buggy machine required a loop to set all characters
!===================================================================================================================================
      SCAN_LINE: DO istep=1,lenin             ! look through input string one character at a time
         c=instr(istep:istep)                 ! get next character
         iade=ICHAR(c)                        ! get ADE of the character
         expand_tabs : SELECT CASE (iade)     ! take different actions depending on which character was found
         CASE(9)                              ! test if character is a tab and move pointer out to appropriate column
            ipos = ipos + (tabsize - (MOD(ipos-1,tabsize)))
         CASE(10,13)                          ! convert carriage-return and new-line to space ,typically to handle DOS-format files
            ipos=ipos+1
         CASE DEFAULT                         ! c is anything else other than a tab,newline,or return  insert it in output string
            IF(ipos > lenout)THEN
               CALL journal("*notabs* output string overflow")
               EXIT
            ELSE
               outstr(ipos:ipos)=c
               ipos=ipos+1
            ENDIF
         END SELECT expand_tabs
      enddo SCAN_LINE
!===================================================================================================================================
      ipos=MIN(ipos,lenout)                   ! tabs or newline or return characters or last character might have gone too far
      ilen=LEN_TRIM(outstr(:ipos))            ! trim trailing spaces
!===================================================================================================================================
END SUBROUTINE notabs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dilate(3f) - [M_strings:NONALPHA] expand tab characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function dilate(INSTR) result(OUTSTR)
!!
!!     character(len=*),intent=(in)  :: INSTR
!!     character(len=:),allocatable  :: OUTSTR
!!
!!##DESCRIPTION
!!     dilate() converts tabs in INSTR to spaces in OUTSTR.  It assumes a
!!     tab is set every 8 characters. Trailing spaces are removed.
!!
!!     In addition, trailing carriage returns and line feeds are removed
!!     (they are usually a problem created by going to and from MSWindows).
!!
!!##OPTIONS
!!     instr     Input line to remove tabs from
!!
!!##RESULTS
!!     outstr    Output string with tabs expanded.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_dilate
!!
!!    !  test filter to remove tabs and trailing white space from input
!!    !  on files up to 1024 characters wide
!!    use M_strings, only : dilate
!!    implicit none
!!    character(len=:),allocatable :: in
!!    character(len=:),allocatable :: out
!!    integer                      :: i
!!       in='  this is my string  '
!!       ! change spaces to tabs to make a sample input
!!       do i=1,len(in)
!!          if(in(i:i).eq.' ')in(i:i)=char(9)
!!       enddo
!!       write(*,'(a)')in,dilate(in)
!!    end program demo_dilate
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!    Public Domain
function dilate(INSTR) result(OUTSTR)

! ident_32="@(#)M_strings::dilate(3f): convert tabs to spaces and trims line, removing CRLF chars"

CHARACTER(LEN=*),INTENT(IN)   :: instr        ! input line to scan for tab characters
CHARACTER(LEN=:),allocatable  :: outstr       ! tab-expanded version of INSTR produced
integer                       :: i
integer                       :: icount
integer                       :: ilen
   icount=0
   do i=1,len(instr)
      if(instr(i:i).eq.char(9))icount=icount+1
   enddo
   allocate(character(len=(len(instr)+8*icount)) :: outstr)
   call notabs(instr,outstr,ilen)
   outstr=outstr(:ilen)
!===================================================================================================================================
END function dilate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    adjustc(3f) - [M_strings:WHITESPACE] center text
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   pure function adjustc(string[,length])
!!
!!    character(len=*),intent(in)  :: string
!!    integer,intent(in),optional  :: length
!!    character(len=:),allocatable :: adjustc
!!
!!##DESCRIPTION
!!     Centers input text in a string of the length specified. Returns a
!!     string of length LENGTH if LENGTH is present. Otherwise returns a
!!     string of the length of the input string.
!!
!!##OPTIONS
!!     string  input string to trim and center
!!     length  line length to center text in, optional.
!!
!!##RETURNS
!!     adjustc  centered output string
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_adjustc
!!    use M_strings, only : adjustc
!!    !  using length of the input string
!!       write(*,'(a)')       '================================'
!!       write(*,'(a)')adjustc('centered string                 ')
!!       write(*,'(a)')adjustc('                 centered string')
!!       write(*,'(a)')adjustc('  centered string               ')
!!    !  using explicit output string length
!!       write(*,'(a)')repeat('=',50)
!!       write(*,'(a)')adjustc('this is a centered string',50)
!!       write(*,'(a)')repeat('=',50)
!!    end program demo_adjustc
!!
!!   Expected output
!!
!!    ================================
!!            centered string
!!            centered string
!!            centered string
!!    ==================================================
!!                this is a centered string
!!    ==================================================
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function adjustc(string,length)

! ident_33="@(#)M_strings::adjustc(3f): center text"

!>
!! PROCEDURE   adjustc(3f)
!! DESCRIPTION center text using implicit or explicit length
!!##VERSION     2.0, 20160711
!! AUTHOR      John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: string         ! input string to trim and center
integer,intent(in),optional  :: length         ! line length to center text in
character(len=:),allocatable :: adjustc        ! output string
integer                      :: inlen
integer                      :: ileft          ! left edge of string if it is centered
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(length))then                     ! optional length
      inlen=length                             ! length will be requested length
      if(inlen.le.0)then                       ! bad input length
         inlen=len(string)                     ! could not use input value, fall back to length of input string
      endif
   else                                        ! output length was not explicitly specified, use input string length
      inlen=len(string)
   endif
   allocate(character(len=inlen):: adjustc)    ! create output at requested length
   adjustc(1:inlen)=' '                        ! initialize output string to all blanks
!-----------------------------------------------------------------------------------------------------------------------------------
   ileft =(inlen-len_trim(adjustl(string)))/2  ! find starting point to start input string to center it
   if(ileft.gt.0)then                          ! if string will fit centered in output
      adjustc(ileft+1:inlen)=adjustl(string)   ! center the input text in the output string
   else                                        ! input string will not fit centered in output string
      adjustc(1:inlen)=adjustl(string)         ! copy as much of input to output as can
   endif
end function adjustc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    nospace(3f) - [M_strings:WHITESPACE] remove all whitespace from
!!    input string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function nospace(str) - remove all whitespace from input string
!!
!!     character(len=*),intent(in)          :: str
!!     character(len=:),allocatable         :: nospace
!!
!!##DESCRIPTION
!!    nospace(3f) removes space, tab, carriage return, new line, vertical
!!    tab, formfeed and null characters (called "whitespace"). The output
!!    is returned trimmed.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_nospace
!!     use M_strings, only: nospace
!!     implicit none
!!     character(len=:),allocatable  :: s
!!        s='  This     is      a     test  '
!!        write(*,*) 'original input string is ....',s
!!        write(*,*) 'processed output string is ...',nospace(s)
!!        if(nospace(s).eq.'Thisisatest')then
!!           write(*,*)'nospace test passed'
!!        else
!!           write(*,*)'nospace test error'
!!        endif
!!     end program demo_nospace
!!
!!   Expected output
!!
!!     original input string is ....  This     is      a     test
!!     processed output string is ...Thisisatest
!!     nospace test passed
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function nospace(line)

! ident_34="@(#)M_strings::nospace(3f): remove all whitespace from input string"

character(len=*),intent(in)    ::  line             ! remove whitespace from this string and return it
character(len=:),allocatable   ::  nospace          ! returned string
integer                        ::  ipos             ! position to place next output character at
integer                        ::  i                ! counter to increment from beginning to end of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   allocate(nospace,mold=line)                      ! initially make output line length of input line
   nospace(:len_trim(nospace))=' '
   ipos=0
   do i=1,len_trim(line)                            ! increment from first to last character of the input line
      if ( isspace( line(i:i) ) ) cycle             ! if a blank is encountered skip it
      ipos=ipos+1                                   ! increment count of non-blank characters found
      nospace(ipos:ipos)=line(i:i)                  ! store non-blank character in output
   enddo
   nospace=trim(nospace)                            ! blank out unpacked part of line
end function nospace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    stretch(3f) - [M_strings:LENGTH] return string padded to at least
!!    specified length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function stretch(str,length,pattern,suffix) result(strout)
!!
!!     character(len=*),intent(in)         :: str
!!     integer,intent(in)                  :: length
!!     character(len=*)intent(in),optional :: pattern
!!     character(len=*)intent(in),optional :: suffix
!!     character(len=:),allocatable        :: strout
!!
!!##DESCRIPTION
!!    stretch(3f) pads a string with spaces to at least the specified
!!    length. If the trimmed input string is longer than the requested
!!    length the original string is returned trimmed of trailing spaces.
!!
!!##OPTIONS
!!    str      the input string to return trimmed, but then padded to
!!             the specified length if shorter than length
!!    length   The minimum string length to return
!!    pattern  optional string to use as padding. Defaults to a space.
!!    suffix   optional string to append to output string
!!
!!##RETURNS
!!    strout  The input string padded to the requested length or
!!            the trimmed input string if the input string is
!!            longer than the requested length.
!!
!!##EXAMPLE
!!
!!  Sample Program:
!!
!!   program demo_stretch
!!    use M_strings, only : stretch
!!    implicit none
!!    character(len=10)            :: string='abcdefghij'
!!    character(len=:),allocatable :: answer
!!    integer                      :: i
!!       answer=stretch(string,5)
!!       write(*,'("[",a,"]")') answer
!!       answer=stretch(string,20)
!!       write(*,'("[",a,"]")') answer
!!       i=30
!!       write(*,*)
!!       write(*,'(1x,a,i0)') &
!!        & stretch('CHAPTER 1 : The beginning ',i,'.'), 1    ,&
!!        & stretch('CHAPTER 2 : The end ',i,'.'),       1234 ,&
!!        & stretch('APPENDIX ',i,'.'),                  1235
!!       write(*,*)
!!       write(*,'(1x,a,i7)') &
!!        & stretch('CHAPTER 1 : The beginning ',i,'.'), 1    ,&
!!        & stretch('CHAPTER 2 : The end ',i,'.'),       1234 ,&
!!        & stretch('APPENDIX ',i,'.'),                  1235
!!       write(*,*)
!!       write(*,*) &
!!        & stretch('CHAPTER 1 : The beginning ',i,suffix=': '), 1
!!       write(*,*) &
!!        & stretch('CHAPTER 2 : The end ',i,suffix=': '),1234
!!       write(*,*) &
!!        & stretch('APPENDIX ',i,suffix=': '),           1235
!!   end program demo_stretch
!!
!!   Results:
!!
!!    [abcdefghij]
!!    [abcdefghij          ]
!!
!!     CHAPTER 1 : The beginning ....1
!!     CHAPTER 2 : The end ..........1234
!!     APPENDIX .....................1235
!!
!!     CHAPTER 1 : The beginning ....      1
!!     CHAPTER 2 : The end ..........   1234
!!     APPENDIX .....................   1235
!!
!!     CHAPTER 1 : The beginning     :            1
!!     CHAPTER 2 : The end           :         1234
!!     APPENDIX                      :         1235
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function stretch(line,length,pattern,suffix) result(strout)

! ident_35="@(#)M_strings::stretch(3f): return string padded to at least specified length"

character(len=*),intent(in)                  :: line
integer,intent(in)                           :: length
character(len=*),intent(in),optional         :: pattern
character(len=*),intent(in),optional         :: suffix
!-!character(len=max(length,len(trim(line)))) :: strout
character(len=:),allocatable                 :: strout
   if(present(pattern))then
      strout=atleast(line,length,pattern)
   else
      strout=atleast(line,length)
   endif
   if(present(suffix))then
      strout=strout//suffix
   endif
end function stretch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   atleast(3f) - [M_strings:LENGTH] return string padded to at least
!!   specified length
!!   (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!   function atleast(str,length,pattern) result(strout)
!!
!!    character(len=*)                           :: str
!!    integer,intent(in)                         :: length
!!    character(len=max(length,len(trim(line)))) ::  strout
!!    character(len=*),optional                  ::  pattern
!!
!!##DESCRIPTION
!!   atleast(3f) pads a string with spaces to at least the specified
!!   length. If the trimmed input string is longer than the requested
!!   length the trimmed string is returned.
!!
!!##OPTIONS
!!   str      the input string to return trimmed, but then padded to
!!            the specified length if shorter than length
!!   length   The minimum string length to return
!!   pattern  optional string to use as padding. Defaults to a space.
!!
!!##RETURNS
!!   strout  The input string padded to the requested length or
!!           the trimmed input string if the input string is
!!           longer than the requested length.
!!
!!##EXAMPLE
!!
!!  Sample Program:
!!
!!    program demo_atleast
!!     use M_strings, only : atleast
!!     implicit none
!!     character(len=10)            :: string='abcdefghij'
!!     character(len=:),allocatable :: answer
!!     integer                      :: i
!!        answer=atleast(string,5)
!!        write(*,'("[",a,"]")') answer
!!        answer=atleast(string,20)
!!        write(*,'("[",a,"]")') answer
!!        i=30
!!        write(*,*)
!!        write(*,'(1x,a,1x,i0)') &
!!         & atleast('CHAPTER 1 : The beginning ',i,'.'), 1   , &
!!         & atleast('CHAPTER 2 : The end ',i,'.'),       1234, &
!!         & atleast('APPENDIX ',i,'.'),                  1235
!!        write(*,*)
!!        write(*,'(1x,a,i7)') &
!!         & atleast('CHAPTER 1 : The beginning ',i,'.'), 1   , &
!!         & atleast('CHAPTER 2 : The end ',i,'.'),       1234, &
!!         & atleast('APPENDIX ',i,'.'),                  1235
!!    end program demo_atleast
!!
!!  Results:
!!
!!    [abcdefghij]
!!    [abcdefghij          ]
!!
!!     CHAPTER 1 : The beginning .... 1
!!     CHAPTER 2 : The end .......... 1234
!!     APPENDIX ..................... 1235
!!
!!     CHAPTER 1 : The beginning ....      1
!!     CHAPTER 2 : The end ..........   1234
!!     APPENDIX .....................   1235
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function atleast(line,length,pattern) result(strout)

! ident_36="@(#)M_strings::atleast(3f): return string padded to at least specified length"

character(len=*),intent(in)                :: line
integer,intent(in)                         :: length
character(len=*),intent(in),optional       :: pattern
character(len=max(length,len(trim(line)))) :: strout
if(present(pattern))then
   strout=line//repeat(pattern,len(strout)/len(pattern)+1)
else
   strout=line
endif
end function atleast
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    lenset(3f) - [M_strings:LENGTH] return string trimmed or padded to specified length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function lenset(str,length) result(strout)
!!
!!     character(len=*)                     :: str
!!     character(len=length)                :: strout
!!     integer,intent(in)                   :: length
!!
!!##DESCRIPTION
!!    lenset(3f) truncates a string or pads it with spaces to the specified
!!    length.
!!
!!##OPTIONS
!!    str     input string
!!    length  output string length
!!
!!##RESULTS
!!    strout  output string
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_lenset
!!      use M_strings, only : lenset
!!      implicit none
!!      character(len=10)            :: string='abcdefghij'
!!      character(len=:),allocatable :: answer
!!         answer=lenset(string,5)
!!         write(*,'("[",a,"]")') answer
!!         answer=lenset(string,20)
!!         write(*,'("[",a,"]")') answer
!!     end program demo_lenset
!!
!!    Expected output:
!!
!!     [abcde]
!!     [abcdefghij          ]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function lenset(line,length) result(strout)

! ident_37="@(#)M_strings::lenset(3f): return string trimmed or padded to specified length"

character(len=*),intent(in)  ::  line
integer,intent(in)           ::  length
character(len=length)        ::  strout
   strout=line
end function lenset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    merge_str(3f) - [M_strings:LENGTH] pads strings to same length and
!!    then calls MERGE(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function merge_str(str1,str2,expr) result(strout)
!!
!!     character(len=*),intent(in),optional :: str1
!!     character(len=*),intent(in),optional :: str2
!!     logical,intent(in)              :: expr
!!     character(len=:),allocatable    :: strout
!!
!!##DESCRIPTION
!!    merge_str(3f) pads the shorter of str1 and str2 to the longest length
!!    of str1 and str2 and then calls MERGE(padded_str1,padded_str2,expr).
!!    It trims trailing spaces off the result and returns the trimmed
!!    string. This makes it easier to call MERGE(3f) with strings, as
!!    MERGE(3f) requires the strings to be the same length.
!!
!!    NOTE: STR1 and STR2 are always required even though declared optional.
!!          this is so the call "STR_MERGE(A,B,present(A))" is a valid call.
!!          The parameters STR1 and STR2 when they are optional parameters
!!          can be passed to a procedure if the options are optional on the
!!          called procedure.
!!
!!##OPTIONS
!!    STR1    string to return if the logical expression EXPR is true
!!    STR2    string to return if the logical expression EXPR is false
!!    EXPR    logical expression to evaluate to determine whether to return
!!            STR1 when true, and STR2 when false.
!!##RESULT
!!     MERGE_STR  a trimmed string is returned that is otherwise the value
!!                of STR1 or STR2, depending on the logical expression EXPR.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!     program demo_merge_str
!!     use M_strings, only : merge_str
!!     implicit none
!!     character(len=:), allocatable :: answer
!!        answer=merge_str('first string', &
!!         & 'second string is longer',10.eq.10)
!!        write(*,'("[",a,"]")') answer
!!        answer=merge_str('first string', &
!!         & 'second string is longer',10.ne.10)
!!        write(*,'("[",a,"]")') answer
!!     end program demo_merge_str
!!
!!   Expected output
!!
!!     [first string]
!!     [second string is longer]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function merge_str(str1,str2,expr) result(strout)
! for some reason the MERGE(3f) intrinsic requires the strings it compares to be of equal length
! make an alias for MERGE(3f) that makes the lengths the same before doing the comparison by padding the shorter one with spaces

! ident_38="@(#)M_strings::merge_str(3f): pads first and second arguments to MERGE(3f) to same length"

character(len=*),intent(in),optional :: str1
character(len=*),intent(in),optional :: str2
character(len=:),allocatable         :: str1_local
character(len=:),allocatable         :: str2_local
logical,intent(in)                   :: expr
character(len=:),allocatable         :: strout
integer                              :: big
   if(present(str2))then
      str2_local=str2
   else
      str2_local=''
   endif
   if(present(str1))then
      str1_local=str1
   else
      str1_local=''
   endif
   big=max(len(str1_local),len(str2_local))
   ! note: perhaps it would be better to warn or fail if an optional value that is not present is returned, instead of returning ''
   strout=trim(merge(lenset(str1_local,big),lenset(str2_local,big),expr))
end function merge_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    compact(3f) - [M_strings:WHITESPACE] converts contiguous whitespace
!!    to a single character (or nothing)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function compact(STR,CHAR) result (OUTSTR)
!!
!!     character(len=*),intent(in)          :: STR
!!     character(len=*),intent(in),optional :: CHAR
!!     character(len=len(str))              :: OUTSTR
!!
!!##DESCRIPTION
!!    COMPACT(3f) converts multiple spaces, tabs and control characters
!!    (called "whitespace") to a single character or nothing. Leading
!!    whitespace is removed.
!!
!!##OPTIONS
!!    STR     input string to reduce or remove whitespace from
!!    CHAR    By default the character that replaces adjacent
!!            whitespace is a space. If the optional CHAR parameter is supplied
!!            it will be used to replace the whitespace. If a null character is
!!            supplied for CHAR whitespace is removed.
!!
!!##RETURNS
!!    OUTSTR  string of same length as input string but with all contiguous
!!            whitespace reduced to a single space and leading whitespace
!!            removed
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_compact
!!     use M_strings, only : compact
!!     implicit none
!!     ! produces 'This is a test               '
!!     write(*,*)compact('  This     is      a     test  ')
!!     ! produces 'Thisisatest                  '
!!     write(*,*)compact('  This     is      a     test  ',char='')
!!     ! produces 'This:is:a:test               '
!!     write(*,*)compact('  This     is      a     test  ',char=':')
!!     ! note CHAR is used to replace the whitespace, but if CHAR is
!!     ! in the original string it is just copied
!!     write(*,*)compact('A  AA    A   AAAAA',char='A')
!!     ! produces (original A characters are left as-is) 'AAAAAAAAAAAA'
!!     ! not 'A'
!!    end program demo_compact
!!
!!    Expected output
!!
!!     >This is a test
!!     >Thisisatest
!!     >This:is:a:test
!!     >AAAAAAAAAAAA
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!elemental pure function compact(str,char) result (outstr)
function compact(str,char) result (outstr)

! ident_39="@(#)M_strings::compact(3f): Converts white-space to single spaces"

character(len=*),intent(in)          :: str
character(len=*),intent(in),optional :: char
character(len=len(str))              :: outstr
character(len=1)                     :: ch
integer                              :: i
integer                              :: position_in_output
logical                              :: last_was_space
character(len=1)                     :: char_p
logical                              :: nospace
if(present(char))then
   char_p=char
   if(len(char).eq.0)then
      nospace=.true.
   else
      nospace=.false.
   endif
else
   char_p=' '
   nospace=.false.
endif
   outstr=' '
   last_was_space=.false.
   position_in_output=0

   IFSPACE: do i=1,len_trim(str)
     ch=str(i:i)
     select case(ichar(ch))
       case(0:32,127)                                         ! space or tab character or control character
         if(position_in_output.eq.0)then                      ! still at beginning so ignore leading whitespace
            cycle IFSPACE
         elseif(.not.last_was_space) then                     ! if have not already put out a space output one
           if(.not.nospace)then
              position_in_output=position_in_output+1
              outstr(position_in_output:position_in_output)=char_p
           endif
         endif
         last_was_space=.true.
       case(:-1,33:126,128:)                                  ! not a space, quote, or control character so copy it
         position_in_output=position_in_output+1
         outstr(position_in_output:position_in_output)=ch
         last_was_space=.false.
     end select
   enddo IFSPACE

end function compact
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     noesc(3f) - [M_strings:NONALPHA] convert non-printable characters
!!     to a space
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental function noesc(INSTR)
!!
!!     character(len=*),intent(in) :: INSTR
!!     character(len=len(instr))   :: noesc
!!
!!##DESCRIPTION
!!      Convert non-printable characters to a space.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_noesc
!!
!!     use M_strings, only : noesc
!!     implicit none
!!     character(len=128) :: ascii
!!     character(len=128) :: cleared
!!     integer            :: i
!!     ! fill variable with base ASCII character set
!!     do i=1,128
!!        ascii(i:i)=char(i-1)
!!     enddo
!!     cleared=noesc(ascii)
!!     write(*,*)'characters and their ADE (ASCII Decimal Equivalent)'
!!     call ade(ascii)
!!     write(*,*)'Cleared of non-printable characters'
!!     call ade(cleared)
!!     write(*,*)'Cleared string:'
!!     write(*,*)cleared
!!     contains
!!       subroutine ade(string)
!!       implicit none
!!       ! the string to print
!!       character(len=*),intent(in) :: string
!!       ! number of characters in string to print
!!       integer :: ilen
!!       ! counter used to step thru string
!!       integer :: i
!!          ! get trimmed length of input string
!!          ilen=len_trim(string(:len(string)))
!!
!!          ! replace lower unprintable characters with spaces
!!          write(*,101)(merge(string(i:i),' ',&
!!          & ichar(string(i:i)).ge.32         &
!!          & .and.                            &
!!          & ichar(string(i:i)).le.126)       &
!!          & ,i=1,ilen)
!!
!!          ! print ADE value of character underneath it
!!          write(*,202)     (ichar(string(i:i))/100,    i=1,ilen)
!!          write(*,202)(mod( ichar(string(i:i)),100)/10,i=1,ilen)
!!          write(*,202)(mod((ichar(string(i:i))),10),   i=1,ilen)
!!       ! format for printing string characters
!!       101   format(*(a1:))
!!       ! format for printing ADE values
!!       202   format(*(i1:))
!!       end subroutine ade
!!     end program demo_noesc
!!
!!    Expected output
!!
!!    The string is printed with the ADE value vertically beneath.
!!    The original string has all the ADEs from 000 to 127. After
!!    NOESC(3f) is called on the string all the "non-printable"
!!    characters are replaced with a space (ADE of 032).
!!
!!   characters and their ADE (ASCII Decimal Equivalent)
!!
!!    >                                 !"#$%&'()*+,-./0123456789
!!    :;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!    >0000000000000000000000000000000000000000000000000000000000
!!    0000000000000000000000000000000000000000001111111111111111111111111111
!!    >00000000001111111111222222222233333333334444444444555555555566666666
!!    667777777777888888888899999999990000000000111111111122222222
!!    >012345678901234567890123456789012345678901234567890123456789012345678
!!    90123456789012345678901234567890123456789012345678901234567
!!
!!   Cleared of non-printable characters
!!
!!    >                                 !"#$%&'()*+,-./0123456789
!!    :;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!    >0000000000000000000000000000000000000000000000000000000000
!!    000000000000000000000000000000000000000000111111111111111111111111111
!!    >3333333333333333333333333333333333333333444444444455555555
!!    556666666666777777777788888888889999999999000000000011111111112222222
!!    >2222222222222222222222222222222223456789012345678901234567
!!    890123456789012345678901234567890123456789012345678901234567890123456
!!
!!   Cleared string:
!!
!!    >                                  !"#$%&'()*+,-./0123456789:;<=>?@
!!    ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental function noesc(INSTR)

! ident_40="@(#)M_strings::noesc(3f): convert non-printable characters to a space"

character(len=*),intent(in) :: INSTR      ! string that might contain nonprintable characters
character(len=len(instr))   :: noesc
integer                     :: ic,i10
!-----------------------------------------------------------------------------------------------------------------------------------
   noesc=''                               ! initialize output string
   do i10=1,len_trim(INSTR(1:len(INSTR)))
      ic=ichar(INSTR(i10:i10))
      if(ic.le.31.or.ic.eq.127)then       ! find characters with ADE of 0-31, 127
         noesc(I10:I10)=' '               ! replace non-printable characters with a space
      else
         noesc(I10:I10)=INSTR(i10:i10)    ! copy other characters as-is from input string to output string
      endif
   enddo
end function noesc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      string_to_value(3f) - [M_strings:NUMERIC] subroutine returns numeric
!!      value from string
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine string_to_value(chars,valu,ierr)
!!
!!     character(len=*),intent(in)              :: chars   ! input string
!!     integer|real|doubleprecision,intent(out) :: valu
!!     integer,intent(out)                      :: ierr
!!
!!##DESCRIPTION
!!    Returns a numeric value from a numeric character string.
!!
!!    Works with any g-format input, including integer, real, and
!!    exponential. If the input string begins with "B", "Z", or "O"
!!    and otherwise represents a positive whole number it is assumed to
!!    be a binary, hexadecimal, or octal value. If the string contains
!!    commas they are removed. If the string is of the form NN:MMM... or
!!    NN#MMM then NN is assumed to be the base of the whole number.
!!
!!    If an error occurs in the READ, IOSTAT is returned in IERR and
!!    value is set to zero. if no error occurs, IERR=0.
!!
!!##OPTIONS
!!       CHARS  input string to read numeric value from
!!
!!##RETURNS
!!    VALU   numeric value returned. May be INTEGER, REAL, or
!!              DOUBLEPRECISION.
!!    IERR   error flag (0 == no error)
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_string_to_value
!!     use M_strings, only: string_to_value
!!     implicit none
!!     real :: value
!!     integer :: ierr
!!     character(len=80) :: string
!!        string=' -40.5e-2 '
!!        call string_to_value(string,value,ierr)
!!        write(*,*) 'value of string ['//trim(string)//'] is ',value
!!    end program demo_string_to_value
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine a2r(chars,valu,ierr)

! ident_41="@(#)M_strings::a2r(3fp): subroutine returns real value from string"

character(len=*),intent(in) :: chars                      ! input string
real,intent(out)            :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(ierr.eq.0)then
      if(valu8.le.huge(valu))then
         valu=real(valu8)
      else
         call journal('sc','*a2r*','- value too large',valu8,'>',huge(valu))
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2r
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2i(chars,valu,ierr)

! ident_42="@(#)M_strings::a2i(3fp): subroutine returns integer value from string"

character(len=*),intent(in) :: chars                      ! input string
integer,intent(out)         :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(valu8.le.huge(valu))then
      if(valu8.le.huge(valu))then
         valu=int(valu8)
      else
         call journal('sc','*a2i*','- value too large',valu8,'>',huge(valu))
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2i
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2d(chars,valu,ierr,onerr)

! ident_43="@(#)M_strings::a2d(3fp): subroutine returns double value from string"

!     1989,2016 John S. Urban.
!
!  o works with any g-format input, including integer, real, and exponential.
!  o if an error occurs in the read, iostat is returned in ierr and value is set to zero. If no error occurs, ierr=0.
!  o if the string happens to be 'eod' no error message is produced so this string may be used to act as an end-of-data.
!    IERR will still be non-zero in this case.
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: chars                        ! input string
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu                         ! value read from input string
integer,intent(out)          :: ierr                         ! error flag (0 == no error)
class(*),optional,intent(in) :: onerr
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"     ! format used to build frmt
character(len=15)            :: frmt                         ! holds format built to read input string
character(len=256)           :: msg                          ! hold message from I/O errors
integer                      :: intg
integer                      :: pnd
integer                      :: basevalue, ivalu
character(len=3),save        :: nan_string='NaN'
!----------------------------------------------------------------------------------------------------------------------------------
   ierr=0                                                       ! initialize error flag to zero
   local_chars=unquote(chars)
   msg=''
   if(len(local_chars).eq.0)local_chars=' '
   call substitute(local_chars,',','')                          ! remove any comma characters
   pnd=scan(local_chars,'#:')
   if(pnd.ne.0)then
      write(frmt,fmt)pnd-1                                      ! build format of form '(BN,Gn.0)'
      read(local_chars(:pnd-1),fmt=frmt,iostat=ierr,iomsg=msg)basevalue   ! try to read value from string
      if(decodebase(local_chars(pnd+1:),basevalue,ivalu))then
         valu=real(ivalu,kind=kind(0.0d0))
      else
         valu=0.0d0
         ierr=-1
      endif
   else
      select case(local_chars(1:1))
      case('z','Z','h','H')                                     ! assume hexadecimal
         frmt='(Z'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('b','B')                                             ! assume binary (base 2)
         frmt='(B'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('o','O')                                             ! assume octal
         frmt='(O'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case default
         write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
         read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
      end select
   endif
   if(ierr.ne.0)then                                            ! if an error occurred ierr will be non-zero.
      if(present(onerr))then
         select type(onerr)
         type is (integer)
            valu=onerr
         type is (real)
            valu=onerr
         type is (doubleprecision)
            valu=onerr
         end select
      else                                                      ! set return value to NaN
         read(nan_string,'(g3.3)')valu
      endif
      if(local_chars.ne.'eod')then                           ! print warning message except for special value "eod"
         call journal('sc','*a2d* - cannot produce number from string ['//trim(chars)//']')
         if(msg.ne.'')then
            call journal('sc','*a2d* - ['//trim(msg)//']')
         endif
      endif
   endif
end subroutine a2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    s2v(3f) - [M_strings:NUMERIC] function returns doubleprecision
!!    numeric value from a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function s2v(string[,ierr][,onerr])
!!
!!     character(len=*)             :: string
!!     doubleprecision              :: s2v
!!     integer,intent(out),optional :: ierr
!!     class(*),intent(in),optional :: onerr
!!
!!##DESCRIPTION
!!    This function converts a string to a DOUBLEPRECISION numeric value.
!!
!!    The intrinsics INT(3f), REAL(3f), and DBLE(3f) are also extended
!!    to take CHARACTER variables. The KIND= keyword is not supported
!!    on the extensions.
!!
!!##OPTIONS
!!
!!     string   holds string assumed to represent a numeric value
!!     ierr     If an error occurs the program is stopped if the optional
!!              parameter IERR is not present. If IERR returns a non-zero
!!              value an error occurred.
!!     onerr    The value to return on error. A value of NaN is
!!              returned on error by default.
!!
!!##RETURNS
!!     s2v      numeric value read from string
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_s2v
!!
!!     use M_strings, only: s2v, int, real, dble
!!     implicit none
!!     character(len=8)              :: s=' 10.345 '
!!     integer                       :: i
!!     character(len=14),allocatable :: strings(:)
!!     doubleprecision               :: dv
!!     integer                       :: errnum
!!
!!     ! different strings representing INTEGER, REAL, and DOUBLEPRECISION
!!     strings=[&
!!     &' 10.345       ',&
!!     &'+10           ',&
!!     &'    -3        ',&
!!     &'    -4.94e-2  ',&
!!     &'0.1           ',&
!!     &'12345.678910d0',&
!!     &'              ',& ! Note: will return zero without an error message
!!     &'1 2 1 2 1 . 0 ',& ! Note: spaces will be ignored
!!     &'WHAT?         ']  ! Note: error messages will appear, zero returned
!!
!!     ! a numeric value is returned,
!!     ! so it can be used in numeric expression
!!     write(*,*) '1/2 value of string is ',s2v(s)/2.0d0
!!     write(*,*)
!!     write(*,*)' STRING            VALUE                    ERROR_NUMBER'
!!     do i=1,size(strings)
!!        ! Note: not a good idea to use s2v(3f) in a WRITE(3f) statement,
!!        ! as it does I/O when errors occur, so called on a separate line
!!        dv=s2v(strings(i),errnum)
!!        write(*,*) strings(i)//'=',dv,errnum
!!     enddo
!!     write(*,*)"Extended intrinsics"
!!     write(*,*)'given inputs:',s,strings(:8)
!!     write(*,*)'INT(3f):',int(s),int(strings(:8))
!!     write(*,*)'REAL(3f):',real(s),real(strings(:8))
!!     write(*,*)'DBLE(3f):',dble(s),dble(strings(:8))
!!     write(*,*)"That's all folks!"
!!
!!     end program demo_s2v
!!
!!    Expected output
!!
!!     >1/2 value of string is    5.1725000000000003
!!     >
!!     > STRING            VALUE                    ERROR_NUMBER
!!     > 10.345       =   10.345000000000001                0
!!     >+10           =   10.000000000000000                0
!!     >    -3        =  -3.0000000000000000                0
!!     >    -4.94e-2  =  -4.9399999999999999E-002           0
!!     >0.1           =  0.10000000000000001                0
!!     >12345.678910d0=   12345.678910000001                0
!!     >              =   0.0000000000000000                0
!!     >1 2 1 2 1 . 0 =   12121.000000000000                0
!!     >*a2d* - cannot produce number from string [WHAT?]
!!     >*a2d* - [Bad value during floating point read]
!!     >WHAT?         =   0.0000000000000000             5010
!!     >Extended intrinsics
!!     >given inputs: 10.345 10.345 +10 -3 -4.94e-2 0.1
!!     12345.678910d0 1 2 1 2 1 . 0
!!     >INT(3f): 10 10 10 -3 0 0 12345 0 12121
!!     >REAL(3f): 10.3450003 10.3450003 10.0000000 -3.00000000
!!     -4.94000018E-02
!!     >          0.100000001 12345.6787 0.00000000 12121.0000
!!     >DBLE(3f): 10.345000000000001 10.345000000000001
!!     10.000000000000000
!!     >          -3.0000000000000000 -4.9399999999999999E-002
!!     0.10000000000000001
!!     >          12345.678910000001 0.0000000000000000
!!     12121.000000000000
!!     >That's all folks!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!>
!!##PROCEDURE:
!! DESCRIPTION: s2v(3f): function returns doubleprecision number from string;zero if error occurs
!!##VERSION:     2.0, 20160704
!! AUTHOR:      John S. Urban
doubleprecision function s2v(chars,ierr,onerr)
!  1989 John S. Urban

! ident_44="@(#)M_strings::s2v(3f): returns doubleprecision number from string"


character(len=*),intent(in)  :: chars
integer,optional             :: ierr
doubleprecision              :: valu
integer                      :: ierr_local
class(*),intent(in),optional :: onerr

   ierr_local=0
   if(present(onerr))then
      call a2d(chars,valu,ierr_local,onerr)
   else
      call a2d(chars,valu,ierr_local)
   endif
   if(present(ierr))then ! if error is not returned stop program on error
      ierr=ierr_local
      s2v=valu
   elseif(ierr_local.ne.0)then
      write(*,*)'*s2v* stopped while reading '//trim(chars)
      stop 1
   else
      s2v=valu
   endif
end function s2v
!===================================================================================================================================
! calls to s2v(3f) for extending intrinsics int(3f), real(3f), dble(3f)
!===================================================================================================================================
doubleprecision function dble_s2v(chars)
character(len=*),intent(in) :: chars
   dble_s2v=s2v(chars)
end function dble_s2v
!===================================================================================================================================
real function real_s2v(chars)
character(len=*),intent(in) :: chars
   real_s2v=real(s2v(chars))
end function real_s2v
!===================================================================================================================================
integer function int_s2v(chars)
character(len=*),intent(in) :: chars
   int_s2v=int(s2v(chars))
end function int_s2v
!===================================================================================================================================
function ints_s2v(chars)
integer,allocatable         :: ints_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(ints_s2v(isize))
   do i=1,isize
      ints_s2v(i)=int(s2v(chars(i)))
   enddo
end function ints_s2v
!===================================================================================================================================
function reals_s2v(chars)
real,allocatable            :: reals_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(reals_s2v(isize))
   do i=1,isize
      reals_s2v(i)=real(s2v(chars(i)))
   enddo
end function reals_s2v
!===================================================================================================================================
function dbles_s2v(chars)
doubleprecision,allocatable :: dbles_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(dbles_s2v(isize))
   do i=1,isize
      dbles_s2v(i)=s2v(chars(i))
   enddo
end function dbles_s2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()())()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      value_to_string(3f) - [M_strings:NUMERIC] return numeric string
!!      from a numeric value
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine value_to_string(value,chars[,ilen,ierr,fmt,trimz])
!!
!!     character(len=*) :: chars  ! minimum of 23 characters required
!!     !--------
!!     ! VALUE may be any <em>one</em> of the following types:
!!     doubleprecision,intent(in)               :: value
!!     real,intent(in)                          :: value
!!     integer,intent(in)                       :: value
!!     logical,intent(in)                       :: value
!!     !--------
!!     character(len=*),intent(out)             :: chars
!!     integer,intent(out),optional             :: ilen
!!     integer,optional                         :: ierr
!!     character(len=*),intent(in),optional     :: fmt
!!     logical,intent(in)                       :: trimz
!!
!!##DESCRIPTION
!!    value_to_string(3f) returns a numeric representation of a numeric
!!    value in a string given a numeric value of type REAL, DOUBLEPRECISION,
!!    INTEGER or LOGICAL. It creates the string using internal writes. It
!!    then removes trailing zeros from non-zero values, and left-justifies
!!    the string.
!!
!!##OPTIONS
!!       VALUE   input value to be converted to a string
!!       FMT     You may specify a specific format that produces a string
!!               up to the length of CHARS; optional.
!!       TRIMZ   If a format is supplied the default is not to try to trim
!!               trailing zeros. Set TRIMZ to .true. to trim zeros from a
!!               string assumed to represent a simple numeric value.
!!
!!##RETURNS
!!       CHARS   returned string representing input value, must be at least
!!               23 characters long; or what is required by optional FMT
!!               if longer.
!!       ILEN    position of last non-blank character in returned string;
!!               optional.
!!       IERR    If not zero, error occurred; optional.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!      program demo_value_to_string
!!      use M_strings, only: value_to_string
!!      implicit none
!!      character(len=80) :: string
!!      integer           :: ilen
!!         call value_to_string(3.0/4.0,string,ilen)
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string(3.0/4.0,string,ilen,fmt='')
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string&
!!         &(3.0/4.0,string,ilen,fmt='("THE VALUE IS ",g0)')
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string(1234,string,ilen)
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string(1.0d0/3.0d0,string,ilen)
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!      end program demo_value_to_string
!!
!!    Expected output
!!
!!     The value is [0.75]
!!     The value is [      0.7500000000]
!!     The value is [THE VALUE IS .750000000]
!!     The value is [1234]
!!     The value is [0.33333333333333331]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine value_to_string(gval,chars,length,err,fmt,trimz)

! ident_45="@(#)M_strings::value_to_string(3fp): subroutine returns a string from a value"

class(*),intent(in)                      :: gval
character(len=*),intent(out)             :: chars
integer,intent(out),optional             :: length
integer,optional                         :: err
integer                                  :: err_local
character(len=*),optional,intent(in)     :: fmt         ! format to write value with
logical,intent(in),optional              :: trimz
character(len=:),allocatable             :: fmt_local
character(len=1024)                      :: msg

!  Notice that the value GVAL can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,LOGICAL)

   if (present(fmt)) then
      select type(gval)
      type is (integer)
         fmt_local='(i0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (real)
         fmt_local='(bz,g23.10e3)'
         fmt_local='(bz,g0.8)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         fmt_local='(bz,g0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (logical)
         fmt_local='(l1)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      class default
         call journal('*value_to_string* UNKNOWN TYPE')
         chars=' '
      end select
      if(fmt.eq.'') then
         chars=adjustl(chars)
         call trimzeros_(chars)
      endif
   else                                                  ! no explicit format option present
      err_local=-1
      select type(gval)
      type is (integer)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (real)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (logical)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      class default
         chars=''
      end select
      chars=adjustl(chars)
      if(index(chars,'.').ne.0) call trimzeros_(chars)
   endif
   if(present(trimz))then
      if(trimz)then
         chars=adjustl(chars)
         call trimzeros_(chars)
      endif
   endif

   if(present(length)) then
      length=len_trim(chars)
   endif

   if(present(err)) then
      err=err_local
   elseif(err_local.ne.0)then
      !! cannot currently do I/O from a function being called from I/O
      !!write(ERROR_UNIT,'(a)')'*value_to_string* WARNING:['//trim(msg)//']'
      chars=chars//' *value_to_string* WARNING:['//trim(msg)//']'
   endif

end subroutine value_to_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      v2s(3f) - [M_strings:NUMERIC] return numeric string from a numeric value
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       function v2s(value) result(outstr)
!!
!!        integer|real|doubleprecision|logical,intent(in ) :: value
!!        character(len=:),allocatable :: outstr
!!        character(len=*),optional,intent(in) :: fmt
!!
!!##DESCRIPTION
!!    v2s(3f) returns a representation of a numeric value as a
!!    string when given a numeric value of type REAL, DOUBLEPRECISION,
!!    INTEGER or LOGICAL. It creates the strings using internal WRITE()
!!    statements. Trailing zeros are removed from non-zero values, and the
!!    string is left-justified.
!!
!!##OPTIONS
!!    VALUE   input value to be converted to a string
!!    FMT     format can be explicitly given, but is limited to
!!            generating a string of eighty or less characters.
!!
!!##RETURNS
!!    OUTSTR  returned string representing input value,
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_v2s
!!    use M_strings, only: v2s
!!    write(*,*) 'The value of 3.0/4.0 is ['//v2s(3.0/4.0)//']'
!!    write(*,*) 'The value of 1234    is ['//v2s(1234)//']'
!!    write(*,*) 'The value of 0d0     is ['//v2s(0d0)//']'
!!    write(*,*) 'The value of .false. is ['//v2s(.false.)//']'
!!    write(*,*) 'The value of .true. is  ['//v2s(.true.)//']'
!!    end program demo_v2s
!!
!!   Expected output
!!
!!     The value of 3.0/4.0 is [0.75]
!!     The value of 1234    is [1234]
!!     The value of 0d0     is [0]
!!     The value of .false. is [F]
!!     The value of .true. is  [T]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
! very odd compiler problems in many (but not all) programs using this routine; GNU Fortran (GCC) 5.4.0; 20161030
function v2s_bug(gval) result(outstr)

! ident_46="@(#)M_strings::v2s_bug(3f): function returns string given numeric value"

class(*),intent(in)          :: gval                         ! input value to convert to a string
character(len=:),allocatable :: outstr                       ! output string to generate
character(len=80)            :: string
   call value_to_string(gval,string)
   outstr=trim(string)
end function v2s_bug
!===================================================================================================================================
function d2s(dvalue,fmt) result(outstr)

! ident_47="@(#)M_strings::d2s(3fp): private function returns string given doubleprecision value"

doubleprecision,intent(in)   :: dvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(dvalue,string,fmt=fmt)
   else
      call value_to_string(dvalue,string)
   endif
   outstr=trim(string)
end function d2s
!===================================================================================================================================
function r2s(rvalue,fmt) result(outstr)

! ident_48="@(#)M_strings::r2s(3fp): private function returns string given real value"

real,intent(in)              :: rvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(rvalue,string,fmt=fmt)
   else
      call value_to_string(rvalue,string)
   endif
   outstr=trim(string)
end function r2s
!===================================================================================================================================
function i2s(ivalue,fmt) result(outstr)

! ident_49="@(#)M_strings::i2s(3fp): private function returns string given integer value"

integer,intent(in)           :: ivalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(ivalue,string,fmt=fmt)
   else
      call value_to_string(ivalue,string)
   endif
   outstr=trim(string)
end function i2s
!===================================================================================================================================
function l2s(lvalue,fmt) result(outstr)

! ident_50="@(#)M_strings::l2s(3fp): private function returns string given logical value"

logical,intent(in)           :: lvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)             :: string
   if(present(fmt))then
      call value_to_string(lvalue,string,fmt=fmt)
   else
      call value_to_string(lvalue,string)
   endif
   outstr=trim(string)
end function l2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    isnumber(3f) - [M_strings:NUMERIC] determine if a string represents a number
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function isnumber(str,msg)
!!
!!     character(len=*),intent(in)  :: str
!!     character(len=:),intent(out),allocatable,optional  :: msg
!!
!!##DESCRIPTION
!!     ISNUMBER(3f) returns a value greater than zero if the string represents
!!     a number, and a number less than or equal to zero if it is a bad number.
!!     Blank characters are ignored.
!!
!!##OPTIONS
!!     str  the string to evaluate as to whether it represents a numeric value
!!          or not
!!     msg  An optional message describing the string
!!
!!##RETURNS
!!     isnumber  the following values are returned
!!
!!                1 for an integer             [-+]NNNNN
!!                2 for a whole number         [-+]NNNNN.
!!                3 for a real value           [-+]NNNNN.MMMM
!!                4 for a exponential value    [-+]NNNNN.MMMM[-+]LLLL
!!                                             [-+]NNNNN.MMMM[ed][-+]LLLL
!!
!!               values less than 1 represent an error
!!
!!##EXAMPLES
!!
!!   As the example shows, you can use an internal READ(3f) along with the
!!   IOSTAT= parameter to check (and read) a string as well.
!!
!!     program demo_isnumber
!!     use M_strings, only : isnumber
!!     implicit none
!!     character(len=256) :: line
!!     real               :: value
!!     integer            :: ios
!!     integer            :: answer
!!     character(len=256) :: message
!!     character(len=:),allocatable :: description
!!        write(*,*)'Begin entering values, one per line'
!!        do
!!           read(*,'(a)',iostat=ios)line
!!           !
!!           ! try string as number using list-directed input
!!           line=''
!!           read(line,*,iostat=ios,iomsg=message) value
!!           if(ios.eq.0)then
!!              write(*,*)'VALUE=',value
!!           elseif( is_iostat_end(ios) ) then
!!              stop 'end of file'
!!           else
!!              write(*,*)'ERROR:',ios,trim(message)
!!           endif
!!           !
!!           ! try string using isnumber(3f)
!!           answer=isnumber(line,msg=description)
!!           if(answer.gt.0)then
!!              write(*,*) &
!!              & ' for ',trim(line),' ',answer,':',description
!!           else
!!              write(*,*) &
!!              & ' ERROR for ',trim(line),' ',answer,':',description
!!           endif
!!           !
!!        enddo
!!     end program demo_isnumber
!!
!!  Example run
!!
!!    > Begin entering values
!!    > ERROR:          -1 End of file
!!    >  ERROR for            -1 :null string
!!    >10
!!    > VALUE=   10.0000000
!!    >  for 10            1 :integer
!!    >20
!!    > VALUE=   20.0000000
!!    >  for 20            1 :integer
!!    >20.
!!    > VALUE=   20.0000000
!!    >  for 20.            2 :whole number
!!    >30.1
!!    > VALUE=   30.1000004
!!    >  for 30.1            3 :real number
!!    >3e1
!!    > VALUE=   30.0000000
!!    >  for 3e1            4 :value with exponent
!!    >1-2
!!    > VALUE=   9.99999978E-03
!!    >  for 1-2            4 :value with exponent
!!    >100.22d-4
!!    > VALUE=   1.00220004E-02
!!    >  for 100.22d-4            4 :value with exponent
!!    >1--2
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1--2           -5 :bad number
!!    >e
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for e           -6 :missing leading value before exponent
!!    >e1
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for e1           -6 :missing leading value before exponent
!!    >1e
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1e           -3 :missing exponent
!!    >1e+
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1e+           -4 :missing exponent after sign
!!    >1e+2.0
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1e+2.0           -5 :bad number
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function isNumber(string,msg,verbose)
implicit none

! ident_51="@(#)M_strings::isnumber(3f): Determines if a string is a number of not."

character(len=*),intent(in)    :: string
character(len=:),intent(out),allocatable,optional :: msg
logical,intent(in),optional                      :: verbose
integer                      :: isnumber

integer             :: i,iend
character(len=1),allocatable :: z(:)
character(len=:),allocatable :: message
logical                      :: founddigit
logical                      :: verbose_local

   i=1
   founddigit=.false.
   isnumber=0
   z=switch(trim(nospace(string)))
   iend=size(z)
   message='not a number'
   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif
   DONE : block
      if(iend.eq.0)then
         isnumber=-1                   ! string is null
         message='null string'
         exit DONE
      endif

      if(index('+-',z(i)).ne.0) i=i+1  ! skip optional leading sign
      if(i.gt.iend)then
         isnumber=-2                   ! string was just a sign
         message='just a sign'
         exit DONE
      endif

      call next()                      ! position I to next non-digit or end of string+1

      if(i.gt.iend)then
         isnumber=1                    ! [+-]NNNNNN
         message='integer'
         exit DONE
      endif
      if(z(i).eq.'.')then              ! a period would be OK at this point
         i=i+1
      endif

      if(i.gt.iend)then                ! [+-]NNNNNN.
         isnumber=2
         message='whole number'
         exit DONE
      endif

      call next()                      ! position I to next non-digit or end of string+1
      if(i.gt.iend)then
         isnumber=3                    ! [+-]NNNNNN.MMMM
         message='real number'
         exit DONE
      endif

      if(index('eEdD',z(i)).ne.0)then
         i=i+1
         if(i.eq.2)then
            isnumber=-6                   ! [+-]NNNNNN[.[MMMM]]e but a value must follow
            message='missing leading value before exponent'
            exit DONE
         endif
      endif
      if(i.gt.iend)then
         isnumber=-3                   ! [+-]NNNNNN[.[MMMM]]e but a value must follow
         message='missing exponent'
         exit DONE
      endif
      if(.not.founddigit)then
         isnumber=-7
         message='missing value before exponent'
         exit DONE
      endif
      if(index('+-',z(i)).ne.0) i=i+1
      if(i.gt.iend)then
         isnumber=-4                   ! [+-]NNNNNN[.[MMMM]]e[+-] but a value must follow
         message='missing exponent after sign'
         exit DONE
      endif
      call next()                      ! position I to next non-digit or end of string+1
      if(i.gt.iend)then
         isnumber=4                    ! [+-]NNNNNN.MMMMe[+-]LL
         message='value with exponent'
         exit DONE
      endif
      isnumber=-5
      message='bad number'
   endblock DONE
   if(verbose_local)then
      write(*,*)trim(string)//' is '//message
   endif
   if(present(msg))then
      msg=message
   endif

contains
   subroutine next() ! move to next non-digit or end of string+1
      integer :: j
      do j=i,iend
         if(.not.isdigit(z(j)))then
            exit
         endif
         founddigit=.true.
         if(verbose_local) write(*,*)'I=',i,' J=',j,' Z(j)=',z(j)
      enddo
      i=j
      if(verbose_local)then
         write(*,*)'I and J=',i
         if(i.le.iend) then
            write(*,*)'Z(I)=',z(i)
         else
            write(*,*)'====>'
         endif
      endif
   end subroutine next
end function isNumber
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    trimzeros_(3fp) - [M_strings:NUMERIC] Delete trailing zeros from
!!    numeric decimal string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine trimzeros_(str)
!!
!!     character(len=*)  :: str
!!
!!##DESCRIPTION
!!    TRIMZEROS_(3f) deletes trailing zeros from a string representing a
!!    number. If the resulting string would end in a decimal point, one
!!    trailing zero is added.
!!
!!##OPTIONS
!!    str   input string will be assumed to be a numeric value and have
!!          trailing zeros removed
!!##EXAMPLES
!!
!!    Sample program:
!!
!!       program demo_trimzeros_
!!       use M_strings, only : trimzeros_
!!       character(len=:),allocatable :: string
!!          write(*,*)trimzeros_('123.450000000000')
!!          write(*,*)trimzeros_('12345')
!!          write(*,*)trimzeros_('12345.')
!!          write(*,*)trimzeros_('12345.00e3')
!!       end program demo_trimzeros_
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine trimzeros_(string)

! ident_52="@(#)M_strings::trimzeros_(3fp): Delete trailing zeros from numeric decimal string"

! if zero needs added at end assumes input string has room
character(len=*)             :: string
character(len=len(string)+2) :: str
character(len=len(string))   :: exp          ! the exponent string if present
integer                      :: ipos         ! where exponent letter appears if present
integer                      :: i, ii
   str=string                                ! working copy of string
   ipos=scan(str,'eEdD')                     ! find end of real number if string uses exponent notation
   if(ipos>0) then                           ! letter was found
      exp=str(ipos:)                         ! keep exponent string so it can be added back as a suffix
      str=str(1:ipos-1)                      ! just the real part, exponent removed will not have trailing zeros removed
   endif
   if(index(str,'.').eq.0)then               ! if no decimal character in original string add one to end of string
      ii=len_trim(str)
      str(ii+1:ii+1)='.'                     ! add decimal to end of string
   endif
   do i=len_trim(str),1,-1                   ! scanning from end find a non-zero character
      select case(str(i:i))
      case('0')                              ! found a trailing zero so keep trimming
         cycle
      case('.')                              ! found a decimal character at end of remaining string
         if(i.le.1)then
            str='0'
         else
            str=str(1:i-1)
         endif
         exit
      case default
         str=str(1:i)                        ! found a non-zero character so trim string and exit
         exit
      end select
   enddo
   if(ipos>0)then                            ! if originally had an exponent place it back on
      string=trim(str)//trim(exp)
   else
      string=str
   endif
end subroutine trimzeros_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    listout(3f) - [M_strings:NUMERIC] expand a list of numbers where negative
!!    numbers denote range ends (1 -10 means 1 thru 10)
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine listout(icurve_lists,icurve_expanded,inums,ierr)
!!
!!    integer,intent(in)    :: icurve_lists(:)
!!    integer,intent(out)   :: icurve_expanded(:)
!!    integer,intent(out)   :: inums
!!    integer,intent(out)   :: ierr
!!
!!##DESCRIPTION
!!    expand a list of whole numbers where negative numbers indicate a range.
!!    So [10,-20] would be expanded to [10,11,12,13,14,15,16,17,18,19,20].
!!
!!##OPTIONS
!!    icurve_lists(:)      input array
!!
!!##RETURNS
!!    icurve_expanded(:)   output array; assumed large enough to hold
!!                         returned list
!!    inums                number of icurve_expanded numbers on output
!!    ierr                 zero if no error occurred
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_listout
!!     use M_strings, only : listout
!!     implicit none
!!     integer,allocatable :: icurve_lists(:)
!!     integer :: icurve_expanded(1000)
!!     ! icurve_lists is input array
!!     integer :: inums
!!     ! icurve_expanded is output array
!!     integer :: i
!!     ! number of icurve_lists values on input,
!!     ! number of icurve_expanded numbers on output
!!     integer :: ierr
!!        icurve_lists=[1, 20, -30, 101, 100, 99, 100, -120, 222, -200]
!!        inums=size(icurve_lists)
!!        call listout(icurve_lists,icurve_expanded,inums,ierr)
!!        if(ierr.eq.0)then
!!           write(*,'(i0)')(icurve_expanded(i),i=1,inums)
!!        else
!!           write(*,'(a,i0)')'error occurred in *listout* ',ierr
!!           write(*,'(i0)')(icurve_expanded(i),i=1,inums)
!!        endif
!!     end program demo_listout
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine listout(icurve_lists,icurve_expanded,inums_out,ierr)
implicit none

! ident_53="@(#)M_strings::listout(3f): copy icurve_lists to icurve_expanded expanding negative numbers to ranges (1 -10 means 1 thru 10)"

!   Created: 19971231
integer,intent(in)    :: icurve_lists(:)             ! input array
integer,intent(out)   :: icurve_expanded(:)          ! output array
integer,intent(out)   :: inums_out                   ! number of icurve_expanded numbers on output
integer,intent(out)   :: ierr                        ! status variable

character(len=80)     :: temp1
integer               :: i80, i90
integer               :: imin, imax
integer               :: idirection, icount
integer               :: iin
integer               :: inums_max

   ierr=0
   icurve_expanded=0                          ! initialize output array
   inums_out=0                                ! initialize number of significant values in output array

   inums_max=size(icurve_expanded)
   if(inums_max.eq.0)then
      ierr=-2
      return
   endif

   iin=size(icurve_lists)
   if(iin.gt.0)then
      icurve_expanded(1)=icurve_lists(1)
   endif

   icount=2
      do i90=2,iin
         if(icurve_lists(i90).lt.0)then
            imax=abs(icurve_lists(i90))
            imin=abs(icurve_lists(i90-1))
            if(imin.gt.imax)then
               idirection=-1
               imin=imin-1
            elseif(imax.gt.imin)then
               idirection=1
               imin=imin+1
            else
               idirection=1
            endif
            do i80=imin,imax,idirection
               if(icount.gt.inums_max) then
                  write(temp1,'(a,i5,a)')'*listout* only ',inums_max,' values allowed'
                  ierr=-1
                  call journal(temp1)
                  inums_out=icount-1
                  exit
               endif
               icurve_expanded(icount)=i80
               icount=icount+1
            enddo
         else
            icurve_expanded(icount)=icurve_lists(i90)
            icount=icount+1
         endif
      enddo
   inums_out=icount-1

end subroutine listout
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     quote(3f) - [M_strings:QUOTES] add quotes to string as if written
!!     with list-directed input
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function quote(str,mode,clip) result (quoted_str)
!!
!!    character(len=*),intent(in)          :: str
!!    character(len=*),optional,intent(in) :: mode
!!    logical,optional,intent(in)          :: clip
!!    character(len=:),allocatable         :: quoted_str
!!
!!##DESCRIPTION
!!    Add quotes to a CHARACTER variable as if it was written using
!!    list-directed input. This is particularly useful for processing
!!    strings to add to CSV files.
!!
!!##OPTIONS
!!    str    input string to add quotes to, using the rules of
!!           list-directed input (single quotes are replaced by two
!!           adjacent quotes)
!!    mode   alternate quoting methods are supported:
!!
!!             DOUBLE   default. replace quote with double quotes
!!             ESCAPE   replace quotes with backslash-quote instead of
!!                      double quotes
!!
!!    clip   default is to trim leading and trailing spaces from the
!!           string. If CLIP is .FALSE. spaces are not trimmed
!!
!!##RESULT
!!    quoted_str  The output string, which is based on adding quotes to STR.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_quote
!!    use M_strings, only : quote
!!    implicit none
!!    character(len=:),allocatable :: str
!!    character(len=1024)          :: msg
!!    integer                      :: ios
!!    character(len=80)            :: inline
!!       do
!!          write(*,'(a)',advance='no')'Enter test string:'
!!          read(*,'(a)',iostat=ios,iomsg=msg)inline
!!          if(ios.ne.0)then
!!             write(*,*)trim(inline)
!!             exit
!!          endif
!!
!!          ! the original string
!!          write(*,'(a)')'ORIGINAL     ['//trim(inline)//']'
!!
!!          ! the string processed by quote(3f)
!!          str=quote(inline)
!!          write(*,'(a)')'QUOTED     ['//str//']'
!!
!!          ! write the string list-directed to compare the results
!!          write(*,'(a)',iostat=ios,iomsg=msg) 'LIST DIRECTED:'
!!          write(*,*,iostat=ios,iomsg=msg,delim='none') inline
!!          write(*,*,iostat=ios,iomsg=msg,delim='quote') inline
!!          write(*,*,iostat=ios,iomsg=msg,delim='apostrophe') inline
!!       enddo
!!    end program demo_quote
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function quote(str,mode,clip) result (quoted_str)
character(len=*),intent(in)          :: str                ! the string to be quoted
character(len=*),optional,intent(in) :: mode
logical,optional,intent(in)          :: clip
character(len=:),allocatable         :: quoted_str

character(len=1),parameter           :: double_quote = '"'
character(len=20)                    :: local_mode

   if(present(clip))then
      if(clip)then
         quoted_str=adjustl(str)
      else
         quoted_str=str
      endif
   else
      quoted_str=str
   endif

   local_mode=merge_str(mode,'DOUBLE',present(mode))

   select case(lower(local_mode))
   case('double')
      quoted_str=double_quote//trim(replace(quoted_str,'"','""'))//double_quote
   case('escape')
      quoted_str=double_quote//trim(replace(quoted_str,'"','\"'))//double_quote
   case default
      call journal('sc','*quote* ERROR: unknown quote mode ',local_mode)
      quoted_str=str
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
end function quote
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     unquote(3f) - [M_strings:QUOTES] remove quotes from string as if
!!     read with list-directed input
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function unquote(quoted_str,esc) result (unquoted_str)
!!
!!    character(len=*),intent(in)          :: quoted_str
!!    character(len=1),optional,intent(in) :: esc
!!    character(len=:),allocatable         :: unquoted_str
!!
!!##DESCRIPTION
!!    Remove quotes from a CHARACTER variable as if it was read using
!!    list-directed input. This is particularly useful for processing
!!    tokens read from input such as CSV files.
!!
!!    Fortran can now read using list-directed input from an internal file,
!!    which should handle quoted strings, but list-directed input does not
!!    support escape characters, which UNQUOTE(3f) does.
!!
!!##OPTIONS
!!    quoted_str  input string to remove quotes from, using the rules of
!!                list-directed input (two adjacent quotes inside a quoted
!!                region are replaced by a single quote, a single quote or
!!                double quote is selected as the delimiter based on which
!!                is encountered first going from left to right, ...)
!!    esc         optional character used to protect the next quote
!!                character from being processed as a quote, but simply as
!!                a plain character.
!!
!!##RESULT
!!    unquoted_str  The output string, which is based on removing quotes
!!                  from quoted_str.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_unquote
!!       use M_strings, only : unquote
!!       implicit none
!!       character(len=128)           :: quoted_str
!!       character(len=:),allocatable :: unquoted_str
!!       character(len=1),parameter   :: esc='\'
!!       character(len=1024)          :: msg
!!       integer                      :: ios
!!       character(len=1024)          :: dummy
!!       do
!!          write(*,'(a)',advance='no')'Enter test string:'
!!          read(*,'(a)',iostat=ios,iomsg=msg)quoted_str
!!          if(ios.ne.0)then
!!             write(*,*)trim(msg)
!!             exit
!!          endif
!!
!!          ! the original string
!!          write(*,'(a)')'QUOTED       ['//trim(quoted_str)//']'
!!
!!          ! the string processed by unquote(3f)
!!          unquoted_str=unquote(trim(quoted_str),esc)
!!          write(*,'(a)')'UNQUOTED     ['//unquoted_str//']'
!!
!!          ! read the string list-directed to compare the results
!!          read(quoted_str,*,iostat=ios,iomsg=msg)dummy
!!          if(ios.ne.0)then
!!             write(*,*)trim(msg)
!!          else
!!             write(*,'(a)')'LIST DIRECTED['//trim(dummy)//']'
!!          endif
!!       enddo
!!    end program demo_unquote
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function unquote(quoted_str,esc) result (unquoted_str)
character(len=*),intent(in)          :: quoted_str              ! the string to be unquoted
character(len=1),optional,intent(in) :: esc                     ! escape character
character(len=:),allocatable         :: unquoted_str
integer                              :: inlen
character(len=1),parameter           :: single_quote = "'"
character(len=1),parameter           :: double_quote = '"'
integer                              :: quote                   ! whichever quote is to be used
integer                              :: before
integer                              :: current
integer                              :: iesc
integer                              :: iput
integer                              :: i
logical                              :: inside
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(esc))then                           ! select escape character as specified character or special value meaning not set
      iesc=ichar(esc)                             ! allow for an escape character
   else
      iesc=-1                                     ! set to value that matches no character
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   inlen=len(quoted_str)                          ! find length of input string
   allocate(character(len=inlen) :: unquoted_str) ! initially make output string length of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(inlen.ge.1)then                             ! double_quote is the default quote unless the first character is single_quote
      if(quoted_str(1:1).eq.single_quote)then
         quote=ichar(single_quote)
      else
         quote=ichar(double_quote)
      endif
   else
      quote=ichar(double_quote)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   before=-2                                      ! initially set previous character to impossible value
   unquoted_str(:)=''                             ! initialize output string to null string
   iput=1
   inside=.false.
   STEPTHROUGH: do i=1,inlen
      current=ichar(quoted_str(i:i))
      if(before.eq.iesc)then                      ! if previous character was escape use current character unconditionally
           iput=iput-1                            ! backup
           unquoted_str(iput:iput)=char(current)
           iput=iput+1
           before=-2                              ! this could be second esc or quote
      elseif(current.eq.quote)then                ! if current is a quote it depends on whether previous character was a quote
         if(before.eq.quote)then
           unquoted_str(iput:iput)=char(quote)    ! this is second quote so retain it
           iput=iput+1
           before=-2
         elseif(.not.inside.and.before.ne.iesc)then
            inside=.true.
         else                                     ! this is first quote so ignore it except remember it in case next is a quote
            before=current
         endif
      else
         unquoted_str(iput:iput)=char(current)
         iput=iput+1
         before=current
      endif
   enddo STEPTHROUGH
!-----------------------------------------------------------------------------------------------------------------------------------
   unquoted_str=unquoted_str(:iput-1)
!-----------------------------------------------------------------------------------------------------------------------------------
end function unquote
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    describe(3f) - [M_strings] returns a string describing the name of
!!    a single character
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function describe(ch) result (string)
!!
!!     character(len=1),intent(in)   :: ch
!!     character(len=:),allocatable  :: string
!!
!!##DESCRIPTION
!!    describe(3f) returns a string describing long name of a single
!!    character
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_describe
!!     use M_strings, only : describe
!!     implicit none
!!     integer :: i
!!        do i=1,128  ! fill variable with base ASCII character set
!!           write(*,*)describe(char(i-1))
!!        enddo
!!    end program demo_describe
!!
!!   Expected output
!!
!!     ctrl-@ or ctrl-? (NUL) null
!!     ctrl-A (SOH) start of heading
!!     ctrl-B (STX) start of text
!!     ctrl-C (ETX) end of text
!!     ctrl-D (EOT) end of transmission
!!     ctrl-E (ENQ) enquiry
!!     ctrl-F (ACK) acknowledge
!!     ctrl-G (BEL) bell
!!     ctrl-H (BS) backspace
!!     ctrl-I (HT) horizontal tabulation
!!     ctrl-J (LF) line feed
!!     ctrl-K (VT) vertical tabulation
!!     ctrl-L (FF) form feed
!!     ctrl-M (CR) carriage return
!!     ctrl-N (SO) shift out
!!     ctrl-O (SI) shift in
!!     ctrl-P (DLE) data link escape
!!     ctrl-Q (DC1) device control 1
!!     ctrl-R (DC2) device control 2
!!     ctrl-S (DC3) device control 3
!!     ctrl-T (DC4) device control 4
!!     ctrl-U (NAK) negative acknowledge
!!     ctrl-V (SYN) synchronous idle
!!     ctrl-W (ETB) end of transmission block
!!     ctrl-X (CAN) cancel
!!     ctrl-Y (EM) end of medium
!!     ctrl-Z (SUB) substitute
!!     ctrl-[ (ESC) escape
!!     ctrl-\ or ctrl-@ (FS) file separator
!!     ctrl-] (GS) group separator
!!     ctrl-^ or ctrl-= (RS) record separator
!!     ctrl-_ (US) unit separator
!!     space
!!     ! exclamation point
!!     " quotation marks
!!     # number sign
!!     $ currency symbol
!!     % percent
!!     & ampersand
!!     ' apostrophe
!!     ( left parenthesis
!!     ) right parenthesis
!!     * asterisk
!!     + plus
!!     , comma
!!     - minus
!!     . period
!!     / slash
!!     0 zero
!!     1 one
!!     2 two
!!     3 three
!!     4 four
!!     5 five
!!     6 six
!!     7 seven
!!     8 eight
!!     9 nine
!!     : colon
!!     ; semicolon
!!     < less than
!!     = equals
!!     > greater than
!!     ? question mark
!!     @ at sign
!!     majuscule A
!!     majuscule B
!!     majuscule C
!!     majuscule D
!!     majuscule E
!!     majuscule F
!!     majuscule G
!!     majuscule H
!!     majuscule I
!!     majuscule J
!!     majuscule K
!!     majuscule L
!!     majuscule M
!!     majuscule N
!!     majuscule O
!!     majuscule P
!!     majuscule Q
!!     majuscule R
!!     majuscule S
!!     majuscule T
!!     majuscule U
!!     majuscule V
!!     majuscule W
!!     majuscule X
!!     majuscule Y
!!     majuscule Z
!!     [ left bracket
!!     \ backslash
!!     ] right bracket
!!     ^ caret
!!     _ underscore
!!     ` grave accent
!!     miniscule a
!!     miniscule b
!!     miniscule c
!!     miniscule d
!!     miniscule e
!!     miniscule f
!!     miniscule g
!!     miniscule h
!!     miniscule i
!!     miniscule j
!!     miniscule k
!!     miniscule l
!!     miniscule m
!!     miniscule n
!!     miniscule o
!!     miniscule p
!!     miniscule q
!!     miniscule r
!!     miniscule s
!!     miniscule t
!!     miniscule u
!!     miniscule v
!!     miniscule w
!!     miniscule x
!!     miniscule y
!!     miniscule z
!!     { left brace
!!     | vertical line
!!     } right brace
!!     ~ tilde
!!     ctrl-? (DEL) delete
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function describe(ch) result (string)

! ident_54="@(#)M_strings::describe(3f): return string describing long name of a single character"

character(len=1),intent(in)   :: ch
character(len=:),allocatable  :: string
! LATER: add hex, octal, decimal, key-press description, alternate names
!  ASCII character codes
   select case (ichar(ch))
   case(     0  ); STRING="ctrl-@ or ctrl-? (NUL) null"
   case(     1  ); STRING="ctrl-A (SOH) start of heading"
   case(     2  ); STRING="ctrl-B (STX) start of text"
   case(     3  ); STRING="ctrl-C (ETX) end of text"
   case(     4  ); STRING="ctrl-D (EOT) end of transmission"
   case(     5  ); STRING="ctrl-E (ENQ) enquiry"
   case(     6  ); STRING="ctrl-F (ACK) acknowledge"
   case(     7  ); STRING="ctrl-G (BEL) bell"
   case(     8  ); STRING="ctrl-H (BS) backspace"
   case(     9  ); STRING="ctrl-I (HT) horizontal tabulation"
   case(    10  ); STRING="ctrl-J (LF) line feed"
   case(    11  ); STRING="ctrl-K (VT) vertical tabulation"
   case(    12  ); STRING="ctrl-L (FF) form feed"
   case(    13  ); STRING="ctrl-M (CR) carriage return"
   case(    14  ); STRING="ctrl-N (SO) shift out"
   case(    15  ); STRING="ctrl-O (SI) shift in"
   case(    16  ); STRING="ctrl-P (DLE) data link escape"
   case(    17  ); STRING="ctrl-Q (DC1) device control 1"
   case(    18  ); STRING="ctrl-R (DC2) device control 2"
   case(    19  ); STRING="ctrl-S (DC3) device control 3"
   case(    20  ); STRING="ctrl-T (DC4) device control 4"
   case(    21  ); STRING="ctrl-U (NAK) negative acknowledge"
   case(    22  ); STRING="ctrl-V (SYN) synchronous idle"
   case(    23  ); STRING="ctrl-W (ETB) end of transmission block"
   case(    24  ); STRING="ctrl-X (CAN) cancel"
   case(    25  ); STRING="ctrl-Y (EM) end of medium"
   case(    26  ); STRING="ctrl-Z (SUB) substitute"
   case(    27  ); STRING="ctrl-[ (ESC) escape"
   case(    28  ); STRING="ctrl-\ or ctrl-@ (FS) file separator"
   case(    29  ); STRING="ctrl-] (GS) group separator"
   case(    30  ); STRING="ctrl-^ or ctrl-= (RS) record separator"
   case(    31  ); STRING="ctrl-_ (US) unit separator"
   case(    32  ); STRING="space"
   case(    33  ); STRING="! exclamation point (screamer, gasper, slammer, startler, bang, shriek, pling)"
   case(    34  ); STRING=""" quotation marks"
   case(    35  ); STRING="# number sign (hash, pound sign, hashtag)"
   case(    36  ); STRING="$ currency symbol"
   case(    37  ); STRING="% percent"
   case(    38  ); STRING="& ampersand"
   case(    39  ); STRING="' apostrophe"
   case(    40  ); STRING="( left parenthesis"
   case(    41  ); STRING=") right parenthesis"
   case(    42  ); STRING="* asterisk"
   case(    43  ); STRING="+ plus"
   case(    44  ); STRING=", comma"
   case(    45  ); STRING="- minus"
   case(    46  ); STRING=". period"
   case(    47  ); STRING="/ slash"
   case(    48  ); STRING="0 zero"
   case(    49  ); STRING="1 one"
   case(    50  ); STRING="2 two"
   case(    51  ); STRING="3 three"
   case(    52  ); STRING="4 four"
   case(    53  ); STRING="5 five"
   case(    54  ); STRING="6 six"
   case(    55  ); STRING="7 seven"
   case(    56  ); STRING="8 eight"
   case(    57  ); STRING="9 nine"
   case(    58  ); STRING=": colon"
   case(    59  ); STRING="; semicolon"
   case(    60  ); STRING="< less than"
   case(    61  ); STRING="= equals"
   case(    62  ); STRING="> greater than"
   case(    63  ); STRING="? question mark"
   case(    64  ); STRING="@ at sign"
   case(    65  ); STRING="A majuscule A"
   case(    66  ); STRING="B majuscule B"
   case(    67  ); STRING="C majuscule C"
   case(    68  ); STRING="D majuscule D"
   case(    69  ); STRING="E majuscule E"
   case(    70  ); STRING="F majuscule F"
   case(    71  ); STRING="G majuscule G"
   case(    72  ); STRING="H majuscule H"
   case(    73  ); STRING="I majuscule I"
   case(    74  ); STRING="J majuscule J"
   case(    75  ); STRING="K majuscule K"
   case(    76  ); STRING="L majuscule L"
   case(    77  ); STRING="M majuscule M"
   case(    78  ); STRING="N majuscule N"
   case(    79  ); STRING="O majuscule O"
   case(    80  ); STRING="P majuscule P"
   case(    81  ); STRING="Q majuscule Q"
   case(    82  ); STRING="R majuscule R"
   case(    83  ); STRING="S majuscule S"
   case(    84  ); STRING="T majuscule T"
   case(    85  ); STRING="U majuscule U"
   case(    86  ); STRING="V majuscule V"
   case(    87  ); STRING="W majuscule W"
   case(    88  ); STRING="X majuscule X"
   case(    89  ); STRING="Y majuscule Y"
   case(    90  ); STRING="Z majuscule Z"
   case(    91  ); STRING="[ left bracket"
   case(    92  ); STRING="\ backslash"
   case(    93  ); STRING="] right bracket"
   case(    94  ); STRING="^ caret"
   case(    95  ); STRING="_ underscore"
   case(    96  ); STRING="` grave accent"
   case(    97  ); STRING="a miniscule a"
   case(    98  ); STRING="b miniscule b"
   case(    99  ); STRING="c miniscule c"
   case(   100  ); STRING="d miniscule d"
   case(   101  ); STRING="e miniscule e"
   case(   102  ); STRING="f miniscule f"
   case(   103  ); STRING="g miniscule g"
   case(   104  ); STRING="h miniscule h"
   case(   105  ); STRING="i miniscule i"
   case(   106  ); STRING="j miniscule j"
   case(   107  ); STRING="k miniscule k"
   case(   108  ); STRING="l miniscule l"
   case(   109  ); STRING="m miniscule m"
   case(   110  ); STRING="n miniscule n"
   case(   111  ); STRING="o miniscule o"
   case(   112  ); STRING="p miniscule p"
   case(   113  ); STRING="q miniscule q"
   case(   114  ); STRING="r miniscule r"
   case(   115  ); STRING="s miniscule s"
   case(   116  ); STRING="t miniscule t"
   case(   117  ); STRING="u miniscule u"
   case(   118  ); STRING="v miniscule v"
   case(   119  ); STRING="w miniscule w"
   case(   120  ); STRING="x miniscule x"
   case(   121  ); STRING="y miniscule y"
   case(   122  ); STRING="z miniscule z"
   case(   123  ); STRING="{ left brace"
   case(   124  ); STRING="| vertical line"
   case(   125  ); STRING="} right brace"
   case(   126  ); STRING="~ tilde"
   case(   127  ); STRING="ctrl-? (DEL) delete"
   case default
         STRING='UNKNOWN'//v2s(ICHAR(ch))
   end select
end function describe
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    getvals(3f) - [M_strings:NUMERIC] read arbitrary number of REAL values
!!    from a character variable up to size of VALUES() array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine getvals(line,values,icount,ierr)
!!
!!     character(len=*),intent(in)  :: line
!!     class(*),intent(out)         :: values(:)
!!     integer,intent(out)          :: icount
!!     integer,intent(out),optional :: ierr
!!
!!##DESCRIPTION
!!   GETVALS(3f) reads a relatively arbitrary number of numeric values from
!!   a character variable into a REAL array using list-directed input.
!!
!!   NOTE: In this version null values are skipped instead of meaning to leave
!!         that value unchanged
!!
!!        1,,,,,,,2 / reads VALUES=[1.0,2.0]
!!
!!   Per list-directed rules when reading values, allowed delimiters are
!!   comma, semi-colon and space.
!!
!!   the slash separator can be used to add inline comments.
!!
!!        10.1, 20.43e-1 ; 11 / THIS IS TREATED AS A COMMENT
!!
!!   Repeat syntax can be used up to the size of the output array. These are
!!   equivalent input lines:
!!
!!        4*10.0
!!        10.0, 10.0, 10.0, 10.0
!!
!!##OPTIONS
!!   LINE      A character variable containing the characters representing
!!             a list of numbers
!!
!!##RETURNS
!!   VALUES()  array holding numbers read from string. May be of type
!!             INTEGER, REAL, DOUBLEPRECISION, or CHARACTER. If CHARACTER the
!!             strings are returned as simple words instead of numeric values.
!!   ICOUNT    number of defined numbers in VALUES(). If ICOUNT reaches
!!             the size of the VALUES() array parsing stops.
!!   IERR      zero if no error occurred in reading numbers. Optional.
!!             If not present and an error occurs the program is terminated.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!       program demo_getvals
!!       use M_strings, only: getvals
!!       implicit none
!!       integer,parameter  :: longest_line=256
!!       character(len=longest_line) :: line
!!       real               :: values(longest_line/2+1)
!!       integer            :: ios,icount,ierr
!!       INFINITE: do
!!          read(*,'(a)',iostat=ios) line
!!          if(ios.ne.0)exit INFINITE
!!          call getvals(line,values,icount,ierr)
!!          write(*,'(4(g0,1x))')'VALUES=',values(:icount)
!!       enddo INFINITE
!!       end program demo_getvals
!!
!!  Sample input lines
!!
!!        10,20 30.4
!!        1 2 3
!!        1
!!
!!        3 4*2.5 8
!!        32.3333 / comment 1
!!        30e3;300,    30.0, 3
!!        even 1 like this! 10
!!        11,,,,22,,,,33
!!
!!  Expected output:
!!
!!     VALUES=   10.0000000       20.0000000       30.3999996
!!     VALUES=   1.00000000       2.00000000       3.00000000
!!     VALUES=   1.00000000
!!     VALUES=
!!     VALUES=   3.00000000       2.50000000       2.50000000
!!     2.50000000       2.50000000       8.00000000
!!     VALUES=   32.3333015
!!     VALUES=   30000.0000       300.000000       30.0000000
!!     3.00000000
!!     *getvals* WARNING:[even] is not a number
!!     *getvals* WARNING:[like] is not a number
!!     *getvals* WARNING:[this!] is not a number
!!     VALUES=   1.00000000       10.0000000
!!     VALUES=   11.0000000       22.0000000       33.0000000
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine getvals(line,values,icount,ierr)
implicit none

! ident_55="@(#)M_strings::getvals(3f): read arbitrary number of values from a character variable"

! JSU 20170831

character(len=*),intent(in)  :: line
class(*),intent(out)         :: values(:)
integer,intent(out)          :: icount
integer,intent(out),optional :: ierr

character(len=:),allocatable :: buffer
character(len=len(line))     :: words(size(values))
integer                      :: ios, i, ierr_local,isize

   isize=0
   select type(values)
   type is (integer);          isize=size(values)
   type is (real);             isize=size(values)
   type is (doubleprecision);  isize=size(values)
   type is (character(len=*)); isize=size(values)
   end select

   ierr_local=0

   words=' '                            ! make sure words() is initialized to null+blanks
   buffer=trim(unquote(line))//"/"      ! add a slash to the end so how the read behaves with missing values is clearly defined
   read(buffer,*,iostat=ios) words      ! undelimited strings are read into an array
   icount=0
   do i=1,isize                         ! loop thru array and convert non-blank words to numbers
      if(words(i).eq.' ')cycle

      select type(values)
      type is (integer);          read(words(i),*,iostat=ios)values(icount+1)
      type is (real);             read(words(i),*,iostat=ios)values(icount+1)
      type is (doubleprecision);  read(words(i),*,iostat=ios)values(icount+1)
      type is (character(len=*)); values(icount+1)=words(i)
      end select

      if(ios.eq.0)then
         icount=icount+1
      else
         ierr_local=ios
         write(ERROR_UNIT,*)'*getvals* WARNING:['//trim(words(i))//'] is not a number of specified type'
      endif
   enddo

   if(present(ierr))then
      ierr=ierr_local
   elseif(ierr_local.ne.0)then        ! error occurred and not returning error to main program to print message and stop program
      write(ERROR_UNIT,*)'*getval* error reading line ['//trim(line)//']'
      stop 2
   endif

end subroutine getvals
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      string_to_values(3f) - [M_strings:NUMERIC] read a string representing
!!      numbers into a numeric array
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine string_to_values(line,iread,values,inums,delims,ierr)
!!
!!        character(len=*) :: line
!!        integer          :: iread
!!        real             :: values(*)
!!        integer          :: inums
!!        character(len=*) :: delims
!!        integer          :: ierr
!!
!!##DESCRIPTION
!!    This routine can take a string representing a series of numbers and
!!    convert it to a numeric array and return how many numbers were found.
!!
!!##OPTIONS
!!       LINE     Input string containing numbers
!!       IREAD    maximum number of values to try to read from input string
!!
!!##RESULTS
!!       VALUES   real array to be filled with numbers
!!       INUMS    number of values successfully read (before error occurs
!!                if one does)
!!       DELIMS   delimiter character(s), usually a space. must not be a
!!                null string. If more than one character, a space must
!!                not be the last character or it will be ignored.
!!       IERR     error flag (0=no error, else column number string starts
!!                at that error occurred on).
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!      program demo_string_to_values
!!       use M_strings, only : string_to_values
!!       implicit none
!!       character(len=80)  :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
!!       integer,parameter  :: isz=10
!!       real               :: array(isz)
!!       integer            :: inums, ierr, ii
!!
!!       call string_to_values(s,10,array,inums,' ;',ierr)
!!       call reportit()
!!
!!       call string_to_values('10;2.3;3.1416',isz,array,inums,' ;',ierr)
!!       call reportit()
!!
!!       contains
!!          subroutine reportit()
!!             write(*,*)'string_to_values:'
!!             write(*,*)'input string.............',trim(s)
!!             write(*,*)'number of values found...',inums
!!             write(*,*)'values...................',(array(ii),ii=1,inums)
!!          end subroutine reportit
!!      end program demo_string_to_values
!!
!!    Expected output
!!
!!     string_to_values:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found...           6
!!     values...................   10.0000000  20000.0000  3.45000005
!!     -4.00299978  1234.00000  5678.00000
!!     string_to_values:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found...           3
!!     values...................   10.0000000  2.29999995  3.14159989
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine string_to_values(line,iread,values,inums,delims,ierr)
implicit none
!----------------------------------------------------------------------------------------------------------------------------------
!   1989,1997-12-31,2014 John S. Urban

!   given a line of structure , string , string , string process each
!   string as a numeric value and store into an array.
!   DELIMS contain the legal delimiters. If a space is an allowed delimiter, it must not appear last in DELIMS.
!   There is no direct checking for more values than can fit in VALUES.
!   Quits if encounters any errors in read.
!----------------------------------------------------------------------------------------------------------------------------------

! ident_56="@(#)M_strings::string_to_values(3f): reads an array of numbers from a numeric string"

character(len=*),intent(in)  :: line          ! input string
integer,intent(in)           :: iread         ! maximum number of values to try to read into values
real,intent(inout)           :: values(iread) ! real array to be filled with values
integer,intent(out)          :: inums         ! number of values successfully read from string
character(len=*),intent(in)  :: delims        ! allowed delimiters
integer,intent(out)          :: ierr          ! 0 if no error, else column number undecipherable string starts at
!----------------------------------------------------------------------------------------------------------------------------------
character(len=256)           :: delims_local        ! mutable copy of allowed delimiters
integer                      :: istart,iend,ilen,icol
integer                      :: i10,i20,i40
real                         :: rval
integer                      :: ier
integer                      :: delimiters_length
!----------------------------------------------------------------------------------------------------------------------------------
      delims_local=delims                                 ! need a mutable copy of the delimiter list
      if(delims_local.eq.'')then                          ! if delimiter list is null or all spaces make it a space
         delims_local=' '                                 ! delimiter is a single space
         delimiters_length=1                        ! length of delimiter list
      else
         delimiters_length=len_trim(delims)         ! length of variable WITH TRAILING WHITESPACE TRIMMED
      endif
!----------------------------------------------------------------------------------------------------------------------------------
      ierr=0                                        ! initialize error code returned
      inums=0                                       ! initialize count of values successfully returned
      istart=0
!----------------------------------------------------------------------------------------------------------------------------------
      ilen=0                                        ! ilen will be the position of the right-most non-delimiter in the input line
      do i20=len(line),1,-1                         ! loop from end of string to beginning to find right-most non-delimiter
         if(index(delims_local(:delimiters_length),line(i20:i20)).eq.0)then   ! found a non-delimiter
            ilen=i20
            exit
         endif
      enddo
      if(ilen.eq.0)then                             ! command was totally composed of delimiters
         call journal('*string_to_values* blank line passed as a list of numbers')
         return
      endif
!----------------------------------------------------------------------------------------------------------------------------------
!     there is at least one non-delimiter sub-string
!     ilen is the column position of the last non-delimiter character
!     now, starting at beginning of string find next non-delimiter
      icol=1                                                     ! pointer to beginning of unprocessed part of LINE
      LOOP: dO i10=1,iread,1                                     ! each pass should find a value
         if(icol.gt.ilen) EXIT LOOP                              ! everything is done
         INFINITE: do
            if(index(delims_local(:delimiters_length),line(icol:icol)).eq.0)then           ! found non-delimiter
               istart=icol
               iend=0                                            ! FIND END OF SUBSTRING
               do i40=istart,ilen                                ! look at each character starting at left
                  if(index(delims_local(:delimiters_length),line(i40:i40)).ne.0)then       ! determine if character is a delimiter
                     iend=i40                                    ! found a delimiter. record where it was found
                     EXIT                                        ! found end of substring so leave loop
                  endif
               enddo
              if(iend.eq.0)iend=ilen+1                           ! no delimiters found, so this substring goes to end of line
               iend=iend-1                                       ! do not want to pass delimiter to be converted
               rval=0.0
               call string_to_value(line(istart:iend),rval,ier)  ! call procedure to convert string to a numeric value
               if(ier.eq.0)then                                  ! a substring was successfully converted to a numeric value
                  values(i10)=rval                               ! store numeric value in return array
                  inums=inums+1                                  ! increment number of values converted to a numeric value
               else                                              ! an error occurred converting string to value
                  ierr=istart                                    ! return starting position of substring that could not be converted
                  return
               endif
               icol=iend+2                                       ! set to next character to look at
               CYCLE LOOP                                        ! start looking for next value
            else                                                 ! this is a delimiter so keep looking for start of next string
               icol=icol+1                                       ! increment pointer into LINE
               CYCLE INFINITE
            endif
         enddo INFINITE
      enddo LOOP
!     error >>>>> more than iread numbers were in the line.
end subroutine string_to_values
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      s2vs(3f) - [M_strings:NUMERIC] given a string representing numbers
!!      return a numeric array
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       function s2vs(line[,delim])
!!
!!        character(len=*) :: line
!!        doubleprecision,allocatable :: s2vs(:)
!!
!!##DESCRIPTION
!!    The function S2VS(3f) takes a string representing a series of numbers
!!    and converts it to a numeric doubleprecision array. The string values
!!    may be delimited by spaces, semi-colons, and commas by default.
!!
!!##OPTIONS
!!       LINE   Input string containing numbers
!!       DELIM  optional list of delimiter characters. If a space is
!!              included, it should appear as the left-most character
!!              in the list. The default is " ;," (spaces, semi-colons,
!!              and commas).
!!
!!##RESULTS
!!       S2VS   doubleprecision array
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!      program demo_s2vs
!!      use M_strings, only : s2vs
!!      implicit none
!!      character(len=80) :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
!!      real,allocatable :: values(:)
!!      integer,allocatable :: ivalues(:)
!!      integer :: ii
!!
!!      values=s2vs(s)
!!      ivalues=int(s2vs(s))
!!      call reportit()
!!
!!      contains
!!        subroutine reportit()
!!          write(*,*)'S2VS:'
!!          write(*,*)'input string.............',&
!!           & trim(s)
!!          write(*,*)'number of values found...',&
!!           & size(values)
!!          write(*,*)'values...................',&
!!           & (values(ii),ii=1,size(values))
!!          write(*,'(*(g0,1x))')'ivalues..................',&
!!           & (ivalues(ii),ii=1,size(values))
!!        end subroutine reportit
!!      end program demo_s2vs
!!
!!   Expected output
!!
!!     S2VS:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found...           6
!!     values...................   10.0000000 20000.0000 3.45000005
!!     -4.00299978 1234.00000 5678.00000
!!    ivalues.................. 10 20000 3 -4 1234 5678
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function s2vs(string,delim) result(darray)

! ident_57="@(#)M_strings::s2vs(3f): function returns array of values from a string"

character(len=*),intent(in)        :: string                       ! keyword to retrieve value for from dictionary
character(len=*),optional          :: delim                        ! delimiter characters
character(len=:),allocatable       :: delim_local
doubleprecision,allocatable        :: darray(:)                    ! function type

character(len=:),allocatable       :: carray(:)                    ! convert value to an array using split(3f)
integer                            :: i
integer                            :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(delim))then
      delim_local=delim
   else
      delim_local=' ;,'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call split(string,carray,delimiters=delim_local)         ! split string into an array
   allocate(darray(size(carray)))                           ! create the output array
   do i=1,size(carray)
      call string_to_value(carray(i), darray(i), ier)       ! convert the string to a numeric value
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end function s2vs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isprint(3f) - [M_strings:COMPARE] returns .true. if character is an
!!     ASCII printable character
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isprint(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isprint
!!
!!##DESCRIPTION
!!     isprint(3f) returns .true. if character is an ASCII printable character
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isprint  logical value returns true if character is a
!!             printable ASCII character else false.
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_isprint
!!    use M_strings, only : isprint
!!    implicit none
!!    integer                    :: i
!!    character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!       write(*,'(40(a))')'ISPRINT: ',pack( string, isprint(string) )
!!    end program demo_isprint
!!
!!   Results:
!!
!!    ISPRINT:  !"#$%&'()*+,-./0123456789:;<=>?@ABCDEF
!!    GHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmn
!!    opqrstuvwxyz{|}~
!!
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isprint(onechar)

! ident_58="@(#)M_strings::isprint(3f): indicates if input character is a printable ASCII character"

character,intent(in) :: onechar
logical              :: isprint
   select case (onechar)
      case (' ':'~')   ; isprint=.TRUE.
      case default     ; isprint=.FALSE.
   end select
end function isprint
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isgraph(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     printable character except a space is considered non-printable
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isgraph(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isgraph
!!
!!##DESCRIPTION
!!    isgraph(3f) returns .true. if character is a printable character
!!    except a space is considered non-printable
!!
!!##OPTIONS
!!    onechar   character to test
!!
!!##RETURNS
!!    isgraph   logical value returns true if character is a printable
!!              non-space character
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_isgraph
!!    use M_strings, only : isgraph
!!    implicit none
!!    integer                    :: i
!!    character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!       write(*,'(40(a))')'ISGRAPH: ',pack( string, isgraph(string) )
!!    end program demo_isgraph
!!
!!   Results:
!!
!!    ISGRAPH: !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFG
!!    HIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmno
!!    pqrstuvwxyz{|}~
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isgraph(onechar)

! ident_59="@(#)M_strings::isgraph(3f) :indicates if character is printable ASCII character excluding space"

character,intent(in) :: onechar
logical              :: isgraph
   select case (iachar(onechar))
   case (33:126)
     isgraph=.TRUE.
   case default
     isgraph=.FALSE.
   end select
end function isgraph
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    isalpha(3f) - [M_strings:COMPARE] returns .true. if character is a
!!    letter and .false. otherwise
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!   elemental function isalpha(onechar)
!!
!!    character,intent(in) :: onechar
!!    logical              :: isalpha
!!
!!##DESCRIPTION
!!    isalpha(3f) returns .true. if character is a letter and
!!    .false. otherwise
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isalpha  logical value returns .true. if character is a ASCII letter
!!             or false otherwise.
!!##EXAMPLE
!!
!!
!!   Sample program
!!
!!     program demo_isalpha
!!     use M_strings, only : isalpha
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(40(a))')'ISGRAPH: ',pack( string, isalpha(string) )
!!     end program demo_isalpha
!!
!!   Results:
!!
!!    ISGRAPH: ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklm
!!    nopqrstuvwxyz
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental function isalpha(ch) result(res)

! ident_60="@(#)M_strings::isalpha(3f): Return .true. if character is a letter and .false. otherwise"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('A':'Z','a':'z')
     res=.true.
   case default
     res=.false.
   end select
end function isalpha
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isxdigit(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     hexadecimal digit (0-9, a-f, or A-F).
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isxdigit(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isxdigit
!!
!!##DESCRIPTION
!!     isxdigit(3f) returns .true. if character is a hexadecimal digit (0-9,
!!     a-f, or A-F).
!!
!!##OPTIONS
!!    onechar   character to test
!!
!!##RETURNS
!!    isxdigit  logical value returns true if character is a hexadecimal digit
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!     program demo_isxdigit
!!     use M_strings, only : isxdigit
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(40(a))')'ISXDIGIT: ',pack( string, isxdigit(string) )
!!     end program demo_isxdigit
!!
!!   Results:
!!
!!    ISXDIGIT: 0123456789ABCDEFabcdef
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isxdigit(ch) result(res)

! ident_61="@(#)M_strings::isxdigit(3f): returns .true. if c is a hexadecimal digit (0-9,a-f, or A-F)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('A':'F','a':'f','0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isxdigit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isdigit(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     digit (0,1,...,9) and .false. otherwise
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isdigit(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isdigit
!!
!!##DESCRIPTION
!!     isdigit(3f) returns .true. if character is a digit (0,1,...,9)
!!     and .false. otherwise
!!
!!##EXAMPLES
!!
!!
!!  Sample Program:
!!
!!     program demo_isdigit
!!     use M_strings, only : isdigit, isspace, switch
!!     implicit none
!!     character(len=10),allocatable :: string(:)
!!     integer                       :: i
!!        string=[&
!!        & '1 2 3 4 5 ' ,&
!!        & 'letters   ' ,&
!!        & '1234567890' ,&
!!        & 'both 8787 ' ]
!!        ! if string is nothing but digits and whitespace return .true.
!!        do i=1,size(string)
!!           write(*,'(a)',advance='no')'For string['//string(i)//']'
!!           write(*,*) &
!!            & all(isdigit(switch(string(i))).or.&
!!            & isspace(switch(string(i))))
!!        enddo
!!     end program demo_isdigit
!!
!!  Expected output:
!!
!!        For string[1 2 3 4 5 ] T
!!        For string[letters   ] F
!!        For string[1234567890] T
!!        For string[both 8787 ] F
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isdigit(ch) result(res)

! ident_62="@(#)M_strings::isdigit(3f): Returns .true. if ch is a digit (0-9) and .false. otherwise"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isdigit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isblank(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     blank character (space or horizontal tab).
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isblank(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isblank
!!
!!##DESCRIPTION
!!     isblank(3f) returns .true. if character is a blank character (space
!!     or horizontal tab).
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isblank  logical value returns true if character is a "blank"
!!             ( an ASCII  space or horizontal tab character).
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_isblank
!!     use M_strings, only : isblank
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(*(g0,1x))')'ISXBLANK: ',&
!!        & ichar(pack( string, isblank(string) ))
!!     end program demo_isblank
!!
!!   Results:
!!
!!    ISXBLANK:  9 32
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isblank(ch) result(res)

! ident_63="@(#)M_strings::isblank(3f): returns .true. if character is a blank (space or horizontal tab)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(' ',char(9))
     res=.true.
   case default
     res=.false.
   end select
end function isblank
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isascii(3f) - [M_strings:COMPARE] returns .true. if the character is
!!     in the range char(0) to char(256)
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isascii(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isascii
!!
!!##DESCRIPTION
!!     isascii(3f) returns .true. if the character is in the range char(0)
!!     to char(127)
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isupper  logical value returns true if character is an ASCII
!!             character.
!!##EXAMPLE
!!
!!  Sample program
!!
!!     program demo_isascii
!!     use M_strings, only : isascii
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,255)]
!!        write(*,'(10(g0,1x))')'ISASCII: ', &
!!        & ichar(pack( string, isascii(string) ))
!!     end program demo_isascii
!!
!!  Results:
!!
!!    ISASCII:  0 1 2 3 4 5 6 7 8
!!    9 10 11 12 13 14 15 16 17 18
!!    19 20 21 22 23 24 25 26 27 28
!!    29 30 31 32 33 34 35 36 37 38
!!    39 40 41 42 43 44 45 46 47 48
!!    49 50 51 52 53 54 55 56 57 58
!!    59 60 61 62 63 64 65 66 67 68
!!    69 70 71 72 73 74 75 76 77 78
!!    79 80 81 82 83 84 85 86 87 88
!!    89 90 91 92 93 94 95 96 97 98
!!    99 100 101 102 103 104 105 106 107 108
!!    109 110 111 112 113 114 115 116 117 118
!!    119 120 121 122 123 124 125 126 127
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isascii(ch) result(res)

! ident_64="@(#)M_strings::isascii(3f): returns .true. if character is in the range char(0) to char(127)"

character,intent(in) :: ch
logical              :: res
   select case(ichar(ch))
   case(0:127)
     res=.true.
   case default
     res=.false.
   end select
end function isascii
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isspace(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     null, space, tab, carriage return, new line, vertical tab, or formfeed
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isspace(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isspace
!!
!!##DESCRIPTION
!!     isspace(3f) returns .true. if character is a null, space, tab,
!!     carriage return, new line, vertical tab, or formfeed
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isspace  returns true if character is ASCII white space
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_isspace
!!     use M_strings, only : isspace
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(20(g0,1x))')'ISSPACE: ', &
!!        & ichar(pack( string, isspace(string) ))
!!     end program demo_isspace
!!
!!   Results:
!!
!!    ISSPACE:  0 9 10 11 12 13 32
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isspace(ch) result(res)

! ident_65="@(#)M_strings::isspace(3f): true if null,space,tab,return,new line,vertical tab, or formfeed"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(' ')                 ! space(32)
     res=.true.
   case(char(0))             ! null(0)
     res=.true.
   case(char(9):char(13))    ! tab(9), new line(10), vertical tab(11), formfeed(12), carriage return(13),
     res=.true.
   case default
     res=.false.
   end select
end function isspace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     iscntrl(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     delete character or ordinary control character
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function iscntrl(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: iscntrl
!!
!!##DESCRIPTION
!!     iscntrl(3f) returns .true. if character is a delete character or
!!     ordinary control character
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    iscntrl  logical value returns true if character is a control character
!!
!!##EXAMPLE
!!
!!  Sample program
!!
!!     program demo_iscntrl
!!     use M_strings, only : iscntrl
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(20(g0,1x))')'ISCNTRL: ', &
!!        & ichar(pack( string, iscntrl(string) ))
!!     end program demo_iscntrl
!!
!!   Results:
!!
!!    ISCNTRL:  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
!!    20 21 22 23 24 25 26 27 28 29 30 31 127
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function iscntrl(ch) result(res)

! ident_66="@(#)M_strings::iscntrl(3f): true if a delete or ordinary control character(0x7F or 0x00-0x1F)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(char(127),char(0):char(31))
     res=.true.
   case default
     res=.false.
   end select
end function iscntrl
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     ispunct(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     printable punctuation character
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function ispunct(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: ispunct
!!
!!##DESCRIPTION
!!     ispunct(3f) returns .true. if character is a printable punctuation
!!     character
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    ispunct  logical value returns true if character is a printable
!!             punctuation character.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_ispunct
!!     use M_strings, only : ispunct
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(20(g0,1x))')'ISPUNCT: ', &
!!        & ichar(pack( string, ispunct(string) ))
!!        write(*,'(20(g0,1x))')'ISPUNCT: ', &
!!        & pack( string, ispunct(string) )
!!     end program demo_ispunct
!!   Results:
!!
!!    ISPUNCT:  33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 58 59 60 61
!!    62 63 64 91 92 93 94 95 96 123 124 125 126
!!    ISPUNCT:  ! " # $ % & ' ( ) * + , - . / : ; < =
!!    > ? @ [ \ ] ^ _ ` { | } ~
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function ispunct(ch) result(res)

! ident_67="@(#)M_strings::ispunct(3f): true if a printable punctuation character (isgraph(c)&&"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case (char(33):char(47), char(58):char(64), char(91):char(96), char(123):char(126))
     res=.true.
!  case(' ','0':'9','A':'Z','a':'z',char(128):)
!    res=.true.
!  case(char(0):char(31),char(127))
!    res=.true.
   case default
     res=.false.
   end select
end function ispunct
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     fortran_name(3f) - [M_strings:COMPARE] test if string meets criteria
!!     for being a fortran name
!!
!!##SYNOPSIS
!!
!!
!!     elemental function fortran_name(line) result (lout)
!!
!!      character(len=*),intent(in)  :: line
!!      logical                      :: lout
!!
!!##DESCRIPTION
!!     Determines if a string is an allowed Fortran name. To pass the input
!!     string must be composed of 1 to 63 ASCII characters and start with a
!!     letter and be composed entirely of alphanumeric characters [a-zA-Z0-9]
!!     and underscores.
!!
!!##OPTIONS
!!     LINE   input string to test. Leading spaces are significant but
!!            trailing spaces are ignored.
!!
!!##RETURNS
!!     LOUT   a logical value indicating if the input string passed or failed
!!            the test to see if it is a valid Fortran name or not.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!      program demo_fortran_name
!!      use M_strings, only : fortran_name
!!      implicit none
!!      character(len=*),parameter :: names(*)=[character(len=20) ::  &
!!       & '_name',         'long_variable_name', 'name_',         &
!!       & '12L',           'a__b__c  ',          'PropertyOfGas', &
!!       & '3%3',           '$NAME',              ' ',             &
!!       & 'Variable-name', 'A',                  'x@x' ]
!!      integer :: i
!!         write(*,'(i3,1x,a20,1x,l1)')&
!!         & (i,names(i),fortran_name(names(i)),i=1,size(names))
!!      end program demo_fortran_name
!!
!!    Results:
!!
!!       1 _name                F
!!       2 long_variable_name   T
!!       3 name_                T
!!       4 12L                  F
!!       5 a__b__c              T
!!       6 PropertyOfGas        T
!!       7 3%3                  F
!!       8 $NAME                F
!!       9                      F
!!      10 Variable-name        F
!!      11 A                    T
!!      12 x@x                  F
elemental function fortran_name(line) result (lout)

! ident_68="@(#)M_strings::fortran_name(3f): Return .true. if name is a valid Fortran name"

! determine if a string is a valid Fortran name ignoring trailing spaces (but not leading spaces)
character(len=*),parameter   :: int='0123456789'
character(len=*),parameter   :: lower='abcdefghijklmnopqrstuvwxyz'
character(len=*),parameter   :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
character(len=*),parameter   :: allowed=upper//lower//int//'_'

character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
logical                      :: lout
   name=trim(line)
   if(len(name).ne.0)then
      lout = verify(name(1:1), lower//upper) == 0  &
       & .and. verify(name,allowed) == 0           &
       & .and. len(name) <= 63
   else
      lout = .false.
   endif
end function fortran_name
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isupper(3f) - [M_strings:COMPARE] returns .true. if character is an
!!     uppercase letter (A-Z)
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isupper(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isupper
!!
!!##DESCRIPTION
!!     isupper(3f) returns .true. if character is an uppercase letter (A-Z)
!!
!!##OPTIONS
!!    onechar  character to test
!!##RETURNS
!!    isupper  logical value returns true if character is an uppercase
!!             ASCII character else false.
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_isupper
!!     use M_strings, only : isupper
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(10(g0,1x))')'ISUPPER: ', &
!!        & ichar(pack( string, isupper(string) ))
!!        write(*,'(10(g0,1x))')'ISUPPER: ', &
!!        & pack( string, isupper(string) )
!!     end program demo_isupper
!!
!!  Results:
!!
!!    ISUPPER:  65 66 67 68 69 70 71 72 73
!!    74 75 76 77 78 79 80 81 82 83
!!    84 85 86 87 88 89 90
!!    ISUPPER:  A B C D E F G H I
!!    J K L M N O P Q R S
!!    T U V W X Y Z
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
pure elemental function isupper(ch) result(res)

! ident_69="@(#)M_strings::isupper(3f): returns true if character is an uppercase letter (A-Z)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('A':'Z')
     res=.true.
   case default
     res=.false.
   end select
end function isupper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     islower(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     miniscule letter (a-z)
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function islower(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: islower
!!
!!##DESCRIPTION
!!     islower(3f) returns .true. if character is a miniscule letter (a-z)
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    islowe  logical value returns true if character is a lowercase
!!             ASCII character else false.
!!##EXAMPLE
!!
!!  Sample program
!!
!!     program demo_islower
!!     use M_strings, only : islower
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(15(g0,1x))')'ISLOWER: ', &
!!        & ichar(pack( string, islower(string) ))
!!        write(*,'(15(g0,1x))')'ISLOWER: ', &
!!        & pack( string, islower(string) )
!!     end program demo_islower
!!   Results:
!!
!!    ISLOWER:  97 98 99 100 101 102 103 104 105 106 107 108 109 110
!!    111 112 113 114 115 116 117 118 119 120 121 122
!!    ISLOWER:  a b c d e f g h i j k l m n
!!    o p q r s t u v w x y z
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function islower(ch) result(res)

! ident_70="@(#)M_strings::islower(3f): returns true if character is a miniscule letter (a-z)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('a':'z')
     res=.true.
   case default
     res=.false.
   end select
end function islower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    isalnum,isalpha,iscntrl,isdigit,isgraph,islower,
!!    isprint,ispunct,isspace,isupper,
!!    isascii,isblank,isxdigit(3f) - [M_strings:COMPARE] test membership in
!!    subsets of ASCII set
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    Where "FUNCNAME" is one of the function names in the group, the
!!    functions are defined by
!!
!!     elemental function FUNCNAME(onechar)
!!     character,intent(in) :: onechar
!!     logical              :: FUNC_NAME
!!##DESCRIPTION
!!
!!       These elemental functions test if a character belongs to various
!!       subsets of the ASCII character set.
!!
!!       isalnum    returns .true. if character is a letter (a-z,A-Z)
!!                  or digit (0-9)
!!       isalpha    returns .true. if character is a letter and
!!                  .false. otherwise
!!       isascii    returns .true. if character is in the range char(0)
!!                  to char(127)
!!       isblank    returns .true. if character is a blank (space or
!!                  horizontal tab).
!!       iscntrl    returns .true. if character is a delete character or
!!                  ordinary control character (0x7F or 0x00-0x1F).
!!       isdigit    returns .true. if character is a digit (0,1,...,9)
!!                  and .false. otherwise
!!       isgraph    returns .true. if character is a printable ASCII
!!                  character excluding space
!!       islower    returns .true. if character is a miniscule letter (a-z)
!!       isprint    returns .true. if character is a printable ASCII character
!!       ispunct    returns .true. if character is a printable punctuation
!!                  character (isgraph(c) && !isalnum(c)).
!!       isspace    returns .true. if character is a null, space, tab,
!!                  carriage return, new line, vertical tab, or formfeed
!!       isupper    returns .true. if character is an uppercase letter (A-Z)
!!       isxdigit   returns .true. if character is a hexadecimal digit
!!                  (0-9, a-f, or A-F).
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_isdigit
!!
!!     use M_strings, only : isdigit, isspace, switch
!!     implicit none
!!     character(len=10),allocatable :: string(:)
!!     integer                       :: i
!!        string=[&
!!        & '1 2 3 4 5 ' ,&
!!        & 'letters   ' ,&
!!        & '1234567890' ,&
!!        & 'both 8787 ' ]
!!        ! if string is nothing but digits and whitespace return .true.
!!        do i=1,size(string)
!!           write(*,'(a)',advance='no')'For string['//string(i)//']'
!!           write(*,*) &
!!           all(isdigit(switch(string(i))) .or. &
!!           & isspace(switch(string(i))))
!!        enddo
!!
!!     end program demo_isdigit
!!
!!   Expected output:
!!
!!    For string[1 2 3 4 5 ] T
!!    For string[letters   ] F
!!    For string[1234567890] T
!!    For string[both 8787 ] F
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental function isalnum(ch) result(res)

! ident_71="@(#)M_strings::isalnum(3f): returns true if character is a letter (a-z,A-Z) or digit(0-9)"

character,intent(in)       :: ch
logical                    :: res
   select case(ch)
   case('a':'z','A':'Z','0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isalnum
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    base(3f) - [M_strings:BASE] convert whole number string in base [2-36]
!!    to string in alternate base [2-36]
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function base(x,b,y,a)
!!
!!    character(len=*),intent(in)  :: x
!!    character(len=*),intent(out) :: y
!!    integer,intent(in)           :: b,a
!!##DESCRIPTION
!!
!!    Convert a numeric string from base B to base A. The function returns
!!    FALSE if B is not in the range [2..36] or if string X contains invalid
!!    characters in base B or if result Y is too big
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    x   input string representing numeric whole value
!!    b   assumed base of input string
!!    y   output string
!!    a   base specified for output string
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_base
!!    use M_strings, only : base
!!    implicit none
!!    integer           :: ba,bd
!!    character(len=40) :: x,y
!!
!!    print *,' BASE CONVERSION'
!!    write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
!!    write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
!!    INFINITE: do
!!       write(*,'("Enter number in start base (0 to quit): ")',advance='no')
!!       read *, x
!!       if(x.eq.'0') exit INFINITE
!!       if(base(x,bd,y,ba))then
!!            write(*,'("In base ",I2,": ",A20)')  ba, y
!!        else
!!          print *,'Error in decoding/encoding number.'
!!        endif
!!     enddo INFINITE
!!
!!     end program demo_base
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
logical function base(x,b,y,a)
implicit none
character(len=*),intent(in)  :: x
character(len=*),intent(out) :: y
integer,intent(in)           :: b,a
integer                      :: temp

! ident_72="@(#)M_strings::base(3f): convert whole number string in base [2-36] to string in alternate base [2-36]"

base=.true.
if(decodebase(x,b,temp)) then
   if(codebase(temp,a,y)) then
   else
      print *,'Error in coding number.'
      base=.false.
   endif
else
   print *,'Error in decoding number.'
   base=.false.
endif

end function base
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    base2(3f) - [M_strings:BASE] convert whole number to string in base 2
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function base2(int)
!!
!!    integer,intent(in)           :: int
!!    character(len=:),allocatable :: base2
!!##DESCRIPTION
!!
!!    Convert a whole number to a string in base 2.
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    int   input string representing numeric whole value
!!##RETURNS
!!    base2   string representing input value in base 2
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_base2
!!    use M_strings, only : base2
!!    implicit none
!!    integer                      :: i
!!    character(len=:),allocatable :: string
!!       write(*,'(a)') base2(huge(0))
!!       write(*,'(a)') base2(0)
!!       write(*,'(a)') base2(1-huge(0))
!!    end program demo_base2
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
! 0 in binary: 0
! 42 in binary: 101010
! huge(int) in binary: 1111111111111111111111111111111
! 032 in binary is 100000
! itimes=10000000
!      G_TRICK=base2_f(32)   <BASE2_F  >Processor Time =  0.766 seconds.
!      G_TRICK=base2_fdo(32) <BASE2_FDO>Processor Time =  0.958 seconds.
!      G_TRICK=base2_a(32)   <BASE2_A  >Processor Time =  1.022 seconds.
!      G_TRICK=base2_c(32)   <BASE2_C  >Processor Time =  7.208 seconds.
!      G_TRICK=empty(32)     <EMPTY    >Processor Time =  0.132 seconds.
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2(x) result(str)
!  return string representing number as a binary number.  Fixed-length string:
integer, intent(in) :: x
integer           :: i
character(len=max(1,bit_size(x)-leadz(x))) :: str
    associate(n => len(str))
      str = repeat('0',n)
      do i = 0,n-1
        if (btest(x,i)) str(n-i:n-i) = '1'
      end do
    end associate
end function base2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2_fdo(x) result(str)
!  return string representing number as a binary number.  Fixed-length string: do concurrent
integer, intent(in) :: x
character(len=max(1,bit_size(x)-leadz(x))) :: str

integer :: n, i

    if (x == 0) then
      str(1:1) = '0'
      return
    endif
    n = len(str)
    str = repeat('0',n)
    do concurrent (i = 0:n-1, btest(x,i))
      str(n-i:n-i) = '1'
    end do
end function base2_fdo
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2_a(x) result(str)
!  return string representing number as a binary number. Allocatable-length string:
integer, intent(in) :: x
character(len=:), allocatable :: str

integer :: n, i

    n = max(1,bit_size(x)-leadz(x))
    allocate(character(len=n) :: str)
    if (x == 0) then
      str(1:1) = '0'
      return
    endif

    str = repeat('0',n)
    do concurrent (i = 0:n-1, btest(x,i))
      str(n-i:n-i) = '1'
    end do
end function base2_a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2_c(x) result(str)
! internal write
integer, intent(in) :: x
character(len=max(1,bit_size(x)-leadz(x))) :: str
    write( str, fmt="(b0)" ) x
end function base2_c
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    decodebase(3f) - [M_strings:BASE] convert whole number string in base
!!    [2-36] to base 10 number
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function decodebase(string,basein,out10)
!!
!!    character(len=*),intent(in)  :: string
!!    integer,intent(in)           :: basein
!!    integer,intent(out)          :: out10
!!##DESCRIPTION
!!
!!    Convert a numeric string representing a whole number in base BASEIN
!!    to base 10. The function returns FALSE if BASEIN is not in the range
!!    [2..36] or if string STRING contains invalid characters in base BASEIN
!!    or if result OUT10 is too big
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    string   input string. It represents a whole number in
!!             the base specified by BASEIN unless BASEIN is set
!!             to zero. When BASEIN is zero STRING is assumed to
!!             be of the form BASE#VALUE where BASE represents
!!             the function normally provided by BASEIN.
!!    basein   base of input string; either 0 or from 2 to 36.
!!    out10    output value in base 10
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_decodebase
!!    use M_strings, only : codebase, decodebase
!!    implicit none
!!    integer           :: ba,bd
!!    character(len=40) :: x,y
!!    integer           :: r
!!
!!    print *,' BASE CONVERSION'
!!    write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
!!    write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
!!    INFINITE: do
!!       print *,''
!!       write(*,'("Enter number in start base: ")',advance='no'); read *, x
!!       if(x.eq.'0') exit INFINITE
!!       if(decodebase(x,bd,r)) then
!!          if(codebase(r,ba,y)) then
!!            write(*,'("In base ",I2,": ",A20)')  ba, y
!!          else
!!            print *,'Error in coding number.'
!!          endif
!!       else
!!          print *,'Error in decoding number.'
!!       endif
!!    enddo INFINITE
!!
!!    end program demo_decodebase
!!
!!##AUTHOR
!!    John S. Urban
!!
!!       Ref.: "Math matiques en Turbo-Pascal by
!!              M. Ducamp and A. Reverchon (2),
!!              Eyrolles, Paris, 1988".
!!
!!    based on a F90 Version By J-P Moreau (www.jpmoreau.fr)
!!
!!##LICENSE
!!    Public Domain
logical function decodebase(string,basein,out_baseten)
implicit none

! ident_73="@(#)M_strings::decodebase(3f): convert whole number string in base [2-36] to base 10 number"

character(len=*),intent(in)  :: string
integer,intent(in)           :: basein
integer,intent(out)          :: out_baseten

character(len=len(string))   :: string_local
integer           :: long, i, j, k
real              :: y
real              :: mult
character(len=1)  :: ch
real,parameter    :: XMAXREAL=real(huge(1))
integer           :: out_sign
integer           :: basein_local
integer           :: ipound
integer           :: ierr

  string_local=upper(trim(adjustl(string)))
  decodebase=.false.

  ipound=index(string_local,'#')                                       ! determine if in form [-]base#whole
  if(basein.eq.0.and.ipound.gt.1)then                                  ! split string into two values
     call string_to_value(string_local(:ipound-1),basein_local,ierr)   ! get the decimal value of the base
     string_local=string_local(ipound+1:)                              ! now that base is known make string just the value
     if(basein_local.ge.0)then                                         ! allow for a negative sign prefix
        out_sign=1
     else
        out_sign=-1
     endif
     basein_local=abs(basein_local)
  else                                                                 ! assume string is a simple positive value
     basein_local=abs(basein)
     out_sign=1
  endif

  out_baseten=0
  y=0.0
  ALL: if(basein_local<2.or.basein_local>36) then
    print *,'(*decodebase* ERROR: Base must be between 2 and 36. base=',basein_local
  else ALL
     out_baseten=0;y=0.0; mult=1.0
     long=LEN_TRIM(string_local)
     do i=1, long
        k=long+1-i
        ch=string_local(k:k)
        if(ch.eq.'-'.and.k.eq.1)then
           out_sign=-1
           cycle
        endif
        if(ch<'0'.or.ch>'Z'.or.(ch>'9'.and.ch<'A'))then
           write(*,*)'*decodebase* ERROR: invalid character ',ch
           exit ALL
        endif
        if(ch<='9') then
              j=IACHAR(ch)-IACHAR('0')
        else
              j=IACHAR(ch)-IACHAR('A')+10
        endif
        if(j>=basein_local)then
           exit ALL
        endif
        y=y+mult*j
        if(mult>XMAXREAL/basein_local)then
           exit ALL
        endif
        mult=mult*basein_local
     enddo
     decodebase=.true.
     out_baseten=nint(out_sign*y)*sign(1,basein)
  endif ALL
end function decodebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    codebase(3f) - [M_strings:BASE] convert whole number in base 10 to
!!    string in base [2-36]
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function codebase(in_base10,out_base,answer)
!!
!!    integer,intent(in)           :: in_base10
!!    integer,intent(in)           :: out_base
!!    character(len=*),intent(out) :: answer
!!
!!##DESCRIPTION
!!    Convert a number from base 10 to base OUT_BASE. The function returns
!!    .FALSE. if OUT_BASE is not in [2..36] or if number IN_BASE10 is
!!    too big.
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_codebase
!!    use M_strings, only : codebase
!!    implicit none
!!    character(len=20) :: answer
!!    integer           :: i, j
!!    logical           :: ierr
!!    do j=1,100
!!       do i=2,36
!!          ierr=codebase(j,i,answer)
!!          write(*,*)'VALUE=',j,' BASE=',i,' ANSWER=',answer
!!       enddo
!!    enddo
!!    end program demo_codebase
!!
!!##AUTHOR
!!    John S. Urban
!!
!!     Ref.: "Math matiques en Turbo-Pascal by
!!            M. Ducamp and A. Reverchon (2),
!!            Eyrolles, Paris, 1988".
!!
!!    based on a F90 Version By J-P Moreau (www.jpmoreau.fr)
!!
!!##LICENSE
!!    Public Domain
logical function codebase(inval10,outbase,answer)
implicit none

! ident_74="@(#)M_strings::codebase(3f): convert whole number in base 10 to string in base [2-36]"

integer,intent(in)           :: inval10
integer,intent(in)           :: outbase
character(len=*),intent(out) :: answer
integer                      :: n
real                         :: inval10_local
integer                      :: outbase_local
integer                      :: in_sign
  answer=''
  in_sign=sign(1,inval10)*sign(1,outbase)
  inval10_local=abs(inval10)
  outbase_local=abs(outbase)
  if(outbase_local<2.or.outbase_local>36) then
    print *,'*codebase* ERROR: base must be between 2 and 36. base was',outbase_local
    codebase=.false.
  else
     do while(inval10_local>0.0 )
        n=INT(inval10_local-outbase_local*INT(inval10_local/outbase_local))
        if(n<10) then
           answer=ACHAR(IACHAR('0')+n)//answer
        else
           answer=ACHAR(IACHAR('A')+n-10)//answer
        endif
        inval10_local=INT(inval10_local/outbase_local)
     enddo
     codebase=.true.
  endif
  if(in_sign.eq.-1)then
     answer='-'//trim(answer)
  endif
  if(answer.eq.'')then
     answer='0'
  endif
end function codebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function todecimal(base, instr)

! ident_75="@(#)M_strings::todecimal(3f): given string and base return decimal integer"

! based on an example at rosetta code.
character(len=36),parameter  :: alphanum = "0123456789abcdefghijklmnopqrstuvwxyz"
integer,intent(in)           :: base
character(*),intent(in)      :: instr
character(len=:),allocatable :: instr_local
integer                      :: todecimal
integer                      :: length, i, n

   instr_local=trim(lower(instr))
   todecimal = 0
   length = len(instr_local)
   do i = 1, length
      n = index(alphanum, instr_local(i:i)) - 1
      n = n * base**(length-i)
      todecimal = todecimal + n
   enddo
end function todecimal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function tobase(base, number)

! ident_76="@(#)M_strings::todecimal(3f): given integer and base return string"

! based on an example at rosetta code.
character(len=36),parameter  :: alphanum = "0123456789abcdefghijklmnopqrstuvwxyz"
integer,intent(in)           :: base
integer,intent(in)           :: number
character(len=:),allocatable :: tobase
character(len=31)            :: holdit
integer                      :: number_local, i, rem
   number_local=number

   holdit = "                               "
   do i = 31, 1, -1
      if(number_local < base) then
         holdit(i:i) = alphanum(number_local+1:number_local+1)
         exit
      endif
      rem = mod(number_local, base)
      holdit(i:i) = alphanum(rem+1:rem+1)
      number_local = number_local / base
   enddo
   tobase = adjustl(holdit)
end function tobase

!SUBROUTINE DectoBase(decimal, string, base)
! CHARACTER string
!    string = '0'
!    temp = decimal
!    length = CEILING( LOG(decimal+1, base) )   !<<<<<<<< INTERESTING
!    DO i = length, 1, -1
!      n = MOD( temp, base )
!      string(i) = "0123456789abcdefghijklmnopqrstuvwxyz"(n+1)
!      temp = INT(temp / base)
!    ENDDO
! END
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fmt(3f) - [M_strings:TOKENS] break a long line into a paragraph
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function fmt(source_string,length)
!!
!!    character(len=*),intent(in)       :: source_string
!!    integer,intent(in)                :: length
!!    character(allocatable(len=length)    :: fmt(:)
!!
!!##DESCRIPTION
!!    fmt(3f) breaks a long line into a simple paragraph of specified
!!    line length.
!!
!!    Given a long string break it on spaces into an array such that no
!!    variable is longer than the specified length. Individual words longer
!!    than LENGTH will be placed in variables by themselves.
!!
!!##OPTIONS
!!     SOURCE_STRING  input string to break into an array of shorter strings
!!                    on blank delimiters
!!     LENGTH         length of lines to break the string into.
!!
!!##RETURNS
!!     FMT  character array filled with data from source_string broken at
!!          spaces into variables of length LENGTH.
!!
!!##EXAMPLE
!!
!!  sample program
!!
!!    program demo_fmt
!!    use M_strings, only : fmt
!!    implicit none
!!    character(len=:),allocatable :: paragraph(:)
!!    character(len=*),parameter    :: string= '&
!!     &one two three four five &
!!     &six seven eight &
!!     &nine ten eleven twelve &
!!     &thirteen fourteen fifteen sixteen &
!!     &seventeen'
!!
!!    write(*,*)'LEN=',len(string)
!!    write(*,*)'INPUT:'
!!    write(*,*)string
!!
!!    paragraph=fmt(string,40)
!!    write(*,*)'LEN=',len(paragraph),' SIZE=',size(paragraph)
!!    write(*,*)'OUTPUT:'
!!    write(*,'(a)')paragraph
!!
!!    write(*,'(a)')fmt(string,0)
!!    write(*,'(3x,a)')fmt(string,47)
!!
!!    end program demo_fmt
!!
!!  Results:
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function fmt(source_string,length)

! ident_77="@(#)M_strings::fmt(3f): wrap a long string into a paragraph"

character(len=*),intent(in)       :: source_string
integer,intent(in)                :: length
integer                           :: itoken
integer                           :: istart
integer                           :: iend
character(len=*),parameter        :: delimiters=' '
character(len=:),allocatable      :: fmt(:)
integer                           :: ilines
integer                           :: ilength
integer                           :: iword, iword_max
integer                           :: i
!-----------------------------------------------------------------------------------------------------------------------------------
!  parse string once to find out how big to make the returned array, then redo everything but store the data
!  could store array of endpoints and leave original whitespace alone or many other options
   do i=1,2
      iword_max=0                                  ! length of longest token
      ilines=1                                     ! number of output line output will go on
      ilength=0                                    ! length of output line so far
      itoken=0                                     ! must set ITOKEN=0 before looping on strtok(3f) on a new string.
      do while ( strtok(source_string,itoken,istart,iend,delimiters) )
         iword=iend-istart+1
         iword_max=max(iword_max,iword)
         if(iword.gt.length)then                   ! this token is longer than the desired line length so put it on a line by itself
            if(ilength.ne.0)then
               ilines=ilines+1
            endif
            if(i.eq.2)then                 ! if fmt has been allocated store data, else just gathering data to determine size of fmt
               fmt(ilines)=source_string(istart:iend)//' '
            endif
            ilength=iword+1
         elseif(ilength+iword.le.length)then       ! this word will fit on current line
            if(i.eq.2)then
               fmt(ilines)=fmt(ilines)(:ilength)//source_string(istart:iend)
            endif
            ilength=ilength+iword+1
         else                                      ! adding this word would make line too long so start new line
            ilines=ilines+1
            ilength=0
            if(i.eq.2)then
               fmt(ilines)=fmt(ilines)(:ilength)//source_string(istart:iend)
            endif
            ilength=iword+1
         endif
      enddo
      if(i==1)then                                 ! determined number of lines needed so allocate output array
         allocate(character(len=max(length,iword_max)) :: fmt(ilines))
         fmt=' '
      endif
   enddo
   fmt=fmt(:ilines)
end function fmt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function setbits8(string) result(answer)
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
integer(kind=int8)          :: answer
character(len=8),intent(in) :: string
integer                     :: pos
integer                     :: ilen
   answer=0_int8
   ilen=len(string)
   if(ilen.ne.bit_size(answer))then
      write(stderr,*)'*setbits8* wrong string length =',ilen
      ilen=min(ilen,int(bit_size(answer)))
   endif
   do pos=1,ilen
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits8* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits8
!-----------------------------------------------------------------------------------------------------------------------------------
function setbits16(string) result(answer)
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
integer(kind=int16)          :: answer
character(len=16),intent(in) :: string
integer                      :: pos
integer                      :: ilen
   answer=0_int16
   ilen=len(string)
   if(ilen.ne.bit_size(answer))then
      write(stderr,*)'*setbits16* wrong string length =',ilen
      ilen=min(ilen,int(bit_size(answer)))
   endif
   do pos=1,len(string)
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits16* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits16
!-----------------------------------------------------------------------------------------------------------------------------------
function setbits32(string) result(answer)
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
integer(kind=int32)          :: answer
character(len=32),intent(in) :: string
integer                      :: pos
integer                      :: ilen
   answer=0_int32
   ilen=len(string)
   if(ilen.ne.bit_size(answer))then
      write(stderr,*)'*setbits32* wrong string length =',ilen
      ilen=min(ilen,int(bit_size(answer)))
   endif
   do pos=1,len(string)
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits32* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits32
!-----------------------------------------------------------------------------------------------------------------------------------
function setbits64(string) result(answer)
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
integer(kind=int64)          :: answer
character(len=64),intent(in) :: string
integer                      :: pos
integer                      :: ilen
   answer=0_int64
   ilen=len(string)
   if(ilen.ne.bit_size(answer))then
      write(stderr,*)'*setbits64* wrong string length =',ilen
      ilen=min(ilen,int(bit_size(answer)))
   endif
   do pos=1,len(string)
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits64* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits64
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     msg(3f) - [M_strings] converts any standard scalar type to a string
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!     function msg(g1,g2g3,g4,g5,g6,g7,g8,g9,sep)
!!
!!      class(*),intent(in),optional  :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!      character(len=*),intent(in),optional :: sep
!!      character(len=:),allocatable :: msg
!!
!!##DESCRIPTION
!!     msg(3f) builds a space-separated string from up to nine scalar values.
!!
!!##OPTIONS
!!     g[1-9]  optional value to print the value of after the message. May
!!             be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!             or CHARACTER.
!!     sep     separator between values. Defaults to a space
!!
!!##RETURNS
!!     msg     description to print
!!
!!##EXAMPLES
!!
!!
!!   Sample program:
!!
!!        program demo_msg
!!        use M_strings, only : msg
!!        implicit none
!!        character(len=:),allocatable :: pr
!!        character(len=:),allocatable :: frmt
!!        integer                      :: biggest
!!
!!        pr=msg('HUGE(3f) integers',huge(0),&
!!        & 'and real',huge(0.0),'and double',huge(0.0d0))
!!        write(*,'(a)')pr
!!        pr=msg('real            :',&
!!         & huge(0.0),0.0,12345.6789,tiny(0.0) )
!!        write(*,'(a)')pr
!!        pr=msg('doubleprecision :',&
!!         & huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!        write(*,'(a)')pr
!!        pr=msg('complex         :',&
!!         & cmplx(huge(0.0),tiny(0.0)) )
!!        write(*,'(a)')pr
!!
!!        ! create a format on the fly
!!        biggest=huge(0)
!!        frmt=msg('(*(i',int(log10(real(biggest))),':,1x))',sep='')
!!        write(*,*)'format=',frmt
!!
!!        ! although it will often work, using msg(3f) in an I/O statement
!!        ! is not recommended
!!        write(*,*)msg('program will now stop')
!!
!!        end program demo_msg
!!
!!   Output
!!
!!       HUGE(3f) integers 2147483647 and real 3.40282347E+38
!!       and double 1.7976931348623157E+308
!!       real            : 3.40282347E+38 0.00000000
!!       12345.6787 1.17549435E-38
!!       doubleprecision : 1.7976931348623157E+308 0.0000000000000000
!!       12345.678900000001 2.2250738585072014E-308
!!       complex         : (3.40282347E+38,1.17549435E-38)
!!        format=(*(i9:,1x))
!!        program will now stop
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function msg_scalar(generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)
implicit none

! ident_78="@(#)M_strings::msg_scalar(3fp): writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic1 ,generic2 ,generic3 ,generic4 ,generic5
class(*),intent(in),optional  :: generic6 ,generic7 ,generic8 ,generic9
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=' '
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   msg_scalar=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      !*!type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(l1)') generic
      type is (character(len=*));       write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_scalar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function msg_one(generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)
implicit none

! ident_79="@(#)M_strings::msg_one(3fp): writes a message to a string composed of any standard one dimensional types"

class(*),intent(in)           :: generic1(:)
class(*),intent(in),optional  :: generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable   :: sep_local
character(len=:), allocatable :: msg_one
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=' '
   call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   msg_one=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in),optional :: generic(:)
integer :: i
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int16));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int32));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int64));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (real(kind=real32));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real64));      write(line(istart:),'("[",*(1pg0,1x))') generic
      !*!type is (real(kind=real128));     write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (logical);                write(line(istart:),'("[",*(l1,1x))') generic
      type is (character(len=*));       write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//"]"//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    split2020(3f) - parse a string into tokens
!!
!!##SYNOPSIS
!!
!!   TOKEN form
!!
!!    subroutine split2020 (string, set, tokens, separator)
!!    character(len=*),intent(in) :: string
!!    character(len=*),intent(in) :: set
!!    character(len=:),allocatable,intent(out) :: tokens(:)
!!    character(len=1),allocatable,intent(out),optional :: separator(:)
!!
!!   BOUNDS ARRAY form
!!
!!    subroutine split2020 (string, set, first, last)
!!    character(len=*),intent(in) :: string
!!    character(len=*),intent(in) :: set
!!    integer,allocatable,intent(out) :: first(:)
!!    integer,allocatable,intent(out) :: last(:)
!!
!!   STEP THROUGH BY POSITION form
!!
!!    subroutine split2020 (string, set, pos [, back])
!!    character(len=*),intent(in) :: string
!!    character(len=*),intent(in) :: set
!!    integer,intent(inout)       :: pos
!!    logical,intent(in),optional :: back
!!
!!##DESCRIPTION
!!    Parse a string into tokens. STRING, SET, TOKENS and SEPARATOR must
!!    all be of the same CHARACTER kind type parameter.
!!
!!##OPTIONS
!!    STRING      string to break into tokens
!!
!!    SET         Each character in SET is a token delimiter. A
!!                sequence of zero or more characters in STRING delimited by
!!                any token delimiter, or the beginning or end of STRING,
!!                comprise a token. Thus, two consecutive token delimiters
!!                in STRING, or a token delimiter in the first or last
!!                character of STRING, indicate a token with zero length.
!!
!!                ??? how about if null defaults to all whitespace characters
!!
!!    TOKENS      It is allocated with the lower bound equal to
!!                one and the upper bound equal to the number of tokens in
!!                STRING, and with character length equal to the length of
!!                the longest token. The tokens in STRING are assigned by
!!                intrinsic assignment, in the order found, to the elements
!!                of TOKENS, in array element order.
!!
!!                ???If input is null it still must be of size 1?
!!
!!    SEPARATOR   Each element in SEPARATOR(i) is assigned the value of
!!                the ith token delimiter in STRING.
!!                It is allocated with the lower bound equal to
!!                one and the upper bound equal to one less than the number
!!                of tokens in STRING, and with character length equal to
!!                one.
!!
!!                ???one less than? '' ' '
!!
!!    FIRST     It is allocated with the lower bound equal to one and the
!!              upper bound equal to the number of tokens in STRING. Each
!!              element is assigned, in array element order, the starting
!!              position of each token in STRING, in the order found. If a
!!              token has zero length, the starting position is equal to one
!!              if the token is at the beginning of STRING, and one greater
!!              than the position of the preceding delimiter otherwise.
!!
!!    LAST      It is allocated with the lower bound equal to one and the
!!              upper bound equal to the number of tokens in STRING. Each
!!              element is assigned, in array element order, the ending
!!              position of each token in STRING, in the order found. If
!!              a token has zero length, the ending position is one less
!!              than the starting position.
!!
!!    POS       If BACK is present with the value .TRUE., the value
!!              of POS shall be in the range 0 < POS     LEN (STRING)+1;
!!              otherwise it shall be in the range 0     POS LEN (STRING).
!!
!!              If BACK is absent or is present with the value .FALSE., POS
!!              is assigned the position of the leftmost token delimiter in
!!              STRING whose position is greater than POS, or if there is
!!              no such character, it is assigned a value one greater than
!!              the length of STRING. This identifies a token with starting
!!              position one greater than the value of POS on invocation,
!!              and ending position one less than the value of POS on return.
!!
!!              If BACK is present with the value true, POS is assigned the
!!              position of the rightmost token delimiter in STRING whose
!!              position is less than POS, or if there is no such character,
!!              it is assigned the value zero. This identifies a token with
!!              ending position one less than the value of POS on invocation,
!!              and starting position one greater than the value of POS
!!              on return.
!!
!!              When SPLIT is invoked with a value for POS of
!!              1 <= POS <= LEN(STRING) and STRING(POS:POS) is not a
!!              token delimiter present in SET, the token identified by
!!              SPLIT does not comprise a complete token as described in the
!!              description of the SET argument, but rather a partial token.
!!
!!    BACK      shall be a logical scalar. It is an INTENT (IN) argument. If
!!              POS does not appear and BACK is present with the value true,
!!              STRING is scanned backwards for tokens starting from the
!!              end. If POS does not appear and BACK is absent or present
!!              with the value false, STRING is scanned forwards for tokens
!!              starting from the beginning.
!!
!!##EXAMPLES
!!
!! Sample of uses
!!
!!    program demo_sort2020
!!    use M_strings, only : split2020
!!    implicit none
!!    character(len=*),parameter :: gen='(*("[",g0,"]":,","))'
!!
!!     ! Execution of TOKEN form
!!     block
!!       character (len=:), allocatable :: string
!!       character (len=:), allocatable :: tokens(:)
!!       character (len=*),parameter :: set = " ,"
!!       string = 'first,second,third'
!!       call split2020(string, set, tokens )
!!       write(*,gen)tokens
!!
!!     ! assigns the value ['first ','second','third ' ]
!!     ! to TOKENS.
!!     endblock
!!
!!     ! Execution of BOUNDS form
!!
!!     block
!!       character (len=:), allocatable :: string
!!       character (len=*),parameter :: set = " ,"
!!       integer, allocatable        :: first(:), last(:)
!!       string =    'first,second,,forth'
!!       call split2020 (string, set, first, last)
!!       write(*,gen)first
!!       write(*,gen)last
!!
!!     ! will assign the value [ 1, 7, 14, 15 ] to FIRST,
!!     ! and the value [ 5, 12, 13, 19 ] to LAST.
!!     endblock
!!
!!     ! Execution of STEP form
!!     block
!!       character (len=:), allocatable :: string
!!       character (len=*),parameter :: set = " ,"
!!       integer :: p, istart, iend
!!       string = " one,   last  example  "
!!       do while (p < len(string))
!!         istart = p + 1
!!         call split2020 (string, set, p)
!!         iend=p-1
!!         if(iend.gt.istart)then
!!            print '(t3,a,1x,i0,1x,i0)', string (istart:iend),istart,iend
!!         endif
!!       enddo
!!     endblock
!!    end program demo_sort2020
!!
!!   Results:
!!
!!    [first ],[second],[third ]
!!    [1],[7],[14],[15]
!!    [5],[12],[13],[19]
!!      one 2 4
!!      last 9 12
!!      example 15 21
!!
!!      > ??? option to skip adjacent delimiters (not return null tokens) common with whitespace
!!      > ??? quoted strings, especially CSV both " and ', Fortran adjacent is insert versus other rules
!!      > ??? escape character like \\
!!      > ??? multi-character delimiters like \\n, \\t,
!!      > ??? regular expression separator
!!
!!##AUTHOR
!!    Milan Curcic, "milancurcic@hey.com"
!!
!!##LICENSE
!!    MIT
!!
!!##VERSION
!!    version 0.1.0, copyright 2020, Milan Curcic
  pure subroutine split_tokens(string, set, tokens, separator)
    !! Splits a string into tokens using characters in set as token delimiters.
    !! If present, separator contains the array of token delimiters.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    character(:), allocatable, intent(out) :: tokens(:)
    character, allocatable, intent(out), optional :: separator(:)

    integer, allocatable :: first(:), last(:)
    integer :: n

    call split2020(string, set, first, last)
    allocate(character(len=maxval(last - first) + 1) :: tokens(size(first)))

    do concurrent (n = 1:size(tokens))
      tokens(n) = string(first(n):last(n))
    enddo

    if (present(separator)) then
      allocate(separator(size(tokens) - 1))
      do concurrent (n = 1:size(tokens) - 1)
        separator(n) = string(first(n+1)-1:first(n+1)-1)
      enddo
    endif

  end subroutine split_tokens
!===================================================================================================================================
  pure subroutine split_first_last(string, set, first, last)
    !! Computes the first and last indices of tokens in input string, delimited
    !! by the characters in set, and stores them into first and last output
    !! arrays.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    integer, allocatable, intent(out) :: first(:)
    integer, allocatable, intent(out) :: last(:)

    character :: set_array(len(set))
    logical, dimension(len(string)) :: is_first, is_last, is_separator
    integer :: n, slen

    slen = len(string)

    do concurrent (n = 1:len(set))
      set_array(n) = set(n:n)
    enddo

    do concurrent (n = 1:slen)
      is_separator(n) = any(string(n:n) == set_array)
    enddo

    is_first = .false.
    is_last = .false.

    if (.not. is_separator(1)) is_first(1) = .true.

    do concurrent (n = 2:slen-1)
      if (.not. is_separator(n)) then
        if (is_separator(n - 1)) is_first(n) = .true.
        if (is_separator(n + 1)) is_last(n) = .true.
      else
        if (is_separator(n - 1)) then
          is_first(n) = .true.
          is_last(n-1) = .true.
        endif
      endif
    enddo

    if (.not. is_separator(slen)) is_last(slen) = .true.

    first = pack([(n, n = 1, slen)], is_first)
    last = pack([(n, n = 1, slen)], is_last)

  end subroutine split_first_last
!===================================================================================================================================
  pure subroutine split_pos(string, set, pos, back)
    !! If back is absent, computes the leftmost token delimiter in string whose
    !! position is > pos. If back is present and true, computes the rightmost
    !! token delimiter in string whose position is < pos. The result is stored
    !! in pos.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    integer, intent(in out) :: pos
    logical, intent(in), optional :: back

    logical :: backward
    character :: set_array(len(set))
    integer :: n, result_pos

    !TODO use optval when implemented in stdlib
    !backward = optval(back, .false.)
    backward = .false.
    if (present(back)) backward = back

    do concurrent (n = 1:len(set))
      set_array(n) = set(n:n)
    enddo

    if (backward) then
      result_pos = 0
      do n = pos - 1, 1, -1
        if (any(string(n:n) == set_array)) then
          result_pos = n
          exit
        endif
      enddo
    else
      result_pos = len(string) + 1
      do n = pos + 1, len(string)
        if (any(string(n:n) == set_array)) then
          result_pos = n
          exit
        endif
      enddo
    endif

    pos = result_pos

  end subroutine split_pos
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
  pure function string_tokens(string, set) result(tokens)
    !! Splits a string into tokens using characters in set as token delimiters.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    character(:), allocatable :: tokens(:)
    call split_tokens(string, set, tokens)
  end function string_tokens
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Duplicate the M_journal module in condensed form for now so can be stand-alone on GITHUB
!                                                                     ll
!                                                                      l
!    j                                                                 l
!                                                                      l
!    j                                                                 l
!    j        oooooo    u      u   r rrrrrr   n nnnnn      aaaa        l
!    j       o      o   u      u   rr         nn     n         a       l
!    j       o      o   u      u   r          n      n    aaaaaa       l
! j  j       o      o   u      u   r          n      n   a     a       l
!  jj         oooooo     uuuuuu u  r          n      n    aaaaa a      l
!
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! @(#) place-holder for journal module
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine where_write_message(where,msg)

!@(#) M_journal::where_write_message(3fp): basic message routine used for journal files

character(len=*),intent(in)  :: where
character(len=*),intent(in)  :: msg
logical,save                       :: trailopen=.false.
integer,save                       :: itrail
character,save                     :: comment='#'
integer                            :: i
integer                            :: ios
integer                            :: times             ! number of times written to stdout
character(len=3)                   :: adv               ! whether remaining writes from this call use advancing I/O

character(len=:),allocatable,save  :: prefix_template   ! string to run thru now_ex(3f) to make prefix
character(len=:),allocatable       :: prefix            ! the prefix string to add to output
logical,save                       :: prefix_it=.false. ! flag whether time prefix mode is on or not
character(len=4096)                :: mssge
!-----------------------------------------------------------------------------------------------------------------------------------
   adv='yes'
!-----------------------------------------------------------------------------------------------------------------------------------
   prefix=''
!-----------------------------------------------------------------------------------------------------------------------------------
   times=0
   do i=1,len_trim(where)
      select case(where(i:i))
      case('T','t')
         if(trailopen) then
            write(itrail,'(a)',advance=adv)prefix//trim(msg)
         !!elseif(times.eq.0)then
         !!   write(stdout,'(a)',advance=adv)prefix//trim(msg)
         !!   times=times+1
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('S','s')
         write(stdout,'(a)',advance=adv)prefix//trim(msg)
         times=times+1
      !-----------------------------------------------------------------------------------------------------------------------------
      case('E','e')
         write(stderr,'(a)',advance=adv)prefix//trim(msg)
         times=times+1
      !-----------------------------------------------------------------------------------------------------------------------------
      case('+'); adv='no'
      !-----------------------------------------------------------------------------------------------------------------------------
      case('>'); debug=.true.
      !-----------------------------------------------------------------------------------------------------------------------------
      case('<'); debug=.false.
      !-----------------------------------------------------------------------------------------------------------------------------
      case('%')                       ! setting timestamp prefix
         if(msg.eq.'')then            ! if message is blank turn off prefix
            prefix_it=.false.
         else                         ! store message as string to pass to now_ex() on subsequent calls to make prefix
            prefix_template=msg
            prefix_it=.true.
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('N')                                                   ! new name for stdout
         if(msg.ne.' '.and.msg.ne.'#N#'.and.msg.ne.'"#N#"')then   ! if filename not special or blank open new file
            close(unit=last_int,iostat=ios)
            open(unit=last_int,file=adjustl(trim(msg)),iostat=ios)
            if(ios.eq.0)then
               stdout=last_int
            else
               write(*,*)'*journal* error opening redirected output file, ioerr=',ios
               write(*,*)'*journal* msg='//trim(msg)
            endif
         elseif(msg.eq.' ')then
            close(unit=last_int,iostat=ios)
            stdout=6
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('C','c')
         if(trailopen)then
            write(itrail,'(3a)',advance=adv)prefix,comment,trim(msg)
         elseif(times.eq.0)then
            !! write(stdout,'(2a)',advance=adv)prefix,trim(msg)
            !! times=times+1
         endif
      case('D','d')
         if(debug)then
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)prefix,comment,'DEBUG: ',trim(msg)
            elseif(times.eq.0)then
               write(stdout,'(3a)',advance=adv)prefix,'DEBUG:',trim(msg)
               times=times+1
            endif
         endif
      case('F','f')
         flush(unit=itrail,iostat=ios,iomsg=mssge)
         if(ios.ne.0)then
            write(*,'(a)') trim(mssge)
         endif
      case('A','a')
         if(msg.ne.'')then
            open(newunit=itrail,status='unknown',access='sequential',file=adjustl(trim(msg)),&
            & form='formatted',iostat=ios,position='append')
            trailopen=.true.
         endif
      case('O','o')
         if(msg.ne.'')then
            open(newunit=itrail,status='unknown',access='sequential', file=adjustl(trim(msg)),form='formatted',iostat=ios)
            trailopen=.true.
         else
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)prefix,comment,'closing trail file:',trim(msg)
            endif
            close(unit=itrail,iostat=ios)
            trailopen=.false.
         endif
      case default
         write(stdout,'(a)',advance=adv)'*journal* bad WHERE value '//trim(where)//' when msg=['//trim(msg)//']'
      end select
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine where_write_message
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine flush_trail()
call where_write_message('F','IGNORE THIS STRING')
end subroutine flush_trail
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set_stdout_lun(iounit)
integer,intent(in)                   :: iounit
   stdout=iounit
end subroutine set_stdout_lun
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine where_write_message_all(where, g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, nospace)
implicit none

!$(#) M_journal::where_write_message_all(3f): writes a message to a string composed of any standard scalar types

character(len=*),intent(in)   :: where
class(*),intent(in)           :: g0
class(*),intent(in),optional  :: g1, g2, g3, g4, g5, g6, g7, g8 ,g9
logical,intent(in),optional   :: nospace
!!call where_write_message(where,str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9,nospace))
end subroutine where_write_message_all
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_message_only(message)

!$(#) M_journal::write_message_only(3fp): calls JOURNAL('sc',message)

character(len=*),intent(in)          :: message
!-----------------------------------------------------------------------------------------------------------------------------------
   call where_write_message('sc',trim(message))
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine write_message_only
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function str_scalar(generic0, generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9, &
                  & generica, genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj, &
                  & sep)
implicit none
class(*),intent(in),optional  :: generic0, generic1, generic2, generic3, generic4
class(*),intent(in),optional  :: generic5, generic6, generic7, generic8, generic9
class(*),intent(in),optional  :: generica, genericb, genericc, genericd, generice
class(*),intent(in),optional  :: genericf, genericg, generich, generici, genericj
character(len=*),intent(in),optional :: sep
character(len=:), allocatable :: str_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
character(len=:),allocatable  :: sep_local
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=''
   if(present(generic0))call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   if(present(generica))call print_generic(generica)
   if(present(genericb))call print_generic(genericb)
   if(present(genericc))call print_generic(genericc)
   if(present(genericd))call print_generic(genericd)
   if(present(generice))call print_generic(generice)
   if(present(genericf))call print_generic(genericf)
   if(present(genericg))call print_generic(genericg)
   if(present(generich))call print_generic(generich)
   if(present(generici))call print_generic(generici)
   if(present(genericj))call print_generic(genericj)
   str_scalar=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      !x!type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(l1)') generic
      type is (character(len=*));       write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic

end function str_scalar
!===================================================================================================================================
function str_one(generic0,generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)
implicit none
class(*),intent(in)           :: generic0(:)
class(*),intent(in),optional  :: generic1(:), generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: str_one
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=' '
   call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   str_one=trim(line)
contains

subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in),optional :: generic(:)
integer :: i
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int16));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int32));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int64));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (real(kind=real32));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real64));      write(line(istart:),'("[",*(1pg0,1x))') generic
      !type is (real(kind=real128));     write(line(istart:),'("[",*(1pg0,1x))') generic
      !type is (real(kind=real256));     write(error_unit,'(1pg0)',advance='no') generic
      type is (logical);                write(line(istart:),'("[",*(l1,1x))') generic
      type is (character(len=*));       write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
      class default
         stop 'unknown type in *print_generic*'
   end select
   line=trim(line)//"]"//sep_local
   istart=len_trim(line)+increment
end subroutine print_generic

end function str_one
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function lowercase(str) result(lcstr)

! convert string to lower case leaving quoted strings as is

character (len=*):: str
character (len=len_trim(str)):: lcstr
integer :: ilen
integer :: ioffset
integer :: iquote
integer :: i
integer :: iav
integer :: iqc

ilen=len_trim(str)
ioffset=iachar('A')-iachar('a')
iquote=0
lcstr=str
do i=1,ilen
  iav=iachar(str(i:i))
  if(iquote==0 .and. (iav==34 .or.iav==39)) then
    iquote=1
    iqc=iav
    cycle
  endif
  if(iquote==1 .and. iav==iqc) then
    iquote=0
    cycle
  endif
  if (iquote==1) cycle
  if(iav >= iachar('A') .and. iav <= iachar('Z')) then
    lcstr(i:i)=achar(iav-ioffset)
  else
    lcstr(i:i)=str(i:i)
  endif
enddo

end function lowercase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function uppercase(str) result(ucstr)

! convert string to upper case leaving quoted strings as is

character (len=*):: str
character (len=len_trim(str)):: ucstr
integer :: ilen
integer :: ioffset
integer :: iquote
integer :: i
integer :: iav
integer :: iqc

ilen=len_trim(str)
ioffset=iachar('A')-iachar('a')
iquote=0
ucstr=str
do i=1,ilen
  iav=iachar(str(i:i))
  if(iquote==0 .and. (iav==34 .or.iav==39)) then
    iquote=1
    iqc=iav
    cycle
  endif
  if(iquote==1 .and. iav==iqc) then
    iquote=0
    cycle
  endif
  if (iquote==1) cycle
  if(iav >= iachar('a') .and. iav <= iachar('z')) then
    ucstr(i:i)=achar(iav+ioffset)
  else
    ucstr(i:i)=str(i:i)
  endif
enddo

end function uppercase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine matching_delimiter(str,ipos,imatch)

! Sets imatch to the position in string of the delimiter matching the delimiter
! in position ipos. Allowable delimiters are (), [], {}, <>.

character(len=*) :: str
character :: delim1,delim2,ch
integer :: ipos
integer :: imatch
integer :: lenstr
integer :: idelim2
integer :: istart, iend
integer :: inc
integer :: isum
integer :: i

lenstr=len_trim(str)
delim1=str(ipos:ipos)
select case(delim1)
   case('(')
      idelim2=iachar(delim1)+1
      istart=ipos+1
      iend=lenstr
      inc=1
   case(')')
      idelim2=iachar(delim1)-1
      istart=ipos-1
      iend=1
      inc=-1
   case('[','{','<')
      idelim2=iachar(delim1)+2
      istart=ipos+1
      iend=lenstr
      inc=1
   case(']','}','>')
      idelim2=iachar(delim1)-2
      istart=ipos-1
      iend=1
      inc=-1
   case default
      write(*,*) delim1,' is not a valid delimiter'
      return
end select
if(istart < 1 .or. istart > lenstr) then
   write(*,*) delim1,' has no matching delimiter'
   return
endif
delim2=achar(idelim2) ! matching delimiter

isum=1
do i=istart,iend,inc
   ch=str(i:i)
   if(ch /= delim1 .and. ch /= delim2) cycle
   if(ch == delim1) isum=isum+1
   if(ch == delim2) isum=isum-1
   if(isum == 0) exit
enddo
if(isum /= 0) then
   write(*,*) delim1,' has no matching delimiter'
   return
endif
imatch=i

end subroutine matching_delimiter
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_strings
 
 
!>>>>> build/dependencies/M_list/src/M_list.f90
!>
!!##NAME
!!    M_list(3f) - [M_list] maintain simple lists
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    use M_list, only : insert, replace, remove
!!    use M_list, only : dictionary
!!
!!##DESCRIPTION
!!
!!    The M_list(3fm) module allows for maintaining an array as a sorted
!!    list. An example is given that creates a keyword-value dictionary
!!    using the lists.
!!
!!    The lists are maintained as simple allocatable arrays. Each time an
!!    entry is added or deleted the array is re-allocated. Because of the
!!    expense of reallocating the data these routines are best suited for
!!    maintaining small lists that do not change size frequently.
!!
!!    The advantage is that the dictionary components are simple arrays of
!!    intrinsic types which can be easily accessed with standard routines.
!!
!!    BASIC LIST
!!
!!    subroutine locate(list,value,place,ier,errmsg)  finds the index where a
!!                                                    value is found or should
!!                                                    be in a sorted array and
!!                                                    flag if the value exists
!!                                                    already
!!    subroutine insert(list,value,place)     insert entry into an allocatable
!!                                            array at specified position
!!    subroutine replace(list,value,place)    replace entry in an allocatable
!!                                            array at specified position
!!    subroutine remove(list,place)           remove entry from an allocatable
!!                                            array at specified position
!!
!!    BASIC DICTIONARY
!!
!!    Due to bugs in gfortran up to at least 7.4.0, this next section
!!    does not work.
!!
!!    type dictionary
!!
!!       character(len=:),allocatable :: key(:)
!!       character(len=:),allocatable :: value(:)
!!       integer,allocatable          :: count(:)
!!
!!    %get      get value from type(dictionary) given an existing key
!!    %set      set or replace value for type(dictionary) given a key
!!    %del      delete an existing key from type(dictionary)
!!    %clr      empty a type(dictionary)
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_M_list
!!    use M_list, only : insert, locate, replace, remove
!!    ! create a dictionary with character keywords, values, and value lengths
!!    ! using the routines for maintaining a list
!!
!!     use M_list, only : locate, insert, replace
!!     implicit none
!!     character(len=:),allocatable   :: keywords(:)
!!     character(len=:),allocatable   :: values(:)
!!     integer,allocatable            :: counts(:)
!!     integer                        :: i
!!     ! insert and replace entries
!!     call update('b','value of b')
!!     call update('a','value of a')
!!     call update('c','value of c')
!!     call update('c','value of c again')
!!     call update('d','value of d')
!!     call update('a','value of a again')
!!     ! show array
!!     write(*,'(*(a,"==>","[",a,"]",/))')(trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords))
!!     ! remove some entries
!!     call update('a')
!!     call update('c')
!!     write(*,'(*(a,"==>","[",a,"]",/))')(trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords))
!!     ! get some values
!!     write(*,*)'get b=>',get('b')
!!     write(*,*)'get d=>',get('d')
!!     write(*,*)'get notthere=>',get('notthere')
!!
!!     contains
!!     subroutine update(key,valin)
!!     character(len=*),intent(in)           :: key
!!     character(len=*),intent(in),optional  :: valin
!!     integer                               :: place
!!     integer                               :: ilen
!!     character(len=:),allocatable          :: val
!!     if(present(valin))then
!!        val=valin
!!        ilen=len_trim(val)
!!        ! find where string is or should be
!!        call locate(keywords,key,place)
!!        ! if string was not found insert it
!!        if(place.lt.1)then
!!           call insert(keywords,key,iabs(place))
!!           call insert(values,val,iabs(place))
!!           call insert(counts,ilen,iabs(place))
!!        else
!!           call replace(values,val,place)
!!           call replace(counts,ilen,place)
!!        endif
!!     else
!!        call locate(keywords,key,place)
!!        if(place.gt.0)then
!!           call remove(keywords,place)
!!           call remove(values,place)
!!           call remove(counts,place)
!!        endif
!!     endif
!!     end subroutine update
!!     function get(key) result(valout)
!!     character(len=*),intent(in)   :: key
!!     character(len=:),allocatable  :: valout
!!     integer                       :: place
!!        ! find where string is or should be
!!        call locate(keywords,key,place)
!!        if(place.lt.1)then
!!           valout=''
!!        else
!!           valout=values(place)(:counts(place))
!!        endif
!!     end function get
!!    end program demo_M_list
!!
!!   Results:
!!
!!    d==>[value of d]
!!    c==>[value of c again]
!!    b==>[value of b]
!!    a==>[value of a again]
!!
!!    d==>[value of d]
!!    b==>[value of b]
!!
!!     get b=>value of b
!!     get d=>value of d
!!     get notthere=>
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_list
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,stdout=>OUTPUT_UNIT    ! access computing environment
implicit none
private

public locate        ! [M_list] find PLACE in sorted character array where value can be found or should be placed
   private locate_c
   private locate_d
   private locate_r
   private locate_i
public insert        ! [M_list] insert entry into a sorted allocatable array at specified position
   private insert_c
   private insert_d
   private insert_r
   private insert_i
   private insert_l
public replace       ! [M_list] replace entry by index from a sorted allocatable array if it is present
   private replace_c
   private replace_d
   private replace_r
   private replace_i
   private replace_l
public remove        ! [M_list] delete entry by index from a sorted allocatable array if it is present
   private remove_c
   private remove_d
   private remove_r
   private remove_i
   private remove_l

! ident_1="@(#)M_list::locate(3f): Generic subroutine locates where element is or should be in sorted allocatable array"
interface locate
   module procedure locate_c, locate_d, locate_r, locate_i
end interface

! ident_2="@(#)M_list::insert(3f): Generic subroutine inserts element into allocatable array at specified position"
interface insert
   module procedure insert_c, insert_d, insert_r, insert_i, insert_l
end interface

! ident_3="@(#)M_list::replace(3f): Generic subroutine replaces element from allocatable array at specified position"
interface replace
   module procedure replace_c, replace_d, replace_r, replace_i, replace_l
end interface

! ident_4="@(#)M_list::remove(3f): Generic subroutine deletes element from allocatable array at specified position"
interface remove
   module procedure remove_c, remove_d, remove_r, remove_i, remove_l
end interface

!-----------------------------------------------------------------------------------------------------------------------------------
public dictionary

type dictionary
   character(len=:),allocatable :: key(:)
   character(len=:),allocatable :: value(:)
   integer,allocatable          :: count(:)
   contains
      procedure,public :: get => dict_get
      procedure,public :: set => dict_add    ! insert entry by name into a sorted allocatable character array if it is not present
      procedure,public :: del => dict_delete ! delete entry by name from a sorted allocatable character array if it is present
end type dictionary

logical,save :: debug=.false.
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    locate(3f) - [M_list] finds the index where a string is found or should be in a sorted array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine locate(list,value,place,ier,errmsg)
!!
!!    character(len=:)|doubleprecision|real|integer,allocatable :: list(:)
!!    character(len=*)|doubleprecision|real|integer,intent(in)  :: value
!!    integer, intent(out)                  :: PLACE
!!
!!    integer, intent(out),optional         :: IER
!!    character(len=*),intent(out),optional :: ERRMSG
!!
!!##DESCRIPTION
!!
!!    LOCATE(3f) finds the index where the VALUE is found or should
!!    be found in an array. The array must be sorted in descending
!!    order (highest at top). If VALUE is not found it returns the index
!!    where the name should be placed at with a negative sign.
!!
!!    The array and list must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL,INTEGER)
!!
!!##OPTIONS
!!
!!    VALUE         the value to locate in the list.
!!    LIST          is the list array.
!!
!!##RETURNS
!!    PLACE         is the subscript that the entry was found at if it is
!!                  greater than zero(0).
!!
!!                  If PLACE is negative, the absolute value of
!!                  PLACE indicates the subscript value where the
!!                  new entry should be placed in order to keep the
!!                  list alphabetized.
!!
!!    IER           is zero(0) if no error occurs.
!!                  If an error occurs and IER is not
!!                  present, the program is stopped.
!!
!!    ERRMSG        description of any error
!!
!!##EXAMPLES
!!
!!
!!    Find if a string is in a sorted array, and insert the string into
!!    the list if it is not present ...
!!
!!     program demo_locate
!!     use M_sort, only : sort_shell
!!     use M_list, only : locate
!!     implicit none
!!     character(len=:),allocatable  :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     call update(arr,'b')
!!     call update(arr,'[')
!!     call update(arr,'c')
!!     call update(arr,'ZZ')
!!     call update(arr,'ZZZZ')
!!     call update(arr,'z')
!!
!!     contains
!!     subroutine update(arr,string)
!!     character(len=:),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, plus, ii, end
!!     ! find where string is or should be
!!     call locate(arr,string,place)
!!     write(*,*)'for "'//string//'" index is ',place, size(arr)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        plus=abs(place)
!!        ii=len(arr)
!!        end=size(arr)
!!        ! empty array
!!        if(end.eq.0)then
!!           arr=[character(len=ii) :: string ]
!!        ! put in front of array
!!        elseif(plus.eq.1)then
!!           arr=[character(len=ii) :: string, arr]
!!        ! put at end of array
!!        elseif(plus.eq.end)then
!!           arr=[character(len=ii) :: arr, string ]
!!        ! put in middle of array
!!        else
!!           arr=[character(len=ii) :: arr(:plus-1), string,arr(plus:) ]
!!        endif
!!        ! show array
!!        write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     endif
!!     end subroutine update
!!     end program demo_locate
!!
!!   Results:
!!
!!     for "b" index is            2           5
!!     for "[" index is           -4           5
!!    SIZE=5 xxx,b,aaa,[,ZZZ,
!!     for "c" index is           -2           6
!!    SIZE=6 xxx,c,b,aaa,[,ZZZ,
!!     for "ZZ" index is           -7           7
!!    SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     for "ZZZZ" index is           -6           8
!!    SIZE=8 xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!     for "z" index is           -1           9
!!    SIZE=9 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine locate_c(list,value,place,ier,errmsg)

! ident_5="@(#)M_list::locate_c(3f): find PLACE in sorted character array where VALUE can be found or should be placed"

character(len=*),intent(in)             :: value
integer,intent(out)                     :: place
character(len=:),allocatable            :: list(:)
integer,intent(out),optional            :: ier
character(len=*),intent(out),optional   :: errmsg
integer                                 :: i
character(len=:),allocatable            :: message
integer                                 :: arraysize
integer                                 :: maxtry
integer                                 :: imin, imax
integer                                 :: error
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   arraysize=size(list)
   if(debug)write(stderr,*)'*locate_c* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',trim(value)//' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(stderr,*)'*locate_c* END PLACE=',place,' ARRAYSIZE=',size(list),' LENGTH=',len(list)
end subroutine locate_c
subroutine locate_d(list,value,place,ier,errmsg)

! ident_6="@(#)M_list::locate_d(3f): find PLACE in sorted doubleprecision array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

doubleprecision,allocatable            :: list(:)
doubleprecision,intent(in)             :: value
integer,intent(out)                    :: place
integer,intent(out),optional           :: ier
character(len=*),intent(out),optional  :: errmsg

integer                                :: i
character(len=:),allocatable           :: message
integer                                :: arraysize
integer                                :: maxtry
integer                                :: imin, imax
integer                                :: error

   if(.not.allocated(list))then
      list=[doubleprecision :: ]
   endif
   arraysize=size(list)
   if(debug)write(stderr,*)'*locate_d* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',value,' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(stderr,*)'*locate_d* END PLACE=',place,' ARRAYSIZE=',size(list)
end subroutine locate_d
subroutine locate_r(list,value,place,ier,errmsg)

! ident_7="@(#)M_list::locate_r(3f): find PLACE in sorted real array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

real,allocatable                       :: list(:)
real,intent(in)                        :: value
integer,intent(out)                    :: place
integer,intent(out),optional           :: ier
character(len=*),intent(out),optional  :: errmsg

integer                                :: i
character(len=:),allocatable           :: message
integer                                :: arraysize
integer                                :: maxtry
integer                                :: imin, imax
integer                                :: error

   if(.not.allocated(list))then
      list=[real :: ]
   endif
   arraysize=size(list)
   if(debug)write(stderr,*)'*locate_r* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',value,' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(stderr,*)'*locate_r* END PLACE=',place,' ARRAYSIZE=',size(list)
end subroutine locate_r
subroutine locate_i(list,value,place,ier,errmsg)

! ident_8="@(#)M_list::locate_i(3f): find PLACE in sorted integer array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

integer,allocatable                    :: list(:)
integer,intent(in)                     :: value
integer,intent(out)                    :: place
integer,intent(out),optional           :: ier
character(len=*),intent(out),optional  :: errmsg

integer                                :: i
character(len=:),allocatable           :: message
integer                                :: arraysize
integer                                :: maxtry
integer                                :: imin, imax
integer                                :: error

   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   arraysize=size(list)
   if(debug)write(stderr,*)'*locate_i* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',value,' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(stderr,*)'*locate_i* END PLACE=',place,' ARRAYSIZE=',size(list)
end subroutine locate_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    remove(3f) - [M_list] remove entry from an allocatable array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine remove(list,place)
!!
!!    character(len=:)|doubleprecision|real|integer,intent(inout) :: list(:)
!!    integer, intent(out) :: PLACE
!!
!!##DESCRIPTION
!!
!!    Remove a value from an allocatable array at the specified index.
!!    The array is assumed to be sorted in descending order. It may be of
!!    type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER.
!!
!!##OPTIONS
!!
!!    list    is the list array.
!!    PLACE   is the subscript for the entry that should be removed
!!
!!##EXAMPLES
!!
!!
!!    Sample program
!!
!!     program demo_remove
!!     use M_sort, only : sort_shell
!!     use M_list, only : locate, remove
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!     integer                       :: end
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'Z', 'aaa', 'b', 'b', 'ab', 'bb', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove(arr,1)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove(arr,4)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end program demo_remove
!!
!!   Results:
!!
!!    Expected output
!!
!!     SIZE=9 xxx,bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=8 bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=7 bb,b,b,aaa,ZZZ,Z,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine remove_c(list,place)

! ident_9="@(#)M_list::remove_c(3fp): remove string from allocatable string array at specified position"

character(len=:),allocatable :: list(:)
integer,intent(in)           :: place
integer                      :: ii, end
   if(debug) write(stderr,*)'*remove_c* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[character(len=2) :: ]
   endif
   ii=len(list)
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[character(len=ii) :: list(:place-1) ]
   else
      list=[character(len=ii) :: list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_c* END PLACE=',place,' NEWSIZE=',size(list),' LENGTH=',len(list)
end subroutine remove_c
subroutine remove_d(list,place)

! ident_10="@(#)M_list::remove_d(3fp): remove doubleprecision value from allocatable array at specified position"

doubleprecision,allocatable  :: list(:)
integer,intent(in)           :: place
integer                      :: end
   if(debug) write(stderr,*)'*remove_d* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
           list=[doubleprecision :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_d* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_d
subroutine remove_r(list,place)

! ident_11="@(#)M_list::remove_r(3fp): remove value from allocatable array at specified position"

real,allocatable    :: list(:)
integer,intent(in)  :: place
integer             :: end
   if(debug) write(stderr,*)'*remove_r* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[real :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_r* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_r
subroutine remove_l(list,place)

! ident_12="@(#)M_list::remove_l(3fp): remove value from allocatable array at specified position"

logical,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(debug) write(stderr,*)'*remove_l* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_l* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_l
subroutine remove_i(list,place)

! ident_13="@(#)M_list::remove_i(3fp): remove value from allocatable array at specified position"
integer,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(debug) write(stderr,*)'*remove_i* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_i* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    replace(3f) - [M_list] replace entry in a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine replace(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer, intent(out)          :: PLACE
!!
!!##DESCRIPTION
!!
!!    replace a value in an allocatable array at the specified index. Unless the
!!    array needs the string length to increase this is merely an assign of a value
!!    to an array element.
!!
!!    The array may be of type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER>
!!    It is assumed to be sorted in descending order without duplicate values.
!!
!!    The value and list must be of the same type.
!!
!!##OPTIONS
!!
!!    VALUE         the value to place in the array
!!    LIST          is the array.
!!    PLACE         is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!!   Replace key-value pairs in a dictionary
!!
!!     program demo_replace
!!     use M_list, only  : insert, locate, replace
!!     ! Find if a key is in a list and insert it
!!     ! into the key list and value list if it is not present
!!     ! or replace the associated value if the key existed
!!     implicit none
!!     character(len=20)            :: key
!!     character(len=100)           :: val
!!     character(len=:),allocatable :: keywords(:)
!!     character(len=:),allocatable :: values(:)
!!     integer                      :: i
!!     integer                      :: place
!!     call update('b','value of b')
!!     call update('a','value of a')
!!     call update('c','value of c')
!!     call update('c','value of c again')
!!     call update('d','value of d')
!!     call update('a','value of a again')
!!     ! show array
!!     write(*,'(*(a,"==>",a,/))')(trim(keywords(i)),trim(values(i)),i=1,size(keywords))
!!
!!     call locate(keywords,'a',place)
!!     if(place.gt.0)then
!!        write(*,*)'The value of "a" is',trim(values(place))
!!     else
!!        write(*,*)'"a" not found'
!!     endif
!!
!!     contains
!!     subroutine update(key,val)
!!     character(len=*),intent(in)  :: key
!!     character(len=*),intent(in)  :: val
!!     integer                      :: place
!!
!!     ! find where string is or should be
!!     call locate(keywords,key,place)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        call insert(keywords,key,abs(place))
!!        call insert(values,val,abs(place))
!!     else ! replace
!!        call replace(values,val,place)
!!     endif
!!
!!     end subroutine update
!!    end program demo_replace
!!
!!   Expected output
!!
!!    d==>value of d
!!    c==>value of c again
!!    b==>value of b
!!    a==>value of a again
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine replace_c(list,value,place)

! ident_14="@(#)M_list::replace_c(3fp): replace string in allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: tlen
integer                      :: end
   if(debug) write(stderr,*)'*replace_c* START VALUE=',trim(value),' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   tlen=len_trim(value)
   end=size(list)
   if(place.lt.0.or.place.gt.end)then
           write(stderr,*)'*replace_c* error: index out of range. end=',end,' index=',place
   elseif(len_trim(value).le.len(list))then
      list(place)=value
   else  ! increase length of variable
      ii=max(tlen,len(list))
      kludge=[character(len=ii) :: list ]
      list=kludge
      list(place)=value
   endif
   if(debug)write(stderr,*)'*replace_c* END VALUE=',trim(value),' PLACE=',place,' NEWSIZE=',size(list),' LENGTH=',len(list)
end subroutine replace_c
subroutine replace_d(list,value,place)

! ident_15="@(#)M_list::replace_d(3fp): place doubleprecision value into allocatable array at specified position"

doubleprecision,intent(in)   :: value
doubleprecision,allocatable  :: list(:)
integer,intent(in)           :: place
integer                      :: end
   if(debug) write(stderr,*)'*replace_d* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
           list=[doubleprecision :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_d* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(stderr,*)'*replace_d* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_d
subroutine replace_r(list,value,place)

! ident_16="@(#)M_list::replace_r(3fp): place value into allocatable array at specified position"

real,intent(in)       :: value
real,allocatable      :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(debug) write(stderr,*)'*replace_r* START REPLACE_R VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[real :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_r* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(stderr,*)'*replace_r* END REPLACE_R VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_r
subroutine replace_l(list,value,place)

! ident_17="@(#)M_list::replace_l(3fp): place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*replace_l* START REPLACE_L VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_l* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(stderr,*)'*replace_l* END REPLACE_L VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_l
subroutine replace_i(list,value,place)

! ident_18="@(#)M_list::replace_i(3fp): place value into allocatable array at specified position"

integer,intent(in)    :: value
integer,allocatable   :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*replace_i* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_i* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(stderr,*)'*replace_i* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    insert(3f) - [M_list] insert entry into a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine insert(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer,intent(in)    :: place
!!
!!##DESCRIPTION
!!
!!    Insert a value into an allocatable array at the specified index.
!!    The list and value must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL, or INTEGER)
!!
!!##OPTIONS
!!
!!    list    is the list array. Must be sorted in descending order.
!!    value   the value to place in the array
!!    PLACE   is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!!    Find if a string is in a sorted array, and insert the string into
!!    the list if it is not present ...
!!
!!     program demo_insert
!!     use M_sort, only : sort_shell
!!     use M_list, only : locate, insert
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!     ! add or replace values
!!     call update(arr,'b')
!!     call update(arr,'[')
!!     call update(arr,'c')
!!     call update(arr,'ZZ')
!!     call update(arr,'ZZZ')
!!     call update(arr,'ZZZZ')
!!     call update(arr,'')
!!     call update(arr,'z')
!!
!!     contains
!!     subroutine update(arr,string)
!!     character(len=:),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, end
!!
!!     end=size(arr)
!!     ! find where string is or should be
!!     call locate(arr,string,place)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        call insert(arr,string,abs(place))
!!     endif
!!     ! show array
!!     end=size(arr)
!!     write(*,'("array is now SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end subroutine update
!!     end program demo_insert
!!
!!   Results:
!!
!!     array is now SIZE=5 xxx,b,aaa,ZZZ,,
!!     array is now SIZE=6 xxx,b,aaa,[,ZZZ,,
!!     array is now SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     array is now SIZE=8 xxx,c,b,aaa,[,ZZZ,ZZ,,
!!     array is now SIZE=9 xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!     array is now SIZE=10 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine insert_c(list,value,place)

! ident_19="@(#)M_list::insert_c(3fp): place string into allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: end
   if(debug) write(stderr,*)'*insert_c* START VALUE=',trim(value),' PLACE=',place,' ORIGINALSIZE=',size(list)

   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif

   ii=max(len_trim(value),len(list),2)
   end=size(list)

   if(end.eq.0)then                                          ! empty array
      list=[character(len=ii) :: value ]
   elseif(place.eq.1)then                                    ! put in front of array
      kludge=[character(len=ii) :: value, list]
      list=kludge
   elseif(place.gt.end)then                                  ! put at end of array
      kludge=[character(len=ii) :: list, value ]
      list=kludge
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      kludge=[character(len=ii) :: list(:place-1), value,list(place:) ]
      list=kludge
   else                                                      ! index out of range
      write(stderr,*)'*insert_c* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(stderr,*)'*insert_c* END VALUE=',trim(value),' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_c
subroutine insert_r(list,value,place)

! ident_20="@(#)M_list::insert_r(3fp): place real value into allocatable array at specified position"

real,intent(in)       :: value
real,allocatable      :: list(:)
integer,intent(in)    :: place
integer               :: end

   if(debug) write(stderr,*)'*insert_r* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[real :: ]
   endif

   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                   ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_r* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(stderr,*)'*insert_r* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_r
subroutine insert_d(list,value,place)

! ident_21="@(#)M_list::insert_d(3fp): place doubleprecision value into allocatable array at specified position"

doubleprecision,intent(in)       :: value
doubleprecision,allocatable      :: list(:)
integer,intent(in)               :: place
integer                          :: end
   if(debug) write(stderr,*)'*insert_d* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[doubleprecision :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_d* error: index out of range. end=',end,' index=',place,' value=',value
   endif
   if(debug)write(stderr,*)'*insert_d* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_d
subroutine insert_l(list,value,place)

! ident_22="@(#)M_list::insert_l(3fp): place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*insert_l* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_l* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(stderr,*)'*insert_l* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_l
subroutine insert_i(list,value,place)

! ident_23="@(#)M_list::insert_i(3fp): place value into allocatable array at specified position"

integer,allocatable   :: list(:)
integer,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*insert_i* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_i* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(stderr,*)'*insert_i* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    dict_delete(3f) - [M_list] delete entry by name from an allocatable sorted string array if it is present
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine dict_delete(key,dict)
!!
!!    character(len=*),intent(in) :: key
!!    type(dictionary)            :: dict
!!
!!##DESCRIPTION
!!
!!    Find if a string is in a sorted array, and delete the string
!!    from the dictionary if it is present.
!!
!!##OPTIONS
!!
!!    KEY           the key name to find and delete from the dictionary.
!!    DICTIONARY    the dictionary.
!!
!!##EXAMPLES
!!
!!
!!    delete a key from a dictionary by name.
!!
!!     program demo_dict_delete
!!     use M_list, only : dictionary
!!     implicit none
!!     type(dictionary) :: caps
!!     integer                       :: i
!!     call caps%set(caps,'A','aye')
!!     call caps%set(caps,'B','bee')
!!     call caps%set(caps,'C','see')
!!     call caps%set(caps,'D','dee')
!!
!!     write(*,101)(trim(arr(i)),i=1,size(caps%keys)) ! show array
!!     call  caps%del(caps,'A')
!!     call  caps%del(caps,'C')
!!     call  caps%del(caps,'z')
!!     write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     101 format (1x,*("[",a,"]",:,","))
!!     end program demo_dict_delete
!!
!!    Results:
!!
!!     [z],[xxx],[c],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[aaa],[ZZZ]
!!     [z],[xxx],[aaa],[ZZZ]
!!     [xxx],[aaa],[ZZZ]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine dict_delete(self,key)

! ident_24="@(#)M_list::dict_delete(3f): remove string from sorted allocatable string array if present"

class(dictionary),intent(inout) :: self
character(len=*),intent(in)     :: key
integer                         :: place

   call locate(self%key,key,place)
   if(place.ge.1)then
      call remove(self%key,place)
      call remove(self%value,place)
      call remove(self%count,place)
   endif

end subroutine dict_delete
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    dict_get(3f) - [M_list] get value of key-value pair in a dictionary given key
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine dict_get(dict,key,value)
!!
!!    type(dictionary)            :: dict
!!    character(len=*),intent(in) :: key
!!    character(len=*),intent(in) :: VALUE
!!
!!##DESCRIPTION
!!
!!    get a value given a key from a key-value dictionary
!!
!!    If key is not found in dictionary , return a blank
!!
!!##OPTIONS
!!
!!    DICT     is the dictionary.
!!    KEY      key name
!!    VALUE    value associated with key
!!
!!##EXAMPLES
!!
!!
!!     program demo_locate
!!     use M_list, only : dictionary
!!     implicit none
!!     type(dictionary)             :: table
!!     character(len=:),allocatable :: val
!!     integer          :: i
!!
!!     call table%set('A','value for A')
!!     call table%set('B','value for B')
!!     call table%set('C','value for C')
!!     call table%set('D','value for D')
!!     call table%set('E','value for E')
!!     call table%set('F','value for F')
!!     call table%set('G','value for G')
!!     write(*,*)table%get('A')
!!     write(*,*)table%get('B')
!!     write(*,*)table%get('C')
!!     write(*,*)table%get('D')
!!     write(*,*)table%get('E')
!!     write(*,*)table%get('F')
!!     write(*,*)table%get('G')
!!     write(*,*)table%get('H')
!!     end program demo_locate
!!
!!    Results:
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function dict_get(self,key) result(value)

! ident_25="@(#)M_list::dict_get(3f): get value of key-value pair in dictionary, given key"

!!class(dictionary),intent(inout) :: self
class(dictionary)               :: self
character(len=*),intent(in)     :: key
character(len=:),allocatable    :: value
integer                         :: place
   call locate(self%key,key,place)
   if(place.lt.1)then
      value=''
   else
      value=self%value(place)(:self%count(place))
   endif
end function dict_get
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    dict_add(3f) - [M_list] add or replace a key-value pair in a dictionary
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine dict_add(dict,key,value)
!!
!!    type(dictionary)            :: dict
!!    character(len=*),intent(in) :: key
!!    character(len=*),intent(in) :: VALUE
!!
!!##DESCRIPTION
!!    Add or replace a key-value pair in a dictionary.
!!
!!##OPTIONS
!!    DICT     is the dictionary.
!!    key      key name
!!    VALUE    value associated with key
!!
!!##EXAMPLES
!!
!!    If string is not found in a sorted array, insert the string
!!
!!     program demo_add
!!     use M_list, only : dictionary
!!     implicit none
!!     type(dictionary) :: dict
!!     integer          :: i
!!
!!     call dict%set('A','b')
!!     call dict%set('B','^')
!!     call dict%set('C',' ')
!!     call dict%set('D','c')
!!     call dict%set('E','ZZ')
!!     call dict%set('F','ZZZZ')
!!     call dict%set('G','z')
!!     call dict%set('A','new value for A')
!!     write(*,'(*(a,"==>","[",a,"]",/))')(trim(dict%key(i)),dict%value(i)(:dict%count(i)),i=1,size(dict%key))
!!     end program demo_add
!!
!!    Results:
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine dict_add(self,key,value)

! ident_26="@(#)M_list::dict_add(3f): place key-value pair into dictionary, adding the key if required"

class(dictionary),intent(inout) :: self
character(len=*),intent(in)     :: key
character(len=*),intent(in)     :: value
integer                         :: place
integer                         :: place2
   call locate(self%key,key,place)
   if(place.lt.1)then
      place2=iabs(place)
      call insert( self%key,   key,             place2 )
      call insert( self%value, value,           place2 )
      call insert( self%count, len_trim(value), place2 )
   elseif(place.gt.0)then  ! replace instead of insert
      call insert( self%value, value,           place )
      call insert( self%count, len_trim(value), place )
   endif
end subroutine dict_add
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_list
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
 
 
!>>>>> app/prep.f90
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!  @(#)prep: FORTRAN pre-processor
!  Fortran preprocessor originally based on public-domain FPP pre-processor from Lahey Fortran Code Repository
!     http://www.lahey.com/code.htm
!  Extensively rewritten since under a MIT License.
!     2013-10-03,2020-12-19,2021-06-12 : John S. Urban

module M_fpp                                                             !@(#)M_fpp(3f): module used by prep program
USE ISO_FORTRAN_ENV, ONLY : ERROR_UNIT, OUTPUT_UNIT                      ! access computing environment ; Standard: Fortran 2003
use M_io,        only : slurp, get_tmp, dirname                          ! Fortran file I/O routines
use M_kracken95, only : sget, dissect, lget                              ! load command argument parsing module
use M_strings,   only : nospace, v2s, substitute, upper, lower, isalpha, split, delim, str_replace=>replace, sep, atleast
use M_list,      only : insert, locate, replace, remove                  ! Basic list lookup and maintenance
   implicit none

   logical,save                         :: debug=.false.

   integer,parameter                    :: num=2048                       ! number of named values allowed
   integer,public,parameter             :: G_line_length=4096             ! allowed length of input lines
   integer,public,parameter             :: G_var_len=31                   ! allowed length of variable names

   integer,public                       :: G_numdef=0                     ! number of defined variables in dictionary

   character(len=G_line_length),public  :: G_source                       ! original source file line
   character(len=G_line_length),public  :: G_outline                      ! message to build for writing to output

   character(len=G_var_len),public      :: G_defval(num)                  ! variable values in variable dictionary
   character(len=G_var_len),public      :: G_defvar(num)                  ! variables in variable dictionary

   type file_stack
      integer                              ::  unit_number
      integer                              ::  line_number=0
      character(len=G_line_length)         ::  filename
   end type
   type(file_stack),public              ::  G_file_dictionary(50)

   type parcel_stack
      integer                              ::  unit_number
      integer                              ::  line_number=0
      character(len=G_line_length)         ::  name
   end type
   type(parcel_stack),public            ::  G_parcel_dictionary(500)

   logical,save,public                  :: G_asis=.false.
   logical,save,public                  :: G_expand=.false.
   integer,public                       :: G_iocount=0
   integer,public                       :: G_parcelcount=0
   integer,public                       :: G_io_total_lines=0
   integer,public                       :: G_iwidth                       ! maximum width of line to write on output unit
   logical,public                       :: G_noenv=.false.                ! ignore environment variables in $IFDEF and $IFNDEF

   integer,public                       :: G_iout                         ! output unit
   integer,save,public                  :: G_iout_init                    ! initial output unit
  !integer,public                       :: G_ihelp=ERROR_UNIT             ! output unit for help text
   integer,public                       :: G_ihelp=OUTPUT_UNIT            ! output unit for help text
   character(len=10),public             :: G_outtype='asis'

   integer,public                       :: G_inc_count=0
   character(len=G_line_length),public  :: G_inc_files(50)
   character(len=:),allocatable,save    :: G_MAN
   logical,save                         :: G_MAN_COLLECT=.false.
   logical,save                         :: G_MAN_PRINT=.false.
   character(len=:),allocatable         :: G_MAN_FILE
   character(len=10)                    :: G_MAN_FILE_POSITION='ASIS      '

   integer,public                       :: G_nestl=0                      ! count of if/elseif/else/endif nesting level
   integer,public,parameter             :: G_nestl_max=20                 ! maximum nesting level of conditionals

   logical,save,public                  :: G_write_what=.false.           ! write strings after @(#) similar to what(1).
   logical,save,public                  :: G_system_on=.false.            ! allow system commands or not on $SYSTEM

   logical,public,save                  :: G_condop(0:G_nestl_max)        ! storage to keep track of previous write flags
   data G_condop(0:G_nestl_max) /.true.,G_nestl_max*.false./
   logical,public                       :: G_dc                           ! flag to determine write flag

   logical,public                       :: G_write=.true.                 ! whether non-if/else/endif directives should be processed
   logical,public                       :: G_llwrite=.true.               ! whether to write current line or skip it

   integer,public                       :: G_comment_count=0
   character(len=10),public             :: G_comment_style=' '

   character(len=:),allocatable   :: keywords(:)
   character(len=:),allocatable   :: values(:)
   integer,allocatable            :: counts(:)


   contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cond()       !@(#)cond(3f): process conditional directive assumed to be in SOURCE '$verb...'
   character(len=G_line_length) :: line                    ! directive line with leading prefix character (default is $) removed
   character(len=G_line_length) :: verb                    ! first word of command converted to uppercase
   character(len=G_line_length) :: options                 ! everything after first word of command till end of line or !
   character(len=G_line_length) :: upopts                  ! directive line with leading prefix removed; uppercase; no spaces
   logical,save                 :: eb=.false.
   integer,save                 :: noelse=0
   integer                      :: verblen

   line=adjustl(G_source(2:))                              ! remove leading prefix and spaces from directive line

   if (index(line,'!').ne.0) then                          ! if directive contains an exclamation a comment is present
                                                           ! LIMITATION: EVEN MESSAGES CANNOT CONTAIN COMMENTS
      line=line(:index(line,'!')-1)                        ! trim trailing comment from directive
   endif

   verblen=index(line,' ')
   if(verblen.eq.0)then
      verblen=len(line)
      verb=line
      options=' '
   else
      verb=line(:verblen-1)
      options=adjustl(line(verblen+1:))
   endif
   verb=upper(verb)
   upopts=nospace(upper(options))                          ! remove spaces from directive

   !write(*,*)'G_SOURCE='//trim(g_source)
   !write(*,*)'LINE='//trim(line)
   !write(*,*)'VERB='//trim(verb)
   !write(*,*)'OPTIONS='//trim(options)
   !write(*,*)'UPOPTS='//trim(upopts)

   if(G_asis.and.VERB.eq.'PARCEL')then
      call parcel_case(options)                                       ! end parcel ignoring options
      return
   elseif(G_asis)then
      call write_out(trim(G_source))                                  ! write data line
      return
   elseif(G_write)then                                                ! if processing lines in a logically selected region
                                                                      ! process the directive
      select case(VERB)
      case('  ')                                                      ! entire line is a comment
      case('DEFINE');           call define(upopts,1)                 ! only process DEFINE if not skipping data lines
      case('REDEFINE');         call define(upopts,0)                 ! only process DEFINE if not skipping data lines
      case('UNDEF','UNDEFINE'); call undef(upopts)                    ! only process UNDEF if not skipping data lines
      case('INCLUDE');          call include(options,50+G_iocount)    ! Filenames can be case sensitive
      case('OUTPUT');           call output_case(options)             ! Filenames can be case sensitive
      case('PARCEL');           call parcel_case(upopts)
      case('POST');             call post(upopts)
      case('BLOCK');            call document(options)
      case('SET');              call set(options)
      case('IMPORT');           call import(options)
      case('IDENT','@(#)');     call ident(options)
      case('SHOW') ;            call debug_state(options)
      case('SYSTEM');           call exe()
      case('MESSAGE');          call stderr(G_source(2:))             ! trustingly trim MESSAGE from directive
      case('STOP');             call stop(upopts)
      case('QUIT');             call stop('0')
      end select
   endif
   select case(VERB)                                                  ! process logical flow control even if G_write is false

   case('DEFINE','INCLUDE','SHOW','STOP','QUIT')
   case('SYSTEM','UNDEF','UNDEFINE','MESSAGE','REDEFINE')
   case('OUTPUT','IDENT','@(#)','BLOCK','IMPORT')
   case('PARCEL','POST','SET')
   case(' ')

   case('ELSE','ELSEIF');  call else(verb,upopts,noelse,eb)
   case('ENDIF');          call endif(noelse,eb)
   case('IF');             call if(upopts,noelse,eb)
   case('IFDEF','IFNDEF'); call def(verb,upopts,noelse,eb)

   case default
      call stop_prep('*prep:cond* ERROR(001) - UNKNOWN COMPILER DIRECTIVE ['//trim(verb)//']: '//trim(G_SOURCE))
   end select
end subroutine cond
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine exe()                                 ! @(#)exe(3f): Execute the command line specified by the string COMMAND.
   character(len=G_line_length)  :: command      ! directive line with leading prefix and directive name removed
   character(len=G_line_length)  :: defineme     ! scratch string for writing a DEFINE directive in to return command status
   integer                       :: icmd=0
   integer                       :: cstat
   character(len=256)            :: sstat

   if(G_system_on)then
      command=adjustl(G_source(2:))                                                 ! remove $ from directive
      command=command(7:)                                                           ! trim SYSTEM from directive
      if(G_write_what)then
         call stderr('+'//command)
      endif

      ! not returning command status on all platforms
      call execute_command_line (command, exitstat=icmd,cmdstat=cstat,cmdmsg=sstat) ! execute system command

      if(icmd.ne.0)then                                                             ! if system command failed exit program
         call stop_prep('*prep:exe* ERROR(002) - SYSTEM COMMAND FAILED:'//v2s(icmd))
      endif
   else
      call stop_prep('*prep:exe* ERROR(003) - SYSTEM DIRECTIVE ENCOUNTERED BUT NOT ENABLED:'//trim(G_SOURCE))
   endif

   write(defineme,'("CMD_STATUS=",i8)')icmd
   defineme=nospace(defineme)
   call define(defineme,0)

end subroutine exe
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine output_case(opts)                             !@(#)output_case(3f): process $OUTPUT directive
   character(len=*)              :: opts
   character(len=G_line_length)  :: filename             ! filename on $OUTPUT command
   character(len=20)             :: position
   integer                       :: ios
      call dissect2('output','-oo --append .false.',opts)  ! parse options and inline comment on input line
      filename=sget('output_oo')
      select case(filename)
      case('@')
         G_iout=6
      case(' ')                                          ! reset back to initial output file
         if(G_iout.ne.6.and.G_iout.ne.G_iout_init)then   ! do not close current output if it is stdout or default output file
            close(G_iout,iostat=ios)
         endif
         G_iout=G_iout_init
      case default
         G_iout=61
         close(G_iout,iostat=ios)
         if(lget('output_append'))then; position='append'; else; position='asis'; endif
         open(unit=G_iout,file=filename,iostat=ios,action='write',position=position)
         if(ios.ne.0)then
            call stop_prep('*perf:output_case* ERROR(004) - FAILED TO OPEN OUTPUT FILE:'//trim(filename))
         endif
      end select
   if(G_write_what)then
      write(ERROR_UNIT,'(a)')'*perf:output_case*: OUTPUT FILE CHANGED TO:'//trim(filename)
   endif
end subroutine output_case
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine parcel_case(opts)                             !@(#)parcel_case(3f): process $PARCEL directive
character(len=*)              :: opts
character(len=G_line_length)  :: name                 ! name on $PARCEL command
integer                       :: ios
integer                       :: lun
character(len=256)            :: message
   call dissect2('parcel','-oo ',opts)  ! parse options and inline comment on input line
   name=sget('parcel_oo')
   if(name.eq.'')then
      G_asis=.false.
      G_iout=G_iout_init
   else
      open(newunit=lun,iostat=ios,action='readwrite',status='scratch',iomsg=message)
      if(ios.ne.0)then
         call stop_prep('*prep:parcel_case* ERROR(005) - FAILED TO OPEN PARCEL SCRATCH FILE:'//trim(name)//' '//trim(message))
      else
         G_parcelcount=G_parcelcount+1
         G_parcel_dictionary(G_parcelcount)%name=name
         G_parcel_dictionary(G_parcelcount)%unit_number=lun
         G_asis=.true.
         G_iout=lun
      endif
   endif
end subroutine parcel_case
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine ident(opts)                                 !@(#)ident(3f): process $IDENT directive
   character(len=*)              :: opts
   character(len=G_line_length)  :: lang               ! language on $IDENT command
   character(len=:),allocatable  :: text
   integer,save                  :: ident_count=1

   call dissect2('ident','-oo --language fortran',opts)  ! parse options and inline comment on input line
   text=trim(sget('ident_oo'))
   lang=sget('ident_language')

   select case(lang)
   case('fortran')    !*! should make look for characters not allowed in metadata, continue over multiple lines, ...
      select case(len(text))
      case(:89)
         write(G_iout,'("character(len=*),parameter::ident_",i0,"=""@(#)",a,''"'')')ident_count,text
         ident_count=ident_count+1
      case(90:126)
         write(G_iout,'("character(len=*),parameter::ident_",i0,"=""&")')ident_count
         write(G_iout,'(''&@(#)'',a,''"'')')text
         ident_count=ident_count+1
      case default
         call stop_prep('*prep:exe* ERROR(006) - IDENT TOO LONG:'//trim(G_SOURCE))
      end select
   case('c')
         write(G_iout,'(a)')'#ident "@(#)'//text//'"'
   case default
         call stop_prep('*prep:exe* ERROR(007) - IDENT LANGUAGE UNKNOWN:'//trim(G_SOURCE))
   end select

end subroutine ident
!==================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine define(opts,ireset)                              !@(#)define(3f): process 'DEFINE variablename[=expression]' directive
character(len=*),intent(in)    :: opts                      ! packed uppercase working copy of input line with leading $verb removed
integer,intent(in)             :: ireset                    ! 0= can redefine variable, anything else fail on redefine

   character(len=G_line_length):: temp                      ! scratch
   integer                     :: iequ                      ! location of "=" in the directive, if any
   integer                     :: j                         ! index thru variable dictionary to see if variable is already defined
   integer                     :: iname                     ! length of variable name
   integer                     :: istore                    ! location of variable name in dictionary

! CHECK COMMAND SYNTAX
   iequ=index(opts,'=')                                     ! find "=" in "variable_name=expression" if any
   if (opts(1:1).eq.' '.or.iequ.eq.len_trim(opts)) then     ! no variable name in packed string or string after = is null
      call stop_prep('*prep:define* ERROR(008) - INCOMPLETE STATEMENT:'//trim(opts))
   endif
   if (iequ.gt.G_var_len+1) then                            ! variable name too long
      call stop_prep('*prep:define* ERROR(009) - MISSPELLING OR NAME LENGTH EXCEEDS '//v2s(G_var_len)//' CHARACTERS:'//trim(opts))
   endif

   if(iequ.eq.0)then                                        ! find end of variable name
      iname=len_trim(opts)
   else
      iname=iequ-1
   endif

   call name(opts(:iname))                                  ! check that variable name is composed of allowed characters

   istore=0
   if (G_numdef.ne.0) then                                  ! test for redefinition of defined name
      do j=1,G_numdef
         if (opts(:iname).eq.G_defvar(j)) then
            istore=j
            if(ireset.ne.0)then                             ! fail if redefinitions are not allowed on this call
               call stop_prep('*prep:define* ERROR(010) - REDEFINITION OF DEFINED NAME INVALID:'//trim(opts))
            endif
         endif
      enddo
   endif

   if(istore.eq.0)then                                      ! new variable name
      G_numdef=G_numdef+1                                   ! increment number of defined variables
      istore=G_numdef
   endif
   if (iequ.eq.0) then                                      ! if no = then variable assumes value of 1
      G_defvar(istore)=opts                                 ! store variable name from line with no =value string
      temp='1'                                              ! set string to default value
   else                                                     ! =value string trails name on directive
      G_defvar(istore)=opts(:iequ-1)                        ! store variable name from line with =value string
      temp=opts(iequ+1:)                                       ! get expression
   endif

   call parens(temp)                                        !
   if (iequ.ne.0) then
      temp=opts(:iequ)//temp
   endif

   call math(temp,iequ+1,len_trim(opts))
   call doop(temp,iequ+1,len_trim(opts))
   call logic(temp,iequ+1,len_trim(opts))
   call getval(temp,iequ+1,len_trim(opts),G_defval(istore))

end subroutine define
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function getdate(name) result(s)         !@(#) getdate(3f): Function to write date and time into returned string
character(len=*),optional :: name

! PURPOSE - Return a string with the current date and time
character(len=*),parameter         :: month='JanFebMarAprMayJunJulAugSepOctNovDec'
character(len=*),parameter         :: fmt = '(I2.2,A1,I2.2,I3,1X,A3,1x,I4)'
character(len=*),parameter         :: cdate = '(A3,1X,I2.2,1X,I4.4)'
character(len=:),allocatable       :: s
character(len=80)                  :: line
integer,dimension(8)               :: v
character(len=10) :: name_

   call date_and_time(values=v)
   name_='prep'
   if(present(name))name_=name
   select case(lower(name_))
   case('prep') ! PREP_DATE="00:39  5 Nov 2013"
   write(line,fmt) v(5), ':', v(6), v(3), month(3*v(2)-2:3*v(2)), v(1)
   case('date')
   write(line,'(i4.4,"-",i2.2,"-",i2.2)') v(1),v(2),v(3)
   case('cdate')
   write(line,cdate) month(3*v(2)-2:3*v(2)), v(3), v(1)
   case('long')
   write(line,'(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2," UTC",sp,i0)') v(1),v(2),v(3),v(5),v(6),v(7),v(4)
   case('time')
   write(line,'(i2.2,":",i2.2,":",i2.2)') v(5),v(6),v(7)
   case default
   write(line,'(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2," UTC",sp,i0)') v(1),v(2),v(3),v(5),v(6),v(7),v(4)
   end select
   s=trim(line)
end function getdate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine name(line)                                                 !@(#)name(3f): test for legal variable name
character(len=*)          :: line
   integer                :: i

   if (line(1:1).lt.'A'.or.line(1:1).gt.'Z')then                         ! variable names start with a-z
     call stop_prep("*prepname* ERROR(016) -VARIABLE NAME DOES NOT START WITH ALPHAMERIC(OR GENERAL SYNTAX ERROR):"//trim(G_source))
   endif

   if(len_trim(line).gt.G_var_len)then
      call stop_prep('*prepname* ERROR(017) - VARIABLE NAME EXCEEDS '//v2s(G_var_len)//' CHARACTERS:'//trim(G_source))
   endif

   do i=2,len_trim(line)                                                 ! name uses $  _ and letters (A-Z) digits (0-9)
      if(line(i:i).ne.'$'.and.line(i:i).ne.'_'.and.     &
      & (line(i:i).lt.'A'.or.line(i:i).gt.'Z').and.     &
      & (line(i:i).lt.'0'.or.line(i:i).gt.'9')) then
       call stop_prep('*prepname* ERROR(018) -VARIABLE NAME CONTAINS UNALLOWED CHARACTER(OR GENERAL SYNTAX ERROR):'//trim(G_source))
      endif
   enddo

end subroutine name
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine getval(line,ipos1,ipos2,value)     !@(#)getval(3f): get value from dictionary for given variable name or return input
character(len=G_line_length),intent(in)   :: line                           ! current(maybe partial) directive line
integer,intent(in)                        :: ipos1                          ! beginning position of variable name in LINE
integer,intent(in)                        :: ipos2                          ! ending position of variable name in LINE
character(len=G_var_len),intent(out)      :: value                          ! returned variable value

   character(len=G_line_length)           :: temp                           ! copy of substring being examined
   integer                                :: i
   integer                                :: ivalue

   temp=line(ipos1:ipos2)                                                   ! place variable name/value substring into TEMP

   if (temp(1:1).eq.' ')then                                                ! did not find expected variable name or value
      call stop_prep('*prepgetvalue* ERROR(019) - INCOMPLETE STATEMENT.'//trim(G_SOURCE))
   endif

   if (temp(1:1).ge.'A'.and.temp(1:1).le.'Z') then                          ! appears to be a variable name (not number or logical)

     value=temp(:G_var_len)
     do i=1,G_numdef                                                        ! find defined parameter in dictionary
        if (G_defvar(i).eq.value)exit
     enddo
     if (i.gt.G_numdef)then                                                 ! unknown variable name
        call stop_prep('*prep* ERROR(020) - UNDEFINED PARAMETER IN GETVAL:'//trim(G_source))
     endif
     value=G_defval(i)                                                      ! (trusted) value for variable name found in dictionary
     return
   else                                                                     ! not a variable name, try as a value
     read(temp(1:11),'(i11)',err=3) ivalue                                  ! try string as a numeric integer value
     write(value,'(i11)') ivalue                                            ! write numeric value into VALUE
     return                                                                 ! successfully return numeric VALUE

3    continue                                                               ! failed to read numeric value
     value=temp(:G_var_len)                                                 ! test TEMP as a logical
     if (value.ne.'.FALSE.'.and.value.ne.'.TRUE.')then                      ! if here, value should be a logical
        call stop_prep('*prep* ERROR(021) - SYNTAX ERROR.'//trim(G_source))
     endif
                                                                            ! value is ".TRUE." or ".FALSE."
   endif

   if(temp(1:1).ge.'A')then
      call stop_prep('*prep* ERROR(022) - $DEFINE VALUE MUST BE AN INTEGER OR LOGICAL CONSTANT.'//trim(G_source))
   endif

end subroutine getval
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine undef(opts)                                     !@(#)undef(3f): process UNDEFINE directive
character(len=*)     :: opts                               ! directive with no spaces, leading prefix removed, and all uppercase
   integer                     :: ifound                   ! subscript for location of variable to delete
   integer                     :: i,j

! REMOVE VARIABLE IF FOUND IN VARIABLE NAME DICTIONARY
   if (len_trim(opts).eq.0) then                           ! if no variable name
      call stop_prep('*prepundef* ERROR(023) - INCOMPLETE STATEMENT:'//trim(G_source))
   endif

   ifound=-1                                               ! initialize subscript for variable name to be searched for to bad value
   do i=1,G_numdef                                         ! find defined variable to be undefined by searching dictionary
      if (G_defvar(i).eq.opts)then                         ! found the requested variable name
         ifound=i                                          ! record the subscript that the name was located at
         exit                                              ! found the variable so no longer any need to search remaining names
      endif
   enddo

   if (ifound.lt.1) then                                   ! variable name not found
      return                                               ! quietly ignore unknown name (or syntax error!)
   endif

   do j=ifound,G_numdef-1                                  ! remove variable name and value from list of variable names and values
     G_defvar(j)=G_defvar(j+1)                             ! replace the value to be removed with the one above it and then repeat
     G_defval(j)=G_defval(j+1)
   enddo

   G_numdef=G_numdef-1                                     ! decrement number of defined variables

end subroutine undef
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine if(opts,noelse,eb)                              !@(#)if(3f): process IF and ELSEIF directives
character(len=*)                :: opts
integer,intent(out)             :: noelse
logical                         :: eb
   character(len=G_var_len)     :: value
   integer                      :: ios
   integer                      :: i

   noelse=0
   G_write=.false.

   G_nestl=G_nestl+1                                       ! increment IF nest level
   if (G_nestl.gt.G_nestl_max) then
      call stop_prep('*prep* ABORT(bh) - "IF" BLOCK NESTING TOO DEEP, LIMITED TO '//v2s(G_nestl_max)//' LEVELS:'//trim(G_source))
   endif

   FIND_DEFINED: do                                        ! find and reduce all DEFINED() functions to ".TRUE." or ".FALSE."
      if (index(opts,'DEFINED(').ne.0) then                ! find a DEFINED() function
         call ifdef(opts,index(opts,'DEFINED('))           ! reduce DEFINED() function that was found
         opts=nospace(opts)                                ! remove any spaces from rewritten expression
         cycle                                             ! look for another DEFINED() function
      endif
      exit                                                 ! no remaining DEFINED() functions so exit loop
   enddo FIND_DEFINED

   call parens(opts)
   if (index(opts,'.').eq.0) then                          ! if line should be a variable only
      if (opts(1:1).ge.'A'.and.opts(1:1).le.'Z') then      ! check that variable name starts with a valid character
         call name(opts)                                   ! check that opts contains only a legitimate variable name
         value=opts(:G_var_len)                            ! set VALUE to variable name
         do i=1,G_numdef                                   ! find variable in variable dictionary
            if (G_defvar(i).eq.value) exit
         enddo
         if (i.gt.G_numdef) then                           ! if failed to find variable name
            call stop_prep('*prep* ERROR(024) - UNDEFINED PARAMETER IN IF:'//trim(G_source))
         endif
         read(G_defval(i),'(l4)',iostat=ios) G_dc          ! convert variable value to a logical
         if(ios.ne.0)then
            call stop_prep('*prep* ERROR(025) - CONSTANT LOGICAL EXPRESSION REQUIRED.'//trim(G_source))
         endif
      else                                                 ! this should have been a variable name
         call stop_prep('*prep* ERROR(026) - CONSTANT LOGICAL EXPRESSION REQUIRED:'//trim(G_source))
      endif
   else                                                    ! a period is present in the expression so it needs evaluated
      call eval(opts)                                      ! evaluate line
   endif
   if (.not.G_dc.or..not.G_condop(G_nestl-1).or.eb)then
      return                                               ! check to make sure previous IF was true
   endif
   G_condop(G_nestl)=.true.
   G_write=G_condop(G_nestl)
end subroutine if
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine def(verb,opts,noelse,eb)                           !@(#)def(3f): process IFDEF and IFNDEF directives
character(len=*),intent(in)     :: verb
character(len=*),intent(in)     :: opts
integer,intent(out)             :: noelse
logical                         :: eb
   character(len=G_var_len)     :: value
   integer                      :: i
   integer                      :: istatus

   noelse=0
   G_write=.false.
   G_nestl=G_nestl+1                                 ! increment IF nest level
   if (G_nestl.gt.G_nestl_max) then
      call stop_prep('*prep* ABORT(bh) - "IF" BLOCK NESTING TOO DEEP, LIMITED TO '//v2s(G_nestl_max)//' LEVELS:'//trim(G_source))
   endif
   call name(opts)                                   ! check that opts contains only a legitimate variable name
   value=opts                                        ! set VALUE to variable name
   G_dc=.true.                                       ! initialize
   do i=1,G_numdef                                   ! find variable in variable dictionary
      if (G_defvar(i).eq.value) exit
   enddo
   if (i.gt.G_numdef) then                           ! if failed to find variable name
      G_dc=.false.
   endif
   if((.not.G_noenv).and.(.not.G_dc))then            ! if not found in variable dictionary check environment variables if allowed
      call get_environment_variable(trim(value),status=istatus)
      if(istatus.eq.0)then
         G_dc=.true.
      endif
   endif
   if(verb.eq.'IFNDEF')then
      G_dc=.not.G_dc
   endif
   if (.not.G_dc.or..not.G_condop(G_nestl-1).or.eb)then
      return                                               ! check to make sure previous IFDEF or IFNDEF was true
   endif
   G_condop(G_nestl)=.true.
   G_write=G_condop(G_nestl)
end subroutine def
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine ifdef(line,ipos1)                            !@(#)ifdef(3f): process and reduce DEFINED() function that was found
   character(len=G_line_length)   :: line
   character(len=G_line_length)   :: newl
   integer                        :: ipos1
   character(len=G_var_len)       :: ifvar
   integer                        :: i

   newl=line(ipos1+7:)

   if (len_trim(newl).eq.1.or.index(newl,')').eq.0.or. index(newl,')').eq.2)then
      call stop_prep("*prepifdef* ERROR(027) - INCOMPLETE STATEMENT."//trim(G_SOURCE))
   endif
   if (index(newl,')').gt.33)then
      call stop_prep("*prepifdef* ERROR(028) - MISSPELLING OR NAME LENGTH EXCEEDS "//v2s(G_var_len)//" CHARACTERS."//trim(G_source))
   endif
   ifvar= newl(2:index(newl,')')-1)
   if (newl(2:2).lt.'A'.or.newl(2:2).gt.'Z')then
      call stop_prep("*prepifdef* ERROR(029) - CONSTANT LOGICAL EXPRESSION REQUIRED."//trim(G_source))
   endif
   do i=3,index(newl,')')-1
      IF (NEWL(I:I).NE.'$'.AND.NEWL(I:I).NE.'_'.AND.(NEWL(I:I).LT.'A' &
       &  .OR.NEWL(I:I).GT.'Z').AND.(NEWL(I:I).LT.'0'                 &
       &  .or.newl(i:i).gt.'9')) then
         call stop_prep("*prepifdef* ERROR(030) - CONSTANT LOGICAL EXPRESSION REQUIRED."//trim(G_source))
      endif
   enddo

   G_dc=.false.
   line(ipos1:ipos1+6+index(newl,')'))='.FALSE.'

   do i=1,G_numdef                                        ! sequentially search for variable in variable dictionary
     if (G_defvar(i).eq.ifvar) then
       G_dc=.true.
       line(ipos1:ipos1+6+index(newl,')'))='.TRUE.'
       exit
     endif
   enddo

end subroutine ifdef
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine else(verb,opts,noelse,eb)                       !@(#)else(3f): process else and elseif
character(len=*)              :: verb
character(len=*)              :: opts                      !
integer                       :: noelse
logical                       :: eb

if(debug)then
   write(*,*)'*ELSE* TOP'
   write(*,*)'        G_NESTL =',g_nestl
   write(*,*)'        EB      =',eb
   write(*,*)'        NOELSE  =',noelse
   write(*,*)'        G_WRITE =',g_write
   write(*,*)'        G_CONDOP=',g_condop
endif

   if(noelse.eq.1.or.G_nestl.eq.0) then                    ! test for else instead of elseif
      call stop_prep("*prepelse* ERROR(031) - MISPLACED $ELSE OR $ELSEIF DIRECTIVE:"//trim(G_SOURCE))
   endif
   if(verb.eq.'ELSE')then
      noelse=1
   endif
   if(.not.G_condop(G_nestl-1))return                      ! if was true so ignore else
   eb=.false.
   if(G_condop(G_nestl)) then
       eb=.true.
       G_write=.false.
   elseif(len_trim(opts).ne.0)then                         ! elseif detected
     G_nestl=G_nestl-1                                     ! decrease if level because it will be incremented in subroutine if
     call if(opts,noelse,eb)
   else                                                    ! else detected
     G_condop(G_nestl)=.true.
     G_write=.true.
   endif

if(debug)then
   write(*,*)'*ELSE* BOTTOM'
   write(*,*)'        G_NESTL =',g_nestl
   write(*,*)'        EB      =',eb
   write(*,*)'        NOELSE  =',noelse
   write(*,*)'        G_WRITE =',g_write
   write(*,*)'        G_CONDOP=',g_condop
endif
end subroutine else
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine endif(noelse,eb)                             !@(#)endif(3f): process ENDIF directive
integer,intent(out)           :: noelse
logical,intent(out)           :: eb

   if(debug)then
      write(*,*)'*ENDIF* TOP'
      write(*,*)'        G_NESTL =',g_nestl
      write(*,*)'        EB      =',eb
      write(*,*)'        NOELSE  =',noelse
      write(*,*)'        G_WRITE =',g_write
      write(*,*)'        G_CONDOP=',g_condop
   endif

   ! if no ELSE or ELSEIF present insert ELSE to simplify logic
   if(noelse.eq.0)then
      call else('ELSE',' ',noelse,eb)
   endif

   G_nestl=G_nestl-1                                           ! decrease if level

   if(G_nestl.lt.0)then
      call stop_prep("*prependif* ERROR(032) - MISPLACED $ENDIF DIRECTIVE:"//trim(G_source))
   endif

   noelse=0                                                    ! reset else level
   eb=.not.G_condop(G_nestl+1)
   G_write=.not.eb
   G_condop(G_nestl+1)=.false.

   if(G_nestl.eq.0)then
      G_write=.true.
      eb=.false.
   endif

   if(debug)then
      write(*,*)'*ENDIF* BOTTOM'
      write(*,*)'        G_NESTL =',g_nestl
      write(*,*)'        EB      =',eb
      write(*,*)'        NOELSE  =',noelse
      write(*,*)'        G_WRITE =',g_write
      write(*,*)'        G_CONDOP=',g_condop
   endif

end subroutine endif
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine parens(line)                       !@(#)parens(3f): find subexpressions in parenthesis and process them
character(len=G_line_length)    :: line       ! line        -
   integer                      :: i
   integer                      :: j

   TILLDONE: do
      if (index(line,')').ne.0) then          ! closing parens found
         do i=index(line,')'),1,-1            ! find first right paren, then backwards to left paren (find innermost set of parens)
            if (line(i:i).eq.'(') exit
         enddo
         if (i.eq.0) then
            call stop_prep("*prep* ERROR(033) - CONSTANT LOGICAL EXPRESSION REQUIRED:"//trim(G_source))
         endif
         call math(line,i+1,index(line,')')-1)
         call doop(line,i+1,index(line,')')-1)
         call logic(line,i+1,index(line,')')-1)
         if (i.eq.1.and.index(line,')').eq.len_trim(line)) then          ! rewrite line after no more parens
            line=line(i+1:index(line,')')-1)
         elseif (i.eq.1) then                                            ! rewrite line after first set of parens
            line=line(2:index(line,')')-1)//line(index(line,')')+1:)
         elseif (index(line,')').eq.len_trim(line)) then                 ! rewrite line after last set of parens on line

            if (line(i+1:i+1).eq.'-'.and.index('*/+-',line(i-1:i-1)).ne.0) then
               do j=i-2,1,-1
                  if (index('*/+-',line(j:j)).ne.0) exit
               enddo
               if (j.eq.i-2) then
                  call stop_prep("*prep* 1**(-1) NOT IMPLEMENTED YET")
               endif

               select case (index('*/+-',line(i-1:i-1)))
               case(1,2)
                  if (j.eq.0) then
                     line='-'//line(:i-1)//line(i+2:index(line,')')-1)
                  else
                     line=line(:j)//'(-'//line(j+1:i-1)//line(i+2:index(line,')'))
                  endif
               case(3)
                  line=line(:i-2)//'-'//line(i+2:index(line,')')-1)
               case(4)
                  line=line(:i-2)//'+'//line(i+2:index(line,')')-1)
               case default
               end select
            else
               line=line(:i-1)//line(i+1:index(line,')')-1)
            endif
         elseif (line(i+1:i+1).eq.'-'.and.index('*/+-',line(i-1:i-1)).ne.0) then
            do j=i-2,1,-1
               if (index('*/+-',line(j:j)).ne.0) exit
            enddo
            if (j.eq.i-2) then
               call stop_prep("*prep* 1**(-1) NOT IMPLEMENTED YET")
            endif

            select case (index('*/+-',line(i-1:i-1)))
            case(1,2)
               if (j.eq.0) then
                  line='-'//line(:i-1)//line(i+2:index(line,')')-1)//line(index(line,')')+1:)
               else
                  line=line(:j)//'(-'//line(j+1:i-1)//line(i+2:index(line,')'))//line(index(line,')')+1:)
               endif
            case(3)
               line=line(:i-2)//'-'//line(i+2:index(line,')')-1)//line(index(line,')')+1:)
            case(4)
               line=line(:i-2)//'+'//line(i+2:index(line,')')-1)//line(index(line,')')+1:)
            case default
            end select
         else
            line=line(:i-1)//line(i+1:index(line,')')-1)//line(index(line,')')+1:)
         endif
      line=nospace(line)
      cycle TILLDONE
   elseif (index(line,'(').ne.0) then
      call stop_prep('*prep* ERROR(034) - CONSTANT LOGICAL EXPRESSION REQUIRED:'//trim(G_source))
   endif
   exit
   enddo TILLDONE
end subroutine parens
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine math(line,ipos1,ipos2)                             !@(#)math(3f):
   integer                               :: ipos1
   integer                               :: ipos2
   integer                               :: i,j
   character(len=G_line_length)            :: line
   character(len=G_line_length)            :: newl

   newl=line(ipos1:ipos2)
   i=1

   do
      j=index(newl(i:),'.')
      if (j.ne.0.and.j.ne.1) then
         call domath(newl(i:j+i-2),j-1)
         i=i+j
      elseif (j.eq.1) then
         i=i+1
      else
         call domath(newl(i:),ipos2-i+1)
         exit
      endif
   enddo

   line(ipos1:ipos2)=newl
   line=nospace(line)

end subroutine math
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine domath(line,ipos2)            !@(#)domath(3f): reduce integer expression containing  +-/* and ** operators
character(len=*)                             :: line
integer                                      :: ipos2

   character(len=11)                         :: temp
   character(len=G_line_length)                :: newl
   character(len=2),save                     :: ops(3)= (/'**','*/','+-'/)
   integer                                   :: i
   integer                                   :: j
   integer                                   :: loc
   integer                                   :: minus1
   integer                                   :: i1
   integer                                   :: i2
   integer                                   :: l
   integer                                   :: len
   integer                                   :: numop

   if (ipos2.eq.0) return
   loc=0
   j=0
   minus1=1
   newl=line(:ipos2)
   OVERALL: do numop=1,3                         ! check **, then */, then +-
      TILLDONE: do                               ! keep doing reduction of current operators
        i=index(newl,ops(numop))                 ! find location in input string where operator string was found
        if (numop.ne.1) then                     ! if not the two-character operator ** check for either operator of current group
          i=index(newl,ops(numop)(1:1))          ! find  first operator of group, if present
          j=index(newl,ops(numop)(2:2))          ! find second operator of group, if present
          i=max(i,j)                             ! find right-most operator, if any
          if (i*j.ne.0) i=min(i,j)               ! if at least one operator is present find left-most
        endif
        IF (I.EQ.0) cycle OVERALL                ! did not find these operators

        LEN=1                                    ! operator length
        IF (NUMOP.EQ.1) LEN=2
        IF (I.EQ.len_trim(NEWL)) then            ! if operator is at end of string
           call stop_prep("*prepdomath* ERROR(035) - INCOMPLETE STATEMENT. OPERATOR (**,/,*,+,-) AT STRING END:"//trim(G_SOURCE))
        endif
        IF (I.EQ.1.AND.NUMOP.NE.3) then          ! if operator at beginning of string and not +-
         call stop_prep("*prepdomath* ERROR(036)-SYNTAX ERROR. OPERATOR (**,*,/) NOT ALLOWED TO PREFIX EXPRESSION:"//trim(G_SOURCE))
        endif
        if (.not.(i.eq.1.and.numop.eq.3)) then   ! if processing +- operators and sign at beginning of string skip this
           if (index('*/+-',newl(i-1:i-1)).ne.0.or.index('*/+-',newl(i+len:i+len)).ne.0) then
              call stop_prep('*prepdomath* ERROR(037) - SYNTAX ERROR IN DOMATH:'//trim(G_source))
           endif
        endif

        i1=0
        if (.not.(i.eq.1.and.numop.eq.3)) then
           do j=i-1,1,-1
             if (index('*/+-.',newl(j:j)).eq.0) cycle
             exit
           enddo
           if (.not.(j.eq.i-1.and.j.ne.1))then
              i1=get_integer_from_string(newl(j+1:i-1))
           endif
        endif
        do l=i+len_trim(ops(numop)),len_trim(newl)
          if (index('*/+-.',newl(l:l)).eq.0) cycle
          exit
        enddo

        i2=get_integer_from_string(newl(i+len:l-1))

        if (numop.eq.1) then
          i1=i1**i2*minus1
        else
           select case (index('*/+-',newl(i:i)))
           case(1)
              i1=i1*i2*minus1
           case(2)
              if(i2.eq.0)then
                 call stop_prep('*prepdomath* ERROR(038) - DIVIDE BY ZERO:'//trim(G_source))
              endif
              i1=i1/i2*minus1
           case(3)
           if (i1.ne.0) then
             i1=i1*minus1+i2
           else
             i1=i1+i2*minus1
           endif
           case(4)
              if (i1.ne.0) then
                i1=i1*minus1-i2
              else
                i1=i1-i2*minus1
              endif
           case default
              call stop_prep('*prepdomath* ERROR(039) - INTERNAL PROGRAM ERROR:'//trim(G_source))
           end select
        endif

        if (i1.le.0) then
          if (j.eq.i-1.and.j.ne.1) then
            minus1=-1
            i1=abs(i1)
            loc=j+1
            newl(j+1:j+1)=' '
            l=l-1
            newl=nospace(newl)
          elseif (i.eq.1.and.numop.eq.3) then
            minus1=-1
            i1=abs(i1)
            loc=i
            newl(j:j)=' '
            l=l-1
            j=j-1
            newl=nospace(newl)
          else
            minus1=1
          endif
        else
          minus1=1
        endif
        write(temp,'(i11)') i1
        temp=nospace(temp)
        if (j.eq.0.and.l.gt.len_trim(newl)) then
          newl=temp(:len_trim(temp))
          cycle overall
        elseif (j.eq.0) then
          newl=temp(:len_trim(temp))//newl(l:)
        elseif (l.gt.len_trim(newl)) then
          newl=newl(:j)//temp(:len_trim(temp))
        else
          newl=newl(:j)//temp(:len_trim(temp))//newl(l:)
        endif
        if(i1.lt.0)then  ! if i1 is negative, could produce +-
           call substitute(newl,'+-','-')
        endif
      enddo TILLDONE
   enddo OVERALL

   if (minus1.eq.-1.and.(loc.eq.0.or.loc.eq.1)) then
      newl(:G_line_length)='-'//newl  !*! note potentially trimming a character off the end
   elseif (minus1.eq.-1.and.loc.ne.1) then
      newl=newl(:loc-1)//'-'//newl(loc:)
   endif

   line(:ipos2)=newl(:len_trim(newl))

end subroutine domath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine doop(line,ipos1,ipos2)                       !@(#)doop(3f): find VAL.OP.VAL strings and reduce to .TRUE. or .FALSE.
character(len=G_line_length)       :: line
integer                            :: ipos1
integer                            :: ipos2

   character(len=4),parameter      :: ops(6) = (/'.EQ.','.NE.','.GE.','.GT.','.LE.','.LT.'/)
   character(len=G_var_len)        :: val1
   character(len=G_var_len)        :: val2
   character(len=7)                :: temp

   character(len=G_line_length)    :: newl
   integer                         :: i,j,k

   newl=line(ipos1:ipos2)
   CHECK_EACH_OP_TYPE: do i=1,6
      FIND_MORE_OF: do
         G_dc=.false.
         if (index(newl,ops(i)).ne.0) then                       ! found current operator looking for
            do j=index(newl,ops(i))-1,1,-1
               if (newl(j:j).eq.'.') then
                  exit
               endif
            enddo
            call getval(newl,j+1,index(newl,ops(i))-1,val1)
            do k=index(newl,ops(i))+4,len_trim(newl)
               if (newl(k:k).eq.'.')then
                  exit
               endif
            enddo
            call getval(newl,index(newl,ops(i))+4,k-1,val2)
            select case(i)                                       ! determine truth
            case(1)                                              ! .eq.
               if (val1.eq.val2) G_dc=.true.
            case(2)                                              ! .ne.
               if (val1.ne.val2) G_dc=.true.
            case(3)                                              ! .ge.
               if (val1.ge.val2) G_dc=.true.
            case(4)                                              ! .gt.
               if (val1.gt.val2) G_dc=.true.
            case(5)                                              ! .le.
               if (val1.le.val2) G_dc=.true.
            case(6)                                              ! .lt.
               if (val1.lt.val2) G_dc=.true.
            case default
            end select
            temp='.FALSE.'
            if (G_dc) temp='.TRUE.'
            call rewrit(newl,temp(:len_trim(temp)),j,j,k,k)
            newl=nospace(newl)
            cycle
         endif
         exit
      enddo FIND_MORE_OF
   enddo CHECK_EACH_OP_TYPE
   if (ipos1.ne.1) then
      line=line(:ipos1-1)//newl(:len_trim(newl))//line(ipos2+1:)
   else
      line=newl(:len_trim(newl))//line(ipos2+1:)
   endif
   line=nospace(line)
end subroutine doop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
   logical function trufal(line,ipos1,ipos2)       ! @(#)trufal(3f): convert variable name or .TRUE./.FALSE. to a logical value
   character(len=G_line_length),intent(in) :: line           ! line containing string to interpret as a logical value
   integer,intent(in)                    :: ipos1            ! starting column of substring in LINE
   integer,intent(in)                    :: ipos2            ! ending column of substring in LINE

   character(len=G_var_len)                :: value          ! substring to extract from LINE
   integer                               :: i                ! loop counter
   integer                               :: ios              ! error code returned by an internal READ
   integer                               :: ifound           ! index in dictionary at which a variable name was found, or -1

   trufal=.false.                                            ! initialize return value
   value=line(ipos1:ipos2)                                   ! extract substring from LINE to interpret
   ifound=-1                                                 ! flag if successfully converted string, or index variable name found

   select case (value)                                       ! if string is not a logical string assume it is a variable name
   case ('.FALSE.','.F.')
      ifound=0                                               ! set flag to indicate a good value has been found
      trufal=.false.                                         ! set appropriate return value
   case ('.TRUE.','.T.')
      ifound=0                                               ! set flag to indicate a good value has been found
      trufal=.true.                                          ! set appropriate return value
   case default                                              ! assume this is a variable name, find name in dictionary
      do i=1,G_numdef
         if (G_defvar(i).eq.value) then                      ! found variable name in dictionary
            ifound=i                                         ! record index in diction where variable was found
            exit
         endif
      enddo

      if (ifound.eq.-1) then                                 ! if not a defined variable name stop program
         call stop_prep('*preptrufal* ERROR(040) - UNDEFINED PARAMETER.'//trim(G_source))
      endif

      read(G_defval(ifound),'(l4)',iostat=ios) trufal        ! try to read a logical from the value for the variable name

      if(ios.ne.0)then                                       ! not successful in reading string as a logical value
            call stop_prep('*preptrufal* ERROR(041) - CONSTANT LOGICAL EXPRESSION REQUIRED.'//trim(G_source))
      endif

   end select

   if (ifound.lt.0) then                                     ! not a variable name or string '.TRUE.' or '.FALSE.'
      call stop_prep('*preptrufal* ERROR(042) - CONSTANT LOGICAL EXPRESSION REQUIRED:'//trim(G_source))
   endif

   end function trufal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine logic(line,ipos1,ipos2)           !@(#)logic(3f): process .OP. operator strings
   character(len=*)             :: line
   integer,intent(in)           :: ipos1, ipos2

   logical                      :: one, two
   character(len=7)             :: temp
   character(len=G_line_length) :: newl
   character(len=6),save        :: ops(5)= (/'.NOT. ','.AND. ','.OR.  ','.EQV. ','.NEQV.'/)
   integer                      :: i,j,k,l
   integer                      :: ieqv
   integer                      :: ineqv
   integer                      :: i1
   integer                      :: iop
   integer                      :: len
   integer                      :: len1
   integer                      :: len2

   newl=line(ipos1:ipos2)
   len1=0
   len2=0
   one=.false.
   LOOP: do i=1,3
      20 continue
           LEN=5
           IF (I.EQ.3) LEN=4
           IF (INDEX(NEWL,OPS(I)(:len_trim(OPS(I)))).EQ.0) cycle
           I1=INDEX(NEWL,OPS(I)(:len_trim(OPS(I))))-1
           J=I1+1
           LEN1=0
           IF (I.NE.1) then
              OUTER: DO J=I1,1,-1
                INNER: DO K=1,5
                   LEN1=5
                   IF (K.EQ.3) LEN1=4
                   IF (INDEX(NEWL(J:I1),OPS(K)(:len_trim(OPS(K)))).NE.0) exit OUTER
                enddo INNER
              enddo OUTER
              IF (J.EQ.0) LEN1=1
              ONE=TRUFAL(NEWL,J+LEN1,I1)
           endif

           OUT: DO L=I1+LEN,len_trim(NEWL)
             IN: DO K=1,5
                LEN2=5
                IF (K.EQ.3) LEN2=4
                IF (INDEX(NEWL(I1+LEN:L),OPS(K)(:len_trim(OPS(K)))).NE.0) exit OUT
             enddo IN
           enddo OUT

           IF (L.GT.len_trim(NEWL)) LEN2=0
           TWO=TRUFAL(NEWL,I1+LEN+1,L-LEN2)

           select case(i)
           case(1); G_dc=.not.two
           case(2); G_dc=one.and.two
           case(3); G_dc=one.or.two
           case default
              call stop_prep('*prep* internal error')
           end select

           temp='.FALSE.'
           if (G_dc) temp='.TRUE.'
           call rewrit(newl,temp(:len_trim(temp)),j,j+len1-1,l,l-len2+1)
        goto 20
   enddo LOOP
   TILLDONE: do
      ieqv=index(newl,'.EQV.')
      ineqv=index(newl,'.NEQV')
      if (ieqv*ineqv.eq.0.and.ieqv.ne.ineqv) then
        iop=max(ieqv,ineqv)
      elseif (ieqv.ne.0) then
        iop=min(ieqv,ineqv)
      elseif (ipos1.eq.1) then
        line=newl(:len_trim(newl))//line(ipos2+1:)
        return
      else
        line=line(:ipos1-1)//newl(:len_trim(newl))//line(ipos2+1:)
        return
      endif
      len=5
      if (index(newl,'.EQV.').ne.iop) len=6
      do j=iop-1,1,-1
         if (newl(j:j+1).eq.'V.') exit
      enddo
      if (j.eq.0) len1=1
      one=trufal(newl,j+len1,iop-1)
      do l=iop+len,len_trim(newl)
         if (newl(l:l+1).eq.'.E'.or.newl(l:l+1).eq.'.N') exit
      enddo
      if (l.gt.len_trim(newl)) len2=0
      two=trufal(newl,iop+len,l+len2)
      G_dc=one.eqv.two
      if (len.ne.5) G_dc=one.neqv.two
      temp='.FALSE.'
      if (G_dc) temp='.TRUE.'
      call rewrit(newl,temp(:len_trim(temp)),j,j+len1-1,l,l-len2+1)
   enddo TILLDONE
end subroutine logic
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine eval(line)                                   !@(#)eval(3f): evaluate math expression to .TRUE. or .FALSE.
character(len=G_line_length)        :: line
   character(len=7)               :: value

   call parens(line)
   call math(line,1,len_trim(line))
   call doop(line,1,len_trim(line))
   call logic(line,1,len_trim(line))
   value=line(1:7)

   if (value.ne.'.TRUE.'.and.value.ne.'.FALSE.') then
      call stop_prep('*prepeval* ERROR(043) - value neither true or false:'//trim(value)//' when evaluating: '//trim(G_source))
   endif

   read(value,'(l4)') G_dc

end subroutine eval
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function get_integer_from_string(line) !@(#)get_integer_from_string(3f): read integer value from line(ipos1:ipos2)
                                                         ! assume string is a variable name or an integer value
   character(len=*),intent(in)  :: line                             ! string containing substring to read an integer value from

   character(len=G_var_len)     :: value                            ! the substring
   integer                      :: i                                ! index of variable dictionary where variable name is stored
   integer                      :: ios                              ! I/O error value to check to see if internal reads succeeded
   integer                      :: get_integer_from_string          ! integer value to return if string is converted successfully
   integer                      :: ipos1, ipos2

   ipos1=min(1,len(line))
   ipos2=len(line)
   ipos2=min(max(1,ipos2),ipos2)
   if(len(line).eq.0)then
      get_integer_from_string=0
   elseif (line(ipos1:ipos1).ge.'A'.and.line(ipos1:ipos1).le.'Z') then  ! not a number, now assumed to  be a variable name
      value= line(ipos1:ipos2)                                      ! extract substring that is assumed to be a variable name
      i=-1                                                          ! this will be index where variable name is found in dictionary
      do i=1,G_numdef                                               ! scan variable dictionary for the variable name
        if (G_defvar(i).eq.value) exit
      enddo
      if (i.gt.G_numdef.or.i.lt.0)then                              ! if variable name not found in dictionary, stop
        call stop_prep('*prepgi* ERROR(044) - UNDEFINED PARAMETER:'//trim(G_source))
      endif
      read(G_defval(i),'(i11)',iostat=ios) get_integer_from_string  ! read integer value from the value associated with name
      if(ios.ne.0)then                                              ! failed reading integer from value, stop
        call stop_prep('*prepgi* ERROR(045) - MUST BE INTEGER:'//trim(G_source))
      endif
   else                                                             ! input is not a variable name, assume it represents an integer
      read(line(ipos1:ipos2),'(i11)',iostat=ios) get_integer_from_string               ! try to read integer value from input string
      if(ios.ne.0)then                                              ! failed to convert the string to an integer, so stop
        call stop_prep('*prepgi* ERROR(046) - MUST BE INTEGER:'//trim(G_source))
      endif
   endif                                                            ! return integer value
end function get_integer_from_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine rewrit(line,temp,j,j1,l,l1)                           !@(#)rewrit(3f):
character(len=G_line_length)  line
character(len=*)           :: temp
integer                    :: j
integer                    :: j1
integer                    :: l
integer                    :: l1


   if (j.eq.0.and.l.gt.len_trim(line)) then      ! done
      line=temp
   elseif (j.eq.0) then                          ! first item
      line=temp//line(l1:)
   elseif (l.gt.len_trim(line)) then             ! last item
      if (j1.ne.0) then
         line=line(:j1)//temp
      else
         line=temp
      endif
   else                                          ! middle item
        line=line(:j1)//temp//line(l1:)
   endif
end subroutine rewrit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine document(opts)                    ! @(#)document(3f): process BLOCK command to start or stop special processing
character(len=*),intent(in) :: opts

! CHECK COMMAND SYNTAX
   if(G_outtype.eq.'help')then  ! if in 'help' mode wrap up the routine
      write(G_iout,'(a)')"'']"
      write(G_iout,'(a)')"   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))"
      write(G_iout,'(a)')"   stop ! if --help was specified, stop"
      write(G_iout,'(a)')"endif"
      write(G_iout,'(a)')"end subroutine help_usage"
      !x!write(G_iout,'("!",a)')repeat('-',131)
   elseif(G_outtype.eq.'variable')then     ! if in 'variable' mode wrap up the variable
      write(G_iout,'(a)')"'']"
   elseif(G_outtype.eq.'version')then  ! if in 'version' mode wrap up the routine
      write(G_iout,'("''@(#)COMPILED:       ",a,"'',&")') getdate('long')//'>'
      write(G_iout,'(a)')"'']"
      write(G_iout,'(a)')"   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))"
      !*!write(G_iout,'(a)')'   write(*,*)"COMPILER VERSION=",COMPILER_VERSION()'
      !*!write(G_iout,'(a)')'   write(*,*)"COMPILER OPTIONS=",COMPILER_OPTIONS()'
      write(G_iout,'(a)')"   stop ! if --version was specified, stop"
      write(G_iout,'(a)')"endif"
      write(G_iout,'(a)')"end subroutine help_version"
      !x!write(G_iout,'("!",a)')repeat('-',131)
   endif

   call dissect2('block','--oo --file --varname textblock --append .false.',opts) ! parse options on input line

   ! if a previous command has opened a --file FILENAME flush it, because a new one is being opened or this is an END command
   ! and if a --file FILENAME has been selected open it
   call print_comment_block()

   ! now can start new section
   G_MAN=''
   if(sget('block_file').ne.'')then
      G_MAN_FILE=sget('block_file')
      G_MAN_COLLECT=.true.
   else
      G_MAN_FILE=''
      G_MAN_COLLECT=.false.
   endif

   G_MAN_PRINT=.false.
   if(lget('block_append'))then
      G_MAN_FILE_POSITION='APPEND'
   else
      G_MAN_FILE_POSITION='ASIS'
   endif

   select case(upper(sget('block_oo')))

   case('COMMENT')
      G_outtype='comment'
      G_MAN_PRINT=.true.
      G_MAN_COLLECT=.true.

   case('NULL')
      G_outtype='null'

   case('VARIABLE')
      G_outtype='variable'
      write(G_iout,'(a)')trim(sget('block_varname'))//'=[ CHARACTER(LEN=128) :: &'
      G_MAN_PRINT=.true.

   case('HELP')
      G_outtype='help'
      write(G_iout,'(a)')'subroutine help_usage(l_help)'
      write(G_iout,'(a)')'implicit none'
      write(G_iout,'(a)')'character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"'
      write(G_iout,'(a)')'logical,intent(in)             :: l_help'
      !write(G_iout,'(a)')'character(len=128),allocatable :: help_text(:)'
      write(G_iout,'(a)')'character(len=:),allocatable :: help_text(:)'
      write(G_iout,'(a)')'integer                        :: i'
      write(G_iout,'(a)')'logical                        :: stopit=.false.'
      write(G_iout,'(a)')'stopit=.false.'
      write(G_iout,'(a)')'if(l_help)then'
! NOTE: Without the type specification this constructor would have to specify all of the constants with the same character length.
      write(G_iout,'(a)')'help_text=[ CHARACTER(LEN=128) :: &'

         select case(G_comment_style)  ! duplicate help text as a comment for some code documentation utilities
         case('doxygen')               ! convert plain text to doxygen comment blocks with some automatic markdown highlights
            G_MAN_PRINT=.true.
         case('fort')                  ! convert plain text to ford  comment blocks with some automatic markdown highlights
            G_MAN_PRINT=.true.
         case('none')                  ! do not print comment lines from block
            G_MAN_PRINT=.false.
         case default
         end select

   case('VERSION')
      G_outtype='version'
      write(G_iout,'(a)')'subroutine help_version(l_version)'
      write(G_iout,'(a)')'implicit none'
      write(G_iout,'(a)')'character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"'
      write(G_iout,'(a)')'logical,intent(in)             :: l_version'
      !write(G_iout,'(a)')'character(len=128),allocatable :: help_text(:)'
      write(G_iout,'(a)')'character(len=:),allocatable   :: help_text(:)'
      write(G_iout,'(a)')'integer                        :: i'
      write(G_iout,'(a)')'logical                        :: stopit=.false.'
      write(G_iout,'(a)')'stopit=.false.'
      write(G_iout,'(a)')'if(l_version)then'
! NOTE: Without the type specification this constructor would have to specify all of the constants with the same character length.
      write(G_iout,'(a)')'help_text=[ CHARACTER(LEN=128) :: &'

   case('WRITE')
      G_outtype='write'
   case('','END')
      G_outtype='asis'
      G_MAN_COLLECT=.false.
   case('ASIS')
      G_outtype='asis'
   case default
      write(*,*)'*prepstop* ERROR(047) - UNEXPECTED "BLOCK" OPTION. FOUND:'//trim(G_source)
      write(*,*)'*prepstop* ERROR(048) - UNEXPECTED "BLOCK" OPTION. FOUND:'//trim(sget('block_oo'))
      call stop_prep('*prepstop* ERROR(049) - UNEXPECTED "BLOCK" OPTION. FOUND:'//sget('block_man'))
   end select

   G_comment_count=0
end subroutine document
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stop(opts)                    ! @(#)stop(3f): process stop directive
character(len=*),intent(in) :: opts
integer                     :: ivalue

! CHECK COMMAND SYNTAX
   if(opts.ne.'')then
      ivalue=get_integer_from_string(opts)
      if(ivalue.eq.0)then
         stop
      elseif(ivalue.ge.1.and.ivalue.le.20)then
         !*!stop ivalue
         stop 3
      else
         call stop_prep('*prepstop* ERROR(050) - UNEXPECTED "STOP" VALUE='',i10,''. FOUND:'',a)'//trim(G_source))
      endif
   else
      stop 1
   endif
end subroutine stop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine print_comment_block() !@(#)print_comment_block(3f): format comment block to file in document directory and output
   character(len=:),allocatable :: filename
   character(len=1024)          :: varvalue
   character(len=*),parameter   :: varname='PREP_DOCUMENT_DIR'
   integer                      :: ios,iend,istatus,ilength

   if(.not.allocated(G_MAN))then
      return
   endif

   call get_environment_variable(varname,varvalue,ilength,istatus)
   select case(istatus)
   case(0)
   case(-1);     call stop_prep('ERROR(051) print_comment_block - VARIABLE VALUE TOO LONG:'//trim(varname))
   case(1);     !call stop_prep('ERROR(print_comment_block) - VARIABLE DOES NOT EXIST:'//trim(varname))
   case(2);      call stop_prep('ERROR(052) print_comment_block - COMPILER DOES NOT SUPPORT ENVIRONMENT VARIABLES:'//trim(varname))
   case default; call stop_prep('ERROR(053) print_comment_block - UNEXPECTED STATUS VALUE '//v2s(istatus)//':'//trim(varname))
   end select

   if(ilength.ne.0.and.G_MAN.ne.''.and.G_MAN_FILE.ne.' ')then ! if $BLOCK ... --file FILE is present generate file in directory/doc
      filename=trim(varvalue)//'/doc/'

      iend=len_trim(varvalue)

      if(varvalue(iend:iend).ne.'/')then
         filename=trim(varvalue)//'/doc/'//trim(G_MAN_FILE)
      else
         filename=trim(varvalue)//'doc/'//trim(G_MAN_FILE)
      endif

      if(filename.eq.' ') filename='OOPS.txt'

      open(unit=70,file=filename,iostat=ios,action='write',position=G_MAN_FILE_POSITION)

      if(ios.ne.0)then
         call stop_prep('ERROR(054) print_comment_block - FAILED TO OPEN DOCUMENT OUTPUT FILE:'//trim(filename))
      else
         if(len(G_MAN).gt.1)then                   ! the way the string is built it starts with a newline
            write(70,'(a)',iostat=ios) G_MAN(2:)
         else
            write(70,'(a)',iostat=ios) G_MAN
         endif
         if(ios.ne.0)then
            call stderr('G_MAN='//G_MAN)
            call stop_prep('ERROR(055) print_comment_block - FAILED TO WRITE OUTPUT FILE:'//trim(filename))
         endif
      endif

      close(70,iostat=ios)

   endif

   ! now if $BLOCK COMMENT print comment block
   if(G_MAN_PRINT)then
      call format_G_MAN()
   endif

end subroutine print_comment_block
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine format_g_man()
   character(len=:),allocatable   :: array(:) ! output array of tokens
   integer                        :: ios
   integer                        :: i
   ALL: block
      WRITEIT: block

         select case(G_comment_style)

         case('doxygen')                 ! convert plain text to doxygen comment blocks with some automatic markdown highlights
            if(len(G_MAN).gt.1)then      ! the way the string is built it starts with a newline
               CALL split(G_MAN,array,delimiters=new_line('N'),nulls='return') ! parse string to an array parsing on delimiters
               do i=1,size(array)        ! lines starting with a letter and all uppercase letters is prefixed with "##"
                  if( upper(array(i)).eq.array(i) .and. isalpha(array(i)(1:1)).and.lower(array(i)).ne.array(i))then
                     array(i)='##'//trim(array(i))
                     select case(array(i))
                     case('##SYNOPSIS','##EXAMPLES','##EXAMPLE')
                        array(i)=trim(array(i))//new_line('N')//'!'//'!'
                     endselect
                  else
                     array(i)=' '//trim(array(i))
                  endif
               enddo

               if(size(array).gt.0)then
                  write(G_iout,'("!",">",a)')trim(array(1))
               endif

               do i=2,size(array)
                  write(G_iout,'("!","!",a)',iostat=ios)trim(array(i))
                  if(ios.ne.0)exit WRITEIT
               enddo

            endif
            !x!write(G_iout,'("!",131("="))')

         case('ford')                    ! convert plain text to doxygen comment blocks with some automatic markdown highlights
            if(len(G_MAN).gt.1)then      ! the way the string is built it starts with a newline
               CALL split(G_MAN,array,delimiters=new_line('N'),nulls='return') ! parse string to an array parsing on delimiters
               do i=1,size(array)        ! lines starting with a letter and all uppercase letters is prefixed with "##"
                  if( upper(array(i)).eq.array(i) .and. isalpha(array(i)(1:1)).and.lower(array(i)).ne.array(i))then
                     array(i)='## '//trim(array(i))
                     select case(array(i))
                     case('## SYNOPSIS','## EXAMPLES','## EXAMPLE')
                        array(i)=trim(array(i))//new_line('N')//'!>'
                     endselect
                  else
                     array(i)=' '//trim(array(i))
                  endif
               enddo

               if(size(array).gt.0)then
                  write(G_iout,'("!>",a)')trim(array(1))
               endif

               do i=2,size(array)
                  write(G_iout,'("!>",a)',iostat=ios)trim(array(i))
                  if(ios.ne.0)exit WRITEIT
               enddo

            endif
            !x!write(G_iout,'("!>",131("="))')

         case('none')                    ! ignore comment block

         case default
            if(len(G_MAN).gt.1)then                       ! the way the string is built it starts with a newline
               G_MAN=G_MAN(2:)//repeat(' ',2*len(G_MAN))  ! make sure the white-space exists
               call substitute(G_MAN,NEW_LINE('A'),NEW_LINE('A')//'! ')
               G_MAN='! '//trim(G_MAN)
            endif
            write(G_iout,'(a)',iostat=ios) G_MAN
            if(ios.ne.0)exit WRITEIT
            !x!write(G_iout,'("!",131("="))')
         end select

         exit ALL
      endblock WRITEIT
      call stderr('G_MAN='//G_MAN)
      call stop_prep('ERROR(056) print_comment_block - FAILED TO WRITE COMMENT BLOCK')
   endblock ALL
end subroutine format_g_man
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stops(iexit) !@(#)stops(3f): irritating that the value on STOP is scalar-char-initialization-expr. 1-20 OK this way
integer,intent(in)   :: iexit
   select case(iexit)
   case  (1)      ;  stop  1
   case  (2)      ;  stop  2
   case  (3)      ;  stop  3
   case  (4)      ;  stop  4
   case  (5)      ;  stop  5
   case  (6)      ;  stop  6
   case  (7)      ;  stop  7
   case  (8)      ;  stop  8
   case  (9)      ;  stop  9
   case  (10)     ;  stop  10
   case  (11)     ;  stop  11
   case  (12)     ;  stop  12
   case  (13)     ;  stop  13
   case  (14)     ;  stop  14
   case  (15)     ;  stop  15
   case  (16)     ;  stop  16
   case  (17)     ;  stop  17
   case  (18)     ;  stop  18
   case  (19)     ;  stop  19
   case  (20)     ;  stop  20
   case  default  ;  stop
   end   select
end subroutine stops
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine debug_state(msg)                        !@(#)debug(3f): process $SHOW command or state output when errors occur
character(len=*),intent(in)   :: msg
   integer                    :: i

   write(G_iout,'(a)')'!==============================================================================='
   write(G_iout,'(a)')'! '//trim(msg)

   write(G_iout,'(a)')'! CURRENT STATE OF PREP(1):'
   write(G_iout,'("! TOTAL LINES READ ............... ",i0)')G_io_total_lines     ! write number of lines read
   write(G_iout,'("! CONDITIONAL_NESTING_LEVEL....... ",i0)')G_nestl              ! write nesting level
   write(G_iout,'("! G_WRITE (general processing).... ",l0)')G_write              ! non-if/else/endif directives processed
   write(G_iout,'("! G_LLWRITE (write input lines)... ",l0)')G_llwrite            ! non-if/else/endif directives processed
   write(G_iout,'("! DATE............................ ",a)')getdate()             ! current time stamp
   call write_arguments()
   write(G_iout,'(a)')'! VARIABLES:'
   do i=1,G_numdef                                                                ! print variable dictionary
      write(G_iout,'("! ",a," ! ",a)')G_defvar(i),G_defval(i)                     ! write variable and corresponding value
   enddo

   write(G_iout,'(a)')'! OPEN FILES:'
   write(G_iout,'(a)')'!     ! ---- ! UNIT ! LINE NUMBER ! FILENAME'
   do i=1,G_iocount                                                               ! print file dictionary
      ! write table of files
      write(G_iout,'("!     ! ",i4," ! ",i4," ! ",i11," ! ",a)')i,  &
      &  G_file_dictionary(i)%unit_number,    &
      &  G_file_dictionary(i)%line_number,    &
      &  trim(G_file_dictionary(i)%filename )
   enddo

   write(G_iout,'(a)')'! INCLUDE DIRECTORIES:'
   do i=1,G_inc_count
      write(G_iout,'("! ",a)') trim(G_inc_files(i))
   enddo

   if(size(keywords).gt.0)then
      write(G_iout,'(a)')'! SET STRINGS:'
      write(G_iout,*)'>>>',size(keywords)
      write(G_iout,'(*("! ",a,"==> ",a,/))')(keywords(i),values(i)(:counts(i)),i=1,size(keywords))
   endif

   write(G_iout,'(a)')'!==============================================================================='
end subroutine debug_state
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_arguments() ! @(#)write_arguments(3f): return all command arguments as a string

   integer                      :: istatus          !  status (non-zero means error)
   integer                      :: ilength          !  length of individual arguments
   integer                      :: i                !  loop count
   integer                      :: icount           !  count of number of arguments available
   character(len=255)           :: value            !  store individual arguments one at a time

   write(G_iout,'(a)',advance='no')'! ARGUMENTS ...................... '
   icount=command_argument_count()                  ! intrinsic gets number of arguments
   do i=1,icount
      call get_command_argument(i,value,ilength,istatus)
      write(G_iout,'(a,1x)',advance='no')value(:ilength)
   enddo
   call write_out('')
end subroutine write_arguments
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine include(line,iunit)  ! @(#)include(3f): add file to input file list
implicit none
character(len=G_line_length),intent(in)  :: line
integer,intent(in)                       :: iunit
   integer                               :: ios
   character(len=4096)                   :: message

   if(iunit.eq.5.or.line.eq.'@')then                   ! assume this is stdin
      G_iocount=G_iocount+1
      G_file_dictionary(G_iocount)%unit_number=5
      G_file_dictionary(G_iocount)%filename=line
      return
   endif

   call findit(line)

   open(unit=iunit,file=trim(line),iostat=ios,status='old',action='read',iomsg=message)
   if(ios.ne.0)then
      call debug_state('OPEN IN INCLUDE')
      call stderr(message)
      call stop_prep("*prep* ERROR(057) - FAILED OPEN OF INPUT FILE("//v2s(iunit)//"):"//trim(line))
   else
      rewind(unit=iunit)
      G_iocount=G_iocount+1
      if(G_iocount.gt.size(G_file_dictionary))then
         call stop_prep('*prep* ERROR(058) - INPUT FILE NESTING TOO DEEP:'//trim(G_source))
      endif
      G_file_dictionary(G_iocount)%unit_number=iunit
      G_file_dictionary(G_iocount)%filename=line
      G_file_dictionary(G_iocount)%line_number=0
   endif

end subroutine include
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine post(parcel_name)  ! @(#)post(3f): switch to scratch file defined by PARCEL
implicit none
character(len=*),intent(in)  :: parcel_name
integer                      :: ifound
integer                      :: ios
character(len=4096)          :: message
integer                      :: i
   ifound=-1
   do i=1,G_parcelcount
      if(G_parcel_dictionary(i)%name.eq.parcel_name)then
         ifound=G_parcel_dictionary(i)%unit_number
         exit
      endif
   enddo
   if(ifound.eq.-1)then
      call stop_prep('*prep* ERROR(059) - PARCEL NAME NOT DEFINED:'//trim(G_source))
   else
      inquire(unit=ifound,iostat=ios)
      rewind(unit=ifound,iostat=ios,iomsg=message)
      if(ios.ne.0)then
         call stop_prep('*prep* ERROR(060) - ERROR REWINDING PARCEL:'//trim(G_source)//':'//trim(message))
      endif

      !d!do
      !d!   read(ifound,'(a)',iostat=ios)message
      !d!   if(ios.ne.0)exit
      !d!   write(*,*)'>>>'//trim(message)
      !d!enddo
      !d!rewind(unit=ifound,iostat=ios,iomsg=message)

      G_iocount=G_iocount+1
      if(G_iocount.gt.size(G_file_dictionary))then
         call stop_prep('*prep* ERROR(061) - INPUT FILE NESTING TOO DEEP:'//trim(G_source))
      endif
      G_file_dictionary(G_iocount)%unit_number=ifound
      G_file_dictionary(G_iocount)%filename=parcel_name
      G_file_dictionary(G_iocount)%line_number=0
   endif
end subroutine post
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine findit(line) !@(#)findit(3f): look for filename in search directories if name does not exist and return modified name
character(len=G_line_length)             :: line
   character(len=G_line_length)          :: filename
   logical                               :: file_exist
   integer                               :: i
   integer                               :: iend_dir

   inquire(file=trim(line), exist=file_exist)                    ! test if input filename exists
   if(file_exist)then                                            ! if file exits then return filename
      return
   endif

   if(G_inc_count.gt.0)then                                      ! if search directories have been specified search for file
      do i=1,G_inc_count
         iend_dir=len_trim(G_inc_files(i))
         if(G_inc_files(i)(iend_dir:iend_dir).ne.'/')then
            filename=G_inc_files(i)(:iend_dir)//'/'//trim(line)
         else
            filename=G_inc_files(i)(:iend_dir)//trim(line)
         endif
         inquire(file=trim(filename), exist=file_exist)
         if(file_exist)then                                      ! if find filename exit
            line=filename
            return
         endif
      enddo
   else                                                          ! file did not exist and no search directories have been specified
      filename=trim(line)
   endif

   call stop_prep("*prep* ERROR(062) - MISSING INPUT FILE:"//trim(filename))

end subroutine findit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine opens()                   !@(#)opens(3f): use expression on command line to  open input files

   integer,parameter                     :: n=50                    ! maximum number of tokens to look for
   character(len=G_line_length)          :: array(n)                ! the array to fill with tokens
   character(len=1)                      :: dlim=' '                ! string of single characters to use as delimiters

   integer                               :: icount                  ! how many tokens are found
   integer                               :: ibegin(n)               ! starting column numbers for the tokens in INLINE
   integer                               :: iterm(n)                ! ending column numbers for the tokens in INLINE
   integer                               :: ilen                    ! is the position of last non‐blank character in INLINE
   character(len=G_line_length)          :: in_filename1=''         ! input filename, default is stdin
   character(len=G_line_length)          :: in_filename2=''         ! input filename, default is stdin
   integer                               :: i, ii
   integer                               :: ivalue
   character(len=G_line_length)          :: dir                     ! directory used by an input file

   in_filename1(:G_line_length)  = sget('_prep_i')                  ! get values from command line
   in_filename2(:G_line_length)  = sget('prep_i')                   ! get values from command line
   if(in_filename1.ne.''.and.in_filename2.eq.in_filename1)then
      in_filename2=''
   endif
   if(in_filename2.eq.'')then                                       ! read stdin if no -i on command line
      in_filename2  = '@'
   endif

   ! break command argument prep_i into single words
   call delim(adjustl(trim(sget('_prep_i'))//' '//trim(in_filename2)),array,n,icount,ibegin,iterm,ilen,dlim)
   ivalue=50                                ! starting file unit to use
   do i=icount,1,-1
      G_source='$include '//trim(array(i))  ! for messages
      call include(array(i),ivalue)
      ivalue=ivalue+1

      ALREADY: block                       ! store directory path of input files as an implicit directory for reading $INCLUDE files
         dir=dirname(array(i))
         do ii=1,G_inc_count
            if(G_inc_files(ii).eq.dir)exit ALREADY
         enddo
         G_inc_count=G_inc_count+1
         G_inc_count=min(G_inc_count,size(G_inc_files)) ! guard against too many files; !*! should warn on overflow
         G_inc_files(G_inc_count)=dir
      endblock ALREADY

   enddo

! >>>
!   If ARRAY(N) fills before reaching the end of the line the routine stops.
!   Check "if(iend(icount) .eq. ilen)" to see if you got to the end to warn if not all files include

end subroutine opens
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine includes()         !@(#)includes(3f): use expression on command line to  get include directories

   integer,parameter                     :: n=50                    ! maximum number of tokens to look for
   character(len=1)                      :: dlim=' '                ! string of single characters to use as delimiters
   integer                               :: ibegin(n)               ! starting column numbers for the tokens in G_inc_files
   integer                               :: iterm(n)                ! ending column numbers for the tokens in G_inc_files
   integer                               :: ilen                    ! is the position of last non‐blank character in G_inc_files

   ! G_inc_files is the array to fill with tokens
   ! G_inc_count is the number of tokens found

   ! break command argument prep_I into single words
   call delim(adjustl(trim(sget('_prep_I'))//' '//trim(sget('prep_I'))),G_inc_files,n,G_inc_count,ibegin,iterm,ilen,dlim)
end subroutine includes
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine defines()       !@(#)defines(3f): use expressions on command line to define variables
   integer,parameter                     :: n=300                   ! maximum number of tokens to look for
   character(len=G_line_length)          :: array(n)                ! the array to fill with tokens
   character(len=1)                      :: dlim=' '                ! string of single characters to use as delimiters

   integer                               :: icount                  ! how many tokens are found
   integer                               :: ibegin(n)               ! starting column numbers for the tokens in INLINE
   integer                               :: iterm(n)                ! ending column numbers for the tokens in INLINE
   integer                               :: ilen                    ! is the position of last non‐blank character in INLINE
   character(len=G_line_length)          :: in_define1=''           ! variable definition from environment variable
   character(len=G_line_length)          :: in_define2=''           ! variable definition from environment variable and command
   integer                               :: i

   if(allocated(keywords))deallocate(keywords)
   if(allocated(values))deallocate(values)
   if(allocated(counts))deallocate(counts)
   allocate(character(len=0) :: keywords(0))
   allocate(character(len=0) :: values(0))
   allocate(counts(0))

   in_define1=sget('_prep_oo')
   in_define2=sget('prep_oo')
   if(in_define1.ne.''.and.in_define2.eq.in_define1)then            ! if duplicates remove one
      in_define2=''
   endif
   ! break command argument prep_oo into single words
   call delim(adjustl(trim(in_define1)//' '//trim(in_define2))//' '//trim(sget('prep_D')),array,n,icount,ibegin,iterm,ilen,dlim)
   do i=1,icount
      G_source='$redefine '//trim(array(i))
      call cond() ! convert variable name into a "$define variablename" directive and process it
   enddo


!   If ARRAY(N) fills before reaching the end of the line the routine stops.
!   Check "if(iend(icount) .eq. ilen)" to see if you got to the end.

end subroutine defines
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stop_prep(message)                   !@(#)stop_prep(3f): write MESSAGE to stderr and exit program
character(len=*),intent(in)  :: message
   call stderr(message)
   call stderr(trim(G_SOURCE))
   call debug_state('message')
   stop 1
end subroutine stop_prep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

! This documentation is a combination of
!    o the original Lahey documentation of fpp(1) from "LAHEY FORTRAN REFERENCE MANUAL"; Revision C, 1992;
!    o examination of the code
!    o and documentation for the features subsequently added to the program.

subroutine help_usage(l_help)
implicit none
!@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   prep(1) - [DEVELOPER] pre-process FORTRAN source files                       ',&
'   (LICENSE:MIT)                                                                ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   prep  [[-D] define_list]                                                     ',&
'         [-I include_directories]                                               ',&
'         [-i input_file(s)]                                                     ',&
'         [-o output_file]                                                       ',&
'         [--system]                                                             ',&
'         [--verbose]                                                            ',&
'         [--prefix character|ADE]                                               ',&
'         [--keeptabs]                                                           ',&
'         [--noenv]                                                              ',&
'         [--width n]                                                            ',&
'         [-d ignore|remove|blank]                                               ',&
'         [--comment default|doxygen|ford|none]                                  ',&
'         [--version]                                                            ',&
'         [--help]                                                               ',&
'DESCRIPTION                                                                     ',&
'                                                                                ',&
'   By default the pre-processor prep(1) will interpret lines with "$" in column ',&
'   one, and will output no such lines. Other input is conditionally written to  ',&
'   the output file based on the directives encountered in the input. It does    ',&
'   not support parameterized macros but does support string substitution and    ',&
'   the inclusion of free-format text blocks that may be converted to Fortran    ',&
'   comments or CHARACTER variable definitions while simultaneously being used   ',&
'   to generate documentation files. INTEGER or LOGICAL expressions may be used  ',&
'   to select output lines.                                                      ',&
'                                                                                ',&
'   The suggested suffix for Fortran input files is ".ff" for code files unless  ',&
'   they contain $SYSTEM directives in which case ".FF" is preferred. $INCLUDE   ',&
'   files should use ".ffinc" and ".FFINC" if they include prep(1) directives.   ',&
'   This naming convention is not required.                                      ',&
'                                                                                ',&
'   An exclamation character on a valid directive begins an in-line comment      ',&
'   that is terminated by an end-of-line.                                        ',&
'                                                                                ',&
'   An expression is composed of INTEGER and LOGICAL constants, parameters       ',&
'   and operators. Operators are                                                 ',&
'                                                                                ',&
'     .NOT.  .AND.  .OR.  .EQV.  .NEQV.  .EQ.  .NE.  .GE.                        ',&
'     .GT.   .LE.   .LT.  +      -       *     /     (                           ',&
'     )      **                                                                  ',&
'                                                                                ',&
'   The syntax for the directive lines is as follows:                            ',&
'                                                                                ',&
'    :CONDITIONAL CODE SELECTION                                                 ',&
'     $DEFINE   variable_name[=expression]                 [! comment ]          ',&
'     $UNDEFINE variable_name                              [! comment ]          ',&
'     $IF       expression| [$IFDEF|$IFNDEF variable_name] [! comment ]          ',&
'               { sequence of source statements}                                 ',&
'     [$ELSEIF  {LOGICAL or INTEGER expression}            [! comment ]          ',&
'               { sequence of source statements}]                                ',&
'     [$ELSE                                               [! comment ]          ',&
'               { sequence of source statements}]                                ',&
'     $ENDIF                                               [! comment ]          ',&
'     $STOP     [stop_value]                               [! comment ]          ',&
'                                                                                ',&
'    :MACRO STRING EXPANSION AND TEXT REPLAY                                     ',&
'     $SET      varname  string                                                  ',&
'     $IMPORT   envname(s)                                                       ',&
'     $PARCEL   blockname                                  [! comment ]          ',&
'     $POST     blockname                                  [! comment ]          ',&
'                                                                                ',&
'    :FILE AND TEXT BLOCK USAGE                                                  ',&
'     $OUTPUT   filename  [-append]                        [! comment ]          ',&
'     $INCLUDE  filename                                   [! comment ]          ',&
!     change so just $BLOCK name/$ENDBLOCK and
!     $OUTPUT $BLOCK $POST all replaced by $POST if no options slurp file and process if any options
!     $BLOCK
!     $ENDBLOCK
'     $BLOCK    [comment|null|write|help|version|variable [-varname NAME]]       ',&
'               [-file NAME [-append]]                     [! comment ]          ',&
'                                                                                ',&
'    :IDENTIFIERS                                                                ',&
'     $IDENT | $@(#)    metadata                           [! comment ]          ',&
'                                                                                ',&
'    :INFORMATION                                                                ',&
'     $MESSAGE  message_to_stderr                                                ',&
'     $SHOW                                                [! comment ]          ',&
'                                                                                ',&
'    :SYSTEM COMMANDS                                                            ',&
'     $SYSTEM   system_command                             [! comment ]          ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   define_list, -D define_list  An optional space-delimited list of expressions ',&
'                                used to define variables before file processing ',&
'                                commences.                                      ',&
'   -i input_files               The default input file is stdin. Filenames are  ',&
'                                space-delimited. In a list, @ represents stdin. ',&
'   -o output_file               The default output file is stdout.              ',&
'   -I include_directories       The directories to search for files specified on',&
'                                $INCLUDE directives.                            ',&
'   --prefix ADE|letter  The default directive prefix character is "$".          ',&
'                        Alternatives may be specified by providing an           ',&
'                        ASCII Decimal Equivalent (Common values are 37=%        ',&
'                        42=* 35=# 36=$ 64=@). If the value is not numeric       ',&
'                        it is assumed to be a literal character.                ',&
'                                                                                ',&
'   --help           Display documentation and exit.                             ',&
'   --verbose        All commands on a $SYSTEM directive are echoed              ',&
'                    to stderr with a + prefix. Text following the               ',&
'                    string "@(#)" is printed to stderr similar to               ',&
'                    the Unix command what(1) but is otherwise                   ',&
'                    treated as other text input.                                ',&
'                                                                                ',&
'   --noenv          The $IFDEF and $IFNDEF directives test for an               ',&
'                    internal prep(1) variable and then an                       ',&
'                    environment variable by default. This option                ',&
'                    turns off testing for environment variables.                ',&
'   --system         Allow system commands on $SYSTEM directives to              ',&
'                    be executed.                                                ',&
'   --keeptabs       By default tab characters are expanded assuming             ',&
'                    a stop has been set every eight columns; and                ',&
'                    trailing carriage-return characters are removed.            ',&
'                    Use this flag to prevent this processing from               ',&
'                    occurring.                                                  ',&
'   --comment        try to style comments generated in $BLOCK blocks            ',&
'                    for other utilities such as doxygen. Default is to          ',&
'                    prefix lines with ''! ''. Allowed keywords are              ',&
'                    currently "default", "doxygen","none","ford".               ',&
'                    THIS IS AN ALPHA FEATURE AND NOT FULLY IMPLEMENTED.         ',&
'   -d ignore|remove|blank  Enable special treatment for lines beginning         ',&
'                           with "d" or "D" The letter will be left as-is        ',&
'                           (the default); removed; or replaced with a blank     ',&
'                           character. This non-standard syntax has been         ',&
'                           used to support the optional compilation of          ',&
'                           "debug" code by many Fortran compilers when          ',&
'                           compiling fixed-format Fortran source.               ',&
'   --version        Display version and exit                                    ',&
'   --width n        Maximum line length of the output file. The default is 1024.',&
'                    Typically used to trim fixed-format FORTRAN code that       ',&
'                    contains comments or "ident" labels past column 72 when     ',&
'                    when compiling fixed-format Fortran code.                   ',&
'                                                                                ',&
'   DIRECTIVES                                                                   ',&
'                                                                                ',&
'   $DEFINE variable_name [=expression]                                          ',&
'                                                                                ',&
'   A $DEFINE may appear anywhere in a source file. If the value is ".TRUE."     ',&
'   or ".FALSE." then the parameter is of type LOGICAL, otherwise the            ',&
'   parameter is of type INTEGER and the value must be an INTEGER. If no         ',&
'   value is supplied, the parameter is of type INTEGER and is given the         ',&
'   value 1.                                                                     ',&
'                                                                                ',&
'   Constant parameters are defined from the point they are encountered in a     ',&
'   $DEFINE directive until program termination unless explicitly                ',&
'   undefined with a $UNDEFINE directive.                                        ',&
'                                                                                ',&
'   Example:                                                                     ',&
'                                                                                ',&
'    $define A=1                                                                 ',&
'    $define B=1                                                                 ',&
'    $define C=2                                                                 ',&
'    $if ( A + B ) / C .eq. 1                                                    ',&
'       (a+b)/c is one                                                           ',&
'    $endif                                                                      ',&
'                                                                                ',&
'   $IF/$ELSEIF/$ELSE/$ENDIF directives                                          ',&
'                                                                                ',&
'   Each of the control lines delineates a block of FORTRAN source. If the       ',&
'   expression following the $IF is ".TRUE.", then the lines of FORTRAN          ',&
'   source following are output. If it is ".FALSE.", and an $ELSEIF              ',&
'   follows, the expression is evaluated and treated the same as the $IF. If     ',&
'   the $IF and all $ELSEIF expressions are ".FALSE.", then the lines of         ',&
'   source following the $ELSE are output. A matching $ENDIF ends the            ',&
'   conditional block.                                                           ',&
'                                                                                ',&
'   $IFDEF/$IFNDEF directives                                                    ',&
'                                                                                ',&
'   $IFDEF and $IFNDEF are special forms of the $IF directive that simply test   ',&
'   if a variable name is defined or not. Essentially, these are equivalent:     ',&
'                                                                                ',&
'     $IFDEF varname  ==> $IF DEFINED(varname)                                   ',&
'     $IFNDEF varname ==> $IF .NOT. DEFINED(varname)                             ',&
'                                                                                ',&
'   except that environment variables are tested as well if the --noenv option   ',&
'   is not specified.                                                            ',&
'                                                                                ',&
'   $IDENT metadata [-language fortran|c|shell]                                  ',&
'                                                                                ',&
'   Writes a line using SCCS-metadata format of the following forms:             ',&
'                                                                                ',&
'     language:                                                                  ',&
'     fortran   character(len=*),parameter::ident="@(#)metadata"                 ',&
'     c         #ident "@(#)metadata"                                            ',&
'     shell     #@(#) metadata                                                   ',&
'                                                                                ',&
'   This string is generally included for use with the what(1) command.          ',&
'                                                                                ',&
'   "$@(#)" is an alias for "$IDENT" so the source file itself will contain      ',&
'   SCCS-metadata so the metadata can be displayed with what(1).                 ',&
'                                                                                ',&
'   The default language is "fortran". Depending on your compiler and the        ',&
'   optimization level used when compiling, these strings may or may not         ',&
'   remain in the object files and executables created.                          ',&
'                                                                                ',&
'   Do not use the characters double-quote, greater-than, backslash (">\)        ',&
'   in the metadata to remain compatible with SCCS metadata syntax.              ',&
'   Do not use strings starting with " -" either.                                ',&
'                                                                                ',&
'   $OUTPUT filename [-append]                                                   ',&
'                                                                                ',&
'   Specify the output file to write to. Overrides the initial output file       ',&
'   specified with command line options. If no output filename is given          ',&
'   revert back to initial output file. @ is a synonym for stdout.               ',&
'                                                                                ',&
'      -append [.true.|.false]                                                   ',&
'                                                                                ',&
'   Named files open at the beginning by default. Use the -append switch to      ',&
'   append to the end of an existing file instead of overwriting it.             ',&
'                                                                                ',&
'   $INCLUDE filename                                                            ',&
'                                                                                ',&
'   Nested read of specified input file. Fifty (50) nesting levels are allowed.  ',&
'                                                                                ',&
'   $PARCEL [name]                                                               ',&
'                                                                                ',&
'   The lines between a "$PARCEL name" and "$PARCEL" block are written WITHOUT   ',&
'   expanding directives to a scratch file that can then be read in with the     ',&
'   $POST directive much like a named file can be with $INCLUDE.                 ',&
'                                                                                ',&
'   $POST name                                                                   ',&
'                                                                                ',&
'   Read in the scratch file created by the $PARCEL directive. Combined with     ',&
'   $SET directives this allows you to replay a section of input and replace     ',&
'   strings as a simple templating technique.                                    ',&
'                                                                                ',&
'   $SET name string                                                             ',&
'                                                                                ',&
'   If a $SET directive defines a name prep(1) enters expansion mode. In this    ',&
'   mode anywhere the string "${NAME}" is encountered in subsequent output it    ',&
'   is replaced by "string". Comments should not be used on a $SET directive.    ',&
'   Note expansion of a line may cause it to be longer than allowed by some      ',&
'   compilers. Automatic breaking into continuation lines does not occur.        ',&
'                                                                                ',&
'   IF A $SET DIRECTIVE HAS BEEN DEFINED the "standard" preprocessor values      ',&
'   ${FILE}, ${LINE}, ${DATE}, and ${TIME} are also available. The time          ',&
'   data refers to the time of processing, not the current time nor the time     ',&
'   of compilation or loading.                                                   ',&
'                                                                                ',&
'   $IMPORT names(s)                                                             ',&
'                                                                                ',&
'   The values of environment variables may be imported such that their names    ',&
'   and values will be set as if a $SET command had been done on them.           ',&
'                                                                                ',&
'   $BLOCK [comment|null|write|help|version  [-file NAME [-append]]              ',&
'     or                                                                         ',&
'   $BLOCK VARIABLE --varname NAME  [--file NAME]                                ',&
'                                                                                ',&
'      COMMENT:   write text prefixed by an exclamation and a space              ',&
'      WRITE:     write text as Fortran WRITE(3f) statements                     ',&
'                 The Fortran generated is free-format. It is assumed the        ',&
'                 output will not generate lines over 132 columns.               ',&
'      HELP:      write text as a subroutine called HELP_USAGE                   ',&
'      VERSION:   write text as a subroutine called HELP_VERSION                 ',&
'                 prefixing lines with @(#) for use with the what(1) command.    ',&
'      NULL:      Do not write into current output file                          ',&
'      VARIABLE:  write as a text variable. The name may be defined using the    ',&
'                 --varname switch. Default name is "textblock".                 ',&
'      END:       End block of specially processed text                          ',&
'                                                                                ',&
'   If the "-file NAME" option is present the *unaltered* text is written to     ',&
'   the specified file. This allows documentation to easily be maintained in     ',&
'   the source file. It can be tex, html, markdown or any plain text.            ',&
'   The filename will be prefixed with $PREP_DOCUMENT_DIR/doc/ . If the          ',&
'   environment variable $PREP_DOCUMENT_DIR is not set the option is ignored.    ',&
'                                                                                ',&
'   The text can easily be processed by other utilities such as markdown(1)      ',&
'   or txt2man(1) to produce man(1) pages and HTML documents. $SYSTEM commands   ',&
'   may follow the $BLOCK block text to optionally post-process the doc files.   ',&
'                                                                                ',&
'   A blank value or "END" returns to normal output processing.                  ',&
'                                                                                ',&
'   $SHOW                                                                        ',&
'                                                                                ',&
'   Shows current state of prep(1); including variable names and values; and     ',&
'   the name of the current input files. All output is preceded by an            ',&
'   exclamation character.                                                       ',&
'                                                                                ',&
'   Example:                                                                     ',&
'                                                                                ',&
'    prep A=10 B C D -o paper                                                    ',&
'    $define z=22                                                                ',&
'    $show                                                                       ',&
'    $stop 0                                                                     ',&
'                                                                                ',&
'    !======================================================================     ',&
'    !  CURRENT STATE                                                            ',&
'    !     TOTAL LINES READ ............ 2                                       ',&
'    !     CONDITIONAL_NESTING_LEVEL.... 0                                       ',&
'    !     DATE......................... 11:18 21Jun2013                         ',&
'    !     ARGUMENTS ................... A=10 B C D -o paper                     ',&
'    !  VARIABLES:                                                               ',&
'    !     ! A                               !          10                       ',&
'    !     ! B                               !           1                       ',&
'    !     ! C                               !           1                       ',&
'    !     ! D                               !           1                       ',&
'    !     ! Z                               !          22                       ',&
'    !  OPEN FILES:                                                              ',&
'    !     ! ---- ! UNIT ! LINE NUMBER ! FILENAME                                ',&
'    !     !    1 !    5 !           2 !                                         ',&
'    !======================================================================     ',&
'                                                                                ',&
'   $STOP stop_value                                                             ',&
'                                                                                ',&
'   Stops input file processing. An optional integer value of 0 to 20            ',&
'   will be returned as a status value to the system where supported. A          ',&
'   value of two ("2") is returned if no value is specified. Any value           ',&
'   from one ("1") to twenty ("20") also causes an implicit execution of         ',&
'   the "$SHOW" directive before the program is stopped. A value of "0"          ',&
'   causes normal program termination. "$QUIT" is an alias for "$STOP 0".        ',&
'                                                                                ',&
'   $SYSTEM system_command                                                       ',&
'                                                                                ',&
'   If system command processing is enabled using the --system switch system     ',&
'   commands can be executed for such tasks as creating files to be read or to   ',&
'   further process documents created by $BLOCK. $SYSTEM directives are ignored  ',&
'   by default; as you clearly need to ensure the input file is trusted before   ',&
'   before allowing commands to be executed. Commands that are system-specific   ',&
'   may need to be executed conditionally as well.                               ',&
'                                                                                ',&
'   Examples:                                                                    ',&
'                                                                                ',&
'    $! build variable definitions using GNU/Linux commands                      ',&
'    $SYSTEM echo system=`hostname` > compiled.h                                 ',&
'    $SYSTEM echo compile_time="`date`" >> compiled.h                            ',&
'    $INCLUDE compiled.h                                                         ',&
'                                                                                ',&
'    $! obtain up-to-date copy of source file from HTTP server:                  ',&
'    $SYSTEM wget http://repository.net/src/func.F90 -O - >_tmp.f90              ',&
'    $INCLUDE _tmp.f90                                                           ',&
'    $SYSTEM  rm _tmp.f90                                                        ',&
'                                                                                ',&
'   $UNDEFINE variable_name                                                      ',&
'                                                                                ',&
'   A symbol defined with $DEFINE can be removed with the $UNDEFINE              ',&
'   directive.                                                                   ',&
'                                                                                ',&
'   DEFINED(variable_name)                                                       ',&
'                                                                                ',&
'   A special function called DEFINED() may appear only in a $IF or $ELSEIF.     ',&
'   If "variable_name" has been defined at that point in the source code,        ',&
'   then the function value is ".TRUE.", otherwise it is ".FALSE.". A name is    ',&
'   defined only if it has appeared in the source previously in a $DEFINE        ',&
'   directive or been declared on the command line.                              ',&
'   The names used in compiler directives are district from names in the         ',&
'   FORTRAN source, which means that "a" in a $DEFINE and "a" in a FORTRAN       ',&
'   source statement are totally unrelated.                                      ',&
'   The DEFINED() parameter is NOT valid in a $DEFINE directive.                 ',&
'                                                                                ',&
'   Example:                                                                     ',&
'                                                                                ',&
'    >        Program test                                                       ',&
'    > $IF .NOT. DEFINED (inc)                                                   ',&
'    >        INCLUDE ''''comm.inc''''                                           ',&
'    > $ELSE                                                                     ',&
'    >        INCLUDE ''''comm2.inc''''                                          ',&
'    > $ENDIF                                                                    ',&
'    >        END                                                                ',&
'                                                                                ',&
'   The file, "comm.inc" will be INCLUDEd in the source if the parameter,        ',&
'   "inc", has not been previously defined, while INCLUDE "comm2.inc" will       ',&
'   be included in the source if "inc" has been previously defined. This is      ',&
'   useful for setting up a default inclusion.                                   ',&
'                                                                                ',&
'   Predefined values are                                                        ',&
'                                                                                ',&
'    UNKNOWN = 0 LINUX   = 1 MACOS   = 2 WINDOWS = 3                             ',&
'    CYGWIN  = 4 SOLARIS = 5 FREEBSD = 6 OPENBSD = 7                             ',&
'    In addition OS is set to what the program guesses the system type is.       ',&
'                                                                                ',&
'   $MESSAGE WARNING message                                                     ',&
'                                                                                ',&
'   Write message to stderr                                                      ',&
'                                                                                ',&
'LIMITATIONS                                                                     ',&
'                                                                                ',&
'   $IF constructs can be nested up to 20 levels deep. Note that using           ',&
'   more than two levels typically makes input files less readable.              ',&
'                                                                                ',&
'   $BLOCK END is required after a $BLOCK or --file FILENAME is not written.     ',&
'                                                                                ',&
'   Nesting of $BLOCK sections not allowed.                                      ',&
'                                                                                ',&
'   Messages for $MESSAGE do not treat an exclamation as starting a comment      ',&
'                                                                                ',&
'  Input files                                                                   ',&
'                                                                                ',&
'   o lines are limited to 1024 columns. Text past column 1024 is ignored.       ',&
'   o files currently opened cannot be opened again.                             ',&
'   o a maximum of 50 files can be nested by $INCLUDE                            ',&
'   o filenames cannot contain spaces on the command line.                       ',&
'                                                                                ',&
' Variable names                                                                 ',&
'                                                                                ',&
'   o cannot be redefined unless first undefined.                                ',&
'   o are limited to 31 characters.                                              ',&
'   o must start with a letter (A-Z).                                            ',&
'   o are composed of the letters A-Z, digits 0-9 and _ and $.                   ',&
'   o 2048 variable names may be defined at a time.                              ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'                                                                                ',&
'  Define variables on command line:                                             ',&
'                                                                                ',&
'   Typically, variables are defined on the command line when prep(1) is invoked ',&
'   but can be grouped together into small files that are included with a        ',&
'   $INCLUDE or as input files.                                                  ',&
'                                                                                ',&
'     prep HP size=64 -i hp_directives.dirs test.F90 -o test_out.f90             ',&
'                                                                                ',&
'   defines variables HP and SIZE as if the expressions had been on a $DEFINE    ',&
'   and reads file "hp_directives.dirs" and then test.F90. Output is directed    ',&
'   to test_out.f90                                                              ',&
'                                                                                ',&
'  Basic conditionals:                                                           ',&
'                                                                                ',&
'   > $! set value of variable "a" if it is not specified on the prep(1) command.',&
'   > $IF .NOT.DEFINED(A)                                                        ',&
'   > $   DEFINE a=1  ! so only define the following first version of SUB(3f)    ',&
'   > $ENDIF                                                                     ',&
'   >    program conditional_compile                                             ',&
'   >       call sub()                                                           ',&
'   >    end program conditional_compile                                         ',&
'   > $! select a version of SUB depending on the value of variable "a"          ',&
'   > $IF a .EQ. 1                                                               ',&
'   >    subroutine sub                                                          ',&
'   >       print*, "This is the first SUB"                                      ',&
'   >    end subroutine sub                                                      ',&
'   > $ELSEIF a .eq. 2                                                           ',&
'   >    subroutine sub                                                          ',&
'   >       print*, "This is the second SUB"                                     ',&
'   >    end subroutine sub                                                      ',&
'   > $ELSE                                                                      ',&
'   >    subroutine sub                                                          ',&
'   >       print*, "This is the third SUB"                                      ',&
'   >    end subroutine sub                                                      ',&
'   > $ENDIF                                                                     ',&
'                                                                                ',&
'  Common use of $BLOCK                                                          ',&
'                                                                                ',&
'   > $!                                                                         ',&
'   > $BLOCK NULL --file manual.tex                                              ',&
'   > This is a block of text that will be ignored on output but optionally      ',&
'   > written to a doc/ file when $PREP_DOCUMENT_DIR is set.                     ',&
'   > $BLOCK END                                                                 ',&
'   > $!                                                                         ',&
'   > $BLOCK COMMENT --file manual.tex --append                                  ',&
'   > This is a block of text that will be converted to comments and optionally  ',&
'   > appended to a doc/ file when $PREP_DOCUMENT_DIR is set.                    ',&
'   > $BLOCK END                                                                 ',&
'   > $!                                                                         ',&
'                                                                                ',&
'  Creating a help_usage(3f) subroutine and writing the same documentation to    ',&
'  a doc file (if the environment variable $PREP_DOCUMENT_DIR is set).           ',&
'                                                                                ',&
'   > $!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',&
'   > $! generate help_usage() procedure and file to run thru txt2man(1) or other',&
'   > $! filters to make man(1) page if $PREP_DOCUMENT_DIR is set.               ',&
'   > $!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',&
'   > $BLOCK HELP --file conditional_compile.man                                 ',&
'   > NAME                                                                       ',&
'   >     conditional_compile - basic example for prep(1) pre-processor.         ',&
'   > SYNOPSIS                                                                   ',&
'   >     conditional_example [--help] [--version]                               ',&
'   > DESCRIPTION                                                                ',&
'   >     This is a basic example program showing how documentation can be used  ',&
'   >     to generate program help text                                          ',&
'   > OPTIONS                                                                    ',&
'   >        --help                                                              ',&
'   >               display this help and exit                                   ',&
'   >        --version                                                           ',&
'   >               output version information and exit                          ',&
'   > $BLOCK END                                                                 ',&
'                                                                                ',&
'  Creating a help_version(3f) subroutine                                        ',&
'                                                                                ',&
'   > $!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',&
'   > $! generate help_version() procedure                                       ',&
'   > $BLOCK VERSION                                                             ',&
'   > DESCRIPTION: example program showing conditional compilation with prep(1)  ',&
'   > PROGRAM:     conditional_compile                                           ',&
'   > VERSION:     1.0.0, 20160703                                               ',&
'   > AUTHOR:      John S. Urban                                                 ',&
'   > $BLOCK END                                                                 ',&
'   > $!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',&
'                                                                                ',&
'  Sample program using help_usage(3f) and help_version(3f) and M_kracken95(3f): ',&
'                                                                                ',&
'   > program conditional_compile                                                ',&
'   >    use M_kracken95, only : kracken, lget                                   ',&
'   >    ! use M_kracken95 module to crack command line arguments                ',&
'   >    call kracken("cmd","--help .false. --version .false.")                  ',&
'   >    ! call routine generated by $BLOCK HELP                                 ',&
'   >    call help_usage(lget("cmd_help"))                                       ',&
'   >    ! call routine generated by $BLOCK VERSION                              ',&
'   >    call help_version(lget("cmd_version"))                                  ',&
'   > end program conditional_compile                                            ',&
'                                                                                ',&
' SET USAGE                                                                      ',&
'  Note values are case-sensitive but variable names are not, and there are      ',&
'  pre-defined values for input file, line in input file, date and time that     ',&
'  are NOT ACTIVE until at least one $SET or $IMPORT directive is processed. That',&
'  is, unless a variable name is defined no ${NAME} expansion occurs.            ',&
'                                                                                ',&
'   > $set author  William Shakespeare                                           ',&
'   > $import HOME                                                               ',&
'   > write(*,*)''By ${AUTHOR}''                                                 ',&
'   > write(*,*)''File ${FILE}''                                                 ',&
'   > write(*,*)''Line ${LINE}''                                                 ',&
'   > write(*,*)''Date ${DATE}''                                                 ',&
'   > write(*,*)''Time ${TIME}''                                                 ',&
'   > write(*,*)''HOME ${HOME}''                                                 ',&
'                                                                                ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'                                                                                ',&
'LICENSE                                                                         ',&
'   MIT                                                                          ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage

subroutine help_version(l_version)
implicit none
!@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        prep(1f)>',&
'@(#)DESCRIPTION:    Fortran Pre-processor>',&
!'@(#)VERSION:        4.0.0: 20170502>',&
!'@(#)VERSION:        5.0.0: 20201219>',&
'@(#)VERSION:        6.0.0: 20210613>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE       https://github.com/urbanjost/prep.git/>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_out(line)  ! @(#)writeout(3f):  write (most) source code lines to output file
character(len=*),intent(in)    :: line
   integer                     :: istart

   if(G_write_what)then              ! echo "what" lines to stderr
      istart=index(line,'@(#)')
      write(ERROR_UNIT,'("-->>",a)')trim(line(istart+4:))
   endif

   call www(line)
end subroutine write_out
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine www(line) ! @(#)www(3f):  change line into a WRITE, HELP/VERSION, COMMENT output line
character(len=*),intent(in)    :: line
character(len=:),allocatable   :: buff
character(len=115)             :: chunk
integer                        :: ilen

   select case(trim(G_outtype))

   case('comment')                             ! write as a Fortran comment preceded by two explanations and a space
                                               ! will be written later at end of BLOCK section

   case('null')                                ! do not write

   case('variable')
      buff=trim(line)                          ! do not make a line over 132 characters. Trim input line if needed
      buff=buff//repeat(' ',max(80,len(buff))) ! ensure space in buffer for substitute
      call substitute(buff,"'","''")           ! change single quotes in input to two adjacent single quotes
      ilen=max(len_trim(buff),80)              ! make all lines have at least 80 characters in the string for a more legible output
      write(G_iout,'("''",a,"'',&")') buff(:ilen)
   case('help')
      buff=trim(line)                          ! do not make a line over 132 characters. Trim input line if needed
      buff=buff//repeat(' ',max(80,len(buff))) ! ensure space in buffer for substitute
      call substitute(buff,"'","''")           ! change single quotes in input to two adjacent single quotes
      ilen=max(len_trim(buff),80)              ! make all lines have at least 80 characters in the string for a more legible output
      write(G_iout,'("''",a,"'',&")') buff(:ilen)

   case('version')                             ! write version information with SCCS ID prefix for use with what(1) command
      write(G_iout,'("''@(#)",a,"'',&")')trim(line(:min(len_trim(line),128-1)))//'>'

                                               !*! should handle longer lines and split them
   case('write')                               ! convert string to a Fortran write statement to unit "IO"
      buff=trim(line)                          ! do not make a line over 132 characters. Trim input line if needed
      buff=buff//repeat(' ',max(80,len(buff))) ! ensure space in buffer for substitute
      call substitute(buff,"'","''")
      write(G_iout,'(a)',advance='no')'write(io,''(a)'')'''
      chunk=buff
      write(G_iout,'(a)',advance='no')trim(chunk)
      write(G_iout,'(a)')''''

   case('','asis')
      write(G_iout,'(a)')trim(line(:min(len(line),G_iwidth)))

   case default
      call stop_prep('*prepstop* ERROR(063) - UNEXPECTED "BLOCK" VALUE. FOUND:'//trim(G_source))
      call stop_prep('*prepstop* ERROR(064) - UNEXPECTED "BLOCK" VALUE. FOUND:'//trim(G_outtype))

   end select

   if(G_MAN_COLLECT)then
      G_MAN=G_MAN//new_line('N')//trim(line)
   endif

   G_comment_count=G_comment_count+1

end subroutine www
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! duplicates from other modules
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stderr(msg)
implicit none
character(len=*),intent(in) :: msg
! ident_2="@(#)M_verify::stderr(3f): writes a message to standard error using a standard f2003 method"
integer             :: ios

   write(error_unit,'(a)',iostat=ios) trim(msg)
   flush(unit=output_unit,iostat=ios)
   flush(unit=error_unit,iostat=ios)
end subroutine stderr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine dissect2(verb,init,pars,error_return)
! "@(#)dissect2(3f): convenient call to parse() -- define defaults, then process"
!
      character(len=*),intent(in)  :: verb            ! the name of the command to be reset/defined  and then set
      character(len=*),intent(in)  :: init            ! used to define or reset command options; usually hard-set in the program.
      character(len=*),intent(in)  :: pars            ! defines the command options to be set, usually from a user input file
      integer                      :: ipars           ! length of the user-input string pars.
      integer,intent(out),optional :: error_return
      ipars=len(pars)
      call dissect(verb,init,pars,ipars,error_return)
end subroutine dissect2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine import(line)
character(len=*),intent(in)  :: line
character(len=:),allocatable :: names(:)
integer                      :: i
   names=sep(line)
   do i=1,size(names)
      call set(names(i)//' '//system_getenv(names(i)))
   enddo
end subroutine import
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set(line)
character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
character(len=:),allocatable :: val
integer                      :: iend
integer                      :: i
! create a dictionary with character keywords, values, and value lengths
! using the routines for maintaining a list

  G_expand=.true.

  call dissect2('set','-oo' ,line) ! parse options on input line
  iend=index(line//' ',' ')
  name=upper(line(:iend))
  if(name.eq.'')then
    ! show array
    if(size(keywords).gt.0)then
       write(G_iout,*)'>>>',size(keywords)
       write(G_iout,'(*("!",a,"==>","[",a,"]",/))')(trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords))
    endif
  else
     val=line(min(iend+1,len(line)):)
    ! insert and replace entries
    call update(name,val)
    ! remove some entries
    !call update('a')
    ! get some values
    !write(*,*)'get b=>',get('b')
  endif

contains
subroutine update(key,valin)
character(len=*),intent(in)           :: key
character(len=*),intent(in),optional  :: valin
integer                               :: place
integer                               :: ilen
character(len=:),allocatable          :: val
if(present(valin))then
   val=valin
   ilen=len_trim(val)
   ! find where string is or should be
   call locate(keywords,key,place)
   ! if string was not found insert it
   if(place.lt.1)then
      call insert(keywords,key,iabs(place))
      call insert(values,val,iabs(place))
      call insert(counts,ilen,iabs(place))
   else
      call replace(values,val,place)
      call replace(counts,ilen,place)
   endif
else
   call locate(keywords,key,place)
   if(place.gt.0)then
      call remove(keywords,place)
      call remove(values,place)
      call remove(counts,place)
   endif
endif
end subroutine update

function get(key) result(valout)
character(len=*),intent(in)   :: key
character(len=:),allocatable  :: valout
integer                       :: place
   ! find where string is or should be
   call locate(keywords,key,place)
   if(place.lt.1)then
      valout=''
   else
      valout=values(place)(:counts(place))
   endif
end function get

end subroutine set
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine expand_variables(line)
! @(#) brute force variable substitution. maybe add something like wordexp(3c) with command expansion only if --system?
! this is just to try the concept. Either use wordexp or an equivalent to replicate "here document" processing.
! note no automatic continuation of the line if it extends past allowed length, which for Fortran is currently 132 for free-format
! the way this is written it would do recursive substitution and does not know when there is just not a match
character(len=*)              :: line
character(len=:),allocatable  :: temp,search
integer,parameter             :: toomany=1000
integer                       :: i, j
character(len=4096)           :: scratch

write(scratch,'(i0)')G_file_dictionary(G_iocount)%line_number

call set('LINE ' // scratch)
call set('FILE ' // G_file_dictionary(G_iocount)%filename )
call set('TIME ' // getdate('time'))
call set('DATE ' // getdate('cdate'))
temp=trim(line)
do i=1,toomany
   do j=1,size(keywords)
      if(index(temp,'${').ne.0)then
         search='${'//trim(keywords(j))//'}'
         temp=str_replace(temp,search,values(j)(:counts(j)),ignorecase=.true.)
      else
         exit
      endif
   enddo
enddo
line=temp
end subroutine expand_variables
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getenv(3f) - [M_system:ENVIRONMENT] get environment variable
!!    from Fortran by calling get_environment_variable(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function system_getenv(name,default)
!!
!!     character(len=:),allocatable         :: system_getenv
!!     character(len=*),intent(in)          :: name
!!     character(len=*),intent(in),optional :: default
!!
!!##DESCRIPTION
!!    The system_getenv() function gets the value of an environment variable.
!!
!!##OPTIONS
!!    name     Return the value of the specified environment variable or
!!             blank if the variable is not defined.
!!    default  If the value returned would be blank this value will be used
!!             instead.
!!
!!##EXAMPLE
!!
!!   Sample setting an environment variable from Fortran:
!!
!!    program demo_system_getenv
!!    use M_system, only : system_getenv
!!    implicit none
!!       write(*,'("USER     : ",a)')system_getenv('USER')
!!       write(*,'("LOGNAME  : ",a)')system_getenv('LOGNAME')
!!       write(*,'("USERNAME : ",a)')system_getenv('USERNAME')
!!    end program demo_system_getenv
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function system_getenv(name,default) result(value)

! ident_23="@(#)M_system::system_getenv(3f): call get_environment_variable as a function with a default value(3f)"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: default
integer                              :: howbig
integer                              :: stat
character(len=:),allocatable         :: value

   if(NAME.ne.'')then
      call get_environment_variable(name, length=howbig, status=stat, trim_name=.true.)  ! get length required to hold value
      if(howbig.ne.0)then
         select case (stat)
         case (1)     ! print *, NAME, " is not defined in the environment. Strange..."
            value=''
         case (2)     ! print *, "This processor doesn't support environment variables. Boooh!"
            value=''
         case default ! make string to hold value of sufficient size and get value
            if(allocated(value))deallocate(value)
            allocate(character(len=max(howbig,1)) :: VALUE)
            call get_environment_variable(name,value,status=stat,trim_name=.true.)
            if(stat.ne.0)VALUE=''
         end select
      else
         value=''
      endif
   else
      value=''
   endif
   if(value.eq.''.and.present(default))value=default

end function system_getenv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!> Determine the OS type by guessing
subroutine get_os_type()
!!
!! At first, the environment variable `OS` is checked, which is usually
!! found on Windows. Then, `OSTYPE` is read in and compared with common
!! names. If this fails too, check the existence of files that can be
!! found on specific system types only.
!!
!! Returns OS_UNKNOWN if the operating system cannot be determined.
!!
!! calling POSIX or C routines would be far better, M_system::like system_uname(3f)
!! but trying to use portable Fortran.  If assume compiled by certain compilers could
!! use their extensions as well. Most have a uname(3f) function.
!!
integer, parameter :: OS_UNKNOWN = 0
integer, parameter :: OS_LINUX   = 1
integer, parameter :: OS_MACOS   = 2
integer, parameter :: OS_WINDOWS = 3
integer, parameter :: OS_CYGWIN  = 4
integer, parameter :: OS_SOLARIS = 5
integer, parameter :: OS_FREEBSD = 6
integer, parameter :: OS_OPENBSD = 7
character(len=32) :: val
integer           :: length, rc, r
logical           :: file_exists
character(len=80) :: scratch

   call define('UNKNOWN=0', 0)
   call define('LINUX=1', 0)
   call define('MACOS=2', 0)
   call define('WINDOWS=3', 0)
   call define('CYGWIN=4', 0)
   call define('SOLARIS=5', 0)
   call define('FREEBSD=6', 0)
   call define('OPENBSD=7', 0)

   r = OS_UNKNOWN
   ! Check environment variable `OS`.
   call get_environment_variable('OS', val, length, rc)
   if (rc == 0 .and. length > 0 .and. index(val, 'Windows_NT') > 0) then
       r = OS_WINDOWS
   else
      ! Check environment variable `OSTYPE`.
      call get_environment_variable('OSTYPE', val, length, rc)
      if (rc == 0 .and. length > 0) then
          if (index(val, 'linux') > 0) then      ! Linux
              r = OS_LINUX
          elseif (index(val, 'darwin') > 0) then ! macOS
              r = OS_MACOS
          elseif (index(val, 'win') > 0 .or. index(val, 'msys') > 0) then ! Windows, MSYS, MinGW, Git Bash
              r = OS_WINDOWS
          elseif (index(val, 'cygwin') > 0) then ! Cygwin
              r = OS_CYGWIN
          elseif (index(val, 'SunOS') > 0 .or. index(val, 'solaris') > 0) then ! Solaris, OpenIndiana, ...
              r = OS_SOLARIS
          elseif (index(val, 'FreeBSD') > 0 .or. index(val, 'freebsd') > 0) then ! FreeBSD
              r = OS_FREEBSD
          elseif (index(val, 'OpenBSD') > 0 .or. index(val, 'openbsd') > 0) then ! OpenBSD
              r = OS_OPENBSD
          endif
      endif
   endif
   if(r.eq.OS_UNKNOWN)then
      inquire (file='/etc/os-release', exist=file_exists) ! Linux
      if (file_exists) r = OS_LINUX
      inquire (file='/usr/bin/sw_vers', exist=file_exists) ! macOS
      if (file_exists) r = OS_MACOS
      inquire (file='/bin/freebsd-version', exist=file_exists) ! FreeBSD
      if (file_exists) r = OS_FREEBSD
   endif
   scratch=' '
   write(scratch,'("OS=",i0)')r
   call define(scratch,0)
end subroutine get_os_type
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_fpp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program prep                                                 !@(#)prep(1f): preprocessor for Fortran/FORTRAN source code
use M_kracken95, only : kracken, lget, rget, iget, sget, kracken_comment
use M_strings,   only : notabs, isdigit, switch
use M_fpp

implicit none
character(len=G_line_length) :: out_filename=''           ! output filename, default is stdout
character(len=1)             :: prefix                    ! directive prefix character
character(len=1)             :: letterd                   !

character(len=G_line_length) :: line                      ! working copy of input line
logical                      :: keeptabs=.false.          ! flag whether to retain tabs and carriage returns or not
integer                      :: ilast
integer                      :: ios
character(len=1024)          :: cmd=' &
   & -i                          &
   & -D                          &
   & -I                          &
   & -o                          &
   & --prefix           36       &
   & --keeptabs         .false.  &
   & -d                 ignore   &
   & --help             .false.  &
   & --verbose          .false.  &
   & --system           .false.  &
   & --version          .false.  &
   & --noenv            .false.  &
   & --comment          COMMENT  &
   & --width            1024     &
   & '
logical                       :: isscratch

                                                                        ! allow formatting comments for particular post-processors
   call get_environment_variable('PREP_COMMENT_STYLE',G_comment_style)  ! get environment variable for -comment switch
   if(G_comment_style.eq.'')G_comment_style='default'                   ! if environment variable not set set default
   call substitute(cmd,'COMMENT',trim(G_comment_style))                 ! change command line to have correct default
                                                                        ! this would actually allow any parameter after number

   kracken_comment='!'
   call kracken('prep',cmd)                                       ! define command arguments, default values and crack command line

   G_inc_files=' '

   out_filename(:G_line_length) = sget('prep_o')
   if ( all(isdigit(switch(trim(sget('prep_prefix'))))) ) then   ! if all characters are numeric digits
      prefix = char(iget('prep_prefix'))                         ! assume this is an ADE
   else
      prefix = sget('prep_prefix')                               ! not a digit so not an ADE so assume a literal character
   endif
   G_iwidth                     = iget('prep_width')
   G_iwidth=max(0,G_iwidth)
   letterd(1:1)               = sget('prep_d')
   G_noenv=lget('prep_noenv')

   if(out_filename.eq.'')then                              ! open output file
      G_iout=6
   elseif(out_filename.eq.'@')then
      G_iout=6
      G_IHELP=6
   else
      G_iout=60
      G_IHELP=60
      open(unit=60,file=out_filename,iostat=ios,action='write')
      if(ios.ne.0)then
         call stop_prep('*prep* ERROR(065) - FAILED TO OPEN OUTPUT FILE:'//trim(out_filename))
      endif
   endif
   G_iout_init=G_iout

   call help_version(lget('prep_version'))                 ! if version switch is present display version and exit
   call help_usage(lget('prep_help'))                      ! if help switch is present display help and exit

   keeptabs=lget('prep_keeptabs')
   G_write_what=lget('prep_verbose')                       ! set flag for special mode where lines with @(#) are written to stderr
   G_comment_style=lower(sget('prep_comment'))             ! allow formatting comments for particular post-processors
   G_system_on = lget('prep_system')                       ! allow system commands on $SYSTEM directives

   call get_os_type()
   call defines()                                          ! define named variables declared on the command line
   call includes()                                         ! define include directories supplies on command line
   call opens()                                            ! convert input filenames into $include directives

   READLINE: do                                            ! read loop to read input file
      read(G_file_dictionary(G_iocount)%unit_number,'(a)',end=7) line
      G_io_total_lines=G_io_total_lines+1
      G_file_dictionary(G_iocount)%line_number=G_file_dictionary(G_iocount)%line_number+1

      if(keeptabs)then
         G_source=line
      else
         call notabs(line,G_source,ilast)                  ! expand tab characters and trim trailing ctrl-M from DOS files
      endif

      if(G_expand)then
         call expand_variables(G_source)
      endif

      select case (line(1:1))                              ! special processing for lines starting with 'd' or 'D'
      case ('d','D')
         select case(letterd(1:1))
         case('i')                                         ! ignore
         case('r')                                         ! remove
            cycle
         case('b',' ')                                     ! blank
            line(1:1)=' '
         case('c')                                         ! comment
            line(1:1)='C'
         case('e')                                         ! exclamation
            line(1:1)='!'
         end select
      end select

      if (line(1:1).eq.prefix.and.line(2:2).ne.'{') then   ! prefix must be in column 1 for conditional compile directive
         call cond()                                       ! process directive
      elseif (G_write) then                                ! if last conditional was true then write line
         call write_out(trim(G_source))                    ! write data line
      endif
      cycle

7     continue                                                      ! end of file encountered on input
      if(G_file_dictionary(G_iocount)%unit_number.ne.5)then
         inquire(unit=G_file_dictionary(G_iocount)%unit_number,iostat=ios,named=isscratch)
         if(.not.isscratch.and. (G_file_dictionary(G_iocount)%unit_number.gt.0))then
            close(G_file_dictionary(G_iocount)%unit_number,iostat=ios)
         endif
      endif

      G_iocount=G_iocount-1

      if(G_iocount.lt.1)exit
   enddo READLINE

   if (G_nestl.ne.0) then                                           ! check to make sure all if blocks are closed
      call stop_prep('*prep* ERROR(067) - $IF BLOCK NOT CLOSED.')
   endif
   call print_comment_block()
end program prep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
