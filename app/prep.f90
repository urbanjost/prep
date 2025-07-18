!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! TODO ==>
! LOOK AT TEMPLATING:  Need arrays of values; way to call $POST with command-like options to set values or NAMELIST syntax;
!                      $POST should be elemental so if call with scalars and array it makes a call for each array element
!                      something like a $FOR statement and nicer looping on a parcel
! ${NAME} syntax could include more attributes of a bash(1) here document but perhaps allow expression inside of {} and array and
! implied do?
! ADD
! o line control  # linenumber "file"
! o looping
! CONSIDER
! o make $OUTPUT file nestable
! o allow multiple files on $INCLUDE?
! o undocument $BLOCK HELP|VERSION?
! o %,>>,<< operators
! o replace math parsing with M_calculator (but add logical operators to M_calculator)
! o cpp-like procedure macros
! o cpp or fpp compatibility mode
! o modularize and modernize calculator expression, if/else/endif
!
! REMOVED $REDEFINE and no longer produce warning message if redefine a variable, more like fpp(1) and cpp(1)
!
! some fpp versions allow integer intrinsics, not well documented but things like "#define AND char(34)"
!
! a PROCEDURE variable with current procedure name, maybe MODULE::PROCEDURE::CONTAINS format would be very handy in messages
!
! perhaps change to a more standard CLI syntax; but either way support multiple -D and maybe -D without a space before value
!
! extend $INCLUDE to call libcurl to access remote files
!
! case('ENDBLOCK');           call document(' ') ! BUG: '' instead of 'END' worked with kracken95, not with M_CLI2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!  @(#)prep: Fortran preprocessor
!  Fortran preprocessor originally based on public-domain FPP preprocessor from Lahey Fortran Code Repository
!     http://www.lahey.com/code.htm
!  Extensively rewritten since under a MIT License.
!     2013-10-03, 2020-12-19, 2021-06-12 : John S. Urban
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module prep__internal                                                             !@(#)prep__internal(3f): module used by prep program
USE ISO_FORTRAN_ENV, ONLY : STDERR=>ERROR_UNIT, STDOUT=>OUTPUT_UNIT,STDIN=>INPUT_UNIT
use M_io,        only : get_tmp, dirname, uniq, fileopen, filedelete, get_env, filebyte       ! Fortran file I/O routines
use M_CLI2,      only : set_args, sget, iget, lget, unnamed, specified ! ,print_dictionary ! load command argument parsing module
use M_strings,   only : nospace, v2s, substitute, upper, lower, isalpha, split, delim, str_replace=>replace, sep, pad, unquote
use M_strings,   only : glob, encode_base64
use M_list,      only : dictionary
use M_expr,      only : expr, get_integer_from_string, table
use M_match,     only : getpat, match, regex_pattern
use M_match,     only : YES, ERR
use M_attr,      only : attr, attr_mode
implicit none

integer,parameter                    :: num=2048                       ! number of named values allowed
integer,public,parameter             :: G_line_length=4096             ! allowed length of input lines
integer,public,parameter             :: G_var_len=63                   ! allowed length of variable names

logical,public                       :: G_ident=.false.                ! whether to write IDENT as a comment or CHARACTER

character(len=G_line_length),public  :: G_source                       ! original source file line
character(len=G_line_length),public  :: G_outline                      ! message to build for writing to output

type(dictionary),save                :: macro

type file_stack
   integer                           :: unit_number
   integer                           :: line_number=0
   character(len=G_line_length)      :: filename
end type
type(file_stack),public              :: G_file_dictionary(250)

type parcel_stack
   integer                           :: unit_number
   integer                           :: line_number=0
   character(len=G_line_length)      :: name
end type
type(parcel_stack),public            :: G_parcel_dictionary(500)

integer,save                         :: G_line_number=0
logical,save,public                  :: G_inparcel=.false.
integer,public                       :: G_iocount=0
integer,public                       :: G_parcelcount=0
integer,public                       :: G_io_total_lines=0
integer,public                       :: G_iwidth                       ! maximum width of line to write on output unit
logical,public                       :: G_noenv=.false.                ! ignore environment variables in $IFDEF and $IFNDEF

integer,public                       :: G_iout                         ! output unit
integer,save,public                  :: G_iout_init                    ! initial output unit
!integer,public                       :: G_ihelp=stderr                ! output unit for help text
integer,public                       :: G_ihelp=stdout                 ! output unit for help text
character(len=10),public             :: G_outtype='asis'
integer,public                       :: G_varname_length=128           ! length for variable string definitions
integer,public                       :: G_varname_width=0              ! minimum length for printing variable string definitions

integer,public                       :: G_inc_count=0
character(len=G_line_length),public  :: G_inc_files(50)
character(len=:),allocatable,save    :: G_MAN
logical,save                         :: G_MAN_COLLECT=.false.
logical,save                         :: G_MAN_PRINT=.false.
character(len=:),allocatable         :: G_MAN_FILE
character(len=10)                    :: G_MAN_FILE_POSITION='ASIS      '

integer,public                       :: G_nestl=0                      ! count of if/elseif/else/endif nesting level
integer,public,parameter             :: G_nestl_max=20                 ! maximum nesting level of conditionals

logical,save                         :: G_debug=.false.
logical,save,public                  :: G_verbose=.false.              ! verbose, including write strings after @(#) like what(1).
logical,save,public                  :: G_system_on=.false.            ! allow system commands or not on $SYSTEM

logical,public,save                  :: G_condop(0:G_nestl_max)        ! storage to keep track of previous write flags
data G_condop(0:G_nestl_max) /.true.,G_nestl_max*.false./
logical,public                       :: G_dc                           ! flag to determine write flag

logical,public                       :: G_write=.true.                 ! whether non-if/else/endif directives should be processed
logical,public                       :: G_llwrite=.true.               ! whether to write current line or skip it

integer,public                       :: G_comment_count=0
character(len=10),public             :: G_comment_style=' '
character(len=:),allocatable,public  :: G_comment
character(len=:),allocatable,save    :: G_scratch_file
integer,save                         :: G_scratch_lun=-1

type(regex_pattern)                  :: G_pattern_start
type(regex_pattern)                  :: G_pattern_stop
logical,save                         :: G_extract
character(len=:),allocatable,save    :: G_extract_start
character(len=:),allocatable,save    :: G_extract_stop
character(len=:),allocatable,save    :: G_extract_start0
character(len=:),allocatable,save    :: G_extract_stop0
logical,save                         :: G_extract_auto
logical,save                         :: G_extract_writeflag=.false.
logical,save                         :: G_underscore=.false.
character(len=:),allocatable,save    :: G_cmd
character(len=:),allocatable,save    :: G_file
character(len=:),allocatable,save    :: G_lang

logical                              :: G_cpp
character(len=1),save                :: G_deed='i'

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cond()       !@(#)cond(3f): process conditional directive assumed to be in SOURCE '$verb...'
character(len=G_line_length) :: line                       ! directive line with leading prefix character (default is $) removed
character(len=G_line_length) :: verb                       ! first word of command converted to uppercase
character(len=G_line_length) :: options                    ! everything after first word of command till end of line or !
character(len=G_line_length) :: upopts                     ! directive line with leading prefix removed; uppercase; no spaces
logical,save                 :: eb=.false.
integer,save                 :: noelse=0
integer                      :: verblen
logical                      :: ifound
integer                      :: ierr
character(len=G_var_len)     :: value

   line=adjustl(G_source(2:))                              ! remove leading prefix and spaces from directive line

   if (index(line//' ',G_comment) /= 0) then               ! assume if directive contains G_comment comment is present
                                                           ! LIMITATION: EVEN MESSAGES CANNOT CONTAIN COMMENTS
      line=line(:index(line//' ',G_comment)-1)             ! trim trailing comment from directive
   endif
   if (line(1:1) == G_comment)line=''
   if(line(1:4) == '@(#)')then
      verblen=5
   else
      verblen=scan(line,' (')
   endif
   if(verblen == 0)then
      verblen=len(line)
      verb=line
      options=' '
   else
      verb=line(:verblen-1)
      options=adjustl(line(verblen:))
   endif
   verb=upper(verb)
   upopts=nospace(upper(options))                          ! remove spaces from directive

   if(G_debug.and.G_verbose)then                           ! if processing lines in a logically selected region
      write(stderr,*)'G_SOURCE='//trim(g_source)           ! line as entered
      write(stderr,*)'LINE='//trim(line)                   ! everything before verb removed
      write(stderr,*)'VERB='//trim(verb)                   ! verb in uppercase
      write(stderr,*)'OPTIONS='//trim(options)             ! trimmed options
      write(stderr,*)'UPOPTS='//trim(upopts)               ! trimmed options in uppercase with whitespace removed
      call flushit()
   endif

   ifound=.true.
   if(G_write)then                                                    ! if processing lines in a logically selected region

      if(G_inparcel.and.(VERB /= 'PARCEL'.and.VERB /= 'ENDPARCEL') )then
         call write_out(trim(G_source))                               ! write data line
         return
      endif
                                                                      ! process the directive
      ierr=0
      select case(VERB)
      case('  ')                                                          ! entire line is a comment
      case('DEFINE','DEF','LET'); call expr(upopts,value,ierr,def=.true.) ! only process DEFINE if not skipping data lines
      case('REDEFINE','REDEF');   call expr(upopts,value,ierr,def=.true.) ! only process REDEFINE if not skipping data lines
      case('UNDEF','UNDEFINE','DELETE'); call undef(upper(options))       ! only process UNDEF if not skipping data lines

      case('INCLUDE','READ');     call include_cmd(options,50+G_iocount)  ! Filenames can be case sensitive
      case('OUTPUT','ENDOUTPUT','OPEN','CLOSE'); call output_cmd(options) ! Filenames can be case sensitive

      case('PARCEL');             call parcel_case(upopts)
      case('ENDPARCEL');          call parcel_case('')
      case('POST','CALL','DO');   call prepost(upper(options))

      case('BLOCK');              call document(options)
      case('ENDBLOCK');           call document(' ')

      case('SET','REPLACE','MACRO');      call set(options)
      case('UNSET');              call unset(upper(options))              ! only process UNSET if not skipping data lines

      case('IDENT','@(#)');       call ident(options)
      case('?');                  call write_deed(line(2:))
      case('MESSAGE','WARNING');  call write_err(unquote(options))        ! trustingly trim MESSAGE from directive
      case('SHOW') ;              call show_state(upper(options),msg='')
      CASE('HELP','CRIB');        call crib_help(stderr)

      case('STOP');               call stop(options)
      case('QUIT');               call stop('0 '//options)
      case('ERROR');              call stop('1 '//options)

      CASE('GET_ARGUMENTS');      call write_get_arguments()

      case('DEBUG');            G_debug=.not.G_debug      ;write(stderr,*)'DEBUG:',G_debug
      case('VERBOSE');          G_verbose=.not.G_verbose  ;write(stderr,*)'VERBOSE:',G_verbose

      case('IMPORT','GET_ENVIRONMENT_VARIABLE');           call import(options)
      case('SYSTEM','EXECUTE_COMMAND_LINE');               call exe()

      case default
         ifound=.false.
      end select
      if(ierr /= 0) call stop_prep('9065fd51-1c20-48a6-ac81-94235ada841c','expression invalid:',trim(G_source))
   endif

   select case(VERB)                                                  ! process logical flow control even if G_write is false
   case('ELSE','ELSEIF','ELIF');  call else(verb,upopts,noelse,eb)
   case('ENDIF','FI');            call endif(noelse,eb)
   case('IF');                    call if(upopts,noelse,eb)
   case('IFDEF','IFNDEF');        call def(verb,upopts,noelse,eb)
   case default
      if(.not.ifound)then
         call stop_prep('03f9f928-2781-468a-bb0c-f341457e4aa4',&
                 &'unknown compiler directive:', '['//trim(verb)//']: '//trim(G_SOURCE) )
      endif
   end select

end subroutine cond
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine exe()                             !@(#)exe(3f): Execute the command line specified by the string COMMAND.
character(len=G_line_length) :: command      ! directive line with leading prefix and directive name removed
character(len=G_line_length) :: defineme     ! scratch string for writing a DEFINE directive in to return command status
integer                      :: icmd=0
integer                      :: cstat
integer                      :: ierr
character(len=256)           :: sstat
character(len=G_var_len)     :: value

   if(G_system_on)then
      command=adjustl(G_source(2:))                                                 ! remove $ from directive
      command=command(7:)                                                           ! trim SYSTEM from directive
      if(G_verbose)then
         call write_err('+ '//command)
      endif

      ! not returning command status on all platforms
      call execute_command_line (command, exitstat=icmd,cmdstat=cstat,cmdmsg=sstat) ! execute system command

      if(icmd /= 0)then                                                             ! if system command failed exit program
         call stop_prep('4a60db0e-a8d2-4ee2-891f-7400b8cb5ae3','system command failed:',v2s(icmd))
      endif
   else
      call stop_prep('dc37d5a6-4a4f-40ef-8d7b-5073d8cf9a1d','system directive encountered but not enabled:',trim(G_SOURCE))
   endif

   write(defineme,'("CMD_STATUS=",i8)')icmd
   defineme=nospace(defineme)
   call expr(defineme,value,ierr)    ! only process DEFINE if not skipping data lines
   if(ierr /= 0) call stop_prep('0a92956e-5b6a-459f-821a-7cda7a028995','expression invalid:',trim(G_source))

end subroutine exe
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_get_arguments()                !@(#)write_get_arguments(3f): write block for processing M_CLI command line parsing
integer :: i
character(len=132),parameter :: text(*)=[character(len=132) :: &
"function get_arguments()"                                                                              ,&
"character(len=255)           :: message ! use for I/O error messages"                                  ,&
"character(len=:),allocatable :: string  ! stores command line argument"                                ,&
"integer                      :: get_arguments"                                                         ,&
"integer :: command_line_length"                                                                        ,&
"   call get_command(length=command_line_length)   ! get length needed to hold command"                 ,&
"   allocate(character(len=command_line_length) :: string)"                                             ,&
"   call get_command(string)"                                                                           ,&
"   ! trim off command name and get command line arguments"                                             ,&
"   string=adjustl(string)//' '                    ! assuming command verb does not have spaces in it"  ,&
"   string=string(index(string,' '):)"                                                                  ,&
"   string='&cmd '//string//' /'                   ! add namelist prefix and terminator"                ,&
"   read(string,nml=cmd,iostat=get_arguments,iomsg=message) ! internal read of namelist"                ,&
"   if(get_arguments /= 0)then"                                                                         ,&
"      write(*,'(''ERROR:'',i0,1x,a)')get_arguments, trim(message)"                                       ,&
"      write(*,*)'COMMAND OPTIONS ARE'"                                                                 ,&
"      write(*,nml=cmd)"                                                                                ,&
"      stop 1"                                                                                          ,&
"   endif"                                                                                              ,&
"end function get_arguments"                                                                            ,&
"" ]
do i=1,size(text)
   write(G_iout,'(a)')trim(text(i))
enddo
end subroutine write_get_arguments
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine output_cmd(opts)                                !@(#)output_cmd(3f): process $OUTPUT directive
character(len=*)             :: opts
character(len=G_line_length) :: filename                   ! filename on $OUTPUT command
character(len=20)            :: position
integer                      :: ios
   call dissect2('output','-oo --append F',opts)     ! parse options and inline comment on input line

   if(size(unnamed) > 0.and.opts /= '')then
      filename=unnamed(1)
   else
      filename=' '
   endif

   select case(filename)
   case('@')
      G_iout=stdout
   case(' ')                                               ! reset back to initial output file
      if(G_iout /= stdout.and.G_iout /= G_iout_init)then   ! do not close current output if it is stdout or default output file
         close(G_iout,iostat=ios)
      endif
      G_iout=G_iout_init
   case default
      G_iout=61
      close(G_iout,iostat=ios)
      if(lget('append'))then; position='append'; else; position='asis'; endif
      open(unit=G_iout,file=filename,iostat=ios,action='write',position=position)
      if(ios /= 0)then
         call stop_prep('6b75c2fe-b897-44b4-afbc-cfe7cb32e1b6','failed to open output file:',trim(filename))
      endif
   end select

   if(G_verbose)then
      call write_err( '+ output file changed to: '//trim(filename) )
   endif

end subroutine OUTPUT_CMD
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine parcel_case(opts)                          !@(#)parcel_case(3f): process $PARCEL directive
character(len=*)              :: opts
character(len=G_line_length)  :: name                 ! name on $PARCEL command
integer                       :: ios
integer                       :: lun
character(len=256)            :: message
   if(opts == '')then
      G_inparcel=.false.
      G_iout=G_iout_init
   else
      call dissect2('parcel','-oo ',opts)             ! parse options and inline comment on input line
      if(size(unnamed) > 0.and.opts /= '')then
         name=unnamed(1)
      else
         name=''
      endif
      open(newunit=lun,iostat=ios,action='readwrite',status='scratch',iomsg=message)
      if(ios /= 0)then
         call stop_prep('fb8ce648-6675-43e1-8210-90d245c03f75','failed to open parcel scratch file:',trim(name)//' '//trim(message))
      else
         G_parcelcount=G_parcelcount+1
         G_parcel_dictionary(G_parcelcount)%name=name
         G_parcel_dictionary(G_parcelcount)%unit_number=lun
         G_inparcel=.true.
         G_iout=lun
      endif
   endif
end subroutine parcel_case
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine prepost(opts)                          !@(#)prepost(3f): process $POST directive
character(len=*)                          :: opts
character(len=:),allocatable              :: list
character(len=:),allocatable              :: names(:)        ! names on $POST command
character(len=:),allocatable              :: fors(:)         ! names on $POST --for
integer                                   :: i
integer                                   :: j,jsz
   call dissect2('PARCEL',' --FOR " " ',opts)                 ! parse options and inline comment on input line

   list=''
   if(size(unnamed) == 0.and.opts /= '')then
      list=' '
   else
      do i=1,size(unnamed)
         list=list//' '//unnamed(i)
      enddo
   endif
   call split(list,names,delimiters=' ,')                    ! parse string to an array parsing on delimiters
   list=sget('FOR')
   call split(list,fors,delimiters=' ,')                     ! parse string to an array parsing on delimiters
   jsz=size(fors)
   do i=size(names),1,-1
      if(jsz == 0)then
         call post(names(i))
      else
         do j=jsz,1,-1
            call post(names(i))
            call post(fors(j))
         enddo
      endif
   enddo
end subroutine prepost
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine post(parcel_name)  !@(#)post(3f): switch to scratch file defined by PARCEL
implicit none
character(len=*),intent(in)  :: parcel_name
integer                      :: ifound
integer                      :: ios
character(len=4096)          :: message
integer                      :: i
   ifound=-1
   do i=1,G_parcelcount
      if(G_parcel_dictionary(i)%name == parcel_name)then
         ifound=G_parcel_dictionary(i)%unit_number
         exit
      endif
   enddo
   if(ifound == -1)then
      call stop_prep('7f8de002-15dc-4eed-a851-38f5c14c533c','parcel name not defined for',' PARCEL:'//trim(G_source))
   else
      inquire(unit=ifound,iostat=ios)
      rewind(unit=ifound,iostat=ios,iomsg=message)
      if(ios /= 0)then
         call stop_prep('f5a425ba-b9d8-4934-9ebe-e5b01f1f8bd5','error rewinding',' PARCEL:'//trim(G_source)//':'//trim(message))
      endif

      if(G_debug)then
         do
            read(ifound,'(a)',iostat=ios)message
            if(ios /= 0)exit
            write(stdout,*)'>>>'//trim(message)
         enddo
         rewind(unit=ifound,iostat=ios,iomsg=message)
      endif

      G_iocount=G_iocount+1
      if(G_iocount > size(G_file_dictionary))then
         call stop_prep('c2bd8c69-40b0-4e72-85d9-8637628a39a9','input file nesting too deep:',trim(G_source))
      endif
      G_file_dictionary(G_iocount)%unit_number=ifound
      G_file_dictionary(G_iocount)%filename=parcel_name
      G_file_dictionary(G_iocount)%line_number=0
   endif
end subroutine post
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine ident(opts)                                !@(#)ident(3f): process $IDENT directive
character(len=*)             :: opts
character(len=G_line_length) :: lang                  ! language on $IDENT command
character(len=:),allocatable :: text
integer,save                 :: ident_count=1
integer                      :: i

   call dissect2('ident',' --language fortran',opts)  ! parse options and inline comment on input line

   text=''
   do i=1,size(unnamed)
      text=text//' '//trim(unnamed(i))
   enddo

   lang=sget('language')

   select case(lang)
   case('fortran')    !x! should make look for characters not allowed in metadata, continue over multiple lines, ...
      select case(len(text))
      case(:89)
         if(G_ident)then
            write(G_iout,'("character(len=*),parameter::ident_",i0,"=""@(#)",a,''"'')')ident_count,text
         else
            write(G_iout,'("! ident_",i0,"=""@(#)",a,''"'')')ident_count,text
         endif
         ident_count=ident_count+1
      case(90:126)
         if(G_ident)then
            write(G_iout,'("character(len=*),parameter::ident_",i0,"=""&")')ident_count
            write(G_iout,'(''&@(#)'',a,''"'')')text
         else
            write(G_iout,'("! ident_",i0,"=""@(#)",a,''"'')')ident_count,text
         endif
         ident_count=ident_count+1
      case default
         call stop_prep('d5cf2021-81f9-4175-813f-e18e89616358','description too long:',trim(G_SOURCE))
      end select
   case('c')
         write(G_iout,'(a)')'#ident "@(#)'//text//'"'
   case default
         call stop_prep('d46f723c-3188-43cd-93ad-45f81eeecdc5','language unknown for',' $IDENT'//trim(G_SOURCE))
   end select

end subroutine ident
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function getdate(name) result(s)      !@(#) getdate(3f): Function to write date and time into returned string in different styles
character(len=*),intent(in),optional :: name

character(len=*),parameter           :: month='JanFebMarAprMayJunJulAugSepOctNovDec'
character(len=*),parameter           :: fmt = '(I2.2,A1,I2.2,I3,1X,A3,1x,I4)'
character(len=*),parameter           :: cdate = '(A3,1X,I2.2,1X,I4.4)'
character(len=:),allocatable         :: s
character(len=80)                    :: line
integer                              :: v(8)
character(len=10)                    :: name_

   call date_and_time(values=v)
   name_='prep'
   if(present(name))name_=name
   select case(lower(name_))
   case('prep') ; write(line,fmt) v(5), ':', v(6), v(3), month(3*v(2)-2:3*v(2)), v(1) ! PREP_DATE="00:39  5 Nov 2013"
   case('date') ; write(line,'(i4.4,"-",i2.2,"-",i2.2)') v(1),v(2),v(3)
   case('cdate'); write(line,cdate) month(3*v(2)-2:3*v(2)), v(3), v(1)
   case('long') ; write(line,'(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2," UTC",sp,i0)') v(1),v(2),v(3),v(5),v(6),v(7),v(4)
   case('time') ; write(line,'(i2.2,":",i2.2,":",i2.2)') v(5),v(6),v(7)
   case default ; write(line,'(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2," UTC",sp,i0)') v(1),v(2),v(3),v(5),v(6),v(7),v(4)
   end select
   s=trim(line)
end function getdate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine check_name(line)
! determine if a string is a valid Fortran name ignoring trailing spaces (but not leading spaces)
character(len=*),parameter   :: dig='0123456789'
character(len=*),parameter   :: lower='abcdefghijklmnopqrstuvwxyz'
character(len=*),parameter   :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
character(len=*),parameter   :: allowed=upper//lower//dig//'_'
character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
logical                      :: lout
   name=trim(line)
   if(len(name) /= 0)then
      lout = .true.                         &
      & .and. verify(name,allowed) == 0     &
      & .and. len(name) <= 63
   else
      call stop_prep('ae171d54-23f2-42a2-bf29-7a3f5ed692ab',"null variable name:",trim(G_source))
      lout = .false.
   endif
   if(.not.lout)then
     call stop_prep('973c0777-0e6f-4bbd-aefd-0406e3ebc1ae',&
             & 'name contains unallowed character(or general syntax error):',trim(G_source))
   endif
end subroutine check_name
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine unset(opts)                                     !@(#)unset(3f): process UNSET directive
character(len=*)             :: opts                       ! directive with no spaces, leading prefix removed, and all uppercase
character(len=:),allocatable :: names(:)
integer                      :: i
integer                      :: k
integer                      :: ibug

   ! REMOVE VARIABLE IF FOUND IN VARIABLE NAME DICTIONARY
   ! allow basic globbing where * is any string and ? is any character
   if (len_trim(opts) == 0) then                           ! if no variable name
      call stop_prep('544ca822-3d1f-4369-afa1-a73d2beee896','missing targets for ',' $UNSET:'//trim(G_source))
   endif
   call split(opts,names,delimiters=' ;,')

   do k=1,size(names)
      if(G_verbose)then
         call write_err('+ $UNSET '//names(k))
      endif
      ! added UBOUND call because GFORTRAN returns size of 1 when undefined, OK with ifort and nvfortran
      ibug=minval([size(macro%key),ubound(macro%key)])   ! print variable dictionary
      do i=ibug,1,-1                           ! find defined variable to be undefined by searching dictionary
         if (glob(trim(macro%key(i)),trim(names(k))))then   ! found the requested variable name
            call  macro%del(macro%key(i))
         endif
      enddo
   enddo
end subroutine unset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine if(opts,noelse,eb)                                 !@(#)if(3f): process IF and ELSEIF directives
character(len=*),intent(in)  :: opts
integer,intent(out)          :: noelse
logical                      :: eb
character(len=G_var_len)     :: value
integer                      :: ios
integer                      :: ierr
integer                      :: ithen
character(len=G_line_length) :: expression

   noelse=0
   G_write=.false.

   G_nestl=G_nestl+1                                          ! increment IF nest level
   if (G_nestl > G_nestl_max) then
      call stop_prep('fb3d7c78-1c5c-4469-8e52-68da257f1fa9',&
              & '"$IF" block nesting too deep, limited to '//v2s(G_nestl_max)//' levels,',trim(G_source))
   endif

   expression=opts
   ithen=len_trim(opts)                                       ! trim off ")THEN"
   if(ithen > 5)then
      if(expression(ithen-4:ithen) == ')THEN'.and.expression(1:1) == '(')then
         expression=expression(2:ithen-5)
      endif
   endif

   if(G_debug.and.G_verbose) write(stderr,*)'*if* TOP:EXPRESSION:'//trim(expression)

   value=''
   call expr(expression,value,ierr,logical=.true.)
   if(ierr == 0)then
      read(value,'(l7)',iostat=ios)G_dc
   else
      G_dc=.false.
      call stop_prep('eb5f9a4f-28b5-4740-a331-03fb7510fab8','"$IF" expression invalid:',trim(G_source))
   endif

   if (.not.G_dc.or..not.G_condop(G_nestl-1).or.eb)then
      if(G_debug.and.G_verbose) write(stderr,*)'*if* PREVIOUS:'
      return                                               ! check to make sure previous IF was true
   endif
   G_condop(G_nestl)=.true.
   G_write=G_condop(G_nestl)
   if(G_debug.and.G_verbose) write(stderr,*)'*if* BOT:'

end subroutine if
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine def(verb,opts,noelse,eb)                  !@(#)def(3f): process IFDEF and IFNDEF directives
character(len=*),intent(in)  :: verb
character(len=*),intent(in)  :: opts
integer,intent(out)          :: noelse
logical                      :: eb
character(len=G_var_len)     :: name
character(len=G_var_len)     :: value
character(len=:),allocatable :: varvalue

   noelse=0
   G_write=.false.
   G_nestl=G_nestl+1                                 ! increment IF nest level
   if (G_nestl > G_nestl_max) then
      call stop_prep('cf800a61-7287-453d-a6c1-dbd8878d4da0',&
              & 'block nesting too deep, limited to '//v2s(G_nestl_max)//' levels in:',' $IF'//trim(G_source))
   endif
   call check_name(opts)                             ! check that opts contains only a legitimate variable name
   value=opts                                        ! set VALUE to variable name
   G_dc=.true.                                       ! initialize

   name=table%get(value)
   if (name == '') then                              ! if failed to find variable name
      G_dc=.false.
   endif
   if((.not.G_noenv).and.(.not.G_dc))then            ! if not found in variable dictionary check environment variables if allowed
      varvalue=get_env(value)
      if(len_trim(varvalue) /= 0)then
         G_dc=.true.
      endif
   endif
   if(verb == 'IFNDEF')then
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
subroutine else(verb,opts,noelse,eb)                       !@(#)else(3f): process else and elseif
character(len=*)             :: verb
character(len=*)             :: opts
integer                      :: noelse
logical                      :: eb
character(len=G_line_length) :: expression
integer                      :: ithen

   expression=opts
   ithen=len_trim(opts)  ! trim off ")THEN"
   if(ithen > 5)then
      if(expression(ithen-4:ithen) == ')THEN'.and.expression(1:1) == '(')then
         expression=expression(2:ithen-5)
      endif
   endif

   if(noelse == 1.or.G_nestl == 0) then                    ! test for else instead of elseif
      call stop_prep('12f6abb7-18f6-4d7e-b204-637cf24495b3','misplaced $ELSE or $ELSEIF directive:',trim(G_SOURCE))
      return
   endif
   if(verb == 'ELSE')then
      noelse=1
   endif
   if(.not.G_condop(G_nestl-1))return                      ! if was true so ignore else
   eb=.false.
   if(G_condop(G_nestl)) then
       eb=.true.
       G_write=.false.
   elseif(len_trim(expression) /= 0)then                   ! elseif detected
     G_nestl=G_nestl-1                                     ! decrease if level because it will be incremented in subroutine if
     call if(expression,noelse,eb)
   else                                                    ! else detected
     G_condop(G_nestl)=.true.
     G_write=.true.
   endif

end subroutine else
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine endif(noelse,eb)                             !@(#)endif(3f): process ENDIF directive
integer,intent(out)          :: noelse
logical,intent(out)          :: eb

   ! if no ELSE or ELSEIF present insert ELSE to simplify logic
   if(noelse == 0)then
      call else('ELSE',' ',noelse,eb)
   endif

   G_nestl=G_nestl-1                                           ! decrease if level

   if(G_nestl < 0)then
      call stop_prep('b02d372b-a722-4385-95fe-a218a628578f',"misplaced $ENDIF directive:",trim(G_source))
   endif

   noelse=0                                                    ! reset else level
   eb=.not.G_condop(G_nestl+1)
   G_write=.not.eb
   G_condop(G_nestl+1)=.false.

   if(G_nestl == 0)then
      G_write=.true.
      eb=.false.
   endif

end subroutine endif
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
logical function true_or_false(line,ipos1,ipos2) !@(#)true_or_false(3f): convert variable name or .TRUE./.FALSE. to a logical value
character(len=G_line_length),intent(in) :: line              ! line containing string to interpret as a logical value
integer,intent(in)                      :: ipos1             ! starting column of substring in LINE
integer,intent(in)                      :: ipos2             ! ending column of substring in LINE

character(len=G_var_len)                :: value
character(len=G_var_len)                :: substring
integer                                 :: ios               ! error code returned by an internal READ

   true_or_false=.false.                                     ! initialize return value
   substring=line(ipos1:ipos2)                               ! extract substring from LINE to interpret

   select case (substring)                                   ! if string is not a logical string assume it is a variable name
   case ('.FALSE.','.F.')
      true_or_false=.false.                                  ! set appropriate return value
   case ('.TRUE.','.T.')
      true_or_false=.true.                                   ! set appropriate return value
   case default                                              ! assume this is a variable name, find name in dictionary
      value=table%get(substring)

      if (value == '') then                                  ! if not a defined variable name stop program
         call stop_prep('c759de9e-33a9-41d7-a959-a5ff30e0f0f2',&
                 & 'undefined variable.',' DIRECTIVE='//trim(G_source)//' VARIABLE='//trim(substring))
      else
         read(value,'(l4)',iostat=ios) true_or_false         ! try to read a logical from the value for the variable name
         if(ios /= 0)then                                    ! not successful in reading string as a logical value
            call stop_prep('b56bc1be-7600-4bd5-9fe5-8196b0d9bd7e','constant logical expression required.',trim(G_source))
         endif
      endif

   end select

end function true_or_false
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine document(opts)                    !@(#)document(3f): process BLOCK command to start or stop special processing
character(len=*),intent(in)  :: opts
integer                      :: ierr
integer                      :: ios
character(len=G_line_length) :: options                 ! everything after first word of command till end of line or !
character(len=:),allocatable :: name

! CHECK COMMAND SYNTAX
   if(G_outtype == 'help')then  ! if in 'help' mode wrap up the routine
      write(G_iout,'(a)')"'']"
      write(G_iout,'(a)')"   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))"
      write(G_iout,'(a)')"   stop ! if --help was specified, stop"
      write(G_iout,'(a)')"endif"
      write(G_iout,'(a)')"end subroutine help_usage"
      !x!write(G_iout,'("!",a)')repeat('-',131)
   elseif(G_outtype == 'variable')then     ! if in 'variable' mode wrap up the variable
      write(G_iout,'(a)')"'']"
   elseif(G_outtype == 'system')then
         close(unit=G_scratch_lun,iostat=ios)
         call execute_command_line( trim(G_cmd)//' < '//G_scratch_file//' > '//G_scratch_file//'.out')
         ierr=filedelete(G_scratch_file)
         options=G_scratch_file//'.out'
         call include(options,50+G_iocount)    ! Filenames can be case sensitive
   elseif(G_outtype == 'version')then  ! if in 'version' mode wrap up the routine
      write(G_iout,'("''@(#)COMPILED:       ",a,"'',&")') getdate('long')//'>'
      write(G_iout,'(a)')"'']"
      write(G_iout,'(a)')"   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))"
      !x!write(G_iout,'(a)')'   write(*,*)"COMPILER VERSION=",COMPILER_VERSION()'
      !x!write(G_iout,'(a)')'   write(*,*)"COMPILER OPTIONS=",COMPILER_OPTIONS()'
      write(G_iout,'(a)')"   stop ! if --version was specified, stop"
      write(G_iout,'(a)')"endif"
      write(G_iout,'(a)')"end subroutine help_version"
      !x!write(G_iout,'("!",a)')repeat('-',131)
   endif

   ! parse options on input line
   call dissect2('block','--file --cmd sh --varname textblock --length 128 --width 0 --style "#N#" --append F',opts)
   ! if a previous command has opened a --file FILENAME flush it, because a new one is being opened or this is an END command
   ! and if a --file FILENAME has been selected open it
   call print_comment_block()

   ! now can start new section
   G_MAN=''
   if(sget('file') /= '')then
      G_MAN_FILE=sget('file')
      G_MAN_COLLECT=.true.
   else
      G_MAN_FILE=''
      G_MAN_COLLECT=.false.
   endif

   G_MAN_PRINT=.false.

   if(LGET('append'))then
      G_MAN_FILE_POSITION='APPEND'
   else
      G_MAN_FILE_POSITION='ASIS'
   endif

   if(size(unnamed) > 0.and.opts /= '')then
      name=upper(unnamed(1))
   else
      name=' '
   endif

   select case(name)

   case('COMMENT')
      G_outtype='comment'
      G_MAN_PRINT=.true.
      G_MAN_COLLECT=.true.
      if(sget('style') /= '#N#')then
         G_comment_style=lower(sget('style'))             ! allow formatting comments for particular post-processors
      endif
   case('NULL')
      G_outtype='null'

   case('SET','REPLACE')
      G_outtype='set'
      G_MAN_PRINT=.false.
      G_MAN_COLLECT=.false.
   case('DEFINE')
      G_outtype='define'
      G_MAN_PRINT=.false.
      G_MAN_COLLECT=.false.
   case('REDEFINE')
      G_outtype='redefine'
      G_MAN_PRINT=.false.
      G_MAN_COLLECT=.false.
   case('MESSAGE')
      G_outtype='message'
      G_MAN_PRINT=.false.
      G_MAN_COLLECT=.false.

   case('SHELL','SYSTEM')
      G_outtype='system'
      G_MAN_PRINT=.false.
      G_MAN_COLLECT=.false.
      if(G_system_on)then                             ! if allowing commands to be executed
         flush(unit=G_iout,iostat=ios)
         !!G_scratch_file=scratch('prep_scratch.'))
         G_scratch_file=trim(uniq(get_tmp()//'prep_scratch.'))  !! THIS HAS TO BE A UNIQUE NAME -- IMPROVE THIS
         G_scratch_lun=fileopen(G_scratch_file,'rw',ierr)
         if(ierr < 0)then
            call stop_prep('dec944d1-ff1f-4ff3-9512-a5801fc5e43b','filter command failed to open process:',trim(G_SOURCE))
         endif
      else
         call stop_prep('b664d76c-4887-4f49-b874-90790ec62746',&
                 & 'filter command $BLOCK encountered but system commands not enabled:',trim(G_SOURCE))
      endif

   case('VARIABLE')
      G_outtype='variable'
      G_varname_length=iget('length')
      G_varname_width=iget('width')
      write(G_iout,'(a,i0,a)')trim(sget('varname'))//'=[ CHARACTER(LEN=',G_varname_length,') :: &'
      G_MAN_PRINT=.false.

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
   case(' ','END')
      G_outtype='asis'
      G_MAN_COLLECT=.false.
   case('ASIS')
      G_outtype='asis'
   case default
      if(size(unnamed) > 0)then
         call stop_prep('0c5a4a0e-ac8d-4ef4-b160-de2d344398a6',&
                 & 'unexpected "BLOCK" option. found:"',trim(unnamed(1))//'" in '//trim(G_source) )
      else
         call stop_prep('fde8fa32-3c90-4cae-8386-cb968877973e','unexpected "BLOCK" option. found:"',' " in '//trim(G_source) )
      endif
   end select

   G_comment_count=0
end subroutine document
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine print_comment_block() !@(#)print_comment_block(3f): format comment block to file in document directory and output
character(len=:),allocatable :: filename
character(len=:),allocatable :: varvalue
integer                      :: ios,iend,lun

   if(.not.allocated(G_MAN))then
      return
   endif

   varvalue=get_env('PREP_DOCUMENT_DIR')

   if(varvalue /= ''.and.G_MAN /= ''.and.G_MAN_FILE /= ' ')then ! if $BLOCK --file FILE is present generate file in directory/doc

      iend=len_trim(varvalue)
      if(varvalue(iend:iend) /= '/')then
         filename=trim(varvalue)//'/doc/'//trim(G_MAN_FILE)
      else
         filename=trim(varvalue)//'doc/'//trim(G_MAN_FILE)
      endif

      open(newunit=lun,file=filename,iostat=ios,action='write',position=G_MAN_FILE_POSITION)

      if(ios /= 0)then
         call stop_prep('3f6bb821-f10b-4be0-aa4a-46ba6b2a1e48','failed to open document output file:',trim(filename))
      else
         if(len(G_MAN) > 1)then                   ! the way the string is built it starts with a newline
            write(lun,'(a)',iostat=ios) G_MAN(2:)
         else
            write(lun,'(a)',iostat=ios) G_MAN
         endif
         if(ios /= 0)then
            call write_err('G_MAN='//G_MAN)
            call stop_prep('e9e2c7a9-f3b6-4625-a79c-bb42c5dc798b','failed to write output file:',trim(filename))
         endif
      endif

      close(unit=lun,iostat=ios)

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
character(len=:),allocatable :: array1(:)   ! output array of tokens
character(len=:),allocatable :: array(:)    ! output array of tokens
integer                      :: ios
integer                      :: i

   ALL: block
      WRITEIT: block

         select case(G_comment_style)

         case('doxygen')                 ! convert plain text to doxygen comment blocks with some automatic markdown highlights
            if(len(G_MAN) > 1)then      ! the way the string is built it starts with a newline

               CALL split(G_MAN,array1,delimiters=new_line('N'),nulls='return') ! parse string to an array parsing on delimiters
               if(allocated(array))deallocate(array)
               allocate(character(len=len(array1)+6) :: array(size(array1)))  ! make room for !! and ##
               array(:)=array1
               deallocate(array1)

               do i=1,size(array)        ! lines starting with a letter and all uppercase letters is prefixed with "##"
                  if( upper(array(i)) == array(i) .and. isalpha(array(i)(1:1)).and.lower(array(i)) /= array(i))then
                     array(i)='##'//trim(array(i))
                     select case(array(i))
                     case('##SYNOPSIS','##EXAMPLES','##EXAMPLE')
                        array(i)=trim(array(i))//new_line('N')//'!'//'!'
                     endselect
                  else
                     array(i)=' '//trim(array(i))
                  endif
               enddo

               if(size(array) > 0)then
                  write(G_iout,'("!",">",a)')trim(array(1))
               endif

               do i=2,size(array)
                  write(G_iout,'("!","!",a)',iostat=ios)trim(array(i))
                  if(ios /= 0)exit WRITEIT
               enddo

            endif
            !x!write(G_iout,'("!",131("="))')

         case('ford')                    ! convert plain text to doxygen comment blocks with some automatic markdown highlights
            if(len(G_MAN) > 1)then      ! the way the string is built it starts with a newline
               CALL split(G_MAN,array1,delimiters=new_line('N'),nulls='return') ! parse string to an array parsing on delimiters
               !======================================================================================== nvfortran bug
               ! array=[character(len=(len(array1)+6)) :: array1] !! pad with trailing spaces

               if(allocated(array))deallocate(array)
               allocate(character(len=len(array1)+6) :: array(size(array1)))  ! make room for !! and ##
               array(:)=array1
               !========================================================================================
               deallocate(array1)

               do i=1,size(array)        ! lines starting with a letter and all uppercase letters is prefixed with "##"
                  if( upper(array(i)) == array(i) .and. isalpha(array(i)(1:1)).and.lower(array(i)) /= array(i))then
                     array(i)='## '//trim(array(i))
                     select case(array(i))
                     case('## SYNOPSIS','## EXAMPLES','## EXAMPLE')
                        array(i)=trim(array(i))//new_line('N')//'!>'
                     endselect
                  else
                     array(i)=' '//trim(array(i))
                  endif
               enddo

               if(size(array) > 0)then
                  write(G_iout,'("!>",a)')trim(array(1))
               endif

               do i=2,size(array)
                  write(G_iout,'("!>",a)',iostat=ios)trim(array(i))
                  if(ios /= 0)exit WRITEIT
               enddo

            endif
            !x!write(G_iout,'("!>",131("="))')

         case('none')                    ! ignore comment block

         case default
            if(len(G_MAN) > 1)then                       ! the way the string is built it starts with a newline
               G_MAN=G_MAN(2:)//repeat(' ',2*len(G_MAN))  ! make sure the white-space exists
               call substitute(G_MAN,NEW_LINE('A'),NEW_LINE('A')//'! ')
               G_MAN='! '//trim(G_MAN)
            endif
            write(G_iout,'(a)',iostat=ios) G_MAN
            if(ios /= 0)exit WRITEIT
            !x!write(G_iout,'("!",131("="))')
         end select

         exit ALL
      endblock WRITEIT
      call write_err('G_MAN='//G_MAN)
      call stop_prep('0a63628d-cd62-4b10-a73b-c9d5f0ed8674','failed to write comment block','')
   endblock ALL
end subroutine format_g_man
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine show_state(list,msg)                        !@(#)debug(3f): process $SHOW command or state output when errors occur
character(len=*),intent(in),optional :: list
character(len=*),intent(in) :: msg
integer                     :: i
integer                     :: j
character(len=:),allocatable   :: array(:)    ! output array of tokens
character(len=*),parameter  :: fmt='(*(g0,1x))'
integer                     :: ibugm
integer                     :: ibugt
   if(present(list))then
      if(list /= '')then
         ! print variables:
         CALL split(list,array,delimiters=' ;,') ! parse string to an array parsing on delimiters
         ibugm=minval([size(macro%key),ubound(macro%key)])
         ibugt=minval([size(table%key),ubound(table%key)])
         do j=1,size(array)
            do i=1,ibugm                     ! size(macro%key) bug in gfortran
               if(glob(trim(macro%key(i)),trim(array(j))))then ! write variable and corresponding value
                  write(G_iout,fmt)"! MACRO: ",trim(macro%key(i)),' = ',adjustl(macro%value(i)(:macro%count(i)))
               endif
            enddo
            do i=1,ibugt                     ! size(table%key) bug in gfortran
               if(glob(trim(table%key(i)),trim(array(j))))then ! write variable and corresponding value
                  write(G_iout,fmt)"! VARIABLE: ",trim(table%key(i)),' = ',adjustl(table%value(i)(:table%count(i)))
               endif
            enddo
         enddo
         return
      endif
   endif
   write(G_iout,'(a)')'!==============================================================================='
   write(G_iout,'(a)')'! '//trim(msg)

   write(G_iout,'(a)')'! Current state of prep(1):('//getdate()//')'
   write(G_iout,'("! Total lines read ............... ",i0)')G_io_total_lines     ! write number of lines read
   write(G_iout,'("! Conditional nesting level....... ",i0)')G_nestl              ! write nesting level
   write(G_iout,'("! G_WRITE (general processing).... ",l1)')G_write              ! non-if/else/endif directives processed
   write(G_iout,'("! G_LLWRITE (write input lines)... ",l1)')G_llwrite            ! non-if/else/endif directives processed

   call write_arguments()

   write(G_iout,'(a)')'! Open files:'
   write(G_iout,'(a)')'!    unit ! line number ! filename'
   do i=1,G_iocount                                                               ! print file dictionary
      ! write table of files
      write(G_iout,'("!    ",i4," ! ",i11," ! ",a)') &
      &  G_file_dictionary(i)%unit_number,    &
      &  G_file_dictionary(i)%line_number,    &
      &  trim(G_file_dictionary(i)%filename )
   enddo

   write(G_iout,'(a)')'! INCLUDE directories:'
   do i=1,G_inc_count
      write(G_iout,'("!    ",a)') trim(G_inc_files(i))
   enddo

   ibugt=minval([size(table%key),ubound(table%key)])   ! print variable dictionary
   if(ibugt > 0)then
      write(G_iout,fmt)'! Variables:(There are',ibugt,'variables defined)'
      do i=1,ibugt                    ! size(table%key) bug in gfortran
         write(G_iout,fmt)"!    $DEFINE",trim(table%key(i)),' = ',adjustl(table%value(i)(:table%count(i)) )
      enddo
   endif

   if(G_parcelcount > 0)write(G_iout,'(a)')'! Parcels:'
   do i=1,G_parcelcount
      write(G_iout,fmt) '!   ',trim(G_parcel_dictionary(i)%name)
   enddo

   ibugm=minval([size(macro%key),ubound(macro%key)])   ! print variable dictionary
   if(ibugm > 0)then ! size(macro%key) > 0)then
      write(G_iout,fmt)'! Macros:(There are',ibugm,'keywords defined)'
      do i=1,ibugm                    ! size(table%key) bug in gfortran
         write(G_iout,fmt)"! $SET   ",trim(macro%key(i)),' = ',adjustl(macro%value(i)(:macro%count(i)) )
      enddo
   endif

   write(G_iout,'(a)')'!-------------------------------------------------------------------------------'
end subroutine show_state
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_arguments() !@(#)write_arguments(3f): return all command arguments as a string

integer                      :: istatus          !  status (non-zero means error)
integer                      :: ilength          !  length of individual arguments
integer                      :: i                !  loop count
integer                      :: icount           !  count of number of arguments available
character(len=255)           :: value            !  store individual arguments one at a time

   write(G_iout,'(a)',advance='no')'! Arguments ...................... '
   icount=command_argument_count()               ! intrinsic gets number of arguments
   do i=1,icount
      call get_command_argument(i,value,ilength,istatus)
      write(G_iout,'(a,1x)',advance='no')value(:ilength)
   enddo
   write(G_iout,'(a)')
end subroutine write_arguments
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine include_cmd(opts,iunit)                         !@(#)include_cmd(3f): process $INCLUDE directive
character(len=*),intent(in)  :: opts
integer,intent(in)           :: iunit
character(len=G_line_length) :: filename                   ! filename on $INCLUDE command
character(len=20)            :: position
character(len=:),allocatable :: varname
integer                      :: ios
   call dissect2('include','--base64 F --varname " "',opts)              ! parse options and inline comment on input line
   varname=sget('varname')

   if(size(unnamed) > 0.and.opts /= '')then
      filename=unnamed(1)
   else
      filename=' '
   endif
   if(lget('base64'))then
      block
      character(len=1),allocatable :: text(:)
      integer :: length, lines, width, i, iend
      filename=adjustl(unquote(trim(filename)))            ! remove " from filename using Fortran list-directed I/O rules
      call findit(filename)
      call filebyte(filename,text,length,lines)            ! allocate character array and copy file into it
      if(.not.allocated(text))then
         call stop_prep('77d265df-b299-4ab7-5649-b3f4ffda569e','failed to load file for base64 encoding:',filename)
      else
         text=encode_base64(text,width=0)
         if(varname.ne.'')then
            write(G_iout,'(*(g0))')varname,'= &'
         endif
         iend=size(text)
         do i=1,iend,76
            write(G_iout,'(*(g0))')"'",text(i:min(i+75,iend)),"' // &"
         enddo
         write(G_iout,'(*(a))')"''"
      endif
      endblock
   else
      call include(filename,iunit)
   endif

end subroutine include_cmd
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine include(line,iunit)  !@(#)include(3f): add file to input file list
implicit none
character(len=G_line_length),intent(in)  :: line
integer,intent(in)                       :: iunit
integer                                  :: ios
character(len=4096)                      :: message
character(len=G_line_length)             :: line_unquoted
integer                                  :: iend

   line_unquoted=adjustl(unquote(line))                   ! remove " from filename using Fortran list-directed I/O rules
   iend=max(len_trim(line_unquoted),1)
   if(len(line_unquoted) >= 2)then
      if(line_unquoted(1:1) == '<'.and.line_unquoted(iend:iend) == '>')then       ! remove < and > from filename
         line_unquoted=line_unquoted(2:iend-1)
      endif
   endif

   if(iunit == 5.or.line_unquoted == '@')then                   ! assume this is stdin
      G_iocount=G_iocount+1
      G_file_dictionary(G_iocount)%unit_number=5
      G_file_dictionary(G_iocount)%filename=line_unquoted
      return
   endif

   call findit(line_unquoted)

   open(unit=iunit,file=trim(line_unquoted),iostat=ios,status='old',action='read',iomsg=message)
   if(ios /= 0)then
      call show_state(msg='OPEN IN INCLUDE')
      call write_err(message)
      call stop_prep('952044a1-c02f-4ede-bb27-9d3b9f7362b6','failed open of input file(',v2s(iunit)//"):"//trim(line_unquoted))
   else
      rewind(unit=iunit)
      G_iocount=G_iocount+1
      if(G_iocount > size(G_file_dictionary))then
         call stop_prep('f27e4791-c486-4f1c-9182-851beb3bca99','input file nesting too deep:',trim(G_source))
      endif
      G_file_dictionary(G_iocount)%unit_number=iunit
      G_file_dictionary(G_iocount)%filename=line_unquoted
      G_file_dictionary(G_iocount)%line_number=0
   endif

end subroutine include
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine findit(line) !@(#)findit(3f): look for filename in search directories if name does not exist and return modified name
character(len=G_line_length) :: line
character(len=G_line_length) :: filename
logical                      :: file_exist
integer                      :: i
integer                      :: iend_dir

   inquire(file=trim(line), exist=file_exist)                    ! test if input filename exists
   if(file_exist)then                                            ! if file exits then return filename
      return
   endif

   if(G_inc_count > 0)then                                      ! if search directories have been specified search for file
      do i=1,G_inc_count
         iend_dir=len_trim(G_inc_files(i))
         if(G_inc_files(i)(iend_dir:iend_dir) /= '/')then
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

   call stop_prep('2c97a594-8cc7-4df1-a3d9-04c766c78ada','missing input file:',trim(filename))

end subroutine findit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine opens()                   !@(#)opens(3f): use expression on command line to  open input files

integer,parameter                     :: n=50                       ! maximum number of tokens to look for
character(len=G_line_length)          :: array(n)                   ! the array to fill with tokens
character(len=1)                      :: dlim=' '                   ! string of single characters to use as delimiters

integer                               :: icount                     ! how many tokens are found
integer                               :: ibegin(n)                  ! starting column numbers for the tokens in INLINE
integer                               :: iterm(n)                   ! ending column numbers for the tokens in INLINE
integer                               :: ilength                    ! is the position of last non‐blank character in INLINE
character(len=G_line_length)          :: in_filename2=''            ! input filename, default is stdin
integer                               :: i, ii
integer                               :: ivalue
character(len=G_line_length)          :: dir                        ! directory used by an input file

   if(.not.G_cpp)then
      in_filename2(:G_line_length)  = sget('i')                     ! get values from command line
      if(in_filename2 == '')then                                    ! read stdin if no -i on command line
         in_filename2  = '@'
      endif
   else
      if(size(unnamed) > 0)then
         in_filename2  = unnamed(1)
      else
         in_filename2  = '@'
      endif
   endif

   ! break command argument "i" into single words
   call delim(adjustl(trim(in_filename2)),array,n,icount,ibegin,iterm,ilength,dlim)
   ivalue=50                                ! starting file unit to use
   do i=icount,1,-1
      G_source='$include '//trim(array(i))  ! for messages
      call include(array(i),ivalue)
      ivalue=ivalue+1

      ALREADY: block                 ! store directory path of input files as an implicit directory for reading $INCLUDE files
         dir=dirname(array(i))
         do ii=1,G_inc_count
            if(G_inc_files(ii) == dir)exit ALREADY
         enddo
         G_inc_count=G_inc_count+1
         G_inc_count=min(G_inc_count,size(G_inc_files)) ! guard against too many files; !x! should warn on overflow
         G_inc_files(G_inc_count)=dir
      endblock ALREADY

   enddo

! >>>
!   If ARRAY(N) fills before reaching the end of the line the routine stops.
!   Check "if(iend(icount)  ==  ilength)" to see if you got to the end to warn if not all files include

end subroutine opens
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine includes()         !@(#)includes(3f): use expression on command line to  get include directories

integer,parameter                     :: n=50                    ! maximum number of tokens to look for
character(len=1)                      :: dlim=' '                ! string of single characters to use as delimiters
integer                               :: ibegin(n)               ! starting column numbers for the tokens in G_inc_files
integer                               :: iterm(n)                ! ending column numbers for the tokens in G_inc_files
integer                               :: ilength                 ! is the position of last non‐blank character in G_inc_files

   ! G_inc_files is the array to fill with tokens
   ! G_inc_count is the number of tokens found

   ! break command argument "I" into single words
   call delim(adjustl(trim(sget('I'))),G_inc_files,n,G_inc_count,ibegin,iterm,ilength,dlim)
end subroutine includes
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine defines()       !@(#)defines(3f): use expressions on command line to initialize dictionary and define variables
integer,parameter                     :: n=300                   ! maximum number of tokens to look for
character(len=G_line_length)          :: array(n)                ! the array to fill with tokens
character(len=1)                      :: dlim=' '                ! string of single characters to use as delimiters

integer                               :: icount                  ! how many tokens are found
integer                               :: ibegin(n)               ! starting column numbers for the tokens in INLINE
integer                               :: iterm(n)                ! ending column numbers for the tokens in INLINE
integer                               :: ilength                 ! is the position of last non‐blank character in INLINE
character(len=:),allocatable          :: in_define2              ! variable definition from command line
integer                               :: i

   in_define2=''

   if(.not.G_cpp)then
      do i=1,size(unnamed)
         in_define2=in_define2//' '//unnamed(i)
      enddo
   endif

   ! break command argument prep_oo into single words
   call delim(adjustl(trim(in_define2))//' '//trim(sget('D')),array,n,icount,ibegin,iterm,ilength,dlim)
   do i=1,icount
      G_source='$redefine '//trim(array(i))
      call cond() ! convert variable name into a "$define variablename" directive and process it
   enddo

!   If ARRAY(N) fills before reaching the end of the line the routine stops.
!   Check "if(iend(icount)  ==  ilength)" to see if you got to the end.

end subroutine defines
!===================================================================================================================================
subroutine undef(opts)                                     !@(#)undef(3f): process UNDEFINE directive
character(len=*)             :: opts                       ! directive with no spaces, leading prefix removed, and all uppercase
character(len=:),allocatable :: names(:)
integer                      :: i
integer                      :: k

   ! REMOVE VARIABLE IF FOUND IN VARIABLE NAME DICTIONARY
   ! allow basic globbing where * is any string and ? is any character
   if (len_trim(opts) == 0) then                           ! if no variable name
      call stop_prep('81adcf82-0fcf-4c1a-a445-2999f95107fa','missing targets in',' $UNDEFINE:'//trim(G_source))
   endif
   call split(opts,names,delimiters=' ;,')

   do k=1,size(names)
      if(G_verbose)then
         call write_err('+ $UNDEFINE '//names(k))
      endif
      do i=size(table%key),1,-1                           ! find defined variable to be undefined by searching dictionary
         if (glob(trim(table%key(i)),trim(names(k))))then   ! found the requested variable name
            call  table%del(table%key(i))
         endif
      enddo
   enddo

end subroutine undef
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stop(opts)                    !@(#)stop(3f): process stop directive
character(len=*),intent(in)  :: opts
integer                      :: ivalue
character(len=:),allocatable :: message
integer                      :: iend

! CHECK COMMAND SYNTAX
   if(opts == '')then
      call stop_prep('00000000-0000-0000-0000-000000000000','','',stop_value=1)
   else
      iend=index(opts,' ')
      if(iend == 0)then
         iend=len_trim(opts)
         message=' '
      else
         message=unquote(trim(opts(iend:)))
         write(stderr,'(a)')message
         call flushit()
      endif

      ivalue=get_integer_from_string(opts(:iend))

      if(ivalue == 0)then
         if(.not.G_debug)stop
      elseif(message == '')then
         call stop_prep('00000000-0000-0000-0000-000000000000','','',stop_value=ivalue) ! UNEXPECTED "STOP" VALUE
      else
         if(.not.G_debug)stop ivalue
      endif

   endif
end subroutine stop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stop_prep(key,translate,message,stop_value) !@(#)stop_prep(3f): write MESSAGE to stderr and exit program
character(len=*),intent(in)  :: key
character(len=*),intent(in)  :: translate
character(len=*),intent(in)  :: message
character(len=1024)          :: toscreen
character(len=:),allocatable :: translated
integer,optional :: stop_value
integer :: stop_value_local
   stop_value_local=1
   if( present(stop_value) )stop_value_local=stop_value
   call write_err(trim(G_SOURCE))
   select case(G_lang)
   case('en')
    translated=en(key,translate)
   case default
    translated=trim(translate)
   end select
   if(G_debug)then
      write(toscreen,'("(",a,")<ERROR> <G> ",a,1x,a," </G>")')key,translated,message
   else
      write(toscreen,'("<ERROR> <G> ",a,1x,a," </G>")')translated,message
   endif


   toscreen=attr(toscreen)
   call show_state(msg=trim(toscreen))
   if(.not.G_debug)stop stop_value_local

end subroutine stop_prep
!===================================================================================================================================
! skeleton for supporting alternate languages
function en(key,translate) result(english)
!
! the key can be any string. The string itself, a uuid, the calling function name and a number, ...
!  o  a uuid can be used to location an exact line in a source code
!  o  the string itself allows easily for translating duplicate strings to another language
!
character(len=*),intent(in)  :: key
character(len=*),intent(in)  :: translate
character(len=:),allocatable :: english
select case(key)
case('00000000-0000-0000-0000-000000000000');english=''
case('9065fd51-1c20-48a6-ac81-94235ada841c');english='expression invalid:'
case('03f9f928-2781-468a-bb0c-f341457e4aa4');english='unknown compiler directive:'
case('4a60db0e-a8d2-4ee2-891f-7400b8cb5ae3');english='system command failed:'
case('dc37d5a6-4a4f-40ef-8d7b-5073d8cf9a1d');english='system directive encountered but not enabled:'
case('0a92956e-5b6a-459f-821a-7cda7a028995');english='expression invalid:'
case('6b75c2fe-b897-44b4-afbc-cfe7cb32e1b6');english='failed to open output file:'
case('e1f743a8-b1ad-45e3-943e-3e9e46e7af78');english='failed to open output file:'
case('77d265df-b299-4ab7-5649-b3f4ffda569e');english='failed to load file for base64 encoding:'
case('fb8ce648-6675-43e1-8210-90d245c03f75');english='failed to open parcel scratch file:'
case('7f8de002-15dc-4eed-a851-38f5c14c533c');english='parcel name not defined for'
case('f5a425ba-b9d8-4934-9ebe-e5b01f1f8bd5');english='error rewinding'
case('c2bd8c69-40b0-4e72-85d9-8637628a39a9');english='input file nesting too deep:'
case('d5cf2021-81f9-4175-813f-e18e89616358');english='description too long:'
case('d46f723c-3188-43cd-93ad-45f81eeecdc5');english='language unknown for'
case('ae171d54-23f2-42a2-bf29-7a3f5ed692ab');english='null variable name:'
case('973c0777-0e6f-4bbd-aefd-0406e3ebc1ae');english='name contains unallowed character(or general syntax error):'
case('544ca822-3d1f-4369-afa1-a73d2beee896');english='missing targets for '
case('fb3d7c78-1c5c-4469-8e52-68da257f1fa9');english='"IF" block nesting too deep, limited to '//v2s(G_nestl_max)//' levels,'
case('eb5f9a4f-28b5-4740-a331-03fb7510fab8');english='"IF" expression invalid:'
case('cf800a61-7287-453d-a6c1-dbd8878d4da0');english='block nesting too deep, limited to '//v2s(G_nestl_max)//' levels in:'
case('12f6abb7-18f6-4d7e-b204-637cf24495b3');english='misplaced $ELSE or $ELSEIF directive:'
case('b02d372b-a722-4385-95fe-a218a628578f');english='misplaced $ENDIF directive:'
case('c759de9e-33a9-41d7-a959-a5ff30e0f0f2');english='undefined variable.'
case('b56bc1be-7600-4bd5-9fe5-8196b0d9bd7e');english='constant logical expression required.'
case('dec944d1-ff1f-4ff3-9512-a5801fc5e43b');english='filter command failed to open process:'
case('b664d76c-4887-4f49-b874-90790ec62746');english='filter command $BLOCK encountered but system commands not enabled:'
case('0c5a4a0e-ac8d-4ef4-b160-de2d344398a6');english='unexpected "BLOCK" option. found in '//trim(G_source)
case('fde8fa32-3c90-4cae-8386-cb968877973e');english='unexpected "BLOCK" option. found in '//trim(G_source)
case('3f6bb821-f10b-4be0-aa4a-46ba6b2a1e48');english='failed to open document output file:'
case('e9e2c7a9-f3b6-4625-a79c-bb42c5dc798b');english='failed to write output file:'
case('0a63628d-cd62-4b10-a73b-c9d5f0ed8674');english='failed to write comment block'
case('952044a1-c02f-4ede-bb27-9d3b9f7362b6');english='failed open of input file('
case('f27e4791-c486-4f1c-9182-851beb3bca99');english='input file nesting too deep:'
case('2c97a594-8cc7-4df1-a3d9-04c766c78ada');english='missing input file:'
case('81adcf82-0fcf-4c1a-a445-2999f95107fa');english='missing targets in'
case('89c6cbb8-e29d-484a-ba5e-837cc8b04115');english='failed to write to process:'
case('9407fa5e-6374-4387-a102-051a5beb0eec');english='unexpected "BLOCK" value. Found:'
case('369a62cc-4e35-40af-a525-b8ad5e96d129');english='unexpected "BLOCK" value. Found:'
case('a1dde49d-722b-4811-abba-5867cf4018f9');english='expression invalid:'
case('e1c05d6d-550b-4535-92ec-819453d6c99f');english='incomplete set:'
case('efbbe046-065d-41ca-acef-115909b11bba');english='expression invalid:'
case('6f7e0453-4c1d-4040-9c7d-3adf5112b692');english='block not closed in'
case('61a446d7-7e3e-418a-a2df-cd50dc89148a');english='unknown type:'//sget('type')
case('cd883e90-894a-47ec-9c6f-644b99848ca6');english='Illegal stop pattern '//G_extract_stop
case('f12d13a2-b397-4b59-a857-381994f47bc6');english='Illegal stop pattern '//G_extract_stop
case('fd4cb3f6-1878-44bc-8ab6-f5ae33ef845e');english='Illegal start pattern '//G_extract_start
case('098da459-2255-44a1-8cdb-f4c9825488b8');english='Illegal start pattern '//G_extract_start
case default; english=trim(translate)
end select
end function en
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine warn_prep(message)                   !@(#)warn_prep(3f): write MESSAGE to stderr and and continue program
character(len=*),intent(in)  :: message
   call write_err(message)
   call write_err(trim(G_SOURCE))
end subroutine warn_prep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

! This documentation is a combination of
!    o the original Lahey documentation of fpp(1) from "LAHEY FORTRAN REFERENCE MANUAL"; Revision C, 1992;
!    o documentation for the features subsequently added to the program.
!    o examination of the code.

subroutine setup(help_text,version_text) !@(#)help_usage(3f): prints help information
implicit none
character(len=:),allocatable,intent(out) :: help_text(:)
character(len=:),allocatable,intent(out) :: version_text(:)
!-------------------------------------------------------------------------------
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   prep(1) - [DEVELOPER] preprocess Fortran source files                        ',&
'   (LICENSE:MIT)                                                                ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   prep [[-D] define_list]                                                      ',&
'        [-I include_directories]                                                ',&
'        [-i input_file(s)]                                                      ',&
'        [-o output_file]                                                        ',&
'        [--system]                                                              ',&
'        [--type FILE_TYPE | --start START_STRING --stop STOP_STRING]            ',&
'        [--prefix character|ADE]                                                ',&
'        [--keeptabs]                                                            ',&
'        [--noenv]                                                               ',&
'        [--underscore]                                                          ',&
'        [--width n]                                                             ',&
'        [-d ignore|remove|blank]                                                ',&
'        [--comment default|doxygen|ford|none]                                   ',&
'        [--ident]                                                               ',&
'        [--verbose]                                                             ',&
'        [--help| --usage| --crib| --version]                                    ',&
'                                                                                ',&
'   IMPORTANT                                                                    ',&
'   For compatibility with other utilities where cpp(1)-like syntax is required  ',&
'   if -i is not specified and the unnamed parameters are two or less the        ',&
'   parameters are assumed to be the input file and optional output file         ',&
'   instead of macro definitions (if that first parameter matches an existing    ',&
'   filename). That is, if -i is not specified simple commands like              ',&
'                                                                                ',&
'       prep infile [[-o] outfile]                                               ',&
'                                                                                ',&
'   act like                                                                     ',&
'                                                                                ',&
'       cpp infile [[-o] outfile]                                                ',&
'                                                                                ',&
'   This cpp-like behavior is currently required for custom preprocessors to be  ',&
'   used with the Intel ifx(1) compiler or fpm(1).                               ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'                                                                                ',&
'   prep(1) is a Fortran source preprocessor.                                    ',&
'                                                                                ',&
'   A preprocessor performs operations on input files before they are passed to  ',&
'   a compiler, including conditional selection of lines based on directives     ',&
'   contained in the file. This makes it possible to use a single source file    ',&
'   even when different code is required for different programming environments. ',&
'                                                                                ',&
'   The prep(1) preprocessor has additional features that support free-format    ',&
'   documentation in the same file as the source and the generation of generic   ',&
'   code using a simple templating technique. The basic directives ....          ',&
'                                                                                ',&
'   * Conditionally output parts of the source file (controlled by expressions   ',&
'     on the directives $IF, $IFDEF, $IFNDEF, and $ENDIF. The expressions may    ',&
'     include variables defined on the command line or via the directives        ',&
'     $DEFINE, and $UNDEFINE).                                                   ',&
'                                                                                ',&
'   * Include other files (provided by directive $INCLUDE). This includes        ',&
'     converting the included file to a base64-encoded string.                   ',&
'                                                                                ',&
'   * Define parcels of text that may be replayed multiple times with            ',&
'     expansion, allowing for basic templating (controlled by directives         ',&
'     $PARCEL/$ENDPARCEL and $POST). The mechanism supported is to replace       ',&
'     text of the form ${NAME} with user-supplied strings similar to the         ',&
'     POSIX shell (controlled by directives $SET, $UNSET and $IMPORT).           ',&
'                                                                                ',&
'   * Filter blocks of text and convert them to comments, a CHARACTER array,     ',&
'     Fortran WRITE statements, ... (provided by the $BLOCK directive.)          ',&
'                                                                                ',&
'     The blocks of text may also be written to a file and executed, with        ',&
'     stdout captured and included in the prep(1) output file.                   ',&
'                                                                                ',&
'     Blocked text may optionally be simultaneously written to a separate file,  ',&
'     typically for use as documentation.                                        ',&
'                                                                                ',&
'   * Call system commands (using the $SYSTEM directive).                        ',&
'                                                                                ',&
'   * Generate multiple output files from a single input file (using $OUTPUT).   ',&
'                                                                                ',&
'   * Record the parameters used and the date and time executed as Fortran       ',&
'     comments in the output (using $SHOW).                                      ',&
'                                                                                ',&
'   * Stop the preprocessing (controlled by directive $STOP, $QUIT or $ERROR)    ',&
'     and produce messages on stderr (using $MESSAGE).                           ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   define_list, -D define_list  An optional space-delimited list of expressions ',&
'                                used to define variables before file processing ',&
'                                commences. These can subsequently be used in    ',&
'                                $IF/$ELSE/$ELSEIF and $DEFINE directives.       ',&
'                                                                                ',&
'   -i "input_files"             The default input file is stdin. Filenames are  ',&
'                                space-delimited. In a list, @ represents stdin. ',&
'                                                                                ',&
'   The suggested suffix for Fortran input files is ".ff" for code files unless  ',&
'   they contain $SYSTEM directives in which case ".FF" is preferred. $INCLUDE   ',&
'   files should use ".ffinc" and ".FFINC" if they include prep(1) directives.   ',&
'   This naming convention is not required.                                      ',&
'                                                                                ',&
'   Files may also end in supported suffixes such as ".md", as explained under   ',&
'   the --type option description.                                               ',&
'                                                                                ',&
'   -o output_file               The default output file is stdout.              ',&
'                                                                                ',&
'   -I "include_directories" The directories to search for files specified on    ',&
'                            $INCLUDE directives. May be repeated.               ',&
'                                                                                ',&
'   --system         Allow system commands on $SYSTEM directives to be executed. ',&
'                                                                                ',&
'   --type FILETYPE  This flag indicates to skip input lines until after a       ',&
'                    specific start string is encountered and to stop once a     ',&
'                    specific end string is found, left-justified on lines by    ',&
'                    themselves.                                                 ',&
'                        FileType     Start_String              Stop_String      ',&
'                        --------     ------------              -----------      ',&
'                        md           ```fortran                ```              ',&
'                        markdownMML  ~~~ {: lang=fortran}      ~~~              ',&
'                        html         <xmp>                     </xmp>           ',&
'                        tex          \begin{minted}{Fortran}   \end{minted}     ',&
'                                                                                ',&
'                    The default type is "auto", in which case files will be     ',&
'                    processed according to their file suffix.                   ',&
'                                                                                ',&
'                    This allows for easily extracting code from common document ',&
'                    formats. This is particularly useful with extended markdown ',&
'                    formats, allowing for code source to be easily documented   ',&
'                    and for tests in documents to be able to be extracted and   ',&
'                    tested. "auto" switches processing mode depending on input  ',&
'                    file suffix, treating supported file suffixes               ',&
'                    ("md","markdownMML","html","tex") appropriately.            ',&
'                                                                                ',&
'   --start STRING   Same as --type except along with --stop allows for custom   ',&
'                    strings to be specified. The string is a BRE (Basic Regular ',&
'                    Expression).                                                ',&
'                                                                                ',&
'   --stop STRING    Same as --type except along with --start allows for custom  ',&
'                    strings to be specified. The string is a BRE (Basic Regular ',&
'                    Expression).                                                ',&
'                                                                                ',&
'   --comment        Try to style comments generated in $BLOCK COMMENT blocks    ',&
'                    for other utilities such as doxygen. Default is to          ',&
'                    prefix lines with ''! ''. Allowed keywords are              ',&
'                    currently "default", "doxygen","none","ford".               ',&
'                    THIS IS AN ALPHA FEATURE AND NOT FULLY IMPLEMENTED.         ',&
'                                                                                ',&
'   --prefix ADE|letter  The directive prefix character. The default is "$".     ',&
'                        If the value is numeric it is assumed to be an ASCII    ',&
'                        Decimal Equivalent (Common values are 37=% 42=* 35=#    ',&
'                        36=$ 64=@).                                             ',&
'                                                                                ',&
'   --noenv          The $IFDEF and $IFNDEF directives test for an internal      ',&
'                    prep(1) variable and then an environment variable by        ',&
'                    default. This option turns off testing for environment      ',&
'                    variables.                                                  ',&
'   --underscore     For compatibility with other preprocessors the following    ',&
'                    are equivalent if the --underscore option is specified on   ',&
'                    the command line:                                           ',&
'                                                                                ',&
'                        __FILE__  ${FILE} # name of file being processed        ',&
'                        __LINE__  ${LINE} # line number of current file         ',&
'                        __DATE__  ${DATE} # YYYY-MM-DD                          ',&
'                        __TIME__  ${TIME} # HH:MM:SS                            ',&
'                        __TIMESTAMP__   ${TIMESTAMP} # MMM DD YYYY hh:mm:ss     ',&
'                                                                                ',&
'                    where MMM is a 3-character month-name abbreviation.         ',&
'                                                                                ',&
'   --keeptabs       By default tab characters are expanded assuming a stop has  ',&
'                    been set every eight columns; and trailing carriage-return  ',&
'                    are removed. Use this flag to prevent this processing from  ',&
'                    from occurring.                                             ',&
'                                                                                ',&
'   --ident          The output of the $IDENT directive is in the form of a      ',&
'                    comment by default. If this flag is set the output is       ',&
'                    of the form described in the $IDENT documentation           ',&
'                    so executables and object code can contain the metadata     ',&
'                    for use with the what(1) command. Note this generates an    ',&
'                    unused variable which some compilers might optimize         ',&
'                    away depending on what compilation options are used.        ',&
'                                                                                ',&
'   -d ignore|remove|blank  Enable special treatment for lines beginning         ',&
'                           with "d" or "D". The letter will be left as-is       ',&
'                           (the default); removed; or replaced with a blank     ',&
'                           character. This non-standard "dee''d" syntax has      ',&
'                           been used to support the optional compilation of     ',&
'                           "debug" code by many Fortran compilers when          ',&
'                           compiling fixed-format Fortran source. This also     ',&
'                           controls lines beginning with $?.                    ',&
'                                                                                ',&
'   --width n   Maximum line length of the output file. The default is 1024.     ',&
'               The parameter is typically used to trim fixed-format Fortran     ',&
'               code that contains comments or "ident" labels past column 72     ',&
'               when compiling fixed-format Fortran code.                        ',&
'                                                                                ',&
'   --verbose   All commands on a $SYSTEM directive are echoed to stderr with a  ',&
'               "+" prefix. Text following the string "@(#)" is printed to stderr',&
'               similar to the Unix command what(1) but is otherwise treated as  ',&
'               other text input. Additional descriptive messages are produced.  ',&
'                                                                                ',&
'   --version   Display version and exit                                         ',&
'                                                                                ',&
'   --help      Display documentation and exit.                                  ',&
'                                                                                ',&
'INPUT FILE SYNTAX                                                               ',&
'                                                                                ',&
'   The prep(1) preprocessor directives begin with "$" (by default) in column    ',&
'   one, and prep(1) will output no such lines. Other input is conditionally     ',&
'   written to the output file(s) based on the case-insensitive command names.   ',&
'                                                                                ',&
'   An exclamation character FOLLOWED BY A SPACE on most directives              ',&
'   begins an in-line comment that is terminated by an end-of-line. The space    ',&
'   is required so comments are not confused with C-style logical operators such ',&
'   as "!", which may NOT be followed by a space.                                ',&
'                                                                                ',&
' VARIABLES AND EXPRESSIONS                                                      ',&
'                                                                                ',&
'   INTEGER or LOGICAL expressions are used to conditionally select              ',&
'   output lines. An expression is composed of INTEGER and LOGICAL               ',&
'   constants, variable names, and operators. Operators are processed            ',&
'   as in Fortran and/or C expressions. The supported operators are ...          ',&
'                                                                                ',&
'       #-----#-----#-----#-----#-----#                #-----#-----#             ',&
'       |  +  |  -  |  *  |  /  |  ** | Math Operators #  (  |  )  | Grouping    ',&
'       #-----#-----#-----#-----#-----#                #-----#-----#             ',&
'       Logical Operators                                                        ',&
'       #-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#------#     ',&
'       | .EQ.| .NE.| .GE.| .GT.| .LE.| .LT.|.NOT.|.AND.| .OR.|.EQV.|.NEQV.|     ',&
'       |  == |  /= |  >= |  >  |  <= |  <  |  !  |  && |  || | ==  |  !=  |     ',&
'       #-----#  != #-----#-----#-----#-----#-----#-----#-----#-----#------#     ',&
'             #-----#                                                            ',&
'       C-style operators NOT supported:   %,  <<,  >>, &,  ~                    ',&
'                                                                                ',&
'DIRECTIVES                                                                      ',&
'                                                                                ',&
' The directives fall into the following categories:                             ',&
'                                                                                ',&
' VARIABLE DEFINITION FOR CONDITIONALS                                           ',&
'   Directives for defining variables ...                                        ',&
'                                                                                ',&
'      $DEFINE   variable_name[=expression] [;...]          [! comment ]         ',&
'      $UNDEFINE|$UNDEF variable_name [;...]                [! comment ]         ',&
'                                                                                ',&
'   Details ...                                                                  ',&
'                                                                                ',&
'      $DEFINE variable_name [=expression]; ... [! comment ]                     ',&
'                                                                                ',&
'   Defines a numeric or logical variable name and its value. The variable       ',&
'   names may subsequently be used in the expressions on the conditional output  ',&
'   selector directives $IF, $ELSEIF, $IFDEF, and $IFNDEF.                       ',&
'                                                                                ',&
'   If the result of the expression is ".TRUE." or ".FALSE." then the variable   ',&
'   will be of type LOGICAL, otherwise the variable is of type INTEGER (and the  ',&
'   expression must be an INTEGER expression or null). If no value is supplied   ',&
'   the variable is given the INTEGER value "1".                                 ',&
'                                                                                ',&
'   Variables are defined from the point they are declared in a $DEFINE          ',&
'   directive or the command line until program termination unless explicitly    ',&
'   undefined with a $UNDEFINE directive.                                        ',&
'                                                                                ',&
'   Example:                                                                     ',&
'                                                                                ',&
'    > $define A                        ! will have default value of "1"         ',&
'    > $define B = 10 - 2 * 2**3 / 3    ! integer expressions                    ',&
'    > $define C=1+1; D=(-40)/(-10)                                              ',&
'    > $define bigd= d  >=  a; bigb = ( (b >= c) && (b > 0) )  ! logical         ',&
'    > $if ( A + B ) / C  ==  1                                                  ',&
'    >    (a+b)/c is one                                                         ',&
'    > $endif                                                                    ',&
'   Note expressions are not case-sensitive.                                     ',&
'                                                                                ',&
'       $UNDEFINE variable_name[; ...]                                           ',&
'                                                                                ',&
'   A symbol defined with $DEFINE can be removed with the $UNDEFINE directive.   ',&
'   Multiple names may be specified, preferably separated by semi-colons.        ',&
'                                                                                ',&
'   Basic globbing is supported, where "*" represents any string, and "?"        ',&
'   represents any single character.                                             ',&
'                                                                                ',&
'       DEFINED(variable_name[,...])                                             ',&
'                                                                                ',&
'   A special function called DEFINED() may appear only in a $IF or $ELSEIF.     ',&
'   If "variable_name" has been defined at that point in the source code,        ',&
'   then the function value is ".TRUE.", otherwise it is ".FALSE.". A name is    ',&
'   defined only if it has appeared in the source previously in a $DEFINE        ',&
'   directive or been declared on the command line.                              ',&
'   The names used in compiler directives are district from names in the         ',&
'   Fortran source, which means that "a" in a $DEFINE and "a" in a Fortran       ',&
'   source statement are totally unrelated.                                      ',&
'   The DEFINED() variable is NOT valid in a $DEFINE directive.                  ',&
'                                                                                ',&
'   Example:                                                                     ',&
'                                                                                ',&
'    >        Program test                                                       ',&
'    > $IF .NOT. DEFINED (inc)                                                   ',&
'    >        INCLUDE "comm.inc"                                                 ',&
'    > $ELSE                                                                     ',&
'    >        INCLUDE "comm2.inc"                                                ',&
'    > $ENDIF                                                                    ',&
'    >        END                                                                ',&
'                                                                                ',&
'   The file, "comm.inc" will be included in the source if the variable          ',&
'   "inc", has not been previously defined, while INCLUDE "comm2.inc" will       ',&
'   be included in the source if "inc" has been defined.                         ',&
'                                                                                ',&
'   Predefined variables are                                                     ',&
'                                                                                ',&
'    SYSTEMON=.TRUE. if --system was present on the command line, else .FALSE.   ',&
'                                                                                ',&
'    UNKNOWN = 0 LINUX   = 1 MACOS   = 2 WINDOWS = 3                             ',&
'    CYGWIN  = 4 SOLARIS = 5 FREEBSD = 6 OPENBSD = 7                             ',&
'    In addition OS is set to what the program guesses the system type is.       ',&
'                                                                                ',&
'     > $if OS == LINUX                                                          ',&
'     >    write(*,*)"System type is Linux"                                      ',&
'     > $elseif OS == WINDOWS                                                    ',&
'     >    write(*,*)"System type is MSWindows"                                  ',&
'     > $else                                                                    ',&
'     >    write(*,*)"System type is unknown"                                    ',&
'     > $endif                                                                   ',&
'                                                                                ',&
' CONDITIONAL CODE SELECTION                                                     ',&
'   directives for conditionally selecting input lines ...                       ',&
'                                                                                ',&
'       $IF  logical_integer-based expression |                                  ',&
'       $IFDEF [variable_name|environment_variable] |                            ',&
'       $IFNDEF [variable_name|environment_variable]         [! comment ]        ',&
'               { sequence of source statements}                                 ',&
'       [$ELSEIF|$ELIF logical_integer-based expression      [! comment ]        ',&
'               { sequence of source statements}]                                ',&
'       [$ELSE                                               [! comment ]        ',&
'               { sequence of source statements}]                                ',&
'       $ENDIF                                               [! comment ]        ',&
'       $?  code                                                                 ',&
'                                                                                ',&
'   Details ...                                                                  ',&
'                                                                                ',&
'       $IF/$ELSEIF/$ELSE/$ENDIF directives ...                                  ',&
'                                                                                ',&
'   Each of these control lines delineates a block of source lines. If the       ',&
'   expression following the $IF is ".TRUE.", then the following lines of        ',&
'   source following are output. If it is ".FALSE.", and an $ELSEIF              ',&
'   follows, the expression is evaluated and treated the same as the $IF. If     ',&
'   the $IF and all $ELSEIF expressions are ".FALSE.", then the lines of         ',&
'   source following the optional $ELSE are output. A matching $ENDIF ends the   ',&
'   conditional block.                                                           ',&
'                                                                                ',&
'       $IFDEF/$IFNDEF directives ...                                            ',&
'                                                                                ',&
'   $IFDEF and $IFNDEF are special forms of the $IF directive that simply test   ',&
'   if a variable name is defined or not.                                        ',&
'                                                                                ',&
'   The expressions may optionally be enclosed in parenthesis and followed by    ',&
'   the keyword "THEN", ie. they may use Fortran syntax. For example, the        ',&
'   previous example may also be written as:                                     ',&
'                                                                                ',&
'     > $IF(OS .EQ. LINUX)THEN                                                   ',&
'     >    write(*,*)"System type is Linux"                                      ',&
'     > $ELSEIF(OS .EQ. WINDOWS)THEN                                             ',&
'     >    write(*,*)"System type is MSWindows"                                  ',&
'     > $ELSE                                                                    ',&
'     >    write(*,*)"System type is unknown"                                    ',&
'     > $ENDIF                                                                   ',&
'                                                                                ',&
'   Essentially, these are equivalent:                                           ',&
'                                                                                ',&
'       $IFDEF varname  ==> $IF DEFINED(varname)                                 ',&
'       $IFNDEF varname ==> $IF .NOT. DEFINED(varname)                           ',&
'                                                                                ',&
'   except that environment variables are tested as well by $IFDEF and $IFNDEF   ',&
'   if the --noenv option is not specified, but never by the function DEFINED(), ',&
'   allowing for environment variables to be selectively used or ignored.        ',&
'   The --noenv switch is therefore only needed for compatibility with fpp(1).   ',&
'   For the purposes of prep(1) an environment variable is defined if it is      ',&
'   returned by the system and has a non-blank value.                            ',&
'                                                                                ',&
'   Dee''d lines are simulated by the $? directive as well. See the "-d" option   ',&
'   for a description.                                                           ',&
'                                                                                ',&
' MACRO STRING EXPANSION AND TEXT REPLAY                                         ',&
'   Directives for defining replayable text blocks ...                           ',&
'                                                                                ',&
'       $PARCEL blockname  / $ENDPARCEL                      [! comment ]        ',&
'       $POST     blockname(s)                               [! comment ]        ',&
'       $SET varname  string                                                     ',&
'       $UNSET varname(s)                                    [! comment ]        ',&
'       $IMPORT   envname[;...]                              [! comment ]        ',&
'                                                                                ',&
'   Details ...                                                                  ',&
'                                                                                ',&
'       $PARCEL blockname / $ENDPARCEL                       [! comment ]        ',&
'                                                                                ',&
'   The block of lines between a "$PARCEL name" and "$ENDPARCEL" directive are   ',&
'   written to a scratch file WITHOUT expanding directives. the scratch file can ',&
'   then be read in with the $POST directive much like a named file can be with  ',&
'   $INCLUDE except the file is automatically deleted at program termination.    ',&
'                                                                                ',&
'       $POST     blockname(s)                               [! comment ]        ',&
'                                                                                ',&
'   Read in a scratch file created by the $PARCEL directive. Combined with       ',&
'   $SET and $IMPORT directives this allows you to replay a section of input     ',&
'   and replace strings as a simple templating technique, or to repeat lines     ',&
'   like copyright information or definitions of (obsolescent) Fortran COMMON    ',&
'   blocks, but contained in source files without the need for separate          ',&
'   INCLUDE files or error-prone repetition of the declarations.                 ',&
'                                                                                ',&
'       $SET varname  string                                                     ',&
'                                                                                ',&
'   If a $SET or $IMPORT directive defines a name prep(1) enters expansion mode. ',&
'   In this mode anywhere the string "${NAME}" is encountered in subsequent      ',&
'   output it is replaced by "string".                                           ',&
'                                                                                ',&
'   o values are case-sensitive but variable names are not.                      ',&
'   o expansion of a line may cause it to be longer than allowed by some         ',&
'     compilers. Automatic breaking into continuation lines does not occur.      ',&
'   o comments are not supported on a $SET directive because everything past the ',&
'     variable name becomes part of the value.                                   ',&
'   o The pre-defined values ${FILE}, ${LINE}, ${DATE}, ${TIME} and ${TIMESTAMP} ',&
'     for file, line number in input file, date and time and date-time) are      ',&
'     NOT ACTIVE until at least one $SET or $IMPORT directive is processed. That ',&
'     is, unless a variable is defined no ${NAME} expansion occurs.              ',&
'   o The time and date refers to the time of processing, not the time of        ',&
'     compilation or loading.                                                    ',&
'                                                                                ',&
'   Example:                                                                     ',&
'                                                                                ',&
'    > $set author  William Shakespeare                                          ',&
'    > write(*,*)''By ${AUTHOR}''                                                ',&
'    > write(*,*)''File ${FILE}''                                                ',&
'    > write(*,*)''Line ${LINE}''                                                ',&
'    > write(*,*)''Date ${DATE}''                                                ',&
'    > write(*,*)''Time ${TIME}''                                                ',&
'   ...                                                                          ',&
'                                                                                ',&
'       $UNSET varname(s)                                                        ',&
'                                                                                ',&
'   Unset variables set with the $SET directive.                                 ',&
'                                                                                ',&
'       $IMPORT   envname[;...]                              [! comment ]        ',&
'                                                                                ',&
'   The values of environment variables may be imported just like their names    ',&
'   and values were used on a $SET directive. The names of the variables are     ',&
'   case-sensitive in regards to obtaining the values, but the names become      ',&
'   case-insensitive in prep(). That is, "import home" gets the lowercase        ',&
'   environment variable "home" and then sets the associated value for the       ',&
'   variable "HOME" to the value.                                                ',&
'                                                                                ',&
'    > $import HOME USER                                                         ',&
'    > write(*,*)''HOME ${HOME}''                                                ',&
'    > write(*,*)''USER ${USER}''                                                ',&
'                                                                                ',&
' EXTERNAL FILES                                                                 ',&
'   Directives for reading and writing external files ...                        ',&
'                                                                                ',&
'       $OUTPUT   filename  [--append]                          [! comment ]     ',&
'       $ENDOUTPUT                                              [! comment ]     ',&
'       $INCLUDE filename  [--base64]                           [! comment ]     ',&
'                                                                                ',&
'   Details ...                                                                  ',&
'                                                                                ',&
'       $OUTPUT   filename  [--append]                          [! comment ]     ',&
'                                                                                ',&
'   Specifies the output file to write to. This overrides the initial output file',&
'   specified with command line options. If no output filename is given          ',&
'   prep(1) reverts back to the initial output file. "@" is a synonym for stdout.',&
'                                                                                ',&
'   Files are open at the first line by default. Use the --append switch to      ',&
'   append to the end of an existing file instead of overwriting it.             ',&
'                                                                                ',&
'       $ENDOUTPUT                                              [! comment ]     ',&
'                                                                                ',&
'   Ends writing to an alternate output file begun by a $OUTPUT directive.       ',&
'                                                                                ',&
'       $INCLUDE filename  [--base64]                                            ',&
'                                                                                ',&
'   Read in the specified input file. Fifty (50) nesting levels are allowed.     ',&
'   Following the tradition of cpp(1) if "<filename>" is specified the file is   ',&
'   only searched for relative to the search directories, otherwise it is        ',&
'   searched for as specified first. Double-quotes in the filename are treated   ',&
'   as in Fortran list-directed input.                                           ',&
'                                                                                ',&
'   If the --base64 option is specified, the file is converted to a              ',&
'   base64-encoded string of the form                                            ',&
'    "encoded_characters" // &                                                   ',&
'    "encoded_characters" // &                                                   ',&
'    "encoded_characters" // &                                                   ',&
'    ""                                                                          ',&
'   Various compilers may have limits on how many continuation lines are allowed.',&
'                                                                                ',&
' TEXT BLOCK FILTERS                                                             ',&
'   (--file is ignored unless $PREP_DOCUMENT_DIR is set)                         ',&
'                                                                                ',&
'      $BLOCK   [null|comment|write|variable [--varname NAME]|                   ',&
'               set|system|message|define                                        ',&
'               help|version] [--file NAME [--append]]      [! comment ]         ',&
'      $ENDBLOCK                                            [! comment ]         ',&
'                                                                                ',&
'   Details ...                                                                  ',&
'                                                                                ',&
'   $BLOCK has several forms but in all cases operates on a block of lines:      ',&
'                                                                                ',&
'     basic filtering:                                                           ',&
'      $BLOCK [comment|null|write                 [--file NAME [--append]]       ',&
'     creating a CHARACTER array:                                                ',&
'      $BLOCK VARIABLE --varname NAME             [--file NAME [--append]]       ',&
'     block versions of prep(1) commands:                                        ',&
'      $BLOCK set|system|message|define           [--file NAME [--append]]       ',&
'     specialized procedure construction:                                        ',&
'      $BLOCK help|version                        [--file NAME [--append]]       ',&
'                                                                                ',&
'      NULL:      Do not write into current output file                          ',&
'      COMMENT:   write text prefixed by an exclamation and a space or according ',&
'                 to the style selected by the --comment style selected on the   ',&
'                 command line.                                                  ',&
'      WRITE:     write text as Fortran WRITE(3f) statements                     ',&
'                 The Fortran generated is free-format. It is assumed the        ',&
'                 output will not generate lines over 132 columns.               ',&
'      VARIABLE:  write as a text variable. The name may be defined using        ',&
'                 the --varname switch. Default name is "textblock".             ',&
'      MESSAGE:   All the lines in the block are treated as options to $MESSAGE  ',&
'      SET:       All the lines in the block are treated as options to $SET      ',&
'      DEFINE:    All the lines in the block are treated as options to $DEFINE   ',&
'      SYSTEM:    The lines are gathered into a file and executed by the shell   ',&
'                 with the stdout being written to a scratch file and then read  ',&
'      END:       End block of specially processed text                          ',&
'                                                                                ',&
'   special-purpose modes primarily for use with the M_kracken module:           ',&
'                                                                                ',&
'      HELP:      write text as a subroutine called HELP_USAGE                   ',&
'      VERSION:   write text as a subroutine called HELP_VERSION prefixing       ',&
'                 lines with @(#) for use with the what(1) command.              ',&
'                                                                                ',&
'   If the "--file NAME" option is present the text is written to the            ',&
'   specified file unfiltered except for string expansion. This allows           ',&
'   documentation to easily be maintained in the source file. It can be          ',&
'   markdownMML, tex, html, markdown or any plain text. The filename will be     ',&
'   prefixed with $PREP_DOCUMENT_DIR/doc/ . If the environment variable          ',&
'   $PREP_DOCUMENT_DIR is not set the option is ignored.                         ',&
'                                                                                ',&
'   The --file output can subsequently easily be processed by other utilities    ',&
'   such as markdown(1) or txt2man(1) to produce man(1) pages and HTML documents.',&
'   $SYSTEM commands may follow the $BLOCK block text to optionally post-process ',&
'   the doc files.                                                               ',&
'                                                                                ',&
'   $ENDBLOCK ends the block.                                                    ',&
!!!!$! which is preferred; but a blank value or "END" on a $BLOCK directive does as well.
'                                                                                ',&
' IDENTIFIERS                                                                    ',&
'   Directives for producing metadata ...                                        ',&
'                                                                                ',&
'       $IDENT|$@(#) metadata [--language fortran|c|shell]      [! comment ]     ',&
'                                                                                ',&
'   $IDENT is a special-purpose directive useful to users of SCCS-metadata.      ',&
'   The string generated can be used by the what(1) command,                     ',&
'                                                                                ',&
'   When the command line option "--ident [LANGUAGE]" is specified this directive',&
'   writes a line using SCCS-metadata format of one of the following forms:      ',&
'                                                                                ',&
'     language:                                                                  ',&
'     fortran   character(len=*),parameter::ident="@(#)metadata"                 ',&
'     c         #ident "@(#)metadata"                                            ',&
'     shell     #@(#) metadata                                                   ',&
'                                                                                ',&
'   The default language is "fortran".                                           ',&
'                                                                                ',&
'   Depending on your compiler and the optimization level used when compiling,   ',&
'   the output string may not remain in the object files and executables created.',&
'                                                                                ',&
'   If the -ident switch is not specified, a Fortran comment line is generated   ',&
'   of the form                                                                  ',&
'                                                                                ',&
'       ! ident_NNN="@(#)this is metadata"                                       ',&
'                                                                                ',&
'   "$@(#)" is an alias for "$IDENT" so the source file itself will contain      ',&
'   SCCS-metadata so the metadata can be displayed with what(1) even for the     ',&
'   unprocessed files.                                                           ',&
'                                                                                ',&
'   Do not use the characters double-quote, greater-than, backslash (ie. ">\)    ',&
'   in the metadata to remain compatible with SCCS metadata syntax.              ',&
'   Do not use strings starting with " -" either.                                ',&
'                                                                                ',&
' INFORMATION                                                                    ',&
'   Informative directives for writing messages to stderr or inserting           ',&
'   state information into the output file ...                                   ',&
'                                                                                ',&
'       $SHOW [variable_name[;...]]                          [! comment ]        ',&
'       $MESSAGE  message_to_stderr                                              ',&
'                                                                                ',&
'   Details ...                                                                  ',&
'                                                                                ',&
'       $MESSAGE  message_to_stderr                                              ',&
'                                                                                ',&
'   Write message to stderr.                                                     ',&
'   Note that messages for $MESSAGE do not treat "! " as starting a comment      ',&
'                                                                                ',&
'       $SHOW [variable_name[;...]]                          [! comment ]        ',&
'                                                                                ',&
'   Shows current state of prep(1); including variable names and values and      ',&
'   the name of the current input files. All output is preceded by an            ',&
'   exclamation character.                                                       ',&
'                                                                                ',&
'   If a list of defined variable names is present only those variables and      ',&
'   their values are shown.                                                      ',&
'                                                                                ',&
'   Basic globbing is supported, where "*" represents any string, and "?"        ',&
'   represents any single character.                                             ',&
'                                                                                ',&
'   Example:                                                                     ',&
'                                                                                ',&
'    > prep A=10 B C D -o paper                                                  ',&
'    > $define z=22                                                              ',&
'    > $show B Z                                                                 ',&
'    > $show                                                                     ',&
'    > $show H*;*H;*H*! show beginning with "H", ending with "H", containing "H" ',&
'    > $stop 0                                                                   ',&
'    >                                                                           ',&
'    > !  B  =  1                                                                ',&
'    > !  Z  =  22                                                               ',&
'    > !================================================================         ',&
'    > !                                                                         ',&
'    > ! Current state of prep(1):(18:39 20 Jun 2021)                            ',&
'    > ! Total lines read ............... 2                                      ',&
'    > ! Conditional nesting level....... 0                                      ',&
'    > ! G_WRITE (general processing).... T                                      ',&
'    > ! G_LLWRITE (write input lines)... T                                      ',&
'    > ! Arguments ...................... A=10 B C D -o paper                    ',&
'    > ! Open files:                                                             ',&
'    > !    unit ! line number ! filename                                        ',&
'    > !       5 !           2 ! @                                               ',&
'    > ! INCLUDE directories:                                                    ',&
'    > !    .                                                                    ',&
'    > ! Variables:                                                              ',&
'    > !    $DEFINE UNKNOWN  =  0                                                ',&
'    > !    $DEFINE LINUX  =  1                                                  ',&
'    > !    $DEFINE MACOS  =  2                                                  ',&
'    > !    $DEFINE WINDOWS  =  3                                                ',&
'    > !    $DEFINE CYGWIN  =  4                                                 ',&
'    > !    $DEFINE SOLARIS  =  5                                                ',&
'    > !    $DEFINE FREEBSD  =  6                                                ',&
'    > !    $DEFINE OPENBSD  =  7                                                ',&
'    > !    $DEFINE OS  =  1                                                     ',&
'    > !    $DEFINE A  =  10                                                     ',&
'    > !    $DEFINE B  =  1                                                      ',&
'    > !    $DEFINE C  =  1                                                      ',&
'    > !    $DEFINE D  =  1                                                      ',&
'    > !    $DEFINE Z  =  22                                                     ',&
'    > ! Parcels:                                                                ',&
'    > !================================================================         ',&
'                                                                                ',&
' SYSTEM COMMANDS                                                                ',&
'   Directives that execute system commands ...                                  ',&
'                                                                                ',&
'       $SYSTEM system_command                                                   ',&
'                                                                                ',&
'   If system command processing is enabled using the --system switch system     ',&
'   commands can be executed for such tasks as creating files to be read or to   ',&
'   further process documents created by $BLOCK. $SYSTEM directives are errors   ',&
'   by default; as you clearly need to ensure the input file is trusted before   ',&
'   before allowing commands to be executed. Commands that are system-specific   ',&
'   may need to be executed conditionally as well.                               ',&
'                                                                                ',&
'   Examples:                                                                    ',&
'                                                                                ',&
'    > $! build variable definitions using GNU/Linux commands                    ',&
'    > $SYSTEM echo system=`hostname` > compiled.h                               ',&
'    > $SYSTEM echo compile_time="`date`" >> compiled.h                          ',&
'    > $INCLUDE compiled.h                                                       ',&
'                                                                                ',&
'    > $if systemon      ! if --system switch is present on command line         ',&
'    > $!  obtain up-to-date copy of source file from HTTP server:               ',&
'    > $   SYSTEM wget http://repository.net/src/func.F90 -O - >_tmp.f90         ',&
'    > $   INCLUDE _tmp.f90                                                      ',&
'    > $   SYSTEM  rm _tmp.f90                                                   ',&
'    > $endif                                                                    ',&
'                                                                                ',&
'   System commands may also appear in a $BLOCK section. Combining several       ',&
'   features this uses the Linux getconf(1) command to write some lines          ',&
'   into a scratch file that are then read back in to define variables describing',&
'   the current platform.                                                        ',&
'                                                                                ',&
'    > $IF OS == LINUX                                                           ',&
'    > $                                                                         ',&
'    > $block system ! use getconf(1) command to get system values               ',&
'    > (                                                                         ',&
'    > echo LEVEL_2_CACHE_SIZE $(getconf LEVEL2_CACHE_SIZE)                      ',&
'    > echo LEVEL_3_CACHE_SIZE $(getconf LEVEL3_CACHE_SIZE)                      ',&
'    > ) >_getconf.inc                                                           ',&
'    > $endblock                                                                 ',&
'    > $block set                 ! read in output of getconf(1)                 ',&
'    > $include _getconf.inc                                                     ',&
'    > $endblock                                                                 ',&
'    > $system rm -f _getconf.inc ! cleanup                                      ',&
'    > $                                                                         ',&
'    > $ELSE                                                                     ',&
'    > $                                                                         ',&
'    > $error " ERROR: Not Linux. did not obtain system values"                  ',&
'    > $                                                                         ',&
'    > $ENDIF                                                                    ',&
'    > $! create code using values for this platform                             ',&
'    >    integer, parameter :: L2_CACHE_SZ=${LEVEL2_CACHE_SIZE}                 ',&
'    >    integer, parameter :: L3_CACHE_SZ=${LEVEL3_CACHE_SIZE}                 ',&
'                                                                                ',&
' PROGRAM TERMINATION                                                            ',&
'   Directives for stopping file processing (note there is no comment field):    ',&
'                                                                                ',&
'      $STOP     [stop_value ["message"]]                                        ',&
'      $QUIT     ["message"]                                                     ',&
'      $ERROR    ["message"]                                                     ',&
'                                                                                ',&
'   Details ...                                                                  ',&
'                                                                                ',&
'      $STOP     [stop_value ["message"]]                                        ',&
'                                                                                ',&
'   Stops the prep(1) program. The integer value will be returned as an exit     ',&
'   status value by the system where supported.                                  ',&
'                                                                                ',&
'   o A value of "0" causes normal program termination.                          ',&
'   o The default value is "1".                                                  ',&
'   o comments are not supported on these directives; the entire line following  ',&
'     the directive command becomes part of the message.                         ',&
'   o If a message is supplied it is displayed to stderr.                        ',&
'     If the value is not zero ("0") and no message is supplied the "$SHOW"      ',&
'     directive is called before stopping.                                       ',&
'   o "$QUIT" is an alias for "$STOP 0".                                         ',&
'   o "$ERROR" is a synonym for "$STOP 1"                                        ',&
'                                                                                ',&
'     >$IFNDEF TYPE                                                              ',&
'     >$STOP 10 "ERROR: ""TYPE"" not defined"                                    ',&
'     >$ENDIF                                                                    ',&
'                                                                                ',&
'LIMITATIONS                                                                     ',&
'                                                                                ',&
'   $IF constructs can be nested up to 20 levels deep. Note that using           ',&
'   more than two levels typically makes input files less readable.              ',&
'                                                                                ',&
'   $ENDBLOCK is required after a $BLOCK or --file FILENAME is not written.      ',&
'                                                                                ',&
'   Nesting of $BLOCK sections not allowed.                                      ',&
'   $INCLUDE may be nested fifty (50) levels.                                    ',&
'                                                                                ',&
'   Input files                                                                  ',&
'                                                                                ',&
'   o lines are limited to a maximum of 1024 columns. Text past the limit is     ',&
'     ignored.                                                                   ',&
'   o files cannot be concurrently opened multiple times                         ',&
'   o a maximum of 50 files can be nested by $INCLUDE                            ',&
'   o filenames cannot contain spaces on the command line.                       ',&
'                                                                                ',&
'   Variable names                                                               ',&
'                                                                                ',&
'   o are limited to 63 characters.                                              ',&
'   o must start with a letter (A-Z) or underscore(_).                           ',&
'   o are composed of the letters A-Z, digits 0-9 and _ and $.                   ',&
'   o 2048 variable names may be defined at a time.                              ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'                                                                                ',&
'  Define variables on command line:                                             ',&
'                                                                                ',&
'  Typically, variables are defined on the command line when prep(1) is          ',&
'  invoked but can be grouped together into small files that are included        ',&
'  with a $INCLUDE or as input files.                                            ',&
'                                                                                ',&
'    > prep HP size=64 -i hp_directives.dirs test.F90 -o test_out.f90            ',&
'                                                                                ',&
'  defines variables HP and SIZE as if the expressions had been on a             ',&
'  $DEFINE and reads file "hp_directives.dirs" and then test.F90.                ',&
'  Output is directed to test_out.f90                                            ',&
'                                                                                ',&
'  Basic conditionals:                                                           ',&
'                                                                                ',&
'   > $! set variable "a" if not specified on the prep(1) command.               ',&
'   > $IF .NOT.DEFINED(A)                                                        ',&
'   > $   DEFINE a=1  ! so only define the first version of SUB(3f) below        ',&
'   > $ENDIF                                                                     ',&
'   >    program conditional_compile                                             ',&
'   >       call sub()                                                           ',&
'   >    end program conditional_compile                                         ',&
'   > $! select a version of SUB depending on the value of variable "a"          ',&
'   > $IF a .EQ. 1                                                               ',&
'   >    subroutine sub                                                          ',&
'   >       print*, "This is the first SUB"                                      ',&
'   >    end subroutine sub                                                      ',&
'   > $ELSEIF a  ==  2                                                           ',&
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
'   > This is a block of text that will be ignored except it is optionally       ',&
'   > written to a $PREP_DOCUMENT_DIR/doc/ file when $PREP_DOCUMENT_DIR is set.  ',&
'   > $ENDBLOCK                                                                  ',&
'   >                                                                            ',&
'                                                                                ',&
'  This is a block of text that will be converted to comments and optionally     ',&
'  appended to a $PREP_DOCUMENT_DIR/doc/ file when $PREP_DOCUMENT_DIR is set.    ',&
'                                                                                ',&
'   > $BLOCK COMMENT --file conditional_compile.man                              ',&
'   > NAME                                                                       ',&
'   >    conditional_compile - basic example for prep(1) preprocessor.           ',&
'   > SYNOPSIS                                                                   ',&
'   >    conditional_example [--help] [--version]                                ',&
'   > DESCRIPTION                                                                ',&
'   >    This is a basic example program showing how documentation can be        ',&
'   >    used to generate program help text                                      ',&
'   > OPTIONS                                                                    ',&
'   >    --help     display this help and exit                                   ',&
'   >    --version  output version information and exit                          ',&
'   > $ENDBLOCK                                                                  ',&
'                                                                                ',&
'GENERAL TEMPLATING                                                              ',&
'  A parcel can be posted multiple times, changing the value of variables        ',&
'  before each post.                                                             ',&
'                                                                                ',&
'   > $PARCEL mysub                                                              ',&
'   > subroutine mysub_${TYPE}(a,b)                                              ',&
'   > use, intrinsic :: iso_fortran_env, only : &                                ',&
'   > & real_kinds, real32,real64,real128                                        ',&
'   > implicit none                                                              ',&
'   > integer,parameter  :: wp=${TYPE}                                           ',&
'   > real(kind=wp) :: a,b                                                       ',&
'   >    write(*,*)10.0_wp                                                       ',&
'   >    write(*,*) "this is for type ${TYPE}"                                   ',&
'   > end subroutine mysub_${TYPE}                                               ',&
'   >                                                                            ',&
'   > $ENDPARCEL                                                                 ',&
'   > $set type real32                                                           ',&
'   > $post mysub                                                                ',&
'   > $set type real64                                                           ',&
'   > $post mysub                                                                ',&
'   > $set type real128                                                          ',&
'   > $post mysub                                                                ',&
'                                                                                ',&
'NOTE                                                                            ',&
'  Not documented elsewhere, note that there is a developer flag (--debug) that  ',&
'  can be useful when learning prep(1) usage (but it should not be used in       ',&
'  production). Among other things it deactivates the termination of the program ',&
'  upon detection of an error. This mode thus allows for simple interactive use. ',&
'  In addition, when in this mode entering "$HELP" produces a cribsheet, which   ',&
'  may also be displayed by "prep --crib".                                       ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'                                                                                ',&
'LICENSE                                                                         ',&
'   MIT                                                                          ',&
'']

version_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        prep(1f)>',&
'@(#)DESCRIPTION:    Fortran Preprocessor>',&
!'@(#)VERSION:        4.0.0: 20170502>',&
!'@(#)VERSION:        5.0.0: 20201219>',&
!'@(#)VERSION:        8.1.1: 20220405>',&
!'@(#)VERSION:        9.0.0: 20220804>',&
!'@(#)VERSION:        9.1.0: 20220805>',&
'@(#)VERSION:        9.2.0: 20220814>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE       https://github.com/urbanjost/prep.git/>',&
'']
end subroutine setup
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine crib_help(lun) !@(#)crib_help(3f): prints abridged help information
implicit none
integer,intent(in) :: lun
character(len=:),allocatable :: help_text(:)
integer                        :: i
help_text=[ CHARACTER(LEN=128) :: &
"EXPRESSIONS                                                                     ",&
"  numeric operators are +,-,*,/,**, (). Logical operators are                   ",&
"   >  | .EQ.| .NE.| .GE.| .GT.| .LE.| .LT.|.NOT.|.AND.| .OR.| .EQV.|.NEQV.|     ",&
"   >  |  == |  /= |  >= |  >  |  <= |  <  |  !  |  && |  || |  ==  |  !=  |     ",&
"  $DEFINE variable_name[=expression][;...]                                      ",&
'   > Predefined values are "OS", which is set to a guess of the system type, and',&
"   > UNKNOWN=0 LINUX=1 MACOS=2 WINDOWS=3 CYGWIN=4 SOLARIS=5 FREEBSD=6 OPENBSD=7.",&
"   > SYSTEMON is .TRUE. if --system is present on the command line, else .FALSE.",&
"  $UNDEFINE|$UNDEF variable_name[;...]                                          ",&
"CONDITIONAL CODE SELECTION:                                                     ",&
"  $IF logical_integer-based_expression| [.NOT.] DEFINED(varname[,...])          ",&
"  $IFDEF|$IFNDEF variable_or_envname                                            ",&
"  $ELSEIF|$ELIF logical_integer-based_expression                                ",&
"  $ELSE                                                                         ",&
"  $ENDIF                                                                        ",&
"  $? code ! code is included only if -d switch appears on command line          ",&
"MACRO STRING EXPANSION AND TEXT REPLAY:                                         ",&
"   > Unless at least one variable name is defined no ${NAME} expansion occurs.  ",&
"  $SET varname string                                                           ",&
"  $$UNSET variable_name[;...]                                                   ",&
"  $IMPORT envname[;...]                                                         ",&
"   > $set author  William Shakespeare                                           ",&
"   > $import HOME                                                               ",&
"   > write(*,*)'${AUTHOR} ${DATE} ${TIME} File ${FILE} Line ${LINE} HOME ${HOME}",&
"  $PARCEL blockname ... $ENDPARCEL ! a reuseable parcel of expandable text      ",&
"  $POST   blockname(s)  ! insert a defined parcel of text                       ",&
"EXTERNAL FILES (see $BLOCK ... --file also)                                     ",&
"  $OUTPUT filename [--append]                                                   ",&
"  $INCLUDE filename [--base64]                                                  ",&
"TEXT BLOCK FILTERS (--file writes to $PREP_DOCUMENT_DIR/doc/NAME)               ",&
"  $BLOCK [comment|null|write|variable [--varname NAME]|set|system|message|      ",&
"         define|help|version][--file NAME [--append]] ... $ENDBLOCK             ",&
"INFORMATION                                                                     ",&
"  $MESSAGE message_to_stderr                                                    ",&
"  $SHOW [defined_variable_name][;...]                                           ",&
"SYSTEM COMMANDS (see also: $BLOCK SYSTEM)                                       ",&
"  $SYSTEM command                                                               ",&
"  $STOP [stop_value[ ""message""]] | $QUIT [""message""]| $ERROR [""message""]        "]
   WRITE(lun,'(a)')(trim(help_text(i)),i=1,size(help_text))
end subroutine crib_help
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_out(line)  !@(#)writeout(3f):  write (most) source code lines to output file
character(len=*),intent(in)    :: line
integer                        :: istart

   if(G_verbose)then              ! echo "what" lines to stderr
      istart=index(line,'@(#)')
      if(istart /= 0)then
         call write_err( '+ -->>'//trim(line(istart+4:)) )
      endif
   endif

   call www(line)
end subroutine write_out
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine www(line) !@(#)www(3f):  change line into a WRITE, HELP/VERSION, COMMENT output line
integer,parameter            :: linewidth=128
character(len=*),intent(in)  :: line
character(len=:),allocatable :: buff
character(len=115)           :: chunk
integer                      :: ilength
integer                      :: ios
integer                      :: ierr
character(len=256)           :: message
character(len=G_var_len)     :: value

   ierr=0
   select case(trim(G_outtype))

   case('comment')                             ! write as a Fortran comment preceded by two explanations and a space
                                               ! will be written later at end of BLOCK section

   case('null')                                ! do not write

   case('set','replace')                       ! do not write
      call set(line)

   case('define')                              ! do not write
      call expr(nospace(upper(line)),value,ierr,def=.true.)    ! only process DEFINE if not skipping data lines

   case('redefine')                            ! do not write
      call expr(nospace(upper(line)),value,ierr,def=.true.)    ! only process DEFINE if not skipping data lines

   case('message')                             ! do not write
      call write_err(line)                     ! trustingly trim MESSAGE from directive

   case('system')
      write(G_scratch_lun,'(a)',iostat=ios,iomsg=message)trim(line)
      if(ios < 0)then
         call stop_prep('89c6cbb8-e29d-484a-ba5e-837cc8b04115','failed to write to process:',trim(line)//':'//trim(message))
      endif

   case('variable')
      buff=trim(line)                                 ! do not make a line over 132 characters. Trim input line if needed
      buff=buff//repeat(' ',max(linewidth,len(buff))) ! ensure space in buffer for substitute
      call substitute(buff,"'","''")                  ! change single quotes in input to two adjacent single quotes
      ilength=min(len_trim(buff),linewidth)           ! make all lines have at least linewidth characters for a more legible output
      G_varname_width=max(G_varname_width,ilength)
      write(G_iout,'("''",a,"'',&")') pad(buff(:ilength),G_varname_width,right=.true.)

   case('help')
      buff=trim(line)                                    ! do not make a line over 132 characters. Trim input line if needed
      buff=buff//repeat(' ',max(linewidth,len(buff)))    ! ensure space in buffer for substitute
      call substitute(buff,"'","''")                     ! change single quotes in input to two adjacent single quotes
      ilength=max(80,min(len_trim(buff),linewidth))      ! make all lines have at least 80 characters for a more legible output
      write(G_iout,'("''",a,"'',&")') buff(:ilength)

   case('version')                             ! write version information with SCCS ID prefix for use with what(1) command
      write(G_iout,'("''@(#)",a,"'',&")')trim(line(:min(len_trim(line),128-1)))//'>'

                                               !x! should handle longer lines and split them
   case('write')                               ! convert string to a Fortran write statement to unit "IO"
      buff=trim(line)                          ! do not make a line over 132 characters. Trim input line if needed
      buff=buff//repeat(' ',max(linewidth,len(buff))) ! ensure space in buffer for substitute
      call substitute(buff,"'","''")
      write(G_iout,'(a)',advance='no')'write(io,''(a)'')'''
      chunk=buff
      write(G_iout,'(a)',advance='no')trim(chunk)
      write(G_iout,'(a)')''''

   case('','asis')
      write(G_iout,'(a)')trim(line(:min(len(line),G_iwidth)))

   case default
      call stop_prep('9407fa5e-6374-4387-a102-051a5beb0eec','unexpected "BLOCK" value. Found:',trim(G_source))
      call stop_prep('369a62cc-4e35-40af-a525-b8ad5e96d129','unexpected "BLOCK" value. Found:',trim(G_outtype))

   end select

   if(ierr /= 0) call stop_prep('a1dde49d-722b-4811-abba-5867cf4018f9','expression invalid:',trim(G_source))

   if(G_MAN_COLLECT)then
      G_MAN=G_MAN//new_line('N')//trim(line)
   endif

   G_comment_count=G_comment_count+1

end subroutine www
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_deed(line) !@(#)M_verify::deed(3f): writes a line selectively depending on value of -d option
character(len=*),intent(in)  :: line
integer                      :: ios
character(len=:),allocatable :: lineout

   select case(G_deed(1:1))
   case('i')                                         ! ignore
      return
   case('r')                                         ! remove
      lineout=line
   case('b',' ')                                     ! blank
      lineout='  '//line
   case('c','e')                                     ! comment
      lineout='!'//line
   case default
      lineout=line
   end select
   write(G_iout,'(a)')trim(lineout)
   call flushit()
end subroutine write_deed
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_err(msg) !@(#)M_verify::write_err(3f): writes a message to standard error using a standard f2003 method
character(len=*),intent(in) :: msg
integer                     :: ios

   write(stderr,'(a)',iostat=ios) trim(msg)
   call flushit()
end subroutine write_err
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine dissect2(verb,init,pars) !@(#)dissect2(3f): convenient call to parse() -- define defaults, then process
!
character(len=*),intent(in)  :: verb             ! the name of the command to be reset/defined  and then set
character(len=*),intent(in)  :: init             ! used to define or reset command options; usually hard-set in the program.
character(len=*),intent(in)  :: pars             ! defines the command options to be set, usually from a user input file
   !call dissect(verb,init,pars,len(pars),error_return)
   call set_args(init,string=trim(pars)//' --')
end subroutine dissect2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine import(line)
character(len=*),intent(in)  :: line
character(len=:),allocatable :: names(:)
integer                      :: i
   names=sep(line,' ,;')
   do i=1,size(names)
      call set(names(i)//' '//get_env(names(i)))
   enddo
end subroutine import
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set(line)
character(len=*),intent(in)  :: line
character(len=:),allocatable :: temp
character(len=:),allocatable :: name
character(len=:),allocatable :: val
integer                      :: iend
! create a dictionary with character keywords, values, and value lengths
! using the routines for maintaining a list

  temp=adjustl(line)
  iend=merge(len(temp),index(temp,' '),index(temp,' ') == 0)
  name=adjustl(upper(temp(:iend)))

  if(name /= '')then
    if(len(temp) > iend)then
       val=temp(min(iend+1,len(temp)):)
       call check_name(name)
       if(val == ' ')val='1'
       call macro%set(name,val) ! insert and replace entries
    else
    endif
  else
       call stop_prep('e1c05d6d-550b-4535-92ec-819453d6c99f','incomplete set:',trim(G_SOURCE))
  endif

end subroutine set
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine expand_variables(line)
!@(#) brute force variable substitution. maybe add something like wordexp(3c) with command expansion only if --system?
! this is just to try the concept. Either use wordexp or an equivalent to replicate "here document" processing.
! note no automatic continuation of the line if it extends past allowed length, which for Fortran is currently 132 for free-format
! the way this is written it would do recursive substitution and does not know when there is just not a match
character(len=*)              :: line
character(len=:),allocatable  :: temp,search
integer                       :: i
integer                       :: j
integer                       :: ibug
character(len=4096)           :: scratch

if(index(line,'${') /= 0 .or. (G_underscore .and. index(line,'__') /= 0 ) )then
   write(scratch,'(i0)')G_file_dictionary(G_iocount)%line_number
   call set('LINE ' // scratch)
   call set('FILE ' // G_file_dictionary(G_iocount)%filename )
   call set('TIME ' // getdate('time'))
   call set('DATE ' // getdate('date'))
   call set('TIMESTAMP ' // getdate('cdate'))
   call set('PROCEDURE ' // 'PROCNAME')
   temp=trim(line)
   ibug=minval([size(macro%key),ubound(macro%key)])   ! print variable dictionary
   INFINITE: do i=1,len_trim(line)
      if(G_underscore.and.index(temp,'__') /= 0 )then
         temp=str_replace( temp,  '__LINE__',       '${LINE}',       ignorecase=.false.  )
         temp=str_replace( temp,  '__FILE__',       '${FILE}',       ignorecase=.false.  )
         temp=str_replace( temp,  '__TIME__',       '${TIME}',       ignorecase=.false.  )
         temp=str_replace( temp,  '__DATE__',       '${DATE}',       ignorecase=.false.  )
         temp=str_replace( temp,  '__TIMESTAMP__',  '${TIMESTAMP}',  ignorecase=.false.  )
      endif
      do j=1,ibug
         if(index(temp,'${') /= 0)then
            search='${'//trim(macro%key(j))//'}'
            temp=str_replace(temp,search,macro%value(j)(:macro%count(j)),ignorecase=.true.)
         else
            exit INFINITE
         endif
      enddo
   enddo INFINITE
   line=temp
endif

end subroutine expand_variables
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
!! but trying to use portable Fortran. If assume compiled by certain compilers could
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
character(len=G_var_len) :: val
integer           :: r
logical           :: file_exists
character(len=80) :: scratch

   call put( 'UNKNOWN=0' )
   call put( 'LINUX=1' )
   call put( 'MACOS=2' )
   call put( 'WINDOWS=3' )
   call put( 'CYGWIN=4' )
   call put( 'SOLARIS=5' )
   call put( 'FREEBSD=6' )
   call put( 'OPENBSD=7' )

   r = OS_UNKNOWN
   ! Check environment variable `OS`.
   val=get_env('OS')
   if ( index(val, 'Windows_NT') > 0) then
       r = OS_WINDOWS
   else
      ! Check environment variable `OSTYPE`.
      val=get_env('OSTYPE')
      if (val /= '') then
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
   if(r == OS_UNKNOWN)then
      inquire (file='/etc/os-release', exist=file_exists) ! Linux
      if (file_exists) r = OS_LINUX
      inquire (file='/usr/bin/sw_vers', exist=file_exists) ! macOS
      if (file_exists) r = OS_MACOS
      inquire (file='/bin/freebsd-version', exist=file_exists) ! FreeBSD
      if (file_exists) r = OS_FREEBSD
   endif
   scratch=' '
   write(scratch,'("OS=",i0)')r
   call put(scratch)
end subroutine get_os_type
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function ends_in(string) result(ending)
character(*), intent(in)  :: string
character(:), allocatable :: ending
integer                   :: n1
   n1=index(string,'.',back=.true.)
   if (n1 < 1 .or. n1 == len(string) ) then
       ending=''
   else
       ending=string(n1+1:)
   endif
end function ends_in
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine flushit()
integer :: ios
   flush(unit=stdout,iostat=ios)
   flush(unit=stderr,iostat=ios)
end subroutine flushit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine put(opts)                                 !@(#)expr_short(3f): call expr with just an expression
character(len=*),intent(in)  :: opts
character(len=G_var_len)     :: value
integer                      :: ierr
character(len=G_line_length) :: expression
   expression=upper(opts)
   call expr(expression,value,ierr,def=.true.)
   if(ierr /= 0) call stop_prep('efbbe046-065d-41ca-acef-115909b11bba','expression invalid:',trim(G_source))
end subroutine put
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module prep__internal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program prep                                              !@(#)prep(1f): preprocessor for Fortran/Fortran source code
use M_CLI2,    only : set_args, lget, rget, iget, sget
use M_strings, only : notabs, isdigit, switch, sep, lower
use M_io,      only : getname, basename
use M_attr,    only : attr, attr_mode
use prep__internal
implicit none
character(len=G_line_length) :: out_filename=''           ! output filename, default is stdout
character(len=1)             :: prefix                    ! directive prefix character
character(len=G_line_length) :: line                      ! working copy of input line
logical                      :: keeptabs=.false.          ! flag whether to retain tabs and carriage returns or not
integer                      :: ilast
integer                      :: ios
character(len=:),allocatable :: help_text(:)
character(len=:),allocatable :: version_text(:)
character(len=:),allocatable :: string
character(len=:),allocatable :: cmd
logical                      :: isscratch

   select case(lower(get_env('COLOR','plain')))
   case('ansi','yes','on','always');  call attr_mode(manner='ansi')  ! return to default mode
   case('raw');                       call attr_mode(manner='raw')   ! write as-is
   case('plain','no','off','never');  call attr_mode(manner='plain') ! write as plain text
   case default;                      call attr_mode(manner='plain') ! write as plain text
   end select

   cmd='&
   & -i " "               &
   & -D " "               &
   & -I " "               &
   & -o " "               &
   & --prefix 36          &
   & --keeptabs .false.   &
   & -d ignore            &
   & --help .false.       &
   & --verbose .false.    &
   & --system .false.     &
   & --version .false.    &
   & --crib .false.       &
   & --debug .false.      &
   & --underscore .false. &
   & --noenv .false.     &
   & --comment "'//get_env('PREP_COMMENT_STYLE','default')//'" &
   & --ident .false.      &
   & --width 1024         &
   & --start " "          &
   & --stop " "           &
   & --type auto          &
   & --lang "'//get_env('PREP_LANGUAGE','en')//'" &
   & '
   ! allow formatting comments for particular post-processors
   G_comment='! '
   !JSUkracken_comment=G_comment
   call setup(help_text,version_text)
   call set_args(cmd,help_text,version_text)                ! define command arguments, default values and crack command line
!  cpp>=========================================================================
   ! decide whether to act like cpp or not
   if(specified('i').or.size(unnamed) > 2)then
      G_cpp=.false.
   else
      if(size(unnamed) > 0)then
         if(exists(unnamed(1)))then
            G_cpp=.true.
         else
            G_cpp=.false.
         endif
      else
         G_cpp=.true.
      endif
   endif
!  cpp<=========================================================================
   string=adjustl(trim(sget('prefix')))
   if ( all( isdigit(switch(string)) ) ) then               ! if all characters are numeric digits
      prefix = char(iget('prefix'))                         ! assume this is an ADE
   else
      prefix(1:1) = trim(sget('prefix'))                    ! not a digit so not an ADE so assume a literal character
   endif

   G_inc_files=' '

   G_lang=sget('lang')                                      ! preferred message language
   G_ident=lget('ident')                                    ! write IDENT as comment or CHARACTER variable
   G_iwidth                   = iget('width')
   G_iwidth=max(0,G_iwidth)
   G_deed(1:1)                = trim(sget('d'))
   G_underscore               = lget('underscore')
   G_noenv                    = lget('noenv')

   out_filename(:G_line_length) = sget('o')

   if(G_cpp .and. out_filename == '' )then
      if(size(unnamed) == 2) out_filename=unnamed(2)
   endif

   if(out_filename == '')then                                    ! open output file
      G_iout=stdout
   elseif(out_filename == '@')then
      G_iout=stdout
      G_IHELP=stdout
   else
      G_iout=60
      G_IHELP=60
      open(unit=60,file=out_filename,iostat=ios,action='write')
      if(ios /= 0)then
         call stop_prep('e1f743a8-b1ad-45e3-943e-3e9e46e7af78','failed to open output file:',trim(out_filename))
      endif
   endif
   G_iout_init=G_iout

   if(lget('crib'))then
      call crib_help(stdout)
      stop
   endif
   G_debug=lget('debug')                              ! turn on debug mode for developer

   keeptabs=lget('keeptabs')
   G_verbose=lget('verbose')                          ! set flag for special mode where lines with @(#) are written to stderr
   if(G_verbose)then
      call write_err('+ verbose mode on ')
   endif
   G_comment_style=lower(sget('comment'))             ! allow formatting comments for particular post-processors
   G_system_on = lget('system')                       ! allow system commands on $SYSTEM directives
   if(G_system_on)then
      call put('SYSTEMON=.TRUE.')
   else
      call put('SYSTEMON=.FALSE.')
   endif
   G_extract_start0=''
   G_extract_stop0=''
   if(sget('type').eq.'')then
      G_extract_auto=.true.  ! auto mode where start and end are selected based on file suffix
      G_extract=.false.
      ! if these are set use them instead of auto mode
      G_extract_start=trim(sget('start'))
      G_extract_stop=trim(sget('stop'))
      G_extract_start0=G_extract_start
      G_extract_stop0=G_extract_stop
   else
      G_extract_auto=.false.
      G_extract=.true.
      select case(sget('type'))
      case('md','.md')
         G_extract_start='```fortran'
         G_extract_stop='```'
      case('markdownMML','.markdownMML','MML','.MML','mml','.mml')
         G_extract_start='^ *~~~~* *{: *lang=fortran *}[ ~]*$'
         G_extract_stop='^ *~~~~* *$'
      case('html','.html','htm','.htm')
         ! flaw is HTML is not case sensitive
         G_extract_start=' *<[xX][mM][pP]>'
         G_extract_stop=' *</[xX][mM][pP]>'
      case('tex')
         G_extract_start='\begin{minted}{Fortran}'
         G_extract_stop='\end{minted}'
      case('auto')
         G_extract_start=''
         G_extract_stop=''
         G_extract_auto=.true.
      case('none')
         G_extract_start=''
         G_extract_stop=''
         G_extract=.false.
      case default
         call stop_prep('61a446d7-7e3e-418a-a2df-cd50dc89148a','unknown type:',sget('type'))
      end select
   endif
   if(G_extract_start /= ''.or.G_extract_stop /= '')then
      G_extract=.true.
      if (getpat(trim(G_extract_start), G_pattern_start%pat) .eq. ERR) then
         call stop_prep('098da459-2255-44a1-8cdb-f4c9825488b8','Illegal start pattern ',G_extract_start)
      endif
      if (getpat(trim(G_extract_stop), G_pattern_stop%pat) .eq. ERR) then
         call stop_prep('f12d13a2-b397-4b59-a857-381994f47bc6','Illegal stop pattern ',G_extract_stop)
      endif
   endif

   call get_os_type()
!cpp>==============================================================================
   call defines()                                          ! define named variables declared on the command line
!<cpp==============================================================================
   call includes()                                         ! define include directories supplies on command line
!cpp>==============================================================================
   call opens()                                            ! convert input filenames into $include directives
!<cpp==============================================================================
   if(G_extract) call auto()

   READLINE: do                                            ! read loop to read input file
      read(G_file_dictionary(G_iocount)%unit_number,'(a)',end=7) line
      if(G_extract)then                                    ! in extract mode
         if (match(trim(line)//char(10), G_pattern_start%pat) .eq. YES) then ! start extracting
            G_extract_writeflag=.true.
            cycle READLINE
         elseif (match(trim(line)//char(10), G_pattern_stop%pat) .eq. YES .and. G_extract_writeflag) then ! stop extracting
            G_extract_writeflag=.false.
            cycle READLINE
         elseif(.not.G_extract_writeflag)then                   ! skip if not extracting
            cycle READLINE
         endif
      endif
      !TODO! should line count include skipped lines?
      G_io_total_lines=G_io_total_lines+1
      G_file_dictionary(G_iocount)%line_number=G_file_dictionary(G_iocount)%line_number+1

      if(keeptabs)then
         G_source=line
      else
         call notabs(line,G_source,ilast)                  ! expand tab characters and trim trailing ctrl-M from DOS files
      endif

      if(G_inparcel)then                                   ! do not expand lines stored in a parcel
      elseif(size(macro%key) /= 0)then                     ! expand variables if any variable is defined, else skip for efficiency
         call expand_variables(G_source)                   ! expand ${NAME} strings
      endif

      select case (line(1:1))                              ! special processing for lines starting with 'd' or 'D'
      case ('d','D')
         select case(G_deed(1:1))
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

      if (line(1:1) == prefix.and.line(2:2) /= '{') then   ! prefix must be in column 1 for conditional compile directive
         call cond()                                       ! process directive
      elseif (G_write) then                                ! if last conditional was true then write line
         call write_out(trim(G_source))                    ! write data line
      endif
      cycle

7     continue                                                      ! end of file encountered on input
      if(G_file_dictionary(G_iocount)%unit_number /= 5)then
         inquire(unit=G_file_dictionary(G_iocount)%unit_number,iostat=ios,named=isscratch)
         if(.not.isscratch.and.(G_file_dictionary(G_iocount)%unit_number > 0))then
            close(G_file_dictionary(G_iocount)%unit_number,iostat=ios)
         elseif(isscratch.or.(G_file_dictionary(G_iocount)%unit_number < -1))then
            rewind(unit=G_file_dictionary(G_iocount)%unit_number,iostat=ios)
         endif
      endif

      G_iocount=G_iocount-1
      if(G_scratch_lun /= -1)then
         ios=filedelete(G_scratch_file//'.out')
         G_scratch_lun=-1
      endif

      if(G_iocount < 1)exit
      call auto() ! if in auto mode determine strings for new file

   enddo READLINE

   if (G_nestl /= 0) then                                           ! check to make sure all if blocks are closed
      call stop_prep('6f7e0453-4c1d-4040-9c7d-3adf5112b692','block not closed in',' $IF')
   endif
   call print_comment_block()

   contains

subroutine auto()
   if(G_extract_auto)then
      select case(ends_in(G_file_dictionary(G_iocount)%filename) )
      case('md','.md')
         G_extract_start='```fortran'
         G_extract_stop='```'
      case('markdownMML','.markdownMML','MML','mml')
         G_extract_start='^ *~~~~* *{: *lang=fortran *}[ ~]*$'
         !NOT WORKING G_extract_start='^ *[~`][~`][~`][~`]* *{: *lang=fortran *} *[~`]* *'
         G_extract_stop='^ *~~~~* *$'
         !NOT WORKING G_extract_stop='^ *[~`][~`][~`][~`]* *$'
      case('tex')
         G_extract_start='\begin{minted}{Fortran}'
         G_extract_stop='\end{minted}'
      case('html','.html','htm','.htm')
         G_extract_start=' *<[xX][mM][pP]>'
         G_extract_stop=' *</[xX][mM][pP]>'
      case default
         G_extract_start=G_extract_start0
         G_extract_stop=G_extract_stop0
      end select
      if(G_extract_start == ''.and.G_extract_stop == '')then
         G_extract=.false.
      else
         G_extract=.true.
         if (getpat(trim(G_extract_start), G_pattern_start%pat) .eq. ERR) then
            call stop_prep('fd4cb3f6-1878-44bc-8ab6-f5ae33ef845e','Illegal start pattern ',G_extract_start)
         endif
         if (getpat(trim(G_extract_stop), G_pattern_stop%pat) .eq. ERR) then
            call stop_prep('cd883e90-894a-47ec-9c6f-644b99848ca6','Illegal stop pattern ',G_extract_stop)
         endif
      endif
   endif
end subroutine auto

logical function exists(filename) result(r)
character(len=*), intent(in) :: filename
    inquire(file=filename, exist=r)
end function

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end program prep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
