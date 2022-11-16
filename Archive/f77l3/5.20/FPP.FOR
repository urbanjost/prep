C     -----------------------------------------------------------------------
C
C                       Lahey Computer Systems, Inc.
C                                PO Box 6091
C                         Incline Village, NV 89450
C                            voice: 775-831-2500
C                              fax: 775-831-8123
C                        e-mail: support@lahey.com
C
C              Copyright(c) 1995 - 2005, Lahey Computer Systems, Inc.
C
C     -----------------------------------------------------------------------
C     CONDITIONAL COMPILATION PREPROCESSOR
C
C     The conditional compilation preprocessor translates a file containing
C     conditional compilation directives (conforming to the Lahey syntax
C     conventions) into a FORTRAN source file with no conditional compilation
C     directives.  Use the preprocessor before compiling programs with
C     non-Lahey compilers that do not support the directives' syntax.  Source
C     with conditional compilation directives does not have to be
C     preprocessed when compiled using Lahey FORTRAN.
C
C     The source for the "fpp" conditional compilation preprocessor,
C     "fpp.for", is written in standard FORTRAN 77 source code so that it can
C     be compiled and used on any target processor that supports FORTRAN 77.
C     Compile "fpp.for" with a FORTRAN compiler to create an executable that
C     will run on the target processor.  Once "fpp" is compiled and linked,
C     follow the steps below to use it.
C
C     Syntax:
C
C     fpp
C
C     Where:
C
C     "fpp" is the FORTRAN preprocessor.
C
C     The preprocessor displays the prompt:
C
C     INPUT NAME OF FILE TO PROCESS:
C
C     Enter the name (including extension) of the FORTRAN source file to
C     process.  This is the file containing conditional compilation
C     directives.
C
C     The preprocessor displays the prompt:
C
C     INPUT NAME OF FILE TO CREATE:
C
C     Enter a name for the FORTRAN source file to be created for the
C     compiler.  This will be the file without conditional compilation
C     directives.  Any extension is accepted.  Lahey FORTRAN uses the ".for"
C     extension for standard-format source files.
C
C     NOTE:
C
C     The filenames given in response to these prompts must not be the same.
C
C     The next prompt allows you to specify additional "$" directives or to
C     specify a response file containing additional "$" directives.  The
C     response filename must be preceded by an "@" sign.
C
C     NOTE:
C
C     The preprocessor does not interpret the Lahey compiler ".fig" file or
C     the /D1 compiler option.
C
C     The "$" directive prompt is:
C
C     INPUT ADDITIONAL $ DIRECTIVES, @FILENAME CONTAINING ADDITIONAL $
C     DIRECTIVES, OR CARRIAGE RETURN TO CONTINUE:
C
C     Enter the additional "$" directives, the name of the "$" directives
C     response file, and/or a carriage return.
C
C     Next, compile the processed source using your compiler.
C
C     NOTE:
C
C     If an error occurs, the error message is displayed automatically by the
C     preprocessor.
C
C LOGICALS
C            CONDOP - FLAG TO KEEP TRACK OF PREVIOUS WRITE FLAGS
C            DC     - FLAG TO DETERMINE WRITE FLAG
C            WRITE  - FLAG TO WRITE TO OUTPUT FILE
C
C INTEGERS
C            NESTL  - NEST LEVEL OF IF'S
C            NUMDEF - NUMBER OF DEFINED VARIABLES
C
C CHARACTERS
C            DEFVAL - DEFINED VALUES
C            DEFVAR - DEFINED VARIABLE NAMES
C            SOURCE - ORIGINAL SOURCE FILE LINE
C
      CHARACTER OFILE*12,NFILE*12,LETTER*1,LINE*80,SOURCE*80,DFILE*12
      LOGICAL WRITE
      COMMON /SOURCE/ SOURCE
      COMMON /WRITE/ NESTL, WRITE
      WRITE(*,'('' FORTRAN PreProcessor Version 1.02'',/,
     1          '' (C) Copyright 1990 through 1993; Lahey Computer'',
     2          '' Systems, Inc.'',/,'' All Rights Reserved'',//)')
1     WRITE(*,'('' INPUT NAME OF FILE TO PROCESS: '')')
      READ(*,'(A)') OFILE
      OPEN(1,FILE=OFILE,STATUS='OLD',ERR=8)
2     WRITE(*,'('' INPUT NAME OF FILE TO CREATE: '')')
      READ(*,'(A)') NFILE
      OPEN(2,FILE=NFILE,STATUS='NEW',ERR=9)
3     WRITE(*,'('' INPUT ADDITIONAL $ DIRECTIVES, @FILENAME CONTAINING''
     1  ,'' ADDITONAL $ DIRECTIVES'',/,
     2  '' OR CARRIAGE RETURN TO CONTINUE:  '')')
      READ(*,'(A)') SOURCE
      IF (SOURCE(1:1).EQ.' ') THEN
        GOTO 6
      ELSEIF (SOURCE(1:1).EQ.'$') THEN
        CALL COND
        GOTO 3
      ELSEIF (SOURCE(1:1).EQ.'@') THEN
        DFILE=SOURCE(2:13)
        OPEN(3,FILE=DFILE,STATUS='OLD',ERR=10)
4       READ(3,'(A)',END=5) SOURCE
        IF (SOURCE.EQ.' ') GOTO 4
        CALL COND
        GOTO 4
5       CLOSE(3)
        GOTO 3
      ELSE
        WRITE(*,'('' INVALID INPUT, TRY AGAIN '')')
        GOTO 3
      ENDIF
6     READ(1,'(A)',END=7) LINE
      SOURCE=LINE
      IF (LINE(1:1).EQ.'$') THEN
C $ MUST BE IN COLUMN 1 FOR CONDITIONAL COMPILE DIRECTIVE
        CALL COND
      ELSEIF (WRITE) THEN
C IF LAST CONDITIONAL WAS TRUE THAN WRITE LINE
	  WRITE(2,'(A)') LINE(:IBLANK(LINE,80))
      ENDIF
      GOTO 6
7     IF (NESTL.EQ.0) STOP
C CHECK TO MAKE SURE ALL IF BLOCKS ARE CLOSED
      WRITE(*,'('' FATAL - $IF BLOCK NOT CLOSED.'')')
      STOP
8     WRITE(*,'('' FILE '',A,'' DOES NOT EXIST, ENTER Y TO '',
     1  ''RESTART '')') OFILE(:IBLANK(OFILE,12))
      READ(*,'(A)') LETTER
      IF (LETTER.NE.'Y') STOP
      GOTO 1
9     WRITE(*,'('' FILE '',A,'' ALREADY EXISTS, OVERWRITE Y '')')
     1  NFILE(:IBLANK(NFILE,12))
      READ(*,'(A)') LETTER
      IF (LETTER.EQ.'Y') THEN
        OPEN(2,FILE=NFILE)
        GOTO 3
      ELSE
        GOTO 2
      ENDIF
10    WRITE(*,'('' FILE '',A,'' DOES NOT EXIST, ENTER Y TO '',
     1  ''REENTER '')') SOURCE(2:IBLANK(SOURCE,12))
      READ(*,'(A)') LETTER
      IF (LETTER.NE.'Y') STOP
      GOTO 3
      END
      FUNCTION IBLANK(NFILE,LENGTH)
      CHARACTER NFILE*(*)
C FIND LAST NON-BLANK CHARACTER
      DO 1 I=LENGTH,2,-1
1     IF (NFILE(I:I).NE.' ') GOTO 2
2     IBLANK=I
      END
      SUBROUTINE COND
      CHARACTER LINE*80, CONDIT(5)*2, SOURCE*80
	LOGICAL WRITE, EB
      COMMON /SOURCE/ SOURCE
      COMMON /WRITE/ NESTL, WRITE
	DATA CONDIT /'DE','UN','IF','EL','EN'/, NOELSE /0/, EB /.FALSE./
      LINE=SOURCE(2:)
      DO 1 I=1,IBLANK(LINE,80)
1     IF (ICHAR(LINE(I:I)).GE.97.AND.ICHAR(LINE(I:I)).LE.122)
     1  LINE(I:I)=CHAR(ICHAR(LINE(I:I))-32)
      CALL SPACE(LINE,80)
      IF (INDEX(LINE,'!').NE.0) THEN
        IF (INDEX(LINE,'!').EQ.2) GOTO 8
        LINE=LINE(:INDEX(LINE,'!')-1)
      ENDIF
      DO 2 I=1,5
2     IF (LINE(:2).EQ.CONDIT(I)) GOTO (3,4,5,6,7) I
      GOTO 8
3     IF (WRITE) CALL DEFINE(LINE)
      RETURN
4     IF (WRITE) CALL UNDEF(LINE)
      RETURN
5     CALL IF(LINE,NOELSE,EB)
      RETURN
6     CALL ELSE(LINE,NOELSE,EB)
      RETURN
7     CALL ENDIF(NOELSE,EB)
      RETURN
8     WRITE(*,'('' '',A//'' FATAL - UNKNOWN COMPILER DIRECTIVE.'')')
     1  SOURCE(:IBLANK(SOURCE,80))
      STOP
      END
      SUBROUTINE SPACE(LINE,LENGTH)
      CHARACTER LINE*(*), TEMP*80
      TEMP=' '
      IPOS=0
C REMOVE ALL BLANKS
      DO 1 I=1,LENGTH
        IF (ICHAR(LINE(I:I)).EQ.32) GOTO 1
        IPOS=IPOS+1
        TEMP(IPOS:IPOS)=LINE(I:I)
1     CONTINUE
      LINE=TEMP
      END
      SUBROUTINE DEFINE(LINE)
      PARAMETER (NUM=128)
      CHARACTER DEFVAL(NUM)*31,DEFVAR(NUM)*31,SOURCE*80,LINE*80,TEMP*80
      COMMON /NUMDEF/ NUMDEF
      COMMON /SOURCE/ SOURCE
      COMMON /VARVAL/ DEFVAL, DEFVAR
      LINE=LINE(7:)
C INCREMENT NUMBER OF DEFINED VARIABLES
      NUMDEF=NUMDEF+1
      IEQU=INDEX(LINE,'=')
      IF (LINE(1:1).EQ.' '.OR.IEQU.EQ.IBLANK(LINE,80)) GOTO 2
      IF (IEQU.GT.32) GOTO 3
      IF (IEQU.EQ.0) THEN
C IF NO = THEN VARIABLE ASSUMES VALUE OF 1
        READ(LINE,'(A)') DEFVAR(NUMDEF)
        LINE='1'
      ELSE
        READ(LINE(:IEQU-1),'(A)') DEFVAR(NUMDEF)
      ENDIF
      CALL NAME(DEFVAR(NUMDEF))
      IF (NUMDEF.NE.1) THEN
C TEST FOR REDEFINITION OF DEFINED NAME
        DO 1 J=1,NUMDEF-1
1       IF (DEFVAR(NUMDEF).EQ.DEFVAR(J)) GOTO 4
      ENDIF
      TEMP=LINE(IEQU+1:)
      CALL PARENS(TEMP)
      IF (IEQU.EQ.0) THEN
        LINE=TEMP
      ELSE
        LINE=LINE(:IEQU)//TEMP
      ENDIF
      CALL MATH(LINE,IEQU+1,IBLANK(LINE,80))
      CALL DOOP(LINE,IEQU+1,IBLANK(LINE,80))
      CALL LOGIC(LINE,IEQU+1,IBLANK(LINE,80))
      CALL GETVAL(LINE,IEQU+1,IBLANK(LINE,80),DEFVAL(NUMDEF))
      RETURN
2     WRITE(*,'('' '',A//'' FATAL - INCOMPLETE STATEMENT.'')')
     1  SOURCE(:IBLANK(SOURCE,80))
      STOP
3     WRITE(*,'('' '',A,//,'' FATAL - MISPELLING OR NAME LENGTH '',
     1  ''EXCEEDS 31 CHARACTERS.'')')  SOURCE(:IBLANK(SOURCE,80))
      STOP
4     WRITE(*,'('' '',A,//,'' FATAL - REDEFINITION OF DEFINED NAME '',
     1  ''INVALID.'')') SOURCE(:IBLANK(SOURCE,80))
      STOP
      END
      SUBROUTINE NAME(LINE)
      CHARACTER LINE*31, SOURCE*80
      COMMON /SOURCE/ SOURCE
C TEST FOR LEGAL FORTRAN CHARACTERS
      IF (LINE(1:1).LT.'A'.OR.LINE(1:1).GT.'Z') GOTO 2
      DO 1 I=2,IBLANK(LINE,31)
1     IF (LINE(I:I).NE.'$'.AND.LINE(I:I).NE.'_'.AND.(LINE(I:I).LT.'A'
     1  .OR.LINE(I:I).GT.'Z').AND.(LINE(I:I).LT.'0'
     2  .OR.LINE(I:I).GT.'9')) GOTO 2
      RETURN
2     WRITE(*,'('' '',A//'' FATAL - SYNTAX ERROR.'')')
     1  SOURCE(:IBLANK(SOURCE,80))
      STOP
      END
      SUBROUTINE GETVAL(LINE,IPOS1,IPOS2,VALUE)
      PARAMETER (NUM=128)
      CHARACTER DEFVAL(NUM)*31, DEFVAR(NUM)*31, SOURCE*80, LINE*80,
     1  VALUE*31, TEMP*80
      COMMON /NUMDEF/ NUMDEF
      COMMON /SOURCE/ SOURCE
      COMMON /VARVAL/ DEFVAL, DEFVAR
      TEMP=LINE(IPOS1:IPOS2)
      IF (TEMP(1:1).EQ.' ') GOTO 4
      IF (TEMP(1:1).GE.'A'.AND.TEMP(1:1).LE.'Z') THEN
C IF VARIABLE NAME
        READ(TEMP,'(A31)') VALUE
C FIND DEFINED PARAMETER
        DO 1 I=1,NUMDEF
1       IF (DEFVAR(I).EQ.VALUE) GOTO 2
2       IF (I.GT.NUMDEF) GOTO 5
        VALUE=DEFVAL(I)
      ELSE
C READ IN NUMERIC VALUE
        READ(TEMP,'(I11)',ERR=3) IVAL
        WRITE(VALUE,'(I11)') IVAL
        RETURN
C READ IN LOGICAL VALUE
3       READ(TEMP,'(A)') VALUE
        IF (VALUE.NE.'.FALSE.'.AND.VALUE.NE.'.TRUE.') GOTO 7
        IF (TEMP(1:1).GE.'A') GOTO 6
      ENDIF
      RETURN
4     WRITE(*,'('' '',A,//,'' FATAL - INCOMPLETE STATEMENT.'')')
     1  SOURCE(:IBLANK(SOURCE,80))
      STOP
5     WRITE(*,'('' '',A,//,'' FATAL - UNDEFINED PARAMETER.'')')
     1  SOURCE(:IBLANK(SOURCE,80))
      STOP
6     WRITE(*,'('' '',A,//,'' FATAL - $DEFINE VALUE MUST BE AN '',
     1  ''INTEGER OR LOGICAL CONSTANT.'')') SOURCE(:IBLANK(SOURCE,80))
      STOP
7     WRITE(*,'('' '',A,//,'' FATAL - SYNTAX ERROR.'')')
     1  SOURCE(:IBLANK(SOURCE,80))
      END
      SUBROUTINE UNDEF(LINE)
      PARAMETER (NUM=128)
      CHARACTER DEFVAL(NUM)*31, DEFVAR(NUM)*31, SOURCE*80, LINE*80
      COMMON /NUMDEF/ NUMDEF
      COMMON /SOURCE/ SOURCE
      COMMON /VARVAL/ DEFVAL, DEFVAR
      LINE=LINE(9:)
      IF (IBLANK(LINE,80).EQ.0) GOTO 4
C FIND DEFINED VARIABLE TO BE UNDEFINED
      DO 1 I=1,NUMDEF
1     IF (DEFVAR(I).EQ.LINE) GOTO 2
2     IF (I.GT.NUMDEF) RETURN
      DO 3 J=1,NUMDEF-1
        DEFVAR(J)=DEFVAR(J+1)
3     DEFVAL(J)=DEFVAL(J+1)
C DECREMENT NUMBER OF DEFINED VARIABLES
      NUMDEF=NUMDEF-1
      RETURN
4     WRITE(*,'('' '',A//'' FATAL - INCOMPLETE STATEMENT.'')')
      STOP
      END
	SUBROUTINE IF(LINE,NOELSE,EB)
      PARAMETER (NUM=128)
	LOGICAL CONDOP(0:8), DC, WRITE, EB
      CHARACTER DEFVAL(NUM)*31, DEFVAR(NUM)*31, SOURCE*80, LINE*80,
     1  VALUE*31
      COMMON /CONDOP/ CONDOP
      COMMON /DC/ DC
      COMMON /NUMDEF/ NUMDEF
      COMMON /SOURCE/ SOURCE
      COMMON /VARVAL/ DEFVAL, DEFVAR
      COMMON /WRITE/ NESTL, WRITE
      LINE=LINE(3:)
      NOELSE=0
      WRITE=.FALSE.
C INCREMENT IF NEST LEVEL
      NESTL=NESTL+1
      IF (NESTL.GT.8) GOTO 4
7     IF (INDEX(LINE,'DEFINED(').NE.0) THEN
        CALL IFDEF(LINE,INDEX(LINE,'DEFINED('))
        CALL SPACE(LINE,80)
        GOTO 7
      ENDIF
      CALL PARENS(LINE)
      IF (INDEX(LINE,'.').EQ.0) THEN
C IF VARIABLE ONLY
        IF (LINE(1:1).GE.'A'.AND.LINE(1:1).LE.'Z') THEN
C IF VARIABLE NAME
          READ(LINE,'(A31)') VALUE
C FIND DEFINED PARAMETER
          DO 1 I=1,NUMDEF
1         IF (DEFVAR(I).EQ.VALUE) GOTO 2
2         IF (I.GT.NUMDEF) GOTO 5
          READ(DEFVAL(I),'(L4)',ERR=6) DC
        ELSE
          GOTO 6
        ENDIF
      ELSE
        CALL EVAL(LINE)
      ENDIF
C CHECK TO MAKE SURE PREVIOUS IF WAS TRUE
3     IF (.NOT.DC.OR..NOT.CONDOP(NESTL-1).OR.EB) RETURN
      CONDOP(NESTL)=.TRUE.
      WRITE=CONDOP(NESTL)
      RETURN
4     WRITE(*,'('' '',A,//,'' ABORT - $IF BLOCK NESTING TOO DEEP, '',
     1  ''LIMITED TO 8 LEVELS.'')') SOURCE(:IBLANK(SOURCE,80))
      STOP
5     WRITE(*,'('' '',A,//,'' FATAL - UNDEFINED PARAMETER.'')')
     1  SOURCE(:IBLANK(SOURCE,80))
      STOP
6     WRITE(*,'('' '',A,//,'' FATAL - CONSTANT LOGICAL EXPRESSION '',
     1  ''REQUIRED.'')') SOURCE(:IBLANK(SOURCE,80))
      STOP
      END
      SUBROUTINE IFDEF(LINE,IPOS1)
      PARAMETER (NUM=128)
      LOGICAL DC
      CHARACTER DEFVAL(NUM)*31, DEFVAR(NUM)*31, SOURCE*80, LINE*80,
     1  IFVAR*31, NEWL*80
      COMMON /DC/ DC
      COMMON /NUMDEF/ NUMDEF
      COMMON /SOURCE/ SOURCE
      COMMON /VARVAL/ DEFVAL, DEFVAR
      NEWL=LINE(IPOS1+7:)
      IF (IBLANK(NEWL,80).EQ.1.OR.INDEX(NEWL,')').EQ.0.OR.
     1  INDEX(NEWL,')').EQ.2) GOTO 3
      IF (INDEX(NEWL,')').GT.33) GOTO 4
      READ(NEWL(2:INDEX(NEWL,')')-1),'(A31)') IFVAR
      IF (NEWL(2:2).LT.'A'.OR.NEWL(2:2).GT.'Z') GOTO 5
      DO 1 I=3,INDEX(NEWL,')')-1
1     IF (NEWL(I:I).NE.'$'.AND.NEWL(I:I).NE.'_'.AND.(NEWL(I:I).LT.'A'
     1  .OR.NEWL(I:I).GT.'Z').AND.(NEWL(I:I).LT.'0'
     2  .OR.NEWL(I:I).GT.'9')) GOTO 5
      DC=.FALSE.
      LINE(IPOS1:IPOS1+6+INDEX(NEWL,')'))='.FALSE.'
      DO 2 I=1,NUMDEF
        IF (DEFVAR(I).EQ.IFVAR) THEN
          DC=.TRUE.
          LINE(IPOS1:IPOS1+6+INDEX(NEWL,')'))='.TRUE.'
          RETURN
        ENDIF
2     CONTINUE
      RETURN
3     WRITE(*,'('' '',A,//,'' FATAL - INCOMPLETE STATEMENT.'')')
     1  SOURCE(:IBLANK(SOURCE,80))
      STOP
4     WRITE(*,'('' '',A,//,'' FATAL - MISSPELLING OR NAME LENGTH '',
     1  ''EXCEEDS 31 CHARACTERS.'')') SOURCE(:IBLANK(SOURCE,80))
      STOP
5     WRITE(*,'('' '',A,//,'' FATAL - CONSTANT LOGICAL EXPRESSION '',
     1  ''REQUIRED.'')') SOURCE(:IBLANK(SOURCE,80))
      STOP
      END
	SUBROUTINE ELSE(LINE,NOELSE,EB)
	LOGICAL CONDOP(0:8), WRITE, EB
      CHARACTER LINE*80, SOURCE*80
      COMMON /CONDOP/ CONDOP
      COMMON /SOURCE/ SOURCE
      COMMON /WRITE/ NESTL, WRITE
      IF (NOELSE.EQ.1.OR.NESTL.EQ.0) GOTO 1
C TEST FOR ELSE INSTEAD OF ELSEIF
      IF (IBLANK(LINE,80).EQ.4) NOELSE=1
      IF (.NOT.CONDOP(NESTL-1)) RETURN
C IF WAS TRUE SO IGNORE ELSE
	EB=.FALSE.
      IF (CONDOP(NESTL)) THEN
	  EB=.TRUE.
	  WRITE=.FALSE.
      ELSEIF (IBLANK(LINE,80).NE.4) THEN
C ELSEIF DETECTED
C DECREASE IF LEVEL BECAUSE IT WILL BE INCREMENTED IN SUBROUTINE IF
        NESTL=NESTL-1
        LINE=LINE(5:)
	  CALL IF(LINE,NOELSE,EB)
      ELSE
C ELSE DETECTED
        CONDOP(NESTL)=.TRUE.
        WRITE=.TRUE.
      ENDIF
      RETURN
1     WRITE(*,'('' '',A,//,'' FATAL - MISPLACED $ELSE OR $ELSEIF '',
     1  ''DIRECTIVE.'')') SOURCE(:IBLANK(SOURCE,80))
      STOP
      END
	SUBROUTINE ENDIF(NOELSE,EB)
	LOGICAL CONDOP(0:8), WRITE, EB
      CHARACTER SOURCE*80
      COMMON /CONDOP/ CONDOP
      COMMON /SOURCE/ SOURCE
      COMMON /WRITE/ NESTL, WRITE
C DECREASE IF LEVEL
      NESTL=NESTL-1
C RESET ELSE LEVEL
      NOELSE=0
	EB=.NOT.CONDOP(NESTL+1)
	WRITE=.NOT.EB
	CONDOP(NESTL+1)=.FALSE.
	IF (NESTL.EQ.0) THEN
	  WRITE=.TRUE.
	  EB=.FALSE.
	ENDIF
	RETURN
1     WRITE(*,'('' '',A//'' FATAL - MISPLACED $ENDIF DIRECTIVE.'')')
     1  SOURCE(:IBLANK(SOURCE,80))
      STOP
      END
      SUBROUTINE PARENS(LINE)
      CHARACTER LINE*80, SOURCE*80
          COMMON /SOURCE/ SOURCE
    1     IF (INDEX(LINE,')').NE.0) THEN
C CLOSING PARENS FOUND
C FIND INNER MOST SET OF PARENS
            DO 2 I=INDEX(LINE,')'),1,-1
    2       IF (LINE(I:I).EQ.'(') GOTO 3
    3       IF (I.EQ.0) GOTO 4
            CALL MATH(LINE,I+1,INDEX(LINE,')')-1)
            CALL DOOP(LINE,I+1,INDEX(LINE,')')-1)
            CALL LOGIC(LINE,I+1,INDEX(LINE,')')-1)
            IF (I.EQ.1.AND.INDEX(LINE,')').EQ.IBLANK(LINE,80)) THEN
C REWRITE LINE AFTER NO MORE PARENS
          LINE=LINE(I+1:INDEX(LINE,')')-1)
        ELSEIF (I.EQ.1) THEN
C REWRITE LINE AFTER FIRST SET OF PARENS
          LINE=LINE(2:INDEX(LINE,')')-1)//LINE(INDEX(LINE,')')+1:)
        ELSEIF (INDEX(LINE,')').EQ.IBLANK(LINE,80)) THEN
C REWRITE LINE AFTER LAST SET OF PARENS ON LINE
          IF (LINE(I+1:I+1).EQ.'-'.AND.INDEX('*/+-',LINE(I-1:I-1)).NE.0)
     1       THEN
            DO 5 J=I-2,1,-1
5           IF (INDEX('*/+-',LINE(J:J)).NE.0) GOTO 6
6           IF (J.EQ.I-2) THEN
              PRINT*,'1**(-1) NOT IMPLEMENTED YET'
              STOP
            ENDIF
            GOTO (7,7,8,9) INDEX('*/+-',LINE(I-1:I-1))
7           IF (J.EQ.0) THEN
              LINE='-'//LINE(:I-1)//LINE(I+2:INDEX(LINE,')')-1)
            ELSE
              LINE=LINE(:J)//'(-'//LINE(J+1:I-1)//
     1             LINE(I+2:INDEX(LINE,')'))
            ENDIF
            GOTO 10
8           LINE=LINE(:I-2)//'-'//LINE(I+2:INDEX(LINE,')')-1)
            GOTO 10
9           LINE=LINE(:I-2)//'+'//LINE(I+2:INDEX(LINE,')')-1)
            GOTO 10
          ELSE
            LINE=LINE(:I-1)//LINE(I+1:INDEX(LINE,')')-1)
          ENDIF
       ELSEIF (LINE(I+1:I+1).EQ.'-'.AND.
     1      INDEX('*/+-',LINE(I-1:I-1)).NE.0) THEN
            DO 15 J=I-2,1,-1
15          IF (INDEX('*/+-',LINE(J:J)).NE.0) GOTO 16
16          IF (J.EQ.I-2) THEN
              PRINT*,'1**(-1) NOT IMPLEMENTED YET'
              STOP
            ENDIF
            GOTO (17,17,18,19) INDEX('*/+-',LINE(I-1:I-1))
17          IF (J.EQ.0) THEN
              LINE='-'//LINE(:I-1)//LINE(I+2:INDEX(LINE,')')-1)//
     1             LINE(INDEX(LINE,')')+1:)
            ELSE
              LINE=LINE(:J)//'(-'//LINE(J+1:I-1)//
     1             LINE(I+2:INDEX(LINE,')'))//LINE(INDEX(LINE,')')+1:)
            ENDIF
            GOTO 10
18           LINE=LINE(:I-2)//'-'//LINE(I+2:INDEX(LINE,')')-1)//
     1            LINE(INDEX(LINE,')')+1:)
            GOTO 10
19           LINE=LINE(:I-2)//'+'//LINE(I+2:INDEX(LINE,')')-1)//
     1             LINE(INDEX(LINE,')')+1:)
            GOTO 10
        ELSE
          LINE=LINE(:I-1)//LINE(I+1:INDEX(LINE,')')-1)//
     1         LINE(INDEX(LINE,')')+1:)
        ENDIF
10      CALL SPACE(LINE,80)
        GOTO 1
      ELSEIF (INDEX(LINE,'(').NE.0) THEN
        GOTO 4
      ENDIF
      RETURN
4     WRITE(*,'('' '',A,//,'' FATAL - CONSTANT LOGICAL EXPRESSION '',
     1  ''REQUIRED.'')') SOURCE(:IBLANK(SOURCE,80))
      STOP
      END
      SUBROUTINE MATH(LINE,IPOS1,IPOS2)
      CHARACTER LINE*80, NEWL*80
      NEWL=LINE(IPOS1:IPOS2)
      I=1
31    J=INDEX(NEWL(I:),'.')
      IF (J.NE.0.AND.J.NE.1) THEN
        CALL DOMATH(NEWL(I:J+I-2),J-1)
        I=I+J
        GOTO 31
      ELSEIF (J.EQ.1) THEN
        I=I+1
        GOTO 31
      ELSE
        CALL DOMATH(NEWL(I:),IPOS2-I+1)
      ENDIF
      LINE(IPOS1:IPOS2)=NEWL
      CALL SPACE(LINE,80)
      END
      SUBROUTINE DOMATH(LINE,IPOS2)
      CHARACTER LINE*(*), SOURCE*80, OPS(3)*2, TEMP*11, NEWL*80
      COMMON /SOURCE/ SOURCE
      DATA OPS /'**','*/','+-'/
      IF (IPOS2.EQ.0) RETURN
      MINUS1=1
      NEWL=LINE(:IPOS2)
      DO 99 NUMOP=1,3
1       I=INDEX(NEWL,OPS(NUMOP))
        IF (NUMOP.NE.1) THEN
          I=INDEX(NEWL,OPS(NUMOP)(1:1))
          J=INDEX(NEWL,OPS(NUMOP)(2:2))
          I=MAX(I,J)
          IF (I*J.NE.0) I=MIN(I,J)
        ENDIF
        IF (I.EQ.0) GOTO 99
        LEN=1
        IF (NUMOP.EQ.1) LEN=2
        IF (I.EQ.IBLANK(NEWL,80)) GOTO 999
        IF (I.EQ.1.AND.NUMOP.NE.3) GOTO 997
        IF (I.EQ.1.AND.NUMOP.EQ.3) GOTO 17
        IF (INDEX('*/+-',NEWL(I-1:I-1)).NE.0.OR.
     1      INDEX('*/+-',NEWL(I+LEN:I+LEN)).NE.0) GOTO 997
17      I1=0
        IF (I.EQ.1.AND.NUMOP.EQ.3) GOTO 18
        DO 2 J=I-1,1,-1
          IF (INDEX('*/+-.',NEWL(J:J)).EQ.0) GOTO 2
          GOTO 3
2       CONTINUE
3       IF (J.EQ.I-1.AND.J.NE.1) GOTO 18
        I1=IVAL(NEWL,J+1,I-1)
18      DO 4 L=I+IBLANK(OPS(NUMOP),2),IBLANK(NEWL,80)
          IF (INDEX('*/+-.',NEWL(L:L)).EQ.0) GOTO 4
          GOTO 5
4       CONTINUE
5       I2=IVAL(NEWL,I+LEN,L-1)
        IF (NUMOP.EQ.1) THEN
          I1=I1**I2*MINUS1
        GOTO 16
        ENDIF
        GOTO (12,13,14,15) INDEX('*/+-',NEWL(I:I))
12      I1=I1*I2*MINUS1
        GOTO 16
13      I1=I1/I2*MINUS1
        GOTO 16
14      IF (I1.NE.0) THEN
          I1=I1*MINUS1+I2
        ELSE
          I1=I1+I2*MINUS1
        ENDIF
        GOTO 16
15      IF (I1.NE.0) THEN
          I1=I1*MINUS1-I2
        ELSE
          I1=I1-I2*MINUS1
        ENDIF
16      IF (I1.LE.0) THEN
          IF (J.EQ.I-1.AND.J.NE.1) THEN
            MINUS1=-1
            I1=ABS(I1)
            LOC=J+1
            NEWL(J+1:J+1)=' '
            L=L-1
            CALL SPACE(NEWL,80)
          ELSEIF (I.EQ.1.AND.NUMOP.EQ.3) THEN
            MINUS1=-1
            I1=ABS(I1)
            LOC=I
            NEWL(J:J)=' '
            L=L-1
            J=J-1
            CALL SPACE(NEWL,80)
          ELSE
            MINUS1=1
          ENDIF
        ELSE
          MINUS1=1
        ENDIF
30      WRITE(TEMP,'(I11)') I1
        CALL SPACE(TEMP,11)
        IF (J.EQ.0.AND.L.GT.IBLANK(NEWL,80)) THEN
          NEWL=TEMP(:IBLANK(TEMP,11))
          GOTO 99
        ELSEIF (J.EQ.0) THEN
          NEWL=TEMP(:IBLANK(TEMP,11))//NEWL(L:)
        ELSEIF (L.GT.IBLANK(NEWL,80)) THEN
          NEWL=NEWL(:J)//TEMP(:IBLANK(TEMP,11))
        ELSE
          NEWL=NEWL(:J)//TEMP(:IBLANK(TEMP,11))//NEWL(L:)
        ENDIF
        GOTO 1
99    CONTINUE
      IF (MINUS1.EQ.-1.AND.(LOC.EQ.0.OR.LOC.EQ.1)) THEN
        NEWL='-'//NEWL
      ELSEIF (MINUS1.EQ.-1.AND.LOC.NE.1) THEN
        NEWL=NEWL(:LOC-1)//'-'//NEWL(LOC:)
      ENDIF
      LINE(:IPOS2)=NEWL(:IBLANK(NEWL,80))
      RETURN
997   WRITE(*,'('' '',A//'' FATAL - SYNTAX ERROR.'')')
     1  SOURCE(:IBLANK(SOURCE,80))
      STOP
999   WRITE(*,'('' '',A//'' FATAL - INCOMPLETE STATEMENT.'')')
     1  SOURCE(:IBLANK(SOURCE,80))
      STOP
      END
      SUBROUTINE DOOP(LINE,IPOS1,IPOS2)
      LOGICAL DC
      COMMON /DC/ DC
      CHARACTER LINE*80, OPS(6)*4, VAL1*31, VAL2*31, TEMP*7, NEWL*80
      DATA OPS /'.EQ.','.NE.','.GE.','.GT.','.LE.','.LT.'/
      DC=.FALSE.
      NEWL=LINE(IPOS1:IPOS2)
      DO 14 I=1,6
15      IF (INDEX(NEWL,OPS(I)).NE.0) THEN
          DO 16 J=INDEX(NEWL,OPS(I))-1,1,-1
16        IF (NEWL(J:J).EQ.'.') GOTO 17
17        CALL GETVAL(NEWL,J+1,INDEX(NEWL,OPS(I))-1,VAL1)
          DO 18 K=INDEX(NEWL,OPS(I))+4,IBLANK(NEWL,80)
18        IF (NEWL(K:K).EQ.'.') GOTO 19
19        CALL GETVAL(NEWL,INDEX(NEWL,OPS(I))+4,K-1,VAL2)
          GOTO (5,6,7,8,9,10) I
5         IF (VAL1.EQ.VAL2) DC=.TRUE.
          GOTO 13
6         IF (VAL1.NE.VAL2) DC=.TRUE.
          GOTO 13
7         IF (VAL1.GE.VAL2) DC=.TRUE.
          GOTO 13
8         IF (VAL1.GT.VAL2) DC=.TRUE.
          GOTO 13
9         IF (VAL1.LE.VAL2) DC=.TRUE.
          GOTO 13
10        IF (VAL1.LT.VAL2) DC=.TRUE.
13        TEMP='.FALSE.'
          IF (DC) TEMP='.TRUE.'
          CALL REWRIT(NEWL,TEMP(:IBLANK(TEMP,7)),J,J,K,K)
          CALL SPACE(NEWL,80)
          GOTO 15
        ENDIF
14    CONTINUE
      IF (IPOS1.NE.1) THEN
        LINE=LINE(:IPOS1-1)//NEWL(:IBLANK(NEWL,80))//LINE(IPOS2+1:)
      ELSE
        LINE=NEWL(:IBLANK(NEWL,80))//LINE(IPOS2+1:)
      ENDIF
      CALL SPACE(LINE,80)
      END
      SUBROUTINE LOGIC(LINE,IPOS1,IPOS2)
      LOGICAL DC, ONE, TWO, TRUFAL
      COMMON /DC/ DC
      CHARACTER LINE*(*), OPS(5)*6, TEMP*7, NEWL*80
      DATA OPS /'.NOT.','.AND.','.OR.','.EQV.','.NEQV.'/
      NEWL=LINE(IPOS1:IPOS2)
      DO 14 I=1,3
20      LEN=5
        IF (I.EQ.3) LEN=4
        IF (INDEX(NEWL,OPS(I)(:IBLANK(OPS(I),6))).EQ.0) GOTO 14
        I1=INDEX(NEWL,OPS(I)(:IBLANK(OPS(I),6)))-1
        J=I1+1
        LEN1=0
        IF (I.EQ.1) GOTO 11
        DO 16 J=I1,1,-1
          DO 16 K=1,5
          LEN1=5
          IF (K.EQ.3) LEN1=4
16      IF (INDEX(NEWL(J:I1),OPS(K)(:IBLANK(OPS(K),6))).NE.0) GOTO 17
17      IF (J.EQ.0) LEN1=1
        ONE=TRUFAL(NEWL,J+LEN1,I1)
11      DO 18 L=I1+LEN,IBLANK(NEWL,80)
          DO 18 K=1,5
          LEN2=5
          IF (K.EQ.3) LEN2=4
18      IF (INDEX(NEWL(I1+LEN:L),OPS(K)(:IBLANK(OPS(K),6))).NE.0)
     1    GOTO 19
19      IF (L.GT.IBLANK(NEWL,80)) LEN2=0
        TWO=TRUFAL(NEWL,I1+LEN+1,L-LEN2)
        GOTO (21,22,23) I
21      DC=.NOT.TWO
        GOTO 15
22      DC=ONE.AND.TWO
        GOTO 15
23      DC=ONE.OR.TWO
15      TEMP='.FALSE.'
        IF (DC) TEMP='.TRUE.'
        CALL REWRIT(NEWL,TEMP(:IBLANK(TEMP,7)),J,J+LEN1-1,L,L-LEN2+1)
        GOTO 20
14    CONTINUE
24    IEQV=INDEX(NEWL,'.EQV.')
      INEQV=INDEX(NEWL,'.NEQV')
      IF (IEQV*INEQV.EQ.0.AND.IEQV.NE.INEQV) THEN
        IOP=MAX(IEQV,INEQV)
      ELSEIF (IEQV.NE.0) THEN
        IOP=MIN(IEQV,INEQV)
      ELSEIF (IPOS1.EQ.1) THEN
        LINE=NEWL(:IBLANK(NEWL,80))//LINE(IPOS2+1:)
        RETURN
      ELSE
        LINE=LINE(:IPOS1-1)//NEWL(:IBLANK(NEWL,80))//LINE(IPOS2+1:)
        RETURN
      ENDIF
      LEN=5
      IF (INDEX(NEWL,'.EQV.').NE.IOP) LEN=6
      DO 26 J=IOP-1,1,-1
26    IF (NEWL(J:J+1).EQ.'V.') GOTO 27
27    IF (J.EQ.0) LEN1=1
      ONE=TRUFAL(NEWL,J+LEN1,IOP-1)
      DO 28 L=IOP+LEN,IBLANK(NEWL,80)
28    IF (NEWL(L:L+1).EQ.'.E'.OR.NEWL(L:L+1).EQ.'.N') GOTO 29
29    IF (L.GT.IBLANK(NEWL,80)) LEN2=0
      TWO=TRUFAL(NEWL,IOP+LEN,L+LEN2)
      DC=ONE.EQV.TWO
      IF (LEN.NE.5) DC=ONE.NEQV.TWO
      TEMP='.FALSE.'
      IF (DC) TEMP='.TRUE.'
      CALL REWRIT(NEWL,TEMP(:IBLANK(TEMP,7)),J,J+LEN1-1,L,L-LEN2+1)
      GOTO 24
      END
      SUBROUTINE EVAL(LINE)
      LOGICAL DC
      COMMON /DC/ DC
      CHARACTER LINE*80, VALUE*7
      CALL PARENS(LINE)
      CALL MATH(LINE,1,IBLANK(LINE,80))
      CALL DOOP(LINE,1,IBLANK(LINE,80))
      CALL LOGIC(LINE,1,IBLANK(LINE,80))
      READ(LINE,'(A7)') VALUE
      IF (VALUE.NE.'.TRUE.'.AND.VALUE.NE.'.FALSE.') GOTO 1
      READ(VALUE,'(L4)') DC
      RETURN
1     PRINT*,'ERROR'
      STOP
      END
      FUNCTION IVAL(LINE,IPOS1,IPOS2)
      PARAMETER (NUM=128)
      CHARACTER DEFVAL(NUM)*31, DEFVAR(NUM)*31, LINE*(*), VALUE*11
      COMMON /NUMDEF/ NUMDEF
      COMMON /VARVAL/ DEFVAL, DEFVAR
      IF (LINE(IPOS1:IPOS1).GE.'A'.AND.LINE(IPOS1:IPOS1).LE.'Z') THEN
        READ(LINE(IPOS1:IPOS2),'(A11)') VALUE
        DO 2 I=1,NUMDEF
2       IF (DEFVAR(I).EQ.VALUE) GOTO 3
3       IF (I.GT.NUMDEF) GOTO 999
        READ(DEFVAL(I),'(I11)',ERR=998) IVAL
      ELSE
        READ(LINE(IPOS1:IPOS2),'(I11)',ERR=998) IVAL
      ENDIF
      RETURN
998   PRINT*,'MUST BE INTEGER'
      STOP
999   PRINT*,'FATAL - UNDEFINED PARAMETER.'
      STOP
      END
      LOGICAL FUNCTION TRUFAL(LINE,IPOS1,IPOS2)
      PARAMETER (NUM=128)
      CHARACTER DEFVAL(NUM)*31, DEFVAR(NUM)*31, LINE*80, VALUE*31,
     1  SOURCE*80
      COMMON /NUMDEF/ NUMDEF
      COMMON /SOURCE/ SOURCE
      COMMON /VARVAL/ DEFVAL, DEFVAR
      TRUFAL=.FALSE.
      READ(LINE(IPOS1:IPOS2),'(A)') VALUE
      IF (VALUE.NE.'.FALSE.'.AND.VALUE.NE.'.TRUE.') GOTO 1
      IF (VALUE.EQ.'.TRUE.') TRUFAL=.TRUE.
      RETURN
1     DO 2 I=1,NUMDEF
2     IF (DEFVAR(I).EQ.VALUE) GOTO 3
3     IF (I.GT.NUMDEF.AND.INDEX(VALUE,'.').NE.0) GOTO 4
      IF (I.GT.NUMDEF) GOTO 5
      READ(DEFVAL(I),'(L4)',ERR=4) TRUFAL
      RETURN
4     WRITE(*,'('' '',A,//,'' FATAL - CONSTANT LOGICAL EXPRESSION '',
     1  ''REQUIRED.'')') SOURCE(:IBLANK(SOURCE,80))
      STOP
5     WRITE(*,'('' '',A,//,'' FATAL - UNDEFINED PARAMETER.'')')
     1  SOURCE(:IBLANK(SOURCE,80))
      STOP
      END
      SUBROUTINE REWRIT(LINE,TEMP,J,J1,L,L1)
      CHARACTER LINE*80, TEMP*(*)
      IF (J.EQ.0.AND.L.GT.IBLANK(LINE,80)) THEN
C DONE
        LINE=TEMP
      ELSEIF (J.EQ.0) THEN
C FIRST ITEM
        LINE=TEMP//LINE(L1:)
      ELSEIF (L.GT.IBLANK(LINE,80)) THEN
C LAST ITEM
        IF (J1.NE.0) THEN
          LINE=LINE(:J1)//TEMP
        ELSE
          LINE=TEMP
        ENDIF
      ELSE
C MIDDLE ITEM
        LINE=LINE(:J1)//TEMP//LINE(L1:)
      ENDIF
      END
      BLOCK DATA
      LOGICAL CONDOP(0:8), WRITE
      COMMON /CONDOP/ CONDOP
      COMMON /NUMDEF/ NUMDEF
      COMMON /WRITE/ NESTL, WRITE
      DATA WRITE /.TRUE./, CONDOP /.TRUE.,8*.FALSE./, NUMDEF /0/,
     1  NESTL /0/
      END
