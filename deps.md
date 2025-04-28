flowchart TB
  N1[prep]
  N2[M_CLI2]
  N3[M_io]
  N4[M_strings]
  N5[M_list]
  N6[M_history]
  N7[M_match]
  N8[M_attr]
  click N1 href "https://github.com/urbanjost" "Fortran pre-processor"
  click N2 href "https://github.com/urbanjost/M_CLI2.git" "Unix-style commandline parsing using a prototype command"
  click N3 href "https://github.com/urbanjost/M_io.git" "I/O-related tools"
  click N4 href "https://github.com/urbanjost/M_strings.git" "string manipulation"
  click N5 href "https://github.com/urbanjost/M_list.git" " maintain sorted lists of INTRINSIC type"
  click N6 href "https://github.com/urbanjost/M_history.git" "Input History Editor"
  click N7 href "https://github.com/urbanjost/M_match.git" "Basic Regular Expressions"
  click N8 href "https://github.com/urbanjost/M_attr.git" "ANSI control escape sequences using an XML-like syntax for attributes like color on video displays and emulators"
  N1-->N2
  N1-->N3
  N1-->N4
  N1-->N5
  N1-->N6
  N1-->N7
  N1-->N8
  N6-->N4
