grammar InlineStdin;

source
  : (line)+
  ;

line
  : comment
  | UntilNewline
  ;

comment
  : singleLineComment
  | multiLineComment
  ;

singleLineComment
  : '//IN:'
  | '// ' stdin
  ;

multiLineComment
  : '/*IN:' ('* '? stdin)+ '*'? '*/'
  ;

stdin
  : UntilNewline
  ;

CmtStart : '//' ;
UntilNewline: ~[*\n/]+ ~'\n'*;

WS  :   [ \t\n\r]+ -> skip ;
