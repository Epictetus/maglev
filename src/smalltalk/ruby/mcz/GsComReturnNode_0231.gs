
set class GsComReturnNode
category: '*maglev-Documentation'
classmethod:
comment
^ ' A GsComReturnNode represents a return from a method or block

    expr,   a non-leaf GsCompilerIRNode
 
    returnKind, a SmallInteger  
        0 means normal return from method or block
        1 means Smalltalk return-from-home (same as 0 if at lexLevel 0)
        2 means Ruby ''break'' in block; return to caller of caller

'
%


set class GsComReturnNode
category: '*maglev-Instance Initialization'
method:
breakFromRubyBlock: aNode
  kind := COMPAR_RETURN_NODE .
  expr := aNode .
  returnKind := -1 " would be COM_RTN_TWO_LEVELS " .
  self error:'COM_RTN_TWO_LEVELS not implemented in VM yet' .
%


set class GsComReturnNode
category: '*maglev-Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  aStream print:' returnKind:' int: returnKind ; cr ;
    indentMore .
  expr printFormattedOn: aStream .
  aStream indentLess ; nextPut: $) ; cr .
%


set class GsComReturnNode
category: '*maglev-Instance Initialization'
method:
return: aNode
  kind := COMPAR_RETURN_NODE .
  expr := aNode .
  returnKind := COM_RTN_NORMAL .
%


set class GsComReturnNode
category: '*maglev-Instance Initialization'
method:
returnFromHome: aNode
  kind := COMPAR_RETURN_NODE .
  expr := aNode .
  returnKind := COM_RTN_FROM_HOME.
%

