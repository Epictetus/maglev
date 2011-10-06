
set class GsComStatementsNode
category: '*maglev-Documentation'
classmethod:
comment
^
'A GsComStatementsNode encapsulates an array of IR nodes

 instVars are
    list       an Array of GsCompilerIRNode''s , of size >= 1
'
%


set class GsComStatementsNode
category: '*maglev-Instance creation'
classmethod:
new
  ^ self _basicNew initialize
%


set class GsComStatementsNode
category: '*maglev-Instance Initialization'
method:
initialize
  kind := COMPAR_STATEMENTS_NODE .
%


set class GsComStatementsNode
category: '*maglev-Printing'
method:
lastLineNumber
  | lst sz |
  lst := list .
  (sz := lst size) _downTo: 1 do:[:n | | num |
    num := (lst at: n) lastLineNumber .
    num ifNotNil:[  
       ^ num
    ].
  ].
  ^ nil
%


set class GsComStatementsNode
category: '*maglev-Printing'
method:
lastSourceOffset
  | lst sz |
  lst := list .
  (sz := lst size) _downTo: 1 do:[:n | | num |
    num := (lst at: n) lastSourceOffset .
    num ifNotNil:[  
       ^ num
    ].
  ].
  ^ nil
%


set class GsComStatementsNode
category: '*maglev-Printing'
method:
lineNumber
  | lst |
  (lst := list) size ~~ 0 ifTrue:[ ^ ( lst at: 1) lineNumber ]  .
%


set class GsComStatementsNode
category: '*maglev-Instance Initialization'
method:
list: anArray
  list := anArray
%


set class GsComStatementsNode
category: '*maglev-Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  aStream
    nextPutAll:' list:'; do: list ; 
  nextPut: $) ; cr .
%


set class GsComStatementsNode
category: '*maglev-Printing'
method:
sourceOffset
  | lst |
  (lst := list) size ~~ 0 ifTrue:[ ^ ( lst at: 1) sourceOffset ]  .
%

