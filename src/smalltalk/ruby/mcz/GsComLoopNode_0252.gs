
set class GsComLoopNode
category: '*maglev-Documentation'
classmethod:
comment
^
'A GsComLoopNode encapsulates a Ruby while or until loop.

 instVars are
    send         a GsComSendNode , the send of whileTrue: or whileFalse:
    breakLabel,  a GsComLabelNode , the target of a possible break
    iterResult, a GsCompilerIRNode, literal result of in-line for loop, or nil
'
%


set class GsComLoopNode
category: '*maglev-Instance creation'
classmethod:
new
  ^ self _basicNew initialize
%


set class GsComLoopNode
category: '*maglev-Instance Initialization'
method:
breakLabel: aLabelNode
  breakLabel := aLabelNode
%


set class GsComLoopNode
category: '*maglev-Instance Initialization'
method:
initialize
  kind := COMPAR_LOOP_NODE .
%


set class GsComLoopNode
category: '*maglev-Instance Initialization'
method:
iterResult: aLiteralNode
  iterResult := aLiteralNode
%


set class GsComLoopNode
category: '*maglev-Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  aStream nextPutAll:' send: '; indentMore ; cr .
     send printFormattedOn: aStream.
  aStream  indentLess ; cr ;
     nextPutAll:' label: ' .
  breakLabel printFormattedOn: aStream.
  iterResult ifNotNil:[
    aStream nextPutAll:' iterResult: ' .
    iterResult printFormattedOn: aStream.
  ].
  aStream nextPut: $) ; cr .
%


set class GsComLoopNode
category: '*maglev-Instance Initialization'
method:
send: aSendNode
  send := aSendNode
%

