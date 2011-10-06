
set class GsComGotoNode
category: '*maglev-Documentation'
classmethod:
comment
^
'GsComGotoNode represents an unconditional jump within the current
 method, and may be used in the implementation of
 break/next/redo/retry where the affected loop is within the
 same method as the break/next/redo/retry .

 instVars are
   target -  a GsComLabelNode  for a goto within a method
             nil for a non-local goto
   argNode a GsCompilerIRNode, or nil
   argForValue, a Boolean, if true evaluate argNode for value, if false for effect
   targetKind , a ComParGotoKind
'
%


set class GsComGotoNode
category: '*maglev-Instance creation'
classmethod:
new
  ^ self _basicNew initialize
%


set class GsComGotoNode
category: '*maglev-Instance creation'
classmethod:
_basicNew
"create an instance registered with VM for finalization of cData"

<primitive: 674>
self _primitiveFailed: #_basicNew
%


set class GsComGotoNode
category: '*maglev-Instance Initialization'
method:
argNode: anIrNode
  argNode := anIrNode
%


set class GsComGotoNode
category: '*maglev-Instance Initialization'
method:
initialize
  kind := COMPAR_GOTO_NODE .
%


set class GsComGotoNode
category: '*maglev-Instance Initialization'
method:
localRubyBreak: aLabelNode 
  targetKind :=  COM_GOTO_BREAK.
  target := aLabelNode .
  argForValue :=  true
%


set class GsComGotoNode
category: '*maglev-Instance Initialization'
method:
localRubyNext: aLabelNode argForValue: aBoolean
  targetKind := COM_GOTO_NEXT .
  target := aLabelNode  .
  argForValue := aBoolean
%


set class GsComGotoNode
category: '*maglev-Instance Initialization'
method:
localRubyRedo: aLabelNode
  targetKind := COM_GOTO_REDO .
  target := aLabelNode  .
  argForValue := false
%


set class GsComGotoNode
category: '*maglev-Instance Initialization'
method:
nonLocalRubyNext
  "target left as nil"
  targetKind := COM_GOTO_NEXT .
  argForValue := true
%


set class GsComGotoNode
category: '*maglev-Instance Initialization'
method:
nonLocalRubyRedo
  "target left as nil"
  targetKind := COM_GOTO_REDO .
  argForValue := false
%


set class GsComGotoNode
category: '*maglev-Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  aStream print:' target objId:' int: target asOop ; cr ;
    nextPutAll: ' kind:' ; nextPutAll: 
         ( #( #next #redo #retry #break ) at: targetKind) ;
    print: ' argForValue:' bool: argForValue ; cr ;
    nextPutAll: '    argNode:' .
  argNode == nil ifTrue:[ aStream nextPutAll:'nil' ]
    ifFalse:[ argNode printFormattedOn: aStream  ].
  aStream nextPut: $) ; cr .
%

