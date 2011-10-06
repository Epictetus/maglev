
set class GsComLabelNode
category: '*maglev-Documentation'
classmethod:
comment
^ 
'GsComLabelNode represents the target of a goto,
  or of a break/next/redo/retry .  A break/next/redo/retry
  whose target is within the same method  is  the same as a goto .

 instVars are
   argForValue - a boolean , tells whether arg to a break/next
      should be evaluated for value or for effect.

   lexLevel , a SmallInteger , lexical level of this label
'
%


set class GsComLabelNode
category: '*maglev-Instance creation'
classmethod:
new
^ self _basicNew initialize
%


set class GsComLabelNode
category: '*maglev-Instance creation'
classmethod:
_basicNew
"create an instance registered with VM for finalization of cData"

<primitive: 674>
self _primitiveFailed: #_basicNew
%


set class GsComLabelNode
category: '*maglev-Instance Initialization'
method:
argForValue
  ^ argForValue
%


set class GsComLabelNode
category: '*maglev-Instance Initialization'
method:
initialize
  argForValue := false .
  lexLevel := -1 .
  kind := COMPAR_LABEL_NODE
%


set class GsComLabelNode
category: '*maglev-Instance Initialization'
method:
lexLevel
  ^ lexLevel
%


set class GsComLabelNode
category: '*maglev-Instance Initialization'
method:
lexLevel: anInt
  lexLevel := anInt
%


set class GsComLabelNode
category: '*maglev-Instance Initialization'
method:
lexLevel: anInt argForValue: aBoolean
  lexLevel := anInt .
  argForValue := aBoolean
%


set class GsComLabelNode
category: '*maglev-Printing'
method:
printFormattedOn: aStream
  super printOn: aStream  .
  aStream nextPutAll: ' argForValue:', argForValue asString ;
     print:' lexLevel:' int: lexLevel ;
  nextPut: $) ; cr .
%

