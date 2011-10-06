
set class GsComArrayBuilderNode
category: '*maglev-Documentation'
classmethod:
comment
^
'A GsComArrayBuilderNode represents a non-literal array construction ,
 example:  { 1 , 2 , 3 }  

 instVars
   elements, an Array , 
          each element is a non-leaf GsCompilerIRNode
           for an expression which produces an element of the array.

 The generator emits code to execute each element expression, pushing
 each element on the stack and then emits an array constructor bytecode.
'
%


set class GsComArrayBuilderNode
category: '*maglev-Instance Initialization'
classmethod:
new
  ^ self _basicNew initialize
%


set class GsComArrayBuilderNode
category: '*maglev-Instance Initialization'
classmethod:
with: aNode
 | res |
 (res := self new) appendElement: aNode .
  ^ res
%


set class GsComArrayBuilderNode
category: '*maglev-Instance creation'
classmethod:
_basicNew
"create an instance registered with VM for finalization of cData"

<primitive: 674>
self _primitiveFailed: #_basicNew
%


set class GsComArrayBuilderNode
category: '*maglev-Instance Initialization'
method:
appendElement: aGsCompilerIRNode
  aGsCompilerIRNode ifNil:[ self error:'invalid nil arg to GsComArrayBuilderNode'].
  elements addLast: aGsCompilerIRNode
%


set class GsComArrayBuilderNode
category: '*maglev-Instance Initialization'
method:
initialize
  kind := COMPAR_ARRAY_BUILDER_NODE .
  elements := { } .
%


set class GsComArrayBuilderNode
category: '*maglev-Printing'
method:
printFormattedOn: aStream

  super printOn: aStream .
  aStream print:' count:' int: elements size ; 
      do: elements ;
  nextPut: $) ; cr .
%


set class GsComArrayBuilderNode
category: '*maglev-Instance Initialization'
method:
size
  ^ elements size
%

