
set class GsComCascadeNode
category: '*maglev-Documentation'
classmethod:
comment
^
'A GsComCascadeNode represents a Smalltalk style cascaded message send
 such as
   anObj m1 ; m2 ; m3 .

 instVars
    rcvr , a non-leaf GsCompilerIRNode
    sends, an Array , each element is a GsComSendNode 
'
%


set class GsComCascadeNode
category: '*maglev-Instance Initialization'
method:
appendSend: aNode
  sends addLast: aNode
%


set class GsComCascadeNode
category: '*maglev-Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  aStream nextPutAll:' rcvr:'. rcvr printFormattedOn: aStream .
  aStream nextPutAll:' sends:'; do: sends ;
    nextPut: $) ; cr .
%


set class GsComCascadeNode
category: '*maglev-Instance Initialization'
method:
rcvr: aNode
  rcvr := aNode .
  sends := { } .
  kind := COMPAR_CASCADE_NODE
%

