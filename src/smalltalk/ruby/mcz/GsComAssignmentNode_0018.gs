
set class GsComAssignmentNode
category: '*maglev-Documentation'
classmethod:
comment
^
'A  GsComAssignmentNode represents an assignment operator , Smalltalk := 
  
    dest  ,  a GsComVarLeaf ,  the destination variable or temp 
    source , a non-leaf GsCompilerIRNode , the source expression
    assignKind , a SmallInteger , 0 = normal, 1 = method arg default value
'
%


set class GsComAssignmentNode
category: '*maglev-Instance Initialization'
method:
dest: aGsComVarLeaf source: sourceNode
    "returns receiver."
  kind := COMPAR_ASSIGNMENT_NODE .
  dest := aGsComVarLeaf .
  source := sourceNode .
  assignKind := 0 .
  sourceNode ifNil:[ self error:'illegal nil source for assignment'].
  aGsComVarLeaf ifNil:[ self error: 'illegal nil destination for assignment'].
%


set class GsComAssignmentNode
category: '*maglev-Printing'
method:
printFormattedOn: aStream

  super printOn: aStream .
  aStream print:' assignKind:' int: assignKind ; cr ; 
  nextPut: $( ; indentMore .
     dest printFormattedOn: aStream .
  aStream  nextPutAll:') := (' .
     source printFormattedOn: aStream .
  aStream indentLess ; nextPutAll: '))'  ; cr .
%


set class GsComAssignmentNode
category: '*maglev-Instance Initialization'
method:
setMethodArgDefault
  assignKind := 1
%

