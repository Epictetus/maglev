
set class GsComVariableNode
category: '*maglev-Documentation'
classmethod:
comment
^' A GsComVariableNode node represents a fetch from a 
  an argument, temporary , or literal variable.

  instVars are
     leaf  , a GsComVarLeaf
'
%


set class GsComVariableNode
category: '*maglev-Instance Creation'
classmethod:
globalNamed: aSymbol inDict: aDictionary
| assoc node |
assoc := aDictionary associationAt: aSymbol .
(node := self new) 
  leaf: (GsComVarLeaf new literalVariable: assoc).
^ node
%


set class GsComVariableNode
category: '*maglev-Instance Creation'
classmethod:
newSelf
| node |
(node := self new) leaf: (GsComVarLeaf new initializeSelf).
^ node
%


set class GsComVariableNode
category: '*maglev-Instance Initialization'
method:
leaf: aGsComVarLeaf

kind := COMPAR_VAR_NODE .
leaf := aGsComVarLeaf
%


set class GsComVariableNode
category: '*maglev-Accessing'
method:
litVarValue
  ^ leaf litVarValue
%


set class GsComVariableNode
category: '*maglev-Accessing'
method:
litVarValueOrNil
  ^ leaf litVarValue
%


set class GsComVariableNode
category: '*maglev-Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  leaf printFormattedOn: aStream .
  aStream   nextPut: $) ; cr .
%


set class GsComVariableNode
category: '*maglev-Accessing'
method:
varLeaf
  ^ leaf
%

