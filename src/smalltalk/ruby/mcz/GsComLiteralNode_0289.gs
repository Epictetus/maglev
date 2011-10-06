
set class GsComLiteralNode
category: '*maglev-Documentation'
classmethod:
comment
^ ' A GsComLiteralNode represents a push or load of the literal specifed
  by the leaf instVar

    leaf , a GsComLitLeaf 
'
%


set class GsComLiteralNode
category: '*maglev-Instance Creation'
classmethod:
newConstantRef: aRubyConstantRef
 ^ self new leaf: (GsComLitLeaf new  constRefLiteral: aRubyConstantRef)
%


set class GsComLiteralNode
category: '*maglev-Instance Creation'
classmethod:
newFalse
  ^ self new leaf: (GsComLitLeaf new specialLiteral: false)
%


set class GsComLiteralNode
category: '*maglev-Instance Creation'
classmethod:
newInteger: aSmallInt
 ^ self new leaf: (GsComLitLeaf new  integerLiteral: aSmallInt)
%


set class GsComLiteralNode
category: '*maglev-Instance Creation'
classmethod:
newNil
  ^ self new leaf: (GsComLitLeaf new specialLiteral: nil)
%


set class GsComLiteralNode
category: '*maglev-Instance Creation'
classmethod:
newObject: anObject
 ^ self new leaf: (GsComLitLeaf new  objectLiteral: anObject)
%


set class GsComLiteralNode
category: '*maglev-Instance Creation'
classmethod:
newRemoteNil
  ^ self new leaf: (GsComLitLeaf new specialLiteral: _remoteNil)
%


set class GsComLiteralNode
category: '*maglev-Instance Creation'
classmethod:
newString: aString
  | str |
  str := aString .
  str isInvariant ifFalse:[ str := aString copy ].
  ^ self new leaf: (GsComLitLeaf new stringLiteral: str ).
%


set class GsComLiteralNode
category: '*maglev-Instance Creation'
classmethod:
newTrue
  ^ self new leaf: (GsComLitLeaf new specialLiteral: true)
%


set class GsComLiteralNode
category: '*maglev-Printing'
method:
leaf: aGsComLitLeaf
  leaf := aGsComLitLeaf .
  kind := COMPAR_LIT_NODE .
%


set class GsComLiteralNode
category: '*maglev-Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  leaf printFormattedOn: aStream .
  aStream nextPut: $) ; cr .
%


set class GsComLiteralNode
category: '*maglev-Accessing'
method:
symbolLiteralValue
  | val |
  val := leaf symbolLiteralValue .
  val ifNil:[ self error:'invalid leaf for symbolLiteralValue'].
  ^ val
%

