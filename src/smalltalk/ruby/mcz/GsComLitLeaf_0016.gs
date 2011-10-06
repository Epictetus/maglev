
set class GsComLitLeaf
category: '*maglev-Documentation'
classmethod:
comment
^
'GsComLitLeaf  represents a literal leaf , and has these instVars:

  stringForm , a String  ,   the source form of the literal reference ,
                     stringForm is nil if literal is a GsNMethod (for Ruby)
  litValue   , an Object ,   the value of the literal
  litKind    , SmallInteger   , a ComParLitEtype

 A GsComLitLeaf is typically referenced by a GsComLiteralNode 
 thus allowing canonicalization of literal references, if the
 Parser maintains a dictionary, keyed on string source form of a literal
 and with values being instances of GsComLitLeaf.

 stringForm and litValue are provided separately in case a parser wants
 to do conversions such as string to numeric during a second pass.  The
 generator only looks at litValue , and the stringForm is present in the IR
 graph for use in debugging the parser or generator.
 The generator may change litValue to a canonicalize zero sized 
 literal arrays or strings, such as  Smalltalk  #() , ''''  .
 In 64bit Gemstone,  the float 0.0  is a SmallDouble and no longer needs
 canonicalization.

 The instance initializers below assume the parser would like
 to pass the string form of numeric literal, and have the class library
 convert from String to numeric form immediately.  String to numeric
 conversion could generate errors if the parser does not catch all illegal
 floating point formats, etc.   So if the parser did not want to worry
 about errors during numeric conversion, it might want other methods
 to trigger numeric conversion later ...

 The COMPAR constants must agree with ComParLitEtype in src/comparse.ht  .

 The following numeric literals are not supported yet by these methods:
   COMPAR_SCALED_DEC_LIT ,   ScaledDecimal, assume not needed by Ruby
   COMPAR_DECIMAL_FLT_LIT ,  DecimalFloat, assume not needed by Ruby
   COMPAR_FixedPoint_LIT ,   FixedPoint  , assume not needed by Ruby
'
%


set class GsComLitLeaf
category: '*maglev-Instance creation'
classmethod:
newNil
  ^ self new specialLiteral: nil
%


set class GsComLitLeaf
category: '*maglev-Instance Initialization'
method:
arrayLiteral: anArray

"anArray is a literal such as the Smalltalk literal  #( 1 2 abc )
 which can be constructed without executing any bytecodes. 

 In Smalltalk , these arrays are  Invarant , and the  Parser
 canonicalizes them based on comparing substrings from the source code.
"
  self setIRnodeKind .
  "stringForm left as nil for now"
  litKind := COMPAR_ARRAY_LIT .
  litValue := anArray .
  anArray immediateInvariant 
%


set class GsComLitLeaf
category: '*maglev-Instance Initialization'
method:
characterLiteral: aCharacter
  self setIRnodeKind .
  litValue := aCharacter .
  litKind := COMPAR_CHAR_LIT .
%


set class GsComLitLeaf
category: '*maglev-Instance Initialization'
method:
constRefLiteral: aRubyConstantRef

  self setIRnodeKind .
  litKind := COMPAR_ASSOC_LIT .  "fault into old_gen if possible"
  litValue := aRubyConstantRef .
%


set class GsComLitLeaf
category: '*maglev-Instance Initialization'
method:
deferredGlobalLiteral: anObject

"used in ruby IR "

  self setIRnodeKind .
  "stringForm left as nil for now"
  litKind := COMPAR_ARRAY_LIT .
  litValue := anObject .   "do not make invariant."
%


set class GsComLitLeaf
category: '*maglev-Instance Initialization'
method:
floatLiteral: aFloat
  litValue := aFloat .
  self setIRnodeKind .
  litKind := COMPAR_FLT_LIT .
%


set class GsComLitLeaf
category: '*maglev-Instance Initialization'
method:
floatLiteralFromString: aString

  "See Float>>fromString: for details of NaN representations in the input."

  self floatLiteral: (Float fromString: aString)
%


set class GsComLitLeaf
category: '*maglev-Instance Initialization'
method:
integerLiteral: anInteger
  litValue := anInteger .
  self setIRnodeKind .
  litKind := COMPAR_INT_LIT
%


set class GsComLitLeaf
category: '*maglev-Instance Initialization'
method:
integerLiteralFromString: aString
  stringForm := aString .
  self integerLiteral: (Integer fromString: aString)
%


set class GsComLitLeaf
category: '*maglev-Instance Initialization'
method:
methodLiteral: anASTnode

"anASTnode is the root of a graph of the AST for the method,
 need to transform that tree to tree beginning with a GsComMethNode 
 at the time we want to compile the method.  So the AST tree is
 sometimes persistent.
"
  self setIRnodeKind .
  "sourceForm left as nil,  method has source in it's debug info."
  litValue := anASTnode .
  litKind := COMPAR_METHOD_LIT .
%


set class GsComLitLeaf
category: '*maglev-Instance Initialization'
method:
objectLiteral: anObj

"Used for generic non-Assocation non-invariant literals
 in Ruby methods."

  self setIRnodeKind .
  "stringForm left as nil for now"
  litKind := COMPAR_ARRAY_LIT .
  litValue := anObj .   "do not set invariant"
%


set class GsComLitLeaf
category: '*maglev-Printing'
method:
printFormattedOn: aStream

  super printOn: aStream .
  aStream print:' litValue:' int: litValue ;
          print:' litKind:' int: litKind ;
      nextPut: $) ; cr .
%


set class GsComLitLeaf
category: '*maglev-Instance Initialization'
method:
rubyCopyingStringLiteral: aString

  "This literal will be accessed with a PUSH_COPYOF_LIT bytecode
   see also comments in stringLiteral: . "

  aString _isOneByteString ifFalse:[
    self error:'expected a String'   "detect parser problems, relax when QB string"
  ].
  self setIRnodeKind .
  stringForm := aString .
  litKind := COMPAR_RUBY_COPYING_STR_LIT .
  litValue := aString .
  aString immediateInvariant .
%


set class GsComLitLeaf
category: '*maglev-Instance Initialization'
method:
setIRnodeKind
  kind :=  COMPAR_LIT_LEAF
%


set class GsComLitLeaf
category: '*maglev-Instance Initialization'
method:
specialLiteral: aValue
  "aValue is expected to be an instance of Boolean or UndefinedObject"
  self setIRnodeKind .
    litValue := aValue .
  litKind := COMPAR_SPECIAL_LIT .
%


set class GsComLitLeaf
category: '*maglev-Instance Initialization'
method:
stringLiteral: aString

" aString is a  string literal such as the Smalltalk literal  'abc'  
  that can be completely generated by the Parser.

 It is the responsibility of the parser to maintain a dictionary
 of String literals if it is desired to canonicalize Strings  within
 method compilations, or across method compilations .  All String
 literals will be made invariant by the code generator."  

  self setIRnodeKind .
  stringForm := aString .
  litKind := COMPAR_STR_LIT .
  litValue := aString .

  aString immediateInvariant .
%


set class GsComLitLeaf
category: '*maglev-Instance Initialization'
method:
symbolLiteral: aString

  | sym |
  self setIRnodeKind .
  stringForm := aString .
  sym := aString asSymbol .
  sym class == DoubleByteSymbol ifTrue:[
    self error:'DoubleByteSymbol not supported for Ruby.'
  ].
  litValue :=  sym .
  litKind := COMPAR_SYM_LIT .
%


set class GsComLitLeaf
category: '*maglev-Accessing'
method:
symbolLiteralValue
  litKind == COMPAR_SYM_LIT ifFalse:[ self error:'not a symbol leaf' ].
  ^ litValue
%

