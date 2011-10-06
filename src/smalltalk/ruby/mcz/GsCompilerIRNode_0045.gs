
set class GsCompilerIRNode
category: '*maglev-Documentation'
classmethod:
comment
^ '
 GsCompilerIRNode  is the abstract superclass of all classes used
 to implement nodes of the IR graph which is input to the 
 code generator comgen.c  in Gemstone64 v3.0.   The instVars are:

  kind      , a SmallInteger, per ComIRNodeEType
  srcOffset, a SmallInteger , 1-based character position in the source , or nil .
  lineNumber, a SmallInteger, optional 1-based line number in the source,
                              or -1 or nil if unknown
         with Smalltalk parser lineNumber used for debugging compiler and slow filein
         with Ruby parser written in Ruby , lineNumber only used to hold
         starting line of a  GsComMethNode .
 
  See comments for GsComMethNode for more details on srcOffset and lineNumber.

 In the documentation of subclasses,
    a non-leaf GsCompilerIRNode
 means instance of any subclass 
 except GsComLitLeaf, GsComSelectorLeaf, GsComVarLeaf 
 
 The class variables for GsCompilerIRNode include symbolic names
 for the various COMPAR, COM_RTN, and Bc_ constants used in
 instance methods in subclasses.  The class variables are populated
 at server image build by the code in src/bom.c , from constants in 
 src/comparse.ht and src/bytecode.ht .
'
%


set class GsCompilerIRNode
category: '*maglev-Accessing'
classmethod:
rubyCompatibilityLevel
  "Returns 18 for MRI 1.8 compatibility, 19 for MRI 1.9, etc "

  ^ COM_RUBY_COMPAT_LEVEL
%


set class GsCompilerIRNode
category: '*maglev-Accessing'
method:
hasPosition
  ^ srcOffset ~~ nil or:[ lineNumber ~~ nil ]
%


set class GsCompilerIRNode
category: '*maglev-Accessing'
method:
kind
  ^ kind
%


set class GsCompilerIRNode
category: '*maglev-Accessing'
method:
lastLineNumber
  ^ lineNumber
%


set class GsCompilerIRNode
category: '*maglev-Accessing'
method:
lastSourceOffset
  ^ srcOffset
%


set class GsCompilerIRNode
category: '*maglev-Accessing'
method:
lineNumber
  ^ lineNumber
%


set class GsCompilerIRNode
category: '*maglev-Updating'
method:
lineNumber: aSmallInteger
  "to be used only after initializating the node .
   The argument is a positive one-based  line number"

  lineNumber := aSmallInteger
%


set class GsCompilerIRNode
category: '*maglev-Accessing'
method:
litVarValueOrNil
  ^ nil
%


set class GsCompilerIRNode
category: '*maglev-Printing'
method:
printFormattedOn: aStream
  self printOn: aStream
%


set class GsCompilerIRNode
category: '*maglev-Printing'
method:
printOn: aStream

  aStream nextPut: $(; space;  nextPutAll: self class name ; 
     nextPutAll:' objId: ' ; nextPutAll: self asOop asString .
  srcOffset ifNotNil:[
     aStream nextPutAll:' srcOfs:' ; nextPutAll: srcOffset asString ; space
   ] ifNil:[ 
     aStream nextPutAll:' line:' ; nextPutAll: lineNumber asString ; space
   ].
%


set class GsCompilerIRNode
category: '*maglev-Printing'
method:
printString
  | strm |
  strm := IndentingStream newPrinting .
  self printFormattedOn: strm .
  ^ strm contents .
%


set class GsCompilerIRNode
category: '*maglev-Transformation'
method:
returnNode

^ GsComReturnNode new return: self
%


set class GsCompilerIRNode
category: '*maglev-Accessing'
method:
sourceOffset
  ^ srcOffset
%


set class GsCompilerIRNode
category: '*maglev-Updating'
method:
sourceOffset: aSmallInteger
  "Argument is a 1-based character offset into the source string."
  srcOffset ifNil:[ srcOffset := aSmallInteger ]
%


set class GsCompilerIRNode
category: '*maglev-Accessing'
method:
symbolLiteralValue

  ^ nil "caller should signal a RubyParseError"
%


set class GsCompilerIRNode
category: '*maglev-Updating'
method:
validateEnvironment: anInteger
  "range check on an environment identifier for a method or send node"
  (anInteger < 0 or:[ anInteger > 16rFF ]) ifTrue:[
    anInteger error:'out of range' . 
    ^ 0
  ].
  ^ anInteger
%


set class GsCompilerIRNode
category: '*maglev-Accessing'
method:
varLeaf
  ^ nil
%

