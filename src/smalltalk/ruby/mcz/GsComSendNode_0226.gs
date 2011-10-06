
set class GsComSendNode
category: '*maglev-Documentation'
classmethod:
comment
^
' A  GsComSendNode represents a message send, instVars are:

    rcvr,    a non-leaf GsCompilerIRNode , the receiver expression,
              can be nil if this GsComSendNode is part of a cascade .
    selLeaf,  a Symbol or a  GsComSelectorLeaf
    arguments, an Array , each element 
                is the non-leaf GsCompilerIRNode for an argument expression
    controlOp, a SmallInteger , a ComParCtlOpEType
    envFlags,  a SmallInteger  
       bits 16rFF  - message send environmentId 0..255
           16r100  -  during args evaluation, evaluate last arg first (ruby block_pass)
          16r1000  -  bypass ruby method protection checks for this send
         16r10000  - environmentId is from an @rubyN:

    If envFlags specifies message send environment == 1 and
    the parent GsComMethNode.rubyInfo has isRuby == 1, 
    then the GsComMethNode.environment is used as the actual environmentId
    for the send.

    controlOp is used to trigger the optimization of certain kinds of
    blocks as in-line code,  and to convert certain sends like ifTrue:
    to a special branch bytecode . It must be specified by the parser.
'
%


set class GsComSendNode
category: '*maglev-Class Initialization'
classmethod:
initialize
  "initialize the control op dictionary; these are the selectors for
   which the generator can generate in-line branching or looping code,
   if the receiver and/or arguments to the send meet certain critiera.

   For example:
     If the argument to ifTrue:  is a block , the block can be inlined.

   The criteria to be met for optimizing each of these selectors are
   not yet encoded into the behavior of GsComSendNode .  Currently
   the checks are implemented only in comparse.c .
  "

  | dict |
  dict := IdentityKeyValueDictionary new .
  self _removeClassVar: #ControlOpDict  ifAbsent:[].
  self _addInvariantClassVar: #ControlOpDict value: dict .

  "all of the following branch constructs will observe the isRuby attribute
   of the current method node .
   for example:  'ifTrue:'  means  'if neither false nor nil' in a Ruby method.
  "
  		"ruby_selector_suffix dependent"
  dict at: #ifTrue:          put: COMPAR__IF_TRUE ;
       at: #ifFalse:         put: COMPAR__IF_FALSE ;
       at: #ifTrue:ifFalse:  put: COMPAR_IF_TRUE_IF_FALSE  ;
       at: #ifFalse:ifTrue:  put: COMPAR_IF_FALSE_IF_TRUE  ;
       at: #ifNil:ifNotNil:  put: COMPAR_IF_NIL_IF_NOTNIL ;
       at: #ifNotNil:ifNil:  put: COMPAR_IF_NOTNIL_IF_NIL ;
       at: #ifNil:           put: COMPAR_IF_NIL ;
       at: #ifNotNil:        put: COMPAR_IF_NOT_NIL ;
       at: #or:              put: COMPAR_OR_SELECTOR  ;
       at: #and:             put: COMPAR_AND_SELECTOR  ;
       at: #whileFalse:      put: COMPAR_WHILE_FALSE  ;
       at: #'whileFalse#1__'  put: COMPAR_WHILE_FALSE  ;

       at: #whileTrue:       put: COMPAR_WHILE_TRUE  ;
       at: #'whileTrue#1__'   put: COMPAR_WHILE_TRUE  ;

       at: #untilFalse:      put: COMPAR_UNTIL_FALS_COLON ;
       at: #'untilFalse#1__'  put: COMPAR_UNTIL_FALS_COLON  ;

       at: #untilTrue:       put: COMPAR_UNTIL_TRU_COLON ;
       at: #'untilTrue#1__'   put: COMPAR_UNTIL_TRU_COLON  ;

       at: #untilFalse       put: COMPAR_UNTIL_FALSE ;
       at: #untilTrue        put: COMPAR_UNTIL_TRUE ;
       at: #whileFalse       put: COMPAR_UNTIL_TRUE ;
       at: #whileTrue        put: COMPAR_UNTIL_FALSE ;
       at: #repeat      put: COMPAR_FOREVER_repeat ;

       at: #to:do:           put: COMPAR_TO_DO ;
       at: #to:by:do:        put: COMPAR_TO_BY_DO ;
       at: #timesRepeat:     put: COMPAR_TIMES_REPEAT ;
       at: #_downTo:do:      put: COMPAR__DOWNTO_DO ;
       at: #_downTo:by:do:   put: COMPAR__DOWNTO_BY_DO .
%


doit
GsComSendNode initialize.
%


set class GsComSendNode
category: '*maglev-Instance Creation'
classmethod:
new
  ^ self _basicNew initialize
%


set class GsComSendNode
category: '*maglev-Instance Initialization'
method:
appendArgument: aNode
  aNode == nil ifTrue:[ self error:'illegal nil argument'].
  arguments addLast: aNode
%


set class GsComSendNode
category: '*maglev-Instance Initialization'
method:
environment
  ^ envFlags bitAnd: 16rFF
%


set class GsComSendNode
category: '*maglev-Instance Initialization'
method:
environment: anInteger
  envFlags := ((envFlags bitShift: -8) bitShift: 8)
               bitOr: (self validateEnvironment: anInteger) 
%


set class GsComSendNode
category: '*maglev-Instance Initialization'
method:
initialize
  arguments := { } .
  controlOp := COMPAR_NO_OPTIMIZATION "a normal send" .
  envFlags := 0 .
  kind := COMPAR_SEND_NODE .
%


set class GsComSendNode
category: '*maglev-Transformation'
method:
optimizationPossible
  "Return true if optimization to an in-line  branch or loop could be possible,
   otherwise return false .
   To be sent after initializing the selector."

  | sel |
  (sel := selLeaf) _isSymbol ifFalse:[ sel := sel selector ].
  ^ (ControlOpDict at: sel otherwise: 0 ) ~~ 0 
%


set class GsComSendNode
category: '*maglev-Transformation'
method:
optimize

  "Attempt to optimize the receiver to a special selector.
   Generates an error if receiver is not optimizable .
   Use  optimizationPossible  to determine if  optimize  would succeed."

  | op sel |
  (sel := selLeaf) _isSymbol ifFalse:[ sel := sel selector ].
  op := ControlOpDict at: sel otherwise: 0 .
  op ~~ 0 ifTrue:[ 
    controlOp := op 
  ] ifFalse: [
    self error:'not optimizable'
  ]
%


set class GsComSendNode
category: '*maglev-Printing'
method:
printFormattedOn: aStream
  | sel |
  super printOn: aStream .
  aStream nextPutAll:' selLeaf: '; indentMore ; cr .
  (sel := selLeaf) _isSymbol ifTrue:[
     aStream nextPut: $# ; nextPut: $' ; nextPutAll: sel; nextPut: $' ; nextPut: $  .
  ] ifFalse:[
     sel printFormattedOn: aStream.
  ].
  aStream  indentLess ; cr ;
     print:' controlOp:' int: controlOp ; 
     print:' envFlags:' int: envFlags ; cr;
  nextPutAll:' rcvr:' .
    rcvr ~~ nil ifTrue:[ rcvr printFormattedOn: aStream] 
              ifFalse:[ aStream nextPutAll:'nil '].
  aStream nextPutAll: ' args:' ; do: arguments ;
  nextPut: $) ; cr .
%


set class GsComSendNode
category: '*maglev-Instance Initialization'
method:
rcvr: aGsCompilerIRNode 
  rcvr := aGsCompilerIRNode 
%


set class GsComSendNode
category: '*maglev-Instance Initialization'
method:
rubySelector: aSymbol 
  " for a send NOT to super"

  selLeaf := GsComSelectorLeaf newSelector: aSymbol env: 1 .
  envFlags ~~ 0 ifTrue:[ self error:'should set flags after selector'].
  envFlags := 1 .  
%


set class GsComSendNode
category: '*maglev-Instance Initialization'
method:
rubySelector: aSymbol toSuper: superBool
  superBool ifTrue:[
    selLeaf := aSymbol  "can't optimize send to super"
  ] ifFalse:[
    selLeaf := GsComSelectorLeaf newSelector: aSymbol env: 1
  ].
  envFlags ~~ 0 ifTrue:[ self error:'should set flags after selector'].
  envFlags := 1 .  
%


set class GsComSendNode
category: '*maglev-Instance Initialization'
method:
rubySelectorInBridge: aSymbol 
  selLeaf := GsComSelectorLeaf newSelector: aSymbol env: 1 . 
    " and  comgen.c should generate a SEND_CURRENT"
  envFlags ~~ 0 ifTrue:[ self error:'should set flags after selector'].
  envFlags := 1 .
%


set class GsComSendNode
category: '*maglev-Instance Initialization'
method:
setBypassRubyProtection
  envFlags := envFlags bitOr: BypassProtection_MASK
%


set class GsComSendNode
category: '*maglev-runtime'
method:
setEvalLastArgFirst
  self error:'UNEXPECTED use of Deprecated setEvalLastArgFirst'.
  envFlags := envFlags bitOr: EvalLastArgFirst_MASK

%


set class GsComSendNode
category: '*maglev-Instance Initialization'
method:
stSelector: aSymbol 
  selLeaf := GsComSelectorLeaf newSelector: aSymbol env: 0 .
  envFlags := 0 .  
%

