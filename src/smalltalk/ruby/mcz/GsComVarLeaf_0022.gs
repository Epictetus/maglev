
set class GsComVarLeaf
category: '*maglev-Documentation'
classmethod:
comment
^
'a GsComVarLeaf   represents a method arg or temp, a block arg or temp,
                an an instance variable , or a literal variable,
                or a reference to self or super

  varName ,  a Symbol  , the name of the variable
  litVarAssoc , an Association , non-nil only for a literal variable
  varKind    , a SmallInteger, a ComParVarEType
  lexLevel   , a SmallInteger,  the lexical level in terms of block scoping ,
         0 means the home method, 1 is first block level 
         parser increments it''s lexLevel when entering both normal and in-lined blocks

  varOffset  is -1 for a Ruby dynamic instVar
                 0 for self or super
                 0 for a literal variable
                 zero-based offset into the instVars for an instance variable,
                 zero-based offset into the method or block args for an arg ,
                 0 for a method or block temp ( generator will assign an offset)

  for a Smalltalk literal variable,  varName is the key in the Association

  The parser is responsible for canonicalizing all references to a given
  arg, temp or instVar  within a method to be references to the same  
  GsComVarLeaf.
  Thus multiple GsComVariableNode''s may reference a single GsComVarLeaf,
  within a single compilation.
'
%


set class GsComVarLeaf
category: '*maglev-Instance creation'
classmethod:
new
"create an instance registered with VM for finalization of cData"

<primitive: 674>
self _primitiveFailed: #new
%


set class GsComVarLeaf
category: '*maglev-Printing'
classmethod:
varKindToString: aKind
  aKind == COMPAR_BLOCK_ARG_VAR ifTrue:[ ^ 'BLOCK_ARG'].
  aKind == COMPAR_BLOCK_TEMP_VAR ifTrue:[ ^ 'BLOCK_TEMP'].
  aKind == COMPAR__INST_VAR ifTrue:[ ^ 'INST_VAR'].         
  aKind == COMPAR_LIT_VAR ifTrue:[ ^ 'LIT_VAR'].        
  aKind == COMPAR_METHOD_ARG_VAR ifTrue:[ ^ 'METHOD_ARG'].
  aKind == COMPAR_METHOD_TEMP_VAR ifTrue:[ ^ 'METHOD_TEMP'].
  aKind == COMPAR_METH_VC_GLOBAL ifTrue:[ ^ 'METH_VC_GLOBAL'].
  aKind == COMPAR_SELF_VAR ifTrue:[ ^ 'SELF'].
  aKind == COMPAR_SUPER_VAR ifTrue:[ ^ '_SUPER'].
  "COMPAR_LIT_VAR_SPECIAL_LITERAL not legal, from dbf conversion only"
  ^ 'INVALID_VAR'
%


set class GsComVarLeaf
category: '*maglev-Instance Initialization'
method:
blockArg: argNameSymbol argNumber: oneBasedArgNum forBlock: aGsComBlockNode 
  self setIRnodeKind.
  argNameSymbol _isSymbol ifFalse:[ self error:'expected a symbol'].
  self setVarName: argNameSymbol .
  oneBasedArgNum < 1 ifTrue:[ oneBasedArgNum error:'out of range'].
  varOffset :=  oneBasedArgNum - 1 . "convert to zero based"
  varKind := COMPAR_BLOCK_ARG_VAR .
  lexLevel := aGsComBlockNode lexLevel
%


set class GsComVarLeaf
category: '*maglev-Instance Initialization'
method:
blockTemp: tempNameSymbol sourceLexLevel: aLevel
  "code generator will delete block temps from
   the method if there are no IR references other than the definition"
  aLevel < 1 ifTrue:[ aLevel error:'out of range'].  
  self setIRnodeKind.
  tempNameSymbol _isSymbol ifFalse:[ self error:'expected a symbol'].
  self setVarName: tempNameSymbol .
  varOffset := 0 . "generator will assign offsets for temps"
  varKind := COMPAR_BLOCK_TEMP_VAR .
  lexLevel := aLevel 
%


set class GsComVarLeaf
category: '*maglev-Instance Initialization'
method:
initializeSelf
  self setIRnodeKind .
  varName :=  #self .
  varOffset := 0  .
  varKind := COMPAR_SELF_VAR .  
  lexLevel := 0 .
%


set class GsComVarLeaf
category: '*maglev-Instance Initialization'
method:
initializeSuper
  self setIRnodeKind .
  varName :=  #super .
  varOffset := 0 .
  varKind := COMPAR_SUPER_VAR .  
  lexLevel := 0 .
%


set class GsComVarLeaf
category: '*maglev-Instance Initialization'
method:
instanceVariable: ivNameSymbol ivOffset: oneBasedIvOfs
  "Parser must lookup instVar names in class definitions to determine the
   oneBasedIvOfs.  Alternatively, additional behavior could be provided here to
   take an ivName and a Class and do the lookup here ..."  
  self setIRnodeKind.
  self setVarName: ivNameSymbol .
  varKind := COMPAR__INST_VAR.
  oneBasedIvOfs > 0 ifTrue:[
    "instVar at known offset, convert to zero based"
    varOffset := oneBasedIvOfs - 1. 
  ] ifFalse:[
    oneBasedIvOfs == -1 ifTrue:[
      varOffset := -1 "ruby dynamic instVar"
    ] ifFalse:[
      oneBasedIvOfs error:'out of range'
    ]
  ].
%


set class GsComVarLeaf
category: '*maglev-Querying'
method:
isArg
  ^ varKind == COMPAR_METHOD_ARG_VAR or:[ varKind == COMPAR_BLOCK_ARG_VAR]
%


set class GsComVarLeaf
category: '*maglev-Querying'
method:
isTemp
  "return true if varKind one of 
    COMPAR_METHOD_TEMP_VAR COMPAR_BLOCK_TEMP_VAR, COMPAR_METH_VC_GLOBAL"

  ^ varKind <= COMPAR_METH_VC_GLOBAL
%


set class GsComVarLeaf
category: '*maglev-Accessing'
method:
lexLevel
  ^ lexLevel
%


set class GsComVarLeaf
category: '*maglev-Instance Initialization'
method:
literalVariable: anAssociation
  "Smalltalk style global variable, class variable, etc .
   Hopefully usable for Ruby globals ? "
  | key |
  self setIRnodeKind.
  key := anAssociation key .
  key _isSymbol ifFalse:[ key error:'bad arg kind'].
  self setVarName:  key .
  litVarAssoc := anAssociation .
  (anAssociation isKindOf: SymbolAssociation) ifFalse:[
    self error:'arg to literalVariable: is not a SymbolAssociation'
  ].
  varOffset := 0 .
  varKind := COMPAR_LIT_VAR . 
%


set class GsComVarLeaf
category: '*maglev-Accessing'
method:
litVarValue
  varKind == COMPAR_LIT_VAR ifFalse:[ self error:'not a literal variable'].
  ^ litVarAssoc _value
%


set class GsComVarLeaf
category: '*maglev-Instance Initialization'
method:
methodArg: argNameSymbol argNumber: oneBasedArgNum
  self setIRnodeKind.
  argNameSymbol _isSymbol ifFalse:[ self error:'expected a symbol'].
  self setVarName: argNameSymbol .
  oneBasedArgNum < 1 ifTrue:[ oneBasedArgNum error:'out of range'].
  varOffset :=  oneBasedArgNum - 1 . "convert to zero based"
  varKind := COMPAR_METHOD_ARG_VAR.
  lexLevel := 0
%


set class GsComVarLeaf
category: '*maglev-Instance Initialization'
method:
methodTemp: aSymbol 
  "code generator will delete method temps from
   the method if there are no IR references other than the definition"
  self setIRnodeKind .
  self setVarName: aSymbol .
  varOffset := 0 .  "generator will assign offsets for temps"
  varKind := COMPAR_METHOD_TEMP_VAR  .
  lexLevel := 0
%


set class GsComVarLeaf
category: '*maglev-Instance Initialization'
method:
methVcGlobal: aSymbol 
  "code generator will allocate a slot for this even if the definition
   is the only reference in the IR."
  self setIRnodeKind .
  self setVarName: aSymbol .
  aSymbol == #'$~' ifTrue:[ varOffset := 0 ] 
    ifFalse:[ aSymbol == #'$_' ifTrue:[ varOffset := 1  ]
      ifFalse:[  aSymbol == #'__lexPath' ifTrue:[ varOffset := 2 ]
        ifFalse:[ self error:'invalid arg to methVcGlobal:']]].
  varKind := COMPAR_METH_VC_GLOBAL  .   
  lexLevel := 0
%


set class GsComVarLeaf
category: '*maglev-Printing'
method:
printFormattedOn: aStream

  super printOn: aStream .
  aStream print:' varName: ' symbol: varName ;
      nextPutAll: ' varKind:' ; nextPutAll: self varKindString ;
      print: ' lexLevel:' int: lexLevel ; 
      print: ' varOffset:' int: varOffset ; 
      nextPut: $) ; cr .
%


set class GsComVarLeaf
category: '*maglev-Instance Initialization'
method:
setIRnodeKind
  kind :=  COMPAR_VAR_LEAF
%


set class GsComVarLeaf
category: '*maglev-Instance Initialization'
method:
setVarName: aName
  varName := aName .
%


set class GsComVarLeaf
category: '*maglev-Accessing'
method:
varKind
  ^ varKind
%


set class GsComVarLeaf
category: '*maglev-Printing'
method:
varKindString
  ^ self class varKindToString: varKind
%


set class GsComVarLeaf
category: '*maglev-Accessing'
method:
varName
  ^ varName
%


set class GsComVarLeaf
category: '*maglev-Accessing'
method:
varOffset
  ^ varOffset
%

