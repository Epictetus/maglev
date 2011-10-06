
set class GsComSelectorLeaf
category: '*maglev-Accessing'
classmethod:
classToISAselector: aClass
  "Return the special selector to use in optimizing Ruby coerce_to 
   for coercion to aClass .  Return nil if aClass does not have
   a special _is*  selector. "
  | val |
  val := SpecialISAselectors at: aClass otherwise: nil .
  val == nil ifFalse: [ val := val at: 1 ] .
  ^ val
%


set class GsComSelectorLeaf
category: '*maglev-Accessing'
classmethod:
classToRubyClassName: aClass
  "Return the ruby class name to use in optimizing Ruby coerce_to 
   for coercion to aClass .  Return nil if aClass does not have
   a special _is*  selector. "
  | val |
  val := SpecialISAselectors at: aClass otherwise: nil .
  ^ (val == nil) ifTrue: [ aClass name ] 
                 ifFalse: [ val at: 2 ] .
%


set class GsComSelectorLeaf
category: '*maglev-Documentation'
classmethod:
comment
^ 
' A GsComSelectorLeaf represents the selector used in a message send.
  instVars are
    selector,  a Symbol 
    specialOpcode , a SmallInteger 
    specialSendClass , a Behavior  

  If specialOpcode is non-zero the send may be optimizable
  to a special bytecode.  We could have the generator (comgen.c) make
  this determination; currently  the parser determines what is optimizable.
  example:    ==  sent to any Smalltalk object is a special bytecode ,
                not a normal send .

  The parser is disallowing redefinition of some methods in the base
  Smalltalk classes if you are not logged into the Repository as SystemUser .

  Some selector leafs may be initialized at start of parsing and kept
  in a dictionary of special sends, etc.  These instances will have srcOffset == 1.

  For the optimizable sends,  specialSendClass
    can be Object  for a send such as     ==
    can be  ExecBlock  for sends like   value:
    can be SmallInteger for  sends like    +     , etc .
  otherwise specialSendClass is nil .
'
%


set class GsComSelectorLeaf
category: '*maglev-Initialization'
classmethod:
newSelector: aSymbol env: envId
  | entry |
  entry :=  SpecialSendsDict at: aSymbol otherwise: nil .
  entry ifNil:[
    ^ aSymbol " a non-optimized send"
  ] ifNotNil:[ | entryEnv |
    entryEnv := entry atOrNil: 4 .
    (entryEnv ~~ nil and:[ entryEnv ~~ envId]) ifTrue:[
       ^ aSymbol "not optimized in this environment"
    ].
    ^ self _basicNew _init: entry sym: aSymbol
  ]
%


set class GsComSelectorLeaf
category: '*maglev-Initialization'
classmethod:
reimplementationAllowed: aSymbol inEnv: anInt
  "returns true if reimplementation is allowed in specified environmentId"
| env |
env := SpecialObjectSends at: aSymbol otherwise: nil .
env ifNotNil:[
  env < 0 ifTrue:[ ^ false "disallowed in all environments"].
  ^ anInt ~~ env 
].
^ true
%


set class GsComSelectorLeaf
category: '*maglev-Initialization'
classmethod:
selectorForSuper: aSymbol
  "cannot optimize a send to super "
  ^ aSymbol
%


set class GsComSelectorLeaf
category: '*maglev-Initialization'
classmethod:
_initializeSpecialSelectors
  | specSendsDict specialISAdict objectSpecSends data |

  " syms are the ruby or Smalltalk selectors that will be sent for
    the specified bytecodes.  comparse.c takes care of
    optimizations for Smalltalk sends of those bytecodes, when compiling
    Smalltalk source. "

  "each triple  in data Array is  
       1. selector to be optimized 
                   (either a Ruby or Smalltalk selector from AST to IR phase)
       2. special send bytecode to use ,   
       3. class on which special send is defined 
       4. environment in which selector is a special send on Object, 
           nil means all environments.
   special sends not defined on class Object get a send-site cache
   during bytecode generation and may fall back to real method"
  		"ruby_selector_suffix dependent"
  data := { 
  { #'+#1__' . Bc_SEND_SPECIAL_PLUS_u1_u32 . SmallInteger . 1 } .   "for Ruby"
  { #'-#1__' . Bc_SEND_SPECIAL_MINUS_u1_u32 . SmallInteger . 1 } . 
  { #'*#1__' . Bc_SEND_SPECIAL_MULTIPLY_u1_u32 . SmallInteger . 1 } . 
  { #'>=#1__' . Bc_SEND_SPECIAL_GTE_u1_u32 . SmallInteger . 1 } . 
  { #'<=#1__' . Bc_SEND_SPECIAL_Lte_u1_u32 . SmallInteger . 1 } . 
  { #'<#1__'  . Bc_SEND_SPECIAL_LT_u1_u32 . SmallInteger . 1 } . 

  { #'+' . Bc_SEND_SPECIAL_PLUS_u1_u32 . SmallInteger . 0 } .   "for Smalltalk"
  { #'-' . Bc_SEND_SPECIAL_MINUS_u1_u32 . SmallInteger . 0 } . 
  { #'*' . Bc_SEND_SPECIAL_MULTIPLY_u1_u32 . SmallInteger . 0 } . 
  { #'>=' . Bc_SEND_SPECIAL_GTE_u1_u32 . SmallInteger . 0 } . 
  { #'<=' . Bc_SEND_SPECIAL_Lte_u1_u32 . SmallInteger . 0 } . 
  { #'<'  . Bc_SEND_SPECIAL_LT_u1_u32 . SmallInteger . 0 } . 

  { #'==' . Bc_SEND_SPECIAL_EQEQ . Object .   0 } . 
  { #'~~' . Bc_SEND_SPECIAL_NENE . Object .   0  } . 

  { #'_equal?#1__' . Bc_SEND_SPECIAL_EQEQ . Object . 1 } .  "Ruby identity compare"
  { #'_not_equal?#1__' . Bc_SEND_SPECIAL_NENE . Object . 1 } .  "Ruby identity compare"
  { #'_is_a?#1__' . Bc_SEND_SPECIAL_RUBYKINDOF_u1 . Object .  1 } .  "Ruby instance of"
  { #'_kind_of?#1__' . Bc_SEND_SPECIAL_RUBYKINDOF_u1 . Object . 1 } . "Ruby kindOf"
  { #'call#0__' . Bc_SEND_CALL_u1_u32 . ExecBlock . 1 } . 
  { #'call#1__' . Bc_SEND_CALL_u1_u32 . ExecBlock . 1 } . 
  { #'call#2__' . Bc_SEND_CALL_u1_u32 . ExecBlock . 1 } . 
  { #'call#3__' . Bc_SEND_CALL_u1_u32 . ExecBlock . 1 } . 
  { #'call#0*_' . Bc_SEND_CALL_STAR_u1_u32 . ExecBlock . 1 } . 
  " any call variation with & does not use  Bc_SEND_CALL (1.8.7) "
  { #'_not#0__' . Bc_rubyNOT . Object . Object . 1 } .  
  { #'_isInteger#0__' . Bc_SEND_SPECIAL_IS_INTEGER . Object . 1 } . 
  { #'_isSmallInteger#0__' . Bc_SEND_SPECIAL_IS_SMALLINT . Object . 1 } . 
  { #'_isFixnum#0__' . Bc_SEND_SPECIAL_IS_SMALLINT . Object . 1 } . 
  { #'_isNumeric#0__' . Bc_SEND_SPECIAL_IS_NUMBER . Object . 1 } . 
  { #'_isFloat#0__' . Bc_SEND_SPECIAL_IS_FLOAT . Object . 1 } . 
  { #'_isSymbol#0__' . Bc_SEND_SPECIAL_IS_SYMBOL . Object . 1 } . 
  { #'_isExecBlock#0__' . Bc_SEND_SPECIAL_IS_ExecBlock . Object . 1 } . 
  { #'_isBlock#0__' . Bc_SEND_SPECIAL_IS_ExecBlock . Object . 1 } . 
  { #'_isArray#0__' . Bc_SEND_SPECIAL_IS_Array . Object . 1 } . 
  { #'_isStringOrSymbol#0__' . Bc_SEND_SPECIAL_IS_OneByteString . Object . 1 } . "used in Ruby"
  { #'_isString#0__' . Bc_SEND_SPECIAL_IS_RubyString . Object .         1 } .  "used in Ruby"
  { #'_isHash#0__' . Bc_SEND_SPECIAL_IS_RubyHash . Object .        1 } . "used in Ruby"
  { #'_isRegexp#0__' . Bc_SEND_SPECIAL_IS_Regexp . Object } . 
  { #'_isRange#0__' . Bc_SEND_SPECIAL_IS_Range . Object } .

  { #_isOneByteString . Bc_SEND_SPECIAL_IS_OneByteString . Object . 0 } . "used in Smalltalk"
  { #_isExceptionClass . Bc_SEND_SPECIAL_IS_ExceptionClass . Object . 0 } . 
  { #_isNumber . Bc_SEND_SPECIAL_IS_NUMBER . Object . 0 } . 
  { #_isScaledDecimal . Bc_SEND_SPECIAL_IS_ScaledDecimal . Object . 0 } . "Smalltalk"
  { #_isRubyString . Bc_SEND_SPECIAL_IS_RubyString . Object .     0 } .  "used in Smalltalk"
  { #_isRubyHash . Bc_SEND_SPECIAL_IS_RubyHash . Object    . 0 } . "used in Smalltalk"
  { #_isInteger . Bc_SEND_SPECIAL_IS_INTEGER . Object . 0 } .
  { #_isSmallInteger . Bc_SEND_SPECIAL_IS_SMALLINT . Object . 0 } .
  { #_isFloat . Bc_SEND_SPECIAL_IS_FLOAT . Object . 0 } .
  { #_isSymbol . Bc_SEND_SPECIAL_IS_SYMBOL . Object . 0 } .
  { #_isExecBlock  . Bc_SEND_SPECIAL_IS_ExecBlock . Object . 0 } .
  { #_isArray . Bc_SEND_SPECIAL_IS_Array . Object . 0 } .
  { #_isString . Bc_SEND_SPECIAL_IS_RubyString . Object .  0 } .
  { #_isRegexp . Bc_SEND_SPECIAL_IS_Regexp . Object . 0 } .
  { #_isRange . Bc_SEND_SPECIAL_IS_Range . Object }

  }.
  objectSpecSends := IdentityKeyValueDictionary new .
  specSendsDict := IdentityKeyValueDictionary new .
  data do:[:triple | | sym bc cls |
    sym := triple at: 1 . bc := triple at: 2 .  cls := triple at: 3 .
    specSendsDict at: sym put: { bc . cls } .
    cls == Object ifTrue:[ | env |
      env := triple atOrNil: 4 .
      env ifNil:[ env := -1 ].
      objectSpecSends at: sym put: env .
    ].
  ].
  objectSpecSends immediateInvariant .
  specSendsDict immediateInvariant .

  specialISAdict := IdentityKeyValueDictionary new .
  specialISAdict   "env 0 selectors used in Ruby coerce_to logic"
        at: Symbol put: #( _isSymbol Symbol ) ;
        at: SmallInteger put: #(  _isSmallInteger  Fixnum ) ;
        at: Float put: #(  _isFloat  Float ) ;
        at: Integer put: #(  _isInteger  Integer ) ;
        at: Number put: #(  _isNumber  Number ) ;
        at: ExecBlock put: #(  _isExecBlock  ExecBlock ) ;
        at: Array put: #(  _isArray  Array ) ;
        at: String put: #(  _isRubyString  String ) ;
        at: RubyHash put: #(  _isRubyHash  Hash ) ;
        at: Regexp put: #(  _isRegexp  Regexp ) ;
        at: Range put: #(  _isRange  Range )  .
  specialISAdict immediateInvariant .

  #( #SpecialSendsDict #SpecialISAselectors #SpecialObjectSends ) do:[:sym|
    self _removeClassVar: sym ifAbsent:[].
  ].
  self  _addInvariantClassVar: #SpecialSendsDict value: specSendsDict ;
    _addInvariantClassVar: #SpecialISAselectors value: specialISAdict ;
    _addInvariantClassVar: #SpecialObjectSends value: objectSpecSends .
%


set class GsComSelectorLeaf
category: '*maglev-Printing'
method:
printFormattedOn: aStream

  super printOn: aStream .
  aStream nextPutAll: selector printString ; 
      print: ' specialOpcode:' int: specialOpcode ; 
      nextPutAll: ' specialSendClass:' ; 
      nextPutAll:(specialSendClass ~~ nil ifTrue:[ specialSendClass name] ifFalse:['nil']) ; 
      nextPut: $) .
%


set class GsComSelectorLeaf
category: '*maglev-Accessing'
method:
selector
  ^ selector
%


set class GsComSelectorLeaf
category: '*maglev-Instance Initialization'
method:
setIRnodeKind
%


set class GsComSelectorLeaf
category: '*maglev-Initialization'
method:
_init: specialSendsEntry sym: aSymbol
  kind := COMPAR_SELECTOR_LEAF .
  selector := aSymbol .
  specialOpcode := specialSendsEntry at: 1 .
  specialSendClass := specialSendsEntry at: 2 .
  ^ self
%

