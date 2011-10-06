
set class Object
category: '*maglev-runtime'
classmethod:
addException: anException
  | res |
  self == Object ifFalse:[ ArgumentTypeError signal:'expected anException or Object'].
  (res := ExceptionSet new)
     addException: Exception ;
     addException: anException .
  ^ res 

%


set class Object
category: '*maglev-cextensions'
classmethod:
cextGlobalVarAt: stringArg  put: aValue
  | assoc aString |
  aString := stringArg .
  aString size == 0 ifTrue:[ ^ nil ].  "rb_gv_set() checks for size 0"
  (aString at: 1) == $$ ifFalse:[
    aString := '$' , aString
  ]. 
  assoc := Object rubyGlobalVarAssoc: aString asSymbol env: 1 .
  assoc globalVarValue: aValue .
  ^ aValue

%


set class Object
category: '*maglev-cextensions'
classmethod:
cextGlobalVarGet: stringArg
  | sym assoc aString |
  aString := stringArg .
  aString size == 0 ifTrue:[ ^ nil ].
  (aString at: 1) == $$ ifFalse:[
    aString := '$' , aString 
  ].
  sym := Symbol _existingWithAll: aString .
  sym ifNil:[ ^ nil ].
  ^ (Object rubyGlobalVarAssoc: sym env: 1 ) globalVarValue

%


set class Object
category: '*maglev-cextensions'
classmethod:
cextGlobalVariables: envId 
  | ns arr |
  ns := Object transientNameSpace: envId .
  arr := { } .
  ns keysAndValuesDo:[ :aKey :aVal |  
    (aKey at: 1) ==  $$  ifTrue:[ arr add: (String withAll: aKey)].
  ].
  "  #'$?'  not currently included  , it is a runtime dynamic value"
  ^ arr 

%


set class Object
category: '*maglev-Ruby private'
classmethod:
rubyPrivateInstSize
^ 0
%


set class Object
category: '*maglev-runtime'
method:
addRubyClassVar: aSymbol  value: aValue  mref: modulRef 
  "called from generated code"
   ^ modulRef _classForRubyClassVar  addRubyClassVar: aSymbol value: aValue env: 1"__callerEnvId"

%


set class Object
category: '*maglev-runtime'
method:
addRubySingletonClass: envId

"Insert a new singleton class in the receiver's class hierarchy.
 Returns receiver, or signals an error.
 The new class is persistable.  "

| vCls prevCls |
prevCls := self virtualClass .
vCls := self _addRubySingletonClass: true envId: envId .
vCls _isOneByteString ifTrue:[
  ArgumentTypeError signal: 'add singleton class disallowed, ', vCls .
].
"env 1 method dicts left as nil"
"name spaces left as nil."
vCls persistentRubySuperclass: envId put: prevCls .
vCls transientRubySuperclass: envId put: prevCls .
  "new singleton class has empty method dicts, so no need to clear
     lookup caches yet"
^ self

%


set class Object
category: '*maglev-Ruby Class Membership'
method:
class
"return first non-virtual non-singleton class at or above receiver's class.

 The old method  Object>>_class is not implemented in extent0.ruby.dbf
"
<primitive: 730>
^ self _primitiveFailed: #class
%


set class Object
category: '*maglev-runtime'
method:
classForConstantLookup: envId forModuleEval: aBoolean 
  "actual usage of this method
    is to get class for constant definition for top level of an eval"
  | cls |
  cls := self virtualClass .
  [ cls isRubySingletonClass or:[ cls isRubyModuleInclude] ] whileTrue:[
    cls := cls rubySuperclass: envId .
  ].
  ^ cls

%


set class Object
category: '*maglev-runtime'
method:
clsMethodDefnTarget
  ^ self class

%


set class Object
category: '*maglev-runtime'
method:
instanceEvalString: aString with: vcGlobalsArr args: argsArr
    "A ruby primitive.
     instance_eval comes here.
     Evaluate aString with self set to this object"
  | defStk envId lexSelfStk  defnTarget  selfCls cld |
  envId := 1"__callerEnvId" . 
  selfCls := self virtualClass   .
  defnTarget := self _singletonClass: envId .
  cld := GsProcess _current _clientData .
  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: defnTarget .
  cld at: 7 put: defnTarget " _rubyThreadDataAt: 7 put: " .

  selfCls == Object ifTrue:[ selfCls := nil ].  "don't need to include top-level"
  (lexSelfStk := RubyCompilerState current lexicalSelfStack ) push: selfCls .
  ^ [  | file line |
      file := argsArr atOrNil: 2 .
      file _stringCharSize == 0 ifTrue:[ file := nil ].
      line := argsArr atOrNil: 3 .
      line _isSmallInteger ifFalse:[ line := 0 ].
      RubyCompiler new evaluateString: aString binding: (argsArr at: 1) with: vcGlobalsArr 
        fileName: file lineNumber: line  env: envId
    ] ensure:[
      defStk pop: defnTarget.
      lexSelfStk pop: selfCls .
    ]

%


set class Object
category: '*maglev-Ruby private'
method:
isMetaOrModule
 ^ false
%


set class Object
category: '*maglev-runtime'
method:
isNameSpace
  ^ false

%


set class Object
category: '*maglev-runtime'
method:
is_aModule
  ^ false

%


set class Object
category: '*maglev-runtime'
method:
nameSpace: envId
  self class == Object ifTrue:[
    ^ Object transientNameSpaceForStore: envId
  ].
  ArgumentTypeError signal:'left side of :: is not a class/module ' .
  ^ nil

%


set class Object
category: '*maglev-runtime'
method:
nameSpaceOrNil: envId
  self class == Object ifTrue:[
    ^ Object transientNameSpaceForStore: envId
  ].
  ^ nil

%


set class Object
category: '*maglev-runtime'
method:
not
	^ false

%


set class Object
category: '*maglev-Ruby Message Handling'
method:
perform: aSymbol env: envId

<primitive: 2014>
^ self _doesNotUnderstand: aSymbol args: #() envId: envId reason: 0
%


set class Object
category: '*maglev-runtime'
method:
rubyConstAssociationAt: aSymbol
  ArgumentTypeError signal: 'left side of :: is neither a class nor module'

%


set class Object
category: '*maglev-runtime'
method:
rubyConstAssociationAt: aSymbol env: envId
  ArgumentTypeError signal: 'left side of :: is neither a class nor module' .
  ^ nil

%


set class Object
category: '*maglev-runtime'
method:
rubyConstAssociationAtOrNil: aSymbol env: envId
  ArgumentTypeError signal: 'left side of :: is neither a class nor module' .
  ^ nil

%


set class Object
category: '*maglev-runtime'
method:
rubyConstAt: aName env: envId put: aValue
  ArgumentTypeError signal:'left side of :: is not a class/module '

%


set class Object
category: '*maglev-runtime'
method:
rubyConstAt: aSymbol put: aValue
  ArgumentTypeError signal:'left side of :: is not a class/module '

%


set class Object
category: '*maglev-runtime'
method:
rubyConstDecl: aSymbol put: aValue
   "called from generated code"
  ^ self virtualClass rubyConstAt: aSymbol env: 1"__callerEnvId" put: aValue .

%


set class Object
category: '*maglev-runtime'
method:
rubyEval1: lexPath block: aBlock
  "A ruby primitive, for instance_eval.  lexPath is ignored .
   Evaluate aBlock with the block's self set to this object"
  | stk defnTarget cld |
  cld := GsProcess _current _clientData .
  stk := cld at: 3 . " _rubyThreadDataAt: 3 , meth def target stack"
  stk push: (defnTarget := self _singletonClass: 1 ).
  cld at: 7 put: defnTarget . "_rubyThreadDataAt: 7 put: "
  ^ [ | val |
      aBlock ifNotNil:[ | blk |
        blk := aBlock setSelf: self . "copies aBlock if needed"
        val := blk @ruby1:value: self  .   
      ] .
      val
    ] ensure:[
      stk pop: defnTarget
    ]

%


set class Object
category: '*maglev-runtime'
method:
rubyInstvarAt: aString
  "a ruby primitive. "

  | sym stSym |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString _rubyAt1: 0) == 64"$@" ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ].
  ((aString _rubyAt1: 1) == 95"$_" and:[ aString at: 2 equals:'_st_' ]) ifTrue:[
    stSym := (aString copyFrom: 6 to: aString size) asSymbol .
    sym := stSym
  ] ifFalse:[
    sym := aString asSymbol .
    stSym := (aString copyFrom: 2 to: aString size) asSymbol .
  ].
  ^ self _rubyInstvarAt: { stSym . sym . nil . 0 }

%


set class Object
category: '*maglev-runtime'
method:
rubyInstvarAt: aString put: aValue 
  "a ruby primitive "
  ^ self rubyInstvarAt: aString put: aValue env: 1"__callerEnvId" 

%


set class Object
category: '*maglev-runtime'
method:
rubyInstvarAt: aString put: aValue env: envId
  "Returns aValue"
  | sym  stSym |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString _rubyAt1: 0) == 64"$@" ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name', self class rubyName
  ].
  ((aString _rubyAt1: 1) == 95"$_" and:[ aString at: 2 equals:'_st_' ]) ifTrue:[
    stSym := (aString copyFrom: 6 to: aString size) asSymbol .
    sym := stSym
  ] ifFalse:[
    sym := aString asSymbol .
    stSym := (aString copyFrom: 2 to: aString size) asSymbol
  ].
  ^ self _rubyInstvarAt: { stSym . sym . nil . 0 } put: aValue
             privateSize:  self rubyPrivateSize

%


set class Object
category: '*maglev-runtime'
method:
rubyInstVarDefined: aSymbol 
  "support method for generated code"
  ^ self _rubyInstVarDefinedQ: aSymbol

%


set class Object
category: '*maglev-runtime'
method:
rubyInstvarNames

"A ruby primitive.
 Returns an Array of Strings which are names
 of named and dynamic instVars of the receiver visible to Ruby.
 names in the result start with '@' ."

| arr sz res  |
arr :=  self _instvarNamesAfter: self rubyPrivateSize .
res := Array new: (sz := arr size) .
1 to: sz do:[ :n | | sym |
   sym := arr at: n .
   (sym at: 1) == $@
     ifTrue:[  res at: n put:( String withAll: sym)]
     ifFalse:[ | nam symSiz |
       nam := String new: (symSiz := sym size) + 5 .
       nam replaceFrom: 1 to: 5 with: '@_st_' startingAt: 1 .
       nam replaceFrom: 6 to: symSiz + 5 with: sym startingAt: 1 .
       res at: n put: nam
   ].
].
^ res

%


set class Object
category: '*maglev-runtime'
method:
rubyIvDefined: aString
  "a ruby primitive"
  | sym |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString at: 1) == $@ ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ].
  ((aString at: 2) == $_ and:[ aString at: 2 equals:'_st_' ]) ifTrue:[
    sym := (aString copyFrom: 6 to: aString size) asSymbol
  ] ifFalse:[
    sym := aString asSymbol
  ].
  ^ self _rubyInstvarDefined: { sym . sym . nil . 0 }

%


set class Object
category: '*maglev-runtime'
method:
rubyMethod: aSymbol
  "a ruby primitive, and called from generated code.
   Returns an instance of RubyMeth , or signals a NameError"
   |  m  |
   m := self virtualClass rubyMethodFor: aSymbol env: 1"__callerEnvId" .
   m object: self .
   ^ m

%


set class Object
category: '*maglev-runtime'
method:
rubyMethods: includeSuper protection: protInt
  "a ruby primitive"
  ^ self virtualClass rubyMethods: includeSuper protection: protInt 
            env: 1"__callerEnvId"

%


set class Object
category: '*maglev-runtime'
method:
rubyPrint: aString
	GsFile gciLogClient: aString

%


set class Object
category: '*maglev-Ruby private'
method:
rubyPrivateSize
^ 0  "inline Object instSize"
%


set class Object
category: '*maglev-runtime'
method:
rubyRemoveIv: aString
  "a ruby primitive"
  | sym descr |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString at: 1) == $@ ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ].
  ((aString at: 2) == $_ and:[ aString at: 2 equals:'_st_' ]) ifTrue:[
    sym := (aString copyFrom: 6 to: aString size) asSymbol
  ] ifFalse:[
    sym := aString asSymbol
  ].
  descr := { sym . sym . nil . 0 } . 
  (self _rubyInstvarDefined: descr) == false ifFalse:[ | val |
      val := self _rubyInstvarAt: descr .
      self _rubyInstvarAt: descr put: _remoteNil privateSize: self rubyPrivateSize .
      ^ val
  ] ifTrue:[
     NameError signal:'instance variable ' , sym , ' not defined'
  ]

%


set class Object
category: '*maglev-runtime'
method:
rubySelectorForFrame: anInteger
	^ (GsProcess _methodInFrameContents: (GsProcess _frameContentsAt: anInteger)) selector

%


set class Object
category: '*maglev-runtime'
method:
rubySingletonClass
  " a ruby primitive"
  ^ self _singletonClass: 1"__callerEnvId" 

%


set class Object
category: '*maglev-runtime'
method:
rubySingletonClassForExtend
  "a ruby primitive"
  | envId cls |
  envId := 1 "__callerEnvId" .
  self class == Module ifTrue:[   "a  class<<self   within a  module "
    cls := self moduleMethodsModuleOrNil ifNil:[
       cls := self _rubyModuleIncludeSelfEnv: envId
    ].
  ] ifFalse:[
    self isBehavior ifTrue:[
       cls := self isMeta ifTrue:[ self _singletonClassFor: envId ]
                  ifFalse:[ self virtualClass "extending metaclass" ].
    ] ifFalse:[ cls := self _singletonClassFor: envId ].
  ].
  ^ cls

%


set class Object
category: '*maglev-runtime'
method:
rubySingletonMethods: includeModules protection: protInt
  "a ruby primitive"
  ^ self rubySingletonMethods: includeModules protection: protInt 
     env: 1"__callerEnvId"

%


set class Object
category: '*maglev-runtime'
method:
rubySingletonMethods: includeModules protection: protInt env: envId
  "Return an IdentitySet of Symbols, the names of the singleton methods for receiver.
   If receiver has no singleton methods, return an empty array.
   If includeModules is true, then also include methods from mixed in
   modules."
  | curClass set hidden |
  set := IdentitySet new .  hidden := IdentitySet new .
  curClass := self virtualClass .

  "For a class, the singleton class is the meta class"
  self isBehavior ifTrue: [
    self theMetaClass nonBridgeRubySelectorsInto: set hiddenInto: hidden protection: protInt env: envId.
    includeModules ifTrue: [ | c |
      c := curClass .
      [ c ~~ nil and: [ c ~~ Object and: [ c ~~ Object theMetaClass ] ] ] whileTrue: [
      c nonBridgeRubySelectorsInto: set hiddenInto: hidden protection: protInt env: envId .
      c := c rubySuperclass: envId  .
      ] .
    ] .
  ] .

  [ curClass ~~ nil and: [ curClass isRubyVirtual ] ] whileTrue: [
    (curClass _isIncludableInSingletons: includeModules) ifTrue:[ 
	   curClass nonBridgeRubySelectorsInto: set hiddenInto: hidden protection: protInt
	               env: envId 
	 ] .
     curClass := curClass rubySuperclass: envId .
  ] .
  ^ set 

%


set class Object
category: '*maglev-Ruby Class Membership'
method:
virtualClass
"return receiver's class which may be a singleton or virtual class"
<primitive: 610>
^ self _primitiveFailed: #virtualClass
%


set class Object
category: '*maglev-Ruby Message Handling'
method:
with: argOne perform: aSymbol env: envId 

<primitive: 2014>
^ self _doesNotUnderstand: aSymbol args: { argOne }  envId: envId reason: 0
%


set class Object
category: '*maglev-Ruby Message Handling'
method:
with: argOne with: argTwo perform: aSymbol env: envId

<primitive: 2014>
^ self _doesNotUnderstand: aSymbol args: { argOne . argTwo }  envId: envId reason: 0
%


set class Object
category: '*maglev-Ruby Message Handling'
method:
with: argOne with: argTwo with: argThree block: blk perform: aSymbol env: envId

<primitive: 2014>
^ self _doesNotUnderstand: aSymbol args: { argOne . argTwo . argThree . blk }  envId: envId reason: 0
%


set class Object
category: '*maglev-Ruby Message Handling'
method:
with: argOne with: argTwo with: argThree perform: aSymbol env: envId

<primitive: 2014>
^ self _doesNotUnderstand: aSymbol args: { argOne . argTwo . argThree }  envId: envId reason: 0
%


set class Object
category: '*maglev-Ruby Message Handling'
method:
withArgs: anArray block: aBlock perform: aSymbol env: envId

<primitive: 2014>
"Assume aSymbol is of form #'foo#0*&' "
| prefix |
prefix := aSymbol rubySelectorPrefixSymbol .
^ self _doesNotUnderstand: prefix args: anArray block: aBlock envId: envId
%


set class Object
category: '*maglev-Ruby Message Handling'
method:
withArgs: anArray perform: aSymbol env: envId

<primitive: 2014>
"Assume aSymbol is of form #'foo#0*_' "
| prefix |
prefix := aSymbol rubySelectorPrefixSymbol .
^ self _doesNotUnderstand: prefix args: anArray block: nil envId: envId
%


set class Object
category: '*maglev-Ruby Message Handling'
method:
withBlock: argOne perform: aSymbol env: envId

<primitive: 2014>
"Assume aSymbol is of form #'foo#0_&' "
| prefix |
prefix := aSymbol rubySelectorPrefixSymbol .
^ self _doesNotUnderstand: prefix args: {} block: argOne envId: envId
%


set class Object
category: '*maglev-Ruby private'
method:
_addRubySingletonClass: committedOk envId: envId

"Insert a new singleton class in the receiver's class hierarchy.
 Returns the new class, or a String giving reason for failure
"
<primitive: 682>
committedOk _validateClass: Boolean .
envId _validateClass: SmallInteger .
(envId < 1 or:[ envId > 255]) ifTrue:[ OutOfRange signal:'invalid envId'].
self _primitiveFailed: #_addRubySingletonClass:envId:
     args: { committedOk . envId }
%


set class Object
category: '*maglev-Ruby private'
method:
_atCachedIv: anOffset put: aValue

"Returns aValue. Receiver is expected to be a literal object that will
 be referenced by memory pointer from a loaded method.  Store aValue
 into specified instVar without marking receiverDirty.  Used for
 RubyDRegexpOnceLiteral and similar ruby literals"

<primitive: 813>
"receiver is large object, or anOffset is out of range of named instVars"
self _primitiveFailed: #_atCachedIv:put: args: { anOffset . aValue }
%


set class Object
category: '*maglev-Ruby private'
method:
_bindingContext: numFrames
"Returns nil
     or  an Array , { aVariableContext . methodDefTarget . aGsNMethod . ... . homeGsNMethod}
 for the frame which is numFrames above sender's sender's frame.
 The result contains the VariableContext for specified frame (if any),
 and a list of methods active on
 stack from current block(if any) up to homeMethod(if found),
 based on searching the stack for references to the sender's VariableContext.
 Last element of result is  nil  if homeMethod is not found."

| arr |
arr := self __bindingContext: numFrames + 1 .
arr ifNotNil:[
  arr at: 2 put: GsProcess currentMethDefTarget
].
^ arr
%


set class Object
category: '*maglev-runtime'
method:
_classForRubyClassVar
  ^ self virtualClass _classForRubyClassVar

%


set class Object
category: '*maglev-Ruby Message Handling'
method:
_doesNotUnderstand: aPrefix args: anArray block: aBlock envId: envId 
  "used by ruby send implementation"

envId ~~ 0  ifTrue: [ 
  aPrefix ~~ #method_missing ifTrue:[
    aBlock ifNil:[
      ^ self @ruby1:method_missing: aPrefix __STAR: anArray 
    ] ifNotNil:[ 
      ^ self @ruby1:method_missing: aPrefix __STAR: anArray __BLOCK: aBlock
    ].
    ^ self doesNotUnderstand: aPrefix args: { anArray . aBlock } envId: envId
  ].
] ifFalse: [
  "smalltalk env 0"
  ^ self doesNotUnderstand: { aPrefix . anArray . aBlock }
].
%


set class Object
category: '*maglev-Ruby Message Handling'
method:
_doesNotUnderstand: aSymbol args: anArray envId: envId reason: dnuKind

"Send from within the VM.
 Generates an error reporting that the receiver cannot respond to a message.
 because no compiled method was found for aSymbol in method environment
 envId.  If envId is 1 (Ruby), then call _method_missing to hook into the Ruby handling.
 envId is a SmallInteger, 0 for Smalltalk , 1 for Ruby , 
 2..255 for future use by Smalltalk package managers .

 This implementation must be in the persistent method dictionary for Object, 
 since the method is preloaded during VM startup.  
 Reimplementations in a session methods dictionary will not be seen.
"
dnuKind ~~ 0 ifTrue:[ self _dnuError: dnuKind args: anArray reason: dnuKind ]. 
envId == 1  ifTrue: [ |  prefix |
  prefix := aSymbol rubySelectorPrefixSymbol .   "Fix Trac 913"
  "prefix == #'a_prefix' ifTrue:[ nil pause ].  uncomment for debugging Ruby DNU"
  prefix ~~ #method_missing ifTrue:[
    (aSymbol _rubyAt1: -1) == 16r26 "== $& " ifTrue:[ | args blk |
      args := anArray _rubyAt: 0 length: anArray size - 1 .
      blk := anArray _rubyAt: -1 .
      ^ self @ruby1:method_missing: prefix __STAR: args __BLOCK: blk 
    ].
    ^ self @ruby1:method_missing: prefix __STAR: anArray 
  ]. 
  ^ self doesNotUnderstand: aSymbol args: anArray envId: envId
] ifFalse: [
  "smalltalk env 0"
  ^ self doesNotUnderstand: { aSymbol . anArray }
].
%


set class Object
category: '*maglev-Ruby private'
method:
_getRubyVcGlobal: varIdx

" varDescr is a SmallInteger containing two fields:
    varIdx := varDescr bitAnd: 16rF
    caller := varDescr bitShift: - 4
  varIdx == 0 , Ruby scope-local global  $~
            1 , Ruby scope-local global  $_
            2 , Ruby scope-local eval lexical path
  caller specifies how many frames up stack to go to fetch $~
  and must be >= 2 . Returns nil if the caller value
  exceeds the stack depth.

 Returns value of $~ or $_ from specified frame.
 Returns nil if specified global
   does not exist in specified frame's home context.
"
<primitive: 773>
self _primitiveFailed: #_getRubyVcGlobal: args: { varIdx }
%


set class Object
category: '*maglev-Ruby Class Membership'
method:
_isRubyString
"(Optimized selector.)  Returns false if the receiver is a Symbol,
 else if the receiver is an instance of String or a subclass thereof returns true,
 else returns false."

^ self _isRubyString
%


set class Object
category: '*maglev-runtime'
method:
_methodDefTargetClass: envId
  | cls | 
  cls := self .
  self isBehavior ifFalse:[ cls := self virtualClass ]. 
  cls isRubySingletonClass ifTrue:[ ^ cls ].
  [ cls isRubyVirtual ] whileTrue:[
     cls := cls rubySuperclass: envId
  ].
  ^ cls 

%


set class Object
category: '*maglev-runtime'
method:
_nameForMethodMissing
"A ruby primitive , part of fix for Trac 752"

 ^ self virtualClass rubyFullName: 1"__callerEnvId"

%


set class Object
category: '*maglev-Ruby private'
method:
_respondsTo: aSymbol private: includePrivateBoolean flags: flags

"Returns true if receiver understands specified selector, false otherwise.
 flags is a Smallinteger with bit masks 
    environmentId                  16rFF  
    ruby lookup semantics         16r100
    ruby receiver is self        16r1000 (for future use)
    cache successes in code_gen 16r10000 
    all other bits ignored
"
<primitive: 688>
| envId |
envId := 1"__callerEnvId" .
aSymbol _isSymbol ifFalse:[ | sym |
  aSymbol _isOneByteString ifTrue:[
    "avoid creating symbols unnecessarily"
    (sym := Symbol _existingWithAll: aSymbol) ifNil:[ ^ false ].
  ].
  sym := [ 
    aSymbol @ruby1:to_sym 
  ] onSynchronous: Exception do:[:ex | 
    ArgumentError signal: 'expected a Symbol or String'
  ].
  sym _isSymbol ifTrue:[
    ^ self _respondsTo: sym private: includePrivateBoolean flags: flags
  ].
].
self _primitiveFailed:#_respondsTo:private:flags: 
     args: { aSymbol . includePrivateBoolean . flags }
%


set class Object
category: '*maglev-Ruby private'
method:
_rubyBasicDup

"Returns a copy of the receiver which shares the receiver's instance
 variables, using the non-singleton class of the receiver.
"

<primitive: 808>
self _primitiveFailed: #_rubyBasicDup .
self _uncontinuableError
%


set class Object
category: '*maglev-Ruby private'
method:
_rubyBasicDupNamedIvs
  "Returns a copy of the receiver with named instVars (both fixed and dynamic)
   copied from the receiver,  and without copying any varying instVars.

   Example if receiver is an instance of a subclass of Array, 
   the result will have varing size 0,
   and will replicate all Ruby named instVars of the receiver. 
  "
  <primitive: 873>
self _primitiveFailed: #_rubyBasicDupNamedIvs
%


set class Object
category: '*maglev-runtime'
method:
_rubyBindingCtx
  "Private,  only for use in building TOPLEVEL_BINDING "
  ^ self _bindingContext: 0

%


set class Object
category: '*maglev-runtime'
method:
_rubyClassVarDefinedQ: aSymbol mref: modulRef 
  "called from generated code"
  ^ modulRef _rubyClassVarDefinedQ: aSymbol env: 1"__callerEnvId"

%


set class Object
category: '*maglev-runtime'
method:
_rubyClassVarGet: aSymbol  mref: modulRef 
  "called from generated code"
  ^ modulRef _classForRubyClassVar _rubyClassVarGet: aSymbol env: 1"__callerEnvId"

%


set class Object
category: '*maglev-runtime'
method:
_rubyClassVarGetOrNil: aSymbol  mref: modulRef 
  "called from generated code"
  ^ modulRef _rubyClassVarGetOrNil: aSymbol env: 1"__callerEnvId"

%


set class Object
category: '*maglev-Ruby enumeration'
method:
_rubyEach1: aBlock
  "A ruby primitive,
   All Ruby definitions of each&  compiled as definition of each&
   All Ruby sends of each&   compiled as send of __each&
   Object.rb  has    primitive_nobridge_env '__each&', _rubyEach ...  "
   
  ^ [
      "@ruby1:__each__:  translated to env1 send of #'each#0_&' by smalltalk parser"
      self @ruby1:__each__: aBlock
        "Ruby next   is handled by Bc_RUBY_NEXT bytecode which does a
         return from aBlock, thus letting the #'each&'  method
         process the next element of the enumeration"
    ] onException: RubyBreakException do: [:ex | | args |
      args := ex gsArguments .
      (args at: 1) ifTrue:[  "Ruby break, terminate enumeration"
        ^ args at: 2
      ] ifFalse:[
        ex retry "Ruby retry,  restart the enumeration"
      ]
    ] .
%


set class Object
category: '*maglev-Ruby compare'
method:
_rubyEqualQ: anObject
  ^ self == anObject
%


set class Object
category: '*maglev-Ruby private'
method:
_rubyInitializeFrom: anObject

"copy fixed and dynamic instVars from anObject into self, growing
 self if needed to hold the dynamic instVars."

<primitive: 698>
"primitive fails if receiver and arg byte format and number of bytes different"
self _primitiveFailed: #_rubyInitializeFrom: args: { anObject } .
self _uncontinuableError
%


set class Object
category: '*maglev-runtime'
method:
_rubyInspect
  "a ruby primitive"
  | str names ts envId |
  envId := 1"__callerEnvId" .
  str := '#<' copy .
  str addAll: (self class rubyFullName: envId)  .
  ts := GsProcess _recursionGuardSet .
  (ts _addIfAbsent: self) ifFalse:[  "already in ts"
    str addAll: '...>' .  
   ^ str 
  ].
  [ | s |
    s := str .
    s addAll: ':0x'.
    s addAll: self asOop hex.
    names := self  rubyInstvarNames . 
    1 to: names size do:[:n | | nam |
      s add: $  .
      s addAll: (nam := names at: n)  ; add: $=  .
      s addAll: ((self rubyInstvarAt: nam asSymbol ) @ruby1:inspect ). 
    ].
    s add: $> . 
  ] ensure:[
    ts remove: self
  ].
  ^ str

%


set class Object
category: '*maglev-Ruby private'
method:
_rubyInstvarAt: descrArray
"Return the value of the specified instance variable of the receiver,
 or nil if there is no such instance variable.
 Called from generated code. 

 descrArray is { stSymbol . rubySymbol . cachedClass . cachedOffset }
 stSymbol is used to search for fixed instVars, rubySymbol used to access
 dynamic instVars .

 Sends of this method are generated by IR generation phase of .mcz code.
"
<primitive: 768>  "prim fails if  defined?(instVar) would be false"
^ nil 
%


set class Object
category: '*maglev-Ruby private'
method:
_rubyInstvarAt: descrArray put: aValue privateSize: privateSize
"Stores aValue of the specified instance variable of the receiver,
 creating a dynamic instance variable if needed.
 Returns aValue.
 Called from generated code.

 descrArray is { stSymbol . rubySymbol . cachedClass . cachedOffset }
 stSymbol is used to search for fixed instVars, rubySymbol used to access
 dynamic instVars .

 Signals a NameError if instVar found is private to
 Smalltalk or if receiver is a special object.

 Sends of this method are generated by IR generation phase of .mcz code.
"
<primitive: 769>
descrArray _validateClass: Array .
self _primitiveFailed:  #_rubyInstvarAt:put:privateSize:
     args: { descrArray . aValue . privateSize }
%


set class Object
category: '*maglev-Ruby private'
method:
_rubyInstvarDefined: descrArray
"Return the value of the specified instance variable of the receiver,
 or false if there is no such instance variable.
 Called from generated code. 

 descrArray is { stSymbol . rubySymbol . cachedClass . cachedOffset }
 stSymbol is used to search for fixed instVars, rubySymbol used to access
 dynamic instVars .

 Sends of this method are generated by IR generation phase of .mcz code.
"
<primitive: 768>  "prim fails if  defined?(instVar) would be false"
^ false 
%


set class Object
category: '*maglev-Ruby private'
method:
_rubyInstVarDefinedQ: varDescr

"Used to implement Ruby      defined? @ivname

 varDescr must be a zero-based SmallInteger offset of a named
 instVar , or a Symbol naming a  dynamic instVar  .
 Returns #expression if the instVar is defined, otherwise returns nil.

 For a named instVar, defined means a ruby bytecode has stored
 into the instVar, and is determined by iv value ~~ remoteNil .
"
<primitive: 781>
self _primitiveFailed: #_rubyInstVarDefinedQ: args: { varDescr }
%


set class Object
category: '*maglev-Ruby private'
method:
_rubyKindOf: aClass
  "a ruby primitive, to handle sends of kind_of? , is_a? "
  ^ self _rubyKindOf: aClass env: 1"__callerEnvId" 
%


set class Object
category: '*maglev-Ruby private'
method:
_rubyKindOf: aClass env: envId
  <primitive: 825>
  envId _validateClass: SmallInteger .
  self _primitiveFailed: #_rubyKindOf:env: args: { aClass . envId }
%


set class Object
category: '*maglev-Ruby private'
method:
_rubyNext: kindInt with: anFP

"Implements Ruby  'next' and 'redo' keywords .
 kindInt is  0 for next, 1 for redo .
 The argument to 'next' is the receiver.
 anFP must be nil at entry, and the stack word
 corresponding to anFP is private to the primitive."

<primitive: 2007>
self _primitiveFailed: #_rubyNext:with: args: { kindInt . anFP }
%


set class Object
category: '*maglev-Ruby support'
method:
_rubyNilQ

"implements  nil?  "

^ self == nil
%


set class Object
category: '*maglev-runtime'
method:
_rubyTo: anInteger

^ Range from: self to: anInteger 

%


set class Object
category: '*maglev-runtime'
method:
_rubyTo_: anInteger

^ Range from: self limit: anInteger 

%


set class Object
category: '*maglev-Ruby support'
method:
_ruby_eqlQ: anObject

^ self == anObject
%


set class Object
category: '*maglev-runtime'
method:
_singletonClass: envId 
  | cls |
  self class == Module ifTrue:[   "a  class<<self   within a  module "
    cls := self moduleMethodsModuleOrNil ifNil:[ 
       cls := self _rubyModuleIncludeSelfEnv: envId 
    ].
  ] ifFalse:[  
    self isBehavior ifTrue:[ 
       cls := self virtualClass . "extending metaclass"
    ] ifFalse:[ cls := self _singletonClassFor: envId ].
  ].
  ^ cls 

%


set class Object
category: '*maglev-runtime'
method:
_singletonClassFor: envId
   | cls |
   (cls := self virtualClass)  isRubySingletonClass ifFalse:[
      self addRubySingletonClass: envId .
      cls := self virtualClass .
      cls isRubySingletonClass ifFalse:[ self error:'_singletonClassFor:, creation failed' ].
   ].
   ^ cls

%


set class Object
category: '*maglev-Ruby private'
method:
_storeRubyVcGlobal: varDescr

" varDescr is a SmallInteger containing two fields:
    varIdx := varDescr bitAnd: 16rF
    caller := varDescr bitShift: - 4
  varIdx == 0 , Ruby scope-local global  $~
            1 , Ruby scope-local global  $_
            2 , Ruby scope-local eval lexical path
  caller specifies how many frames up stack to go to fetch $~
  and must be >= 2 . Returns nil if the caller value
  exceeds the stack depth.

 Stores the receiver into 's  $~ or $_ of specified frame,
 if frame's home context contains a reference to $~ or $_ .
 Returns receiver if a store was done,  returns nil if
 no reference to the global was found in specified frame's home context
"
<primitive: 772>
self _primitiveFailed: #_storeRubyVcGlobal: args: { varDescr }
%


set class Object
category: '*maglev-Ruby private'
method:
__bindingContext: numFrames
"Returns nil 
     or  an Array , { aVariableContext . nil . aGsNMethod . ... . homeGsNMethod} 
 for the frame which is numFrames above sender's sender's frame. 
 The result contains the VariableContext for specified frame (if any), 
 and a list of methods active on
 stack from current block(if any) up to homeMethod(if found), 
 based on searching the stack for references to the sender's VariableContext.
 Last element of result is  nil  if homeMethod is not found.
"
<primitive: 788>  
numFrames _validateClass: SmallInteger .
self _primitiveFailed: #__bindingContext: args: { numFrames }
%


set class Object
category: '*maglev-Ruby private'
method:
__callerEnvId

  AbstractException signal:'__callerEnvId should not be used'

"PREVIOUS USE, when using env2 for ruby compiler written in ruby"
  "Returns envId of the caller of the method containing the send of __callerEnvId"

  "Since Maglev is no longer using env 2 for the ruby compiler, we
   have hard coded   1""__callerEnvId"" in the Smalltalk methods for Maglev,
   so that when the caller is a _returnToC frame (from C extension) we
   will still get correct env.  Alternatively, the callback from C extension
   into the VM would need to push a different kind of _returnToC frame that
   showed as enviroment 1 ..."

 " ^ self __callerEnvId"  "an optimized selector , not a recursive send"
%


set class Object
category: '*maglev-Ruby enumeration'
method:
__superEach: aBlock
  "A ruby primitive in env 1 only .
   source copied by code in RubyCompiler and compiled in a Ruby class ,
   using smalltalk parsing, and 
   when a super send of #'each&' found  in a Ruby method in that class."
  
  ^ [
      "@ruby1:__each__:  translated to env1 send of #'each#0_&' by smalltalk lexer,
        comgen.c  uses RUBY_SUPER bytecode for send to super with env >= 1. "
      super @ruby1:__each__: aBlock
        "Ruby next   is handled by Bc_RUBY_NEXT bytecode which does a
         return from aBlock, thus letting the #'each&'  method
         process the next element of the enumeration"
    ] onException: RubyBreakException do: [:ex | | args |
      args := ex gsArguments .
      (args at: 1) ifTrue:[  "Ruby break, terminate enumeration"
        ^ args at: 2
      ] ifFalse:[
        ex retry "Ruby retry,  restart the enumeration"
      ]
    ] .
%


set class Object
category: '*maglev-Ruby private'
method:
__threadRubyEnvId
  AbstractException signal:'__threadRubyEnvId: should not be used'

"PREVIOUS USE, when using env2 for ruby compiler written in ruby"
  "Return value last saved by __threadSaveCallerEnv or __threadRubyEnvId:"

"  ^ self __threadRubyEnvId"  "an optimized selector , not a recursive send"
%


set class Object
category: '*maglev-Ruby private'
method:
__threadRubyEnvId: envId

  AbstractException signal:'__threadRubyEnvId: should not be used'
"PREVIOUS USE, when using env2 for ruby compiler written in ruby"
  "Set the current GsProcess's rubyEnvId .
   Returns previous value of rubyEnvId ."
" <primitive: 836> 

  self _primitiveFailed: #__threadRubyEnvId: args: { envId }
"
%


set class Object
category: '*maglev-Ruby private'
method:
__threadSaveCallerEnv

  AbstractException signal:'__threadSaveCallerEnv should not be used'
"PREVIOUS USE, when using env2 for ruby compiler written in ruby"
  "save __callerEnvId in current GsProcess's rubyEnvId .
   Not yet reentrant .  
   Returns the new value of rubyEnvId."
" <primitive: 835> 

  self _primitiveFailed: #_threadSaveCallerEnv
"
%

