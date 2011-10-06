
set class RubyConstantRef
category: '*maglev-Documentation'
classmethod:
comment
^ 'RubyConstantRef is a subclass of Array .
   Instances of RubyConstantRef are used as literals in methods
   to implement access to a Ruby constant. 

   instVars
      lexPathSize : a SmallInteger , 0 means dynamic ArgumentTypeError

      globalAssoc : cached instance of a RubySymbolAssociation, or nil

      varying instVars :
       self[1..lexPathSize-1] is lexicalPath in order to be searched, 
          elements are instances of Module ,
                   or Arrays of size 1 , referencing a Module . 
       self[lexPathSize] is top-level scope, i.e.  Object .
       self[lexPathSize+1] is nil, or class of self for an instance eval
       self[lexPathSize+2 .. self size] are Symbols from the path terms
           as they appear in Ruby source code.
           For a reference   x = A::B ,  self[self size] == #B
   '
%


set class RubyConstantRef
category: '*maglev-runtime'
classmethod:
_abstractCall: rcvrBlock definedQ: selectorSym
    "returns  'method'  if receiver responds to aSymbol, nil otherwise
     flags say don't cache result in code_gen, ruby lookup, env 1"

  | rcvr |
  rcvr := [ rcvrBlock value ] onException: Exception do:[ :ex | ^ nil  ].
  ^ (rcvr _respondsTo: selectorSym private: true flags: 16r00101) 
       ifTrue:[ 'method' copy ] 
  

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
c3name: aSymbol
  "used by RubyColon3Node "
  lexPathSize := 1 .
  self size: 3 ;
       at: 1 + 2 put: aSymbol ;
       at: 1 + 1 put: nil "evalSelfCls" ;
       at: 1 put: Object .
  "globalAssoc left as nil" 

%


set class RubyConstantRef
category: '*maglev-as yet unclassified'
method:
definedQconst
  "called from generated code"
  ^ self definedQconst: 1"__callerEnvId"
%


set class RubyConstantRef
category: '*maglev-as yet unclassified'
method:
definedQconst: envId
<primitive: 841>  "primitive does  globalAssoc:=nil if
        cached globalAssoc no longer valid due to serial number increment.
        primitive always fails."
| sym lpSize idx assoc n mySize aModule prevAssoc rns firstCls parent |
(assoc := globalAssoc) ifNotNil:[
  ^ assoc definedQ "use cached value from previous resolve"
].
lpSize := lexPathSize .     mySize := self size .
lpSize ifNil:[
   ArgumentTypeError signal:'left hand side of :: is not a class or module'
].
idx := lpSize + 2.
sym := self at: idx  .  idx := idx + 1 .
n := 1 .
[ n <= lpSize ] whileTrue:[ "search specified lex scopes "
  aModule := self at: n .    n := n + 1 .
  aModule ifNotNil:[
    firstCls ifNil:[ firstCls := aModule] .
    parent := aModule .
    rns := aModule nameSpace: envId .
    rns ifNotNil:[ assoc := rns resolveConstant: sym ].
  ].
  assoc ifNotNil:[
    prevAssoc := assoc .
    assoc isDefined ifNotNil:[ n := lpSize + 1 "terminate while loop" ]
                       ifNil:[ assoc := nil "keep searching"].
  ].
].
assoc ifNil:[ | cls |  "search inheritance hierarchy"
  cls := self at: lpSize + 1 . "instanceEvalSelfCls"
  cls ifNil:[
    firstCls ifNil:[
      cls := Object
    ] ifNotNil:[
      firstCls == Kernel ifTrue:[ cls := Object]
                        ifFalse:[ cls := firstCls rubySuperclass: envId ].
    ].
  ].
  [ cls ~~ nil and:[ assoc == nil] ] whileTrue:[ "dynamic lookup"
     "probe both normal and virtual classes"
     parent := cls .
     (rns := cls nameSpace: envId) ifNotNil:[
        assoc := rns resolveConstant: sym .
        assoc ifNotNil:[
         prevAssoc := assoc .
         assoc isDefined ifNil:[ assoc := nil ].
        ].
     ].
     cls := cls rubySuperclass: envId .
  ].
  assoc ifNil:[
    idx > mySize ifTrue:[ assoc := prevAssoc ].
    assoc ifNil:[ ^ nil ].
  ].
].
[ idx <= mySize ] whileTrue:[  |val |
  "evaluate second and subsequent :: terms"
  assoc isDefined ifNil:[ ^ nil ].
  (val := assoc _valueNoAction) ifNil: [ 
    "We're dealing with a RubyAutoloadAssociation, Trac 937"
    val := assoc _valueFor: sym inClass: parent env: envId  . "do autoload"
    assoc isDefined ifNil: [ ^ nil ] .
    val := assoc _valueNoAction .
  ].
  parent := val .
  assoc := val rubyConstAssociationAtOrNil: (self at: idx) env: envId .
  assoc ifNil:[ ^ nil ].
  idx := idx + 1
].
self setGlobalAssoc_noMarkDirty: assoc .
^ assoc definedQ
%


set class RubyConstantRef
category: '*maglev-runtime'
method:
dyn_definedQinContext: anObject
 "called from generated code"
 | idx mySize assoc envId |
    "evaluate  :: terms"   
envId := 1"__callerEnvId" .
anObject ifNil:[ ^ nil ] .
assoc := anObject  rubyConstAssociationAtOrNil: (self at: 1) env: envId .
assoc ifNil:[ ^ nil ].
mySize := self size .   
idx := 2 .
[ idx <= mySize ] whileTrue:[ | val |
  assoc isDefined ifNil:[ ^ nil ]. 
  val := assoc _valueNoAction .
  assoc := val rubyConstAssociationAtOrNil: (self at: idx) env: envId .
  assoc ifNil:[ ^ nil ].
  idx := idx + 1
].
^ assoc definedQ

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
dyn_resolveInContext: anObject
  "called from generated code"
  | idx mySize assoc val envId aModule sym |
"evaluate  :: terms"  
aModule := anObject .
assoc := anObject  rubyConstAssociationAt: (sym := self at: 1)  env: (envId := 1"__callerEnvId") . 
mySize := self size .   
idx := 2 .
[ idx <= mySize ] whileTrue:[ | prev |
	aModule := assoc _valueFor: sym inClass: aModule env: envId . "possible autoload"
  assoc := aModule rubyConstAssociationAt: (sym := self at: idx) env: envId .
  idx := idx + 1
].
"no setGlobalAssoc...  , result is not cachable due to first term not a constant name"
val := assoc _valueFor: sym inClass: aModule env: envId . "possible trigger of autoload"

^ val

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
globalVarName: aSymbol
  self at: 1 put: aSymbol

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
initialize
  lexPathSize := 0 .

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
irDefinedQNode
  "maybe not used ?"
  | node |
  (node := GsComSendNode new)
       stSelector:  #definedQconst ;
       rcvr: (GsComLiteralNode newObject: self  ) .
  ^ node

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
lexPathSize
  ^ lexPathSize

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
name: aSymbol c2lexPath: lexPath evalSelfCls:  selfCls 
  "used by RubyColon2Node   "
  | lpSize  cnt newSiz |
  cnt := 0 .
  lpSize := lexPath size . 
  lexPathSize := lpSize .  
  self size: (newSiz := lpSize + 2) ;
       at: newSiz     put: aSymbol ;
       at: lpSize + 1 put: selfCls .
  lpSize ~~ 0 ifTrue:[  
    self replaceFrom: 1 to: lpSize with: lexPath startingAt: 1 .
  ].
  "globalAssoc left as nil"

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
notFoundValue
  ^ NameError signal: 'unresolved constant ' , self pathToString

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
pathToString
  | idx last str |
  idx := lexPathSize + 1 .
  last := self size  .
  str := String withAll: (self at: idx) .
  idx := idx + 1 .
  [ idx <= last ] whileTrue:[
    str add:  '::' ; add: (self at: idx) .
    idx := idx + 1 .
  ].
  ^ str

%


set class RubyConstantRef
category: '*maglev-as yet unclassified'
method:
resolveConst
  "invoked from generated code only"
<primitive: 790>    "prim fails if no cached association"
| sym lpSize idx assoc n mySize val prevAssoc envId firstCls rns "hist" parent |
lpSize := lexPathSize .  mySize := self size .
lpSize ifNil:[ 
   ArgumentTypeError signal:'left hand side of :: is not a class or module' 
].
idx := lpSize + 2.
sym := self at: idx  .  idx := idx + 1 .
n := 1 .
envId := 1"__callerEnvId" .
"hist := { } . "    "uncomment for debugging "
[ n <= lpSize ] whileTrue:[ | aModule |  "search specified lex scopes"
  "for a Colon3 node, this will include root scope"
  aModule := self at: n .    n := n + 1  .
  "hist add: { #lexScope .  aModule name . aModule . n . idx }. "
  aModule ifNotNil:[ 
    firstCls ifNil:[ 
      firstCls := aModule.
    ] .
    parent := aModule .
    rns := aModule nameSpace: envId .
    assoc := rns ifNotNil:[ rns resolveConstant: sym ].
    "hist add: { #firstCls . aModule . sym . assoc } . "
  ].
  assoc ifNotNil:[
    prevAssoc := assoc .
    (assoc := assoc isDefinedForResolve: sym inClass: parent env: envId ) ifNotNil:[ 
      n := lpSize + 1 "terminate while loop" 
    ].
  ].
].
assoc ifNil:[ | cls errCls |  "search inheritance hierarchy"
  cls := self at: lpSize + 1 . "lexical self class in an instance_eval"
  errCls := cls .
  cls ifNil:[
    firstCls ifNil:[ 
      cls := Object . errCls := cls .
      "hist add: { #firstCls_i . nil . Object } . "
    ] ifNotNil:[ 
      firstCls == Kernel ifTrue:[ cls := Object ]
                        ifFalse:[ cls := firstCls rubySuperclass: envId ].
      errCls := firstCls.
      "hist add: { #firstCls_i . firstCls . cls }. "
    ].
  ].
  [ cls ~~ nil and:[ assoc == nil] ] whileTrue:[ "dynamic lookup"
    "probe both normal and virtual classes"
    parent := cls .
    (rns := cls nameSpace: envId) ifNotNil:[ 
       assoc := rns resolveConstant: sym .
       assoc ifNotNil:[
          prevAssoc := assoc .
          assoc := assoc isDefinedForResolve: sym inClass: parent env: envId .
       ].
    ]. 
    "hist add: { #cls . cls . sym . assoc }. "
    cls := cls rubySuperclass: envId .
  ].
  assoc ifNil:[
    idx > mySize ifTrue:[ assoc := prevAssoc ].
    assoc ifNil:[ assoc := self _constantMissing: sym in: errCls env: envId].
  ].
].
[ idx <= mySize ] whileTrue:[ "evaluate second and subsequent :: terms"
  val := assoc _valueFor: sym inClass: parent env: envId .  "autoload can be triggered here"
  parent := val .
  assoc := val rubyConstAssociationAt: (sym := self at: idx) env: envId .
  "hist add: { #second__ . val . sym . assoc }. "
  idx := idx + 1
].
val := assoc _valueFor: sym inClass: parent env: envId . "possible trigger of autoload, etc"

self setGlobalAssoc_noMarkDirty: assoc .
"(SessionTemps current at:#TrapResolve otherwise: false) ifTrue:[ hist pause ].  "
^ val
%


set class RubyConstantRef
category: '*maglev-runtime'
method:
resolveGlobalVarAsgn: aValue
  "called from generated code."
| assoc |
assoc :=  self resolveGlobalVarAssoc .
assoc ifNotNil:[
  assoc rubyGlobalVarValue: aValue .
] ifNil:[ | trc dict rns aSymbol envId |
  envId := 1 "__callerEnvId " .
  aSymbol := self at: 1 .
  dict := SessionTemps current at: #RUBY_traceGlobalVars otherwise: nil .
  dict ifNotNil:[
    (trc := dict at: aSymbol otherwise: nil ) ifNotNil:[
      1 to: trc size do:[ :n | | blk |
        blk := trc at: n .
        blk @ruby1:value: aValue  
      ]
    ].
  ].
  rns := Object transientNameSpaceForStore: envId .
  (assoc := rns associationAt: aSymbol otherwise: nil) ifNil:[
    assoc := RubyGlobalVarAssociation newWithKey: aSymbol .
    rns addTransientAssociation: assoc .
  ].
  assoc rubyGlobalVarValue: aValue .
  trc ifNotNil:[ self setGlobalAssoc_noMarkDirty: assoc ].
].
^ aValue

%


set class RubyConstantRef
category: '*maglev-as yet unclassified'
method:
resolveGlobalVarAssoc
  "called from smalltalk code only.
   returns nil or the cached association for the global variable. "

<primitive: 862> 
self _primitiveFailed: #resolveGlobalVarAssoc
%


set class RubyConstantRef
category: '*maglev-runtime'
method:
resolveGlobalVarValue
  "called from generated code.
   returns nil or the value of the global variable. "

| assoc |
assoc :=  self resolveGlobalVarAssoc .
assoc ifNil:[
  assoc := Object rubyGlobalVarAssoc: (self at:1) env: 1"__callerEnvId" .
  self setGlobalAssoc_noMarkDirty: assoc . "cache it"
].
^ assoc globalVarValue .

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
setDynamicTypeError
  lexPathSize := nil

%


set class RubyConstantRef
category: '*maglev-Ruby support'
method:
setGlobalAssoc_noMarkDirty: anAssoc

"stores anAssoc into the globalAssoc instVar of receiver,
  without marking receiver dirty.  Sets noStub bit in receiver."

<primitive: 791>
self size > 2000 ifTrue:[ self error:'too many terms in Constant path'].
self _primitiveFailed: #setGlobalAssoc_noMarkDirty: args: { anAssoc }
%


set class RubyConstantRef
category: '*maglev-runtime'
method:
_constantMissing: aSymbol in: aClass env: envId 
  | val assoc |
  val := aClass @ruby1:const_missing: aSymbol  .
  (assoc := RubySymbolAssociation newWithKey: aSymbol) _value: val .
  ^ assoc

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
_valueNoAction
  ^ nil

%

