
set class CCallout
category: '*maglev-Ruby support'
classmethod:
cextNamed: aName cFunc: addressInt nArgs: nArgs

  | res |
  (res := self _basicNew) _cextName: aName cFunc: addressInt nArgs: nArgs .
  ^ res
%


set class CCallout
category: '*maglev-Ruby support'
classmethod:
_rubyNew: argsArray
  | res |
  (res := self _basicNew) 
     _setLibrary: (argsArray at: 1)"cLibrary" ;
     _name: (argsArray at:2)"aName"
        result: (argsArray at:3) "resType"
        args: (argsArray at:4) "argumentTypes"
        varArgsAfter: (argsArray at:5) "usually -1" .
  res _enumsDict: (argsArray atOrNil: 6) .
  ^ res   
%


set class CCallout
category: '*maglev-Ruby support'
method:
callCextension: aSelf with: argsArray block: aBlock ex: fakeEx

<primitive: 905>
"invoke the function described by the receiver.  A call to
 this method will have been installed by an rb_define_method()
 from within the C extension's C code during initialization 
 of the library.   The caller must pass fakeEx == nil"
fakeEx ifNotNil:[
  "primitive has detected  exception return from below the C code"
  fakeEx return: nil  . 
] ifNil:[
  argsArray class == Array ifFalse:[
    ArgumentTypeError new name:'argsArray' expectedClass: Array 
	actualArg: argsArray ; signal .
  ].
  argsArray size > 8 ifTrue:[
    ArgumentError new signal: 'maximum of 8 args to a C extension function'
  ].
].
^ self _primitiveFailed: #callCextension:with:block:ex:
       args: { aSelf . argsArray . aBlock . fakeEx }
%


set class CCallout
category: '*maglev-Ruby support'
method:
signatureString
  | res fn types nTypes |
  types := argTypes ifNotNil:[ 
    ^ super signatureString   "a Normal FFI callout"
  ].
  "handle callout to a Ruby C extension"
  (res := resultType asString) .
  (fn := fName) ifNotNil:[
     nTypes := types size .
     res add: $  ; add: fn ; add: $(  ;
         add: argCounts asString ; add: ' VALUE(s) ';
        add: $) . 
  ].
  ^ res
%


set class CCallout
category: '*maglev-runtime'
method:
translateArgs: argsArray usingEnums: enumsInfo 
 "handles Symbol to int64 translation using named Enums"

 1 to: enumsInfo size by: 2 do:[ :n| | ofs anEnum v |
   ofs := enumsInfo at: 1 .
   anEnum := enumsInfo at: 2. 
   v := anEnum @ruby1:__sym_to_value: (argsArray at: ofs) .
   v ifNil:[ ArgumentError signal:'symbol not valid for specified argument Enum' ].
   argsArray at: ofs put: v .
 ].

%


set class CCallout
category: '*maglev-runtime'
method:
translateResult: res usingEnum: anEnum
  | r |
  r := anEnum @ruby1:__val_to_symbol: res  .
  r ifNil:[ ArgumentError signal: 'value not valid for specified result Enum' ].
  ^ r

%


set class CCallout
category: '*maglev-Ruby support'
method:
_cextName: aName cFunc: addressInt nArgs: nArgs 
  "library left as nil"
  fName := aName .
  "argTypes left as nil"
  "result and args are all OopType , handled by prim callCextensionWith:"
  resultType := #uint64 .
  cTypes := #uint64 "actually an array of OopType" .
  argCounts := nArgs . "maybe negative, for signature f(int n, VALUE *a, VALUE self)"
  "argTypesDict left as nil."

  self _storeCextAddress: addressInt .
%


set class CCallout
category: '*maglev-runtime'
method:
_compileCaller: rubyName In: aModule enums: eArray
  "A ruby primitive.
   Install a copy of the method CCallout>>_rubyToCcallTemplate:  ,
   which will call the C function specified by the receiver.
   as a module method with name rubyName in aModule.
   eArray is a Ruby Array   [ FFI::Enums , argEnums , retEnum ] "
      "ruby_selector_suffix dependent"

  | envId cmstr argEnums retEnum syms enums templateSel suffix  
      cm litOfs lit callerSel cls  |
  envId := 1"__callerEnvId" .
  cmstr := '"  CALLING:
     ' copy .
  cmstr add: self signatureString ; add: ' "' .
  argEnums := eArray at: 2 .
  retEnum := eArray at: 3 .
  argEnums ifNil:[ retEnum ifNil:[ syms := #( _rubyToCcallTemplate: ) . enums := #() ] 
              ifNotNil:[ syms := #( _rubyToCcallTemplateEnumresult:  CCalloutResultEnumGoesHere )  .
                 enums := { retEnum } ]]
        ifNotNil:[ retEnum ifNil:[ syms := #( _rubyToCcallTemplateEnumargs: CCalloutEnumsInfoGoesHere )  .
                                 enums := { argEnums } ]
                ifNotNil:[ syms := #( _rubyToCcallTemplateEnumargs_results: CCalloutEnumsInfoGoesHere 
                    CCalloutResultEnumGoesHere ) .
               enums := { argEnums . retEnum }] ].
  templateSel := syms at: 1 .
  self _lastArgTypeIsCallback ifTrue:[
    templateSel := (templateSel, 'ampersand:') asSymbol .
    suffix := '#0*&' .
  ] ifFalse:[
    suffix := '#0*_' 
  ].
  "---- was workBlk"
  cm := (CCallout compiledMethodAt: templateSel )
      _copyForClass: aModule aliasFrom: nil to: nil
     comment: cmstr .
  litOfs := cm literalsOffset .
  (lit := cm basicAt: litOfs ) == #CCalloutInstanceGoesHere ifFalse:[
    self error:'inconsistent literals, #CCalloutInstanceGoesHere not found'
  ].
  cm at: litOfs put: self  .
  enums size == 0 ifFalse:[
    2 to: syms size do:[:n | | ofs aSym |
  lit := cm basicAt: (ofs := litOfs + n - 1) .
  lit == (aSym := syms at: n) ifFalse:[
    self error:'inconsistent literals, ' ,  aSym , '  not found'
  ].
  cm at: ofs put: (enums at: n - 1)
    ].
  ].
  cm immediateInvariant .
  (callerSel := rubyName copy)  addAll: suffix .
  callerSel := callerSel asSymbol .
  aModule is_aModule ifTrue:[ cls := aModule moduleMethodsModule ]
      ifFalse:[ cls := aModule class ].
  cls addRubySelector: callerSel method: cm env: envId  .
  RubyBridge installBridgesFor: callerSel in: cls argsDescr: nil
      optArgs: 0 protection: 0 primKind: 0 env: envId .
  "----"

  ^ cm

%


set class CCallout
category: '*maglev-cextensions'
method:
_compileCextCaller: rubyName In: aModule
  | envId templateSel cmstr cm litOfs lit callerSel |
  envId := 1 .
    cmstr := '"  CALLING:
     ' copy .
  cmstr add: self signatureString ; add: ' "' .
  templateSel := #_rubyCextCallTemplate:block: .
  cm := (CCallout compiledMethodAt: templateSel )
      _copyForClass: aModule aliasFrom: nil to: nil
     comment: cmstr .
  litOfs := cm literalsOffset .
  (lit := cm basicAt: litOfs ) == #CCalloutInstanceGoesHere ifFalse:[
    self error:'inconsistent literals, #CCalloutInstanceGoesHere not found'
  ].
  cm at: litOfs put: self  .
  cm immediateInvariant .
  callerSel := rubyName _asSymbolWithRubySuffix: 16r3 " #0*& " .
  aModule addRubySelector: callerSel method: cm env: envId  .
  RubyBridge installBridgesFor: callerSel in: aModule argsDescr: nil
       optArgs: 0 protection: 0 primKind: 0 env: envId .
  ^ cm

%


set class CCallout
category: '*maglev-Ruby support'
method:
_enumsDict: aGsMethodDictionary
  untaggedEnumsDict := aGsMethodDictionary
%


set class CCallout
category: '*maglev-runtime'
method:
_lastArgTypeIsCallback
  | types sz |
  sz := (types := argTypes) size.
  sz ~~ 0 ifTrue:[ ^ ( types at: sz) isKindOfClass: CCallin ].
  ^ false

%


set class CCallout
category: '*maglev-cextensions'
method:
_rubyCextCallTemplate: argsArray block: aBlock
  | inst |
  inst := #CCalloutInstanceGoesHere .
  ^ inst callCextension: self with: argsArray block: aBlock ex: nil 

%


set class CCallout
category: '*maglev-runtime'
method:
_rubyToCcallTemplate: argsArray
  | inst |
  inst := #CCalloutInstanceGoesHere .
  ^ inst callWith: argsArray

%


set class CCallout
category: '*maglev-runtime'
method:
_rubyToCcallTemplate: argsArray ampersand: aBlock
  | inst arr |
  inst := #CCalloutInstanceGoesHere .
  aBlock ifNotNil:[ argsArray add: aBlock ].
  ^ inst callWith: argsArray

%


set class CCallout
category: '*maglev-runtime'
method:
_rubyToCcallTemplateEnumargs: argsArray
 | inst enumsInfo |
 inst := #CCalloutInstanceGoesHere .
 enumsInfo := #CCalloutEnumsInfoGoesHere . "Array of pairs ( 1-based argNum, an FFI::Enum)"

 inst translateArgs: argsArray usingEnums: enumsInfo . 
 ^ inst callWith: argsArray

%


set class CCallout
category: '*maglev-runtime'
method:
_rubyToCcallTemplateEnumargs: argsArray ampersand: aBlock
 | inst enumsInfo |
 inst := #CCalloutInstanceGoesHere .
 enumsInfo := #CCalloutEnumsInfoGoesHere . "Array of pairs ( 1-based argNum, an FFI::Enum)"

 inst translateArgs: argsArray usingEnums: enumsInfo .
 aBlock ifNotNil:[ argsArray add: aBlock ].
 ^ inst callWith: argsArray

%


set class CCallout
category: '*maglev-runtime'
method:
_rubyToCcallTemplateEnumargs_results: argsArray

 | inst enumsInfo res resEnum |
 inst := #CCalloutInstanceGoesHere .
 enumsInfo := #CCalloutEnumsInfoGoesHere . "Array of pairs ( 1-based argNum, an FFI::Enum)"
 resEnum := #CCalloutResultEnumGoesHere .  " an FFI::Enum "

 inst translateArgs: argsArray usingEnums: enumsInfo . 
 res := inst callWith: argsArray .
 ^ inst translateResult: res usingEnum: resEnum .

%


set class CCallout
category: '*maglev-runtime'
method:
_rubyToCcallTemplateEnumargs_results: argsArray ampersand: aBlock

 | inst enumsInfo res resEnum |
 inst := #CCalloutInstanceGoesHere .
 enumsInfo := #CCalloutEnumsInfoGoesHere . "Array of pairs ( 1-based argNum, an FFI::Enum)"
 resEnum := #CCalloutResultEnumGoesHere .  " an FFI::Enum "

 inst translateArgs: argsArray usingEnums: enumsInfo .
 aBlock ifNotNil:[ argsArray add: aBlock ].
 res := inst callWith: argsArray .
 ^ inst translateResult: res usingEnum: resEnum .

%


set class CCallout
category: '*maglev-runtime'
method:
_rubyToCcallTemplateEnumresult: argsArray

 | inst res resEnum |
 inst := #CCalloutInstanceGoesHere .
 resEnum := #CCalloutResultEnumGoesHere .

 res := inst callWith: argsArray .
 ^ inst translateResult: res usingEnum: resEnum .

%


set class CCallout
category: '*maglev-runtime'
method:
_rubyToCcallTemplateEnumresult: argsArray ampersand: aBlock
 | inst res resEnum |
 inst := #CCalloutInstanceGoesHere .
 resEnum := #CCalloutResultEnumGoesHere .
 aBlock ifNotNil:[ argsArray add: aBlock ].
 res := inst callWith: argsArray .
 ^ inst translateResult: res usingEnum: resEnum .

%


set class CCallout
category: '*maglev-Ruby support'
method:
_storeCextAddress: addressInt

"stores addressInt in cData.dlsymResult 
  and sets library := 1  
     to indicate In a C extension .so, not in an FFI .so"
<primitive: 906>
self _primitiveFailed: #_storeCextAddress: args: { addressInt }
%

