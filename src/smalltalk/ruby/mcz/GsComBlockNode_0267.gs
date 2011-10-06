
set class GsComBlockNode
category: '*maglev-Documentation'
classmethod:
comment
^
'A GsComBlockNode represents a block within a method. It may be
 either a real block or a block that will be inlined by the parser.

   lexLevel, a SmallInteger > 0, lexical level of the block within the
                source of the home method, without regard to inlining. 
   lastSrcOffset,  end of the source for the block within the method,
          a byteOffset if srcOffset is non-nil, otherwise a line number.
   blkKind, a SmallInteger,  a ComBlockEKind , always 0 for Ruby
   args, each element is a GsComVarLeaf
   lastArgInfo , a SmallInteger
   temps, an Array , each element is a GsComVarLeaf
   statements ,an Array or OrderedCollection ,  the body of the block
   terms  , an Array, used only for Gemstone select block, {:x| ... }
'
%


set class GsComBlockNode
category: '*maglev-Instance creation'
classmethod:
new
"create an instance registered with VM for finalization of cData"

<primitive: 674>
self _primitiveFailed: #new
%


set class GsComBlockNode
category: '*maglev-Instance Initialization'
method:
appendArg: aGsComVarLeaf
  | argKnd needSynth |
  needSynth := false .
  (argKnd := aGsComVarLeaf varKind) == COMPAR_BLOCK_ARG_VAR ifTrue:[
    (aGsComVarLeaf lexLevel < lexLevel and:[ COM_RUBY_COMPAT_LEVEL == 18]) ifTrue:[
      "Fix Ticket 113, block arg escaping to outer block arg,
       Synthesize a blockArg node, and add an assignment to the
       outer arg"
      needSynth := true 
    ] ifFalse:[
      args addLast: aGsComVarLeaf.      "normal block arg"
      ^ self .
    ].
  ].
  ((argKnd == COMPAR_METHOD_TEMP_VAR 
      or:[ argKnd == COMPAR_BLOCK_TEMP_VAR
        or:[ argKnd == COMPAR_METHOD_ARG_VAR] ]) 
    and:[ aGsComVarLeaf lexLevel < lexLevel 
        and:[ COM_RUBY_COMPAT_LEVEL == 18 ]]) ifTrue:[
    "Fix for Ticket 21, ruby block args escaping to outer level.
     Synthesize a blockArg node, and add an assignment to the 
     outer temp"
    needSynth := true
  ].
  needSynth ifTrue:[
    | synthArg assnNod ags |  
    synthArg := GsComVarLeaf new blockArg: aGsComVarLeaf varName
                argNumber: (ags := args) size + 1
                forBlock: self .
    ags add: synthArg .
    assnNod := GsComAssignmentNode new dest: aGsComVarLeaf
                source: (GsComVariableNode new leaf: synthArg ).
    statements insertObject: assnNod at: 1 .
    ^ self
  ].
  self error: 'VarLeaf.varKind=', (GsComVarLeaf varKindToString: argKnd) ,
        ' illegal for a block arg'   
%


set class GsComBlockNode
category: '*maglev-Instance Initialization'
method:
appendMasgnDummyArg
  "for a Ruby block of the form  { | x, |  } append a dummy arg
   so the block-args processing will then do the right thing at runtime"
  | ags dummy |
  (ags := args) size == 1 ifFalse:[ self error:'before appendMasgnDummyArg, size not 1'].
  dummy := GsComVarLeaf new blockArg: #_dummyarg argNumber: 2 forBlock: self .
  ags add: dummy 
%


set class GsComBlockNode
category: '*maglev-Instance Initialization'
method:
appendStatement: aNode
  aNode sourceOffset ifNil:[ | bsiz |
    (bsiz := statements size) ~~ 0 ifTrue:[
      aNode sourceOffset:( (statements at: bsiz) sourceOffset).
    ]
  ].
  statements addLast: aNode . ^
  srcOffset ifNil:[ | ofs |
    ofs := aNode sourceOffset .
    ofs ifNotNil:[ srcOffset := ofs ] 
        ifNil:[ lineNumber ifNil:[ lineNumber := aNode lineNumber]]. 
  ].
%


set class GsComBlockNode
category: '*maglev-Instance Initialization'
method:
appendTemp: aGsComVarLeaf
  aGsComVarLeaf varKind == COMPAR_BLOCK_TEMP_VAR ifFalse:[
    self error: 'bad arg kind ', aGsComVarLeaf varKindString 
  ].
  temps addLast: aGsComVarLeaf
%


set class GsComBlockNode
category: '*maglev-Accessing'
method:
args
  ^ args
%


set class GsComBlockNode
category: '*maglev-Instance Initialization'
method:
lastLineNumber: anInt
  | first last |
  last := anInt .
  srcOffset ifNotNil:[ self error:'should use lastSourceOffset:' ].
  (first := lineNumber) ifNil:[ 
    lineNumber := anInt 
  ] ifNotNil:[ 
    anInt < first ifTrue:[ 
      statements do:[ :aNode | | num |
        (num := aNode lineNumber) ifNotNil:[
           num < first ifTrue:[ first := num ].
           num > last ifTrue:[ last := num ].
        ]
      ].
      lineNumber := first.
    ]
  ].
  lastSrcOffset := last  "store a line number"
%


set class GsComBlockNode
category: '*maglev-Instance Initialization'
method:
lastSourceOffset: anOffset 
  lastSrcOffset := anOffset
%


set class GsComBlockNode
category: '*maglev-Accessing'
method:
lexLevel
  ^ lexLevel
%


set class GsComBlockNode
category: '*maglev-Instance Initialization'
method:
lexLevel: aLevel
"the primary initialization method"
  kind := COMPAR_BLOCK_NODE.
  aLevel < 0 ifTrue:[ aLevel error:'out of range' ].
  lexLevel := aLevel .
  blkKind := 0 .
  args := { } .
  temps := { } .
  statements := { } .
  lastArgInfo := 0 .
  "terms left as nil"
%


set class GsComBlockNode
category: '*maglev-Accessing'
method:
numArgs
  ^ args size
%


set class GsComBlockNode
category: '*maglev-Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  aStream print:' lexLevel:' int: lexLevel ; 
    print:' lastArgInfo:' int: lastArgInfo ; cr ;
    nextPutAll:' args:'; do: args ; 
    nextPutAll:' temps:'; do: temps ; 
    nextPutAll:' statements:'; do: statements ; 
  nextPut: $) ; cr .
%


set class GsComBlockNode
category: '*maglev-Instance Initialization'
method:
setAmpersandArg
  lastArgInfo := lastArgInfo bitOr: HasBlockArg_mask 
%


set class GsComBlockNode
category: '*maglev-Instance Initialization'
method:
setLastArgStar
  "filter out anonymous star here,   {|*| }  equivalent to { }  "
  args size ~~ 0 ifTrue:[ lastArgInfo := lastArgInfo bitOr: LastArgStar_mask ].
%


set class GsComBlockNode
category: '*maglev-Instance Initialization'
method:
setNoArgsDecl
  lastArgInfo := lastArgInfo bitOr: NoDeclaredArgs_mask  .
%


set class GsComBlockNode
category: '*maglev-Instance Initialization'
method:
_appendLastStatement: aNode
  "OBSOLETE"
  self appendStatement: aNode .
  srcOffset ifNotNil:[ 
    self lastSourceOffset: aNode lastSourceOffset
  ] ifNil: [ | num |
    num := aNode lastLineNumber .
    num ifNotNil:[ self lastLineNumber: num ].
  ]
%


set class GsComBlockNode
category: '*maglev-Instance Initialization'
method:
_computeLastLineNumber
  "OBSOLETE"
  | list sz |
  list := statements .
  srcOffset ifNotNil:[
    (sz := list size) _downTo: 1 do:[:n | | num |
      num := (list at: n) lastSourceOffset .
      num ifNotNil:[
         self lastSourceOffset: num .
         ^ self
      ].
    ].
    self lastSourceOffset: srcOffset .
    ^ self
  ].
  (sz := list size) _downTo: 1 do:[:n | | num |
      num := (list at: n) lastLineNumber .
      num ifNotNil:[
         self lastLineNumber: num .
         ^ self
      ].
  ].
  sz > 1 ifTrue:[ self error:'could not find a last source position'].
%


set class GsComBlockNode
category: '*maglev-Instance Initialization'
method:
_finishLastStatement
  "OBSOLETE"
  | stmts last |
  stmts := statements .
  last := stmts atOrNil: stmts size .
  last ifNotNil:[
    srcOffset ifNotNil:[
      self lastSourceOffset: last lastSourceOffset
    ] ifNil:[ | num |
      num := last lastLineNumber .
      num ifNotNil:[ self lastLineNumber: num ].
    ]
  ]
%

