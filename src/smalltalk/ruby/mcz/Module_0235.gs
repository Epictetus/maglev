
set class Module
category: '*maglev-runtime'
classmethod:
cextDefineClass: aString parent: parentArg superclass: superCls env: envId
  "implementation for rb_define_class_under() .   
   does not support fixed instvars yet"

^ RubyCompiler new defineClassNamed: aString asSymbol 
   rubyMethod: nil inScope: parentArg superclass: superCls env: envId 
   fixedIvs: #()

%


set class Module
category: '*maglev-runtime'
classmethod:
cextDefineModule: aString parent: parentArg env: envId
^ RubyCompiler new defineModuleNamed: aString asSymbol
     rubyMethod: nil inScope: parentArg  env: 1 

%


set class Module
category: '*maglev-Ruby support'
classmethod:
clearVmTransientState: envId

<primitive: 851>
envId _validateClass: SmallInteger .
(envId < 0 or:[ envId > 255]) ifTrue:[ OutOfRange signal:'envId out of range'].
self _primitiveFailed: #clearVmTransientState: args: { envId }
%


set class Module
category: '*maglev-Documentation'
classmethod:
comment
^ 'Module is identically the Ruby class Module.

   Uses of copies of Modules as Ruby virtual classes is as follows.
   The actual creation of
   the virtual classes is done by primitives 682 and 694

   include a Module into a Class    (OM_RUBY_INCLUDE_Module)
   see   _rubyIncludeModule:envId:  in .mcz

  include a Module into a Class for class-side methods (forMeta==true)
    aClass includeModule: aModule forMeta: OM_RUBY_INCLUDE_Meta
    see _rubyIncludeMeta:envId:

  include a Module into itself so methods are available as both
   class and instance methods
       see _rubyIncludeSelf:envId:

  create a virtual class for a Module to support the   
   	module_function     method
     see _rubyIncludeModuleMethodsModuleEnv:
'
%


set class Module
category: '*maglev-runtime'
classmethod:
excludeInternalMethodNames: set
   "set is an IdentitySet of Symbols. 
    Returns an Array with contents of set filtered
    to exclude most methods beginning with __ , and converted to Strings. "
  | arr destIdx setSize |
  arr := Array new: (setSize := set size) .
  destIdx := 0 .
  1 to: setSize do:[:n || sym |
    sym := set _at: n .
    (sym at: 1 equals: '__' ) ifTrue:[ "exclude internal implementation methods"
	   (sym == #__send__ or:[ sym == #__id__]) ifTrue:[
		  arr at: (destIdx := destIdx + 1) put: sym asString .
		].
    ] ifFalse:[  
      arr at: (destIdx := destIdx + 1) put: sym asString .
    ].
  ] .
  arr size: destIdx .
  ^ arr .

%


set class Module
category: '*maglev-Ruby support'
classmethod:
isMetaOrModule
  ^ true
%


set class Module
category: '*maglev-runtime'
classmethod:
isModifiable_oops_notModule
  ^ false

%


set class Module
category: '*maglev-runtime'
classmethod:
moduleNesting
  "Result is the contents of rtModuleStack, excluding Object"
  | stk sz |
  stk := GsProcess _current _rubyThreadDataAt: 5 "rtModuleStack" .
  sz := stk size.
  sz > 1 ifTrue:[ | k res |
    k := sz - 1.
    res := Array new: k .
    2 to: sz do:[:j | res at: k put: (stk at: j) . k := k - 1 ].
    ^ res
  ] ifFalse:[  
    ^ { }
  ].

%


set class Module
category: '*maglev-Instance creation'
classmethod:
_newModule
  "returns a new instance of receiver, with name == #'' and empty method dict."
  <primitive: 771>
  self _primitiveFailed: #_newModule   "receiver was not a subclass of Module"
%


set class Module
category: '*maglev-Wrapping Smalltalk classes'
classmethod:
_wrappedSmalltalkClasses

"Returns an Array of pairs,
   aClass, 1  means aClass will be in DataCuratorObjectSecurityPolicy in extent0.ruby.dbf. 
   aClass, 0  means class remains in SystemObjectSecurityPolicy and should not be 
		  extended in Ruby.
 A commented out class will not have a name in Ruby module Smalltalk."

^ { 
AbstractCharacter . 	1 . 
AbstractCollisionBucket . 1 . 
AbstractDictionary . 	1 . 
"AbstractException"
AbstractSession . 	1 . 
AbstractUserProfileSet . 	1 . 
"Activation is obsolete"
ArgumentError .  1 . "mapped to Ruby by Ruby bootstrap"
Array .  1 . "mapped to Ruby by Ruby bootstrap"
Association . 	1 . 
AutoComplete . 	1 . 
Bag . 	1 . 
BasicSortNode .  0 .  "implementation of indexing"
Behavior .       1 . "mapped to Ruby by Ruby bootstrap"
BinaryFloat . 	0 .   "not visible from pure Ruby"
BitSet . 	1 . 
"Block is obsolete"
"BlockClosure obsolete"
BlockSorter . 	1 . 
Boolean .        1 . "mapped to Ruby by Ruby bootstrap"
BtreeBasicInteriorNode . 0 .   "implementation of indexing"
BtreeBasicLeafNode . 	0 .   "implementation of indexing"
BtreeComparisonForCompare . 	0 .   "implementation of indexing"
BtreeComparisonForSort . 	0 .   "implementation of indexing"
BtreeComparisonQuerySpec . 	0 .   "implementation of indexing"
BtreeInteriorNode . 	0 .   "implementation of indexing"
BtreeLeafNode . 	0 .   "implementation of indexing"
BtreeNode . 	0 .   "implementation of indexing"
BtreeQuerySpec . 	0 .   "implementation of indexing"
BtreeRangeComparisonQuerySpec .  0 .   "implementation of indexing"
BtreeReadStream . 	0 .   "implementation of indexing"
BucketValueBag . 	0 .   "implementation of indexing"
ByteArray . 	1 . 
CannotReturn .   1 . "mapped to Ruby by Ruby bootstrap"
"CanonicalStringDictionary  obsolete"
"CanonStringBucket, 	0,  private to AllSymbols implementation"
CanonStringDict . 	1 . 
CanonSymbolDict . 	0 .  "class of AllSymbols, not extendable in Ruby"
CBuffer . 	1 . 
CByteArray . 	1 . 
CFunction . 	1 . 
Character . 	1 . 
CharacterCollection . 	1 . 
ClampSpecification . 	0 . "private to GBS implementation"
Class .  1 .                "mapped to Ruby by Ruby bootstrap"
ClassHistory . 	1 . 
ClassOrganizer . 	1 . 
ClassSet . 	1 . 
CLibrary . 	1 . 
ClientForwarder . 	0 .  "private to Smalltalk GUI clients"
ClusterBucket . 	1 . 
ClusterBucketArray . 	1 . 
Collection . 	1 . 
CollisionBucket . 	0 . "only subclasses extendable from Ruby"
"CompiledMethod obsolete"
CompileError . 	1 . 
"ComplexBlock obsolete"
"ComplexVCBlock obsolete"
ConstrainedPathEvaluator . 	1 .  "implementation of indexing"
ConstrainedPathTerm . 	0 .  "implementation of indexing"
ControlInterrupt .       1 . "mapped to Ruby by Ruby bootstrap"
CPointer . 	1 . 
CriticalSection . 0 .   "only subclasses extendable from Ruby"
CZstream .       1 . "mapped to Ruby by Ruby bootstrap"
Date . 	1 . 
DateAndTime . 	1 . 
DateAndTimeANSI . 	1 . 
DateTime . 	1 . 
DecimalFloat . 	1 . 
Delay . 	0 .    "used by ProcessScheduler, only subclasses extendable from Ruby"
DependencyList . 0 .  "implementation of indexing"
DepListBucket .  0 .  "implementation of indexing"
DepListTable . 	0 .  "implementation of indexing"
Dictionary . 	1 . 
DoubleByteString . 	1 . 
DoubleByteSymbol . 	1 . 
Duration . 	1 . 
EOFError .       1 . "mapped to Ruby by Ruby bootstrap"
EqualityIndexQueryEvaluator . 	0 . "implementation of indexing"
Error .  1 . "mapped to Ruby by Ruby bootstrap"
"ErrorDescription, 	0,  private to GciInterface"
"EUCString, 	0,   obsolete from Ruby perspective"
"EUCSymbol, 	0,   obsolete from Ruby perspective"
ExampleSetTest . 	1 . 
"Exception, 	0,  private to Smalltalk, do not use from Ruby"
"ExceptionSet, 	0,  private to Smalltalk, do not use from Ruby"
ExecBlock .      1 . "mapped to Ruby by Ruby bootstrap"
"ExecBlock0, 	0,  private to VM"
"ExecBlock1, 	0,  private to VM"
"ExecBlock2, 	0,  private to VM"
"ExecBlock3, 	0,  private to VM"
"ExecBlock4, 	0,  private to VM"
"ExecBlock5, 	0,  private to VM"
"ExecBlockN, 	0,  private to VM"
"ExecutableBlock obsolete"
FalseClass .     1 . "mapped to Ruby by Ruby bootstrap"
FastIdentityKeyValueDictionary . 	1 . 
Float .  1 . "mapped to Ruby by Ruby bootstrap"
FloatingPointError .     1 . "mapped to Ruby by Ruby bootstrap"
Fraction . 	1 . 
GciInterface . 	0 .  "only subclasses extendable from Ruby"
GemStoneParameters . 	1 . 
GsClassDocumentation . 	1 . 
GsCloneList . 	1 . 
"GsComArrayBuilderNode, 0,  private to code generator"
"GsComAssignmentNode, 	0,    private to code generator"
"GsComBlockNode, 	0,  private to code generator"
"GsComCascadeNode, 	0,  private to code generator"
"GsComGotoNode, 	0,  private to code generator"
"GsComLabelNode, 	0,  private to code generator"
"GsComLiteralNode, 	0,  private to code generator"
"GsComLitLeaf, 	0,  private to code generator"
"GsComLoopNode, 	0,  private to code generator"
"GsComMethNode, 	0,  private to code generator"
"GsCommitList, 	0,  private to code generator"
"GsComPathNode, 	0,  private to code generator"
"GsCompilerIRNode, 	0,  private to code generator"
"GsComReturnNode, 	0,  private to code generator"
"GsComSelectorLeaf, 	0,  private to code generator"
"GsComSendNode, 	0,  private to code generator"
"GsComStatementsNode, 	0,  private to code generator"
"GsComTermNode, 	0,  private to code generator"
"GsComVariableNode, 	0,  private to code generator"
"GsComVarLeaf, 	0,  private to code generator"
GsCurrentSession . 	1 . 
GsDocText . 	1 . 
"GsExceptionHandler, 	0,    private to Smalltalk legacy code"
GsFile .         1 . "mapped to Ruby by Ruby bootstrap"
GsFileStat .     1 . "mapped to Ruby by Ruby bootstrap"
GsfClassDefinitionInfo . 	1 . 
GsfModificationLog . 	1 . 
GsInterSessionSignal . 	1 . 
"GsMethod obsolete"
GsMethodDictionary . 	1 . 
"GsNativeCode, 	0,  private to VM"
GsNMethod .      1 . "mapped to Ruby by Ruby bootstrap"
GsPackage . 	0 .  "only subclasses extendable from Ruby"
GsPackageLibrary . 	0 .  "only subclasses extendable from Ruby"
GsPackagePolicy . 	0 .  "only subclasses extendable from Ruby"
GsProcess .      1 . "mapped to Ruby by Ruby bootstrap"
GsSession . 	1 . 
GsSessionMethodDictionary . 0 . "only subclasses extendable from Ruby"
GsSocket . 	0 .  "only subclasses extendable from Ruby"
"GsStackBuffer obsolete"
Halt . 	1 . 
IdentityBag . 	1 . 
IdentitySet .    1 . "mapped to Ruby by Ruby bootstrap"
IdentityBtreeNode . 	0 . "implementation of indexing"
IdentityCollisionBucket . 	0 . "only subclasses extendable from Ruby"
IdentityDictionary . 	1 .  "only subclasses extendable from Ruby"
IdentityIndex . 	0 .  "implementation of indexing"
IdentityIndexQueryEvaluator . 	0 . "implementation of indexing"
IdentityKeySoftValueDictionary . 1 . 
IdentityKeyValueDictionary . 	0 . "only subclasses extendable from Ruby, see Ruby IdentityHash"
IdentitySoftCollisionBucket . 	0 . "only subclasses extendable from Ruby"
IndentingStream . 	1 . 
IndexDictionaryEntryHolder . 	0 .  "implementation of indexing"
IndexedQueryEvaluator . 	0 .  "implementation of indexing"
IndexList . 	0 . 	"implementation of indexing"
IndexManager . 	1 . 
IndexManagerAutoCommitPolicy . 	1 . 
Integer .        1 . "mapped to Ruby by Ruby bootstrap"
IntegerKeyValueDictionary . 	1 . 
Interval . 	0 .  "only subclasses extendable from Ruby, see Ruby Range"
InvariantArray . 	1 . 
"InvariantEUCString, 	0,  obsolete, don't use in Ruby"
InvariantString . 	1 . 
IO .     1 . "mapped to Ruby by Ruby bootstrap"
IOError .        1 . "mapped to Ruby by Ruby bootstrap"
IPSocket .       1 . "mapped to Ruby by Ruby bootstrap"
ISOLatin . 	1 . 
"JapaneseString, 	0,  obsolete, don't use in Ruby"
"JISCharacter, 	0,  obsolete, don't use in Ruby"
"JISString, 	0,  obsolete, don't use in Ruby"
Kernel .         1 . "mapped to Ruby by Ruby bootstrap"
KeySoftValueDictionary . 1 . 
KeyValueDictionary . 	0 . "only subclasses extendable from Ruby, see Ruby Hash" 
"LanguageDictionary, 	0,  obsolete, don't use in Ruby"
"Large2ByteLeaf, 	0,  private to VM"
"Large4ByteLeaf, 	0,  private to VM"
"Large8ByteLeaf, 	0,  private to VM"
LargeInteger .   1 . "mapped to Ruby by Ruby bootstrap"
Locale . 	1 . 
LogEntry . 	1 . 
Magnitude . 	1 . 
MappingInfo . 	1 . 
MatchData .      1 . "mapped to Ruby by Ruby bootstrap"
MessageNotUnderstood .   1 . "mapped to Ruby by Ruby bootstrap"
Metaclass3 .  1 . "mapped to Ruby by Ruby bootstrap"
"MethodContext obsolete"
Module .         1 . "mapped to Ruby by Ruby bootstrap"
MultiByteString . 	1 . 
NameError .      1 . "mapped to Ruby by Ruby bootstrap"
Notification . 	1 . 
NscBuilder . 	0 .  "implementation of indexing"
Number .         1 . "mapped to Ruby by Ruby bootstrap"
Object .         1 . "mapped to Ruby by Ruby bootstrap"
"ObsoleteMetaclass  	private to Smalltalk"
"ObsDoubleByteString  obsolete"
"ObsDoubleByteSymbol  obsolete"
"ObsFloat  obsolete"
"ObsLargeNegativeInteger  obsolete"
"ObsLargePositiveInteger  obsolete"
"Obsolete23ClampSpecification  obsolete"
"Obsolete23GsFile  obsolete"
"ObsoleteClampSpecification  obsolete"
"ObsoleteDateTime  obsolete"
"ObsoleteDateTime50  obsolete"
"ObsoleteDictionary  obsolete"
"ObsoleteException  obsolete"
"ObsoleteGsFile  obsolete"
"ObsoleteGsProcess  obsolete"
"ObsoleteGsSocket  obsolete"
"ObsoleteIdentityCollisionBucket  obsolete"
"ObsoleteIdentityDictionary  obsolete"
"ObsoleteLanguageDictionary  obsolete"
"ObsoleteRcCollisionBucket  obsolete"
"ObsoleteSymbol  obsolete"
"ObsoleteSymbolAssociation  obsolete"
"ObsoleteSymbolDictionary  obsolete"
"ObsoleteSymbolKeyValueDictionary  obsolete"
"ObsoleteSymbolListDictionary  obsolete"
"ObsoleteSymbolSet  obsolete"
"ObsoleteTimeZone  obsolete"
"ObsoleteVariableContext  obsolete"
"ObsQuadByteString  obsolete"
"ObsSmallFloat  obsolete"
"OldRepository  obsolete"
OffsetError .    1 . "mapped to Ruby by Ruby bootstrap"
OutOfRange .     1 . "mapped to Ruby by Ruby bootstrap"
AlmostOutOfMemory .    1 . "mapped to Ruby by Ruby bootstrap"
OrderedCollection . 	1 . 
PassiveObject . 	1 . 
PathEvaluator . 	0 .  "implementation of indexing"
PathSorter . 	0 .  "implementation of indexing"
PathTerm . 	0 .  "implementation of indexing"
PositionableStream . 	1 . 
PrintStream . 	1 . 
"Process obsolete"
"Processor, 	0,  private to Smalltalk, in Ruby use Thread "
"ProcessorScheduler, 	0,  private to Smalltalk, in Ruby use Thread"
ProfMonitor . 	1 . 
ProfMonitorEntry . 	1 . 
QuadByteString . 	1 . 
"QuadByteSymbol, 	0,  private , instances not allowed yet"
QueryExecuter . 	0 .  "implementation of indexing"
Range .  1 . "mapped to Ruby by Ruby bootstrap"
RangeEqualityIndex . 	0 .  "implementation of indexing"
RangeIndexReadStream . 	0 .  "implementation of indexing"
"RcBtreeBasicInteriorNode, 	0,   private to Smalltalk RC for now"
"RcBtreeBasicLeafNode, 	0, private to Smalltalk RC for now"
"RcBtreeInteriorNode, 	0, private to Smalltalk RC for now"
"RcBtreeLeafNode, 	0, private to Smalltalk RC for now"
"RcCollisionBucket, 	0, private to Smalltalk RC for now"
RcCounter . 	1 . 
RcCounterElement . 	1 . 
RcIdentityBag . 	1 . 
"RcIndexBucket, 	0,  private to Smalltalk RC for now"
"RcIndexBucketWithCache, 	0,  private to Smalltalk RC for now"
"RcIndexDictionary, 	0,  private to Smalltalk RC for now"
RcKeyValueDictionary . 	1 . 
RcQueue . 	1 . 
RcQueueElement . 	1 . 
RcQueueEntry . 	1 . 
RcQueueRemovalSeqNumbers . 	1 . 
RcQueueSessionComponent . 	1 . 
RcRangeEqualityIndex . 	0 .  "implementation of indexing"
ReadStream . 	1 . 
RedoLog . 	1 . 
"ReenterBlock obsolete"
Regexp .         1 . "mapped to Ruby by Ruby bootstrap"
RegexpError .    1 . "mapped to Ruby by Ruby bootstrap"
Repository . 	1 . 
ResumableTestFailure . 	1 . 
ResumableTestFailureTestCase . 	1 . 
"RubyBreakException, 	0,   private to Smalltalk"
"RubyConstantRef, 	0,   private to Smalltalk"
RubyDirectory .  1 . "mapped to Ruby by Ruby bootstrap"
RubyEnv .        1 . "mapped to Ruby by Ruby bootstrap"
RubyHash .       1 . "mapped to Ruby by Ruby bootstrap"
RubyIdentityHash .       1 . "mapped to Ruby by Ruby bootstrap"
RubyNotImplementedError .        1 . "mapped to Ruby by Ruby bootstrap"
RubyScriptError .        1 . "mapped to Ruby by Ruby bootstrap"
RubySystemExit .         1 . "mapped to Ruby by Ruby bootstrap"
RubyThreadGroup .        1 . "mapped to Ruby by Ruby bootstrap"
"RubyThrowException, 	0,   private to Smalltalk "
RubyTime .       1 . "mapped to Ruby by Ruby bootstrap"
RubyRuntimeError .   1 . "mapped to Ruby by Ruby bootstrap"
ScaledDecimal . 	1 . 
SecurityError .  1 . "mapped to Ruby by Ruby bootstrap"
GsObjectSecurityPolicy . 	1 . 
GsObjectSecurityPolicySet . 	1 . 
SelectBlock . 	0 .   "implementation of indexing"
"Semaphore, 	0,    private to Smalltalk, Ruby should use ruby ConditionVariable"
SequenceableCollection . 	1 . 
SessionTemps . 	1 .  
Set . 	1 . 
SetValuedPathEvaluator . 	0 .  "implementation of indexing"
SetValuedPathTerm . 	0 .  "implementation of indexing"
SharedQueue . 	1 . 
"SimpleBlock obsolete"
SimpleTestResource . 	1 . 
SimpleTestResourceTestCase . 	1 . 
SmallDouble .    1 . "mapped to Ruby by Ruby bootstrap"
"SmallFloat, 	0,  deprecated, should not be used from Ruby"
SmallInteger .   1 . "mapped to Ruby by Ruby bootstrap"
RubySocket .     1 . "mapped to Ruby by Ruby bootstrap"
SocketError .    1 . "mapped to Ruby by Ruby bootstrap"
SocketErrorEBADF . 	1 . 
SocketErrorECONNRESET . 	1 . 
SocketErrorENOTCONN . 	1 . 
SocketErrorEPIPE . 	1 . 
SoftCollisionBucket . 	0 . "only subclasses extendable from Ruby for now"
SoftReference . 	1 . 
SortBlockNode . 	0 .  "implementation of indexing"
SortedCollection . 	1 . 
SortNode . 	0 .  "implementation of indexing"
SortNodeArray . 	0 .  "implementation of indexing"
"StackBuffer obsolete"
AlmostOutOfStack .  1 . "mapped to Ruby by Ruby bootstrap"
"StackSegment obsolete" 
Stream . 	1 . 
String .         1 . "mapped to Ruby by Ruby bootstrap"
StringKeyValueDictionary . 	1 . 
StringPair . 	1 . 
StringPairSet . 	1 . 
SUnitDelay . 	1 . 
SUnitNameResolver . 	1 . 
SUnitTest . 	1 . 
Symbol .         1 . "mapped to Ruby by Ruby bootstrap"
SymbolAssociation . 	1 . 
SymbolDictionary . 	1 . 
SymbolKeyValueDictionary . 	1 . 
SymbolList . 	1 . 
SymbolSet . 	1 . 
System .         1 . "mapped to Ruby by Ruby bootstrap"
SystemCallError .        1 . "mapped to Ruby by Ruby bootstrap"
SystemLoginNotification . 	1 . 
TCPServer .      1 . "mapped to Ruby by Ruby bootstrap"
TCPSocket .      1 . "mapped to Ruby by Ruby bootstrap"
UDPSocket .      1 . "mapped to Ruby by Ruby bootstrap"
TestCase . 	1 . 
TestFailure . 	1 . 
TestResource . 	1 . 
TestResult . 	1 . 
TestSuite . 	1 . 
ThreadError .    1 . "mapped to Ruby by Ruby bootstrap"
Time . 	1 . 
TimeZone . 	1 . 
TimeZoneInfo . 	1 . 
TimeZoneTransition . 	1 . 
TransactionBoundaryDefaultPolicy . 	1 . 
"TraversalBuffer, 	0,  private to GBS implementation"
TrueClass .      1 . "mapped to Ruby by Ruby bootstrap"
ArgumentTypeError .      1 . "mapped to Ruby by Ruby bootstrap"
UndefinedObject .        1 . "mapped to Ruby by Ruby bootstrap"
"UnimplementedFloat1, 	0,  private to Smalltalk"
"UnimplementedFloat2, 	0,  private to Smalltalk"
UnorderedCollection . 	1 . 
UserDefinedError . 	1 . 
Exception .  1 . "mapped to Ruby by Ruby bootstrap"
UserProfile . 	1 . 
UserProfileSet . 	1 . 
"UserSecurityData, 	0,  private to Smalltalk"
VariableContext .        1 . "mapped to Ruby by Ruby bootstrap"
Warning . 	1 . 
WriteStream . 	1 . 
ZeroDivide .     1  "mapped to Ruby by Ruby bootstrap"
 }
%


set class Module
category: '*maglev-runtime'
method:
addModuleMethod: aSelector
  "a ruby primitive.
   Implement the Ruby instance method   module_function   for Module "

  ^ self addModuleMethod: aSelector env: 1"__callerEnvId" 

%


set class Module
category: '*maglev-runtime'
method:
addModuleMethod: aSelector env: envId 
  "  Implement the Ruby instance method   module_function   for Module "
      "ruby_selector_suffix dependent"
  aSelector _isSymbol ifTrue:[
    "Add entries for specified selector and all related bridge methods
       to class-side module"
    | cm base moduleMethsMod masks longSels  |
    base := aSelector prefixIfRubySelector .
    cm := self compiledMethodAt: (base _asSymbolWithRubySuffix: 16r3 " #0*& ")
                     rubyEnv: envId .
    cm ifNil:[  NameError signal:'no such method ', aSelector ].
    cm _isSmallInteger ifTrue:[
      NameError signal:'method ', aSelector , ' was undef-ed or protection overridden'
    ].
    moduleMethsMod := self moduleMethodsModule .
    masks := RubyBridge suffixOptionMasks . 
    1 to: masks size do:[ :n | |  meth fullSel |
      fullSel :=  base _asSymbolWithRubySuffix: ( masks at: n ).
      meth := self compiledMethodAt: fullSel rubyEnv: envId .
      meth ifNotNil:[ | mmth |
        mmth := meth _copyForClass: self aliasFrom: nil to: nil comment:'
COPIED by module_function' .
       mmth setRubyProtection: 0 . "ensure public"
       mmth immediateInvariant .
        moduleMethsMod addRubySelector: fullSel  method: mmth env: envId .
        meth canBeWritten ifFalse:[ |mcpy |
           mcpy := meth _copyForClass: self aliasFrom: nil to: nil comment:'
COPIED by module_function to get authorization to make private'.
         mcpy immediateInvariant .
         self addRubySelector: fullSel method: mcpy env: envId .
        ].
      ].
    ].
    longSels := self selectors4moreArgs: base env: envId .
    1 to: longSels size do:[:m | | aSel meth |
       meth := self compiledMethodAt: (aSel := longSels at:m) rubyEnv: envId .
       meth ifNotNil:[ | mmth |
         mmth := meth _copyForClass: self aliasFrom: nil to: nil comment:'
COPIED by module_function' .
        mmth setRubyProtection: 0 . "ensure public"
        mmth immediateInvariant .
         moduleMethsMod addRubySelector: aSel  method: mmth env: envId .
       ].
     ].
    self setProtection: 2"private" methods: { base } env: envId longSels: longSels. "make original method private"
  ] ifFalse:[
    aSelector == true ifTrue:[
      "Create the class-side module, and set flag so all subsequent method defs
       get replicated in the class-side module."
      self moduleMethodsModule .  
      (self transientNameSpaceForStore: envId) at: #_module_methods_all compilePut: true .
    ] ifFalse:[ 
      aSelector == false ifTrue:[ self disableModuleMethodsAll: envId ]
             ifFalse:[ ArgumentError signal:'Module>>addModuleMethod: , invalid argument' ].
    ].
  ].

%


set class Module
category: '*maglev-runtime'
method:
addModuleMethodIfEnabled: aSelector env: envId
  (self allModuleMethodsEnabled: envId) ifTrue:[ | theMod cm |
    theMod := self moduleMethodsModule .
    cm := self compiledMethodAt: aSelector rubyEnv: envId .
    cm ifNil:[ NameError signal:'no such method ', aSelector ].
    cm _isSmallInteger ifTrue:[
      NameError signal:'method ', aSelector , ' was undef-ed or protection overridden'
    ].
    theMod  addRubySelector: aSelector method: cm env: envId.
  ].

%


set class Module
category: '*maglev-runtime'
method:
addRubyClassVar: aSymbol value: aValue env: envId
    " Returns aValue"
  | assoc tns  |
  aSymbol _isSymbol ifFalse:[ ArgumentTypeError signal:'name of a class variable must be a Symbol' ].
  (aSymbol at:1 equals:'@@') ifFalse:[ NameError signal:'class variable must start with @@'].
  tns := self _rubyClassVarNameSpaceForStore: aSymbol env: envId .
       "(SessionTemps current at:#TrapCV otherwise: false) ifTrue:[ self pause ]."
  ^ tns at: aSymbol classVarPut: aValue .
 


%


set class Module
category: '*maglev-runtime'
method:
addRubyVirtualSuperclass: includedClass forMeta: forMetaBool env: envId

"If receiver does not already contain includedClass as a virtual superclass,
 insert a copy of includedClass into the superclass chain of the receiver.
 After the insert, the current superclass will be superclass of the copy, 
 and the receiver's superclass will be the copy .  The copy will be a
 ruby virtual class , and is the result."

  | aCls incCls list res startVirt vCls aModu includedVcls |
  (includedClass isKindOf: Module) ifFalse:[
    ArgumentTypeError signal:'argument is not a Module'
  ].
  includedClass isRubyVirtual ifTrue:[
    self error:'argument to addRubyVirtualSuperclass is a virtual class'.
  ].
  list := { includedClass } .
  incCls := includedClass rubySuperclass: envId .
  [ incCls ~~ Object ] whileTrue:[ | primary |
     (primary := incCls rubyPrimaryCopy) class == Module ifTrue:[
       list add: primary
     ].
     incCls := incCls rubySuperclass: envId 
  ].
  aModu := list at: 1 .
  (includedVcls := self _includedModule: aModu env: envId) ifNotNil:[
    vCls := includedVcls .
  ] ifNil:[
    vCls := self _rubyIncludeModule: aModu envId: envId .
  ].
  res := vCls  .
  2 to: list size do:[ :n | 
    aModu := list at: n .
    (includedVcls := self _includedModule: aModu env: envId) ifNotNil:[
      vCls := includedVcls . 
    ] ifNil:[
      vCls := vCls _rubyIncludeModule: aModu envId: envId .
    ].
  ].
  ^ res

%


set class Module
category: '*maglev-runtime'
method:
allModuleMethodsEnabled: envId
  | tns assoc |
  self isMetaModule ifFalse:[
    (tns := self transientNameSpace: envId) ifNotNil:[
      assoc := tns associationAt: #_module_methods_all otherwise: nil . 
      assoc ifNotNil:[ ^ assoc _value == true ].
    ].
  ].
  ^ false

%


set class Module
category: '*maglev-browsing'
method:
allSubclassesDo: aBlock 

   self allSubclasses do: [:cl | aBlock value: cl].

%


set class Module
category: '*maglev-browsing'
method:
asClassSideNode
	^ OBMetaclassNode on: self theNonMetaClass

%


set class Module
category: '*maglev-browsing'
method:
asNode
  ^ OBClassNode on: self

%


set class Module
category: '*maglev-runtime'
method:
bootAddConstAssociation: aName env: envId 
  "returns an Association"
  
  ^ (self transientNameSpaceForStore:  envId ) bootAddConstAssociation: aName env: envId

%


set class Module
category: '*maglev-runtime'
method:
bootConstantLookup: aSym env: envId 
  "compile time resolve constant, returns an Association or nil "
  | cls assoc trap |
  cls := self classForConstantLookup: envId forModuleEval: false .
  assoc := (cls transientNameSpaceForStore:  envId) 
                bootConstantLookup: aSym env: envId .
  assoc ifNotNil:[ ^ assoc ].
  self ~~ Object ifTrue:[
    ^ Object bootConstantLookup: aSym env: envId.
  ].
  ^ nil

%


set class Module
category: '*maglev-Ruby support'
method:
changeToSecurityPolicyForRubyExtension: anObjectSecurityPolicy

"If self objectSecurityPolicy ~~ anObjectSecurityPolicy,
 then for self and  self class
   move each of
     the class 
     the class's  env 1, method dict (if any)
   to specfied GsObjectSecurityPolicy.
 
 This allows env 1..n methods and name spaces to be added 
 by UserProfile's  with write access to anObjectSecurityPolicy .
 Ruby methods don't use categories, so don't bother with categorys.
 Env 0 methods/dicts will not be moved, and the package manager
 will use session methods for .mcz code in env 0, if the
 receiver was originally in  SystemObjectSecurityPolicy .
"
 | aCls cvs |
 (cvs := classVars) ifNotNil:[ 
    cvs _objectSecurityPolicy: anObjectSecurityPolicy 
  ].
  aCls := self .
  2 timesRepeat:[ | mds dict |
    aCls _objectSecurityPolicy: anObjectSecurityPolicy .
    mds := aCls.methDicts .
    mds _isArray ifTrue:[
      mds _objectSecurityPolicy: anObjectSecurityPolicy .
      dict := aCls persistentMethodDictForEnv: 1 .
      dict ifNotNil:[ dict _objectSecurityPolicy: anObjectSecurityPolicy ].
    ].
    aCls := self class 
  ].
%


set class Module
category: '*maglev-runtime'
method:
childScopeAt: aSymbol isDefine: definBool env: envId 
 
  ^ (self transientNameSpaceForStore:  envId ) 
       childScopeAt: aSymbol isDefine: definBool env: envId 
  
  

%


set class Module
category: '*maglev-runtime'
method:
classForConstantLookup: envId forModuleEval: aBoolean 
   ^ self

%


set class Module
category: '*maglev-runtime'
method:
clearTransientState: envId
  | tmds ofs |
  ofs := envId*4 .
  (tmds := transientMethDicts) _isArray ifTrue:[
     tmds _rubyAt:ofs put: nil ;  "method dict"
          _rubyAt:ofs + 1 put: nil ;  "name space"
          _rubyAt:ofs + 2 put: nil .  "ruby super class"
  ].
  "higher layer reponsible for _refreshClassCache"

%


set class Module
category: '*maglev-cextensions'
method:
defineCextMeth: selPrefixStr cFunc: addressInt nArgs: nArgs prot: protInt
  "Install the C function at address addressInt as instance method 
   in env 1 of receiver with ruby selector selPrefixStr .
   library is left as nil ( resolves to entire process)
  
   protInt ignored for now."
 
  | cf |
  cf := CCallout cextNamed: selPrefixStr cFunc: addressInt nArgs: nArgs .
  cf _compileCextCaller: selPrefixStr In: self
   

%


set class Module
category: '*maglev-Browsing'
method:
definition

  ^ 'Module newModule name: ', name printString , '
       "does not include code to install in a dictionary" '
%


set class Module
category: '*maglev-runtime'
method:
deleteMethods: envId
  "Delete receivers' method dictionaries for the specified envId"
  | mds ofs |
  ofs := envId*4 .
  (mds := transientMethDicts) _isArray ifTrue:[ 
     mds _rubyAt:ofs put: nil .  "method dict"
  ].
  (mds := methDicts) _isArray ifTrue:[ | rns |
    rns := mds atOrNil: ofs + 2 .
    mds _rubyAt: ofs put: nil .  "method dict"
  ].

%


set class Module
category: '*maglev-runtime'
method:
disableModuleMethodsAll: envId
   (self transientNameSpaceForStore: envId)  at: #_module_methods_all  compilePut: false 

%


set class Module
category: '*maglev-Ruby support'
method:
extraDict
  "browser support"
  ^ nil
%


set class Module
category: '*maglev-runtime'
method:
freezeModule
  | envId | 
  envId := 1"__callerEnvId" .
  self _freezeModule: envId .
  self virtualClass _freezeModule: envId .

%


set class Module
category: '*maglev-runtime'
method:
initNameSpacesForExtend: envId
  "create name spaces and method dicts for extending a module.
   Returns receiver's transient name space"
  | tns tmds ofs pm |
  ofs := envId*4 + 2 .
  tns := (tmds := transientMethDicts) atOrNil: ofs .
  tns ifNil:[ | pns mds |
    (mds := methDicts) _isArray ifTrue:[
      pns := mds atOrNil: ofs .
    ] ifFalse:[
      mds := { mds }. "convert from single env 0 dict"
      methDicts := mds .
    ].
    (pns == nil and:[ (pm := RubyCompilerState current persistenceMode)  ]) ifTrue:[
      "extending a class in persistent mode"
      pns := RubyNameSpace new initializeForModule: self  env: envId.
      mds _rubyAt: ofs - 1  put: pns .
      (mds atOrNil: 1) ifNotNil:[ "a class with existing smalltalk behavior"
        self isRubySingletonClass ifFalse:[
          RubyContext default trackRubyClass: self env: envId  . "for ruby context reset"
        ].
      ].
    ].
    tns := RubyTransientNameSpace new initializeForModule: self persistentCopy: pns env: envId. 
    tmds _rubyAt: ofs - 1 put: tns . 
    self isCommitted ifTrue:[ (SessionTemps current at: #RubyPinnedClasses) add: self ].
  ] ifNotNil:[
     "else class already created or extended from ruby"
  ].
  tns moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to extend a frozen module/class'].
  ^ tns

%


set class Module
category: '*maglev-runtime'
method:
init_parentNs: parTns tns: tnsArg env: envId
  | pm cst tns  |
  pm := (cst := RubyCompilerState current) persistenceMode .
  self _setRubyModulePersistentBit: pm .

  (tns := tnsArg) ifNotNil:[
     self installTransientNameSpace: tns persistentMode: pm env: envId .
    (pm and:[ cst installingPrims ]) ifTrue:[
        tns copyAssociationsToPns "for constants created at compile time"
     ].
  ] ifNil:[
    tns := self initNameSpacesForExtend: envId . 
  ].
  tns parent: parTns "maybe nil" .
  
  "for MODULE_moduFcts , do not share name spaces"
  ^ self

%


set class Module
category: '*maglev-runtime'
method:
installTransientNameSpace: tns persistentMode: pm env: envId 
  "for use at module/class creation. incoming tns maybe created by parser"
  | pns |
  tns initializeForModule: self env: envId .
  self transientNameSpace: envId put: tns .
  pm ifTrue:[
    (pns := tns persistentCopy) ifNil:[ 
	   pns := RubyNameSpace new .
	   tns persistentCopy: pns .
	 ].
    pns initializeForModule: self env: envId .
    self persistentNameSpace: envId put: pns 
  ].

%


set class Module
category: '*maglev-Ruby support'
method:
isMetaOrModule
  ^ true
%


set class Module
category: '*maglev-runtime'
method:
isVirtual
  ^ (format bitAnd: 16r14000) ~~ 0

%


set class Module
category: '*maglev-runtime'
method:
is_aModule
  ^ true

%


set class Module
category: '*maglev-runtime'
method:
methodDefined: aSymbol rubyEnv: envId 
  "do not look in Object or Kernel .
   does not consider the package policy.
   Returns nil or an Array { cm . protectionOverride }"
  | cls cm override |
  cls := self .
  [
    (cm := cls compiledMethodAt: aSymbol rubyEnv: envId) ifNotNil:[
      cm _isSmallInteger ifTrue:[
        override ifNil:[ override := cm ].
      ] ifFalse:[
        ^ { cm . override ifNil:[ 0 ] }
      ].
    ].
    "include virtual classes for ruby modules. "
    cls := cls rubySuperclass: envId .
    cls ~~ Object  "stop before Object, assume virtualKernel above Object"
  ] whileTrue .
  ^ nil

%


set class Module
category: '*maglev-runtime'
method:
moduleFrozen
  "a ruby primitive"
  | ns |
  ns := self nameSpace: 1"__callerEnvId" .
  ns ifNotNil:[ ^ ns moduleFrozen ].
  ^ false

%


set class Module
category: '*maglev-runtime'
method:
moduleIncludeSelf: envId
  "include a Module into itself so methods are available as both
     class and instance methods.   "
  | cls pm vCls vmd | 
  cls := self virtualClass .
  [ cls ~~ nil ] whileTrue:[
    (cls rubyPrimaryCopy == self and:[ cls isRubyModuleIncludeSelf]) ifTrue:[
      ^ self "already included"
    ] ifFalse:[ 
      cls := cls rubySuperclass: envId .
    ].
  ].
  self _rubyModuleIncludeSelfEnv: envId .
  ^ self

%


set class Module
category: '*maglev-runtime'
method:
moduleMethodsModule
  "Return the virtual Module which holds the class-side method dictionary
   for a Module "
  | meta |
  meta := self virtualClass .
  meta isRubyModuleFunctions ifTrue:[ ^ meta ].
  self error:'cannot find module_methods module' .

%


set class Module
category: '*maglev-runtime'
method:
moduleMethodsModuleOrNil
  "Return the virtual Module which holds the class-side method dictionary
   for a Module "
  | meta |
  meta := self virtualClass .
  meta isRubyModuleFunctions ifTrue:[ ^ meta ].
  ^ nil

%


set class Module
category: '*maglev-runtime'
method:
myClass
   "used when receiver can be either a Module or a RubyNameSpace"
  ^ self

%


set class Module
category: '*maglev-Ruby support'
method:
name
  | pcopy |
  (self isMetaModule and:[ (pcopy := primaryCopy) ~~ nil]) ifTrue:[
     ^ 'meta', pcopy name
  ].
  ^ name
%


set class Module
category: '*maglev-runtime'
method:
nameSpace: envId
  "return name space to use for an environment 1  lookup.  if tns is nil,
     then use pns for a faulted-in ruby class "
  | ns mds isVirt ofs |
  ofs := envId*4 + 2 .
  ns := transientMethDicts atOrNil: ofs .
  ns ifNotNil:[ ^ ns ].
  
  (isVirt := (format bitAnd: 16r14000) ~~ 0) ifTrue:[ 
     "RUBY_VIRTUAL, or MODULE_inclSelf"
     ns := primaryCopy transientNameSpace: envId .
     ns ifNotNil:[ ^ ns ].
  ].
  (mds := methDicts) _isArray ifTrue:[
    ns := mds atOrNil: ofs  .
    ns ifNotNil:[ ^ ns ].
  ].
  isVirt ifTrue:[
    ns := primaryCopy persistentNameSpace: envId 
  ].
  ^ ns

%


set class Module
category: '*maglev-runtime'
method:
nameSpaceForGlobalVar
  "called from generated code for RubyGlobalVarAliasNode"
  ^ self transientNameSpaceForStore: 1"__callerEnvId" 

%


set class Module
category: '*maglev-runtime'
method:
nameSpaceOrNil: envId
  ^ self nameSpace: envId

%


set class Module
category: '*maglev-runtime'
method:
newModule
  "a ruby primitive"
  | mod |
  mod := self _newModule .
  mod name: #'' .
  mod init_parentNs: nil tns: nil  env: 1"__callerEnvId" .
  ^ mod

%


set class Module
category: '*maglev-runtime'
method:
persistentNameSpace: envId
  "result nil if receiver not persistable yet"
  | mds |
  (mds := methDicts) _isArray ifFalse:[
     envId == 0 ifTrue:[ ^ mds ].
     ^ nil
  ].
  ^ mds atOrNil: (envId*4 + 2)

%


set class Module
category: '*maglev-runtime'
method:
persistentNameSpace: envId put: aNs
  | mds ofs |
  (mds := methDicts) _isArray ifFalse:[
    self persistentMethodDictForEnv: envId put: nil .
    mds := methDicts .
  ].
  (mds atOrNil: (ofs := envId*4 + 2) ) ifNotNil:[
    self error:' persistentNameSpace already present'
  ].
  mds _rubyAt: ofs - 1  put: aNs

%


set class Module
category: '*maglev-runtime'
method:
persistentRubySuperclass: envId
  | mds |
  (mds := methDicts) _isArray ifFalse:[
	  ^ superClass "use env0 super class" 
  ].

  ^ (mds atOrNil: (envId*4 + 3) ) ifNil:[ superClass "use env0 super class"  ]

%


set class Module
category: '*maglev-runtime'
method:
persistentRubySuperclass: envId put: aCls
  | mds |
  (mds := methDicts) _isArray ifFalse:[
    self persistentMethodDictForEnv: envId put: nil .
    mds := methDicts .
  ].
  mds _rubyAt: (envId*4 + 2"zero based") put: aCls 

%


set class Module
category: '*maglev-runtime'
method:
removeConst: aSymbol env: envId
  | rns |
  rns := self transientNameSpace: envId .
  rns ifNil:[
    rns := self persistentNameSpace: envId .
    (rns includesKey: aSymbol) ifFalse:[
      NameError signal: 'uninitialized constant object ', aSymbol.
    ].
    RubyCompilerState current persistenceMode ifFalse:[
      ArgumentTypeError signal:'cannot remove constant from persistent module in transient mode'
    ].
  ].
  self _incrementCachedConstantsSerialNum .
  ^ rns removeConst: aSymbol .

%


set class Module
category: '*maglev-runtime'
method:
removeRubySelector: selectorSymbol env: envId 
  | ns |
  (ns := self nameSpace: envId ) ifNotNil:[
    ns moduleFrozen ifTrue:[
      ArgumentTypeError signal:'attempt to modify a frozen module/class'
    ].
  ].
  ^ super removeRubySelector: selectorSymbol env: envId

%


set class Module
category: '*maglev-runtime'
method:
rubyAncestors
  "a ruby primitive"
  | arr cls  envId |
  envId := 1"__callerEnvId" .
  arr := { self }.
  cls := self .
  [ true ] whileTrue:[
     "ancestors of a Module stops before Object"
     cls := cls rubySuperclass:  envId .
     (cls == nil or:[ cls == Object]) ifTrue:[ ^ arr ] .
     cls isRubyVirtual ifTrue:[
       arr add: cls rubyPrimaryCopy
     ] ifFalse:[
       (cls nameSpace: envId) ifNotNil:[ arr add: cls ]
                "ifNil:[ do not include a smalltalk class] ".
     ].
  ].

%


set class Module
category: '*maglev-runtime'
method:
rubyAutoload: aName file: fileToLoad
  "a ruby primitive.
   Register fileToLoad as the file to require when aSymbolName
   is referenced in the current name space."

  ^ (self transientNameSpaceForStore: 1"__callerEnvId") 
       rubyAutoload: aName asSymbol file: fileToLoad

%


set class Module
category: '*maglev-runtime'
method:
rubyAutoloadAssociationAtOrNil: aSymbol env: envId
  | rns assoc cls |
  cls := self .
  [ cls ~~ nil ] whileTrue:[ 
    "probe both normal and virtual classes"
     (rns := cls nameSpace: envId) ifNotNil: [
       (assoc := rns resolveConstant: aSymbol) ifNotNil:[
	      assoc isDefined ifNotNil:[
           ^ assoc
         ].
       ] .
     ] .
     cls := cls rubySuperclass: envId .
  ].
  ^ nil

%


set class Module
category: '*maglev-runtime'
method:
rubyAutoloadFileFor: aSymOrString
  "a ruby primitive.
  Return the name of the file registered for autoloading of aSymOrString."
  | assoc ns |
  ns := self nameSpace: 1"__callerEnvId" .
  ns ifNotNil:[
    assoc := ns resolveConstant: aSymOrString asSymbol .
    assoc ifNotNil: [
      (assoc isKindOf: RubyAutoloadAssociation) ifTrue: [ ^ assoc fileName ]
    ] .
  ].
  ^ nil .

%


set class Module
category: '*maglev-runtime'
method:
rubyClassVarRemove: aSymbol
  "a ruby primitive"
  ^ self rubyClassVarRemove: aSymbol env: 1"__callerEnvId"

%


set class Module
category: '*maglev-runtime'
method:
rubyClassVarRemove: aSymbol env: envId
  " Returns value of the removed  class var "
  | assoc  ns |
  aSymbol _isSymbol ifFalse:[ ArgumentTypeError signal:'name of a class variable must be a Symbol' ].
  (aSymbol at:1 equals:'@@') ifFalse:[ NameError signal:'class variable must start with @@'].
  assoc := self _rubyClassVarAssociation: aSymbol env: envId .
  assoc ifNil:[   
    NameError signal: 'class variable not found ' , aSymbol .
  ].
  ns := self _classForRubyClassVar transientNameSpaceForStore: envId .
  ns removeKey: aSymbol .
  ^ assoc _value

%


set class Module
category: '*maglev-runtime'
method:
rubyClassVarSet: aName value: aValue
  "a ruby primitive"

  ^ self addRubyClassVar: aName asSymbol value: aValue env: 1"__callerEnvId"

%


set class Module
category: '*maglev-runtime'
method:
rubyConstants
    "a ruby primitive.
     Return an Array of Strings, the names of constants defined in this module
       and any included modules."
    | moduleNames arr topCls cls envId |
    moduleNames := self _rubyConstants: (envId := 1"__callerEnvId") .
    cls := self .
    topCls := Object .
    [ cls ~~ topCls ] whileTrue:[ 
      cls := cls rubySuperclass: envId .
      cls ~~ topCls ifTrue:[ moduleNames addAll: (cls _rubyConstants: envId)].   
    ].
    (arr := { }) _addAllFromNsc: moduleNames .
    1 to: arr size do:[:m |
      arr at: m put: (arr at: m) asString . "convert Symbols to Strings"
    ].
    ^ arr 

%


set class Module
category: '*maglev-runtime'
method:
rubyConstAssociationAt: aSymbol env: envId
  | assoc val  |
  name == #'' ifTrue:[ NameError signal:'empty NameSpace for ', self name ] .

  assoc := self rubyConstAssociationAtOrNil: aSymbol env: envId.
  assoc ifNil:[
    val := self @ruby1:const_missing: aSymbol .
    (assoc := RubySymbolAssociation newWithKey: aSymbol) _value: val .
  ].
  ^ assoc

%


set class Module
category: '*maglev-runtime'
method:
rubyConstAssociationAtOrNil: aSymbol env: envId
  | rns assoc cls |
  cls := self .
  [ cls ~~ nil and:[ cls ~~ Object "fix Trac672"] ] whileTrue:[ 
    "probe both normal and virtual classes"
     (rns := cls nameSpace: envId) ifNotNil: [
       (assoc := rns resolveConstant: aSymbol) ifNotNil:[
          ^ assoc
       ] .
     ] .
     cls := cls rubySuperclass: envId .
  ].
  ^ nil

%


set class Module
category: '*maglev-runtime'
method:
rubyConstAt: aName
  "a ruby primitive"
    "Runtime support for a dynamic Colon2Node, searches superClasses "
  | assoc sym envId |
  (sym := aName) _isSymbol ifFalse:[
    sym _isOneByteString ifFalse:[
       aName ifNil:[ ArgumentTypeError signal:'left side of :: is not a class/module ' ]
               ifNotNil:[ ArgumentTypeError signal:'expected a String or Symbol'].
    ].
    sym := aName asSymbol .
  ].
  assoc := self rubyConstAssociationAtOrNil: sym env: (envId := 1"__callerEnvId").
  assoc ifNotNil:[ ^ assoc _valueFor: sym inClass: self env: envId  ].
  self == Kernel ifTrue:[ assoc := Object rubyConstAssociationAtOrNil: sym env: envId ].
  assoc ifNotNil:[ ^ assoc _valueFor: sym inClass: self env: envId  ]
        ifNil:[ ^ self @ruby1:const_missing: sym ]

%


set class Module
category: '*maglev-runtime'
method:
rubyConstAt: sym env: envId
  "called from smalltalk code"
  | assoc |
  sym _isSymbol ifFalse:[ ArgumentTypeError signal:'expected a Symbol'].
  assoc := self rubyConstAssociationAtOrNil: sym env: envId .
  ^ assoc ifNotNil:[  assoc _valueFor: sym inClass: self env: envId  ]
        ifNil:[ self @ruby1:const_missing: sym ]

%


set class Module
category: '*maglev-runtime'
method:
rubyConstAt: aName env: envId put: aValue
    "Returns aValue"
  | tns sym pns cst |
  (sym := aName) _isSymbol ifFalse:[ 
    sym _isOneByteString ifFalse:[ ArgumentTypeError signal:'expected a String or Symbol'].
    sym := aName asSymbol .
  ].
  tns := self transientNameSpaceForStore: envId .
  self _incrementCachedConstantsSerialNum .
  tns at: sym transientRuntimePut: aValue .    
  (pns := tns persistentCopy) ifNotNil:[
    (cst := RubyCompilerState current) persistenceMode ifTrue:[
      [ 
        pns at: sym runtimePut: aValue 
      ] on: ArgumentTypeError do:[:ex | | ok |
        cst installingPrims ifTrue:[
          (pns associationAt: sym otherwise: nil ) ifNotNil:[ :assoc|
             assoc isInvariant ifTrue:[ | oldVal |
               (oldVal := assoc _valueNoAction ) == aValue 
                ifTrue:[ ok := true ]
                ifFalse:[
                  [ 
                    oldVal = aValue ifTrue:[ ok := true ] "attempt env 0 first"
                  ] onSynchronous: AbstractException do:[ :ax |
                    [ 
                      (oldVal @ruby1:==: aValue ) ifTrue:[ ok := true ] 
                    ] onSynchronous: AbstractException do:[ :bx | "reattempt in env 0"
                      "ignore"
                    ]
                 ]. 
               ].
             ] 
          ]
        ].
        ok ifNil:[ ex outer ]
      ].
    ].
  ].
  (aValue isBehavior and:[  aValue name == #'']) ifTrue:[ 
     "assignment to a ruby constant has side effect of naming a class 
        which previously had no name."
     aValue setNameSpaceParent: tns  name: sym env: envId .
  ].
  ^ aValue 

%


set class Module
category: '*maglev-runtime'
method:
rubyConstAt: aName put: aValue
  "called from generated code, for a ConstDeclNode."
  ^ self rubyConstAt: aName env: 1"__callerEnvId" put: aValue

%


set class Module
category: '*maglev-runtime'
method:
rubyConstDecl: aSymbol put: aValue
   "a ruby primitive"
  ^ self rubyConstAt: aSymbol env: 1"__callerEnvId" put: aValue .

%


set class Module
category: '*maglev-runtime'
method:
rubyConstDefined: aSymbol 
  "a ruby primitive. "
  |  rns assoc envId |
  (rns := self nameSpace: (envId := 1"__callerEnvId") ) ifNotNil: [ 
    assoc := rns resolveConstant: aSymbol  .
    assoc ifNotNil:[ ^ assoc isDefined ].
  ].
  self == Object ifTrue:[ | cls |
    cls := self rubySuperclass: envId .
    assoc := self rubyConstAssociationAtOrNil: aSymbol env: envId .
    assoc ifNotNil:[ ^ assoc isDefined ].
  ].
  ^ false 

"ruby 1.9 code:   | assoc |
  assoc := self rubyConstAssociationAtOrNil: aSymbol 
        env: 1""__callerEnvId""  . 
  assoc ifNotNil:[ ^ assoc isDefined ]. 
  ^ false 
"

%


set class Module
category: '*maglev-runtime'
method:
rubyContextReset: envId
  "note,  does not remove the virtual class for moduFcts from Kernel.
   Kernel is the only persistent Module reachable after clearing
   Object's name spaces. "
  | mds ofs |
  ofs := envId*4 .
  (mds := transientMethDicts) _isArray ifTrue:[ 
     mds _rubyAt:ofs put: nil ;  "method dict"
          _rubyAt:ofs + 1 put: nil ;  "name space"
          _rubyAt:ofs + 2 put: nil .  "ruby super class"
  ].
  (mds := methDicts) _isArray ifTrue:[ 
    mds _rubyAt: ofs put: nil ;  "method dict"
        _rubyAt: ofs + 1 put: nil ;  "name space"
        _rubyAt: ofs + 2 put: nil .  "ruby super class"
  ].

%


set class Module
category: '*maglev-runtime'
method:
rubyFullName
  "a ruby primitive"
  ^ self rubyFullName: 1"__callerEnvId"

%


set class Module
category: '*maglev-runtime'
method:
rubyFullName: envId
  "called from Smalltalk code only"
  | ns nam |
  (ns := self nameSpace: envId ) ifNotNil:[ ^ ns fullName ].
  (nam := name) ifNil:[ ^ '' copy ].  "Ruby meta classes have no name"
  ^ String withAll: nam

%


set class Module
category: '*maglev-cextensions'
method:
rubyFullNameSymbol: envId
  "called from Smalltalk code only"
  | ns nam |
  (ns := self nameSpace: envId ) ifNotNil:[ ^ ns fullNameSymbol ].
  (nam := name) ifNil:[ ^ #''  ].  "Ruby meta classes have no name"
  ^ nam asSymbol

%


set class Module
category: '*maglev-runtime'
method:
rubyGlobalVarAssoc: aSymbol env: envId 
  "returns an Associaton, creating it if needed" 
  | rns assoc val  |
  self == Object ifFalse:[ ArgumentTypeError signal:'global variables only in Object.nameSpace'].
  rns := self transientNameSpaceForStore: envId  .
  (assoc := rns associationAt: aSymbol otherwise: nil) ifNil:[ 
    val := rns initialValueForGlobal: aSymbol.
    assoc := RubyGlobalVarAssociation newWithKey: aSymbol .
    val ifNotNil:[ assoc globalVarValue: val ]. 
    rns addTransientAssociation: assoc .
  ].
  ^ assoc 

%


set class Module
category: '*maglev-runtime'
method:
rubyGlobalVarDefinedQ: aSymbol
  "called from generated code"
  | rns assoc |
  self == Object ifFalse:[ ArgumentTypeError signal:'global variables only in Object.nameSpace'].
  rns := self transientNameSpaceForStore: 1"__callerEnvId"  .
  (assoc := rns associationAt: aSymbol otherwise: nil) ifNil:[ 
     (rns initialValueForGlobal: aSymbol) ifNil:[ ^ nil ].
  ] ifNotNil:[
    assoc  isDefined ifNil:[ ^ nil ]
  ].
  ^ 'global-variable' copy 

%


set class Module
category: '*maglev-runtime'
method:
rubyIncludedModules
  "a ruby primitive"
  | arr cls envId |
  envId := 1"__callerEnvId" .
  arr := {  }.
  cls := self rubySuperclass: envId .
  [ true ] whileTrue:[
     cls ifNil:[ ^ arr ] .
     cls == Object ifTrue:[ ^ arr ]. "stop at Object for Modules"
     cls isRubyVirtual ifTrue:[ | primary |
          (primary := cls rubyPrimaryCopy) class == Module ifTrue:[
               arr add: primary
           ].
     ]. 
     cls := cls rubySuperclass: envId 
  ].

%


set class Module
category: '*maglev-runtime'
method:
rubyInstanceMethods: includeSuper protection: protInt
  "a ruby primitive"
  ^ self rubyMethods: includeSuper protection: protInt env: 1"__callerEnvId"

%


set class Module
category: '*maglev-runtime'
method:
rubyInstvarNames
  "a ruby primitive"
  ^ self rubyInstvarNames: 1"__callerEnvId"

%


set class Module
category: '*maglev-runtime'
method:
rubyInstvarNames: envId 
  "Return an Array of Strings, the elements are names of
   dynamic class instance variables"
| list blk ns |
list := IdentitySet new .
blk := [ :aKey :aValue | 
  ((aKey at:1) == $@ and:[ aKey size >= 2 and:[ (aKey at: 2) ~~ $@ ]]) ifTrue:[ list add: aKey  ].
].
(ns := self transientNameSpace: envId) ifNotNil:[  ns keysAndValuesDo: blk ].
(ns := self persistentNameSpace: envId) ifNotNil:[ ns keysAndValuesDo: blk ].
list := list asArray .
1 to: list size do:[:n |  list at: n put: (String withAll: (list at:n)) ].
^ list

%


set class Module
category: '*maglev-runtime'
method:
rubyMethodProtection
  "a ruby primitive"
  ^ self rubyMethodProtection: 1"__callerEnvId"

%


set class Module
category: '*maglev-runtime'
method:
rubyMethodProtection: envId
  | ns cvs  |
  self isRubyModuleFunctions ifFalse:[ 
    (ns := self nameSpace: envId ) ifNotNil:[ ^ ns methodProtection ].
  ].
  ^ 0

%


set class Module
category: '*maglev-runtime'
method:
rubyMethodProtection: anInt env: envId
  "anInt :  0==public, 1==protected, 2==private"
  (self  transientNameSpaceForStore: envId) methodProtection: anInt

%


set class Module
category: '*maglev-runtime'
method:
rubyMethods: includeSuper protection: protInt
  "a ruby primitive"
  | cls |
   (cls := self moduleMethodsModuleOrNil) ifNotNil:[ 
     ^ cls rubyMethods: includeSuper protection: protInt env: 1"__callerEnvId"
   ] ifNil:[ 
     ^ IdentitySet new 
   ]

%


set class Module
category: '*maglev-runtime'
method:
rubyMethods: includeSuper protection: protInt env: envId
  "Return an IdentitySet of Symbols "
| set curClass  hidden inclSuper |
  set := IdentitySet new . hidden := IdentitySet new .
  self nonBridgeRubySelectorsInto: set hiddenInto: hidden 
	  protection: protInt env: envId .
  includeSuper  ifTrue:[
    curClass := self .
    [ true ] whileTrue:[  
       (curClass := curClass rubySuperclass: envId) ifNil:[ ^ set ]. 
       curClass == Object ifTrue:[ ^ set  "Fix Trac 719" ].
       curClass isRubyModuleInclude ifTrue:[
          curClass nonBridgeRubySelectorsInto: set hiddenInto: hidden 
			protection: protInt env: envId 
       ].
     ].
  ].
  ^  set 

%


set class Module
category: '*maglev-runtime'
method:
rubyName
  ^ name

%


set class Module
category: '*maglev-runtime'
method:
rubyNameForModuleInit
  ^ name

%


set class Module
category: '*maglev-runtime'
method:
rubyRemoveConst: aSymbol
  "a ruby primitive.
   Remove the constant named aSymbol.  
   Pre-defined classes and singleton objects (e.g., true) can't be removed.
   TODO: Check if the constant is disallowed at a finer granularity.  The PickAxe book documents
   that you aren't allowed to remove the constants, but from irb, I can do: 
      Object.class_eval { remove_const :Object } 
   and it seems to work..."
  ^ self removeConst: aSymbol env: 1"__callerEnvId"

%


set class Module
category: '*maglev-runtime'
method:
rubyTransientConst: aSymbol put: aValue block: aBlock
  "a Ruby primitive"
  self rubyTransientConst: aSymbol put: aValue block: aBlock env: 1"__callerEnvId"

%


set class Module
category: '*maglev-runtime'
method:
rubyTransientConst: aName put: aValue block: aBlock env: envId
  | tns pns sym assoc passoc |
  (sym := aName) _isSymbol ifFalse:[ 
    sym _isOneByteString ifFalse:[ ArgumentTypeError signal:'expected a String or Symbol'].
    sym := aName asSymbol .
  ].
  tns := self transientNameSpaceForStore: envId .
  self _incrementCachedConstantsSerialNum .
  assoc := tns _at: sym otherwise: nil .
  (assoc ~~ nil and:[ assoc class == RubyTransientConstantAssociation]) ifTrue:[
      ArgumentError signal:'non-transient constant ', sym, ' already exists'.
  ].
  (pns := tns persistentCopy) ifNotNil:[ passoc := pns _at: sym otherwise: nil ].
  (passoc ~~ nil and:[ passoc class == RubyTransientConstantAssociation]) ifTrue:[
     RubyCompilerState current reloadingPrims ifFalse:[
       ArgumentError signal:'persistent non-transient constant ', sym, ' already exists'
     ].
  ].
  assoc ifNil:[
    (assoc := RubyTransientConstantAssociation new) key: sym  .
    tns addTransientAssociation: assoc .  
  ].
  assoc block: aBlock class: self env: envId ; 
        _value: aValue .
  (pns ~~ nil and:[RubyCompilerState current persistenceMode]) ifTrue:[
    passoc ifNil:[ 
      (passoc := RubyTransientConstantAssociation new) key: sym  .
      pns addAssociation: passoc .
    ].
    passoc block: aBlock class: self env: envId .  "do not do _value: "
  ].
  ^ aValue

%


set class Module
category: '*maglev-runtime'
method:
scopeAtPath: aPath env: envId  
 
  ^ (self transientNameSpaceForStore:  envId ) 
      scopeAtPath: aPath env: envId 
  
  

%


set class Module
category: '*maglev-runtime'
method:
selectors4moreArgs: baseSel  env: envId 
  "Return Ruby selectors in specified environment of the receiver with given base prefix
   and which have 4 or more fixed args 
   (i.e. which exceed max number of fixed args for a bridge method)."
      "ruby_selector_suffix dependent"
  | aDict selSet  blk |
  selSet := IdentitySet new .
  blk := [:aKey | | nFixed | 
     (aKey _rubyAt1: -4) == 35 "$#" ifTrue:[  "it is a ruby selector"
       nFixed := (aKey _rubyAt1: -3) - 48"$0" .
       (nFixed > 3 and:[ aKey at: 1 equals: baseSel ]) ifTrue:[  selSet add: aKey ].
     ].
  ].
  (aDict := self persistentMethodDictForEnv: envId ) ifNotNil:[   aDict keysDo: blk ].
  (aDict := self transientMethodDictForEnv: envId) ifNotNil:[ aDict keysDo: blk ].
  ^ selSet asArray .

%


set class Module
category: '*maglev-runtime'
method:
selectors4moreColons: baseSel  env: envId 
  "Return Ruby selectors in specified environment of the receiver with given base prefix
   and which have 4 or more colons (i.e. exceed max number of colons for a bridge method)."
  | target aDict selSet  blk |
  target := baseSel , '::::' .
  selSet := IdentitySet new .
  blk := [:aKey | (aKey at: 1 equals: target) ifTrue:[ selSet add: aKey ] ].
  (aDict := self persistentMethodDictForEnv: envId ) ifNotNil:[   aDict keysDo: blk ].
  (aDict := self transientMethodDictForEnv: envId) ifNotNil:[ aDict keysDo: blk ].
  ^ selSet asArray .

%


set class Module
category: '*maglev-runtime'
method:
setNameSpaceParent: aTransientNs env: envId 
  | tns |
  tns := self transientNameSpaceForStore: envId .
  tns parent: aTransientNs 

%


set class Module
category: '*maglev-runtime'
method:
setNameSpaceParent: aTransientNs name: aSymbol env: envId
  | tns |
  self changeNameTo: aSymbol .
  tns := self transientNameSpaceForStore: envId .
  tns parent: aTransientNs name: aSymbol.

%


set class Module
category: '*maglev-runtime'
method:
setProtection: protInt classmethods: selectorsArray
  "a ruby primitive"
  | sz |
  (sz := selectorsArray size) ~~ 0 ifTrue:[ | mm |
     (mm := self moduleMethodsModuleOrNil) ifNotNil:[ | count |
        count :=  mm setProtection: protInt methods: selectorsArray 
                env: 1"__callerEnvId" longSels: nil  .
        count == sz ifFalse:[ 
          sz == 1 ifTrue:[ NameError signal: (selectorsArray at: 1), ' not found' ].
          NameError signal: 'one or more selectors not found'.
        ].
     ].
  ].
  ^ self 

%


set class Module
category: '*maglev-runtime'
method:
setProtection: protInt methods: selectorsArray
  "a ruby primitive"
  ^ self setProtection: protInt methods: selectorsArray env: 1"__callerEnvId" longSels: nil 

%


set class Module
category: '*maglev-runtime'
method:
setProtection: protInt methods: selectorsArray env: envId longSels: longSelectors 
 "protInt is 0 public, 1 protected, 2 private.
  Returns a SmallInteger, the number of selectors in selectorsArray
  for which protection was changed"
      "ruby_selector_suffix dependent"
 | sz count |
 count := 0 .
 (sz := selectorsArray size) == 0 ifTrue:[  | tns |
    "this path not used for meta classes/modules"
  "set default visibility for subsequent compilations, and shutoff _module_methods_all"
    self rubyMethodProtection: protInt env: envId .
    self disableModuleMethodsAll: envId .
 ] ifFalse:[
   1 to: sz do:[:n | | aName baseSel genSel cm found |  
     found := false .
     aName := selectorsArray at: n .
     baseSel := aName asSymbol "do not send rubySelectorPrefixSymbol" .
     genSel := baseSel _asSymbolWithRubySuffix: 16r3 " #0*& " .
     cm := self compiledMethodAt: genSel rubyEnv: envId .
     cm ifNil:[  | ary |
       ary := self lookupSelector: genSel rubyEnv: envId .
       ary ifNotNil:[ cm := ary at: 1 ].
       " if cm not nil here, we are adding a protection override"
     ].
     cm ifNotNil:[ | masks genericBridgeCms longSels override |
       genericBridgeCms := RubyBridge genericErrCmSet .
       masks := RubyBridge suffixOptionMasks .
       1 to: masks size do:[ :n | | mask  sel mth |
         mask := masks at: n .
         sel := baseSel _asSymbolWithRubySuffix: mask .
         mth := self compiledMethodAt: sel rubyEnv: envId .
         mth ifNotNil:[
           mth _isSmallInteger ifTrue:[
             mth >= METH_prot_override_private ifTrue:[ 
               "change a protection override"
               override := METH_prot_override_public - protInt .
               self addRubySelector: sel method: override env: envId .
               found := true .
             ] ifFalse:[
               "skip an undef-ed method"
             ] 
           ] ifFalse:[
             (genericBridgeCms includes: mth) ifTrue: [ |newCm|
                "Do not change protection on generic bridge methods, as that
                 will affect all other classes that also share that method"
                newCm := mth _copyForClass: self aliasFrom: nil to: nil comment: nil.
                newCm setRubyProtection: protInt .
                newCm immediateInvariant .
                self addRubySelector: sel method: newCm env: envId .
                found := true .
              ] ifFalse: [
                mth setRubyProtection: protInt .
                found := true .
              ] . 
            ]
          ] ifNil:[  "add a protection override"
            override := METH_prot_override_public - protInt .
            self addRubySelector: sel method: override env: envId .
            found := true .
          ]
       ].
       longSels := longSelectors ifNil:[  self selectors4moreArgs: baseSel env: envId ].
       1 to: longSels size do:[:n | | sel mth |
         sel := longSels at: n.
         mth := self compiledMethodAt: sel rubyEnv: envId .
         mth ifNotNil:[
           mth _isSmallInteger ifTrue:[
             mth >= METH_prot_override_private ifTrue:[ 
               "change a protection override"
               override := METH_prot_override_public - protInt .
               self addRubySelector: sel method: override env: envId .
               found := true .
             ] ifFalse:[
               "skip an undef-ed method"
             ]
           ] ifFalse:[
             mth setRubyProtection: protInt .
             found := true .
           ].
         ]
       ].
     ] "ifNil:[  ignore methods not found ]" .
     found ifTrue:[ count := count + 1 ].
   ].
 ].
 ^ count

%


set class Module
category: '*maglev-runtime'
method:
transientMethodDictForEnv: envId
  | ofs dict |
  ofs := envId*4 + 1 .
  dict := transientMethDicts atOrNil: ofs .
  dict ifNil:[
    (format bitAnd: 16r14000) ~~ 0 ifTrue:[ 
      "GC_MODULE_inclSelf_CLASS | GC_RUBY_VIRTUAL_CLASS"
       dict := primaryCopy transientMethodDictForEnv: envId .
    ].
  ].
  ^ dict

%


set class Module
category: '*maglev-Ruby support'
method:
transientMethodDictForEnv: envId put: aValue
  "aValue should be a GsMethodDictionary, or nil ,
   caller responsible for _refreshClassCache.
   For ruby, not a protected method. 
   NOTE, VM class creation and fault-in initializes transientMethDicts
   to an empty array. "

transientMethDicts _rubyAt: (envId*4)"zero based"  put: aValue
%


set class Module
category: '*maglev-runtime'
method:
transientMethodDictForStoreEnv: envId
  | mds dict ofs |
  ofs := envId*4 + 1 .
  (mds := transientMethDicts) _isArray ifTrue:[
    dict := mds atOrNil: ofs .
  ] ifFalse:[ 
    envId == 0 ifTrue:[ dict := mds ]
  ]. 
  dict ifNotNil:[ ^ dict ].
  dict := GsMethodDictionary new .
  self transientMethodDictForEnv: envId put: dict .
  self isCommitted ifTrue:[ (SessionTemps current at: #RubyPinnedClasses) add: self ].
  ^ dict 

%


set class Module
category: '*maglev-runtime'
method:
transientNameSpace: envId
  | tns ofs | 
  tns := transientMethDicts atOrNil: (ofs := envId*4 + 2) .
  tns ifNil:[ 
    (format bitAnd: 16r14000) ~~ 0 ifTrue:[   
      "GC_RUBY_VIRTUAL_CLASS|GC_MODULE_inclSelf_CLASS"
      "rcvr is virtual , from include of a Module, or module inclSelf"
       tns := primaryCopy _transientNameSpaceAtOfs: ofs .
    ].
  ].
  ^ tns

%


set class Module
category: '*maglev-runtime'
method:
transientNameSpace: envId put: aNs 
  | tmds ofs |
  ((tmds := transientMethDicts) atOrNil: (ofs := envId*4 + 2) ) ifNotNil:[ 
     self error:' transientNameSpace already present' 
  ].
  tmds _rubyAt: ofs - 1 put: aNs . 

%


set class Module
category: '*maglev-runtime'
method:
transientNameSpaceForStore: envId
  | tns ofs |
  (format bitAnd: 16r14000) ~~ 0 ifTrue:[ "MODULE_inclSelf or RUBY_VIRTUAL"
    ^ primaryCopy transientNameSpaceForStore: envId
  ].
  ofs := envId*4 + 2 .
  (tns := transientMethDicts atOrNil: ofs) ifNotNil:[ ^ tns ].
  ^ self initNameSpacesForExtend: envId . 

%


set class Module
category: '*maglev-runtime'
method:
transientRubySuperclass: envId
  "return superclass for specified env "
  ^ transientMethDicts atOrNil: (envId*4 + 3) 

%


set class Module
category: '*maglev-runtime'
method:
transientRubySuperclass: envId put: aCls
  "NOTE, VM class creation and fault-in initializes transientMethDicts
   to an empty array. "
  self isCommitted ifTrue:[ (SessionTemps current at: #RubyPinnedClasses) add: self ].
  transientMethDicts _rubyAt: (envId*4 + 2"zero based") put: aCls

%


set class Module
category: '*maglev-Ruby support'
method:
_addRubyVirtualSuperclass: includedClass kind: kindInt envId: envId 
  | result |
(includedClass class == Module or:[ includedClass == Kernel ]) ifFalse:[ 
  ((includedClass isKindOf: Module) and:[ (includedClass isKindOf: Metaclass3) not]) ifFalse:[
    self error:'arg is not a Module'
  ].
].
(includedClass instSize == 0 or:[
   self _instVarNames = includedClass _instVarNames]) ifTrue:[
    | disallowed |
    (self isKindOfClass: Number) ifTrue:[ disallowed := true ].
    (self isKindOfClass: IO) ifTrue:[ disallowed := true ].
    (self isKindOfClass: KeyValueDictionary) ifTrue:[ disallowed := true ].
    disallowed ifNil:[ | ctxCls ctx |
      result := self _addVirtualSuperclassPrim: includedClass kind: kindInt envId: envId .
      result _isOneByteString ifTrue:[
        self error: 'insert class disallowed, ', result .
      ].
      self _clearLookupCaches: envId . 
    ] ifNotNil:[
      self error: 'receiver ' , self name , ' may not have virtual classes'.
    ].
] ifFalse:[
    self error:'inconsistent instVars'. 
].
^ result
%


set class Module
category: '*maglev-Ruby support'
method:
_addVirtualSuperclassPrim: includedClass kind: anInt envId: envId

"Returns the new virtual class, or a String describing failure. 

 Sends to super within instance methods of the receiver will bypass any
 ruby virtual classes in the superclass chain and will go to the first
 non-virtual superclass.

 Generates an error if any of the following are true
   includedClass is not an instance of Module .

   the argument defines any instVars and they are not exactly the
   same instVars as the receiver's current superclass .

   receiver is a committed class (because method-lookup cache coherency
     in multiple session case probably can't handle changes to
     superclass chain correctly)

   argument has any of these format bits set
     NON_PERSISTENT, DB_TRANSIENT, RUBY_VIRTUAL, SUBCLASS_DISALLOWED,
     INDEXABLE, NO_STRUCT_UPDATE, TRAV_BY_CALLBACK

   argument is a subclass any of the classes Number, IdentityBag,
     SequenceableCollection, KeyValueDictionary, Exception,  ExecBlock,
     Regexp.   ( this list checked for in primitive) .

   receiver is a subclass of one of the classes   Number, IO, KeyValueDictionary ,
     because those classes contain Smalltalk env0 implementations of Ruby methods
     and the env0 implementations contain sends to super,
     that would not see the virtual class.
"
<primitive: 694>
anInt _validateClass: SmallInteger .
envId _validateClass: SmallInteger .
(envId < 1 or:[ envId > 255]) ifTrue:[ OutOfRange signal:'invalid envId'].
^ self _primitiveFailed: #_addVirtualSuperclass:kind:envId:
  args: { includedClass . anInt . envId }
%


set class Module
category: '*maglev-runtime'
method:
_checkIncludeRubyModule: aModule 
  "a ruby primitive"
  | envId |
  envId := 1"__callerEnvId" .
  (aModule class == Module or:[aModule == Kernel] )  ifFalse:[
    ((aModule isKindOf: Module) and:[ (aModule isKindOf: Metaclass3) not]) ifFalse:[
      ArgumentTypeError signal:'argument to include is not a Module'
    ].
  ].
 aModule == self ifTrue:[ 
   self moduleIncludeSelf: envId   .
   ^ false "caller should not call __include"
 ].
 ^ true

%


set class Module
category: '*maglev-runtime'
method:
_classForRubyClassVar
  self isRubyModuleFunctions ifTrue:[ ^ primaryCopy ].
  ^ self

%


set class Module
category: '*maglev-runtime'
method:
_freezeModule: envId
  (self transientNameSpaceForStore: envId) freezeModule .
  (self transientMethodDictForStoreEnv: envId) immediateInvariant .

%


set class Module
category: '*maglev-runtime'
method:
_includedModule: aModule env: envId
  "returns a virtual class, or nil "  
  | aCls |
  aModule == self ifTrue:[ ^ true ].
  aCls := self rubySuperclass: envId .
  [ aCls ~~ nil ] whileTrue:[
    aCls rubyPrimaryCopy == aModule ifTrue:[
      ^ aCls
    ].
    aCls := aCls rubySuperclass: envId .
  ].
  ^ nil

%


set class Module
category: '*maglev-runtime'
method:
_includeRubyModule: aModule 
  "a ruby primitive"

  ^ self addRubyVirtualSuperclass: aModule forMeta: false 
           env: 1"__callerEnvId" .

%


set class Module
category: '*maglev-Ruby support'
method:
_incrementCachedConstantsSerialNum

"Returns the new value of the serial number, a SmallInteger.
 The VM's value of the serial number is used in RubyConstantRef prims 790 and 791"
<primitive: 840>

self _primitiveFailed: #_incrementCachedConstantsSerialNum
%


set class Module
category: '*maglev-Ruby support'
method:
_name
  | pcopy |
  (self isMetaModule and:[ (pcopy := primaryCopy) ~~ nil]) ifTrue:[
     ^ 'meta', pcopy name
  ].
  ^ name
%


set class Module
category: '*maglev-Ruby support'
method:
_persistable

^  (format bitAnd: 16r800000) == 0  "GC_RubyModuleNP bit"
%


set class Module
category: '*maglev-runtime'
method:
_rubyClassVarAssociation: aSymbol env: envId

| aCls ns assoc |
aCls := self .
[ aCls ~~ nil ] whileTrue:[ 
  (ns := aCls _classForRubyClassVar nameSpace: envId ) ifNotNil:[
    assoc := ns resolveConstant: aSymbol .
    assoc ifNotNil:[ "(SessionTemps current at:#TrapCV otherwise: false) ifTrue:[ self pause ]."
	                  ^ assoc ] .
  ].
  aCls isRubySingletonClass ifTrue:[ ^ nil ].  "per Trac 654"
  aCls := aCls rubySuperclass: envId .
].
aCls := self virtualClass .   "handle a class which contains   extend aModule , Trac 445 "
[ aCls ~~ nil ] whileTrue:[
  (ns := aCls _classForRubyClassVar nameSpace: envId) ifNotNil:[
    assoc := ns resolveConstant: aSymbol .
    assoc ifNotNil:[ "(SessionTemps current at:#TrapCV otherwise: false) ifTrue:[ self pause ]."
	                   ^ assoc ] .
  ].
  aCls := aCls rubySuperclass: envId
].
"(SessionTemps current at:#TrapCV otherwise: false) ifTrue:[ self pause ]."
^ nil

%


set class Module
category: '*maglev-runtime'
method:
_rubyClassVarDefined: aSymbol  env: envId
  "Return true or false if receiver defines the specified variable.
   does not search hierarchy."
|  assoc  ns |
(aSymbol size > 2 and:[ aSymbol at:1 equals: '@@']) ifTrue:[ 
  ( ns := self _classForRubyClassVar nameSpace: envId ) ifNotNil:[
    assoc := ns resolveConstant: aSymbol .
    assoc ifNotNil:[ ^ true ] .
  ].
  ^ false .
].
NameError signal:'invalid class variable name' .
^ false

%


set class Module
category: '*maglev-runtime'
method:
_rubyClassVarDefinedQ: aSymbol env: envId
  "Return nil  or  'class-variable' "
|  assoc  |
(aSymbol size > 2 and:[ aSymbol at:1 equals: '@@']) ifTrue:[
  assoc := self _rubyClassVarAssociation: aSymbol env: envId .
  assoc ifNotNil:[ ^ assoc definedQ ].
].
^ nil

%


set class Module
category: '*maglev-runtime'
method:
_rubyClassVarGet: aSymbol
  "called from generated code"
  ^ self _rubyClassVarGet: aSymbol env: 1"__callerEnvId"

%


set class Module
category: '*maglev-runtime'
method:
_rubyClassVarGet: aSymbol env: envId

|  assoc  |
(aSymbol size >= 2 and:[ aSymbol at:1 equals: '@@']) ifTrue:[
  assoc := self _rubyClassVarAssociation: aSymbol env: envId .
  assoc ifNotNil:[ ^ assoc _value ].
].
NameError signal: 'undefined class variable ', aSymbol .
^ nil

%


set class Module
category: '*maglev-runtime'
method:
_rubyClassVarGetOrNil: aSymbol env: envId
|  assoc  |
(aSymbol size > 2 and:[ aSymbol at:1 equals: '@@']) ifTrue:[
  assoc := self _rubyClassVarAssociation: aSymbol env: envId.
  assoc ifNotNil:[ ^ assoc _value ].
].
^ nil

%


set class Module
category: '*maglev-runtime'
method:
_rubyClassVarNames: envId
  "Return an Array of Strings, the elements are names of
   ruby class variables"
| arr ns aCls  |
arr := { } .
aCls := self .
[ aCls ~~ nil ] whileTrue:[
  ns := aCls _classForRubyClassVar nameSpace: envId .
  ns ifNotNil:[
    ns  keysAndValuesDo:[ :aKey :aValue | 
      ((aKey at:1) == $@ and:[ aKey size > 2 and:[ (aKey at: 2) == $@ ]]) ifTrue:[ 
         arr add: (String withAll: aKey) 
      ].
    ].
  ].
  aCls := aCls rubySuperclass: envId
].
^ arr

%


set class Module
category: '*maglev-runtime'
method:
_rubyClassVarNameSpaceForStore: aSymbol env: envId
  "returns a transient name space or nil "
| aCls cvCls ns tns assoc |
aCls := self .
[ aCls ~~ nil ] whileTrue:[ 
  (ns := (cvCls := aCls _classForRubyClassVar) nameSpace: envId ) ifNotNil:[
    assoc := ns resolveConstant: aSymbol .
    assoc ifNotNil:[ tns := cvCls transientNameSpaceForStore: envId .
	   "(SessionTemps current at:#TrapCV otherwise: false) ifTrue:[ self pause ]."
	   ^ tns  ] .
  ].
  aCls := aCls rubySuperclass: envId
].
aCls := self virtualClass .   "handle a class which contains   extend aModule , Trac 445 "
[ aCls ~~ nil ] whileTrue:[
  (ns := (cvCls := aCls _classForRubyClassVar) nameSpace: envId) ifNotNil:[
    assoc := ns resolveConstant: aSymbol .
    assoc ifNotNil:[ tns :=  cvCls transientNameSpaceForStore: envId .
	    "(SessionTemps current at:#TrapCV otherwise: false) ifTrue:[ self pause ]."
	   ^ tns ] .
  ].
  aCls := aCls rubySuperclass: envId
].
tns := self _classForRubyClassVar transientNameSpaceForStore: envId .
"(SessionTemps current at:#TrapCV otherwise: false) ifTrue:[ self pause ]."
^ tns 

%


set class Module
category: '*maglev-runtime'
method:
_rubyConstants: envId
    "Return an IdentitySet of the names of the constants defined in this module "
   | ns |
   (ns := self nameSpace: envId) ifNotNil:[ ^ ns _rubyConstants].
   ^ IdentitySet new 

%


set class Module
category: '*maglev-runtime'
method:
_rubyConstantsFreeze
  "a ruby primitive.
   intended for use during bootstrap loading only"
  | ns | 
  RubyCompilerState current reloadingPrims ifFalse:[
    (ns := self persistentNameSpace: 1"__callerEnvId") ifNotNil:[ 
      ns _rubyConstantsFreeze 
    ].
  ]

%


set class Module
category: '*maglev-runtime'
method:
_rubyIncludeModule: aModule envId: envId
    | vCls pm |
    vCls := self _addRubyVirtualSuperclass: aModule kind: OM_RUBY_INCLUDE_Module
                envId: envId  .
    vCls transientRubySuperclass: envId put: (self rubySuperclass: envId) .
    pm := RubyCompilerState current persistenceMode .
    (pm and:[ self _persistable]) ifTrue:[
      vCls persistentRubySuperclass: envId put: (self persistentRubySuperclass: envId) .
      self persistentRubySuperclass: envId put: vCls .
    ].
    self transientRubySuperclass: envId put: vCls .
    self _clearLookupCaches: envId .
    ^ vCls

%


set class Module
category: '*maglev-runtime'
method:
_rubyInspect
  "a ruby primitive"
  | str |
  str := self rubyFullName: 1"__callerEnvId" .
  str size ~~ 0 ifTrue:[ ^ str ].
  str := '#<Module:0x' copy .
  str addAll: self asOop hex ;
     add: $> . 
  ^ str 

%


set class Module
category: '*maglev-runtime'
method:
_rubyInspect: envId 
  "called from smalltalk only"
  ^ self rubyFullName: envId

%


set class Module
category: '*maglev-runtime'
method:
_rubyModuleIncludeSelfEnv: envId
  "include a Module into a Module to provide the methodDict for module_methods"
  | vCls modCls pm tsuper psuper nam |
"nam := self name . "
  vCls := self _addRubyVirtualSuperclass: self kind: OM_MODULE_INCLUDE_self 
    envId: envId .  
  envId ~~ 1 ifTrue:[ self error:'envId > 1 not implemented']. "for name spaces"
  modCls := self virtualClass .
  tsuper := modCls rubySuperclass: envId .
  psuper := modCls persistentRubySuperclass: envId .
  "vCls transientNameSpace left as nil  "
  pm := RubyCompilerState current persistenceMode .
  (pm and:[ self _persistable]) ifTrue:[
    "vCls persistentNameSpace left as nil"
    vCls persistentRubySuperclass: envId put: psuper .
    modCls persistentRubySuperclass:  envId put:vCls .
  ].
  vCls transientRubySuperclass: envId put: tsuper .
  modCls transientRubySuperclass: envId put: vCls .
  self _clearLookupCaches: envId  .  "takes care of modCls also"
"GsFile gciLogServer:'IncludeModuleSelf for ' , nam . "
"nam == #Type ifTrue:[ self pause ]. "
  ^ vCls

%


set class Module
category: '*maglev-Ruby support'
method:
_rubySubclassOf: aClass
  "a ruby primitive"
  ^ self _rubySubclassOf: aClass env: 1"__callerEnvId" 
%


set class Module
category: '*maglev-Ruby support'
method:
_rubySubclassOf: aClass env: envId
  <primitive: 842>
  envId _validateClass: SmallInteger .
  self _primitiveFailed: #_rubySubclassOf:env: args: { aClass . envId }
%


set class Module
category: '*maglev-Ruby support'
method:
_rubySuperclass
  "a Ruby primitive"
  ^ self rubySuperclass: 1"__callerEnvId"
%


set class Module
category: '*maglev-runtime'
method:
_setPersistable
  "a ruby primitive"  
  | envId tns  parTns parPns savePm cst  wasPersistable |
  envId := 1"__callerEnvId" .
  wasPersistable := self _setRubyModulePersistentBit: true .
  wasPersistable ifFalse:[
    tns := self transientNameSpaceForStore: envId .
    ( parTns := tns parent) == tns ifTrue:[  ^ self  "do nothing  when self==Object" ].
    (parPns := parTns persistentCopy) ifNil:[ ArgumentError signal:'parent is not yet persistable' ].
    tns persistentCopy ifNil:[ | pns |
       pns := RubyNameSpace new initializeForModule: self env: envId .
       pns parent: parPns .
       tns persistentCopy: pns .
       self persistentNameSpace: envId put: pns . 
    ].
    cst := RubyCompilerState current .
    savePm := cst persistenceMode .
    [ cst persistenceMode: true .
       parTns at: name runtimePut: self .
    ] ensure:[
      cst persistenceMode: savePm
    ]
  ].

%


set class Module
category: '*maglev-runtime'
method:
_setTopNameSpace: tns persistent: pns  env: envId
  | ofs |
  "Private initialize persistent name space for Object , for bootstrap only."
  self == Object ifFalse:[ self error:'can only be applied to Object' ].
  self initNameSpacesForExtend: envId .
  ofs := envId*4 + 2  .
  methDicts _rubyAt: ofs - 1 put: pns .
  transientMethDicts _rubyAt: ofs - 1 put: tns

%


set class Module
category: '*maglev-runtime'
method:
_transientNameSpaceAtOfs: ofs 
  "receiver should be a primary copy"
  ^ transientMethDicts atOrNil: ofs

%

