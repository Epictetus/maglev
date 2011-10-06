
set class Kernel
category: '*maglev-runtime'
method:
atExit: aBlock
  aBlock _isExecBlock ifFalse:[ ArgumentTypeError signal:'argument must be a block'].
  RubyCompiler addExitHandler: aBlock .
  ^ aBlock

%


set class Kernel
category: '*maglev-Ruby support'
method:
catch1: aSymbol do: aBlock
  "catch:do: for env 1"
| sym | 
(sym := aSymbol) _isSymbol ifFalse:[
  aSymbol _isRubyString ifTrue:[ sym := aSymbol asSymbol ]
               ifFalse:[ ArgumentTypeError signal:'expected a Symbol or String'].
].
aBlock _isExecBlock ifFalse:[ 
  (aBlock ifNil:[ CannotReturn] ifNotNil:[ ArgumentTypeError]) signal:'expected a block'.
].
^ aBlock rescue1: RubyThrowException do:[ :ex | 
    ex name == sym ifTrue:[ 
      ex return: ex _value 
    ].
    ex pass 
  ]  
%


set class Kernel
category: '*maglev-Ruby support'
method:
catch2: aSymbol do: aBlock
  "catch:do: for env 2"
| sym | 
(sym := aSymbol) _isSymbol ifFalse:[
  aSymbol _isRubyString ifTrue:[ sym := aSymbol asSymbol ]
               ifFalse:[ ArgumentTypeError signal:'expected a Symbol or String'].
].
aBlock _isExecBlock ifFalse:[ 
  (aBlock ifNil:[ CannotReturn] ifNotNil:[ ArgumentTypeError]) signal:'expected a block'.
].
^ aBlock rescue2: RubyThrowException do:[ :ex | 
    ex name == sym ifTrue:[ 
      ex return: ex _value
    ].
    ex pass 
  ]  
%


set class Kernel
category: '*maglev-Ruby support'
method:
execv: commandString opcode: opcodeInt envVars: envArray fdDups: fdDupsArray args: arrayOfStrings

"A ruby primitive , used by Ruby Kernel.exec , Kernel.spawn

 opcode == 0 , exec , using execve() which
  terminates the current session to execute specified command.  
  This primitive will return
  or generate Smalltalk errors only in the event of malloc failures
  or invalid argument kinds.  Otherwise the primitive terminates
  Smalltalk/Ruby execution, disconnects from the stone process,
  and calls execv().  If execv() returns, error information is printed
  to stdout and this process exits with non-zero status. 

 opcode == 1 , spawn  using posix_spawn and return pid of the child
    or a negated errno value. (Not available on HPUX)

 envArray contains environment variables information, to modify
 the environment before execv() is called.
  envArray[0] is a boolean saying whether to call clearenv() (linux only)
  envArray[1..n] are name/value or name/nil  pairs to be used
  as args to setenv() or unsetenv() .

  fdDupsArray is an array of file descriptor pairs to be used to call dup2()
  before execv() is called.   See Ruby documentation of Kernel.exec for
  more details.
"
<primitive: 890>
self _primitiveFailed: #execv:opcode:envVars:fdDups:args: 
     args: { commandString . opcodeInt . envArray . fdDupsArray . arrayOfStrings }

%


set class Kernel
category: '*maglev-runtime'
method:
includeRubyModule: aModule
  "a ruby primitive.
   used by  include   at  ruby main program level, not within a class or module"
  self class == Object ifFalse:[ ArgumentTypeError signal:'invalid receiver for include'].
  ^ Object addRubyVirtualSuperclass: aModule forMeta: false env: 1"__callerEnvId"  

%


set class Kernel
category: '*maglev-runtime'
method:
resolveSmalltalkGlobal: aName
  "intented for bootstrap only"
  | assoc |
  assoc := System myUserProfile resolveSymbol: (aName asSymbol) .
  assoc ifNil:[ self error:'Smalltalk global ' , aName , ' not found' ].
  ^ assoc _value

%


set class Kernel
category: '*maglev-runtime'
method:
rubyGlobalVariables
  "a ruby primitive"
  | ns arr |
  ns := Object transientNameSpace: 1"__callerEnvId"  .
  arr := { } .
  ns keysAndValuesDo:[ :aKey :aVal |  
    (aKey at: 1) ==  $$  ifTrue:[ arr add: (String withAll: aKey)].
  ].
  "  #'$?'  not currently included  , it is a runtime dynamic value"
  ^ arr 

%


set class Kernel
category: '*maglev-runtime'
method:
rubyKernelAutoload: aName file: aFile
  "a ruby primitive.
  Install an RubyAutoloadAssociation in the top level name space"
  ^ (Object nameSpace: 1"__callerEnvId" ) 
       rubyAutoload: aName asSymbol file: aFile 

%


set class Kernel
category: '*maglev-runtime'
method:
rubyKernelAutoloadFileFor:  aSymOrString
  "a ruby primitive.
  Return the name of the file registered for autoloading of aSymOrString.
  see also Module>>rubyAutoloadFileFor:"
  | assoc ns |
  ns := Object nameSpace: 1"__callerEnvId" .
  assoc := ns resolveConstant: aSymOrString asSymbol .
  assoc ifNotNil: [
    (assoc isKindOf: RubyAutoloadAssociation) ifTrue: [ ^ assoc fileName ] .
  ] .
  ^ nil .

%


set class Kernel
category: '*maglev-Ruby support'
method:
selectRead: readIos write: writeIos error: errIos timeout: timeoutArr

 <primitive: 845> 
  "primitive adds the readIos,writeIos,errIos
   to the scheduler's poll info, and always fails.
   If any of the IOs is a GsFile, that IO will
   have been immediately added to the ioSelectResult.
   Each of readIos, writeIos, errIos must be nil or an Array of IOs.
   timeoutArr must be a one-element array containing nil, or a
   SmallInteger milliseconds to wait .
   Returns an Array of 3 Arrays, containing IOs ready for read,write
   or with errors.
"
 | proc sched res msToWait |
 sched := GsProcess _scheduler .
 proc := sched activeProcess .
 (res := proc clearIoSelectResult) ifNotNil:[ ^ res ].
 msToWait := timeoutArr at: 1 .  "Ruby code does arg kind checking"
 msToWait ifNil:[
   sched _waitForIoSelect  .  "wait forever until a socket is ready"
 ] ifNotNil:[
   sched _waitForIoSelect: msToWait
 ].
 "scheduler poll primitive 192 has filled in in proc.ioSelectResult"
 ^ proc clearIoSelectResult
%


set class Kernel
category: '*maglev-runtime'
method:
smalltalkUserGlobalsAt: aName put: aValue
  "intended for bootstrap only"
  | assoc |
  RubyCompilerState current installingPrims ifFalse:[
	self error:'storing into UserGlobals not allowed outside of bootstrap'
  ].
  UserGlobals at: aName put: aValue .

%


set class Kernel
category: '*maglev-Ruby support'
method:
sprintf: formatString with: argsArray

"A ruby primitive.
 Ruby  printf per Pickaxe page 529..532"
<primitive: 767>
formatString _validateClass: String .
argsArray _validateClass: Array  .
self _primitiveFailed: #sprintf:with: args: { formatString . argsArray }
%


set class Kernel
category: '*maglev-Ruby support'
method:
throw: aSymbol
| sym |
(sym := aSymbol) _isSymbol ifFalse:[
  aSymbol _isRubyString ifTrue:[ sym := aSymbol asSymbol ]
               ifFalse:[ ArgumentTypeError signal:'expected a Symbol or String'].
].
RubyThrowException new name: sym ; signal
%


set class Kernel
category: '*maglev-Ruby support'
method:
throw: aSymbol with: aValue
| sym |
(sym := aSymbol) _isSymbol ifFalse:[
  aSymbol _isRubyString ifTrue:[ sym := aSymbol asSymbol ]
               ifFalse:[ ArgumentTypeError signal:'expected a Symbol or String'].
].
RubyThrowException new name: sym value: aValue ; signal
%


set class Kernel
category: '*maglev-runtime'
method:
traceGlobalVarAssign: aSymbol block: aBlock
  "a ruby primitive"
  | tmps dict envId |
  envId := 1"__callerEnvId" .
  (aSymbol at:1) == $$ ifFalse:[ ArgumentError signal:'name must begin with ''$'' ' ].
  tmps := SessionTemps current .
  dict := tmps at: #RUBY_traceGlobalVars otherwise: nil .
  aBlock ifNil:[  "untrace_var" | tns |
      tns := Object transientNameSpaceForStore: envId .
      (tns includesKey: aSymbol) ifFalse:[ NameError signal: 'undefined global variable ' , aSymbol ].
      dict ifNil:[ ^ { } ].
      ^ dict removeKey: aSymbol otherwise: { }  .
  ] ifNotNil:[ | trc |
     dict ifNil:[
       tmps at:#RUBY_traceGlobalVars put: ( dict := IdentityKeyValueDictionary new )
     ].
     trc := dict at: aSymbol otherwise: nil.
     trc ifNil:[  dict at: aSymbol put: ( trc := { } ) ].
     trc add: aBlock.
     Module _incrementCachedConstantsSerialNum .
     ^ nil
  ]

%


set class Kernel
category: '*maglev-runtime'
method:
waitpid: pidInt flags: flagsInt
  "Calls waitpid() .
   Updates Ruby $? and returns an Array  { pid . status },
   else returns a SmallInteger errno."
| parr pid rawStatus |
parr := System _waitpid: pidInt flags: flagsInt .
parr _isArray ifTrue:[
  pid := parr at: 1 .
  rawStatus := parr at: 2 .
  pid == pidInt ifTrue:[ | arr |
    arr := { rawStatus .  (rawStatus bitAnd: 16rFF)"childStatus" .
           (pid == pidInt)"completedBool" . nil "errMsg". 0 "errno" } .
    GsProcess _current _rubyThreadDataAt: 2 "GC_RubyGsProcessClientData_childProcStatus"
              put:  (RubyProcessStatus with: arr ) .
  ].
].
^ parr  

%


set class Kernel
category: '*maglev-runtime'
method:
_eval: aString binding: aBinding with: vcGlobalsSelf fileName: aFileName lineNumber: anInt 
    "A ruby primitive.
     Evaluate aString.  vcGlobals is an Array of size 3"
  | defStk lexSelfStk envId cld aClass |
  envId := 1"__callerEnvId" .
  cld := GsProcess _current _clientData .
  aClass := aBinding methodDefTarget .

  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: aClass .
  cld at: 7 put: aClass " _rubyThreadDataAt: 7 put: " .

  (lexSelfStk := RubyCompilerState current lexicalSelfStack ) push: nil .
  ^ [
       RubyCompiler new evaluateString: aString binding: aBinding with: vcGlobalsSelf 
           fileName: aFileName lineNumber: anInt  env: envId
    ] ensure:[
       defStk pop: aClass . 
       lexSelfStk pop: nil .
    ].

%


set class Kernel
category: '*maglev-runtime'
method:
_forkvExec: commandStr
  "A ruby primitive.
   Result is an Array  { rawStatus . childStatus . resultString , errMsg, errno } "
| arr resultStr |
arr := System _performOnServer: commandStr .
resultStr := arr at: 3 .
resultStr ifNil:[  | rawStatus childStatus errno errMsg |
  rawStatus := arr at: 1 .
  childStatus := arr at: 2 .
  errno := arr at: 5 .
  errMsg := arr at: 4 .
  commandStr _error: #hostErrPerform args:{ errMsg . errno . rawStatus . childStatus }.
  ^ nil
].
GsProcess _current _rubyThreadDataAt: 2 "OC_RubyGsProcessClientData_childProcStatus+1"
            put:  (RubyProcessStatus with: arr ) .
^ arr

%


set class Kernel
category: '*maglev-Ruby support'
method:
_highPriorityWaitForMilliseconds: millisecondCount
  "Set the active process's priority high and then
   suspends the active process for millisecondCount milliseconds.
   Returns number of milliseconds slept.
   millisecondCount==0 is equivalent to yield .
   "
  | sched oldPrio proc nowMs interval |
  sched := GsProcess _scheduler .
  nowMs := sched _now .
  interval := millisecondCount .
  interval <= 10 ifTrue:[
    interval == 0 ifTrue:[
      " don't change priority if just yielding"
      sched _waitForMilliseconds: interval  .
      ^ sched _now - nowMs .
    ].
    interval <= 0 ifTrue:[
      ArgumentError signal:'sleep time must be positive'
    ].
    "don't sleep less than probable clock resolution, to avoid
    failing to yield."
    interval := 10 . 
  ].
  "raise priority so timeout will interrupt other thread running hot"
  proc := sched activeProcess .
  oldPrio := proc _raisePriority .
  sched _waitForMilliseconds: interval  .
  proc priority: oldPrio .
  ^ sched _now - nowMs .
%


set class Kernel
category: '*maglev-Ruby support'
method:
_lastDnuProtection

"Returns a SmallInteger 0..2 the method protection associated with 
 most recent ruby MNU, and clears that state in the VM."
<primitive: 787>

self _primitiveFailed: #_lastDnuProtection
%


set class Kernel
category: '*maglev-runtime'
method:
_rubyLoop1: aBlock
  "a ruby primitive, conforming to Object>>_rubyEach1: "
    aBlock ifNil:[ CannotReturn signal:'no block given']. 
    [ 
      [
        [ true ] whileTrue:[    aBlock @ruby1:value ].     
      ] onException: RubyStopIterationError do: [:ex |   "added for 1.8.7"
        ^ self "Ruby StopIteration caught, terminate iteraton"
      ]
    ] onException: RubyBreakException do: [:ex | | args |
      args := ex gsArguments .
      (args at: 1)  ifTrue:[  "Ruby break, terminate enumeration"
        ^ args at: 2
      ] ifFalse:[
        ex retry "Ruby retry - restart the enumeration"
      ]
    ] .
    ^ self

%


set class Kernel
category: '*maglev-runtime'
method:
_rubyLoop2: aBlock
  "a ruby primitive, conforming to Object>>_rubyEach1: "
    aBlock ifNil:[ CannotReturn signal:'no block given'].
    [
      [
        [ true ] whileTrue:[    aBlock @ruby1:value ].
      ] onException: RubyStopIterationError do: [:ex |   "added for 1.8.7"
        ^ self "Ruby StopIteration caught, terminate iteraton"
      ]
    ] onException: RubyBreakException do: [:ex | | args |
      args := ex gsArguments .
      (args at: 1)  ifTrue:[  "Ruby break, terminate enumeration"
        ^ args at: 2
      ] ifFalse:[
        ex retry "Ruby retry - restart the enumeration"
      ]
    ] .
    ^ self

%


set class Kernel
category: '*maglev-runtime'
method:
_system: commandStr
  "A ruby primitive.
   Result is an Array  { rawStatus . childStatus . childCompletedBool  . 
            errMsg. errno } "
| arr done |
arr := System __system: commandStr .
GsProcess _current _rubyThreadDataAt: 2 "GC_RubyGsProcessClientData_childProcStatus"
            put:  (RubyProcessStatus with: arr ) .
^ arr

%


set class Kernel
category: '*maglev-Ruby support'
method:
_waitForMilliseconds: millisecondCount
  "Suspends the active process for millisecondCount milliseconds.
   Returns number of milliseconds slept.
   millisecondCount==0 is equivalent to yield .
   "
  | sched proc nowMs interval |
  sched := GsProcess _scheduler .
  nowMs := sched _now .
  interval := millisecondCount .
  interval <= 10 ifTrue:[
    interval == 0 ifTrue:[
      " don't change priority if just yielding"
      sched _waitForMilliseconds: interval  .
      ^ sched _now - nowMs .
    ].
    interval <= 0 ifTrue:[
      ArgumentError signal:'sleep time must be positive'
    ].
    "don't sleep less than probable clock resolution, to avoid
    failing to yield."
    interval := 10 . 
  ].
  proc := sched activeProcess .
  sched _waitForMilliseconds: interval  .
  ^ sched _now - nowMs .
%

