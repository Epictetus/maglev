
set class Float
category: '*maglev-Ruby numeric'
classmethod:
fromReadStreamInts: anArray

"anArray should be an Array of size two holding SmallIntegers,
 each representing 4 bytes of the IEEE binary result. "

^ self _mathPrim: anArray opcode: 0
%


set class Float
category: '*maglev-Ruby numeric'
method:
_rubyAdd: aNumber

"Returns the sum of the receiver and aNumber."

<primitive: 106>
^ self _rubyRetry: #'+#1__' coercing: aNumber env: 1"__callerEnvId"
%


set class Float
category: '*maglev-Ruby numeric'
method:
_rubyAsFormattedString

"Return a string formatted as specified by the hardcoded array. Note that this is a hack
 to allow Benchmark class to work around the lack of Ruby formatting (gsub!)"

^ self asStringUsingFormat: #(10 6 false)
%


set class Float
category: '*maglev-Ruby numeric'
method:
_rubyAsString

"Returns a String corresponding to the value of the receiver.  Where applicable,
 returns one of the following Strings: 'Infinity', '-Infinity', 'NaN' ."

<primitive: 510>
^ self _primitiveFailed: #_rubyAsString
%


set class Float
category: '*maglev-Ruby numeric'
method:
_rubyDivide: aNumber

"Divide the receiver by aNumber and returns the result."

<primitive: 108>
^ self _rubyRetry: #'/#1__' coercing: aNumber env: 1"__callerEnvId"
%


set class Float
category: '*maglev-Ruby numeric'
method:
_rubyEqual: aNumber

"Returns true if the receiver is equal to aNumber; returns false otherwise."

<primitive: 119>
^ [ | arr |
     arr := aNumber @ruby1:coerce: self. 
	"arr is { coercedSelf . coercedNum }"
      (arr at: 1) @ruby1:==: (arr at: 2)
  ] onSynchronous: Exception do:[:ex|
    aNumber @ruby1:==: self 
  ]
%


set class Float
category: '*maglev-Ruby numeric'
method:
_rubyLt: aNumber

"Returns true if the receiver is less than aNumber; returns false otherwise."

<primitive: 118>
^ self _rubyRetry: #'<#1__' coercing: aNumber env: 1"__callerEnvId"
%


set class Float
category: '*maglev-Ruby numeric'
method:
_rubyLteq: aNumber

"Returns true if the receiver is less than aNumber; returns false otherwise."

<primitive: 121>
^ self _rubyRetry: #'<=#1__' coercing: aNumber env: 1"__callerEnvId"
%


set class Float
category: '*maglev-Ruby numeric'
method:
_rubyModulo: aNumber

"Divide the receiver by aNumber and returns the remainder.  
 Returns PlusQuietNaN if aNumber == 0.0 "
<primitive: 800>
^ self _rubyRetry: #'%#1__' coercing: aNumber env: 1"__callerEnvId"
%


set class Float
category: '*maglev-Ruby numeric'
method:
_rubyMultiply: aNumber

"Multiply the receiver by aNumber and returns the result."

<primitive: 102>
^ self _rubyRetry: #'*#1__' coercing: aNumber env: 1"__callerEnvId"
%


set class Float
category: '*maglev-Ruby numeric'
method:
_rubyRaisedTo: aNumber

^ self _mathPrim: aNumber opcode: 4
%


set class Float
category: '*maglev-Ruby numeric'
method:
_rubyRetry: aSelector coercing: aNumber env: envId
  "aSelector includes the ruby selector suffix.
   send sites are ruby_selector_suffix dependent"
| arr  |
aNumber _isNumber ifFalse:[
  ArgumentTypeError signal:'cannot coerce a ',  aNumber class name  , ' to a Number'
].
arr := self @ruby1:coerce: aNumber.  "coerce aNumber to a Float"
"arr is { coercedNum . coercedSelf} "

^ (arr at: 2) with: (arr at: 1) perform: aSelector env: envId 
%


set class Float
category: '*maglev-Ruby numeric'
method:
_rubySubtract: aNumber

"Returns the difference between the receiver and aNumber."

<primitive: 107>
^ self _rubyRetry: #'-#1__' coercing: aNumber env: 1"__callerEnvId"
%


set class Float
category: '*maglev-Ruby numeric'
method:
_ruby_eqlQ: aNumber

"If aNumber is an instance of Float or SmallDouble, return
 true if receiver and aNumber have the same numeric value,
 else return false."

aNumber _isFloat ifTrue:[
  ^ self = aNumber .
].
^ false
%


set class Float
category: '*maglev-Ruby numeric'
method:
_ruby_finiteQ

"Return true if the receiver is neither a NaN nor an Infinity"

  ^ self _getKind < 3 or:[ self = 0.0]
%


set class Float
category: '*maglev-Ruby numeric'
method:
_ruby_infiniteQ

self _getKind == 3 ifTrue:[ "is an infinity"
  self > 0.0 ifTrue:[ ^ 1 ].
  ^ -1
].
^ nil
%

