
set class Integer
category: '*maglev-Ruby numeric'
classmethod:
fromString: aString radix: aSmallInt
  "result is 0 if aString contains all spaces, or
   first non-space character of aString is not a
   a legal numeric digit for specified radix."
<primitive: 893>
aString _validateClasses: { String . MultiByteString } .
aSmallInt _validateClass: SmallInteger .
(aSmallInt < 0 or:[ aSmallInt > 36]) ifTrue:[
  OutOfRange signal:'radix out of range'
].
self _primitiveFailed: #fromString:radix: args: { aString . aSmallInt }
%


set class Integer
category: '*maglev'
method:
rubyLiteralNodeClass 
	^ RubyFixnumNode

%


set class Integer
category: '*maglev-Ruby numeric'
method:
_asHexDigest

| digits idx num res |
digits := String new .
self <= 0 ifTrue:[ OutOfRange signal:'_asHexDigest requires positive number'].
num := self .
idx := 0 .
[num = 0] whileFalse: [
  idx := idx + 1 .
  digits codePointAt: idx put: (num bitAnd: 16rFF) .
  num := num bitShift: -8 . 
].
"the bytes are now in reverse order and must be swapped around"
res := String new: idx .
1 to: idx do:[:n |
  res codePointAt: n put: (digits codePointAt: idx - n + 1 )
].
^ res
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_bitShiftRight: aNumber

"Returns the receiver shifted right by aNumber bits"

^ self bitShift: 0 - aNumber .
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyAdd: anInteger

"Returns the sum of the receiver and anInteger."

<primitive: 258>

^ self _rubyRetry: #'+#1__' coercing: anInteger env: 1"__callerEnvId"
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyBitAnd: anInteger

"a ruby primitive.
 Returns an Integer whose bits are the logical and of the receiver's bits and
 the bits of anInteger."

<primitive: 740>
^ self @ruby1:__bit_and: anInteger
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyBitOr: anInteger

"A ruby primitive.
 Returns an Integer whose bits are the logical or of the receiver's bits and
 the bits of anInteger."

<primitive: 741>
^ self @ruby1:__bit_or: anInteger
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyBitXor: anInteger

"A ruby primitive.
 Returns an Integer whose bits are the logical xor of the receiver's bits and
 the bits of anInteger."

<primitive: 743>
^ self @ruby1:__bit_xor: anInteger
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyDivide: aNumber

"a ruby primitive.
 Returns the result of dividing the receiver by anInteger."

<primitive: 263>
(aNumber == 0)
  ifTrue: [^ self _errorDivideByZero]
  ifFalse: [^ self _rubyRetry: #'/#1__' coercing: aNumber env: 1"__callerEnvId" ]
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyEqual: aNumber

"A ruby primitive.
 Returns true if the receiver is equal to aNumber; returns false otherwise."

<primitive: 24>
aNumber _isFloat ifTrue:[ ^ self asFloat = aNumber ].
"envId := self __callerEnvId ."
^ [ | arr |
    arr := aNumber @ruby1:coerce: self. "arr is { coercedSelf . coercedNum }"
    (arr at: 1) @ruby1:==: (arr at: 2)
  ] onSynchronous: Exception do:[:ex|
    aNumber @ruby1:==: self
  ]
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyGt: aNumber

"A ruby primitive.
 Returns true if the receiver is greater than aNumber; returns false
 otherwise."

<primitive: 22>
aNumber _isFloat ifTrue:[ ^ self asFloat > aNumber ].
^ self _rubyRetry: #'>#1__' coercing: aNumber env: 1"__callerEnvId"
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyGteq: aNumber

"A ruby primitive.
 Returns true if the receive is greater than aNumber; returns false
 otherwise."

<primitive: 23>
aNumber _isFloat ifTrue:[ ^ self asFloat >= aNumber ].
^ self _rubyRetry: #'>=#1__' coercing: aNumber env: 1"__callerEnvId"
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyLt: aNumber

"A ruby primitive.
 Returns true if the receiver is less than aNumber; returns false otherwise."

<primitive: 20>
aNumber _isFloat ifTrue:[ ^ self asFloat < aNumber ].
^ self _rubyRetry: #'<#1__' coercing: aNumber env: 1"__callerEnvId"
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyLteq: aNumber
 
"A ruby primitive.
 Returns true if the receiver is less than or equal to aNumber; returns false
 otherwise."

<primitive: 21>
aNumber _isFloat ifTrue:[ ^ self asFloat <= aNumber ].
^ self _rubyRetry: #'<=#1__' coercing: aNumber env: 1"__callerEnvId"
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyModulo: aNumber

"a ruby primitive.
 Returns the modulus defined in terms of // ."
<primitive: 264>
(aNumber == 0) ifTrue:[ ^ self _errorDivideByZero].
^ self _rubyRetry: #'%#1__' coercing: aNumber env: 1"__callerEnvId"
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyMultiply: anInteger

"Returns the product of the receiver and anInteger."

<primitive: 260>

^ self _rubyRetry: #'*#1__' coercing: anInteger env: 1"__callerEnvId"
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyRaisedTo: aNumber
  "a ruby primitive"
(aNumber _isInteger) ifFalse:[
  aNumber _isNumber ifFalse:[ ArgumentTypeError signal:'argument is not a Numeric'].
  ^ self @ruby1:__fraised_to: aNumber 
].
aNumber < 0 ifTrue:[
  ^ 1.0 / (self _raisedToPositiveInteger: (0 - aNumber))
].
^ [
   self _raisedToPositiveInteger: aNumber
  ] onSynchronous: FloatingPointError do:[:ex |
    ex number == 2503 ifTrue:[  "ex is LargeInteger overflow error"
       ex return:( self asFloat raisedToInteger: aNumber)
    ] .
    ex outer
  ]
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyShiftLeft: anInteger

"A ruby primitive.
 If anInteger is positive, the shift is to the left"

<primitive: 742>
^ self @ruby1:__shift_left: anInteger
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubySubtract: anInteger

"Returns the difference between the receiver and anInteger."

<primitive: 259>
^ self _rubyRetry: #'-#1__' coercing: anInteger env: 1"__callerEnvId"
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyTo: anInteger

^ Range from: self to: anInteger 
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_rubyTo_: anInteger

^ Range from: self limit: anInteger 
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_ruby_eqlQ: aNumber

"Return true if aNumber is a Bignum with the same value as self."

aNumber _isInteger ifFalse:[ ^ false ].
aNumber _isSmallInteger == self _isSmallInteger ifFalse:[ ^ false ].
^ self = aNumber
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_st_rubyBitAnd: anInteger

"called from smalltalk.
 Returns an Integer whose bits are the logical and of the receiver's bits and
 the bits of anInteger."

<primitive: 740>
^ self @ruby1:__bit_and: anInteger
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_st_rubyBitOr: anInteger

"called from smalltalk.
 Returns an Integer whose bits are the logical or of the receiver's bits and
 the bits of anInteger."

<primitive: 741>
^ self @ruby1:__bit_or: anInteger
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_st_rubyBitXor: anInteger

"called from smalltalk.
 Returns an Integer whose bits are the logical xor of the receiver's bits and
 the bits of anInteger."

<primitive: 743>
^ self @ruby1:__bit_xor: anInteger
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_st_rubyDivide: aNumber

"invoked from smalltalk.
 Returns the result of dividing the receiver by anInteger."

<primitive: 263>
(aNumber == 0)
  ifTrue: [^ self _errorDivideByZero]
  ifFalse: [^ self _rubyRetry: #'/#1__' coercing: aNumber env: 1"__threadRubyEnvId" ]
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_st_rubyEqual: aNumber

"A ruby primitive.
 Returns true if the receiver is equal to aNumber; returns false otherwise."

<primitive: 24>
aNumber _isFloat ifTrue:[ ^ self asFloat = aNumber ].
"envId := self __threadRubyEnvId .  "
^ [ | arr |
    arr := aNumber @ruby1:coerce: self. "arr is { coercedSelf . coercedNum }"
    (arr at: 1) @ruby1:==: (arr at: 2)
  ] onSynchronous: Exception do:[:ex|
    aNumber @ruby1:==: self 
  ]
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_st_rubyGt: aNumber

"called from smalltalk.
 Returns true if the receiver is greater than aNumber; returns false
 otherwise."

<primitive: 22>
aNumber _isFloat ifTrue:[ ^ self asFloat > aNumber ].
^ self _rubyRetry: #'>#1__' coercing: aNumber env: 1"__threadRubyEnvId"
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_st_rubyGteq: aNumber

"called from smalltalk.
 Returns true if the receive is greater than aNumber; returns false
 otherwise."

<primitive: 23>
aNumber _isFloat ifTrue:[ ^ self asFloat >= aNumber ].
^ self _rubyRetry: #'>=#1__' coercing: aNumber env: 1"__threadRubyEnvId"
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_st_rubyLt: aNumber

"called from smalltalk.
 Returns true if the receiver is less than aNumber; returns false otherwise."

<primitive: 20>
aNumber _isFloat ifTrue:[ ^ self asFloat < aNumber ].
^ self _rubyRetry: #'<#1__' coercing: aNumber env: 1"__threadRubyEnvId"
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_st_rubyLteq: aNumber
 
"called from smalltalk.
 Returns true if the receiver is less than or equal to aNumber; returns false
 otherwise."

<primitive: 21>
aNumber _isFloat ifTrue:[ ^ self asFloat <= aNumber ].
^ self _rubyRetry: #'<=#1__' coercing: aNumber env: 1"__threadRubyEnvId"
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_st_rubyModulo: aNumber

"called from smalltalk.
 Returns the modulus defined in terms of // ."
<primitive: 264>
(aNumber == 0) ifTrue:[ ^ self _errorDivideByZero].
^ self _rubyRetry: #'%#1__' coercing: aNumber env: 1"__threadRubyEnvId"
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_st_rubyRaisedTo: aNumber
  "called from smalltalk"
(aNumber _isInteger) ifFalse:[
  aNumber _isNumber ifFalse:[ ArgumentTypeError signal:'argument is not a Numeric'].
  ^ self @ruby1:__fraised_to: aNumber
].
aNumber < 0 ifTrue:[
  ^ 1.0 / (self _raisedToPositiveInteger: (0 - aNumber))
].
^ [
   self _raisedToPositiveInteger: aNumber
  ] onSynchronous: FloatingPointError do:[:ex |
    ex number == 2503 ifTrue:[  "ex is LargeInteger overflow error"
       ex return:( self asFloat raisedToInteger: aNumber)
    ] .
    ex outer
  ]
%


set class Integer
category: '*maglev-Ruby numeric'
method:
_st_rubyShiftLeft: anInteger

"called from smalltalk.
 If anInteger is positive, the shift is to the left"

<primitive: 742>
^ self @ruby1:__shift_left: anInteger
%

