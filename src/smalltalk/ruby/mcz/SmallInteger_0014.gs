
set class SmallInteger
category: '*maglev-runtime'
method:
codePointIsRubyWhitespace
  "receiver should be result of aString>>codePointAt: "
    self <= 32 ifTrue:[
      self == 32 ifTrue: [^true].    "space"
      self == 10 ifTrue: [^true].    "line feed"
      self == 13 ifTrue: [^true].    "cr"
      self == 9 ifTrue: [^true].    "tab"
      self == 11 ifTrue:[ ^ true ].   "VT"
      self == 12 ifTrue: [^true].    "form feed"
    ].
    ^false

%


set class SmallInteger
category: '*maglev'
method:
rubyLiteralNodeClass 
	^ RubyFixnumNode

%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_bitShiftRight: aNumber

"Returns the receiver shifted right by aNumber bits"

^ self bitShift: 0 - aNumber .
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_rubyBitAnd: aNumber

"A ruby primitive.
 Returns an Integer whose bits are the logical and of the receiver's bits and
 the bits of aNumber."

<primitive: 14>
"self __threadSaveCallerEnv ."
^ super _st_rubyBitAnd: aNumber
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_rubyBitOr: aNumber

"a ruby primitive.
 Returns an Integer whose bits are the logical or of the receiver's bits and
 the bits of aNumber."

<primitive: 15>
"self __threadSaveCallerEnv ."
^ super _st_rubyBitOr: aNumber
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_rubyBitXor: aNumber

"a ruby primitive..
 Returns an Integer whose bits are the logical xor of the receiver's bits and
 the bits of aNumber."

<primitive: 16>
"self __threadSaveCallerEnv ."
^ super _st_rubyBitXor: aNumber
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_rubyDivide: aNumber

"A ruby primitive. Divides the receiver by aNumber. "

<primitive: 12>
(aNumber == 0) ifTrue: [^ self _errorDivideByZero].
"self __threadSaveCallerEnv . "
^ super _st_rubyDivide:  aNumber
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_rubyEqual: aNumber

"A ruby primitive.
 Returns true if the receiver is equal to
 aNumber; returns false otherwise."

<primitive: 7>
"self __threadSaveCallerEnv ."
^ super _st_rubyEqual: aNumber
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_rubyGt: aNumber

"A ruby primitive.
 Returns true if the receiver is greater than aNumber;
 returns false otherwise."

<primitive: 4>
"self __threadSaveCallerEnv ."
^ super _st_rubyGt: aNumber
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_rubyGteq: aNumber

"A ruby primitive.
 Returns true if the receiver is greater than or equal to
 aNumber; returns false otherwise."

<primitive: 6>
"self __threadSaveCallerEnv ."
^ super _st_rubyGteq: aNumber
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_rubyLt: aNumber

"A ruby primitive.
 Returns true if the receiver is less than aNumber;
 returns false otherwise. "

<primitive: 3>
"self __threadSaveCallerEnv ."
^ super _st_rubyLt: aNumber
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_rubyLteq: aNumber

"A ruby primitive.
 Returns true if the receiver is less than or equal to
 aNumber; returns false otherwise."

<primitive: 5>
"self __threadSaveCallerEnv ."
^ super _st_rubyLteq: aNumber
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_rubyModulo: aNumber

"a ruby primitive. Returns the modulus defined in terms of // "
<primitive: 11>
"self __threadSaveCallerEnv ."
^ super _st_rubyModulo: aNumber
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_rubyRaisedTo: aNumber

"A ruby primitive.
 Returns the receiver raised to the power of the argument."

<primitive: 662>
"primitive handles SmallInteger and Float args. "

"self __threadSaveCallerEnv ."
^ super _st_rubyRaisedTo: aNumber
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_rubyShiftLeft: aNumber

" a ruby primitive"
<primitive: 17>
"self __threadSaveCallerEnv ."
^ super _st_rubyShiftLeft: aNumber
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_rubySize

"Returns number of bytes in machine representation of the receiver"
^ 8
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_rubyToSym

"Return the Symbol whose objectId is self, else return nil.
 DoubleByteSymbols  are not returned."
| obj |
obj := Object _objectForOop: self .
(obj _isSymbol and:[ obj _isOneByteString]) ifTrue:[ ^ obj ].
^ nil
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_ruby_eqlQ: aNumber

"Return true if aNumber is a SmallInteger with the same value as self."
aNumber _isSmallInteger ifTrue:[
   ^ self = aNumber
].
^ false
%


set class SmallInteger
category: '*maglev-Ruby numeric'
method:
_ruby_id2name

| o |
self <= 0 ifTrue:[ ^ nil ].
o := Object _objectForOop: self .
o _isSymbol ifTrue:[ 
  ^ String withAll: o . " note DoubleByteSymbol not supported"
].
^ nil
%

