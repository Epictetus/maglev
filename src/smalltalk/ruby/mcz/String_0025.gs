
set class String
category: '*maglev-Ruby support'
classmethod:
new
"optimization, reimplement to hide squeak style Object(C)>>new ,
  no need for initialize on byte objects"

<primitive: 51>

self _primitiveFailed: #new .
%


set class String
category: '*maglev-Ruby support'
classmethod:
_installRubyVariables
  "Install ruby class variables."
  | table sym |

  "This table is used to escape double quoted strings.  The table is
  indexed by the ASCII value of a character.  Any non-zero entries are
  output, preceeded by $\.  E.g., ASCII newlines (ASCII value 10) are
  printed as '\n', since $n is the value at index 10."

  sym := #RubyEscapeTranslationTable .
  (self _resolveClassVar: sym ) ifNil:[
    table := ByteArray new: 256 .
    table at:  7 + 1 put: $a asciiValue;
        at:  8 + 1 put: $b asciiValue;
        at:  9 + 1 put: $t asciiValue;
        at: 10 + 1 put: $n asciiValue;
        at: 11 + 1 put: $v asciiValue;
        at: 12 + 1 put: $f asciiValue;
        at: 13 + 1 put: $r asciiValue;
        at: 27 + 1 put: $e asciiValue;
        at: 34 + 1 put: $" asciiValue;
        at: 35 + 1 put: $# asciiValue;
        at: 92 + 1 put: $\ asciiValue .
    String _addInvariantClassVar: sym value: table .
  ].
%


set class String
category: '*core-squeak'
method:
asOctetString
  ^ self
%


set class String
category: '*maglev-runtime'
method:
asRegexpWithOptions: aNumber
	^ Regexp new: self options: aNumber

%


set class String
category: '*maglev-Ruby support'
method:
executeOnServer

  ^ System performOnServer: self
%


set class String
category: '*maglev-runtime'
method:
fill: length with: fillCharacters startingAt: start

"Copy characters from fillCharacters into self, startingAt start. Cycle through fillCharacters as necessary
to get to length characters.  '1234' fill: 2 with 'ABC' startingAt: 3 gives: '12AB'. 
'1234567890' fill: 5 with 'ABC' startingAt: 2 gives: '1ABCAB7890'.
Returns self."

"TODO: error checking"
| srcSize current leftToFill |
srcSize := fillCharacters size.
srcSize == 1 ifTrue:[ | ch idx |
  ch := fillCharacters at: 1 .
  start to: start + length - 1  do:[ :n |  self at: n put: ch ] .
] ifFalse:[
    current := start.
    1 to: (length // srcSize) do:[:i|
      self replaceFrom: current to: current + srcSize - 1 
		with: fillCharacters startingAt: 1 .
      current := current + srcSize
    ].
    leftToFill := length \\ srcSize.
    leftToFill >= 1 ifTrue:[
      self replaceFrom: current to: current + leftToFill - 1
	    with: fillCharacters startingAt: 1
    ].
].

%


set class String
category: '*maglev-runtime'
method:
isDigitsForInteger
  "Returns true if receiver is all digits 
   or has a leading minus sign and all digits"
  | idx |
  idx := 1 .
  ( self at: idx) == $- ifTrue:[ idx := idx + 1] .
  idx to: self size do:[:n |
	(self at: n) isDigit ifFalse:[ ^ false ].
  ].
  ^ true

%


set class String
category: '*core-squeak'
method:
isOctetString
  ^ true  
%


set class String
category: '*maglev-debugging'
method:
lineForOffset: aByteOffset
  "returns a one-based line number corresponding to the
   1-based byte offset."
  | line lf |
  line := 1 .
  lf := Character lf .
  1 to: self size do:[:j |
    j >= aByteOffset ifTrue:[ ^ line ].
    (self at: j) == lf ifTrue:[ line := line + 1 ].
  ].
  ^ line

%


set class String
category: '*maglev-runtime'
method:
md5sumDigest
  ^ self md5sum _asHexDigest

%


set class String
category: '*maglev-runtime'
method:
md5sumHexString
  ^ self md5sum asHexString

%


set class String
category: '*maglev-runtime'
method:
padded: direction to: length withString: padString

"Pad self with characters from padString to fill out to length.  Repeats padString as much as necessary."

| oldSize numPadChars  |

length <= (oldSize := self size) ifTrue:[ ^ self ].

"grow the receiver"
numPadChars := length - oldSize.
self size: length.
padString size == 0 ifTrue:[ self error:'pad string empty']. "TODO correct Ruby exception"
direction == #left ifTrue:[
  self fill: numPadChars with: padString startingAt: (oldSize + 1).
  ^self 
].
direction == #right ifTrue:[
  oldSize ~~ 0 ifTrue:[ 
    self replaceFrom: numPadChars + 1 to: numPadChars + oldSize with: self startingAt: 1
  ].
  self fill: numPadChars with: padString startingAt: 1.
  ^ self 
].
direction == #center ifTrue:[|half lSize rSize|
  half := numPadChars / 2 .
  lSize := half floor.
  rSize := half ceiling.
  oldSize ~~ 0 ifTrue:[ 
    self replaceFrom: lSize + 1 to: lSize + oldSize with: self startingAt: 1 .
  ].
  self fill: lSize with: padString startingAt: 1.
  self fill: rSize with: padString startingAt: (lSize + oldSize + 1).
  ^ self
].
ArgumentError signal:'unknown direction'.

%


set class String
category: '*maglev-runtime'
method:
prefixIfRubySelector
      "ruby_selector_suffix dependent, and send sites"
  (self _rubyAt1: -4) == 35"$#" ifTrue:[ ^ self rubySelectorPrefixSymbol ].
  ^ self

%


set class String
category: '*maglev-runtime'
method:
rubyCapitalize

"Return a copy of self with all but the first lower case"
|result|
self size == 0 ifTrue:[ ^ self species new ].
result := self asLowercase.
result at: 1 put: ((self at: 1) asUppercase).
^ result.

%


set class String
category: '*maglev-Ruby support'
method:
rubyConcatenate: aString
  <primitive: 819>
  "envId := self __threadSaveCallerEnv ."
  aString _isRubyString ifFalse:[ | str |
    [ 
      str := aString @ruby1:to_str 
    ] onSynchronous: AbstractException do:[ :ex| "swallow ex" ] .
      str _isRubyString ifFalse:[
        ArgumentTypeError signal:'String#+ , cannot convert arg to String with to_str'. 
      ].
    ^ self rubyConcatenate: str 
  ].
  self _primitiveFailed: #rubyConcatenate: args: { aString }
%


set class String
category: '*maglev-runtime'
method:
rubyCount: templates
  "A ruby primitive.
   Return a count of characters in the receiver as specified by templates."
  |characters count| 
  characters := BitSet forTemplates: templates env: 1"__callerEnvId" .
  count := 0.
  1 to: self size do:[ :i |
    ( characters at: (self codePointAt: i)) == 1  ifTrue:[ count := count +  1 ]
  ].
  ^ count.

%


set class String
category: '*maglev-runtime'
method:
rubyDelete: templates
  "a ruby primitive.
   Return a copy of the receiver, minus those characters specified by templates"
  | characters result resIdx  |
  characters := BitSet forTemplates: templates "bits == 1 are chars to delete"
                        env: 1"__callerEnvId" .
  result := self species new .
  resIdx := 1 .
  1 to: self size do:[ :i | | ch |
    ch := self codePointAt: i .
    (characters at:  ch  ) == 0 ifTrue:[ 
      result codePointAt: resIdx put: ch . 
      resIdx := resIdx + 1 
    ]
  ].
  ^ result.

%


set class String
category: '*maglev-runtime'
method:
rubyDeleteInPlace: templates
  "A ruby primitive.
   Delete characters specified by templates from receiver."
  | characters destIdx recSize |
  characters := BitSet forTemplates: templates "bits == 1 are chars to delete"
                        env: 1"__callerEnvId" .
  destIdx := 0 .
  1 to: (recSize := self size) do:[ :n | 
    (characters at:  (self codePointAt: n )) == 1 ifTrue:[ " found first deletion point"
      destIdx := n - 1 .
       n + 1 to: recSize do:[ :k | | ch |
          ch := self codePointAt: k . 
          (characters at: ch ) == 0 ifTrue:[
              destIdx := destIdx + 1 .
              self codePointAt: destIdx put: ch 
          ].
       ].
       self size: destIdx . 
       ^ self 
    ]
  ].
  ^ nil  "no deletions found"

%


set class String
category: '*maglev-runtime'
method:
rubyDowncaseInPlace
	"Ensure all characters in self are lowercase. Return nil if no change, otherwise return self."
	| changed |
	changed := false .
	1 to: self size do:[ :i | | ch |
		ch := self at: i .
		ch isUppercase ifTrue:[ 
			changed := true .
			self at: i put: ch asLowercase ]].
	changed ifTrue:[ ^ self ] ifFalse:[ ^ nil ].

%


set class String
category: '*maglev-runtime'
method:
rubyDumpInto: result
| tbl lastSpecialVal vArr |
tbl := #( '\000' '\001' '\002' '\003' '\004' '\005' '\006' '\a'   "000 .. 007"
          '\b'   '\t'    '\n'   '\v'   '\f'    '\r'   '\016' '\017' "010 .. 017"
          '\020' '\021' '\022' '\023' '\024' '\025' '\026' '\027' "020 .. 027"
          '\030' '\031' '\032' '\e'   '\034' '\035' '\036' '\037'  "030 .. 037"
          ' '     '!'    '\"'    '\#').                              "040 .. 043"
vArr := { nil }.
"The dump output must be wrapped in double quotes"
result addLast: $" .
lastSpecialVal := tbl size - 1.
1 to: self size do:[ :n | | ch v  |
  ch := self at: n .
  v := ch asciiValue .
  v <= lastSpecialVal ifTrue:[
    result addAll: (tbl at: (v + 1))
  ] ifFalse:[
    v >= 127 ifTrue:[
	  vArr at: 1 put: v .
      result addLast: (Module sprintf: '\%o' with: vArr ) 
    ] ifFalse:[
      result addLast: ch .
    ].
  ].
].
result addLast: $".
^ result

%


set class String
category: '*maglev-runtime'
method:
rubyFindLastAlphaNumeric
 "return the index of the last alpha numeric character in receiver.
  return nil if none."
|idx|
idx := self size.
[idx >=1 ] whileTrue: [
	(self at: idx) rubyIsAlphaNumeric ifTrue: [ ^ idx ] .
	idx := idx - 1 .
].
^ nil .

%


set class String
category: '*maglev-runtime'
method:
rubyLiteralClass
	(self at: 1) == $/
		ifTrue: [ ^ RubyRegexpNode ].
		
	^ (self includesSubString: '..' )
		ifTrue:[ RubyDotNode ]
		ifFalse:[ RubyAbstractNumberNode ]

%


set class String
category: '*maglev-Ruby support'
method:
rubySelectorPrefix
  "return the selector prefix of the receiver.

  ruby selector format is  prefix#N*&
    # is always  $#
    N is one character $0 .. $z
    number of fixed args is N - $0
    * is either $*  or $_
    & is either $&  or $_
  "
  		"ruby_selector_suffix dependent"
  <primitive: 794>   "primitive fails if receiver is a large object"
  | sz |
  (sz := self size) > 1024 ifTrue:[ Error signal:'max Symbol size is 1024' ].
  sz < 4 ifTrue:[ Error signal:'missing ruby selector suffix' ].
  (self _rubyAt1: -4) == 35"$#" ifFalse:[ Error signal:'invalid ruby selector suffix'].

  self _primitiveFailed: #rubySelectorPrefix args: #() .
%


set class String
category: '*maglev-Ruby support'
method:
rubySelectorPrefixSymbol
  " Return the selector prefix of the receiver as a Symbol

  ruby selector format is  prefix#N*&
    # is always  $#
    N is one character $0 .. $z
    number of fixed args is N - $0
    * is either $*  or $_
    & is either $&  or $_
  "
  		"ruby_selector_suffix dependent"
  <primitive: 812>
  | sz |
  (sz := self size) > 1024 ifTrue:[ Error signal:'max Symbol size is 1024' ].
  sz < 4 ifTrue:[ Error signal:'missing ruby selector suffix' ].
  (self _rubyAt1: -4) == 35"$#"  ifFalse:[ Error signal:'invalid ruby selector suffix'].
  self _primitiveFailed: #rubySelectorPrefixSymbol args: #() .
%


set class String
category: '*maglev-runtime'
method:
rubySelectorSuffix
    "Return a String containing the suffix of self"
      "ruby_selector_suffix dependent"
  | res |
  res := self _rubyAt1: -4 length: 4 .
  res ifNotNil:[
    (res at: 1) == $# ifTrue:[ ^ res ].
  ].
  Error signal:'missing ruby selector suffix'

%


set class String
category: '*maglev-runtime'
method:
rubySqueeze
 "return a copy of self with each repeating character sequence 
  reduced to a single character"
| n dest prev ch sz res |
sz := self size .
sz > 1 ifTrue:[
  res := self species new: sz // 2 .
  prev := -1 .
  n := 1 .
  dest := 0 .
  [ n <= sz and:[(ch := self at: n) ~~ prev]] whileTrue:[
    dest := dest + 1 .
    res at: dest put: ch .
    n := n + 1 .
    prev := ch
  ].
  [ n <= sz ] whileTrue:[ 
    (ch := self at: n) == prev ifTrue:[
      n := n + 1 .
      [ n <= sz and:[ (ch := self at: n) == prev ]] whileTrue:[
	n := n + 1
      ].
      n <= sz ifTrue:[
        dest := dest + 1 .
        res at: dest put: ch .
      ] 
    ] ifFalse:[
      dest := dest + 1 .
      res at: dest put: ch .
    ].
    n := n + 1 .
    prev := ch .
  ].
  dest < sz ifTrue:[ res size: dest ].
] ifFalse:[
  res := self copy .
].
^ res

%


set class String
category: '*maglev-runtime'
method:
rubySqueeze: templates
 "A Ruby primitive.
  return a copy of receiver , converting each repeating character sequence per templates
  to a single character."
| res n dest prev ch sz charset |
charset := BitSet forTemplates: templates env: 1"__callerEnvId" .
sz := self size .
sz > 1 ifTrue:[
  res := self species new: sz // 2 .
  prev := -1 .
  n := 1 .
  dest := 0 .
  [ n <= sz and:[(ch := self codePointAt: n) ~~ prev]] whileTrue:[
    dest := dest + 1 .
    res codePointAt: dest put: ch .
    n := n + 1 .
    prev := ch
  ].
  [ n <= sz ] whileTrue:[ 
    ((ch := self codePointAt: n) == prev and:[ (charset at: prev) == 1]) ifTrue:[
      n := n + 1 .
      [ n <= sz and:[ (ch := self codePointAt: n) == prev ]] whileTrue:[
    n := n + 1
      ].
      n <= sz ifTrue:[
        dest := dest + 1 .
        res codePointAt: dest put: ch .
      ] 
    ] ifFalse:[
      dest := dest + 1 .
      res codePointAt: dest put: ch .
    ].
    n := n + 1 .
    prev := ch .
  ].
  dest < sz ifTrue:[ res size: dest ].
] ifFalse:[
  res := self copy .
].
^ res

%


set class String
category: '*maglev-runtime'
method:
rubySqueezeSelf
 "convert each repeating character sequence in receiver to a single character"
| n dest prev ch sz |
sz := self size .
sz > 1 ifTrue:[
  prev := -1 .
  n := 1 .
  [ n <= sz and:[(ch := self at: n) ~~ prev]] whileTrue:[
    n := n + 1 .
    prev := ch
  ].
  dest := n - 1 .
  [ n <= sz ] whileTrue:[ 
    (ch := self at: n) == prev ifTrue:[
      n := n + 1 .
      [ n <= sz and:[ (ch := self at: n) == prev ]] whileTrue:[
	n := n + 1
      ].
      n <= sz ifTrue:[
        dest := dest + 1 .
        self at: dest put: ch .
      ] 
    ] ifFalse:[
      dest := dest + 1 .
      self at: dest put: ch .
    ].
    n := n + 1 .
    prev := ch .
  ].
  dest < sz ifTrue:[ self size: dest .   ^ self ]
           ifFalse:[ ^ nil "no changes made" ]
] ifFalse:[
  ^ nil "no changes made"
].

%


set class String
category: '*maglev-runtime'
method:
rubySqueezeSelf: templates
 "A ruby primitive.
  Convert each repeating character sequence in receiver per templates
  to a single character"
| n dest prev ch sz charset |
charset := BitSet forTemplates: templates env: 1"__callerEnvId" .
sz := self size .
sz > 1 ifTrue:[
  prev := -1 .
  n := 1 .
  [ n <= sz and:[(ch := self codePointAt: n) ~~ prev]] whileTrue:[
    n := n + 1 .
    prev := ch
  ].
  dest := n - 1 .
  [ n <= sz ] whileTrue:[ 
    ((ch := self codePointAt: n) == prev and:[ (charset at: prev) == 1]) ifTrue:[
      n := n + 1 .
      [ n <= sz and:[ (ch := self codePointAt: n) == prev ]] whileTrue:[
        n := n + 1
      ].
      n <= sz ifTrue:[
        dest := dest + 1 .
        self codePointAt: dest put: ch .
      ] 
    ] ifFalse:[
      dest := dest + 1 .
      self codePointAt: dest put: ch .
    ].
    n := n + 1 .
    prev := ch .
  ].
  dest < sz ifTrue:[ self size: dest .  ^ self ]
           ifFalse:[ ^ nil " no changes made "]
] ifFalse:[
  ^ nil " no changes made"
]

%


set class String
category: '*maglev-runtime'
method:
rubySucc
"Implements  Ruby   succ!  for String "
| idx res carry sz lastAlphaNumericIdx |

sz := self size .
sz == 0 ifTrue: [^ self ] .

lastAlphaNumericIdx := self rubyFindLastAlphaNumeric .
lastAlphaNumericIdx == nil 
	ifTrue: [ 
		"String has no alpha numerics, so increment everything"
		idx := sz . 
		[ idx >= 1 ] whileTrue:[
			carry := self _rubyIncrementCharAt: idx .
  			carry == nil ifTrue:[ ^ self ] .
  			idx := idx - 1 .
		].
	] ifFalse: [|firstAlphaNumericIdx|
		"String has alphanumerics, so increment starting at rightmost alpha numeric"
		firstAlphaNumericIdx := lastAlphaNumericIdx .
		idx := lastAlphaNumericIdx .
		[ idx >= 1 ] whileTrue:[
		     (self at: idx) rubyIsAlphaNumeric ifTrue: [
			  firstAlphaNumericIdx := idx .
			  carry := self _rubyIncrementCharAt: idx .
  			  carry == nil ifTrue:[ ^ self ] 
			] .
  			idx := idx - 1
		].
		carry ~~ nil ifTrue:[ self insertAll: carry at: firstAlphaNumericIdx ] .
		^ self .
	] .
carry ~~ nil ifTrue: [ self insert: carry at: 1 ] .
^ self

%


set class String
category: '*maglev-runtime'
method:
rubySwapcaseInPlace

"Return a copy of self with all characters swap cased.  Return nil if receiver unchanged.
  Only works on ascii range a-z and A-Z.  Locale ignorant."
|modified|
modified := false .
1 to: self size do: [ :i ||ch ascii|
	ch := self at: i .
	ascii := ch asciiValue .
	(ascii >= 65 and:[ ascii <= 90] ) ifTrue: [
		modified := true .
		self at: i put: ch asLowercase 
	].
	(ascii >= 97 and:[ ascii <= 122] ) ifTrue: [
		modified := true .
		self at: i put: ch asUppercase
	] .
] .
modified ifTrue:[ ^ self ] ifFalse: [^ nil ] .

%


set class String
category: '*maglev-runtime'
method:
rubyTrFrom: from to: to

"Translate characters in self.  from is a set of characters to translate (possibly negated). to
is a set of corresponding characters to translate to.  The first character in from is changed
to the first character in to, and so on.  If there are more characters in from than to, extend to
with its last character.  Modifies self and returns self, unless no changes made, and then returns nil."

| map idx changed selfsiz |

( (selfsiz := self size) == 0 or:[ from size == 0]) ifTrue:[ ^ nil].

changed := false .
map := from _trMapping: to.

"A nil in the map means to remove that character from the string."
idx := 0 .
1 to: selfsiz do:[:i| | current replacement |
	current := ((self codePointAt: i) + 1) . "Characters are zero based"
	replacement := map at: current .
	replacement ifNotNil:[ 
		idx := idx + 1. 
		((self at: idx) == replacement) ifFalse: [ changed := true ] .
		self at: idx put: replacement.
	] .
].

idx ~~ (selfsiz ) ifTrue: [ self size: idx ] .
changed ifTrue: [ ^ self ] ifFalse: [ ^ nil ] .

%


set class String
category: '*maglev-runtime'
method:
rubyTrSqueezeFrom: from to: to

"Like rubyTrFrom:to:, but removes duplicate characters from modified sections of self.
Returns self, or nil if self not modified."

|map modSet modified writeIndex prevModChar|
(self size = 0 or:[ from size = 0]) ifTrue:[ ^ self ].

map := from _trMapping: to .
modified := false .
modSet := BitSet forTemplateString: from .
prevModChar := nil .
writeIndex := 1 .
1 to: self size do:[:i| | currentByte |
	currentByte := self codePointAt: i .
	(modSet at: currentByte) = 1 
		ifTrue:[ | newChar |
			modified := true .
			newChar := map at: (currentByte + 1) .
			(newChar ~~ prevModChar) 
				 ifTrue:[
					"This is not a repeated character, so modify"
					self at: writeIndex put: newChar .
					writeIndex := writeIndex + 1 .
					prevModChar := newChar]]
		ifFalse:[
			prevModChar := nil .
			writeIndex ~~ i ifTrue:[ self at: writeIndex put: (self at: i) ] .
			writeIndex := writeIndex + 1 ]].
modified ifTrue:[ self size: (writeIndex - 1) . ^ self ] ifFalse:[ ^ nil ].

%


set class String
category: '*maglev-Ruby support'
method:
rubyUnpack: aString
 "A ruby primitive.
  Per Pickaxe p 624 , table 27.14
  TODO: type in all the unpack documentation here."

<primitive: 722>
aString _isOneByteString ifFalse:[
  aString _error: #rtErrBadArgKind args:{ String }
].
self _primitiveFailed: #rubyUnpack: args: { aString }
%


set class String
category: '*maglev-runtime'
method:
rubyUpcaseInPlace
	"Ensure all characters in self are uppercase. Return nil if no change, otherwise return self."
	| changed |
	changed := false .
	1 to: self size do:[ :i | | ch |
		ch := self at: i .
		ch isLowercase ifTrue:[ 
			changed := true .
			self at: i put: ch asUppercase ]].
	changed ifTrue:[ ^ self ] ifFalse:[ ^ nil ].

%


set class String
category: '*maglev-runtime'
method:
rubyUpperCaseAt: anIndex
  "anIndex is one-based. "
  
  ^ (self at: anIndex) asUppercase asciiValue


%


set class String
category: '*maglev-runtime'
method:
sequenceExpand

^ self sequenceExpand: false.

%


set class String
category: '*maglev-runtime'
method:
sequenceExpand: skipFirst

"Return a copy of self with all sequences expanded.  A sequence is two characters separated by $-, e.g., a-z.
If skipFirst is true, then skip the first character."

| expanded index limit rcvrSize chCls  |

expanded := String new.
(rcvrSize := self size) == 0 ifTrue:[ ^ expanded ].

index :=  skipFirst ifTrue:[ 2 ] ifFalse:[ 1].
limit := rcvrSize - 2 .
chCls := Character .
[ index <= limit  ] whileTrue:[
	(self at: index + 1) == $- 
	  ifTrue:[ "expand character sequence: e.g., a-z"
		(self codePointAt: index) to: (self codePointAt: (index + 2))
		     do:[:i| expanded add: (chCls withValue: i)].
		index := index + 2 ]
	ifFalse:[ expanded add: (self at: index) ].
	index := index + 1 
] .
[ index <= rcvrSize ] whileTrue:[
	expanded add: (self at: index) .
	index := index + 1 
] .
^ expanded.

%


set class String
category: '*maglev-runtime'
method:
suffixIfRubySelector
      "ruby_selector_suffix dependent, and send sites"
  (self _rubyAt1: -4) == 35"$#" ifTrue:[ ^ self rubySelectorSuffix ].
  ^ nil

%


set class String
category: '*maglev-runtime'
method:
tailForPrint: aSize
  self size > aSize ifTrue:[ ^ '...', (self last: aSize) ] 
	              ifFalse:[ ^ self ].

%


set class String
category: '*maglev-runtime'
method:
terminatedWith: aCharacter
  | sz |
  (sz := self size) == 0 ifTrue:[ ^ '/' copy ].
  (self at: sz) == aCharacter ifTrue:[ ^ self ]
                      ifFalse:[ ^ self , aCharacter ]

%


set class String
category: '*maglev-Ruby support'
method:
_asSymbolWithRubySuffix: argInt

"Return a Symbol consisting of self concatenated with specified
  ruby suffix .

 argInt contains bits described by these masks:
     16rFFC - number of fixed args 0..74
     16r2 - append a $*
     16r1 - append a $&
"
  	"send sites are ruby_selector_suffix dependent"
<primitive: 809>   "primitive fails if receiver is large object or DoubleByteSymbol"
  | sz |
  (sz := self size) > 1020 ifTrue:[ Error signal:'max Symbol size is 1024' ].
  (argInt // 4) > 74 ifTrue:[ Error signal:'max of 74 fixed args exceeded' ].

  self _primitiveFailed: #_asSymbolWithRubySuffix: args: { argInt } .
%


set class String
category: '*maglev-Ruby support'
method:
_at: anIndex equals: aString

 "A Ruby primitive.
  Returns true if aCharCollection is contained in the receiver, starting at
  anIndex.  Returns false otherwise.  The comparison is case-sensitive.

 Allows anIndex == one past end of receiver and returns false . "
   
 <primitive: 778>
 | sz |
 anIndex _isSmallInteger ifFalse:[ anIndex _validateClass: SmallInteger].
 anIndex == ((sz := self size) + 1) ifTrue:[ ^ false ].
 ((anIndex <= 0) or: [(anIndex > sz)]) ifTrue: [ 
     ^ self _errorIndexOutOfRange: anIndex 
 ].
 self _primitiveFailed: #_at:equals: args: { anIndex . aString }
%


set class String
category: '*maglev-runtime'
method:
_inspect
  ^ '"' , self, $"

%


set class String
category: '*maglev-Ruby support'
method:
_rubyAddAll: anArg

  "A Ruby primitive.
   If anArg is a String, appends anArg to the receiver.
   else if anArg is a SmallInteger  >= 0 and <= 255,
   appends   Character withValue:anArg    to the receiver.
   Returns the receiver."

<primitive: 689>
anArg _isSmallInteger ifTrue:[
  ArgumentTypeError signal:'<<: ' , anArg asString , ' out of range' .
].
(anArg _isOneByteString) ifFalse:[
  anArg _error: #rtErrBadArgKind args:{ SmallInteger . String }.
].
self _primitiveFailed:#_rubyAddAll: args: { anArg }
%


set class String
category: '*maglev-Ruby support'
method:
_rubyAt1: anOffset
  "A ruby primitive.
   Ruby  aString[anIntOrRangeOrRegexp] for env 1

   If argument is an SmallInteger,
     anOffset is zero based.
     negative offsets go backwards from end,  -1 means last element.
     returns nil if anOffset is out of range,
     else returns character code at specified position.
  "
<primitive: 686>
anOffset _isOneByteString  ifTrue:[ "a String"  | ofs |
  ofs := self _findString: anOffset startingAt: 1 ignoreCase: false .
  ofs ~~ 0 ifTrue:[ ^ anOffset ].
  ^ nil.
].
anOffset _isRegexp ifTrue:[ "a Regexp" | aMatchData |
  aMatchData := anOffset match: self .
  aMatchData _storeRubyVcGlobal: 16r20 . "store into caller's $~ "
  aMatchData ~~ nil ifTrue:[  ^ aMatchData _rubyAt:0 ].
  ^ nil .
].
^ self @ruby1:__prim_at_failed: anOffset
%


set class String
category: '*maglev-Ruby support'
method:
_rubyAt1: anOffset length: aCount

 "A ruby primitive.
  Returns an instance of receiver's class
   containing specified substring of the receiver,
   or returns nil if anOffset is out of range.
  Negative offsets go backwards from end,  -1 means last element.
  For env 1.
"
<primitive: 687>
^ self @ruby1:__prim_at_length_failed: anOffset _: aCount
%


set class String
category: '*maglev-Ruby support'
method:
_rubyAt1: offsetArg length: aCount put: aString

  "If offsetArg is an SmallInteger,  anOffset is zero based , and
   replace elements anOffset .. aCount-1 of receiver with contents of aString .

   If offsetArg is a Regexp , replace the portion of the receiver
   specified by  (offsetArg.match(self))[aCount] with aString ,
   where aCount is zero based.

   returns aString."

  | anOffset valSize start end rcvrSize newSize matchSize cnt |
  aCount _isSmallInteger ifFalse:[
    ^ self @ruby1:__prim_at_length_put_failed: offsetArg _: aCount _: aString
  ].
  aString _isOneByteString ifFalse:[
    ^ self @ruby1:__prim_at_length_put_failed: offsetArg _: aCount _: aString
  ].
  anOffset := offsetArg .
  offsetArg _isSmallInteger ifTrue:[
    rcvrSize := self size .
    start := anOffset .  "zero based, first elem to replace"
    start < 0 ifTrue:[ 
      start := start + rcvrSize .
      start < 0 ifTrue:[ ^ OffsetError signal: 'String#[index,count]=str, index too small' ].
    ].
    start > rcvrSize ifTrue:[ ^ OffsetError signal: 'String#[index,count]=str, index too large'].
    aCount < 0 ifTrue:[ OffsetError signal: 'String#[index,count]= , count < 0' ].
    end := start + aCount - 1 . "end is zero based, last elem to replace"
    end >= rcvrSize ifTrue:[ end := rcvrSize - 1 ].
    matchSize := end - start + 1 .
  ] ifFalse:[ "expect a Regexp "  | aMatchData mOfs |
    anOffset _isRegexp ifFalse:[
      ^ self @ruby1:__prim_at_length_put_failed: offsetArg _: aCount _: aString
    ].
    aMatchData := anOffset"aRegexp" match: self  . "anOffset is a Regexp"
    aMatchData == nil ifTrue:[
       ^ OffsetError signal:'argument regex not found'.
    ].
    aCount < 0 ifTrue:[  
      cnt := (aMatchData size // 2) "nMatches" + aCount .
      cnt <= 0 ifTrue:[ ^ OffsetError signal:'String#[regexp,count] , count out of range'].
      mOfs := (cnt * 2) + 1 .
    ] ifFalse:[
      mOfs := (aCount * 2) + 1 . "mOfs is one based"
      mOfs > aMatchData size ifTrue:[ 
         ^ OffsetError signal:'String#[regexp,count] , count out of range'
      ]
    ].
    start := (aMatchData at: mOfs)  . "(zero based matchStart)  "
    end := aMatchData at: mOfs + 1 .     "zero based matchLimit == zero-based end"
    matchSize := end - start .
    rcvrSize := self size .
  ].
  start := start + 1 . "convert to one based"
  end := end + 1 .
  valSize := aString size .
  newSize := rcvrSize + valSize - matchSize .
  newSize < 0 ifTrue: [
    self size: 0 .
    ^ aString .
  ] .
  valSize < matchSize ifTrue:[ "shrinking receiver"
    end < rcvrSize ifTrue: [  
      self replaceFrom:  (cnt := start + valSize)"destIdx" 
           to: cnt + rcvrSize - (end + 1) with: self startingAt: end + 1
    ] .
    self size: newSize .
  ] ifFalse:[ "growing receiver" 
      self size: newSize .
      cnt "numToSave" := rcvrSize - end .
      cnt > 0 ifTrue:[
        self replaceFrom: (cnt := newSize - cnt + 1) 
             to: cnt + rcvrSize - (end + 1) with: self startingAt: end + 1 .
      ].
  ].
  valSize ~~ 0 ifTrue: [
    self replaceFrom: start to: start + valSize - 1 with: aString startingAt: 1 
  ] .
  ^ aString
%


set class String
category: '*maglev-Ruby support'
method:
_rubyAt1: anOffset put: aValue
  "A ruby primitive.
   Ruby  [int]=  for env 1

   If anOffset is an SmallInteger and aValue is a SmallInteger ,
     (aValue bitAnd:255) replaces specified character of receiver, without auto-grow.
   If anOffset is an SmallInteger and aValue is a String, deletes character
   at anOffset, and then inserts aValue at anOffset .
   If both are Strings, replaces first occurrance of anOffset in receiver with aValue.

   Returns aValue
  "
  <primitive: 690>
  anOffset _isOneByteString ifTrue:[  "anOffset is a String"  | argString ofs argSize |
    argString := anOffset .
    ofs := self _findString: argString startingAt: 1 ignoreCase: false .
    ofs == 0 ifTrue:[
       ^ OffsetError signal:'argument string not found'.
    ].
    ^ self _rubyAt1: ofs - 1 length: argString size put: aValue
  ].
  anOffset _isRegexp ifTrue:[ "anOffset is a Regexp" |aMatchData mOfs mStart mLimit|
    aMatchData := anOffset match: self  .
    aMatchData == nil ifTrue:[
        ^ OffsetError signal:'argument regex not found'.
    ].
    mStart := aMatchData at: 1 . "mStart is zero based "
    mLimit := aMatchData at: 2 .
    ^ self _rubyAt1: mStart length: mLimit - mStart  put: aValue
  ].
  ^ self @ruby1:__prim_at_put_failed: anOffset _: aValue
%


set class String
category: '*maglev-Ruby support'
method:
_rubyAt: anOffset length: aCount

 "A ruby primitive.
  Returns an instance of receiver's class
   containing specified substring of the receiver,
   or returns nil if anOffset is out of range.
  Negative offsets go backwards from end,  -1 means last element.
  Used by Smalltalk code in MatchData .
"
<primitive: 687>
^ self _primitiveFailed: #_rubyAt:length: args: { anOffset . aCount }
%


set class String
category: '*maglev-Ruby support'
method:
_rubyCompare1: aString
  "A Ruby primitive"
  <primitive: 844>
  ^ self @ruby1:__prim_compare_failed: aString
%


set class String
category: '*maglev-Ruby support'
method:
_rubyEqual1: aString
  "A Ruby primitive"
  <primitive: 843>
  ^ self @ruby1:__prim_equal_failed: aString
%


set class String
category: '*maglev-runtime'
method:
_rubyIncrementCharAt: idx
" returns nil if no carry, or a Character that represents
  the character to insert at start of result if idx==1 , per Ruby succ  method"
|  v |
v := self codePointAt: idx .
"$A is ascii 65  $Z is ascii 90"
(v >= 65 and:[ v <= 89])  ifTrue: [ self codePointAt: idx put: ( v + 1 ). ^ nil ] .
v == 90 ifTrue: [ self at: idx put: $A.  ^ $A] .

"$a is ascii 97  $z is ascii 122"
(v >= 97 and:[ v <= 121] ) ifTrue: [ self codePointAt: idx put: ( v + 1). ^ nil ] .
v == 122 ifTrue: [ self at: idx put: $a . ^$a ] .

"$0 is ascii 48  $9 is ascii 57"
(v >= 48 and:[ v <= 56] ) ifTrue: [ self codePointAt: idx put: (v + 1). ^ nil ] .
v == 57 ifTrue: [ self at: idx put: $0 . ^$1 ] .

v <= 254 ifTrue:[ self codePointAt: idx put: ( v + 1). ^ nil ]
       ifFalse:[  self codePointAt: idx put:  0 .
	            ^ Character withValue: 1  ] 

%


set class String
category: '*maglev-Ruby support'
method:
_rubyInspect

"A ruby primitive.
  Returns a String whose contents are a displayable representation of the
 receiver."

"This reimplementation is for efficiency and to make the String | describe
 method more robust."

self size > 1000000 ifTrue:[
 ^ self
] ifFalse: [ | str |
  str := String new .
  str add: $" .
  self _rubyQuoteOn: str .
  str add: $" .
  ^ str
]
%


set class String
category: '*maglev-runtime'
method:
_rubyLstrip

"Returns a new String containing the same Characters as the receiver,
 but with leading whitespace separators removed."

| sz |
(sz := self size) == 0 ifTrue:[ ^ self copy ].
(self codePointAt: 1) codePointIsRubyWhitespace ifFalse: [ ^ self copy ].
2 to: sz do:[:j |
  (self codePointAt: j) codePointIsRubyWhitespace ifFalse:[ 
    ^ self copyFrom: j to: sz
  ].
].
^ self class new

%


set class String
category: '*maglev-runtime'
method:
_rubyLstripInPlace

"delete leading whitespace separators from receiver"
| sz |
(sz := self size) == 0 ifTrue:[ ^ nil ].
(self codePointAt: 1) codePointIsRubyWhitespace ifFalse: [ ^ nil ].
2 to: sz do:[:j |
  (self codePointAt: j) codePointIsRubyWhitespace ifFalse:[
     self removeFrom: 1 to: j - 1 .
     ^ self.
  ].
].
self size: 0 . 
^ nil

%


set class String
category: '*maglev-Ruby support'
method:
_rubyQuoteOn: aString

"Puts a displayable representation of receiver on aStream escaping self
according to ruby double quote semantics (e.g., tab is replaced with '\n'
and $\ is escaped, etc). The characters that are translated:

  \a  ASCII 7   Bell
  \b  ASCII 8   Backspace
  \t  ASCII 9   Tab
  \n ASCII 10  New Line
  \v ASCII 11  Vertical Tab
  \f  ASCII 12  Form Feed
  \r ASCII 13  Carriage Return
  \e  ASCII 27 ESC

Characters that are quoted: Backslash $\ ASCII 92 and double quote ASCII 34"
 | charCls vArr |
  charCls := Character .
  vArr := { nil }.
  1 to: self size do: [:n | | c xlated av chDone |
    c := self at: n .
    c == $# ifTrue:[ | nextByte |
      nextByte := self _rubyAt1: n  .  "atOrNil: n + 1"
      (nextByte == 36 or:[ nextByte == 64 or:[ nextByte == 123]]) ifFalse:[
         "next char not one of  $  @  {  ,  do not escape " 
        aString add: c .
        chDone  := true .
      ] .
    ] .
    chDone ifNil:[
      av := c asciiValue .
      xlated := RubyEscapeTranslationTable at: (av + 1) .
      (xlated == 0) ifTrue: [
	(av between: 32 and: 126) ifTrue: [
	  aString add: c
	] ifFalse: [
	  vArr at: 1 put: av .
	  aString addAll: (Module sprintf:'\%03o' with: vArr)
	] .
      ] ifFalse: [
	aString add: $\ ; add: ( charCls withValue: xlated) .
      ] .
    ] .
  ] .
%


set class String
category: '*maglev-Ruby support'
method:
_rubyReplace: aString

  "Replace contents of receiver with contents of argument.
   Returns the receiver."

  aString ~~ self ifTrue:[ | argSize arg |
    (arg:= aString) _isOneByteString ifFalse:[ 
      "self __threadSaveCallerEnv ."
      arg := [:a | a @ruby1:to_str ] value: aString
          onSynchronous: Exception do: [:ex| nil ].
      arg _isOneByteString ifFalse:[
         ArgumentTypeError signal: 'in _rubyReplace:, to_str did not return a String'
      ].
    ].
    self size: (argSize := arg size) .
    argSize ~~ 0 ifTrue:[
      self replaceFrom: 1 to: argSize with: arg startingAt: 1 .
    ].
  ].
  ^ self
%


set class String
category: '*maglev-runtime'
method:
_rubyRstrip

"Returns a new String containing the same Characters as the receiver,
 but with trailing whitespace or NUL removed."

| sz cp |

(sz := self size) == 0 ifTrue: [ ^ self copy ].
((cp := self codePointAt: sz ) == 0 or:[ cp codePointIsRubyWhitespace ]) ifFalse:[
  ^ self copy 
].
sz - 1 _downTo: 1 do:[ :j |
  (self codePointAt: j) codePointIsRubyWhitespace ifFalse:[ "for 1.9 add   or:[ cp == 0 ]"
    ^ self copyFrom: 1 to: j 
  ].
].
^ self class new

%


set class String
category: '*maglev-runtime'
method:
_rubyRstripInPlace
 "delete trailing whitespace or NUL from receiver."
| sz cp |
(sz := self size) == 0 ifTrue: [ ^ nil ].
((cp := self codePointAt: sz ) == 0 or:[ cp codePointIsRubyWhitespace ]) ifFalse:[
  ^ nil 
].
sz - 1 _downTo: 1 do:[ :j |
  (self codePointAt: j) codePointIsRubyWhitespace ifFalse:[ "for 1.9 add   or:[ cp == 0 ]"
     self removeFrom: j + 1 to: sz . 
     ^ self 
  ].
].
self size: 0 .
^ self

%


set class String
category: '*maglev-runtime'
method:
_rubyStrip
| first limit sz cp |
((sz := self size) == 0) ifTrue: [
   ^ self copy
].
limit := sz + 1.
first := 1 .
(self codePointAt: 1) codePointIsRubyWhitespace ifTrue: [ | j |
  first := nil .
  j := 2.
  [ j == limit ] whileFalse: [
      (self codePointAt: j) codePointIsRubyWhitespace ifTrue: [
         j := j + 1.
      ] ifFalse:[
         first := j.
         j := limit .
       ].
  ].
  first ifNil: [ ^ self class new ].
].

((cp := self codePointAt: sz ) == 0 or:[ cp codePointIsRubyWhitespace ]) ifTrue:[
  sz - 1 _downTo: 1 do:[ :k |
    (self codePointAt: k) codePointIsRubyWhitespace ifFalse:[ "for 1.9 add   or:[ cp == 0 ]"
      ^ self copyFrom: first to: k
    ].
  ].
].
first == 1 ifTrue:[  ^ self copy ].
^ self copyFrom: first to: sz .

%


set class String
category: '*maglev-runtime'
method:
_rubyStripInPlace
| first limit sz cp newSiz |
((sz := self size) == 0) ifTrue: [
   ^ nil "no modification made"
].
limit := sz + 1.
first := 1 .
(self codePointAt: 1) codePointIsRubyWhitespace ifTrue: [ | j |
  first := nil .
  j := 2.
  [ j == limit ] whileFalse: [
      (self codePointAt: j) codePointIsRubyWhitespace ifTrue: [
         j := j + 1.
      ] ifFalse:[
         first := j.
         j := limit .
       ].
  ].
  first ifNil: [ self size: 0 . ^ self ].
].

((cp := self codePointAt: sz) == 0 or:[ cp codePointIsRubyWhitespace]) ifTrue: [
  sz - 1 _downTo: 1 do:[ :k |
    (self codePointAt: k) codePointIsRubyWhitespace ifFalse:[ "for 1.9 add   or:[ cp == 0 ]"
       self replaceFrom: 1 to: (newSiz := 1 + k - first) with: self startingAt: first .
       self size: newSiz .
       ^ self
    ].
  ].
].
first ~~ 1 ifTrue:[  
  newSiz := 1 + sz - first .
  self replaceFrom: 1 to: newSiz with: self startingAt: first .
  self size: newSiz .
  ^ self
].
^ nil "no modification made"

%


set class String
category: '*maglev-runtime'
method:
_trMapping: to

"Create a translation map for tr.  Characters in self are mapped to characters in the to string.
If self starts with $^, then the sense is negated. Character ranges (e.g., 'a-z') are expanded.
A nil in the map means to delete that character. If self is the single character $^, then treat
as non-negating."

| map expandedTo expandedFrom defaultCh negating toSiz |

negating := (self size > 1) and: [ (self at: 1) == $^] . "Handle self is '^'"
expandedTo := to sequenceExpand .
expandedFrom := self sequenceExpand: negating. "Note: this will not have the leading $^."
toSiz := expandedTo size .
defaultCh := toSiz == 0 ifTrue:[ nil ] ifFalse: [expandedTo at: toSiz] .

negating
	ifTrue:[
		"We are negating the characters in from.  All characters not in from get
		set to the default character."
		map := Array new: 256 withAll: defaultCh .
		1 to: expandedFrom size do:[:i| | index | 
			index := (expandedFrom codePointAt: i) + 1 .
			"Set all the characters we are not translating back to themsleves."
			map at: index put: (expandedFrom at: i)]]
	ifFalse:[ |  frSiz n index limit chCls  | 
		map := Array new: 256 .
		chCls := Character .
		1 to: 256 do:[:i| map at: i put: (chCls withValue: (i-1)) ] .
		frSiz := expandedFrom size .
		limit := toSiz min: frSiz .
		n := 1 .
		[ n <= limit ] whileTrue:[ | toCh |
		   index := (expandedFrom codePointAt: n) + 1 .
		   toCh := expandedTo at: n .
		   map at: index put: toCh .
		   n := n + 1 ].
	    [ n <= frSiz ] whileTrue:[
			index := (expandedFrom codePointAt: n) + 1 .
			map at: index put: defaultCh .
			n := n + 1 ] .
		].
^ map.

%

