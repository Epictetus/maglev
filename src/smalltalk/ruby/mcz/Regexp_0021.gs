
set class Regexp
category: '*maglev-Ruby private'
classmethod:
basicNew

 "disallowed"
  self shouldNotImplement: #basicNew
%


set class Regexp
category: '*maglev-Ruby private'
classmethod:
new
 "disallowed"
  self shouldNotImplement: #new
%


set class Regexp
category: '*maglev-Ruby support'
classmethod:
new: aStringOrRegexp
  "Return a new instance of the receiver, 
  for specified pattern string .

  Regexp variant of argument not implemented yet
  "

^ (self rubyNewCFinalizer_stBaseClass: Regexp)
         _compile: aStringOrRegexp options: 0
%


set class Regexp
category: '*maglev-Ruby support'
classmethod:
new: aString options: anInteger
  "Return a new instance of the receiver, 
  for specified pattern string and options.  
  langString must specifiy ASCII ."

  ^ (self rubyNewCFinalizer_stBaseClass: Regexp)
	 _compile: aString options: anInteger
%


set class Regexp
category: '*maglev-Ruby support'
classmethod:
new: aString options: anInteger lang: langString

  "Return a new instance of the receiver, 
  for specified pattern string and options.  
  langString must specifiy ASCII ."

  | res status opts |

  aString _isRegexp ifTrue:[ ^ aString copy ].
  
  (opts := anInteger) _isSmallInteger ifFalse:[
    ArgumentError signal: 'options should be a SmallInteger'
  ].
  langString ifNotNil:[
    langString _isOneByteString ifFalse:[
      ArgumentError signal: 'lang string should be a String'.
    ].
    opts := self optsFromLang: langString opts: opts .
  ].
   
  res := self rubyNewCFinalizer_stBaseClass: Regexp .
  status := res _compile: aString options: opts .
  status ~~ res ifTrue:[ RegexpError signal: status ].
  ^ res
%


set class Regexp
category: '*maglev-Ruby support'
classmethod:
optsFromLang: aString opts: opts
  | ch langbits |
  aString size == 0 ifTrue:[ ^ opts ].
  ch := (aString at: 1) asLowercase  .
  (ch == $n and:[
    (aString equalsNoCase:'n') or:[aString equalsNoCase:'none']]) ifTrue:[
      langbits := KCODE_NONE
  ] ifFalse:[
  (ch == $u and:[
    (aString equalsNoCase:'u') or:[aString equalsNoCase:'utf8']]) ifTrue:[
      langbits := KCODE_UTF8 .
  ] ifFalse:[
  (ch == $e and:[
    (aString equalsNoCase:'e') or:[aString equalsNoCase:'euc']]) ifTrue:[
      langbits := KCODE_EUC .
  ] ifFalse:[
  (ch == $s and:[
    (aString equalsNoCase:'s') or:[aString equalsNoCase:'sjis']]) ifTrue:[
      langbits := KCODE_SJIS .
  ]]]].
  langbits ifNotNil:[
     ^ (opts bitAnd: 16rFFFFFFFF) bitOr:( langbits bitShift: 32"KCODE_shift" ). 
  ].
  ^ opts
%


set class Regexp
category: '*maglev-Ruby private'
classmethod:
_basicNew

  ^ self rubyNewCFinalizer_stBaseClass: Regexp
%


set class Regexp
category: '*maglev-Ruby support'
method:
= aRegexp

 "Return true if the receiver and arg have equal source patterns and 
  the same casefold values.
   Note, kcode assumed nil."

  aRegexp _isRegexp ifFalse:[ ^ false ] .
  ^ source = aRegexp source and:[ self casefold == aRegexp casefold ]
%


set class Regexp
category: '*maglev-Ruby support'
method:
casefold
  "Return true if the IGNORECASE bit is set in the receiver's options"

  ^ (options bitAnd: IGNORECASE) ~~ 0
%


set class Regexp
category: '*maglev-Ruby support'
method:
kcode
  | kc |
  kc := options bitShift: -32  "- KCODE_shift" .
  kc == 0 ifTrue:[ ^ nil ].
  (kc == KCODE_NONE) ifTrue:[ ^ 'none' copy ].
  (kc == KCODE_UTF8) ifTrue:[ ^ 'utf8' copy ].
  (kc == KCODE_EUC)  ifTrue:[ ^ 'euc' copy ].
  (kc == KCODE_SJIS) ifTrue:[ ^ 'sjis' copy ].
  ^ nil
%


set class Regexp
category: '*maglev-Ruby support'
method:
match: aString
  "Returns a MatchData or nil ."

  ^ self _search: aString from: 0 to: nil .
%


set class Regexp
category: '*maglev-Ruby support'
method:
options
  "Returns options specified when receiver was created."
  ^ options 
%


set class Regexp
category: '*maglev-Ruby support'
method:
rubyPrivateSize
^ 2 "inline  Regexp instSize"
%


set class Regexp
category: '*maglev-Ruby support'
method:
source
  "Returns the original pattern string used to create the receiver."
  ^ source
%


set class Regexp
category: '*maglev-Ruby support'
method:
to_s
  | optsstr res idx src |  
   "options bit masks MULTILINE = 4; EXTENDED = 2; IGNORECASE = 1 "
  optsstr := #( '(?-mix:'  
               '(?i-mx:'
               '(?x-mi:'
               '(?ix-m:'
               '(?m-ix:'
               '(?mi-x:'
               '(?mx-i:'
               '(?mix:' ) .
  res := (optsstr at: (options bitAnd: 16r7) + 1 ) copy .
  src := source .
  (idx := src indexOf: $/ startingAt: 1 ) == 0 ifTrue:[
    res addAll: src .
  ] ifFalse: [  | prev |
     prev := 1 .
     [
       res add: (src copyFrom: prev to: idx - 1); add:  '\/'.
       prev := idx + 1 .
       idx := src indexOf: $/ startingAt: prev .
       idx ~~ 0
     ] whileTrue .
     res add: (src copyFrom: prev to: src size )  .
  ].
  res add: $) .
  ^ res
%


set class Regexp
category: '*maglev-Private'
method:
_compile: aString options: anInteger
  "Initialize the receiver, for specified pattern string
   and options.  Returns receiver if successful or an error String
   if onig_new() returned non-zero status. 
   
   If anInteger is a SmallInteger , then
     anInteger & 16rFFFFFFFF contains bits per ONIG_OPTION_* in oniguruma.h.
     anInteger & 16rFFFF00000000 contains a KCODE value per one of the
     KCODE* class variables, and is translated to an ONIG_ENCODING* value .
   else anInteger==nil implies  ONIG_OPTION_IGNORECASE,
   else any other value of anInteger implies  ONIG_OPTION_NONE 
   "

  <primitive: 683>

  aString _validateClass:  String  . "Regexp arg not implemented yet"
  ^self _primitiveFailed: #_compile:options: args: { aString . anInteger }
%


set class Regexp
category: '*maglev-Private'
method:
_matchCBytes:aCByteArray from: startOffset limit: limitOffset string: aString
  "Returns an instance of MatchData or nil. 
   calls onig_search to do a match_start search starting at startOffset
   within aCByteArray and searching to the end of aCByteArray .
   startOffset must be a SmallInteger >= 0.
  Returns  nil  if startOffset out of bounds .
  aCByteArray must be of same size as aString, and contain
  same contents as string; primitive fails if sizes do  not match,
  otherwise caller responsible for contents being the same.
  aCByteArray is used by onig_search.  Any resulting MatchData's
  will reference aString . 
  Caller responsible for ensuring that aString is frozen or otherwise
  not modified between calling this method and last reference
  to any MatchData produced by this method."

 <primitive: 824>
  aString _validateClass:  String .
  aCByteArray class == CByteArray 
    ifFalse:[ ArgumentError signal:'not a CByteArray' ].
  aString size == aCByteArray size 
    ifFalse:[ ArgumentError signal:'inconsistent sizes'].
  "could be failed  recompile after faulting in a persistent Regexp."
  ^ self _primitiveFailed: #_matchCBytes:from:limit:string:
         args: { aCByteArray . startOffset . limitOffset . aString }
%


set class Regexp
category: '*maglev-Ruby support'
method:
_regex_to_s
  | optsstr res src |  
   "options bit masks MULTILINE = 4; EXTENDED = 2; IGNORECASE = 1 "
  optsstr := #( '(?-mix:'  
               '(?i-mx:'
               '(?x-mi:'
               '(?ix-m:'
               '(?m-ix:'
               '(?mi-x:'
               '(?mx-i:'
               '(?mix:' ) .
  res := (optsstr at: (options bitAnd: 16r7) + 1 ) copy .
  src := source .
  res addAll: src ;
      add: $) .
  ^ res
%


set class Regexp
category: '*maglev-Private'
method:
_search: aString from: aStart to: aLimit 
  "Returns an instance of MatchData or nil. calls onig_search .
   aStart must be a SmallInteger >= 0. 
   if aLimit == nil , limit is aString size
   else if aLimit == true , use  onig_match() searching
      from aStart to  aString.size
   else aLimit must be a SmallInteger >= 0 and <= aString size .
   if aLimit < aStart,  this is a backwards search.
   If aString is nil, returns nil .
   If aString is a String of size 0, returns nil.
  "

  <primitive: 684>
  aString _validateClass:  String .
  "could be failed  recompile after faulting in a persistent Regexp."
  ^ self _primitiveFailed: #_search:from:to: 
         args: { aString . aStart . aLimit }
%

