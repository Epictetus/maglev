
set class RubyEnv
category: '*maglev-runtime'
classmethod:
_current
  "Return the current instance of RubyEnv , the value of ruby ENV ,
   for use within smalltalk code.  not used from generated code."

  ^ self _currentEnvAssoc _value

%


set class RubyEnv
category: '*maglev-runtime'
classmethod:
_currentAssociation: aKey with: aBlock
   | tempsDict assoc   |
assoc := (tempsDict := SessionTemps current) associationAt: aKey otherwise: nil .
assoc ifNil:[  | val |
  val := aBlock value.
  assoc := RubySymbolAssociation newWithKey: aKey .
  assoc _value:  aBlock value .
  tempsDict addAssociation: assoc .
].
^ assoc

%


set class RubyEnv
category: '*maglev-runtime'
classmethod:
_currentEnvAssoc
  ^ self _currentAssociation: #RubyENVCurrent with:
    [ | list dict siz |
       list := RubyEnv _getAllEnvVars .
       dict := RubyEnv _basicNew:( siz := list size) .
         1 to: siz  by: 2 do:[:n |
           dict at: (list at: n) put: (list at: n + 1) immediateInvariant
         ].
       dict
    ]

%


set class RubyEnv
category: '*maglev-runtime'
classmethod:
_currentPlatformAssoc
  ^ self _currentAssociation:  #RubyPLATFORMCurrent 
                 with: [ AbstractException cpuOsKindString ]

%


set class RubyEnv
category: '*maglev-Ruby support'
classmethod:
_envPrim: opcode with: aString  with: argString
"opcode 0  getAllEnvVars()
        1  getenv(aString)
        2  putenv(aString, aString)
        3  unsetenv(aString)"
<primitive: 766>
aString _validateClass: String  .
opcode >= 1 ifTrue:[
  aString ifNotNil:[ aString _validateClass: String  ].
  opcode == 2 ifTrue:[ argString _validateClass: String  ].
].
self _primitiveFailed:#_envPrim:with:with: 
     args: { opcode . aString . argString }
%


set class RubyEnv
category: '*maglev-Ruby support'
classmethod:
_getAllEnvVars

"Returns an Array of environment names and values.
   obtained by enumerating the list
       extern char** environ;   
 which is defined in /usr/include/unistd.h  .
 Each element of the result is a String 
  (result at:1) is the name of the first environment variable,
  (result at:2) is the value of the first environment variable"
 
^ self _envPrim:0 with: nil with: nil
%


set class RubyEnv
category: '*maglev-Ruby support'
classmethod:
_getenv: aString
  " Returns a String, or nil"
  ^ self _envPrim:1 with: aString with: nil
%


set class RubyEnv
category: '*maglev-Ruby support'
classmethod:
_putenv: aKey with: aValue
  "aKey and aValue must be Strings.
   Returns 0 if successful, otherwise an errno SmallInt."

  ^ self _envPrim:2 with: aKey with: aValue
%


set class RubyEnv
category: '*maglev-Ruby support'
classmethod:
_unsetenv: aKey
  "aKey be a String.
   Returns 0 if successful, otherwise an errno SmallInt."

  ^ self _envPrim:3 with: aKey with: nil
%


set class RubyEnv
category: '*maglev-Ruby support'
method:
at: aKey put: aValue
  "invoked from Smalltalk only"

  self class _putenv: aKey with: aValue .
  ^ super @ruby1:__atkey_put: aKey _: aValue 
%

