
set class Number
category: '*maglev-runtime'
method:
asString
"provided to aid in topaz display of method temps, etc.
 In the base Smalltalk image, Number has no implementation of asString,
  and each numeric subclass provides its own implementation."

^ [
    self @env1:to_s 
  ] onException: AbstractException do:[:ex | 
    ex return: super asString
  ]

%


set class Number
category: '*maglev-runtime'
method:
_inspect
  ^ self asString

%


set class Number
category: '*maglev-Ruby numeric'
method:
_rubyRetry: aSelector coercing: aNumber env: envId
  "aSelector includes the ruby selector suffix.
   send sites are ruby_selector_suffix dependent"
^ [ | arr  |
    arr := aNumber @ruby1:coerce:  self .  
    "arr is { coercedSelf. coercedNum }"
    (arr at: 1) with: (arr at: 2) perform: aSelector env: envId 
  ] onSynchronous: AbstractException do:[:ex |
    ex return:( ArgumentTypeError signal:'numeric coercion failure')
  ]
%

