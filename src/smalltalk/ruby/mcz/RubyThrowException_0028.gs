
set class RubyThrowException
category: '*maglev-Ruby support'
classmethod:
comment
^'RubyThrowException is used in implementation of Ruby methods Kernel#catch,
Kernel#throw , and Signal#trap .
'
%


set class RubyThrowException
category: '*maglev-Ruby support'
classmethod:
_initSignalConstants: envId
 | kind arr tns dict sigArr cls tmps sigTerm |
 tmps := SessionTemps current .
 kind := (tmps at: #Maglev_ruby_debugFlag otherwise: false)
       ifTrue:[ 3 ] ifFalse:[ 2 ].
 arr := RubySocket _socketOsConstants: kind .
 dict := RubyHash new .
 sigArr := { } .
 1 to: arr size by: 2 do:[:k | | nam val |
    val := (arr at: k + 1) .
    val ifNotNil:[
       nam := arr at: k .
       dict at: nam put: val .
    ].
    sigArr _rubyAt: val put: nam .
 ].
 dict immediateInvariant .
 sigArr immediateInvariant .
 tns := (cls := RubySignal)  transientNameSpaceForStore: envId .
 tns at: #TrappableByName transientRuntimePut: dict ;
     at: #TrappableSignals transientRuntimePut: sigArr ;
     at: #TrappedSignals  transientRuntimePut:  RubyIdentityHash new .
 (sigTerm := dict at: 'TERM') ifNotNil:[
   tns at: #SigTERM transientRuntimePut: sigTerm
 ].
 tmps at:#RUBY_RubySignal putNoStub: cls . "protect in-memory copy from GC"
 ^ self
%


set class RubyThrowException
category: '*maglev-Ruby support'
method:
initialize
  gsNumber := ERR_RubyThrowException .
  gsResumable := true .
  gsTrappable := true 
%


set class RubyThrowException
category: '*maglev-Ruby support'
method:
name
 "return the name of a throw ."
 ^ gsArgs
%


set class RubyThrowException
category: '*maglev-Ruby support'
method:
name: aSymbol
  gsArgs := aSymbol
%


set class RubyThrowException
category: '*maglev-Ruby support'
method:
name: aSymbol value: anObject
  gsArgs := aSymbol .
  value := anObject
%


set class RubyThrowException
category: '*maglev-Ruby support'
method:
signalNumber
 "return the signal number associated with Signal#trap"
 ^ signalNum
%


set class RubyThrowException
category: '*maglev-Ruby support'
method:
_value
  ^ value
%

