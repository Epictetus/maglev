
set class CByteArray
category: '*maglev-Ruby support'
classmethod:
fromAddress: anInteger
  ^ self _newFrom: anInteger offset: 0 numBytes: 0 gcFree: 0
%


set class CByteArray
category: '*maglev-Ruby support'
classmethod:
_vmOwnedMalloc: numBytes
 "(freed by in-memory GC, in VM owned memory, memory is zeroed"

^ self _newFrom: nil offset: 0 numBytes: numBytes gcFree: AutoFree_VmOwned
%


set class CByteArray
category: '*maglev-Ruby support'
classmethod:
_vmOwnedWithAll: aString
  "used by Ruby parser"
 
| res argSiz |
res := self _vmOwnedMalloc: (argSiz := aString size) .
argSiz ~~ 0 ifTrue:[
  res copyFrom: aString from: 1 to: argSiz into: 0 .
].
^ res
%


set class CByteArray
category: '*maglev-Ruby support'
method:
_rubyByteAt: anOffset

  "anOffset is zero based . 
   If anOffset < 0 or anOffset >= self size, primitive fails"

<primitive: 823>
^ nil   "offset was out of bounds, return nil"
%

