
set class UNIXServer
category: '*maglev-as yet unclassified'
method:
bind: aPath

  | res |
  res := self _twoArgPrim: 13 with: aPath with: nil .
  res == self ifFalse:[ self signalSocketError:'bind failed' ].
  ^ self
%


set class UNIXServer
category: '*maglev-as yet unclassified'
method:
speciesForAccept

  ^ UNIXSocket
%

