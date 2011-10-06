
set class IndentingStream
category: '*maglev-Documentation'
classmethod:
comment
^
' IndentingStream wraps another stream, but indents each line to its indent level.
        This is for pretty-printing nested structures such as ParseNodes.
        New protocol includes #indentMore and #indentLess
'
%


set class IndentingStream
category: '*maglev-Instance creation'
classmethod:
newPrinting

^ self new stream: (PrintStream on: String new )
%


set class IndentingStream
category: '*maglev-Instance creation'
classmethod:
on: aStream
  ^self new stream: aStream
%


set class IndentingStream
category: '*maglev-Instance creation'
method:
contents
^ stream contents
%


set class IndentingStream
category: '*maglev-Instance creation'
method:
cr
        stream cr.
        indentLevel timesRepeat: [stream nextPutAll: '  ']
%


set class IndentingStream
category: '*maglev-Instance creation'
method:
do: aCollection 
| first idx |
first := true .
self indentMore ; nextPutAll: ' ( '.
idx := 1.
aCollection do:[:aNode| 
  first ifFalse:[ self cr ].
  first := false .
  self nextPutAll: idx asString ; nextPut: $: .
  aNode printFormattedOn: self .
  idx := idx + 1 .
].
self indentLess; nextPutAll: ' )'; cr .
%


set class IndentingStream
category: '*maglev-Instance creation'
method:
indentLess
        indentLevel := indentLevel - 1
%


set class IndentingStream
category: '*maglev-Instance creation'
method:
indentMore
        indentLevel := indentLevel + 1
%


set class IndentingStream
category: '*maglev-Instance creation'
method:
nextPut: aChar
  " assume aChar is not  CR "
  stream nextPut: aChar
%


set class IndentingStream
category: '*maglev-Instance creation'
method:
nextPutAll: aCollection
        stream nextPutAll: aCollection
%


set class IndentingStream
category: '*maglev-Instance creation'
method:
print: aLabel bool: aBoolean
  stream nextPutAll: aLabel ; nextPutAll: aBoolean asString 
%


set class IndentingStream
category: '*maglev-Instance creation'
method:
print: aLabel int: anInteger
  stream nextPutAll: aLabel ; nextPutAll: anInteger asString 
%


set class IndentingStream
category: '*maglev-Instance creation'
method:
print: aLabel symbol: aSymbol
  stream nextPutAll: aLabel ; nextPutAll: aSymbol printString 
%


set class IndentingStream
category: '*maglev-Instance creation'
method:
space
   stream space
%


set class IndentingStream
category: '*maglev-Instance creation'
method:
stream: aStream
    indentLevel := 0.
    stream := aStream
%

