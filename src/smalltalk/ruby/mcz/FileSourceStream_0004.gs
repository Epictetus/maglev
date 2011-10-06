
set class FileSourceStream
category: '*maglev-Private'
method:
initializeWith: aGsFile

| status |
self initialize .

input := aGsFile .
inputSize := aGsFile fileSize .
bufSize := inputSize min: NormalBufSize .
buf := String new: bufSize .
status := self _readNextChunk .
%


set class FileSourceStream
category: '*maglev-Private'
method:
_readNextChunk

"read next chunk from a GsFile, returns number of bytes read"
| numToRead status |
numToRead := bufSize min: (inputSize - position + 1) .
status := input next: numToRead ofSize: 1 into: buf .
status = numToRead ifFalse:[  | fileErr |
  fileErr := input lastErrorString .
  self error:'File Read Error', fileErr
].
^ numToRead
%


set class FileSourceStream
category: '*maglev-Private'
method:
_seek: aPosition

"set position of a GsFile input, returns self "
| status |
status := input position: aPosition .
status == aPosition ifFalse:[  | fileErr |
  fileErr := input lastErrorString .
  self error:'File Seek Error', fileErr
].
%

