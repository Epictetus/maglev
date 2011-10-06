
set class IO
category: '*maglev-Ruby support'
method:
binmode

"Set the receiver to binary mode.  Has no effect on Unix."

^ self
%


set class IO
category: '*maglev-Ruby support'
method:
fcntl: op with: arg

"supported operations:  F_GETFD, F_GETFL, F_SETFL, FD_CLOEXEC.
  arguments must use the OS-dependent constants returned by
     RubySocket _socketOsConstants: 1
  F_GETFD returns value of   fileDescriptor instVar .
  Returns -1 if the arg (an Array) contains a return value,
    -2 if operation not supported, 
     0 if successful set operation,
    > 0 failed and is an errno returned.
"
<primitive: 848>
op _validateClass: SmallInteger .
arg _validateClass: Array .
self _primitiveFailed: #fcntl:with: args: { op . arg }
%


set class IO
category: '*maglev-Ruby support'
method:
setSync: aBoolean

  "has no effect, 
   GsFile and GsSocket do not buffer output in object memory"

^ self
%


set class IO
category: '*maglev-Ruby support'
method:
stat

"Returns an instance of GsFileStat describing the receiver."

^ GsFile fstat: fileDescriptor isLstat: false
%


set class IO
category: '*maglev-Ruby support'
method:
sync

  "GsFile and GsSocket do not buffer output in object memory"
  ^ true
%

