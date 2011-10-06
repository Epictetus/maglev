
set class GsSocket
category: '*maglev-Ruby support'
classmethod:
_basicNew
  ^ self rubyNewCFinalizer_stBaseClass: GsSocket
%


set class GsSocket
category: '*maglev-Ruby support'
classmethod:
_existingSocketForFd: fdInt

  ^ self _twoArgClassPrim: 17 with: fdInt with: nil
%


set class GsSocket
category: '*maglev-Ruby support'
classmethod:
_gethostbyaddr: aString

 "The bytes of aString are passed as addr to
    gethostbyaddr(const void *addr, socklen_t len, int type);
  Returns an Array containing the elements of a struct hostent, 
  or a SmallInteger errno. "

  ^ self _twoArgClassPrim: 18 with: aString with: nil
%


set class GsSocket
category: '*maglev-Ruby support'
classmethod:
_getsockaddr: serviceString host: hostString

  "Returns a String containing the bytes of a AF_INET/AF_INET6 sockaddr"

  ^ self _twoArgClassPrim: 19 with: serviceString with: hostString
%


set class GsSocket
category: '*maglev-Ruby support'
classmethod:
_getsockaddrUnix: pathString
  "Returns a String containing the bytes of a AF_UNIX sockaddr"

  ^ self _twoArgClassPrim: 21 with: pathString with: nil
%


set class GsSocket
category: '*maglev-Ruby support'
classmethod:
_unpackSockAddr: sockaddrString flags: anInt
  "Returns an Array, or a SmallInteger errno"
  ^ self _twoArgClassPrim: 20 with: sockaddrString with: anInt 
%


set class GsSocket
category: '*maglev-Ruby support'
classmethod:
_unpackSockAddrUnix: sockaddrString
  "Returns a path String"
  ^ self _twoArgClassPrim: 22 with: sockaddrString with: nil
%


set class GsSocket
category: '*maglev-Ruby support'
method:
_bindAddr: sockaddrString
  "returns self or SmallInteger errno"
  ^ self _twoArgPrim: 24 with: sockaddrString with: nil 
%


set class GsSocket
category: '*maglev-Ruby support'
method:
_peerSockAddr
  "Returns peer's sockaddr String or a SmallInteger errno"
  ^ self _zeroArgPrim: 32
%


set class GsSocket
category: '*maglev-Ruby support'
method:
_socketLocation
  "Returns receiver's sockaddr String or a SmallInteger errno"
  ^ self _zeroArgPrim: 33
%

