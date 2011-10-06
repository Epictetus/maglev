
set class RubyDirectory
category: '*maglev-Documentation'
classmethod:
comment
^ 'Instances of RubyDirectory represent a directory on the file system

   instVars
      entries - an Array of Strings
   '
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_chdir: aString

"aString specifies directory to change to .
 Result is 0 if successful or an errno SmallInteger "

^ self _prim: 0 with: aString with: nil
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_errno
  "fetch current value of errno , and clear errno"
^ self _uidPrim:8 with: nil
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_getgrgent: anInt
  "anInt == 0 means  setgrent() to rewind enumeration to start, and then call getgrent().
   anInt == 1 means get next group with getgrent()  .
   result is an Array:  name, passwd, gid, groupName1 ... groupNameN ,
       or nil if no more entries in which case endgrent() will have been called
  not thread safe, enumeration position destroyed by _getgrnam:,  _getgrgid: ."

(anInt == 0 or:[ anInt == 1]) ifFalse:[ ArgumentTypeError signal:'invalid argument'].
^ self _uidPrim: 5 with: anInt
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_getgrgid: anInt
   "calls getgrgid() with specified arg,
   result is an Array:   name, passwd, gid,  Array of group names
            or nil if anInt is not the number of a group."
anInt _isSmallInteger ifFalse:[ ArgumentTypeError signal:'invalid argument'].
^ self _uidPrim:7 with: anInt
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_getgrnam: aString
  "calls getgrnam() with specified arg,
   result is an Array:    name, passwd, gid, Array of group names .
            or nil if aString is not the name of a group."

aString _isRubyString ifFalse:[ ArgumentTypeError signal:'invalid argument'].
^ self _uidPrim:6 with: aString
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_getlogin
  "Returns Unux login name of the current process via getlogin(), if available.
   Result is a String, or nil "

| res |
res := self _uidPrim: 0 with: nil .
res ifNil:[ | uid arr |
  uid := self _uidPrim: 1 with: nil . "getuid()"
  arr := self _getpwuid: uid .
  arr ifNotNil:[ res := arr at: 1 ].
].
^ res
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_getpwent: anInt
  "anInt == 0 : setpwent() to rewind enumeration to start, and then call getpwent().
   anInt == 1 : get next password entry with getpwent() 
   result is an Array:  name, passwd, uid, gid, realName, homeDirPath, shell ,
         or nil if no more entries available, in which case endpwent() 
         will have been called
  not thread safe, enumeration position destroyed by _getpwuid:, _getpwnam: .
 "
(anInt == 0 or:[ anInt == 1]) ifFalse:[ ArgumentTypeError signal:'invalid argument'].
^ self _uidPrim: 2 with: anInt 
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_getpwnam: aString
  "returns an Array  name, passwd, uid, gid, realName, homeDirPath, shell,
         or nil if aString is not a valid user name "

aString _isRubyString ifFalse:[ ArgumentTypeError signal:'invalid argument'].
^ self _uidPrim: 4 with: aString
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_getpwuid: anInt
  "returns an Array  name, passwd, uid, gid, realName, homeDirPath, shell,
         or nil if anInt is not a valid uid 
   If anInt==nil, returns the password information for the uid obtained from getuid() "

anInt _isSmallInteger ifFalse:[ ArgumentTypeError signal:'invalid argument'].
^ self _uidPrim: 3 with: anInt
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_getuid
   "calls getuid() for the current user.
   result is an Array:   name, passwd, uid, gid, realName, homeDirPath, shell."
^ self _uidPrim: 1 with: nil
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_getwd

"Return a String specifying current directory.
 Result is a String , or an errno SmallInteger"
^ self _prim: 2 with: nil with: nil
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_mkdir: aString permissions: aSmallInt
"create the directory specified by aString with permissions aSmallInt.
 Returns 0 if successful, otherwise an errno SmallInt."

^ self _prim: 4 with: aString with: aSmallInt
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_new: aString

"Return a new instance containing the contents of the specified
 directory, or returns an errno SmallInteger"
| list res |
list := GsFile _contentsOfServerDirectory: aString expandPath: false .
list _isSmallInteger ifTrue:[ ^ list "an errno" ].
(res := self rubyBasicNew) entries: list .
^ res
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_prim: opcode with: aString  with: anInt
"opcode 0  chdir(aString)   
        1  rmdir(aString)
	2  getwd() 
        3  <not used>
        4  mkdir(aString, anInt) 
        5  umask(anInt) (used by GsFile)"
<primitive: 765>
aString ifNotNil:[ aString _validateClass: String  ].
anInt ifNotNil:[ anInt _validateClass: SmallInteger ].
self _primitiveFailed:#_prim:with:with: args: { opcode . aString . anInt }
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_rmdir: aString

"Remove the specified directory,
 Result is 0  if successful or an errno SmallInteger"
^ self _prim: 1 with: aString with: nil
%


set class RubyDirectory
category: '*maglev-Ruby support'
classmethod:
_uidPrim: opcode with: arg
"opcode 0   getlogin
        1   getuid()
        2   [setpwent() ] ; getpwent() , enumeration not thread-safe
        3   getpwuid(uidInt)    destroys getpwend enumeration position
        4   getpwnam(nameStr)   destroys getpwent enumeration position
        5   [setgrent() ]; getgrent() , enumeration not thread-safe
        6   getgrnam(nameStr)	destroys getgrent enumeration position
        7   getgrgid(gidInt)
        8   return current value of errno , and clear errno "

<primitive: 792>
self _primitiveFailed: #_uidPrim:with: args: { opcode . arg }
%


set class RubyDirectory
category: '*maglev-Ruby support'
method:
entries
  ^ entries
%


set class RubyDirectory
category: '*maglev-Ruby support'
method:
entries: anArray
  entries := anArray
%

