
set class AbstractException
category: '*maglev-runtime'
classmethod:
addException: anException
  | res |
  (res := ExceptionSet new)
     addException: self ;
     addException: anException .
  ^ res

%


set class AbstractException
category: '*maglev-Ruby support'
classmethod:
commentRubyMapping
^ '
The mapping from Smalltalk to Ruby exceptions is:

  Smalltalk                        	Ruby
  ---------------------        		-------------

  AbstractException 
    Exception     			Exception
  ... RubySystemExit                    ... SystemExit    (def in ruby image)
  ... RubyScriptError                   ... ScriptError   (def in ruby image)
  ...   RubyLoadError                   ...   LoadError   (def in .mcz)
  ...   RubyNotImplementedError         ...   NotImplementedError (def in ruby image)
  ...   RubyParseError                  ...   SyntaxError  (def in .mcz)
      ControlInterrupt			    SignalException
						Interrupt (def in rubyimage)
        Break
        Breakpoint 
        ClientForwarderSend 
        Halt    
        TerminateProcess
      Error				... StandardError
        CompileError
        EndOfStream
        ExternalError 
          IOError			   IOError
	    EOFError			   EOFError    (def in ruby image)
            SocketError			     SocketError
  ...         SocketErrorEAGAIN         ...	    (all 5 def in ruby image)
  ...         SocketErrorEBADF          ...         Ruby has separate Errno::EBADF..EPIPE
  ...         SocketErrorECONNRESET     ...         .  as subclass of SystemCallError
  ...         SocketErrorENOTCONN       ...         .
  ...         SocketErrorEPIPE          ...         .
          SystemCallError  		  SystemCallError
        ImproperOperation 
          ArgumentError  		    ArgumentError
          ArgumentTypeError 		    TypeError
          CannotReturn        		    LocalJumpError
          NotFoundError  
          OffsetError                       IndexError
          OutOfRange 			    RangeError
            FloatingPointError		 	FloatDomainError
          RegexpError                       RegexpError
          RubyRuntimeError		    RuntimeError    (def in ruby image)
        IndexingErrorPreventingCommit
        InternalError 
          GciTransportError   
        LockError
        NameError 			   NameError
          MessageNotUnderstood 		     NoMethodError
        NumericError
          ZeroDivide         		    ZeroDivisionError
        RepositoryError
					     Errno::xxx  (def in ruby image)
        SecurityError			  SecurityError
        SignalBufferFull
        ThreadError			  ThreadError
        TransactionError
        UncontinuableError
        UserDefinedError
      Notification    
        Admonition
          AlmostOutOfStack 		  SystemStackError
          AlmostOutOfMemory		  NoMemoryError
          RepositoryViewLost
        Deprecated
        FloatingPointException
        InterSessionSignal
        ObjectsCommittedNotification
        TransactionBacklog 
        Warning      
          CompileWarning
      TestFailure
    RubyBreakException  
    RubyThrowException 					
'
%


set class AbstractException
category: '*maglev-Ruby support'
classmethod:
rubyBasicNew

^ self rubyBasicNew_stBaseClass: AbstractException
%


set class AbstractException
category: '*maglev-Ruby support'
classmethod:
rubyPrivateInstSize
^ AbstractException instSize
%


set class AbstractException
category: '*maglev-Ruby support'
classmethod:
signal: signalText ignoring: ignoredValue

 ^ self new signal: signalText
%


set class AbstractException
category: '*maglev-runtime'
method:
backtraceToLevel: aLevel 
 "convert the gsStack from AbstractException>>signal, if any
  to a Ruby stack report and cache it in gsStack."
| arr nativeStk |
arr := gsStack .
(arr _isArray and:[ (nativeStk := arr atOrNil:1 ) class == Boolean]) ifTrue:[
  "convert  from raw gsStack to Ruby backtrace Array "
  | backtrace level ofs arrSiz |
  backtrace := { } .
  level := 1 .
  ofs := 2 .
  arrSiz := arr size .
  [ level <= aLevel and:[ ofs < (arrSiz - 1) ]] whileTrue:[ | ip meth |
    level := level + 1.
    (ip := arr atOrNil: ofs + 1 ) ifNotNil:[ | env farr stepPoint |
      meth := arr at: ofs .
      ip < 0 ifTrue:[ ip := 0 ].
      nativeStk ifTrue:[ ip := meth _nativeIpOffsetToPortable: ip asReturn: false].
      stepPoint := meth _stepPointForIp: ip level: 2 isNative: nativeStk .
      farr := { 
          meth _descrForStack . 
          (meth _lineNumberForStep: stepPoint ) + meth _lineNumberBias.
         ( env := meth environmentId ) .
         meth homeMethod _rubyName 
      }.
      env ~~ 0 ifTrue:[ | fileLine |
          farr add: meth isRubyBridgeMethod .
          fileLine := meth _fileAndLine .
          fileLine ifNotNil:[ farr addAll: fileLine ].
      ].
      backtrace add: farr .
    ].
    ofs := ofs + 2 . 
  ].
  gsStack := backtrace .
  arr := backtrace . 
].
arr ifNil:[ arr := { } . gsStack := arr ].
^ arr

%


set class AbstractException
category: '*maglev-Ruby support'
method:
rubyPrivateSize
^ 8 "inline  AbstractException instSize"
%


set class AbstractException
category: '*maglev-Ruby support'
method:
_rubyReraise
  self _handlerActive ifTrue:[
    ^ self shallowCopy _signalWith: nil 
  ] ifFalse:[
    ^ self pass
  ]
%

