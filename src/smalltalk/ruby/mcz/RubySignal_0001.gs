
set class RubySignal
category: '*maglev-Ruby support'
method:
_trapSignal: sigNumber ignore: ignoreArg block: aBlock

"sigNumber must be a SmallInteger, 
 ignoreArg is one of
    nil  install specified block
    1    install  SIG_IGN handler
    2    revert to the VM default for specified signal
 aBlock must be nil, an ExecBlock or RubyProc .
 Installs specified C signal handler in VM .
 Caller responsible for Adjusting contents of the
 Ruby IdentityHash named TrappedSignals to match the args.

 Returns previously installed aBlock or nil .
"
<primitive: 872>

self _primitiveFailed: #_trapSignal:ignore:block: 
     args: { sigNumber . ignoreArg . aBlock }
%

