
set class ProcessorScheduler
category: '*maglev-Ruby support'
method:
_killOtherRubyThreads

"terminate other threads without running their ensure blocks.
 Used in handling of RubySystemExit exception for Kernel#exit . "
| procs |
procs := self allProcesses . "result is an IdentitySet"
procs removeIfPresent: (GsProcess _current) .
1 to: procs size do:[:n | | aProc |
  aProc := procs _at:n  .
  aProc _setTerminated .
  aProc _unscheduleProcess .
].
%


set class ProcessorScheduler
category: '*maglev-Ruby support'
method:
_waitForIoSelect
  "caller must have already invoked Kernel>>selectRead:write:error:timeout:
   to add sockets to the scheduler's  omPtr->GsSocket_poller"

  | proc ws |
  (proc := activeProcess) _waitingOn: #IoSelect .

  self _add: proc toSet: (ws := waitingSet) .
  proc _onQueue: ws .
  self _reschedule 
%


set class ProcessorScheduler
category: '*maglev-Ruby support'
method:
_waitForIoSelect: msToWait
  "caller must have already invoked Kernel>>selectRead:write:error:timeout:
   to add sockets to the scheduler's  omPtr->GsSocket_poller"

  | proc |
  (proc := activeProcess) _waitingOn: #IoSelect .

  proc _signalTime: (self _now) + msToWait.
  self _addDelay: proc to: delayQueue .

  self _reschedule .

  (proc := activeProcess) _signalTime ~~ nil  ifTrue: [
    delayQueue removeIdentical: proc otherwise: nil . "inline _delayUnschedule:"
    proc _signalTime: nil.
  ].
%

