
set class RubyThreadGroup
category: '*maglev-Ruby support'
classmethod:
default

  "should be initialized by GsProcess>>initRubyMainThread , 
   called from RubyCompiler ."

  ^ SessionTemps current at: #RubyDefaultThreadGroup otherwise: nil
%


set class RubyThreadGroup
category: '*maglev-Ruby support'
classmethod:
new
  ^ self _basicNew initialize
%


set class RubyThreadGroup
category: '*maglev-Ruby support'
method:
initialize
  closed := false 
%


set class RubyThreadGroup
category: '*maglev-Ruby support'
method:
list

"Return an Array of all instances of GsProcess known to the ProcessScheduler,
  which are in the group defined by the receiver. "

 | set sched |
 set := IdentitySet new .
 (sched := ProcessorScheduler scheduler) _allProcessesInto: set inGroup: self .
 self == self class default ifTrue:[
   sched _allProcessesInto: set inGroup: nil
 ].
 ^ set asArray
%

