
set class RubySystemExit
category: '*maglev-Ruby support'
method:
initialize
  super initialize .
  gsNumber := ERR_RUBY_SystemExitError .
  runAtExitHandlers := true .
%


set class RubySystemExit
category: '*maglev-Ruby support'
method:
runAtExitHandlers
  ^ runAtExitHandlers
%


set class RubySystemExit
category: '*maglev-Ruby support'
method:
runAtExitHandlers: aValue
  runAtExitHandlers := aValue
%


set class RubySystemExit
category: '*maglev-runtime'
method:
status
  ^ status

%


set class RubySystemExit
category: '*maglev-Ruby support'
method:
status: aValue
  status := aValue
%

