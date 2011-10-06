
set class CCallin
category: '*maglev-Ruby support'
classmethod:
_rubyNew: argsArray
  "a ruby primitive"
  | envId |
  envId := 1"__callerEnvId" .
  ^ self name: (argsArray at:1)"aName"
          result: (argsArray at:2)  "resType"
          args: (argsArray at:3) "argumentTypes" 
          envId: envId 
%

