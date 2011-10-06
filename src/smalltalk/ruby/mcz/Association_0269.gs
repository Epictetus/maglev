
set class Association
category: '*maglev-Ruby support'
classmethod:
trapGlobalAssignment: anAssociation
  "for debugging use only, requires uncommenting of code in 
   value: and _value: "
  TrapGlobalAssignment := anAssociation
%


set class Association
category: '*maglev-Ruby support'
method:
value: aVal

  "self == TrapGlobalAssignment ifTrue:[ self pause ]."
  value := aVal 
%


set class Association
category: '*maglev-Ruby support'
method:
_value: aVal

  "self == TrapGlobalAssignment ifTrue:[ self pause ]."
  value := aVal 
%

