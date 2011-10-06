
set class Boolean
category: '*maglev-Ruby support'
classmethod:
useRubyClasses: aBoolean  bootStrap: forBootBoolean
  "If aBoolean==true, adjust the VM's class lookup table for 
   special objects so that   
     true class == TrueClass
     false class == FalseClass
   else adjust the table so that
     true class == Boolean
     false class == Boolean .
   The class lookup table also controls the class used for method 
   lookup when receiver is true or false .
   If aBoolean == false, also initializes the Ruby C extensions implementation.
   Returns receiver.  
   Invoked during Ruby Session initialization. "
<primitive: 849>
aBoolean _validateClass: Boolean .
forBootBoolean _validateClass: Boolean
%


set class Boolean
category: '*maglev-Ruby support'
classmethod:
usingRubyClasses
  ^ true class == TrueClass 
%

