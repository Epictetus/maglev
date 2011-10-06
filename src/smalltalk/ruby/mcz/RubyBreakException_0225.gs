
set class RubyBreakException
category: '*maglev-Ruby support'
classmethod:
comment
^ 
'RubyBreakException is used to implement the "break" reserved word
in the Ruby language, except in inlined loops where the parser/code generator
translate the "break" to a GOTO bytecode.'
%


set class RubyBreakException
category: '*maglev-Ruby support'
classmethod:
signalBreakWith: aValue
  "signature and body must agree with om::SearchForRubyBreakHandler() in VM.
   sends of #signalBreakWith:  replaced with  Bc_RUBY_BREAK_FROM_PROC_u1_u32,
   when a block converted to a Ruby Proc  .

   A method containing a send of #signalBreakWith:  is forced by the
   code generator(comgen.c) to have a VariableContext .
  "
   | ex |
   (ex := self new) args: { true . aValue } .
   ^ ex signalRubyBreak
%


set class RubyBreakException
category: '*maglev-Ruby support'
classmethod:
signalRubyRetry
  | ex |
  (ex := self new) args: { false } .
  "set message text in case not handled by an on:do: of a each& style iterator"
  ^ ex signal:'retry outside of rescue, ' 
%


set class RubyBreakException
category: '*maglev-Ruby support'
method:
signalRubyBreak
  "signature and caller must agree with om::SearchForRubyBreakHandler()"
  <primitive: 2028>
   "If a handler found, new frame pushed 
      to execute  _executeHandler:   
      and prim does not return.
    else if home method of method 2 frames up is found, 
        returns to home context.
  
    Search for handler looks for frame of the form
      onException: RubyBreakException do:
    and will not find frames like
      onException: { RubyBreakException. OtherError} do: { ... }

    If exception handling succeeds and execution is to resume, 
    either the resume: or the return: primitive will do the 'goto'
    and we don't actually return from this frame .
    If handler not found, primitive fails so we can send defaultAction here.

    See also documentation in RubyBreakException>>signalBreakWith: .
"
 
  ^ self _signalWith: nil  "fall back to normal exception handling"
%

