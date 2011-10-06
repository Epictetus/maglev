
set class MultiByteString
category: '*maglev-Ruby support'
method:
_asSymbolWithRubySuffix: argInt
| str |
str := self _reduce .
str charSize == 1 ifFalse:[ Error signal:'MultiByte ruby selectors not supported'].
^ str _asSymbolWithRubySuffix: argInt
%

