
set class Range
category: '*maglev-Instance Creation'
classmethod:
from: aStart limit: anEnd

^ self basicNew _from: aStart limit: anEnd by: 1 
%


set class Range
category: '*maglev-Instance Creation'
classmethod:
from: aStart to: anEnd

^ self basicNew _from: aStart to: anEnd by: 1 
%


set class Range
category: '*maglev-Instance Creation'
classmethod:
from: aStart to: anEnd by: anIncrement

^ self basicNew _from: aStart to: anEnd by: anIncrement
%


set class Range
category: '*maglev-Comparing'
method:
= aRange

"Returns true if the receiver is equal to the argument, false otherwise."

self == aRange ifTrue:[ ^ true ].

(aRange _isRange) ifFalse:[ ^ false ].
^ (aRange _from = from and:[ aRange limit = self limit ])
   and:[ aRange increment = by ]
%


set class Range
category: '*maglev-Accessing'
method:
excludeEnd
  ^ excludeEnd
%


set class Range
category: '*maglev-Comparing'
method:
hash

"Returns some Integer related to the contents of the receiver.  If two objects
 compare equal (=) to each other, the results of sending hash to each of those
 objects must also be equal."

^ (from + self limit + by) hash
%


set class Range
category: '*maglev-Accessing'
method:
limit

 "Return the limit for an iteration of the C style
    for (int j = begin ; j < limit; j += by) "

  ^ excludeEnd ifTrue:[ to ] ifFalse:[ to + by ]
%


set class Range
category: '*maglev-Private'
method:
rubyPrivateSize
  ^ 4 "Hide smalltalk instance variables from ruby (marshal)"
%


set class Range
category: '*maglev-Private'
method:
_from: aStart limit: anEnd by: anInc

  from := aStart .
  to := anEnd .
  by := anInc .
  excludeEnd := true
%


set class Range
category: '*maglev-Private'
method:
_from: aStart to: anEnd by: anInc

  from := aStart .
  to := anEnd .
  by := anInc .
  excludeEnd := false 
%

