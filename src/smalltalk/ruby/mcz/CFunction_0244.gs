
set class CFunction
category: '*maglev-Ruby support'
classmethod:
_addRubyPrimTypes

"executed once during slowrubyimage build step .
 Add ruby primitive types to the ArgTypesDict, for use by
 varArgs support in the primitives."
| d |
d := ArgTypesDict .
d at: #char  put: (d at: #int8 );
  at: #uchar put: (d at: #uint8) ;
  at: #short put: (d at: #int16) ;
  at: #ushort put: (d at: #uint16) ;
  at: #int   put: (d at: #int32) ;
  at: #long   put: (d at: #int64) ;
  at: #uint   put: (d at: #uint32) ;
  at: #ulong   put: (d at: #uint64) ;
  "double, float not yet supported for varArgs"
  at: #size_t   put: (d at: #int64) ;
  at: #long_long  put: (d at: #int64) ;
  at: #ulong_long put: (d at: #uint64) ;
  at: #pointer put: (d at: #ptr ) ;
  at: #buffer_out put: (d at: #ptr ) ;
  at: #buffer_in put: (d at: #ptr ) ;
  at: #buffer_inout put: (d at: #ptr ) ;
  at: #string put: (d at: #'char*' ) ;
  at: #const_string put: (d at: #'char*' ) .
%

