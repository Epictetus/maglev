category: '*maglev-as yet unclassified'
!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!  Ruby support methods for UnorderedCollection 
!=========================================================================

set class UnorderedCollection

category: '*maglev-Ruby support'
method: 
rubyPrivateSize
  ^ 4 "inline UnorderedCollection instSize"
%
classmethod: 
rubyPrivateInstSize
  ^ UnorderedCollection instSize
%

