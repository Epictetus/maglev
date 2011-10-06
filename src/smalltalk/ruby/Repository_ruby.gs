category: '*maglev-as yet unclassified'
!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!=========================================================================

set class Repository

!  additional methods  for Repository to support Ruby 

category: '*maglev-Ruby Support'
classmethod: 
_loadedClasses: includeModules
  
  ^ SystemRepository _loadedClasses: includeModules
%
